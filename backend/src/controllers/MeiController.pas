unit MeiController;

interface

uses
  Horse,
  System.JSON,
  System.SysUtils,
  FireDAC.Comp.Client,
  dbConfig,
  ApiCnpjService,
  ApiCepService,IdSSLOpenSSLHeaders;

procedure RegistrarMeiRotas;

implementation

procedure CadastrarMei(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Dm: TConfigDM;
  Body, JsonResponse: TJSONObject;
  BodyStr, CNPJ, RazaoSocial, NomeFantasia, InscricaoMunicipal, Email, Telefone, Foto,
  EnderecoRua, EnderecoNumero, EnderecoBairro, EnderecoCidade, EnderecoEstado, EnderecoCEP: string;
  UsuarioID: Integer;
begin
  try
    BodyStr := Req.Body;

    if BodyStr = '' then
    begin
      Res.Status(THTTPStatus.BadRequest).Send('Request body is empty');
      Exit;
    end;

    try
      Body := TJSONObject.ParseJSONValue(BodyStr) as TJSONObject;
    except
      on E: Exception do
      begin
        Res.Status(THTTPStatus.BadRequest).Send('Invalid JSON format');
        Exit;
      end;
    end;

    if Body = nil then
    begin
      Res.Status(THTTPStatus.BadRequest).Send('Invalid JSON format');
      Exit;
    end;

    UsuarioID := Body.GetValue<Integer>('id_usuario', 0);
    if UsuarioID = 0 then
    begin
      Res.Status(THTTPStatus.BadRequest).Send('ID do usuário é obrigatório');
      Exit;
    end;

    CNPJ := Body.GetValue<string>('cnpj', '');
    RazaoSocial := Body.GetValue<string>('razao_social', '');
    NomeFantasia := Body.GetValue<string>('nome_fantasia', '');
    InscricaoMunicipal := Body.GetValue<string>('inscricao_municipal', '');
    EnderecoRua := Body.GetValue<string>('rua', '');
    EnderecoNumero := Body.GetValue<string>('numero', '');
    EnderecoBairro := Body.GetValue<string>('bairro', '');
    EnderecoCidade := Body.GetValue<string>('cidade', '');
    EnderecoEstado := Body.GetValue<string>('estado', '');
    EnderecoCEP := Body.GetValue<string>('cep', '');
    Email := Body.GetValue<string>('email', '');
    Telefone := Body.GetValue<string>('telefone', '');
    Foto := Body.GetValue<string>('foto', '');

    Dm := TConfigDM.Create(nil);
    try
      JsonResponse := Dm.InsertMei(UsuarioID, CNPJ, RazaoSocial, NomeFantasia, InscricaoMunicipal,
        EnderecoRua, EnderecoNumero, EnderecoBairro, EnderecoCidade, EnderecoEstado,
        EnderecoCEP, Email, Telefone, Foto);

      if JsonResponse = nil then
        Res.Status(THTTPStatus.BadRequest).Send('Erro ao cadastrar MEI')
      else
        Res.Send<TJSONObject>(JsonResponse).Status(201);

    finally
      Dm.Free;
    end;

  except
    on E: Exception do
    begin
      Res.Status(THTTPStatus.InternalServerError).Send('Erro ao cadastrar MEI: ' + E.Message);
    end;
  end;
end;


procedure BuscarMei(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Dm: TConfigDM;
  Query: TFDQuery;
  JsonArray: TJSONArray;
  JsonMei: TJSONObject;
  UsuarioID: Integer;
begin
  try
    UsuarioID := StrToIntDef(Req.Params['id_usuario'], 0);

    if UsuarioID = 0 then
    begin
      Res.Status(THTTPStatus.BadRequest).Send('ID do usuário inválido');
      Exit;
    end;

    Dm := TConfigDM.Create(nil);
    try
      Query := Dm.BuscarMei(UsuarioID);
      JsonArray := TJSONArray.Create;

      while not Query.Eof do
      begin
        JsonMei := TJSONObject.Create;
        JsonMei.AddPair('id_mei', Query.FieldByName('id_mei').AsString);
        JsonMei.AddPair('cnpj', Query.FieldByName('cnpj').AsString);
        JsonMei.AddPair('razao_social', Query.FieldByName('razao_social').AsString);
        JsonMei.AddPair('nome_fantasia', Query.FieldByName('nome_fantasia').AsString);
        JsonMei.AddPair('email', Query.FieldByName('email').AsString);
        JsonMei.AddPair('telefone', Query.FieldByName('telefone').AsString);
        JsonMei.AddPair('endereco_rua', Query.FieldByName('endereco_rua').AsString);
        JsonMei.AddPair('endereco_numero', Query.FieldByName('endereco_numero').AsString);
        JsonMei.AddPair('endereco_bairro', Query.FieldByName('endereco_bairro').AsString);
        JsonMei.AddPair('endereco_cidade', Query.FieldByName('endereco_cidade').AsString);
        JsonMei.AddPair('endereco_estado', Query.FieldByName('endereco_estado').AsString);
        JsonMei.AddPair('endereco_cep', Query.FieldByName('endereco_cep').AsString);
        JsonMei.AddPair('foto', Query.FieldByName('foto').AsString); // <- aqui

        JsonArray.AddElement(JsonMei);
        Query.Next;
      end;


      Res.Send<TJSONArray>(JsonArray).Status(200);
    finally
      Dm.Free;
    end;

  except
    on E: Exception do
    begin
      Res.Status(THTTPStatus.InternalServerError).Send('Erro ao buscar MEI: ' + E.Message);
    end;
  end;
end;

procedure BuscarCNPJ(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  CNPJ: string;
  CnpjService: TCnpjService;
  JsonResponse: TJSONObject;
begin
  CNPJ := Req.Params['cnpj'];

  if CNPJ = '' then
  begin
    Res.Status(400).Send('CNPJ não informado');
    Exit;
  end;

  CnpjService := TCnpjService.Create;
  try
    JsonResponse := CnpjService.BuscarCNPJ(CNPJ);
    Res.Send<TJSONObject>(JsonResponse).Status(200);
  finally
    CnpjService.Free;
  end;
end;

procedure BuscarCEP(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  CEP: string;
  CepService: TCepService;
  JsonResponse: TJSONObject;
begin
  CEP := Req.Params['cep'];

  if CEP = '' then
  begin
    Res.Status(400).Send('CEP não informado');
    Exit;
  end;

  CepService := TCepService.Create;
  try
    JsonResponse := CepService.BuscarCEP(CEP);
    Res.Send<TJSONObject>(JsonResponse).Status(200);
  finally
    CepService.Free;
  end;
end;

procedure RegistrarMeiRotas;
begin
  THorse.Post('/mei/cadastrar', CadastrarMei);
  THorse.Get('/mei/:id_usuario', BuscarMei);

  THorse.Get('/mei/cnpj/:cnpj', BuscarCNPJ);
  THorse.Get('/mei/cep/:cep', BuscarCEP);
end;

end.

