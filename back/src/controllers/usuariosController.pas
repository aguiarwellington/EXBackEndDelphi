unit usuariosController;

interface

uses
  Horse,
  dbConfig,
  System.JSON,
  System.SysUtils,
  FireDAC.DApt,
  IdHTTP, IdSSL, IdSSLOpenSSL, Classes,
  AuthMiddleware,
  codeGenerate,
  RESTRequest4D,
  FireDAC.Comp.Client;

procedure RegistrarRotas;

implementation

procedure Login(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Dm: TConfigDM;
  Body, JsonResponse: TJSONObject;
  BodyStr, Provider, ProviderID, Code, ID, Email, Senha: string;
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
        Res.Status(THTTPStatus.BadRequest).Send('Invalid JSON format: ' + E.Message);
        Exit;
      end;
    end;

    if Body = nil then
    begin
      Res.Status(THTTPStatus.BadRequest).Send('Invalid JSON format');
      Exit;
    end;

    ID := Body.GetValue<string>('id', '');
    Email := Body.GetValue<string>('email', '');
    Senha := Body.GetValue<string>('password', '');
    Provider := Body.GetValue<string>('provider', 'normal');
    ProviderID := Body.GetValue<string>('provider_id', '');
    Code := Body.GetValue<string>('code', '');

    Dm := TConfigDM.Create(nil);
    try
      JsonResponse := Dm.UsuarioLogin(ID, Email, Senha, Provider, ProviderID);

      if JsonResponse.GetValue<string>('status') = 'error' then
        Res.Send(JsonResponse).Status(400)
      else
        Res.Send(JsonResponse).Status(200);

    finally
      Dm.Free;
    end;

  except
    on Ex: Exception do
      Res.Send('Ocorreu um erro: ' + Ex.Message).Status(500);
  end;
end;

procedure RegisterUser(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  bodyStr: string;
  body: TJSONObject;
  FirstName, LastName, Email, Password, Provider, ProviderID: string;
  Dm: Tconfigdm;
  JsonResponse: TJSONObject;
begin
  try
    bodyStr := Req.Body;

    if bodyStr = '' then
    begin
      Res.Status(THTTPStatus.BadRequest).Send('Request body is empty');
      Exit;
    end;

    try
      body := TJSONObject.ParseJSONValue(bodyStr) as TJSONObject;
    except
      on E: Exception do
      begin
        Res.Status(THTTPStatus.BadRequest).Send('Invalid JSON format');
        Exit;
      end;
    end;

    if body = nil then
    begin
      Res.Status(THTTPStatus.BadRequest).Send('Invalid JSON format');
      Exit;
    end;

    FirstName := body.GetValue<string>('first_name', '');
    LastName := body.GetValue<string>('last_name', '');
    Email := body.GetValue<string>('email', '');
    Password := body.GetValue<string>('password', '');
    Provider := body.GetValue<string>('provider', 'normal');
    ProviderID := body.GetValue<string>('provider_id', '');


    Dm := TConfigDM.Create(nil);
    try
      if Provider <> 'normal' then
      begin
        JsonResponse := Dm.InsertUser(FirstName, LastName, Email, '', Provider, ProviderID);
      end
      else
      begin
         JsonResponse := Dm.InsertUser(FirstName, LastName, Email, Password, 'normal', '');
      end;

      if JsonResponse.GetValue<string>('status') = 'error' then
        Res.Send(JsonResponse).Status(400)
      else
        Res.Send(JsonResponse).Status(201);

    finally
      Dm.Free;
    end;

  except
    on Ex: Exception do
      Res.Send('Ocorreu um erro: ' + Ex.Message).Status(500);
  end;

end;

procedure EnviarCodigoSMS(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  UserID, Codigo, Destinatario, IP: string;
  JsonRequest, JsonResponse: TJSONObject;
  Resp: IResponse;
  Dm: TConfigDM;

  JsonString: string;
  Username, Password: string;
  URL: string;
begin
  try
    Username := 'wellingtoncarvalho908@gmail.com';
    Password := '9F73ABD1-89B9-5809-1943-8F664EFD2FFC';

    URL := 'https://rest.clicksend.com/v3/sms/send';

    JsonRequest := TJSONObject.ParseJSONValue(Req.Body) as TJSONObject;

    if JsonRequest = nil then
    begin
      Res.Status(400).Send('JSON inválido');
      Exit;
    end;

    UserID := JsonRequest.GetValue<string>('user_id', '');
    Destinatario := JsonRequest.GetValue<string>('phone', '');
    IP := Req.RawWebRequest.RemoteAddr;

    // Gerar código
    Codigo := TcodeGenerate.GerarCodigo;

    // Salvar no banco
    Dm := TConfigDM.Create(nil);
    try
      Dm.SalvarCodigoAutenticacao(strtoint(UserID), Codigo, IP);
    finally
      Dm.Free;
    end;

    JsonRequest := TJSONObject.Create;
    JsonRequest.AddPair('messages', TJSONArray.Create(
      TJSONObject.Create
        .AddPair('to', Destinatario)
        .AddPair('body', 'Seu código de autenticação é: ' + Codigo)
        .AddPair('from', '583839')
    ));

    JsonString := JsonRequest.ToString;

      resp := TRequest.New
      .BaseURL(URL)
      .BasicAuthentication(Username, Password)
      .AddBody(JsonString)
      .ContentType('application/json')
      .Accept('application/json')
      .Post;

    if Resp.StatusCode = 200 then
     Res.Status(200).Send('Mensagem enviada com sucesso')
    else
      Res.Status(400).Send('Erro ao enviar a mensagem')

  except
    on E: Exception do
      Res.Send('Erro ao enviar código: ' + E.Message).Status(500);
  end;
end;

procedure VerificarCodigoExistente(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  UserID, CodigoEnviado: string;
  JsonResponse: TJSONObject;
  Dm: TConfigDM;
begin
  UserID := Req.Body<TJSONObject>.GetValue<string>('user_id', '');

  Req.Body<TJSONObject>.TryGetValue<string>('code', CodigoEnviado);

  Dm := TConfigDM.Create(nil);
  try
    JsonResponse := Dm.VerificarCodigoExistente(UserID, CodigoEnviado);
  finally
    Dm.Free;
  end;

  Res.Send(JsonResponse.ToString).Status(200);
  JsonResponse.Free;
end;

procedure ValidarSessao(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  UserID: string;
  Query: TFDQuery;
  Dm: TConfigDM;
  jsonResp: TJSONObject;
begin
  UserID := Req.Body<TJSONObject>.GetValue<string>('user_id', '');

  Dm := TConfigDM.Create(nil);
  Query := TFDQuery.Create(nil);
  jsonResp := TJSONObject.Create;
  try
    Query.Connection := Dm.conn;
    Query.SQL.Text := 'SELECT id FROM users WHERE id = :id';
    Query.ParamByName('id').AsString := UserID;
    Query.Open;

    if not Query.IsEmpty then
      jsonResp.AddPair('sessao_valida', TJSONBool.Create(True))
    else
      jsonResp.AddPair('sessao_valida', TJSONBool.Create(False));

    Res.Send(jsonResp).Status(200);
  finally
    Query.Free;
    Dm.Free;
  end;
end;

procedure AtivarBiometria(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Dm: TConfigDM;
  UserID: string;
  Ativa: Boolean;
  Query: TFDQuery;
  JsonBody: TJSONObject;
begin
  JsonBody := Req.Body<TJSONObject>;
  UserID := JsonBody.GetValue<string>('user_id', '');

  if UserID = '' then
  begin
    Res.Status(400).Send('ID do usuário não informado');
    Exit;
  end;

  // Só atualiza se o campo 'ativa' estiver presente no JSON
  if not JsonBody.TryGetValue<Boolean>('ativa', Ativa) then
  begin
    Res.Send('Campo "ativa" não fornecido — nenhum update realizado.').Status(200);
    Exit;
  end;

  Dm := TConfigDM.Create(nil);
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Dm.conn;
    Query.SQL.Text := 'UPDATE users SET biometria_ativa = :ativa WHERE id = :id';
    Query.ParamByName('id').AsString := UserID;
    Query.ParamByName('ativa').AsBoolean := Ativa;
    Query.ExecSQL;

    Res.Send('Biometria atualizada com sucesso').Status(200);
  except
    on E: Exception do
      Res.Status(500).Send('Erro ao atualizar biometria: ' + E.Message);
  end;

  Query.Free;
  Dm.Free;
end;


procedure ObterStatusBiometria(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Dm: TConfigDM;
  UserID: string;
  Query: TFDQuery;
  Ativa: Boolean;
begin
  UserID := Req.Body<TJSONObject>.GetValue<string>('user_id', '');

  if UserID = '' then
  begin
    Res.Status(400).Send('ID do usuário não informado');
    Exit;
  end;

  Dm := TConfigDM.Create(nil);
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Dm.conn;
    Query.SQL.Text := 'SELECT biometria_ativa FROM users WHERE id = :id';
    Query.ParamByName('id').AsString := UserID;
    Query.Open;

    Ativa := Query.FieldByName('biometria_ativa').AsBoolean;
    Res.Send(TJSONObject.Create.AddPair('ativa', TJSONBool.Create(Ativa))).Status(200);
  except
    on E: Exception do
      Res.Status(500).Send('Erro ao buscar status da biometria: ' + E.Message);
  end;

  Query.Free;
  Dm.Free;
end;

procedure BiometriaLogada(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  UserID: string;
begin
  UserID := Req.Body<TJSONObject>.GetValue<string>('user_id', '');

  if UserID = '' then
  begin
    Res.Status(400).Send('ID do usuário não informado');
    Exit;
  end;

  Res.Send('Login por biometria validado com sucesso').Status(200);
end;


procedure RegistrarRotas;
begin
  THorse.Get('/usuarios/login', Login);
  THorse.Post('/usuarios/login', Login);
  THorse.Post('/usuarios/register', RegisterUser);

  THorse.Post('/usuarios/validar-sessao', ValidarSessao);

  THorse.Post('/usuarios/ativar-biometria', AtivarBiometria);
  THorse.Post('/usuarios/status-biometria', ObterStatusBiometria);
  THorse.Post('/usuarios/biometria-logada', BiometriaLogada);


  THorse.Post('/usuarios/enviarCodigo', EnviarCodigoSMS);
  THorse.Post('/usuarios/verificar-codigo-existente', VerificarCodigoExistente);
end;

end.

