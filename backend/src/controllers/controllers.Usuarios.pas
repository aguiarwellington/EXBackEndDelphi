unit Controllers.Usuarios;

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

    Dm := Tconfigdm.Create(nil);

    if Provider <> 'normal' then
    begin
      JsonResponse := Dm.InsertUser(FirstName, LastName, Email, '', Provider, ProviderID);  // Senha não necessária para login social
    end
    else
    begin
      JsonResponse := Dm.InsertUser(FirstName, LastName, Email, Password, 'normal', '');  // Para cadastro normal
    end;

    if JsonResponse = nil then
      Res.Status(THTTPStatus.BadRequest).Send('Erro ao registrar usuário')
    else
      Res.Send<TJSONObject>(JsonResponse).Status(201);

    FreeAndNil(Dm);

  except
    on E: Exception do
    begin
      Res.Status(THTTPStatus.InternalServerError).Send('Erro ao processar registro: ' + E.Message);
    end;
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
    Codigo := TcodeGenerate.GerarCodigo; // Gera um código de 6 dígitos

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
  CodigoEnviado := Req.Body<TJSONObject>.GetValue<string>('code', '');

  Dm := TConfigDM.Create(nil);
  try
    JsonResponse := Dm.VerificarCodigoExistente(UserID, CodigoEnviado);
  finally
    Dm.Free;
  end;

  Res.Send(JsonResponse.ToString).Status(200);
  JsonResponse.Free;
end;


procedure RegistrarRotas;
begin
  THorse.Get('/usuarios/login', Login);
  THorse.Post('/usuarios/login', Login);
  THorse.Post('/usuarios/register', RegisterUser);


  THorse.Post('/usuarios/enviarCodigo', EnviarCodigoSMS);
  THorse.Post('/usuarios/verificar-codigo-existente', VerificarCodigoExistente);

end;

end.

