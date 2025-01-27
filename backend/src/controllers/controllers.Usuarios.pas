unit Controllers.Usuarios;

interface

uses
  Horse,
  dbConfig,
  System.JSON,
  System.SysUtils,
  FireDAC.DApt,
  IdHTTP, IdSSL, IdSSLOpenSSL, Classes,
  AuthMiddleware;

procedure RegistrarRotas;

implementation

function GetGoogleIDToken(const Code: string): string;
var
  HTTPClient: TIdHTTP;
  SSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  Params: TStringList;
  Response: string;
  JSONResponse: TJSONObject;
begin
  Result := '';

  HTTPClient := TIdHTTP.Create(nil);
  SSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  Params := TStringList.Create;

  HTTPClient.IOHandler := SSLIOHandler;
  HTTPClient.Request.ContentType := 'application/x-www-form-urlencoded';

  try
    Params.Add('code=' + Code); // Código de autorização obtido (do Insomnia)
    Params.Add('client_id=797814134249-m9l4jbqk7vhvdoqg7822jp79vae9itqj.apps.googleusercontent.com'); // Substitua pelo seu Client ID
    Params.Add('redirect_uri=http://localhost:3000'); // URI de redirecionamento
    Params.Add('grant_type=authorization_code');  // Tipo de grant

    Response := HTTPClient.Post('https://oauth2.googleapis.com/token', Params);

    JSONResponse := TJSONObject.ParseJSONValue(Response) as TJSONObject;
    if JSONResponse <> nil then
    begin
      try
        Result := JSONResponse.GetValue<string>('id_token');  // Pega o ID Token da resposta
      finally
        JSONResponse.Free;
      end;
    end;

  except
    on E: Exception do
      raise Exception.Create('Erro ao obter o ID Token: ' + E.Message);
  end;

  Params.Free;
  HTTPClient.Free;
  SSLIOHandler.Free;
end;

function VerifyGoogleIDToken(const IDToken: string): Boolean;
var
  HTTPClient: TIdHTTP;
  SSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  Params: TStringList;
  Response: string;
begin
  Result := False;

  HTTPClient := TIdHTTP.Create(nil);
  SSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  Params := TStringList.Create;

  HTTPClient.IOHandler := SSLIOHandler;

  try
    // Envia o token para validação no Google
    Params.Add('id_token=' + IDToken);
    Response := HTTPClient.Post('https://oauth2.googleapis.com/tokeninfo', Params);

    if Response.Contains('email') then
    begin
      // Se a resposta contiver o e-mail, o token é válido
      Result := True;
    end;

  except
    on E: Exception do
      raise Exception.Create('Erro ao validar o ID Token do Google: ' + E.Message);
  end;

  Params.Free;
  HTTPClient.Free;
  SSLIOHandler.Free;
end;

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
    ProviderID := Body.GetValue<string>('provider_id', ''); // id_token do Google
    Code := Body.GetValue<string>('code', ''); // Código de autorização do Google

    // Se o login for via Google (usando provider=google)
    if Provider = 'google' then
    begin
      // Obter o ID Token do Google com o código de autorização
      try
        ProviderID := GetGoogleIDToken(Code); // Atribui o id_token ao ProviderID

        // Verificar se o ID Token é válido
        if not VerifyGoogleIDToken(ProviderID) then
        begin
          Res.Status(THTTPStatus.Unauthorized).Send('ID Token inválido');
          Exit;
        end;
      except
        on E: Exception do
        begin
          Res.Status(THTTPStatus.Unauthorized).Send('Erro ao obter o ID Token: ' + E.Message);
          Exit;
        end;
      end;
    end;

    Dm := TConfigDM.Create(nil);
    try
      // Chamar o método de autenticação (pode verificar se o provider é Google e usar o id_token)
      JsonResponse := Dm.UsuarioLogin(ID, Email, Senha, Provider, ProviderID);

      // Verificar o resultado e retornar ao cliente
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
    ProviderID := body.GetValue<string>('provider_id', '');  // Google ou Facebook ID

    Dm := Tconfigdm.Create(nil);

    // Verificando o tipo de cadastro (normal ou social)
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

procedure RegistrarRotas;
begin
  THorse.Get('/usuarios/login', Login);
  THorse.Post('/usuarios/login', Login);
  THorse.Post('/usuarios/register', RegisterUser);
end;

end.

