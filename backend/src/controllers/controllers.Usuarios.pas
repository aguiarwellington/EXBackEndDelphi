unit Controllers.Usuarios;

interface

uses
  Horse,
  dbConfig,
  System.JSON,
  System.SysUtils,
  FireDAC.DApt,
  AuthMiddleware;

procedure RegistrarRotas;

implementation

procedure Login(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Dm: TConfigDM;
  Body, JsonResponse: TJSONObject;
  BodyStr: string;
  ID, Email, Senha, Provider, ProviderID: string;
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
    Senha := Body.GetValue<string>('senha', '');
    Provider := Body.GetValue<string>('provider', 'normal');
    ProviderID := Body.GetValue<string>('provider_id', '');

    Dm := TConfigDM.Create(nil);
    try
      // Chamar o método de autenticação
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

    // Recuperando os dados do corpo da requisição
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

procedure GetUserProfile(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  JsonResponse: TJSONObject;
  UserId: string;
begin
  try
    // Recupera o ID do usuário do cabeçalho ou token
    UserId := Req.Headers['User-ID'];  // Supondo que você use um cabeçalho ou token para isso

    // Aqui você pode pegar as informações do usuário no banco de dados
    // Exemplo fictício de um retorno de perfil
    JsonResponse := TJSONObject.Create;
    JsonResponse.AddPair('id', UserId);
    JsonResponse.AddPair('first_name', 'John');
    JsonResponse.AddPair('last_name', 'Doe');
    JsonResponse.AddPair('email', 'john.doe@example.com');

    Res.Send(JsonResponse).Status(200);

  except
    on E: Exception do
    begin
      Res.Send('Ocorreu um erro: ' + E.Message).Status(500);
    end;
  end;
end;

procedure RegistrarRotas;
begin
  // Rota de login
  THorse.Post('/usuarios/login', Login);

  // Rota de cadastro de usuário
  THorse.Post('/usuarios/register', RegisterUser);

  // Rota protegida para consultar o perfil (precisa de autenticação)
  THorse.Get('/usuarios/profile', GetUserProfile);

end;

end.

