unit Controllers.Usuario;

interface

uses
  Horse,
  dbConfig,
  System.JSON,
  System.SysUtils,
  FireDAC.DApt;

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
        Res.Send<TJSONObject>(JsonResponse).Status(200);

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
  FirstName, LastName, Email, Password: string;
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
        Writeln('Erro ao parsear JSON: ', E.Message);
        Res.Status(THTTPStatus.BadRequest).Send('Invalid JSON format');
        Exit;
      end;
    end;

    if body = nil then
    begin
      Writeln('Erro: JSON inválido.');
      Res.Status(THTTPStatus.BadRequest).Send('Invalid JSON format');
      Exit;
    end;

    FirstName := body.GetValue<string>('first_name', '');
    LastName := body.GetValue<string>('last_name', '');
    Email := body.GetValue<string>('email', '');
    Password := body.GetValue<string>('password', '');

    Dm := Tconfigdm.Create(nil);

    JsonResponse := Dm.InsertUser(FirstName, LastName, Email, Password, 'normal', '');

    if JsonResponse = nil then
      Res.Status(THTTPStatus.BadRequest).Send('Erro ao registrar usuário')
    else
      Res.Send<TJSONObject>(JsonResponse).Status(201);

    FreeAndNil(Dm);

  except
    on E: Exception do
    begin
      Writeln('Erro ao processar POST /usuarios/register: ', E.Message);
      Res.Status(THTTPStatus.InternalServerError).Send(E.Message);
    end;
  end;
end;

procedure RegistrarRotas;
begin
  THorse.Post('/usuarios/login', Login);

  THorse.Post('/usuarios/register', RegisterUser);
end;

end.

