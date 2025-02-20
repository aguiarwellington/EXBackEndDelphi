unit dbConfig;

interface

uses
  System.SysUtils,
  System.Classes,
  FireDAC.Phys.MySQLDef,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Phys.MySQL,
  FireDAC.ConsoleUI.Wait,
  Data.DB,
  FireDAC.Comp.Client,
  DataSet.Serialize,
  DataSet.Serialize.Config,
  System.JSON,
  FireDAC.DApt,
  Vcl.Forms,
  FireDAC.Stan.Param,
  FireDAC.DatS,
  FireDAC.DApt.Intf,
  FireDAC.Comp.DataSet,
  FireDAC.VCLUI.Wait,
  uMd5,
  IdHTTP,
  IdSSL,
  IdSSLOpenSSL;

type
  Tconfigdm = class(TDataModule)
    conn: TFDConnection;
    FDPhysMySQLDriverLink: TFDPhysMySQLDriverLink;
    procedure DataModuleCreate(Sender: TObject);
    procedure connBeforeConnect(Sender: TObject);
  private
    procedure CarregarConfigDB(Connection: TFDConnection);
  public
    function InsertUser(const FirstName, LastName, Email, Password, Provider, ProviderID: string): TJSONObject;
    function UsuarioLogin(const id, Email, Senha, Provider, ProviderID: string): TJSONObject;
    function SalvarCodigoAutenticacao(const UserID : integer; Codigo, IP: string ) : TJSONObject;
    function VerificarCodigoExistente(const UserID: string): TJSONObject;
  end;

var
  configdm: Tconfigdm;

implementation

procedure Tconfigdm.CarregarConfigDB(Connection: TFDConnection);
begin
  //Ter a dll no TFDPhysMySQLDriverLink

  Connection.DriverName := 'MySQL';

  with Connection.Params do
  begin
    Add('Server=localhost');
    Add('Port=3306');
    Add('Database=appex');
    Add('User_Name=root');
    Add('Password=');
  end;
end;

{$R *.dfm}

procedure Tconfigdm.connBeforeConnect(Sender: TObject);
begin
  CarregarConfigDB(Conn);
end;

procedure Tconfigdm.DataModuleCreate(Sender: TObject);
begin
  TDataSetSerializeConfig.GetInstance.CaseNameDefinition := cndLower;
  TDataSetSerializeConfig.GetInstance.Import.DecimalSeparator := '.';
end;

function Tconfigdm.InsertUser(const FirstName, LastName, Email, Password,
  Provider, ProviderID: string): TJSONObject;
var
  Query: TFDQuery;
  JsonResponse: TJSONObject;
  LocalPassword: string;
begin
  JsonResponse := TJSONObject.Create;
  try
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := conn;

      // Verifica se o cadastro é via provider social
      if Provider <> 'normal' then
        LocalPassword := ''
      else
        LocalPassword := SaltPassWord(Password);

      Query.SQL.Text := 'INSERT INTO users (first_name, last_name, email, password, provider, provider_id) ' +
                        'VALUES (:first_name, :last_name, :email, :password, :provider, :provider_id)';

      Query.ParamByName('first_name').AsString := FirstName;
      Query.ParamByName('last_name').AsString := LastName;
      Query.ParamByName('email').AsString := Email;
      Query.ParamByName('password').AsString := LocalPassword;
      Query.ParamByName('provider').AsString := Provider;
      Query.ParamByName('provider_id').AsString := ProviderID;

      Query.ExecSQL;

      JsonResponse.AddPair('status', 'success');
      JsonResponse.AddPair('message', 'Usuário registrado com sucesso.');
    except
      on E: Exception do
      begin
        JsonResponse.AddPair('status', 'error');
        JsonResponse.AddPair('message', 'Erro ao registrar usuário: ' + E.Message);
      end;
    end;
  finally
    Query.Free;
  end;

  Result := JsonResponse;
end;

function Tconfigdm.SalvarCodigoAutenticacao(const UserID: integer; Codigo,
  IP: string): TJSONObject;
var
  Query: TFDQuery;
  JsonResponse: TJSONObject;
begin
  JsonResponse := TJSONObject.Create;

  try
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := conn;

      Query.SQL.Text := 'INSERT INTO autenticacao_codigos (user_id, codigo, data_hora, ip_dispositivo) ' +
                        'VALUES (:user_id, :codigo, :data_hora, :ip_dispositivo)';

      Query.ParamByName('user_id').AsInteger := UserID;
      Query.ParamByName('codigo').AsString := Codigo;
      Query.ParamByName('data_hora').AsDateTime := Now;
      Query.ParamByName('ip_dispositivo').AsString := IP;

      Query.ExecSQL;

      JsonResponse.AddPair('status', 'success');
      JsonResponse.AddPair('message', 'Código de autenticação salvo com sucesso.');
    except
      on E: Exception do
      begin
        JsonResponse.AddPair('status', 'error');
        JsonResponse.AddPair('message', 'Erro ao salvar código: ' + E.Message);
      end;
    end;
  finally
    Query.Free;
  end;

  Result := JsonResponse;
end;

function TConfigDM.UsuarioLogin(const ID, Email, Senha, Provider, ProviderID: string): TJSONObject;
var
  Query: TFDQuery;
  JsonResponse: TJSONObject;
  HttpClient: TIdHTTP;
  GoogleTokenInfo: TStringList;
  GoogleResponse: string;
begin
  JsonResponse := TJSONObject.Create;
  Query := TFDQuery.Create(nil);
  HttpClient := TIdHTTP.Create(nil);
  GoogleTokenInfo := TStringList.Create;

  try
    Query.Connection := conn;

    if Provider = 'normal' then
    begin
      if (Email = '') or (Senha = '') then
      begin
        JsonResponse.AddPair('status', 'error');
        JsonResponse.AddPair('message', 'Email e Senha são obrigatórios para login normal.');
        Exit(JsonResponse);
      end;

      Query.SQL.Text := 'SELECT id, email FROM users ' +
                        'WHERE email = :email AND password = :password';
      Query.ParamByName('email').AsString := Email;
      Query.ParamByName('password').AsString := SaltPassWord(Senha);
    end
    else if Provider = 'Google' then
    begin
      if ProviderID = '' then
      begin
        JsonResponse.AddPair('status', 'error');
        JsonResponse.AddPair('message', 'ProviderID é obrigatório para login com Google.');
        Exit(JsonResponse);
      end;

      try
        HttpClient.Request.UserAgent := 'Delphi';
        GoogleTokenInfo.Add('id_token=' + ProviderID);

        GoogleResponse := HttpClient.Get('https://oauth2.googleapis.com/tokeninfo?id_token=' + ProviderID);

        if GoogleResponse.Contains('error') then
        begin
          JsonResponse.AddPair('status', 'error');
          JsonResponse.AddPair('message', 'Token do Google inválido.');
          Exit(JsonResponse);
        end;
      except
        on E: Exception do
        begin
          JsonResponse.AddPair('status', 'error');
          JsonResponse.AddPair('message', 'Erro ao verificar o token do Google: ' + E.Message);
          Exit(JsonResponse);
        end;
      end;

      Query.SQL.Text := 'SELECT id, email FROM users ' +
                        'WHERE provider = :provider AND provider_id = :providerID';
      Query.ParamByName('provider').AsString := Provider;
      Query.ParamByName('providerID').AsString := ProviderID;
    end
    else
    begin
      JsonResponse.AddPair('status', 'error');
      JsonResponse.AddPair('message', 'Provider inválido.');
      Exit(JsonResponse);
    end;

    Query.Open;

    if not Query.IsEmpty then
    begin
      JsonResponse.AddPair('status', 'success');
      JsonResponse.AddPair('id', Query.FieldByName('id').AsString);
      JsonResponse.AddPair('email', Query.FieldByName('email').AsString);
    end
    else
    begin
      JsonResponse.AddPair('status', 'error');
      JsonResponse.AddPair('message', 'Usuário não encontrado ou credenciais inválidas.');
    end;

  except
    on E: Exception do
    begin
      JsonResponse.AddPair('status', 'error');
      JsonResponse.AddPair('message', 'Erro ao processar login: ' + E.Message);
    end;
  end;

  Query.Free;
  HttpClient.Free;
  GoogleTokenInfo.Free;

  Result := JsonResponse;
end;

function Tconfigdm.VerificarCodigoExistente(const UserID: string): TJSONObject;
var
  Query: TFDQuery;
  JsonResponse: TJSONObject;
begin
  JsonResponse := TJSONObject.Create;
  Query := TFDQuery.Create(nil);

  if not conn.Connected then
    conn.Connected := True;

  try
    Query.Connection := conn;
    Query.SQL.Text := 'SELECT codigo FROM autenticacao_codigos WHERE user_id = :user_id LIMIT 1';
    Query.ParamByName('user_id').AsString := UserID;
    Query.Open;

    if not Query.IsEmpty then
      JsonResponse.AddPair('status', 'success').AddPair('codigo_existe', 'true')
    else
      JsonResponse.AddPair('status', 'success').AddPair('codigo_existe', 'false');
  except
    on E: Exception do
    begin
      JsonResponse.AddPair('status', 'error');
      JsonResponse.AddPair('message', 'Erro ao verificar código: ' + E.Message);
    end;
  end;
  Query.Free;
  Result := JsonResponse;
end;



end.

