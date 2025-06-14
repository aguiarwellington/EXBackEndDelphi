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
  uMd5;

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

      // Verifique se o cadastro é via provider social (Google/Facebook)
      if Provider <> 'normal' then
      begin
        // Se for um cadastro social, não usamos a senha
        LocalPassword := '';  // Não é necessário para Google/Facebook
      end
      else
      begin

        LocalPassword := SaltPassWord(Password);
      end;

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



function TConfigDM.UsuarioLogin(const ID, Email, Senha, Provider, ProviderID: string): TJSONObject;
var
  Query: TFDQuery;
  JsonResponse: TJSONObject;
begin
  JsonResponse := TJSONObject.Create;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := conn;

    if (Provider = 'normal') then
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
    else if (Provider = 'Google') or (Provider = 'Facebook') then
    begin
      if (ProviderID = '') then
      begin
        JsonResponse.AddPair('status', 'error');
        JsonResponse.AddPair('message', 'ProviderID é obrigatório para login com ' + Provider + '.');
        Exit(JsonResponse);
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
  Result := JsonResponse;
end;

end.

