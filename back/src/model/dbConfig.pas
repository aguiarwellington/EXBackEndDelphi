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
  IdSSLOpenSSL,
  System.StrUtils;

type
  Tconfigdm = class(TDataModule)
    conn: TFDConnection;
    FDPhysMySQLDriverLink: TFDPhysMySQLDriverLink;
    procedure DataModuleCreate(Sender: TObject);
    procedure connBeforeConnect(Sender: TObject);
  private
    procedure CarregarConfigDB(Connection: TFDConnection);
  public
   function EmailExists(const Email: string): Boolean;
    function InsertUser(const FirstName, LastName, Email, Password, Provider, ProviderID: string): TJSONObject;
    function UsuarioLogin(const Email, Senha, Provider, ProviderID: string): TJSONObject;
    function SalvarCodigoAutenticacao(const UserID : integer; Codigo, IP: string ) : TJSONObject;
    function VerificarCodigoExistente(const UserID: string): TJSONObject;

    //meis
  function InsertMei(const UsuarioID: Integer; const CNPJ, RazaoSocial, NomeFantasia,
                    InscricaoMunicipal, EnderecoRua, EnderecoNumero, EnderecoBairro, EnderecoCidade,
                    EnderecoEstado, EnderecoCEP, Email, Telefone, Foto: string): TJSONObject;

    function BuscarMei(const UsuarioID: Integer): TFDQuery;
  end;

var
  configdm: Tconfigdm;

implementation

function TConfigDM.BuscarMei(const UsuarioID: Integer): TFDQuery;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := conn;
    Query.SQL.Text := 'SELECT * FROM meicadastro WHERE id_usuario = :id_usuario';
    Query.ParamByName('id_usuario').AsInteger := UsuarioID;
    Query.Open;

    Result := Query; // Retorna o resultado da consulta
  except
    Query.Free;
    raise Exception.Create('Erro ao buscar MEIs.');
  end;
end;


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

function TConfigDM.InsertMei(const UsuarioID: Integer; const CNPJ, RazaoSocial, NomeFantasia,
  InscricaoMunicipal, EnderecoRua, EnderecoNumero, EnderecoBairro, EnderecoCidade,
  EnderecoEstado, EnderecoCEP, Email, Telefone, Foto: string): TJSONObject;
var
  Query: TFDQuery;
  JsonResponse: TJSONObject;
  JaExiste: Boolean;
begin
  JsonResponse := TJSONObject.Create;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := conn;

    // Verificar se já existe registro para o usuário
    Query.SQL.Text := 'SELECT COUNT(*) FROM meicadastro WHERE id_usuario = :id_usuario';
    Query.ParamByName('id_usuario').AsInteger := UsuarioID;
    Query.Open;
    JaExiste := Query.Fields[0].AsInteger > 0;
    Query.Close;

    if JaExiste then
    begin
      Query.SQL.Text :=
        'UPDATE meicadastro SET ' +
        'cnpj = :cnpj, razao_social = :razao_social, nome_fantasia = :nome_fantasia, ' +
        'inscricao_municipal = :inscricao_municipal, endereco_rua = :endereco_rua, ' +
        'endereco_numero = :endereco_numero, endereco_bairro = :endereco_bairro, ' +
        'endereco_cidade = :endereco_cidade, endereco_estado = :endereco_estado, ' +
        'endereco_cep = :endereco_cep, email = :email, telefone = :telefone, foto = :foto ' +
        'WHERE id_usuario = :id_usuario';
    end
    else
    begin
      Query.SQL.Text :=
        'INSERT INTO meicadastro (id_usuario, cnpj, razao_social, nome_fantasia, inscricao_municipal, ' +
        'endereco_rua, endereco_numero, endereco_bairro, endereco_cidade, endereco_estado, ' +
        'endereco_cep, email, telefone, foto) ' +
        'VALUES (:id_usuario, :cnpj, :razao_social, :nome_fantasia, :inscricao_municipal, ' +
        ':endereco_rua, :endereco_numero, :endereco_bairro, :endereco_cidade, :endereco_estado, ' +
        ':endereco_cep, :email, :telefone, :foto)';
    end;

    // Parâmetros comuns
    Query.ParamByName('id_usuario').AsInteger := UsuarioID;
    Query.ParamByName('cnpj').AsString := CNPJ;
    Query.ParamByName('razao_social').AsString := RazaoSocial;
    Query.ParamByName('nome_fantasia').AsString := NomeFantasia;
    Query.ParamByName('inscricao_municipal').AsString := InscricaoMunicipal;
    Query.ParamByName('endereco_rua').AsString := EnderecoRua;
    Query.ParamByName('endereco_numero').AsString := EnderecoNumero;
    Query.ParamByName('endereco_bairro').AsString := EnderecoBairro;
    Query.ParamByName('endereco_cidade').AsString := EnderecoCidade;
    Query.ParamByName('endereco_estado').AsString := EnderecoEstado;
    Query.ParamByName('endereco_cep').AsString := EnderecoCEP;
    Query.ParamByName('email').AsString := Email;
    Query.ParamByName('telefone').AsString := Telefone;

    // Campo foto
    with Query.ParamByName('foto') do
    begin
      DataType := ftMemo;
      Size := Length(Foto);
      AsString := Foto;
    end;

    Query.ExecSQL;

    JsonResponse.AddPair('status', 'success');
    JsonResponse.AddPair('message', IfThen(JaExiste, 'MEI atualizado com sucesso.', 'MEI cadastrado com sucesso.'));

  finally
    Query.Free;
  end;

  Result := JsonResponse;
end;


function TConfigDM.InsertUser(const FirstName, LastName, Email, Password, Provider, ProviderID: string): TJSONObject;
var
  Query: TFDQuery;
  JsonResponse: TJSONObject;
  LocalPassword: string;
begin
  JsonResponse := TJSONObject.Create;
  try
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := Conn;

      // ?? Verifica se o e-mail já está cadastrado
      if EmailExists(Email) then
      begin
        JsonResponse.AddPair('status', 'error');
        JsonResponse.AddPair('message', 'E-mail já cadastrado.');
        Exit(JsonResponse);
      end;

      if Provider <> 'normal' then
        LocalPassword := ''
      else
        LocalPassword := SaltPassWord(Password);

      Query.SQL.Text :=
        'INSERT INTO users (first_name, last_name, email, password, provider, provider_id, biometria_ativa) ' +
        'VALUES (:first_name, :last_name, :email, :password, :provider, :provider_id, :biometria_ativa)';

      Query.ParamByName('first_name').AsString := FirstName;
      Query.ParamByName('last_name').AsString := LastName;
      Query.ParamByName('email').AsString := LowerCase(Trim(Email));
      Query.ParamByName('password').AsString := LocalPassword;
      Query.ParamByName('provider').AsString := Provider;
      Query.ParamByName('provider_id').AsString := ProviderID;
      Query.ParamByName('biometria_ativa').AsBoolean := False;

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

function TConfigDM.EmailExists(const Email: string): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Conn;
    Query.SQL.Text := 'SELECT id FROM users WHERE email = :email';
    Query.ParamByName('email').AsString := LowerCase(Trim(Email));
    Query.Open;
    Result := not Query.IsEmpty;
  finally
    Query.Free;
  end;
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

function TConfigDM.UsuarioLogin(const Email, Senha, Provider, ProviderID: string): TJSONObject;
var
  Query, CheckQuery, UpdateQuery: TFDQuery;
  JsonResponse: TJSONObject;
  HttpClient: TIdHTTP;
  GoogleTokenInfo: TStringList;
  GoogleResponse: string;
  CodigoValido: Boolean;
  CodigoData: TDateTime;
begin
  JsonResponse := TJSONObject.Create;
  Query := TFDQuery.Create(nil);
  CheckQuery := TFDQuery.Create(nil);
  UpdateQuery := TFDQuery.Create(nil);
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

      Query.SQL.Text :=
        'SELECT id, email FROM users ' +
        'WHERE LOWER(email) = LOWER(:email) AND password = :password';
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

      Query.SQL.Text :=
        'SELECT id, email FROM users ' +
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
      CodigoValido := False;

      CheckQuery.Connection := conn;
      CheckQuery.SQL.Text :=
        'SELECT codigo_validado, codigo_validado_em FROM users WHERE id = :id';
      CheckQuery.ParamByName('id').AsString := Query.FieldByName('id').AsString;
      CheckQuery.Open;

      if CheckQuery.FieldByName('codigo_validado').AsBoolean then
      begin
        CodigoData := CheckQuery.FieldByName('codigo_validado_em').AsDateTime;
        if (Trunc(Now) - Trunc(CodigoData) <= 15) then
          CodigoValido := True
        else
        begin
          UpdateQuery.Connection := conn;
          UpdateQuery.SQL.Text := 'UPDATE users SET codigo_validado = 0 WHERE id = :id';
          UpdateQuery.ParamByName('id').AsString := Query.FieldByName('id').AsString;
          UpdateQuery.ExecSQL;
        end;
      end;

      if not CodigoValido then
      begin
        JsonResponse.AddPair('status', 'pending_code');
        JsonResponse.AddPair('message', 'Código expirado ou não validado.');
        JsonResponse.AddPair('requires_2fa', TJSONBool.Create(True));
        JsonResponse.AddPair('user_id', Query.FieldByName('id').AsString);
        Exit(JsonResponse);
      end;

      JsonResponse.AddPair('status', 'success');
      JsonResponse.AddPair('id', Query.FieldByName('id').AsString);
      JsonResponse.AddPair('user_id', Query.FieldByName('id').AsString);
      JsonResponse.AddPair('email', Query.FieldByName('email').AsString);
      JsonResponse.AddPair('requires_2fa', TJSONBool.Create(False));
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
  CheckQuery.Free;
  UpdateQuery.Free;
  HttpClient.Free;
  GoogleTokenInfo.Free;

  Result := JsonResponse;
end;

function TConfigDM.VerificarCodigoExistente(const UserID: string): TJSONObject;
var
  Query: TFDQuery;
  JsonResponse: TJSONObject;
  BiometriaAtiva: Boolean;
  CodigoValidado: Boolean;
begin
  JsonResponse := TJSONObject.Create;
  Query := TFDQuery.Create(nil);

  if not conn.Connected then
    conn.Connected := True;

  try
    Query.Connection := conn;
    Query.SQL.Text := 'SELECT biometria_ativa, codigo_validado FROM users WHERE id = :id';
    Query.ParamByName('id').AsString := UserID;
    Query.Open;

    if not Query.IsEmpty then
    begin
      BiometriaAtiva := Query.FieldByName('biometria_ativa').AsBoolean;
      CodigoValidado := Query.FieldByName('codigo_validado').AsBoolean;

      JsonResponse.AddPair('biometria_ativa', TJSONBool.Create(BiometriaAtiva));
      JsonResponse.AddPair('codigo_validado', TJSONBool.Create(CodigoValidado));
    end
    else
    begin
      JsonResponse.AddPair('biometria_ativa', TJSONBool.Create(False));
      JsonResponse.AddPair('codigo_validado', TJSONBool.Create(False));
    end;
  except
    on E: Exception do
    begin
      JsonResponse.AddPair('status', 'error');
      JsonResponse.AddPair('mensagem', 'Erro ao verificar status 2FA: ' + E.Message);
    end;
  end;

  Query.Free;
  Result := JsonResponse;
end;

end.

