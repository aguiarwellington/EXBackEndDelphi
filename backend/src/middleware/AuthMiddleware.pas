unit AuthMiddleware;

interface

uses
  Horse, System.SysUtils, System.Classes, IdCoder, IdCoderMIME, System.StrUtils;

type
  TAuthMiddleware = class
  public
    class procedure Apply(Request: THorseRequest; Response: THorseResponse; Next: TProc); static;
  end;

implementation

{ TAuthMiddleware }

class procedure TAuthMiddleware.Apply(Request: THorseRequest; Response: THorseResponse; Next: TProc);
var
  AuthHeader, DecodedCredentials: string;
  Credentials: TArray<string>;
  Username, Password: string;
begin
  // Recuperar o cabeçalho Authorization
  AuthHeader := Request.Headers['Authorization'];

  if (AuthHeader <> '') and (Pos('Basic ', AuthHeader) = 1) then
  begin
    // Remover o prefixo "Basic "
    AuthHeader := Copy(AuthHeader, 7, MaxInt);

    // Decodificar as credenciais base64
    DecodedCredentials := TIdDecoderMIME.DecodeString(AuthHeader);

    // Separar usuário e senha com SplitString
    Credentials := SplitString(DecodedCredentials, ':');
    if Length(Credentials) = 2 then
    begin
      Username := Credentials[0];
      Password := Credentials[1];

      // Verificar as credenciais
      if (Username = 'admin') and (Password = 'senha123') then
      begin
        Next;
        Exit;
      end;
    end;
  end;

  Response.Status(401).Send('Unauthorized');
end;

end.

