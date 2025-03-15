unit ApiCnpjService;

interface

uses
  System.JSON, IdHTTP, IdSSL, IdSSLOpenSSL, System.SysUtils, System.Classes;

type
  TCnpjService = class
  public
    function BuscarCNPJ(const CNPJ: string): TJSONObject;
  end;

implementation

function TCnpjService.BuscarCNPJ(const CNPJ: string): TJSONObject;
var
  HTTP: TIdHTTP;
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
  Response: TStringStream;
  JSON: TJSONObject;
  API_URL: string;
begin
  Result := TJSONObject.Create;
  HTTP := TIdHTTP.Create(nil);
  SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  Response := TStringStream.Create;

  try
    // Configurar SSL para HTTPS (TLS 1.2+)
    SSLHandler.SSLOptions.Method := sslvTLSv1_2; // Forçar uso de TLS 1.2
    SSLHandler.SSLOptions.Mode := sslmClient;
    SSLHandler.SSLOptions.VerifyMode := [];
    SSLHandler.SSLOptions.VerifyDepth := 0;

    HTTP.IOHandler := SSLHandler;
    HTTP.Request.ContentType := 'application/json';
    HTTP.Request.Accept := 'application/json';
    HTTP.Request.UserAgent := 'Mozilla/5.0'; // Simular navegador para evitar bloqueios

    // URL da API de CNPJ da BrasilAPI
    API_URL := 'https://brasilapi.com.br/api/cnpj/v1/' + CNPJ;

    try
      HTTP.Get(API_URL, Response);
      JSON := TJSONObject.ParseJSONValue(Response.DataString) as TJSONObject;

      if Assigned(JSON) then
        Result := JSON.Clone as TJSONObject
      else
        Result.AddPair('status', 'error').AddPair('message', 'Erro ao buscar CNPJ: JSON inválido');

    except
      on E: Exception do
      begin
        Result.AddPair('status', 'error');
        Result.AddPair('message', 'Erro ao buscar CNPJ: ' + E.Message);
      end;
    end;

  finally
    Response.Free;
    HTTP.Free;
    SSLHandler.Free;
  end;
end;


end.

