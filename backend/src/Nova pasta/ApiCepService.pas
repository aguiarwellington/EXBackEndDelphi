unit ApiCepService;

interface

uses
  System.JSON, IdHTTP, IdSSL, IdSSLOpenSSL, System.SysUtils, System.Classes;

type
  TCepService = class
  public
    function BuscarCEP(const CEP: string): TJSONObject;
  end;

implementation

function TCepService.BuscarCEP(const CEP: string): TJSONObject;
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
    HTTP.IOHandler := SSLHandler;
    HTTP.Request.ContentType := 'application/json';
    HTTP.Request.Accept := 'application/json';

    // URL da API de CEP da BrasilAPI
    API_URL := 'https://brasilapi.com.br/api/cep/v1/' + CEP;

    try
      HTTP.Get(API_URL, Response);
      JSON := TJSONObject.ParseJSONValue(Response.DataString) as TJSONObject;

      if Assigned(JSON) then
        Result := JSON.Clone as TJSONObject;

    except
      on E: Exception do
      begin
        Result.AddPair('status', 'error');
        Result.AddPair('message', 'Erro ao buscar CEP: ' + E.Message);
      end;
    end;

  finally
    Response.Free;
    HTTP.Free;
    SSLHandler.Free;
  end;
end;

end.

