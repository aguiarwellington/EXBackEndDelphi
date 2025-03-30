unit apiCepService;

interface

uses
  System.SysUtils, System.JSON, RESTRequest4D;

type
  TCepService = class
  public
    function BuscarCEP(const CEP: string): TJSONObject;
  end;

implementation

function TCepService.BuscarCEP(const CEP: string): TJSONObject;
var
  Response: IResponse;
begin
  try
    Response := TRequest.New
      .BaseURL('https://brasilapi.com.br/api/cep/v1/' + CEP)
      .Accept('application/json')
      .Get;

    if Response.StatusCode = 200 then
      Result := TJSONObject.ParseJSONValue(Response.Content) as TJSONObject
    else
      raise Exception.Create('Erro ao buscar CEP: ' + Response.Content);
  except
    on E: Exception do
    begin
      Result := TJSONObject.Create;
      Result.AddPair('status', 'error');
      Result.AddPair('message', 'Erro ao buscar CEP: ' + E.Message);
    end;
  end;
end;

end.

