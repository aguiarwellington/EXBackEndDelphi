unit apiCnpjService;

interface

uses
  System.SysUtils, System.JSON, RESTRequest4D;

type
  TCnpjService = class
  public
    function BuscarCNPJ(const CNPJ: string): TJSONObject;
  end;

implementation

function TCnpjService.BuscarCNPJ(const CNPJ: string): TJSONObject;
var
  Response: IResponse;
begin
  try
    Response := TRequest.New
      .BaseURL('https://brasilapi.com.br/api/cnpj/v1/' + CNPJ)
      .Accept('application/json')
      .Get;

    if Response.StatusCode = 200 then
      Result := TJSONObject.ParseJSONValue(Response.Content) as TJSONObject
    else
      raise Exception.Create('Erro ao buscar CNPJ: ' + Response.Content);
  except
    on E: Exception do
    begin
      Result := TJSONObject.Create;
      Result.AddPair('status', 'error');
      Result.AddPair('message', 'Erro ao buscar CNPJ: ' + E.Message);
    end;
  end;
end;

end.

