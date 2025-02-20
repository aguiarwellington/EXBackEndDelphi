unit codeGenerate;

interface

uses
 System.SysUtils,System.IOUtils, System.Classes  ,FMX.Dialogs;

 type
 TcodeGenerate = class
   public
    class function GerarCodigo: string;
 end;


implementation

{ TcodeGenerate }

class function TcodeGenerate.GerarCodigo: string;
var
  i : integer;
  codigo : string;
begin

  for I := 1 to 6 do
    codigo := codigo + intToStr(Random(10));

  Result := codigo;
end;

end.
