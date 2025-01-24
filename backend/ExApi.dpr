program ExApi;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  controllers.Usuarios in 'src\controllers\controllers.Usuarios.pas',
  horse,
  Horse.Jhonson,
  Horse.CORS,
  FireDAC.DApt,
  FireDAC.Comp.Client,
  uMD5 in 'src\ultils\uMD5.pas',
  AuthMiddleware in 'src\middleware\AuthMiddleware.pas',
  dbConfig in 'src\model\dbConfig.pas' {configdm: TDataModule};

var
  configdm: Tconfigdm;

begin
  try
    Thorse.Use(Jhonson());
    Thorse.Use(Cors);

    controllers.Usuarios.RegistrarRotas;

    THorse.Listen(3000);
    Writeln('Servidor iniciado. Pressione Enter para finalizar.');

    Readln;

  except
    on E: Exception do
      Writeln('Erro inesperado: ', E.ClassName, ': ', E.Message);
  end;

  FreeAndNil(configdm);
end.

