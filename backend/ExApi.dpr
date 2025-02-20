program ExApi;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Horse,
  Horse.Jhonson,
  Horse.CORS,
  controllers.Usuarios in 'src\controllers\controllers.Usuarios.pas',
  FireDAC.Comp.Client,
  uMD5 in 'src\ultils\uMD5.pas',
  AuthMiddleware in 'src\middleware\AuthMiddleware.pas',
  dbConfig in 'src\model\dbConfig.pas' {configdm: TDataModule},
  codeGenerate in 'src\ultils\codeGenerate.pas';

var
  configdm: Tconfigdm;

begin
  try
    THorse.Use(Jhonson());

   HorseCORS
     .AllowedOrigin('*')
     .AllowedCredentials(true)
     .AllowedHeaders('*')
     .AllowedMethods('*')
     .ExposedHeaders('*');

    THorse.Use(CORS);

    controllers.Usuarios.RegistrarRotas;

    //Writeln('Servidor iniciado na porta 3000. Acesse: http://192.168.1.101:3000');

    THorse.Listen(3000);

    Readln;

  except
    on E: Exception do
      Writeln('Erro inesperado: ', E.ClassName, ': ', E.Message);
  end;

  FreeAndNil(configdm);
end.

