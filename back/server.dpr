program server;

uses
  Vcl.Forms,
  servidor in 'servidor.pas' {FrmMain},
  dbConfig in 'dataModuleGlobal\dbConfig.pas' {configdm: TDataModule},
  controllers.usuario in 'controller\controllers.usuario.pas',
  uMD5 in 'Utils\uMD5.pas',
  TokenMiddleware in 'middleware\TokenMiddleware.pas',
  AuthMiddleware in 'middleware\AuthMiddleware.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.CreateForm(Tconfigdm, configdm);
  Application.Run;
end.
