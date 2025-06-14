unit servidor;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  horse,
  Horse.Jhonson,
  Horse.CORS,
  Controllers.usuario;

type
  TFrmMain = class(TForm)
    lblServer: TLabel;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

procedure TFrmMain.FormShow(Sender: TObject);
begin
  Thorse.Use(Jhonson());
  Thorse.Use(Cors);

  Controllers.usuario.RegistrarRotas;

  THorse.Listen(3000);

  lblServer.Caption := 'Servidor rodando na porta: ' + Thorse.Port.ToString;

end;

end.
