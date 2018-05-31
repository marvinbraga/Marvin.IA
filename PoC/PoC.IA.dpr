program PoC.IA;

uses
  Vcl.Forms,
  Marvin.PoC.IA.StartUI in 'Marvin.PoC.IA.StartUI.pas' {FormStart},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Glossy');
  Application.CreateForm(TFormStart, FormStart);
  Application.Run;
end.
