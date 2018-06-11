program PoC.IA;

uses
  Vcl.Forms,
  Marvin.PoC.IA.StartUI in 'Marvin.PoC.IA.StartUI.pas' {FormStart},
  Vcl.Themes,
  Vcl.Styles,
  Marvin.PoC.IA.Iris.Clss in 'Marvin.PoC.IA.Iris.Clss.pas',
  Marvin.PoC.IA.Iris in 'Marvin.PoC.IA.Iris.pas',
  Marvin.PoC.IA.DataConverter in 'Marvin.PoC.IA.DataConverter.pas',
  Marvin.PoC.IA.DataConverter.Clss in 'Marvin.PoC.IA.DataConverter.Clss.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Glossy');
  Application.CreateForm(TFormStart, FormStart);
  Application.Run;
end.
