program PoC.IA;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  Marvin.PoC.IA.StartUI in 'Marvin.PoC.IA.StartUI.pas' {FormStart},
  Marvin.PoC.IA.Iris.Clss in 'Marvin.PoC.IA.Iris.Clss.pas',
  Marvin.PoC.IA.Iris in 'Marvin.PoC.IA.Iris.pas',
  Marvin.PoC.IA.DataConverter in 'Marvin.PoC.IA.DataConverter.pas',
  Marvin.PoC.IA.DataConverter.Clss in 'Marvin.PoC.IA.DataConverter.Clss.pas';

{$R *.res}

begin
  {$WARNINGS OFF}
  ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
  {$WARNINGS ON}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Glossy');
  Application.CreateForm(TFormStart, FormStart);
  Application.Run;
end.
