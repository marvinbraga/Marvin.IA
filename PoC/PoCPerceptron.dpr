program PoCPerceptron;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Marvin.PoC.Perceptron.Clss in 'Marvin.PoC.Perceptron.Clss.pas';

begin
  try
    TPocPerceptron.New.Execute;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
