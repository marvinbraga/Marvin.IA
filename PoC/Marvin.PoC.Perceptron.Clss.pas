unit Marvin.PoC.Perceptron.Clss;

interface

uses
  Marvin.Core.IA.Connectionist.Perceptron,
  Marvin.Core.IA.Connectionist.Perceptron.Clss;

type
  IPocPerceptron = interface
    ['{1F234428-1E19-4BEB-AA46-D903D2F4E668}']
    function Execute: IPocPerceptron;
  end;

  TPocPerceptron = class(TInterfacedObject, IPocPerceptron)
  private
  protected
    function Execute: IPocPerceptron;
  public
    class function New: IPocPerceptron;
  end;

implementation

uses
  Marvin.Core.InterfacedList,
  Marvin.Core.IA.Connectionist.Classifier,
  Marvin.Core.IA.Connectionist.Activation,
  Marvin.Core.IA.Connectionist.ActivateFunction.Clss;

{ TPocPerceptron }

class function TPocPerceptron.New: IPocPerceptron;
begin
  Result := TPocPerceptron.Create;
end;

function TPocPerceptron.Execute: IPocPerceptron;
var
  LInputTrein, LOutputTrein: IList<TDoubleArray>;
  LInputTest, LOutputTest: IList<TDoubleArray>;
  LPerceptron: IPerceptron;
begin
  { cria as listas de dados }
  LInputTrein := TCustomList<TDoubleArray>.Create;
  LOutputTrein := TCustomList<TDoubleArray>.Create;
  LInputTest := TCustomList<TDoubleArray>.Create;
  LOutputTest := TCustomList<TDoubleArray>.Create;
  { recebe os dados }
  LInputTrein
    .Add([0.72, 0.82]).Add([0.91, -0.69]).Add([0.46, 0.80]).Add([0.03, 0.93]).Add([0.12, 0.25])
    .Add([0.96, 0.47]).Add([0.8, -0.75]).Add([0.46, 0.98]).Add([0.66, 0.24]).Add([0.72, -0.15])
    .Add([0.35, 0.01]).Add([-0.16, 0.84]).Add([-0.04, 0.68]).Add([-0.11, 0.1]).Add([0.31, -0.96])
    .Add([0.0, -0.26]).Add([-0.43, -0.65]).Add([0.57, -0.97]).Add([-0.47, -0.03]).Add([-0.72, -0.64])
    .Add([-0.57, 0.15]).Add([-0.25, -0.43]).Add([0.47, -0.88]).Add([-0.12, -0.9]).Add([-0.58, 0.62])
    .Add([-0.48, 0.05]).Add([-0.79, -0.92]).Add([-0.42, -0.09]).Add([-0.76, 0.65]).Add([-0.77, -0.]);

  LOutputTrein
    .Add([-1]).Add([-1]).Add([-1]).Add([-1]).Add([-1])
    .Add([-1]).Add([-1]).Add([-1]).Add([-1]).Add([-1])
    .Add([-1]).Add([-1]).Add([-1]).Add([1]).Add([1])
    .Add([1]).Add([1]).Add([1]).Add([1]).Add([1])
    .Add([1]).Add([1]).Add([1]).Add([1]).Add([1])
    .Add([1]).Add([1]).Add([1]).Add([1]).Add([1]);

  LInputTest.Add([0.46, 0.80]);

  LPerceptron := TPerceptron.New(TSignalActivation.New, -1);
  LPerceptron.Fit(LInputTrein, LOutputTrein);
  LPerceptron.Predict(LInputTest, LOutputTest);

  Writeln(LOutputTest.First.ToString);
  readln;
end;

end.

