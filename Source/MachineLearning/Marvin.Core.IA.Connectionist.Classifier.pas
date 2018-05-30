unit Marvin.Core.IA.Connectionist.Classifier;

interface

uses
  { embarcadero }
  System.Generics.Collections,
  { marvin }
  Marvin.Core.InterfacedList,
  Marvin.Core.IA.Connectionist.Activation;

type
  TDoubleArray = array of Double;

  IClassifier = interface
    ['{FF28CF74-02E0-437A-96A7-F45B404E8439}']
    function ConfigureClassifier: IClassifier;
    function SetHiddenLayerSizes(const AHiddenLayerSizes: Integer): IClassifier;
    function SetActivation(const AActivation: IActivation): IClassifier;
    function SetLearningFile(const ALearningFile: string): IClassifier;
    function SetLearning(const ALearning: Double): IClassifier;
    function SetMaxIter(const AMaxIter: Integer): IClassifier;
    function SetMomentum(const AMomentum: Double): IClassifier;
    function Cost: Double;
    function Epoch: Integer;
    { treino }
    function Fit(const ATrainDataInputs: TDoubleArray; const ATrainDataOutputs: TDoubleArray): IClassifier; overload;
    function Fit(const ATrainDataInputs: TList<TDoubleArray>; const ATrainDataOutputs: TList<TDoubleArray>): IClassifier; overload;
    function Fit(const ATrainDataInputs: TList<TList<Double>>; const ATrainDataOutputs: TList<TList<Double>>): IClassifier; overload;
    { teste }
    function Predict(const ATestDataInputs: TDoubleArray; out APredictedDataOutputs: TDoubleArray): IClassifier; overload;
    function Predict(const ATestDataInputs: TList<TDoubleArray>; out APredictedDataOutputs: TList<TDoubleArray>): IClassifier; overload;
    function Predict(const ATestDataInputs: TList<TList<Double>>; out APredictedDataOutputs: TList<TList<Double>>): IClassifier; overload;
  end;

implementation

end.
