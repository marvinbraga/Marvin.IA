unit Marvin.Core.IA.Connectionist.MLPClassifier.Clss;

{
  MIT License

  Copyright (c) 2018 Marcus Vinicius D. B. Braga

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}

interface

uses
  { embarcadero }
  System.Generics.Collections,
  { marvin }
  Marvin.Core.IA.Connectionist.Classifier,
  Marvin.Core.IA.Connectionist.MultiLayerPerceptron,
  Marvin.Core.IA.Connectionist.Activation;

type
  TMLPClassifier = class sealed(TInterfacedObject, IClassifier)
  private
    FMlp: IMultiLayerPerceptron;
    FInputData: TList<TDoubleArray>;
    FOutputData: TList<TDoubleArray>;
    { hyper parameters }
    FHiddenLayerSizes: Integer; { Number of neurons in the hidden layer. }
    FActivation: IActivation; { Activation function for the hidden layer. }
    FLearningFile: string;
    FLearning: Double;
    // FSolver: ISolver; { The solver for weight optimization. }
    // FAlpha: Double; { L2 penalty (regularization term) parameter. }
    // FBatchSize: Integer;
    // FLearningRate: ILearningRate;
    // FLearningRateInit: Double;
    // FPowerT: Double;
    FMaxIter: Integer;
    // FIsShuffle: Boolean; { Whether to shuffle samples in each iteration. Only used when solver='sgd' or 'adam'. }
    // FRandomState: Integer;
    // FTolerance: Double;
    // FIsVerbose: Boolean;
    // FIsWarmStart: Boolean;
    FMomentum: Double; { Momentum for gradient descent update. Should be between 0 and 1. Only used when solver='sgd'. }
    // FIsNesterovsMomentum: Boolean;
    // FIsEarlyStopping: Boolean;
    // FValidationFraction: Double;
    // FBetaValue1: Double;
    // FBetaValue2: Double;
    // FEpsilon: Double;
    FEpoch: Integer;
    FCost: Double;
    FIsForceLoad: Boolean;
  protected
    { hiperparâmetros }
    function SetHiddenLayerSizes(const AHiddenLayerSizes: Integer): IClassifier;
    function SetActivation(const AActivation: IActivation): IClassifier;
    function SetLearningFile(const ALearningFile: string): IClassifier;
    function SetLearning(const ALearning: Double): IClassifier;
    function SetMaxIter(const AMaxIter: Integer): IClassifier;
    function SetMomentum(const AMomentum: Double): IClassifier;
    function ConfigureClassifier: IClassifier;
    function Epoch: Integer;
    function Cost: Double;
    { treino }
    function Fit(const ATrainDataInputs: TDoubleArray; const ATrainDataOutputs: TDoubleArray): IClassifier; overload;
    function Fit(const ATrainDataInputs: TList<TDoubleArray>; const ATrainDataOutputs: TList<TDoubleArray>): IClassifier; overload;
    function Fit(const ATrainDataInputs: TList<TList<Double>>; const ATrainDataOutputs: TList<TList<Double>>): IClassifier; overload;
    { teste }
    function Predict(const ATestDataInputs: TDoubleArray; out APredictedDataOutputs: TDoubleArray): IClassifier; overload;
    function Predict(const ATestDataInputs: TList<TDoubleArray>; out APredictedDataOutputs: TList<TDoubleArray>): IClassifier; overload;
    function Predict(const ATestDataInputs: TList<TList<Double>>; out APredictedDataOutputs: TList<TList<Double>>): IClassifier; overload;
  protected
    function InitMlp: TMLPClassifier;
    function InitData: TMLPClassifier;
    function ClearData: TMLPClassifier;
    function SetInputsMinMaxValues(const AMinValue: Double; const AMaxValue: Double): TMLPClassifier;
    function SetOutputsMinMaxValues(const AMinValue: Double; const AMaxValue: Double): TMLPClassifier;
    function GetMinMaxValues(var AMinValue: Double; var AMaxValue: Double): TMLPClassifier;
    function Build(const ATrainDataInputs: TList<TDoubleArray>; const ATrainDataOutputs: TList<TDoubleArray>): TMLPClassifier;
    function Train(const ATrainDataInputs: TList<TDoubleArray>; const ATrainDataOutputs: TList<TDoubleArray>): TMLPClassifier;
    function PrepareMlp: TMLPClassifier;
  public
    constructor Create(const ALearningFile: string = 'Marvin.Neural.Learning.mlp'; const AIsForceLoad: Boolean = False; AHiddenLayerSizes: Integer = 100; ALearning: Double = 0.9; AMomentum: Double = 0.9; AMaxIter: Integer = 200); overload;
    constructor Create(const AActivation: IActivation; const ALearningFile: string = 'Marvin.Neural.Learning.mlp'; const AIsForceLoad: Boolean = False; AHiddenLayerSizes: Integer = 100; ALearning: Double = 0.9; AMomentum: Double = 0.9; AMaxIter: Integer = 200); overload;
    destructor Destroy; override;
    { factory methods }
    class function New(const ALearningFile: string = 'Marvin.Neural.Learning.mlp'; const AIsForceLoad: Boolean = False; AHiddenLayerSizes: Integer = 100; ALearning: Double = 0.9; AMomentum: Double = 0.9; AMaxIter: Integer = 200): IClassifier; overload;
    class function New(const AActivation: IActivation; const ALearningFile: string = 'Marvin.Neural.Learning.mlp'; const AIsForceLoad: Boolean = False; AHiddenLayerSizes: Integer = 100; ALearning: Double = 0.9; AMomentum: Double = 0.9; AMaxIter: Integer = 200): IClassifier; overload;
  end;

implementation

uses
  System.SysUtils,
  Marvin.Core.IA.Connectionist.MultiLayerPerceptron.Clss,
  Marvin.Core.IA.Connectionist.Support.Clss;

{ TMLPClassifier }

constructor TMLPClassifier.Create(const ALearningFile: string; const AIsForceLoad: Boolean; AHiddenLayerSizes: Integer; ALearning, AMomentum: Double; AMaxIter: Integer);
begin
  inherited Create;
  Self.InitMlp;
  { recupera os hiperparâmetros }
  FIsForceLoad := AIsForceLoad;
  FLearningFile := ALearningFile;
  FLearning := ALearning;
  FMomentum := AMomentum;
  FMaxIter := AMaxIter;
  FHiddenLayerSizes := AHiddenLayerSizes;
end;

constructor TMLPClassifier.Create(const AActivation: IActivation; const ALearningFile: string; const AIsForceLoad: Boolean; AHiddenLayerSizes: Integer; ALearning, AMomentum: Double; AMaxIter: Integer);
begin
  Self.Create(ALearningFile, AIsForceLoad, AHiddenLayerSizes, ALearning, AMomentum, AMaxIter);
  { recupera os hiperparâmetros }
  FActivation := AActivation;
end;

class function TMLPClassifier.New(const ALearningFile: string; const AIsForceLoad: Boolean;
  AHiddenLayerSizes: Integer; ALearning, AMomentum: Double;
  AMaxIter: Integer): IClassifier;
begin
  Result := TMLPClassifier.Create(ALearningFile, AIsForceLoad, AHiddenLayerSizes, ALearning, AMomentum, AMaxIter);
end;

class function TMLPClassifier.New(const AActivation: IActivation;
  const ALearningFile: string; const AIsForceLoad: Boolean; AHiddenLayerSizes: Integer; ALearning,
  AMomentum: Double; AMaxIter: Integer): IClassifier;
begin
  Result := TMLPClassifier.Create(AActivation, ALearningFile, AIsForceLoad, AHiddenLayerSizes, ALearning, AMomentum, AMaxIter);
end;

destructor TMLPClassifier.Destroy;
begin
  Self.ClearData;
  inherited;
end;

function TMLPClassifier.Epoch: Integer;
begin
  Result := FEpoch;
end;

function TMLPClassifier.Build(const ATrainDataInputs, ATrainDataOutputs: TList<TDoubleArray>): TMLPClassifier;
var
  LMinValue, LMaxValue: Double;
  LNumberOfInputs: Integer;
  LNumberOfOutputs: Integer;
begin
  Result := Self;
  FMlp.SetLearnFile(FLearningFile);
  { recupera os dados }
  FInputData.AddRange(ATrainDataInputs);
  FOutputData.AddRange(ATrainDataOutputs);
  { verifica a estrutura da rede, inputs e outputs }
  LNumberOfInputs := Length(FInputData[0]);
  LNumberOfOutputs := Length(FOutputData[0]);
  { verifica máximos e mínimos }
  Self.GetMinMaxValues(LMinValue, LMaxValue);
  { neurônios da camada de entrada }
  FMlp.AddLayerConfig(LNumberOfInputs);
  { neurônios da camada escondida }
  if FHiddenLayerSizes > 0 then
  begin
    FMlp.AddLayerConfig(FHiddenLayerSizes);
  end;
  FMlp
    { neurônios da camada de saída }
    .AddLayerConfig(LNumberOfOutputs)
    { constrói a rede }
    .Build;
  { Definição da faixa de trabalho dos neurônios de entrada/saída }
  Self
    .SetInputsMinMaxValues(LMinValue, LMaxValue)
    .SetOutputsMinMaxValues(LMinValue, LMaxValue);
end;

function TMLPClassifier.ClearData: TMLPClassifier;

  procedure LClear(AList: TList<TDoubleArray>);
  begin
    if Assigned(AList) then
    begin
      AList.Clear;
      FreeAndNil(AList);
    end;
  end;

begin
  Result := Self;
  LClear(FInputData);
  LClear(FOutputData);
end;

function TMLPClassifier.InitData: TMLPClassifier;
begin
  Result := Self;
  FInputData := TList<TDoubleArray>.Create;
  FOutputData := TList<TDoubleArray>.Create;
end;

function TMLPClassifier.InitMlp: TMLPClassifier;
begin
  Result := Self;
  FMlp := TMultiLayerPerceptron.New;
end;

function TMLPClassifier.ConfigureClassifier: IClassifier;
begin
  Result := Self;
  { inicializa os dados }
  Self.ClearData.InitData;
  { configura a MLP }
  FMlp
    .Clear
    .SetActivation(FActivation)
    .SetLearnFile(FLearningFile)
    .SetLearning(FLearning)
    .SetMomentum(FMomentum);
end;

function TMLPClassifier.Cost: Double;
begin
  Result := FCost;
end;

function TMLPClassifier.Fit(const ATrainDataInputs, ATrainDataOutputs: TDoubleArray): IClassifier;
begin
  { inicializa a rede }
  Result := Self.ConfigureClassifier;
  { recupera os dados }
  FInputData.AddRange(ATrainDataInputs);
  FOutputData.AddRange(ATrainDataOutputs);
end;

function TMLPClassifier.Fit(const ATrainDataInputs, ATrainDataOutputs: TList<TDoubleArray>): IClassifier;
begin
  { inicializa a rede }
  Result := Self.ConfigureClassifier;
  Self
    .Build(ATrainDataInputs, ATrainDataOutputs)
    .Train(ATrainDataInputs, ATrainDataOutputs);
end;

function TMLPClassifier.Fit(const ATrainDataInputs, ATrainDataOutputs: TList<TList<Double>>): IClassifier;
begin
  { inicializa a rede }
  Result := Self.ConfigureClassifier;

end;

function TMLPClassifier.GetMinMaxValues(var AMinValue, AMaxValue: Double): TMLPClassifier;
var
  LNumberOfInputs: Integer;
  LRecordCount: Integer;
  LIndex: Integer;
  LValue: Double;
begin
  Result := Self;
  { verifica máximos e mínimos }
  AMinValue := 1E9;
  AMaxValue := -1E9;
  LNumberOfInputs := Length(FInputData[0]);
  for LRecordCount := 0 to FInputData.Count - 1 do
  begin
    for LIndex := 0 to LNumberOfInputs - 1 do
    begin
      LValue := TDoubleArray(FInputData[LRecordCount])[LIndex];
      if LValue < AMinValue then
      begin
        AMinValue := LValue;
      end;
      if LValue > AMaxValue then
      begin
        AMaxValue := LValue;
      end;
    end;
  end;
end;

function TMLPClassifier.Predict(const ATestDataInputs: TDoubleArray; out APredictedDataOutputs: TDoubleArray): IClassifier;
begin
  Result := Self;

end;

function TMLPClassifier.Predict(const ATestDataInputs: TList<TDoubleArray>; out APredictedDataOutputs: TList<TDoubleArray>): IClassifier;
var
  LNumberOfOutputs: Integer;
  LIndex, LRecordCount: Integer;
  LPredictedOutput: TDoubleArray;
  LValue: Double;
begin
  Result := Self;
  { prepara a rede }
  Self.PrepareMlp;
  { recupera a quantidade de neurônios  }
  LNumberOfOutputs := FMlp.GetOutputLayer.NeuronsCount;
  { cria a lista de resultados }
  APredictedDataOutputs := TList<TDoubleArray>.Create;
  { percorre os dados para teste }
  for LRecordCount := 0 to ATestDataInputs.Count - 1 do
  begin
    { percorre as entradas }
    for LIndex := 0 to Length(ATestDataInputs[LRecordCount]) - 1 do
    begin
      { recupera o valor para teste }
      LValue := TDoubleArray(ATestDataInputs[LRecordCount])[LIndex];
      { informa o valor à rede }
      FMlp.SetInputValue(LIndex, LValue);
    end;
    { testa }
    FMlp.Test;
    { prepara para devolver os valores de saída }
    LPredictedOutput := nil;
    SetLength(LPredictedOutput, LNumberOfOutputs);
    { percorre a saída da rede }
    for LIndex := 0 to LNumberOfOutputs - 1 do
    begin
      { recupera o valor da rede }
      LPredictedOutput[LIndex] := FMlp.GetOutputValue(LIndex);
    end;
    { adiciona valores de saída }
    APredictedDataOutputs.Add(LPredictedOutput);
  end;
end;

function TMLPClassifier.Predict(const ATestDataInputs: TList<TList<Double>>; out APredictedDataOutputs: TList<TList<Double>>): IClassifier;
begin
  Result := Self;

end;

function TMLPClassifier.PrepareMlp: TMLPClassifier;
var
  LFile: string;
begin
  Result := Self;
  if FIsForceLoad then
  begin
    { se infornou um arquivo de aprendizagem }
    if not FLearningFile.Trim.IsEmpty then
    begin
      LFile := ExtractFilePath(ParamStr(0)) + FLearningFile;
      { se o arquivo existe }
      if FileExists(LFile) then
      begin
        { está com erro na utilização }
        FMlp.SetLearnFile(FLearningFile).Load(TSupportSimpleTextFile.New(FMlp));
      end;
    end;
  end;
end;

function TMLPClassifier.SetActivation(const AActivation: IActivation): IClassifier;
begin
  Result := Self;
  FActivation := AActivation;
end;

function TMLPClassifier.SetHiddenLayerSizes(const AHiddenLayerSizes: Integer): IClassifier;
begin
  Result := Self;
  FHiddenLayerSizes := AHiddenLayerSizes;
end;

function TMLPClassifier.SetInputsMinMaxValues(const AMinValue, AMaxValue: Double): TMLPClassifier;
var
  LIndex, LNumberOfInputs: Integer;
begin
  Result := Self;
  LNumberOfInputs := Length(FInputData[0]);
  for LIndex := 0 to LNumberOfInputs - 1 do
  begin
    FMlp.SetInputMinMaxValues(LIndex, AMinValue, AMaxValue);
  end;
end;

function TMLPClassifier.SetLearning(const ALearning: Double): IClassifier;
begin
  Result := Self;
  FLearning := ALearning;
end;

function TMLPClassifier.SetLearningFile(const ALearningFile: string): IClassifier;
begin
  Result := Self;
  FLearningFile := ALearningFile;
end;

function TMLPClassifier.SetMaxIter(const AMaxIter: Integer): IClassifier;
begin
  Result := Self;
  FMaxIter := AMaxIter;
end;

function TMLPClassifier.SetMomentum(const AMomentum: Double): IClassifier;
begin
  Result := Self;
  FMomentum := AMomentum;
end;

function TMLPClassifier.SetOutputsMinMaxValues(const AMinValue, AMaxValue: Double): TMLPClassifier;
var
  LIndex, LNumberOfOutputs: Integer;
begin
  Result := Self;
  LNumberOfOutputs := FMlp.GetOutputLayer.NeuronsCount;
  for LIndex := 0 to LNumberOfOutputs - 1 do
  begin
    FMlp.SetOutputMinMaxValues(LIndex, AMinValue, AMaxValue);
  end;
end;

function TMLPClassifier.Train(const ATrainDataInputs, ATrainDataOutputs: TList<TDoubleArray>): TMLPClassifier;
var
  LInputIndex, LIndex, LCicle: Integer;
  LNumberOfInputs, LNumberOfOutputs: Integer;
begin
  Result := Self;
   { Treinamento }
  FEpoch := 0; // conta o número de épocas
  FCost := 0;
  LNumberOfInputs := FMlp.GetInputLayer.NeuronsCount;
  LNumberOfOutputs := FMlp.GetOutputLayer.NeuronsCount;
  { Faz para o número de ciclos }
  for LCicle := 0 to FMaxIter - 1 do
  begin
    { O numero de entradas pelos períodos, deslocando-se uma amostra à frente a cada treinamento }
    for LIndex := 0 to FInputData.Count - 1 do
    begin
      LInputIndex := 0;
      { informa as entradas }
      while LInputIndex <= (LNumberOfInputs - 1) do
      begin
        { passa o valor para a rede }
        FMlp.SetInputValue(LInputIndex, TDoubleArray(FInputData[LIndex])[LInputIndex]);
        Inc(LInputIndex);
      end;
      LInputIndex := 0;
      { informa as entradas }
      while LInputIndex <= (LNumberOfOutputs - 1) do
      begin
        { passa o valor para a rede }
        FMlp.SetOutputValue(LInputIndex, TDoubleArray(FOutputData[LIndex])[LInputIndex]);
        Inc(LInputIndex);
      end;
      { executa o treino }
      FMlp.Training;
      Inc(FEpoch);
    end;
    FCost := FMlp.Cost;
  end;

  { Após treinar guarda o conhecimento acumulado pela rede }
  FMlp.Save(TSupportSimpleTextFile.New(FMlp));
end;

end.

