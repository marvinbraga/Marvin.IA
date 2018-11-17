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
  { marvin }
  Marvin.Core.InterfacedList,
  Marvin.Core.IA.Connectionist.Classifier,
  Marvin.Core.IA.Connectionist.MultiLayerPerceptron,
  Marvin.Core.IA.Connectionist.Activation;

type
  TMLPClassifier = class sealed(TInterfacedObject, IClassifier)
  private
    FMlp: IMultiLayerPerceptron;
    FInputData: IList<TDoubleArray>;
    FOutputData: IList<TDoubleArray>;
    { hyper parameters }
    FHiddenLayerCount: Word; { Number of hidden layer. }
    FHiddenLayerNeuronsCount: array of Word; { Number of neurons in the hidden layer. }
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
    FEpochs: Integer;
    FEpochsCovered: Integer;
    FCost: Double;
    FIsForceLoad: Boolean;
  protected
    { hiperparâmetros }
    function SetHiddenLayerSizes(const AHiddenLayerCount: Word; const AHiddenLayerNeuronsCount: array of Word): IClassifier;
    function SetActivation(const AActivation: IActivation): IClassifier;
    function SetLearningFile(const ALearningFile: string): IClassifier;
    function SetLearning(const ALearning: Double): IClassifier;
    function SetMaxIter(const AMaxIter: Integer): IClassifier;
    function SetMomentum(const AMomentum: Double): IClassifier;
    function ConfigureClassifier: IClassifier;
    function Epochs: Integer;
    function EpochsCovered: Integer;
    function Cost: Double;
    { treino }
    function Fit(const ATrainDataInputs: TDoubleArray; const ATrainDataOutputs: TDoubleArray): IClassifier; overload;
    function Fit(const ATrainDataInputs: IList<TDoubleArray>; const ATrainDataOutputs: IList<TDoubleArray>): IClassifier; overload;
    function Fit(const ATrainDataInputs: IList<IList<Double>>; const ATrainDataOutputs: IList<IList<Double>>): IClassifier; overload;
    { teste }
    function Predict(const ATestDataInputs: TDoubleArray; out APredictedDataOutputs: TDoubleArray): IClassifier; overload;
    function Predict(const ATestDataInputs: IList<TDoubleArray>; out APredictedDataOutputs: IList<TDoubleArray>): IClassifier; overload;
    function Predict(const ATestDataInputs: IList<IList<Double>>; out APredictedDataOutputs: IList<IList<Double>>): IClassifier; overload;
  protected
    function InitMlp: TMLPClassifier;
    function InitData: TMLPClassifier;
    function ClearData: TMLPClassifier;
    function SetInputsMinMaxValues(const AMinValue: Double; const AMaxValue: Double): TMLPClassifier;
    function SetOutputsMinMaxValues(const AMinValue: Double; const AMaxValue: Double): TMLPClassifier;
    function GetMinMaxValues(var AMinValue: Double; var AMaxValue: Double): TMLPClassifier;
    function Build(const ATrainDataInputs: IList<TDoubleArray>; const ATrainDataOutputs: IList<TDoubleArray>): TMLPClassifier;
    function Train(const ATrainDataInputs: IList<TDoubleArray>; const ATrainDataOutputs: IList<TDoubleArray>): TMLPClassifier;
    function PrepareMlp: TMLPClassifier;
  public
    constructor Create(const AHiddenLayerNeuronsCount: array of Word; const ALearning: Double = 0.9; const AMomentum: Double = 0.9; const AMaxIter: Integer = 200; const ALearningFile: string = 'Marvin.Neural.Learning.mlp'; const AIsForceLoad: Boolean = False); overload;
    constructor Create(const AActivation: IActivation; const AHiddenLayerNeuronsCount: array of Word; const ALearning: Double = 0.9; const AMomentum: Double = 0.9; const AMaxIter: Integer = 200; const ALearningFile: string = 'Marvin.Neural.Learning.mlp'; const AIsForceLoad: Boolean = False); overload;
    destructor Destroy; override;
    { factory methods }
    class function New(const AHiddenLayerNeuronsCount: array of Word; const ALearning: Double = 0.9; const AMomentum: Double = 0.9; const AMaxIter: Integer = 200; const ALearningFile: string = 'Marvin.Neural.Learning.mlp'; const AIsForceLoad: Boolean = False): IClassifier; overload;
    class function New(const AActivation: IActivation; const AHiddenLayerNeuronsCount: array of Word; const ALearning: Double = 0.9; const AMomentum: Double = 0.9; const AMaxIter: Integer = 200; const ALearningFile: string = 'Marvin.Neural.Learning.mlp'; const AIsForceLoad: Boolean = False): IClassifier; overload;
  end;

implementation

uses
  System.SysUtils,
  Marvin.Core.IA.Connectionist.MultiLayerPerceptron.Clss,
  Marvin.Core.IA.Connectionist.Support.Clss;

{ TMLPClassifier }

constructor TMLPClassifier.Create(const AHiddenLayerNeuronsCount: array of Word;
  const ALearning, AMomentum: Double; const AMaxIter: Integer; const ALearningFile: string; const AIsForceLoad: Boolean);
begin
  inherited Create;
  Self.InitMlp;
  { recupera os hiperparâmetros }
  FIsForceLoad := AIsForceLoad;
  FLearningFile := ALearningFile;
  FLearning := ALearning;
  FMomentum := AMomentum;
  FMaxIter := AMaxIter;
  Self.SetHiddenLayerSizes(Length(AHiddenLayerNeuronsCount), AHiddenLayerNeuronsCount);
end;

constructor TMLPClassifier.Create(const AActivation: IActivation; const AHiddenLayerNeuronsCount: array of Word;
  const ALearning, AMomentum: Double; const AMaxIter: Integer; const ALearningFile: string; const AIsForceLoad: Boolean);
begin
  Self.Create(AHiddenLayerNeuronsCount, ALearning, AMomentum, AMaxIter, ALearningFile, AIsForceLoad);
  { recupera os hiperparâmetros }
  FActivation := AActivation;
end;

class function TMLPClassifier.New(const AHiddenLayerNeuronsCount: array of Word; const ALearning, AMomentum: Double; const AMaxIter: Integer; const ALearningFile: string; const AIsForceLoad: Boolean): IClassifier;
begin
  Result := TMLPClassifier.Create(AHiddenLayerNeuronsCount, ALearning, AMomentum, AMaxIter, ALearningFile, AIsForceLoad);
end;

class function TMLPClassifier.New(const AActivation: IActivation; const AHiddenLayerNeuronsCount: array of Word; const ALearning, AMomentum: Double; const AMaxIter: Integer; const ALearningFile: string; const AIsForceLoad: Boolean): IClassifier;
begin
  Result := TMLPClassifier.Create(AActivation, AHiddenLayerNeuronsCount, ALearning, AMomentum, AMaxIter, ALearningFile, AIsForceLoad);
end;

destructor TMLPClassifier.Destroy;
begin
  Self.ClearData;
  inherited;
end;

function TMLPClassifier.Epochs: Integer;
begin
  Result := FEpochs;
end;

function TMLPClassifier.EpochsCovered: Integer;
begin
  Result := FEpochsCovered;
end;

function TMLPClassifier.Build(const ATrainDataInputs, ATrainDataOutputs: IList<TDoubleArray>): TMLPClassifier;
var
  LMinValue, LMaxValue: Double;
  LNumberOfInputs: Integer;
  LNumberOfOutputs: Integer;
  LIndex: Integer;
begin
  Result := Self;
  FMlp.SetLearnFile(FLearningFile);
  { recupera os dados }
  FInputData.Add(ATrainDataInputs);
  FOutputData.Add(ATrainDataOutputs);
  { verifica a estrutura da rede, inputs e outputs }
  LNumberOfInputs := Length(FInputData.Get(0));
  LNumberOfOutputs := Length(FOutputData.Get(0));
  { verifica máximos e mínimos }
  Self.GetMinMaxValues(LMinValue, LMaxValue);
  { neurônios da camada de entrada }
  FMlp.AddLayerConfig(LNumberOfInputs);
  { neurônios das camadas escondida }
  if FHiddenLayerCount > 0 then
  begin
    for LIndex := 1 to FHiddenLayerCount do
    begin
      FMlp.AddLayerConfig(FHiddenLayerNeuronsCount[LIndex]);
    end;
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

  procedure LClear(AList: IList<TDoubleArray>);
  begin
    if Assigned(AList) then
    begin
      AList.Clear;
      AList := nil;
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
  FInputData := TCustomList<TDoubleArray>.Create;
  FOutputData := TCustomList<TDoubleArray>.Create;
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
  FInputData.Add(ATrainDataInputs);
  FOutputData.Add(ATrainDataOutputs);
end;

function TMLPClassifier.Fit(const ATrainDataInputs, ATrainDataOutputs: IList<TDoubleArray>): IClassifier;
begin
  { inicializa a rede }
  Result := Self.ConfigureClassifier;
  Self
    .Build(ATrainDataInputs, ATrainDataOutputs)
    .Train(ATrainDataInputs, ATrainDataOutputs);
end;

function TMLPClassifier.Fit(const ATrainDataInputs, ATrainDataOutputs: IList<IList<Double>>): IClassifier;
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
  LNumberOfInputs := Length(FInputData.Get(0));
  for LRecordCount := 0 to FInputData.Count - 1 do
  begin
    for LIndex := 0 to LNumberOfInputs - 1 do
    begin
      LValue := TDoubleArray(FInputData.Get(LRecordCount))[LIndex];
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

function TMLPClassifier.Predict(const ATestDataInputs: IList<TDoubleArray>; out APredictedDataOutputs: IList<TDoubleArray>): IClassifier;
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
  APredictedDataOutputs := TCustomList<TDoubleArray>.Create;
  { percorre os dados para teste }
  for LRecordCount := 0 to ATestDataInputs.Count - 1 do
  begin
    { percorre as entradas }
    for LIndex := 0 to Length(ATestDataInputs.Get(LRecordCount)) - 1 do
    begin
      { recupera o valor para teste }
      LValue := TDoubleArray(ATestDataInputs.Get(LRecordCount))[LIndex];
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

function TMLPClassifier.Predict(const ATestDataInputs: IList<IList<Double>>; out APredictedDataOutputs: IList<IList<Double>>): IClassifier;
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

function TMLPClassifier.SetHiddenLayerSizes(const AHiddenLayerCount: Word; const AHiddenLayerNeuronsCount: array of Word): IClassifier;
var
  LIndex: Integer;
begin
  Result := Self;
  FHiddenLayerCount := AHiddenLayerCount;
  SetLength(FHiddenLayerNeuronsCount, Length(AHiddenLayerNeuronsCount));
  for LIndex := Low(AHiddenLayerNeuronsCount) to High(AHiddenLayerNeuronsCount) do
  begin
    FHiddenLayerNeuronsCount[LIndex] := AHiddenLayerNeuronsCount[LIndex];
  end;
end;

function TMLPClassifier.SetInputsMinMaxValues(const AMinValue, AMaxValue: Double): TMLPClassifier;
var
  LIndex, LNumberOfInputs: Integer;
begin
  Result := Self;
  LNumberOfInputs := Length(FInputData.Get(0));
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

function TMLPClassifier.Train(const ATrainDataInputs, ATrainDataOutputs: IList<TDoubleArray>): TMLPClassifier;
var
  LDataIndex, LIndex, LCicle: Integer;
  LNumberOfInputs, LNumberOfOutputs: Integer;
begin
  Result := Self;
   { Treinamento }
  FEpochs := FMaxIter; // conta o número de épocas
  FEpochsCovered := 0;
  FCost := 0;
  LNumberOfInputs := FMlp.GetInputLayer.NeuronsCount;
  LNumberOfOutputs := FMlp.GetOutputLayer.NeuronsCount;
  { Faz para o número de ciclos }
  for LCicle := 0 to FEpochs - 1 do
  begin
    { O numero de entradas pelos períodos, deslocando-se uma amostra à frente a cada treinamento }
    for LIndex := 0 to FInputData.Count - 1 do
    begin
      LDataIndex := 0;
      { informa as entradas }
      while LDataIndex <= (LNumberOfInputs - 1) do
      begin
        { passa o valor para a rede }
        FMlp.SetInputValue(LDataIndex, TDoubleArray(FInputData.Get(LIndex))[LDataIndex]);
        Inc(LDataIndex);
      end;
      LDataIndex := 0;
      { informa as saídas }
      while LDataIndex <= (LNumberOfOutputs - 1) do
      begin
        { passa o valor para a rede }
        FMlp.SetOutputValue(LDataIndex, TDoubleArray(FOutputData.Get(LIndex))[LDataIndex]);
        Inc(LDataIndex);
      end;
      { executa o treino }
      FMlp.Training;
    end;
    Inc(FEpochsCovered);
    FCost := FMlp.Cost;
  end;

  { Após treinar guarda o conhecimento acumulado pela rede }
  FMlp.Save(TSupportSimpleTextFile.New(FMlp));
end;

end.

