unit Marvin.Core.IA.Connectionist.Perceptron.Clss;

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
  Marvin.Core.InterfacedList,
  Marvin.Core.IA.Connectionist.Classifier,
  Marvin.Core.IA.Connectionist.Activation,
  Marvin.Core.IA.Connectionist.Perceptron;

type
  TPerceptron = class sealed(TInterfacedObject, IPerceptron)
  private
    FBias: Double;
    FActivation: IActivation;
    FLearningRate: Double;
    FAttributesCount: Integer;
    FEpochs: Integer;
    FEpochsCovered: Integer;
    FInputs: IList<TDoubleArray>;
    FOutputs: IList<TDoubleArray>;
    FWeights: TDoubleArray;
  private
    function GuardData: TPerceptron;
    function AdjustInputs: TPerceptron;
    function AdjustSynapses: TPerceptron;
  protected
    function Inputs: IList<TDoubleArray>;
    function Outputs: IList<TDoubleArray>;
    function Fit(const AInputs: IList<TDoubleArray>; const AOutputs: IList<TDoubleArray>): IPerceptron;
    function Predict(const AInputs: IList<TDoubleArray>; const AOutputs: IList<TDoubleArray>): IPerceptron;
    function Epochs: Integer;
    function LearningRate: Double;
    function Bias: Double;
    function Weights: TDoubleArray;
    function EpochsCovered: Integer;
  public
    constructor Create(const AActivation: IActivation; const ABias: Double; const ALearningRate: Double = 0.1; AEpochs: Integer = 1000);
    destructor Destroy; override;
    class function New(const AActivation: IActivation; const ABias: Double; const ALearningRate: Double = 0.1; AEpochs: Integer = 1000): IPerceptron;
  end;

implementation

uses
  Marvin.Core.IA.Connectionist.Exceptions.Clss;

{ TPerceptron }

class function TPerceptron.New(const AActivation: IActivation; const ABias: Double; const ALearningRate: Double = 0.1; AEpochs: Integer = 1000): IPerceptron;
begin
  Result := TPerceptron.Create(AActivation, ABias, ALearningRate, AEpochs);
end;

constructor TPerceptron.Create(const AActivation: IActivation; const ABias: Double; const ALearningRate: Double = 0.1; AEpochs: Integer = 1000);
begin
  inherited Create;
  FEpochsCovered := 0;
  FBias := ABias;
  FActivation := AActivation;
  FLearningRate := ALearningRate;
  FEpochs := AEpochs;
  FInputs := TCustomList<TDoubleArray>.Create;
  FOutputs := TCustomList<TDoubleArray>.Create;
end;

destructor TPerceptron.Destroy;
begin
  FInputs := nil;
  FOutputs := nil;
  inherited;
end;

function TPerceptron.Epochs: Integer;
begin
  Result := FEpochs;
end;

function TPerceptron.EpochsCovered: Integer;
begin
  Result:= FEpochsCovered;
end;

function TPerceptron.Inputs: IList<TDoubleArray>;
begin
  Result := FInputs;
end;

function TPerceptron.LearningRate: Double;
begin
  Result:= FLearningRate;
end;

function TPerceptron.Outputs: IList<TDoubleArray>;
begin
  Result := FInputs;
end;

function TPerceptron.Fit(const AInputs: IList<TDoubleArray>; const AOutputs: IList<TDoubleArray>): IPerceptron;
var
  LEpoch, LIndex, LIndexWeight: Integer;
  LError: Boolean;
  LInputData, LOutputData: TDoubleArray;
  LAuxError, LOutput, LActivationPotential: Double;
begin
  Result := Self;
  { inicializa os dados }
  FInputs.Clear;
  FOutputs.Clear;
  FInputs.Add(AInputs);
  FOutputs.Add(AOutputs);

  Self
    { verifica se existem dados }
    .GuardData
    { ajusta as entradas }
    .AdjustInputs
    { ajustas os pesos }
    .AdjustSynapses;

  { inicializa o contador de épocas }
  LEpoch := 0;
  repeat
    { inicializa o controle de erros }
    LError := False;
    LInputData := FInputs.First;
    LOutputData := FOutputs.First;

    while not FInputs.Eof do
    begin
      { inicializa o potencial de ativação }
      LActivationPotential := 0;
      { percorre os atributos }
      for LIndex := 0 to FAttributesCount do
      begin
        { faz o cálculo da saída }
        LActivationPotential := LActivationPotential + FWeights[LIndex] * LInputData[LIndex];
      end;
      { calcula a saída }
      LOutput := FActivation.Execute(LActivationPotential);

      { verifica se a saída da rede é diferente da saída desejada }
      if LOutput <> LOutputData[0] then
      begin
        LError := True;
        { calcula o erro }
        LAuxError := LOutputData[0] - LActivationPotential;
        { faz o ajuste dos pesos para cada elemento da amostra }
        for LIndexWeight := 0 to FAttributesCount do
        begin
          FWeights[LIndexWeight] := FWeights[LIndexWeight] + FLearningRate * LAuxError * LInputData[LIndexWeight];
        end;
      end;

      { recupera um novo dado }
      LInputData := FInputs.MoveNext;
      LOutputData := FOutputs.MoveNext;
    end;
    { adiciona uma época }
    Inc(LEpoch);

  until not(LError) or (LEpoch > FEpochs);
  FEpochsCovered := LEpoch;
end;

function TPerceptron.Predict(const AInputs: IList<TDoubleArray>; const AOutputs: IList<TDoubleArray>): IPerceptron;
var
  LInputData, LOutputData: TDoubleArray;
  LOutput, LActivationPotential: Double;
  LIndex: Integer;
begin
  Result := Self;

  LInputData := AInputs.First;
  while not(AInputs.Eof) do
  begin
    { inicializa atributo }
    Insert(-1, LInputData, 0);
    LActivationPotential := 0;

    for LIndex := 0 to FAttributesCount do
    begin
      { calcula a saída }
      LActivationPotential := LActivationPotential + FWeights[LIndex] * LInputData[LIndex];
    end;
    { recupera a saída }
    LOutput := FActivation.Execute(LActivationPotential);
    { adiciona à lista de saídas }
    LOutputData := nil;
    SetLength(LOutputData, 1);
    LOutputData[0] := LOutput;
    AOutputs.Add(LOutputData);

    { próxima entrada }
    LInputData := AInputs.MoveNext;
  end;
end;

function TPerceptron.Weights: TDoubleArray;
begin
  Result:= FWeights;
end;

function TPerceptron.GuardData: TPerceptron;
begin
  Result := Self;
  { verifica se existem dados de entrada }
  if FInputs.Count = 0 then
  begin
    raise EInputDataIsEmpty.Create;
  end;
  { verifica se existem dados de saída }
  if FInputs.Count = 0 then
  begin
    raise EInputDataIsEmpty.Create;
  end;
  { verifica se existe a mesma quantidade de dados }
  if FInputs.Count <> FOutputs.Count then
  begin
    raise EInconsistentData.Create;
  end;
end;

function TPerceptron.AdjustInputs: TPerceptron;
var
  LData: TDoubleArray;
begin
  Result := Self;
  { recupera o número de atributos }
  LData := FInputs.First;
  FAttributesCount := Length(LData);
  { adicona o -1 em cada amostra }
  while not FInputs.Eof do
  begin
    Insert(-1, LData, 0);
    FInputs.Update(FInputs.Position, LData);
    LData := FInputs.MoveNext;
  end;
end;

function TPerceptron.AdjustSynapses: TPerceptron;
var
  LIndex: Integer;
begin
  Result := Self;

  Randomize;
  { inicaliza as synapses }
  SetLength(FWeights, FAttributesCount + 1);
  { adiciona o bias }
  FWeights[0] := FBias;
  for LIndex := 1 to FAttributesCount do
  begin
    { inicializa os pesos com números aleatórios entre 0 e 1 }
    FWeights[LIndex] := (Random(99) + 1) / 100;
  end;
end;

function TPerceptron.Bias: Double;
begin
  Result:= FBias;
end;

end.
