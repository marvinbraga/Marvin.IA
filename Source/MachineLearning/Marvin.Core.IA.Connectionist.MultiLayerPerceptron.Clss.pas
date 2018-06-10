unit Marvin.Core.IA.Connectionist.MultiLayerPerceptron.Clss;

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
  Marvin.Core.InterfacedList,
  Marvin.Core.IA.Connectionist.Activation,
  Marvin.Core.IA.Connectionist.MultiLayerPerceptron,
  Marvin.Core.IA.Connectionist.Neuron,
  Marvin.Core.IA.Connectionist.Layer,
  Marvin.Core.IA.Connectionist.Layers,
  Marvin.Core.IA.Connectionist.Support;

type
  TMultiLayerPerceptron = class(TInterfacedObject, IMultiLayerPerceptron)
  strict private
    FLearnFile: string;
    FIsRead: Boolean;
    FIsOnLoad: Boolean;
    FGama: Double;
    FMomentum: Double;
    FLearning: Double;
    FLayersConfig: TList<Word>;
    FLayers: IList<ILayer>;
    FActivation: IActivation;
  protected
    { inicializa as camadas de acordo com as informações da configuração }
    function InitLayers: TMultiLayerPerceptron;
    { inicializa as sinapses }
    function InitSynapses: TMultiLayerPerceptron;
    { atualiza o IsOnLoad }
    function SetIsOnLoad(const AIsOnLoad: Boolean = True): TMultiLayerPerceptron;
    { verifica se está processando uma carga de dados }
    function IsOnLoad: Boolean;
    { ajusta o controle de MLP pronta }
    function SetIsRead(const AIsRead: Boolean = True): TMultiLayerPerceptron;
    { verifica se a MLP está pronta }
    function IsRead: Boolean;
    { ativação sigmóide }
    function Activate(const AValue: Double; const AActivation: IActivation): Double;
    { para ver a quantidade de pesos zerados }
    function NullWeightsCount: LongInt;
    { avança na rede calculando as saídas }
    function FeedForward: TMultiLayerPerceptron;
    { calcula deltas após feedforward }
    function BackPropagation: TMultiLayerPerceptron;
    function BackPropagationInOutputLayer: TMultiLayerPerceptron;
    function BackPropagationInSynapses: TMultiLayerPerceptron;
    function BackPropagationInNeurons: TMultiLayerPerceptron;
    { corrige pesos após backpropagation }
    function CorrectWeight: TMultiLayerPerceptron;
    function CorrectWeightPruning: TMultiLayerPerceptron;
    { manutenção }
    function Save(const ASupportMultiLayerPerceptron: ISupportMultiLayerPerceptron): IMultiLayerPerceptron;
    function Load(const ASupportMultiLayerPerceptron: ISupportMultiLayerPerceptron): IMultiLayerPerceptron;
    { Atribui a função de ativação }
    function SetActivation(const AActivation: IActivation): IMultiLayerPerceptron;
    { informa os valores para os neurônios da entrada }
    function SetInput(const ANeuronIndex: Integer; const AValue: Double; const AMinValue: Double; const AMaxValue: Double): IMultiLayerPerceptron; overload;
    function SetInput(const ANeuron: INeuron; const AValue: Double; const AMinValue: Double; const AMaxValue: Double): IMultiLayerPerceptron; overload;
    function SetInputValue(const ANeuronIndex: Integer; const AValue: Double): IMultiLayerPerceptron; overload;
    function SetInputValue(const ANeuron: INeuron; const AValue: Double): IMultiLayerPerceptron; overload;
    function SetInputMaxValue(const ANeuronIndex: Integer; const AMaxValue: Double): IMultiLayerPerceptron; overload;
    function SetInputMinValue(const ANeuronIndex: Integer; const AMinValue: Double): IMultiLayerPerceptron; overload;
    function SetInputMinMaxValues(const ANeuronIndex: Integer; const AMinValue: Double; const AMaxValue: Double): IMultiLayerPerceptron; overload;
    { informa os valores para os neurônios da saída }
    function SetOutput(const ANeuronIndex: Integer; const AValue: Double; const AMinValue: Double; const AMaxValue: Double): IMultiLayerPerceptron; overload;
    function SetOutput(const ANeuron: INeuron; const AValue: Double; const AMinValue: Double; const AMaxValue: Double): IMultiLayerPerceptron; overload;
    function SetOutputValue(const ANeuronIndex: Integer; const AValue: Double): IMultiLayerPerceptron; overload;
    function SetOutputValue(const ANeuron: INeuron; const AValue: Double): IMultiLayerPerceptron; overload;
    function SetOutputMaxValue(const ANeuronIndex: Integer; const AMaxValue: Double): IMultiLayerPerceptron; overload;
    function SetOutputMinValue(const ANeuronIndex: Integer; const AMinValue: Double): IMultiLayerPerceptron; overload;
    function SetOutputMinMaxValues(const ANeuronIndex: Integer; const AMinValue: Double; const AMaxValue: Double): IMultiLayerPerceptron; overload;
    { valores máximos e mínimos para o neurônio }
    function SetNeuronMaxValue(const ANeuron: INeuron; const AMaxValue: Double): IMultiLayerPerceptron; overload;
    function SetNeuronMinValue(const ANeuron: INeuron; const AMinValue: Double): IMultiLayerPerceptron; overload;
    { inicializa a sinapse com valores importados }
    function InitSynapse(const ALayer: ILayer; const ASynapseId: string; const ASynapseValue: Double; const ANeuronId: string; const ALinkedNeuronId: string): IMultiLayerPerceptron;
    { recupera os layers }
    function Layers: IList<ILayer>;
    { configuração dos layers - quantidade de neurônios que tem em cada camada }
    function LayersConfig: TList<Word>;
    { pega valores de saída }
    function GetOutputValue(const ANeuronIndex: Integer): Double; overload;
    function GetOutputValue(const ANeuron: INeuron): Double; overload;
    { recupera a camada de entrada }
    function GetInputLayer: ILayer;
    { recupera a camada de saída }
    function GetOutputLayer: ILayer;
    { limpa os dados da MLP }
    function Clear: IMultiLayerPerceptron;
    { atualiza o gama }
    function SetGama(const AGama: Double = 0): IMultiLayerPerceptron;
    { configura uma nova camada informando sua quantidade de neurônios }
    function AddLayerConfig(const ANeuronCount: Word): IMultiLayerPerceptron;
    { inicializa as sinapses }
    function ClearSynapses: IMultiLayerPerceptron;
    { constrói a rede para iniciar }
    function Build: IMultiLayerPerceptron;
    { executa uma época de treinamento da rede }
    function Training: IMultiLayerPerceptron;
    function TrainingPruning(const AEpocs: Longint): Longint;
    { Valores para teste de valores de entrada }
    function Test: IMultiLayerPerceptron;
    { retorna a função custo, ou seja, o erro atual }
    function Cost: Double;
    { elimina os pesos zerados }
    function Purge: IMultiLayerPerceptron;
    { atualiza o momento }
    function SetMomentum(const AMomentum: Double = 0.5): IMultiLayerPerceptron;
    { atualiza o aprendizado }
    function SetLearning(const ALearning: Double = 0.9): IMultiLayerPerceptron;
    { atualiza o aprendizado }
    function SetLearnFile(const ALearnFile: string = ''): IMultiLayerPerceptron;
    { momento para atualização dos pesos sinápticos }
    function Momentum: Double;
    { aprendizado para atualização dos pesos sinápticos }
    function Learning: Double;
    { nome do arquivo de conhecimento }
    function LearnFile: string;
    { recupera o gama }
    function Gama: Double;
  public
    constructor Create; reintroduce; virtual;
    class function New: IMultiLayerPerceptron;
    destructor Destroy; override;
  end;

implementation

uses
  { embarcadero }
  System.SysUtils,
  System.Inifiles,
  System.Classes,
  { marvin }
  Marvin.Core.IA.Connectionist.Layer.Clss,
  Marvin.Core.IA.Connectionist.Layers.Clss,
  Marvin.Core.IA.Connectionist.LayerType.Clss,
  Marvin.Core.IA.Connectionist.Exceptions.Clss,
  Marvin.Core.IA.Connectionist.ActivateFunction.Clss,
  Marvin.Core.IA.Connectionist.Cost.Factory.Clss,
  Marvin.Core.IA.Connectionist.ValueConvertion.Clss;

{ TMultiLayerPerceptron }

function TMultiLayerPerceptron.Activate(const AValue: Double; const AActivation: IActivation): Double;
begin
  Result := AActivation.Execute(AValue);
end;

function TMultiLayerPerceptron.AddLayerConfig(const ANeuronCount: Word): IMultiLayerPerceptron;
begin
  Result := Self;
  if ANeuronCount > 0 then
  begin
    FLayersConfig.Add(ANeuronCount);
  end;
end;

function TMultiLayerPerceptron.BackPropagation: TMultiLayerPerceptron;
begin
  Result := Self
    .BackPropagationInOutputLayer
    .BackPropagationInSynapses
    .BackPropagationInNeurons;
end;

function TMultiLayerPerceptron.BackPropagationInOutputLayer: TMultiLayerPerceptron;
var
  LLayer: ILayer;
  LIndexNeuron: Integer;
begin
  Result := Self;
  { com a camada de saída }
  LLayer := Self.GetOutputLayer;
  for LIndexNeuron := 0 to LLayer.Neurons.Count - 1 do
  begin
    { com cada neurônio }
    with LLayer.Neurons.Get(LIndexNeuron) do
    begin
      { calcula deltas }
      SetDelta(Value * (1 - Value) * (Target - Value));
    end;
  end;
end;

function TMultiLayerPerceptron.BackPropagationInSynapses: TMultiLayerPerceptron;
var
  LPriorLayer: ILayer;
  LIndex, LIndexSynapse: Integer;
begin
  Result := Self;
  { percorre todas as camadas escondidas de tráz para a frente }
  for LIndex := FLayers.Count - 2 downto 1 do
  begin
    LPriorLayer := FLayers.Get(LIndex);
    { percorre todas as sinapses }
    for LIndexSynapse := 0 to LPriorLayer.Synapses.Count - 1 do
    begin
      { com cada sinapse }
      with LPriorLayer.Synapses.Get(LIndexSynapse) do
      begin
        { calcula deltas para camadas escondidas }
        Neuron.SetDelta(Neuron.Delta + Value * LinkedNeuron.Delta);
      end;
    end;
  end;
end;

function TMultiLayerPerceptron.BackPropagationInNeurons: TMultiLayerPerceptron;
var
  LLayer, LPriorLayer: ILayer;
  LIndex, LIndexNeuron: Integer;
begin
  Result := Self;
  { percorre todas as camadas escondidas de tras para a frente }
  for LIndex := FLayers.Count - 2 downto 1 do
  begin
    LPriorLayer := FLayers.Get(LIndex);
    LLayer := FLayers.Get(LIndex + 1);
    { percorre todos os neurônios da camada anterior }
    for LIndexNeuron := 0 to LPriorLayer.Neurons.Count - 1 do
    begin
      { com cada neurônio }
      with LPriorLayer.Neurons.Get(LIndexNeuron) do
      begin
        { normaliza deltas para camadas escondidas }
        SetDelta(Value * (1 - Value) * Delta / LLayer.NeuronsCount);
      end;
    end;
  end;
end;

function TMultiLayerPerceptron.Build: IMultiLayerPerceptron;
begin
  Result := Self;
  { somente se a MLP não estiver preparada }
  if not Self.IsRead then
  begin
    { inicializa as camadas }
    Self
      .InitLayers
      .InitSynapses
      .SetIsRead;
  end;
end;

function TMultiLayerPerceptron.Clear: IMultiLayerPerceptron;
begin
  Result := Self;
  { limpa as configurações }
  FLayersConfig.Clear;
  { limpa as camadas }
  FLayers := nil;
  { inicializa os campos }
  Self
    .SetIsRead(False)
    .SetGama
    .SetMomentum
    .SetLearning
    .SetLearnFile;
end;

function TMultiLayerPerceptron.ClearSynapses: IMultiLayerPerceptron;
var
  LCount: Integer;
begin
  for LCount := 0 to FLayers.Count - 1 do
  begin
    FLayers.Get(LCount).InitSynapses;
  end;
end;

function TMultiLayerPerceptron.CorrectWeight: TMultiLayerPerceptron;
var
  LLayer: ILayer;
  LIndex, LIndexSynapse: Integer;
begin
  Result := Self;
  { percorre todas as camadas }
  for LIndex := 0 to FLayers.Count - 1 do
  begin
    LLayer := FLayers.Get(LIndex);
    { percorre todas as sinapses }
    for LIndexSynapse := 0 to LLayer.Synapses.Count - 1 do
    begin
      { com cada sinapse }
      with LLayer.Synapses.Get(LIndexSynapse) do
      begin
        { corrige os pesos }
        SetDelta((FMomentum * Delta) - (FLearning * LinkedNeuron.Delta * Neuron.Value))
        { atualiza as sinapses }
        .SetValue(Value - Delta);
      end;
    end;
  end;
end;

function TMultiLayerPerceptron.CorrectWeightPruning: TMultiLayerPerceptron;
var
  LLayer: ILayer;
  LIndex, LIndexSynapse: Integer;
  LSquare: Double;
begin
  Result := Self;
  { percorre todas as camadas }
  for LIndex := 0 to FLayers.Count - 1 do
  begin
    LLayer := FLayers.Get(LIndex);
    { percorre todas as sinapses }
    for LIndexSynapse := 0 to LLayer.Synapses.Count - 1 do
    begin
      { com cada sinapse }
      with LLayer.Synapses.Get(LIndexSynapse) do
      begin
        { corrige os pesos }
        LSquare := Sqr(Value);
        SetValue(Value * (1 - (FGama / ((1 + LSquare) * (1 + LSquare)))))
          .SetDelta((0.9 * Delta) - (LinkedNeuron.Delta * Neuron.Value))
          { atualiza as sinapses }
          .SetValue(Value - Delta);
      end;
    end;
  end;
end;

function TMultiLayerPerceptron.Cost: Double;
begin
  { com a camada de saída, seleciona o tipo da função de custo }
  Result := TCostType.Default.New(Self.GetOutputLayer).Calculate;
end;

constructor TMultiLayerPerceptron.Create;
begin
  inherited Create;
  FLayersConfig := TList<Word>.Create;
  Self.Clear;
end;

destructor TMultiLayerPerceptron.Destroy;
begin
  FLayers := nil;
  FreeAndNil(FLayersConfig);
  inherited;
end;

function TMultiLayerPerceptron.FeedForward: TMultiLayerPerceptron;
var
  LLayer, LNextLayer: ILayer;
  LIndexLayers, LIndex: Integer;
begin
  Result := Self;
  { percorre todas as camadas, exceto a camada de saída }
  for LIndexLayers := 0 to FLayers.Count - 2 do
  begin
    LLayer := FLayers.Get(LIndexLayers);
    LNextLayer := FLayers.Get(LIndexLayers + 1);
    { percorre todas as sinapses da camada }
    for LIndex := 0 to LLayer.Synapses.Count - 1 do
    begin
      { recupera a sinapse }
      with LLayer.Synapses.Get(LIndex) do
      begin
        LinkedNeuron.SetValue(LinkedNeuron.Value + Neuron.Value * Value);
      end;
    end;
    { percorre todos os neurônios da camada posterior }
    for LIndex := 0 to LNextLayer.Neurons.Count - 1 do
    begin
      { recupera o neurônio }
      with LNextLayer.Neurons.Get(LIndex) do
      begin
        if not(Assigned(FActivation)) then
        begin
          { executa a ativação com função sigmóide }
          FActivation := TSigmoidActivation.New;
        end;
        SetValue(Self.Activate(Value / LLayer.NeuronsCount, FActivation));
      end;
    end;
  end;
end;

function TMultiLayerPerceptron.Gama: Double;
begin
  Result := FGama;
end;

function TMultiLayerPerceptron.GetInputLayer: ILayer;
begin
  Result := FLayers.First;
end;

function TMultiLayerPerceptron.GetOutputLayer: ILayer;
begin
  Result := FLayers.Last;
end;

function TMultiLayerPerceptron.GetOutputValue(const ANeuronIndex: Integer): Double;
begin
  Result := Self.GetOutputValue(Self.GetOutputLayer.Neurons.Get(ANeuronIndex));
end;

function TMultiLayerPerceptron.GetOutputValue(const ANeuron: INeuron): Double;
begin
  with ANeuron do
  begin
    Result := PositiveOutput.New(Value, MaxValue, MinValue).Convert; { [0, 1] }
  end;
end;

function TMultiLayerPerceptron.InitLayers: TMultiLayerPerceptron;
var
  LLayerType: TLayerType;
  LIndex: Integer;
begin
  Result := Self;
  { inicializa a lista de camadas }
  FLayers := TFactoryLayers.New;
  { cria as camadas de acordo com a configuração }
  LLayerType := InputLayer;
  for LIndex := 0 to FLayersConfig.Count - 1 do
  begin
    { configura a camada de saída }
    if LIndex = FLayersConfig.Count - 1 then
    begin
      LLayerType := OutputLayer;
    end;
    { cria uma nova camada }
    FLayers.Add(TFactoryLayer.New(LLayerType, LIndex, FLayersConfig.Items[LIndex]));
    { configura as camadas escondidas }
    LLayerType := HiddenLayer;
  end;
end;

function TMultiLayerPerceptron.InitSynapse(const ALayer: ILayer; const ASynapseId: string; const ASynapseValue: Double; const ANeuronId: string; const ALinkedNeuronId: string): IMultiLayerPerceptron;
var
  LNextLayer: ILayer;
  LNeuron, LLinkedNeuron: INeuron;

  function LFindNeuron(const AParamLayer: ILayer; const AId: string): INeuron;
  var
    LTempNeuron: INeuron;
    LCount: Integer;
  begin
    Result := nil;
    for LCount := 0 to AParamLayer.Neurons.Count - 1 do
    begin
      LTempNeuron := AParamLayer.Neurons.Get(LCount);
      if LTempNeuron.Id = AId.Trim then
      begin
        Result := LTempNeuron;
        Break;
      end;
    end;
    if not Assigned(Result) then
    begin
      raise ENeuronIsNil.Create;
    end;
  end;

begin
  Result := Self;
  { recupera a próxima camada }
  LNextLayer := FLayers.Get(ALayer.Index + 1);
  { procura o neurônio pelo Id }
  LNeuron := LFindNeuron(ALayer, ANeuronId);
  LLinkedNeuron := LFindNeuron(LNextLayer, ALinkedNeuronId);
  { cria a sinapse }
  ALayer.NewSynapse(LNeuron, LLinkedNeuron, ASynapseValue);
end;

function TMultiLayerPerceptron.InitSynapses: TMultiLayerPerceptron;
var
  LLayer, LNextLayer: ILayer;
  LNeuron, LLinkedNeuron: INeuron;
  LIndex, LCount, LNextCount: Integer;
begin
  Result := Self;
  { inicializa contadores randômicos para instanciar as sinapses }
  Randomize;
  if not Self.IsOnLoad then
  begin
    { não percorre a camada de saída }
    for LIndex := 0 to FLayers.Count - 2 do
    begin
      { recupera as camadas }
      LLayer := FLayers.Get(LIndex);
      LNextLayer := FLayers.Get(LIndex + 1);
      { percorre as camadas }
      for LCount := 0 to LLayer.Neurons.Count - 1 do
      begin
        LNeuron := LLayer.Neurons.Get(LCount);
        for LNextCount := 0 to LNextLayer.Neurons.Count - 1 do
        begin
          LLinkedNeuron := LNextLayer.Neurons.Get(LNextCount);
          { cria a sinapse e adiciona à camada  }
          LLayer.NewSynapse(LNeuron, LLinkedNeuron);
        end;
      end;
      LLayer.InitSynapses;
    end;
  end;
end;

function TMultiLayerPerceptron.IsOnLoad: Boolean;
begin
  Result := FIsOnLoad;
end;

function TMultiLayerPerceptron.IsRead: Boolean;
begin
  Result := FIsRead;
end;

function TMultiLayerPerceptron.Layers: IList<ILayer>;
begin
  Result := FLayers;
end;

function TMultiLayerPerceptron.LayersConfig: TList<Word>;
begin
  Result := FLayersConfig;
end;

function TMultiLayerPerceptron.LearnFile: string;
begin
  Result := FLearnFile;
end;

function TMultiLayerPerceptron.Learning: Double;
begin
  Result := FLearning;
end;

function TMultiLayerPerceptron.Load(const ASupportMultiLayerPerceptron: ISupportMultiLayerPerceptron): IMultiLayerPerceptron;
begin
  Result := Self;
  FIsOnLoad := True;
  try
    ASupportMultiLayerPerceptron.Load;
  finally
    FIsOnLoad := False;
  end;
end;

function TMultiLayerPerceptron.Momentum: Double;
begin
  Result := FMomentum;
end;

class function TMultiLayerPerceptron.New: IMultiLayerPerceptron;
begin
  Result := TMultiLayerPerceptron.Create;
end;

function TMultiLayerPerceptron.NullWeightsCount: LongInt;
var
  LLayer: ILayer;
  LCount, LIndex: Integer;
  LCountLayer: Integer;
begin
  LCount := 0;
  { percorre todas as camadas }
  for LCountLayer := 0 to FLayers.Count - 1 do
  begin
    LLayer := FLayers.Get(LCountLayer);
    { percorre todas as sinapses }
    for LIndex := 0 to LLayer.Synapses.Count - 1 do
    begin
      { com cada sinapse }
      with LLayer.Synapses.Get(LIndex) do
      begin
        { verifica se está com valor igual a zero }
        if Trunc(Value * 100) = 0 then
        begin
          Inc(LCount);
        end;
      end;
    end;
  end;
  Result := LCount;
end;

function TMultiLayerPerceptron.Purge: IMultiLayerPerceptron;
var
  LLayer: ILayer;
  LIndex, LCount: Integer;
begin
  Result := Self;
  LIndex := 0;
  { percorre todas as camadas }
  for LCount := 0 to FLayers.Count - 1 do
  begin
    LLayer := FLayers.Get(LCount);
    { percorre todas as sinapses }
    while LIndex < LLayer.Synapses.Count do
    begin
      { com cada sinapse }
      with LLayer.Synapses.Get(LIndex) do
      begin
        Inc(LIndex);
        if Trunc(Value * 100) = 0 then
        begin
          Dec(LIndex);
          LLayer.Synapses.Delete(LIndex);
        end;
      end;
    end;
  end;
end;

function TMultiLayerPerceptron.SetInput(const ANeuronIndex: Integer; const AValue: Double; const AMinValue: Double; const AMaxValue: Double): IMultiLayerPerceptron;
begin
  Result := Self.SetInput(Self.GetInputLayer.Neurons.Get(ANeuronIndex), AValue, AMinValue, AMaxValue);
end;

function TMultiLayerPerceptron.Save(const ASupportMultiLayerPerceptron: ISupportMultiLayerPerceptron): IMultiLayerPerceptron;
begin
  Result := Self;
  ASupportMultiLayerPerceptron.Save;
end;

function TMultiLayerPerceptron.SetInput(const ANeuron: INeuron; const AValue: Double; const AMinValue: Double; const AMaxValue: Double): IMultiLayerPerceptron;
begin
  Result := Self;
  { passa o valor máximo com a entrada já transformada }
  Self
    .SetNeuronMinValue(ANeuron, AMinValue)
    .SetNeuronMaxValue(ANeuron, AMaxValue)
    .SetInputValue(ANeuron, AValue);
end;

function TMultiLayerPerceptron.SetNeuronMaxValue(const ANeuron: INeuron; const AMaxValue: Double): IMultiLayerPerceptron;
begin
  Result := Self;
  ANeuron.SetMaxValue(AMaxValue);
end;

function TMultiLayerPerceptron.SetInputMinMaxValues(const ANeuronIndex: Integer; const AMinValue, AMaxValue: Double): IMultiLayerPerceptron;
begin
  Result := Self;
  Self.GetInputLayer.Neurons.Get(ANeuronIndex).SetMinValue(AMinValue).SetMaxValue(AMaxValue);
end;

function TMultiLayerPerceptron.SetInputMinValue(const ANeuronIndex: Integer; const AMinValue: Double): IMultiLayerPerceptron;
begin
  Result := Self.SetNeuronMinValue(Self.GetInputLayer.Neurons.Get(ANeuronIndex), AMinValue);
end;

function TMultiLayerPerceptron.SetNeuronMinValue(const ANeuron: INeuron; const AMinValue: Double): IMultiLayerPerceptron;
begin
  Result := Self;
  ANeuron.SetMinValue(AMinValue);
end;

function TMultiLayerPerceptron.Test: IMultiLayerPerceptron;
begin
  { faz somente a passagem dos valores de entrada pelos pesos sinápticos }
  Result := Self.FeedForward;
end;

function TMultiLayerPerceptron.Training: IMultiLayerPerceptron;
begin
  { Presume que os valores de entrada já estejam colocados, junto com os alvos.
    Executa o processo somente uma vez. }
  Result := Self
    .FeedForward
    .BackPropagation
    .CorrectWeight;
end;

function TMultiLayerPerceptron.TrainingPruning(const AEpocs: Longint): Longint;
begin
  { Presume que os valores de entrada já estejam colocados, junto com os alvos.
    Executa o processo somente uma vez. }
  Result := TMultiLayerPerceptron(Self
    .FeedForward
    .BackPropagation
    .SetGama(Self.Gama + (0.1 / AEpocs)))
    .CorrectWeightPruning
    { conta quantas sinapses foram zeradas pelo processo }
    .NullWeightsCount;
end;

function TMultiLayerPerceptron.SetInputMaxValue(const ANeuronIndex: Integer; const AMaxValue: Double): IMultiLayerPerceptron;
begin
  Result := Self.SetNeuronMaxValue(Self.GetInputLayer.Neurons.Get(ANeuronIndex), AMaxValue);
end;

function TMultiLayerPerceptron.SetInputValue(const ANeuron: INeuron; const AValue: Double): IMultiLayerPerceptron;
begin
  Result := Self;
  with ANeuron do
  begin
    SetValue(NegativeInput.New(AValue, MaxValue, MinValue).Convert { [-1,1] });
  end;
end;

function TMultiLayerPerceptron.SetOutput(const ANeuronIndex: Integer; const AValue, AMinValue, AMaxValue: Double): IMultiLayerPerceptron;
begin
  Result := Self.SetOutput(Self.GetOutputLayer.Neurons.Get(ANeuronIndex), AValue, AMinValue, AMaxValue);
end;

function TMultiLayerPerceptron.SetOutput(const ANeuron: INeuron; const AValue, AMinValue, AMaxValue: Double): IMultiLayerPerceptron;
begin
  Result := Self;
  { passa o valor máximo com a entrada já transformada }
  Self.SetNeuronMinValue(ANeuron, AMinValue).SetNeuronMaxValue(ANeuron, AMaxValue).SetOutputValue(ANeuron, AValue);
end;

function TMultiLayerPerceptron.SetOutputMaxValue(const ANeuronIndex: Integer; const AMaxValue: Double): IMultiLayerPerceptron;
begin
  Result := Self.SetNeuronMaxValue(Self.GetOutputLayer.Neurons.Get(ANeuronIndex), AMaxValue);
end;

function TMultiLayerPerceptron.SetOutputMinMaxValues(const ANeuronIndex: Integer; const AMinValue, AMaxValue: Double): IMultiLayerPerceptron;
begin
  Result := Self;
  Self.GetOutputLayer.Neurons.Get(ANeuronIndex).SetMinValue(AMinValue).SetMaxValue(AMaxValue);
end;

function TMultiLayerPerceptron.SetOutputMinValue(const ANeuronIndex: Integer; const AMinValue: Double): IMultiLayerPerceptron;
begin
  Result := Self.SetNeuronMinValue(Self.GetOutputLayer.Neurons.Get(ANeuronIndex), AMinValue);
end;

function TMultiLayerPerceptron.SetOutputValue(const ANeuron: INeuron; const AValue: Double): IMultiLayerPerceptron;
begin
  Result := Self;
  with ANeuron do
  begin
    SetTarget(PositiveInput.New(AValue, MaxValue, MinValue).Convert { [0,1] });
  end;
end;

function TMultiLayerPerceptron.SetOutputValue(const ANeuronIndex: Integer; const AValue: Double): IMultiLayerPerceptron;
begin
  Result := Self.SetOutputValue(Self.GetOutputLayer.Neurons.Get(ANeuronIndex), AValue);
end;

function TMultiLayerPerceptron.SetInputValue(const ANeuronIndex: Integer; const AValue: Double): IMultiLayerPerceptron;
begin
  Result := Self.SetInputValue(Self.GetInputLayer.Neurons.Get(ANeuronIndex), AValue);
end;

function TMultiLayerPerceptron.SetActivation(
  const AActivation: IActivation): IMultiLayerPerceptron;
begin
  Result := Self;
  FActivation := AActivation;
end;

function TMultiLayerPerceptron.SetGama(const AGama: Double): IMultiLayerPerceptron;
begin
  Result := Self;
  FGama := AGama;
end;

function TMultiLayerPerceptron.SetIsOnLoad(const AIsOnLoad: Boolean): TMultiLayerPerceptron;
begin
  Result := Self;
  FIsOnLoad := AIsOnLoad;
end;

function TMultiLayerPerceptron.SetIsRead(const AIsRead: Boolean): TMultiLayerPerceptron;
begin
  Result := Self;
  FIsRead := AIsRead;
end;

function TMultiLayerPerceptron.SetLearnFile(const ALearnFile: string): IMultiLayerPerceptron;
begin
  Result := Self;
  if not Self.IsOnLoad then
  begin
    FLearnFile := EmptyStr;
    if ALearnFile <> EmptyStr then
    begin
      FLearnFile := ALearnFile;
    end;
  end;
end;

function TMultiLayerPerceptron.SetLearning(const ALearning: Double): IMultiLayerPerceptron;
begin
  Result := Self;
  FLearning := ALearning;
end;

function TMultiLayerPerceptron.SetMomentum(const AMomentum: Double): IMultiLayerPerceptron;
begin
  Result := Self;
  FMomentum := AMomentum
end;

end.

