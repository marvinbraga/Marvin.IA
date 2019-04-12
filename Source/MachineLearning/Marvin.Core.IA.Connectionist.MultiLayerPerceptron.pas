unit Marvin.Core.IA.Connectionist.MultiLayerPerceptron;

{
  MIT License

  Copyright (c) 2018-2019 Marcus Vinicius D. B. Braga

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
  Marvin.Core.IA.Connectionist.Neuron,
  Marvin.Core.IA.Connectionist.Layer,
  Marvin.Core.IA.Connectionist.Support,
  Marvin.Core.IA.Connectionist.Activation,
  Marvin.Core.IA.Connectionist.LayerInitInfo;

type
  IMultiLayerPerceptron = interface
    ['{846E81A3-5F83-4B62-8BFA-5F6BCC7E0D89}']
    { Assigns the values to the input neurons. }
    function SetInput(const ANeuronIndex: Integer; const AValue: Double; const AMinValue: Double; const AMaxValue: Double): IMultiLayerPerceptron; overload;
    function SetInput(const ANeuron: INeuron; const AValue: Double; const AMinValue: Double; const AMaxValue: Double): IMultiLayerPerceptron; overload;
    function SetInputValue(const ANeuronIndex: Integer; const AValue: Double): IMultiLayerPerceptron; overload;
    function SetInputValue(const ANeuron: INeuron; const AValue: Double): IMultiLayerPerceptron; overload;
    function SetInputMaxValue(const ANeuronIndex: Integer; const AMaxValue: Double): IMultiLayerPerceptron; overload;
    function SetInputMinValue(const ANeuronIndex: Integer; const AMinValue: Double): IMultiLayerPerceptron; overload;
    function SetInputMinMaxValues(const ANeuronIndex: Integer; const AMinValue: Double; const AMaxValue: Double): IMultiLayerPerceptron; overload;
    { Assigns the values to the output neurons. }
    function SetOutput(const ANeuronIndex: Integer; const AValue: Double; const AMinValue: Double; const AMaxValue: Double): IMultiLayerPerceptron; overload;
    function SetOutput(const ANeuron: INeuron; const AValue: Double; const AMinValue: Double; const AMaxValue: Double): IMultiLayerPerceptron; overload;
    function SetOutputValue(const ANeuronIndex: Integer; const AValue: Double): IMultiLayerPerceptron; overload;
    function SetOutputValue(const ANeuron: INeuron; const AValue: Double): IMultiLayerPerceptron; overload;
    function SetOutputMaxValue(const ANeuronIndex: Integer; const AMaxValue: Double): IMultiLayerPerceptron; overload;
    function SetOutputMinValue(const ANeuronIndex: Integer; const AMinValue: Double): IMultiLayerPerceptron; overload;
    function SetOutputMinMaxValues(const ANeuronIndex: Integer; const AMinValue: Double; const AMaxValue: Double): IMultiLayerPerceptron; overload;
    { Initializes the synapses with the imported values. }
    function InitSynapse(const ALayer: ILayer; const ASynapseId: string; const ASynapseValue: Double; const ANeuronId: string; const ALinkedNeuronId: string): IMultiLayerPerceptron;
    function SetLayersInfo(const ALayersInitInfo: TLayerInitInfoArray): IMultiLayerPerceptron;
    { Assign maximum and minimum values to the neurons. }
    function SetNeuronMaxValue(const ANeuron: INeuron; const AMaxValue: Double): IMultiLayerPerceptron; overload;
    function SetNeuronMinValue(const ANeuron: INeuron; const AMinValue: Double): IMultiLayerPerceptron; overload;
    { atualiza o gama }
    function SetGama(const AGama: Double = 0): IMultiLayerPerceptron;
    { pega valores de saída }
    function GetOutputValue(const ANeuronIndex: Integer): Double; overload;
    function GetOutputValue(const ANeuron: INeuron): Double; overload;
    { recupera os layers }
    function Layers: IList<ILayer>;
    { configuração dos layers - quantidade de neurônios que tem em cada camada }
    function LayersConfig: TList<Word>;
    { recupera a camada de entrada }
    function GetInputLayer: ILayer;
    { recupera a camada de saída }
    function GetOutputLayer: ILayer;
    { limpa os dados da MLP }
    function Clear: IMultiLayerPerceptron;
    { configura uma nova camada informando sua quantidade de neurônios }
    function AddLayerConfig(const ANeuronCount: Word): IMultiLayerPerceptron;
    { constrói a rede para iniciar }
    function Build: IMultiLayerPerceptron;
    { executa uma época de treinamento da rede }
    function Training: IMultiLayerPerceptron;
    { Valores para teste de valores de entrada }
    function Test: IMultiLayerPerceptron;
    { retorna a função custo, ou seja, o erro atual }
    function Cost: Double;
    { elimina os pesos zerados }
    function Purge: IMultiLayerPerceptron;
    { informa função de ativação para a camada de entrada }
    function SetInputActivation(const AInputActivation: IActivation): IMultiLayerPerceptron;
    { atualiza o momento }
    function SetMomentum(const AMomentum: Double = 0.5): IMultiLayerPerceptron;
    { momento para atualização dos pesos sinápticos }
    function Momentum: Double;
    { atualiza o aprendizado }
    function SetLearning(const ALearning: Double = 0.9): IMultiLayerPerceptron;
    { aprendizado para atualização dos pesos sinápticos }
    function Learning: Double;
    { atualiza o aprendizado }
    function SetLearnFile(const ALearnFile: string = ''): IMultiLayerPerceptron;
    { nome do arquivo de conhecimento }
    function LearnFile: string;
    { recupera o gama }
    function Gama: Double;
    { manutenção }
    function Save(const ASupportMultiLayerPerceptron: ISupportMultiLayerPerceptron): IMultiLayerPerceptron;
    function Load(const ASupportMultiLayerPerceptron: ISupportMultiLayerPerceptron): IMultiLayerPerceptron;
  end;

implementation

end.
