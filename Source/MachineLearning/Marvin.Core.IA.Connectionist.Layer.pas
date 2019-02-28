unit Marvin.Core.IA.Connectionist.Layer;

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
  Marvin.Core.IA.Connectionist.Neuron,
  Marvin.Core.IA.Connectionist.Synapse,
  Marvin.Core.IA.Connectionist.LayerType.Clss,
  Marvin.Core.InterfacedList;

type
  ILayer = interface
    ['{73701F43-6ABF-486C-A8EB-ABAC58ADB030}']
    function Clear: ILayer;
    function Assign(const ALayer: ILayer): ILayer;
    function InitSynapses: ILayer;
    function NewNeurons(const ACount: Integer): ILayer;
    function NewSynapse(const ANeuron: INeuron; const ALinkedNeuron: INeuron;
      const AValue: Double = 0): ISynapse;
    function AddSynapse(const ASynapse: ISynapse): ILayer;
    function LayerType: TLayerType;
    function NeuronsCount: Integer;
    function Neurons: IList<INeuron>;
    function Synapses: IList<ISynapse>;
    function Index: Integer;
    function Id: string;
  end;

implementation

end.
