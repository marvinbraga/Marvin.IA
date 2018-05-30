unit Marvin.Core.IA.Connectionist.Layer;

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
