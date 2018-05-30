unit Marvin.Core.IA.Connectionist.Neurons;

interface

uses
  Marvin.Core.IA.Connectionist.Neuron,
  Marvin.Core.InterfacedList;

type
  INeurons = interface
    function List: IList<INeuron>;
    function ToString: string;
  end;

implementation

end.
