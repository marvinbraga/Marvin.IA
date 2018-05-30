unit Marvin.Core.IA.Connectionist.Synapses;

interface

uses
  Marvin.Core.IA.Connectionist.Synapse,
  Marvin.Core.InterfacedList;

type
  ISynapses = interface
    function List: IList<ISynapse>;
    function ToString: string;
  end;

implementation

end.
