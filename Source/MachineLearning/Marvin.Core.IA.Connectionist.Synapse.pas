unit Marvin.Core.IA.Connectionist.Synapse;

interface

uses
  Marvin.Core.IA.Connectionist.Neuron;

type
  ISynapse = interface
    ['{29979914-B17B-4746-A2DB-30517A76CF5D}']
    function Clear: ISynapse;
    function Assign(const ASynapse: ISynapse): ISynapse;
    function Init: ISynapse;
    function SetDelta(const ADelta: Double): ISynapse;
    function SetValue(const AValue: Double): ISynapse;
    function SetNeuron(const ANeuron: INeuron): ISynapse;
    function SetLinkedNeuron(const ANeuron: INeuron): ISynapse;
    function Delta: Double;
    function Value: Double;
    function Neuron: INeuron;
    function LinkedNeuron: INeuron;
    function Id: string;
  end;

implementation

end.
