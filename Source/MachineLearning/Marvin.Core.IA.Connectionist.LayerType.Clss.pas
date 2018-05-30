unit Marvin.Core.IA.Connectionist.LayerType.Clss;

interface

uses
  Marvin.Core.IA.Connectionist.Neuron,
  Marvin.Core.IA.Connectionist.Neuron.Clss;

type
  TLayerType = (InputLayer, OutputLayer, HiddenLayer);

  TLayerTypeHelper = record helper for TLayerType
  public
    function New: INeuron;
  end;

implementation

{ TLayerTypeHelper }

function TLayerTypeHelper.New: INeuron;
begin
  case Self of
    InputLayer: Result := TFactoryNeuron.New;
    HiddenLayer, OutputLayer: Result := TFactoryNeuron.New(0, 1);
  end;
end;

end.
