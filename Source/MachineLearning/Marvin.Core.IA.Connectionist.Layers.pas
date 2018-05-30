unit Marvin.Core.IA.Connectionist.Layers;

interface

uses
  Marvin.Core.IA.Connectionist.Layer,
  Marvin.Core.InterfacedList;

type
  ILayers = interface
    function List: IList<ILayer>;
    function ToString: string;
  end;

implementation

end.
