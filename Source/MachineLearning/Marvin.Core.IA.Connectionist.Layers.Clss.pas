unit Marvin.Core.IA.Connectionist.Layers.Clss;

interface

uses
  { embarcadero }
  System.Generics.Defaults,
  System.Generics.Collections,
  { marvin }
  Marvin.Core.IA.Connectionist.Layers,
  Marvin.Core.IA.Connectionist.Layer,
  Marvin.Core.IA.Connectionist.Layer.Clss,
  Marvin.Core.InterfacedList;

type
  TFactoryLayers = class
  public
    class function New: IList<ILayer>;
  end;

implementation

type
  TLayers = class sealed(TCustomList<ILayer>)
  public
    type
      TLayerComparer = class(TComparer<ILayer>)
      public
        function Compare(ALeft, ARight: ILayer): Integer; reintroduce;
      end;
  public
    constructor Create; reintroduce;
    class function New: IList<ILayer>;
    function ToString: string; override;
  end;

{ TLayers }

constructor TLayers.Create;
begin
  inherited Create(TLayerComparer.Create);
end;

class function TLayers.New: IList<ILayer>;
begin
  Result := TLayers.Create;
end;

function TLayers.ToString: string;
begin
  Result := 'TLayers';
end;

{ TFactoryLayers }

class function TFactoryLayers.New: IList<ILayer>;
begin
  Result := TLayers.New;
end;

{ TLayers.TLayerComparer }

function TLayers.TLayerComparer.Compare(ALeft, ARight: ILayer): Integer;
begin
  Result := -1;
  if ALeft.Id = ARight.Id then
  begin
    Result := 0;
  end;
end;

end.
