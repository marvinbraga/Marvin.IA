unit Marvin.Core.IA.Connectionist.Synapses.Clss;

interface

uses
  { embarcadero }
  System.Generics.Defaults,
  System.Generics.Collections,
  { marvin }
  Marvin.Core.IA.Connectionist.Synapses,
  Marvin.Core.IA.Connectionist.Synapse,
  Marvin.Core.IA.Connectionist.Synapse.Clss,
  Marvin.Core.InterfacedList;

type
  TFactorySynapses = class
  public
    class function New: IList<ISynapse>;
  end;

implementation

type
  TSynapses = class sealed(TCustomList<ISynapse>)
  private
    type
      TSynapseComparer = class(TComparer<ISynapse>)
      public
        function Compare(ALeft, ARight: ISynapse): Integer; reintroduce;
      end;
  public
    constructor Create; reintroduce;
    class function New: IList<ISynapse>;
    function ToString: string; override;
  end;

{ TSynapses }

constructor TSynapses.Create;
begin
  inherited Create(TSynapseComparer.Create);
end;

class function TSynapses.New: IList<ISynapse>;
begin
  Result := TSynapses.Create;
end;

function TSynapses.ToString: string;
begin
  Result := 'TSynapses';
end;

{ TFactorySynapses }

class function TFactorySynapses.New: IList<ISynapse>;
begin
  Result := TSynapses.New;
end;

{ TSynapses.TSynapseComparer }

function TSynapses.TSynapseComparer.Compare(ALeft, ARight: ISynapse): Integer;
begin
  Result := -1;
  if ALeft.Id = ARight.Id then
  begin
    Result := 0;
  end;
end;

end.

