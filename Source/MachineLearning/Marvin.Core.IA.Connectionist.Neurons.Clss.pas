unit Marvin.Core.IA.Connectionist.Neurons.Clss;

interface

uses
  { embarcadero }
  System.Generics.Defaults,
  System.Generics.Collections,
  { marvin }
  Marvin.Core.IA.Connectionist.Neurons,
  Marvin.Core.IA.Connectionist.Neuron,
  Marvin.Core.IA.Connectionist.Neuron.Clss,
  Marvin.Core.InterfacedList;

type
  TFactoryNeurons = class
  public
    class function New: IList<INeuron>;
  end;

implementation

type
  TNeurons = class sealed(TCustomList<INeuron>)
  private
    type
      TNeuronComparer = class(TComparer<INeuron>)
      public
        function Compare(ALeft, ARight: INeuron): Integer; reintroduce;
      end;
  public
    constructor Create; reintroduce;
    class function New: IList<INeuron>;
    function ToString: string; override;
  end;

{ TNeurons }

constructor TNeurons.Create;
begin
  inherited Create(TNeuronComparer.Create);
end;

class function TNeurons.New: IList<INeuron>;
begin
  Result := TNeurons.Create;
end;

function TNeurons.ToString: string;
begin
  Result := 'TNeurons';
end;

{ TFactoryNeurons }

class function TFactoryNeurons.New: IList<INeuron>;
begin
  Result := TNeurons.New;
end;

{ TNeurons.TNeuronComparer }

function TNeurons.TNeuronComparer.Compare(ALeft, ARight: INeuron): Integer;
begin
  Result := -1;
  if (ALeft.Id = ARight.Id) then
  begin
    Result := 0;
  end;
end;

end.
