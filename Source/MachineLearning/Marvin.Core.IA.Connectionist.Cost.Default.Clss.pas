unit Marvin.Core.IA.Connectionist.Cost.Default.Clss;

interface

uses
  { marvin }
  Marvin.Core.IA.Connectionist.Layer,
  Marvin.Core.IA.Connectionist.Cost;

type
  TCostDefault = class(TInterfacedObject, ICost)
  strict private
    FLayer: ILayer;
  protected
    function Calculate: Double;
  public
    constructor Create(const ALayer: ILayer);
    destructor Destroy; override;
    class function New(const ALayer: ILayer): ICost;
  end;

implementation

{ TCostDefault }

function TCostDefault.Calculate: Double;
var
  LIndexNeuron: Integer;
  LCost: Double;
begin
  LCost := 0;
  for LIndexNeuron := 0 to FLayer.Neurons.Count - 1 do
  begin
    { com cada neurônio }
    with FLayer.Neurons.Get(LIndexNeuron) do
    begin
      { calcula o valor para o erro }
      LCost := LCost + Sqr(Target - Value);
    end;
  end;
  { retorna o erro }
  Result := 0.5 * LCost;
end;

constructor TCostDefault.Create(const ALayer: ILayer);
begin
  inherited Create;
  FLayer := ALayer;
end;

destructor TCostDefault.Destroy;
begin
  FLayer := nil;
  inherited;
end;

class function TCostDefault.New(const ALayer: ILayer): ICost;
begin
  Result := TCostDefault.Create(ALayer);
end;

end.

