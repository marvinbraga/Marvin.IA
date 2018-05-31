unit Marvin.Core.IA.Connectionist.Synapse.Clss;

interface

uses
  Marvin.Core.IA.Connectionist.Synapse,
  Marvin.Core.IA.Connectionist.Neuron,
  { classe de neur�nio  }
  Marvin.Core.IA.Connectionist.Neuron.Clss;

type
  TFactorySynapse = class
  public
    class function New(const ANeuron: INeuron; const ALinkedNeuron: INeuron): ISynapse;
  end;

  { Implementa��o de uma sinapse digital. }
  TSynapse = class(TInterfacedObject, ISynapse)
  strict private
    FId: string;
    FDelta: Double;
    FValue: Double;
    FNeuron: INeuron;
    FLinkedNeuron: INeuron;
  protected
  public
    class function NewId: string;
    class function Compare(const Left, Right: ISynapse): Integer;
    constructor Create(const ANeuron: INeuron; const ALinkedNeuron: INeuron);
    destructor Destroy; override;
    function Clear: ISynapse;
    function Assign(const ASynapse: ISynapse): ISynapse;
    function Init: ISynapse;
    function SetDelta(const ADelta: Double): ISynapse;
    function SetValue(const AValue: Double): ISynapse;
    function SetNeuron(const ANeuron: INeuron): ISynapse;
    function SetLinkedNeuron(const ANeuron: INeuron): ISynapse;
    { dados }
    function Delta: Double;
    function Value: Double;
    function Neuron: INeuron;
    function LinkedNeuron: INeuron;
    function Id: string;
  end;

implementation

uses
  System.SysUtils;

{ TFactorySynapse }

class function TFactorySynapse.New(const ANeuron,
  ALinkedNeuron: INeuron): ISynapse;
begin
  Result := TSynapse.Create(ANeuron, ALinkedNeuron);
end;

{ TSynapse }

function TSynapse.Assign(const ASynapse: ISynapse): ISynapse;
begin
  Result := Self;
  FId := ASynapse.Id;
  FValue := ASynapse.Value;
  FDelta := ASynapse.Delta;
  FNeuron := ASynapse.Neuron;
  FLinkedNeuron := ASynapse.LinkedNeuron;
end;

function TSynapse.Clear: ISynapse;
begin
  Result := Self;
  FId := TSynapse.NewId;
  FValue := 0;
  FDelta := 0;
  FNeuron := nil;
  FLinkedNeuron := nil;
end;

class function TSynapse.Compare(const Left, Right: ISynapse): Integer;
begin
  Result := -1;
  if SameText(Left.Id, Right.Id) then
  begin
    Result := 0;
  end;
end;

constructor TSynapse.Create(const ANeuron, ALinkedNeuron: INeuron);
begin
  inherited Create;
  Self.Clear;
  FNeuron := ANeuron;
  FLinkedNeuron := ALinkedNeuron;
end;

function TSynapse.Delta: Double;
begin
  Result := FDelta;
end;

destructor TSynapse.Destroy;
begin
  FNeuron := nil;
  FLinkedNeuron := nil;
  inherited;
end;

function TSynapse.Id: string;
begin
  Result := FId;
end;

function TSynapse.Init: ISynapse;
begin
  Result := Self;
  { inicializa com valores aleat�rios }
  Self.SetValue(Random(1000) / 1000 - 0.5).SetDelta(0);
end;

function TSynapse.LinkedNeuron: INeuron;
begin
  Result := FLinkedNeuron;
end;

function TSynapse.Neuron: INeuron;
begin
  Result := FNeuron;
end;

class function TSynapse.NewId: string;
begin
  Result := TNeuron.NewId;
end;

function TSynapse.SetDelta(const ADelta: Double): ISynapse;
begin
  Result := Self;
  FDelta := ADelta;
end;

function TSynapse.SetLinkedNeuron(const ANeuron: INeuron): ISynapse;
begin
  Result := Self;
  FLinkedNeuron := ANeuron;
end;

function TSynapse.SetNeuron(const ANeuron: INeuron): ISynapse;
begin
  Result := Self;
  FNeuron := ANeuron;
end;

function TSynapse.SetValue(const AValue: Double): ISynapse;
begin
  Result := Self;
  FValue := AValue;
end;

function TSynapse.Value: Double;
begin
  Result := FValue;
end;

end.