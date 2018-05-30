unit Marvin.Core.IA.Connectionist.Neuron.Clss;

interface

uses
  Marvin.Core.IA.Connectionist.Neuron;

type
  { Factory Method para Neuron }
  TFactoryNeuron = class
  public
    class function New(const AValue: Double = 0;
      const ADelta: Double = 0): INeuron;
  end;

  { Implementação de um neurônio digital. }
  TNeuron = class(TInterfacedObject, INeuron)
  strict private
    FId: string;
    FValue: Double;
    FDelta: Double;
    FTarget: Double;
    FMaxValue: Double;
    FMinValue: Double;
  protected
    constructor Create(const AValue: Double = 0; const ADelta: Double = 0);
  public
    class function NewId: string;
    class function Compare(const Left, Right: INeuron): Integer;
    class function New(const AValue: Double = 0;
      const ADelta: Double = 0): INeuron;
    { funções de apoio }
    function Clear: INeuron;
    function Assign(const ANeuron: INeuron): INeuron;
    { properties }
    function SetValue(const AValue: Double): INeuron;
    function SetDelta(const ADelta: Double): INeuron;
    function SetTarget(const ATarget: Double): INeuron;
    function SetMaxValue(const AMaxValue: Double): INeuron;
    function SetMinValue(const AMinValue: Double): INeuron;
    function SetId(const AId: string): INeuron;
    { dados }
    function Value: Double;
    function Delta: Double;
    function Target: Double;
    function MaxValue: Double;
    function MinValue: Double;
    function Id: string;
  end;

implementation

uses
  System.SysUtils;

{ TFactoryNeuron }

class function TFactoryNeuron.New(const AValue, ADelta: Double): INeuron;
begin
  Result := TNeuron.Create(AValue, ADelta);
end;

{ TNeuron }

function TNeuron.Assign(const ANeuron: INeuron): INeuron;
begin
  Result := Self;
  FId := ANeuron.Id;
  FValue := ANeuron.Value;
  FDelta := ANeuron.Delta;
  FTarget := ANeuron.Target;
  FMaxValue := ANeuron.MaxValue;
  FMinValue := ANeuron.MinValue;
end;

function TNeuron.Clear: INeuron;
begin
  Result := Self;
  FId := TNeuron.NewId;
  FValue := 0;
  FDelta := 0;
  FTarget := 0;
  FMaxValue := 0;
  FMinValue := 0;
end;

class function TNeuron.Compare(const Left, Right: INeuron): Integer;
begin
  Result := -1;
  if SameText(Left.Id, Right.Id) then
  begin
    Result := 0;
  end;
end;

constructor TNeuron.Create(const AValue, ADelta: Double);
begin
  inherited Create;
  Self
    .Clear
    .SetValue(AValue)
    .SetDelta(ADelta);
end;

function TNeuron.Delta: Double;
begin
  Result := FDelta;
end;

function TNeuron.Id: string;
begin
  Result := FId;
end;

function TNeuron.MaxValue: Double;
begin
  Result := FMaxValue;
end;

function TNeuron.MinValue: Double;
begin
  Result := FMinValue;
end;

class function TNeuron.New(const AValue, ADelta: Double): INeuron;
begin
  Result := TNeuron.Create(AValue, ADelta);
end;

class function TNeuron.NewId: string;
var
  LId: TGUID;
begin
  CreateGUID(LId);
  Result := StringReplace(GUIDToString(LId), '{', '', [rfReplaceAll]);
  Result := StringReplace(Result, '}', '', [rfReplaceAll]);
  Result := StringReplace(Result, '-', '', [rfReplaceAll]);
end;

function TNeuron.SetDelta(const ADelta: Double): INeuron;
begin
  Result := Self;
  FDelta := ADelta;
end;

function TNeuron.SetId(const AId: string): INeuron;
begin
  Result := Self;
  FId := AId;
end;

function TNeuron.SetMaxValue(const AMaxValue: Double): INeuron;
begin
  Result := Self;
  FMaxValue := AMaxValue;
end;

function TNeuron.SetMinValue(const AMinValue: Double): INeuron;
begin
  Result := Self;
  FMinValue := AMinValue;
end;

function TNeuron.SetTarget(const ATarget: Double): INeuron;
begin
  Result := Self;
  FTarget := ATarget;
end;

function TNeuron.SetValue(const AValue: Double): INeuron;
begin
  Result := Self;
  FValue := AValue;
end;

function TNeuron.Target: Double;
begin
  Result := FTarget;
end;

function TNeuron.Value: Double;
begin
  Result := FValue;
end;

end.
