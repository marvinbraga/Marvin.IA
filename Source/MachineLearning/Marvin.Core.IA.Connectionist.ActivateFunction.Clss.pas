unit Marvin.Core.IA.Connectionist.ActivateFunction.Clss;

interface

uses
  Marvin.Core.IA.Connectionist.Activation;

type
  TDegreeActivation = class(TInterfacedObject, IActivation)
  public
    function Execute(const AValue: Double): Double;
    class function New: IActivation;
  end;

  TSigmoidActivation = class(TInterfacedObject, IActivation)
  public
    function Execute(const AValue: Double): Double;
    class function New: IActivation;
  end;

  THyperbolicTangentActivation = class(TInterfacedObject, IActivation)
  public
    function Execute(const AValue: Double): Double;
    class function New: IActivation;
  end;

  TGaussianActivation = class(TInterfacedObject, IActivation)
  private
    FCenter: Double;
    FSoftness: Double;
    FCenterMaximumDistance: Double;
  public
    constructor Create(const ACenter: Double; const ACenterMaximumDistance: Double; const ASoftness: Double);
    class function New(const ACenter: Double = 0; const ACenterMaximumDistance: Double = 2; const ASoftness: Double = 1): IActivation;
    function Execute(const AValue: Double): Double;
  end;

implementation

{ TSigmoideActivation }

function TSigmoidActivation.Execute(const AValue: Double): Double;
begin
  Result := 1 / (1 + Exp(-AValue));
end;

class function TSigmoidActivation.New: IActivation;
begin
  Result := TSigmoidActivation.Create;
end;

{ TDegreeActivation }

function TDegreeActivation.Execute(const AValue: Double): Double;
begin
  Result := 0;
  if AValue >= 0 then
  begin
    Result := 1;
  end;
end;

class function TDegreeActivation.New: IActivation;
begin
  Result := TDegreeActivation.Create;
end;

{ THyperbolicTangentActivation }

function THyperbolicTangentActivation.Execute(
  const AValue: Double): Double;
begin
  Result := (1 - Exp(-AValue)) / (1 + Exp(-AValue));
end;

class function THyperbolicTangentActivation.New: IActivation;
begin
  Result := THyperbolicTangentActivation.Create;
end;

{ TGaussianActivation }

constructor TGaussianActivation.Create(const ACenter: Double; const ACenterMaximumDistance: Double; const ASoftness: Double);
begin
  inherited Create;
  FCenter := ACenter;
  FSoftness := ASoftness;
  FCenterMaximumDistance := ACenterMaximumDistance;
end;

function TGaussianActivation.Execute(const AValue: Double): Double;
begin
  Result :=  Exp(-(Sqr(AValue - FCenter) / Sqr(FCenterMaximumDistance * FSoftness)));
end;

class function TGaussianActivation.New(const ACenter: Double = 0; const ACenterMaximumDistance: Double = 2; const ASoftness: Double = 1): IActivation;
begin
  Result := TGaussianActivation.Create(ACenter, ACenterMaximumDistance, ASoftness);
end;

end.
