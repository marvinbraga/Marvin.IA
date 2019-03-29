unit Marvin.Core.IA.Connectionist.ActivateFunction.Clss;

{
  MIT License

  Copyright (c) 2018-2019 Marcus Vinicius D. B. Braga

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}

interface

uses
  Marvin.Core.IA.Connectionist.Activation;

type
  TDegree = class(TInterfacedObject, IActivation)
  protected
    function Execute(const AValue: Double): Double;
  public
    class function New: IActivation;
  end;

  TSignal = class(TInterfacedObject, IActivation)
  protected
    function Execute(const AValue: Double): Double;
  public
    class function New: IActivation;
  end;

  TSigmoid = class(TInterfacedObject, IActivation)
  protected
    function Execute(const AValue: Double): Double;
  public
    class function New: IActivation;
  end;

  THyperbolicTangent = class(TInterfacedObject, IActivation)
  protected
    function Execute(const AValue: Double): Double;
  public
    class function New: IActivation;
  end;

  TGaussian = class(TInterfacedObject, IActivation)
  private
    FCenter: Double;
    FSoftness: Double;
    FCenterMaximumDistance: Double;
  protected
    function Execute(const AValue: Double): Double;
  public
    constructor Create(const ACenter: Double; const ACenterMaximumDistance: Double; const ASoftness: Double);
    class function New(const ACenter: Double = 0; const ACenterMaximumDistance: Double = 2; const ASoftness: Double = 1): IActivation;
  end;

  TReLu = class(TInterfacedObject, IActivation)
  protected
    function Execute(const AValue: Double): Double;
  public
    class function New: IActivation;
  end;

  TLeakyReLu = class(TInterfacedObject, IActivation)
  private
    FLeaky: Double;
  protected
    function Execute(const AValue: Double): Double;
  public
    constructor Create; overload;
    constructor Create(const ALeaky: Double); overload;
    class function New: IActivation; overload;
    class function New(const ALeaky: Double): IActivation; overload;
  end;

implementation

uses
  System.Math;

{ TSigmoideActivation }

function TSigmoid.Execute(const AValue: Double): Double;
begin
  Result := 1 / (1 + Exp(-AValue));
end;

class function TSigmoid.New: IActivation;
begin
  Result := TSigmoid.Create;
end;

{ TDegreeActivation }

function TDegree.Execute(const AValue: Double): Double;
begin
  Result := 0;
  if AValue >= 0 then
  begin
    Result := 1;
  end;
end;

class function TDegree.New: IActivation;
begin
  Result := TDegree.Create;
end;

{ TSignalActivation }

function TSignal.Execute(const AValue: Double): Double;
begin
  Result := -1;
  if AValue >= 0 then
  begin
    Result := 1;
  end;
end;

class function TSignal.New: IActivation;
begin
  Result := TSignal.Create;
end;

{ THyperbolicTangentActivation }

function THyperbolicTangent.Execute(const AValue: Double): Double;
begin
  Result := (1 - Exp(-AValue)) / (1 + Exp(-AValue));
end;

class function THyperbolicTangent.New: IActivation;
begin
  Result := THyperbolicTangent.Create;
end;

{ TGaussianActivation }

constructor TGaussian.Create(const ACenter: Double; const ACenterMaximumDistance: Double; const ASoftness: Double);
begin
  inherited Create;
  FCenter := ACenter;
  FSoftness := ASoftness;
  FCenterMaximumDistance := ACenterMaximumDistance;
end;

function TGaussian.Execute(const AValue: Double): Double;
begin
  Result := Exp(-(Sqr(AValue - FCenter) / Sqr(FCenterMaximumDistance * FSoftness)));
end;

class function TGaussian.New(const ACenter: Double = 0; const ACenterMaximumDistance: Double = 2; const ASoftness: Double = 1): IActivation;
begin
  Result := TGaussian.Create(ACenter, ACenterMaximumDistance, ASoftness);
end;

{ TReLu }

function TReLu.Execute(const AValue: Double): Double;
begin
  Result := 0;
  if (AValue >= 0) then
  begin
    Result := AValue;
  end;
end;

class function TReLu.New: IActivation;
begin
  Result := TReLu.Create;
end;

{ TLeakyReLu }

constructor TLeakyReLu.Create;
begin
  inherited;
  FLeaky := 0.2;
end;

constructor TLeakyReLu.Create(const ALeaky: Double);
begin
  Self.Create;
  FLeaky := ALeaky;
end;

function TLeakyReLu.Execute(const AValue: Double): Double;
begin
  Result := FLeaky;
  if (AValue >= 0) then
  begin
    Result := AValue;
  end;
end;

class function TLeakyReLu.New: IActivation;
begin
  Result := TLeakyReLu.Create;
end;

class function TLeakyReLu.New(const ALeaky: Double): IActivation;
begin
  Result := TLeakyReLu.Create(ALeaky);
end;

end.

