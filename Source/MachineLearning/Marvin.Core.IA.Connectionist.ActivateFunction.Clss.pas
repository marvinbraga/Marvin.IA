unit Marvin.Core.IA.Connectionist.ActivateFunction.Clss;

{
  MIT License

  Copyright (c) 2018 Marcus Vinicius D. B. Braga

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
  TDegreeActivation = class(TInterfacedObject, IActivation)
  public
    function Execute(const AValue: Double): Double;
    class function New: IActivation;
  end;

  TSignalActivation = class(TInterfacedObject, IActivation)
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

{ TSignalActivation }

function TSignalActivation.Execute(const AValue: Double): Double;
begin
  Result := -1;
  if AValue >= 0 then
  begin
    Result := 1;
  end;
end;

class function TSignalActivation.New: IActivation;
begin
  Result := TSignalActivation.Create;
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
