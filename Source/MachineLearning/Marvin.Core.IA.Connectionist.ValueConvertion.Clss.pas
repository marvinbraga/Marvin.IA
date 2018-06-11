unit Marvin.Core.IA.Connectionist.ValueConvertion.Clss;

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
  Marvin.Core.IA.Connectionist.ValueConvertion;

type
  TValueConvert = (PositiveInput, NegativeInput, PositiveOutput, NegativeOutput, LinearValue);

  THelperValueConvert = record helper for TValueConvert
  public
    function New(const AValue, AMaxValue, AMinValue: Double): IValueConvertion;
  end;

implementation

type
  TLinearConvertion = class(TInterfacedObject, IValueConvertion)
  private
    FValue: Double;
    FMaxValue: Double;
    FMinValue: Double;
  protected
    function Convert: Double; virtual;
  public
    constructor Create(const AValue, AMaxValue, AMinValue: Double);
    class function New(const AValue, AMaxValue, AMinValue: Double): IValueConvertion;
  end;

  TPositiveInputValue = class sealed(TLinearConvertion)
  protected
    function Convert: Double; override;
  public
    class function New(const AValue, AMaxValue, AMinValue: Double): IValueConvertion;
  end;

  TNegativeInputValue = class sealed(TLinearConvertion)
  protected
    function Convert: Double; override;
  public
    class function New(const AValue, AMaxValue, AMinValue: Double): IValueConvertion;
  end;

  TPositiveOutputValue = class sealed(TLinearConvertion)
  protected
    function Convert: Double; override;
  public
    class function New(const AValue, AMaxValue, AMinValue: Double): IValueConvertion;
  end;

  TNegativeOutputValue = class sealed(TLinearConvertion)
  protected
    function Convert: Double; override;
  public
    class function New(const AValue, AMaxValue, AMinValue: Double): IValueConvertion;
  end;

{ TLinearConvertion }

function TLinearConvertion.Convert: Double;
begin
  Result := FValue;
end;

constructor TLinearConvertion.Create(const AValue, AMaxValue,
  AMinValue: Double);
begin
  inherited Create;
  FValue := AValue;
  FMaxValue := AMaxValue;
  FMinValue := AMinValue;
end;

class function TLinearConvertion.New(const AValue, AMaxValue,
  AMinValue: Double): IValueConvertion;
begin
  Result := TLinearConvertion.Create(AValue, AMaxValue, AMinValue);
end;

{ TPositiveInputValue }

class function TPositiveInputValue.New(const AValue, AMaxValue,
  AMinValue: Double): IValueConvertion;
begin
  Result := TPositiveInputValue.Create(AValue, AMaxValue, AMinValue);
end;

function TPositiveInputValue.Convert: Double;
begin
  { [0,1] }
  Result := 1 + (FValue - FMaxValue) / (FMaxValue - FMinValue);
end;

{ TNegativeInputValue }

function TNegativeInputValue.Convert: Double;
begin
  { [-1,1] }
  Result := 1 + 2 * (FValue - FMaxValue) / (FMaxValue - FMinValue);
end;

class function TNegativeInputValue.New(const AValue, AMaxValue,
  AMinValue: Double): IValueConvertion;
begin
  Result := TNegativeInputValue.Create(AValue, AMaxValue, AMinValue);
end;

{ TPositiveOutputValue }

function TPositiveOutputValue.Convert: Double;
begin
  { [0, 1] }
  Result := (FValue - 1) * (FMaxValue - FMinValue) + FMaxValue;
end;

class function TPositiveOutputValue.New(const AValue, AMaxValue,
  AMinValue: Double): IValueConvertion;
begin
  Result := TPositiveOutputValue.Create(AValue, AMaxValue, AMinValue);
end;

{ TNegativeOutputValue }

function TNegativeOutputValue.Convert: Double;
begin
  { [-1,1] }
  Result := 0.5 * (FValue - 1) * (FMaxValue - FMinValue) + FMaxValue;
end;

class function TNegativeOutputValue.New(const AValue, AMaxValue,
  AMinValue: Double): IValueConvertion;
begin
  Result := TNegativeOutputValue.Create(AValue, AMaxValue, AMinValue);
end;

{ THelperValueConvert }

function THelperValueConvert.New(const AValue, AMaxValue,
  AMinValue: Double): IValueConvertion;
begin
  case Self of
    PositiveInput: Result := TPositiveInputValue.New(AValue, AMaxValue, AMinValue);
    NegativeInput: Result := TNegativeInputValue.New(AValue, AMaxValue, AMinValue);
    PositiveOutput: Result := TPositiveOutputValue.New(AValue, AMaxValue, AMinValue);
    NegativeOutput: Result := TNegativeOutputValue.New(AValue, AMaxValue, AMinValue);
    LinearValue: Result := TLinearConvertion.New(AValue, AMaxValue, AMinValue);
  end;
end;

end.
