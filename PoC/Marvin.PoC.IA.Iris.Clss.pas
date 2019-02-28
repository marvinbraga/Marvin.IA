unit Marvin.PoC.IA.Iris.Clss;

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
  Marvin.PoC.IA.Iris,
  Marvin.PoC.IA.DataConverter,
  Marvin.Core.IA.Connectionist.Classifier;

type
  TIrisData = class(TInterfacedObject, IIrisData)
  private
    FSepal: ISize;
    FPetal: ISize;
    FClassName: string;
  protected
    function Petal: ISize;
    function Sepal: ISize;
    function ClassName: string;
    function ClassValue: Double;
    function GetInputValues: TDoubleArray;
    function GetOutputValues: TDoubleArray;
  public
    constructor Create(const ASepalLength, ASepalWidth, APetalLength, APetalWidth: Double; const AClassName: string);
    class function New(const ASepalLength, ASepalWidth, APetalLength, APetalWidth: Double; const AClassName: string): IIrisData;
  end;

implementation

type
  TSize = class(TInterfacedObject, ISize)
  private
    FWidth: Double;
    FLength: Double;
    procedure SetWidth(const Value: Double);
    procedure SetLength(const Value: Double);
    function GetLength: Double;
    function GetWidth: Double;
  protected
    property Width: Double read GetWidth write SetWidth;
    property Length: Double read GetLength write SetLength;
  public
    constructor Create(const ALength: Double; const AWidth: Double); reintroduce;
    class function New(const ALength: Double; const AWidth: Double): ISize;
  end;

{ TIrisData }

constructor TSize.Create(const ALength, AWidth: Double);
begin
  inherited Create;
  FWidth := AWidth;
  FLength := ALength;
end;

function TSize.GetLength: Double;
begin
  Result := FLength;
end;

function TSize.GetWidth: Double;
begin
  Result := FWidth;
end;

class function TSize.New(const ALength, AWidth: Double): ISize;
begin
  Result := TSize.Create(ALength, AWidth);
end;

procedure TSize.SetLength(const Value: Double);
begin
  FLength := Value;
end;

procedure TSize.SetWidth(const Value: Double);
begin
  FWidth := Value;
end;

{ TIrisData }

function TIrisData.ClassName: string;
begin
  Result := FClassName;
end;

function TIrisData.ClassValue: Double;
const
 LC_SETOSA: string = 'Iris-setosa';
 LC_VERSICOLOR: string = 'Iris-versicolor';
 LC_VIRGINICA: string = 'Iris-virginica';
begin
  if FClassName = LC_SETOSA then
  begin
    Result := 1;
  end
  else if FClassName = LC_VERSICOLOR then
  begin
    Result := 2;
  end
  else //if FClassName = LC_VIRGINICA then
  begin
    Result := 3;
  end;
end;

constructor TIrisData.Create(const ASepalLength, ASepalWidth, APetalLength, APetalWidth: Double; const AClassName: string);
begin
  inherited Create;
  FSepal := TSize.New(ASepalLength, ASepalWidth);
  FPetal := TSize.New(APetalLength, APetalWidth);
  FClassName := AClassName;
end;

class function TIrisData.New(const ASepalLength, ASepalWidth, APetalLength,
  APetalWidth: Double; const AClassName: string): IIrisData;
begin
  Result := TIrisData.Create(ASepalLength, ASepalWidth, APetalLength, APetalWidth, AClassName);
end;

function TIrisData.Petal: ISize;
begin
  Result := FPetal;
end;

function TIrisData.Sepal: ISize;
begin
  Result := FSepal;
end;

function TIrisData.GetInputValues: TDoubleArray;
begin
  Result := nil;
  SetLength(Result, 4);
  { recupera os valores Double }
  Result := [FSepal.Length, FSepal.Width, FPetal.Length, FPetal.Width];
end;

function TIrisData.GetOutputValues: TDoubleArray;
begin
  Result := nil;
  SetLength(Result, 3);
  { recupera os valores Double }
  case Trunc(Self.ClassValue) of
    1: Result := [1, 0, 0];
    2: Result := [0, 1, 0];
  else
    Result := [0, 0, 1];
  end;
end;

end.
