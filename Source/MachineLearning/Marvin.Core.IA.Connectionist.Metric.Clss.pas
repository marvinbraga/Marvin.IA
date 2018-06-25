unit Marvin.Core.IA.Connectionist.Metric.Clss;

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
  System.Classes,
  { marvin }
  Marvin.Core.InterfacedList,
  Marvin.Core.IA.Connectionist.Classifier,
  Marvin.Core.IA.Connectionist.Metric;

implementation

uses
  System.SysUtils;

type
  TAccuracy = class(TInterfacedObject, IMetric)
  private
    FValue: Double;
    FCount: Integer;
  protected
    function Calculate(const ATestOutputData, APredictedData: IList<TDoubleArray>): IMetric;
    function Value: Double;
    function Count: Integer;
  public
    class function New: IMetric;
  end;

  TConfusionMatrix = class(TInterfacedObject, IConfusionMatrix)
  private
    FConfusionMatrix: TDoubleMatrix;
  protected
    function Calculate(const ATestOutputData: IList<TDoubleArray>; const APredictedData: IList<TDoubleArray>): IConfusionMatrix;
    function Matrix: TDoubleMatrix;
  public
    constructor Create;
    class function New: IConfusionMatrix;
    destructor Destroy; override;
  end;

{ TConfusionMatrix }

constructor TConfusionMatrix.Create;
begin
  inherited Create;
end;

destructor TConfusionMatrix.Destroy;
begin
  inherited;
end;

function TConfusionMatrix.Matrix: TDoubleMatrix;
begin
  Result := FConfusionMatrix;
end;

class function TConfusionMatrix.New: IConfusionMatrix;
begin
  Result := TConfusionMatrix.Create;
end;

function TConfusionMatrix.Calculate(const ATestOutputData: IList<TDoubleArray>; const APredictedData: IList<TDoubleArray>): IConfusionMatrix;
begin
  Result := Self;
  { informa o tamanho da matriz }
  SetLength(FConfusionMatrix, Length(ATestOutputData.Get(0)));

end;

{ TAccuracy }

function TAccuracy.Calculate(const ATestOutputData, APredictedData: IList<TDoubleArray>): IMetric;
var
  LCount: Double;
  LTestValue, LPredictValue: string;
begin
  FCount := 0;
  LCount := ATestOutputData.Count;
  if LCount = 0 then
  begin
    LCount := 1;
  end;
  { calcula }
  LTestValue := ATestOutputData.First.ToString;
  LPredictValue := APredictedData.First.ToString;
  while not ATestOutputData.Eof do
  begin
    { achou um acerto }
    if SameText(LTestValue, LPredictValue) then
    begin
      { calcula a contagem }
      FCount := FCount + 1;
    end;
    LTestValue := ATestOutputData.MoveNext.ToString;
    LPredictValue := APredictedData.MoveNext.ToString;
  end;
  { calcula o percentual da acurácia }
  FValue := FCount / LCount;
end;

function TAccuracy.Count: Integer;
begin
  Result := FCount;
end;

class function TAccuracy.New: IMetric;
begin
  Result := TAccuracy.Create;
end;

function TAccuracy.Value: Double;
begin
  Result := FValue;
end;

end.

