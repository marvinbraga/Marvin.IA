unit Marvin.Core.IA.Connectionist.Metric.Clss;

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
  System.Classes,
  { marvin }
  Marvin.Core.InterfacedList,
  Marvin.Core.IA.Connectionist.Classifier,
  Marvin.Core.IA.Connectionist.Metric;

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
    function Configure(const ATestOutputData: IList<TDoubleArray>; const APredictedData: IList<TDoubleArray>): TConfusionMatrix;
    function GetClasses(const ATestOutputData: IList<TDoubleArray>; out AClasses: array of string): TConfusionMatrix;
  protected
    function Calculate(const ATestOutputData: IList<TDoubleArray>; const APredictedData: IList<TDoubleArray>): IConfusionMatrix;
    function Matrix: TDoubleMatrix;
  public
    class function New: IConfusionMatrix;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Generics.Defaults;

{ TConfusionMatrix }

class function TConfusionMatrix.New: IConfusionMatrix;
begin
  Result := TConfusionMatrix.Create;
end;

function TConfusionMatrix.Matrix: TDoubleMatrix;
begin
  Result := FConfusionMatrix;
end;

function TConfusionMatrix.Configure(const ATestOutputData, APredictedData: IList<TDoubleArray>): TConfusionMatrix;
var
  LSize, LIndex: Integer;
begin
  Result := Self;
  LSize := Length(ATestOutputData.Get(0));
  { cria as linhas com "cabeçalho" = 0 e "total" = N }
  SetLength(FConfusionMatrix, LSize + 2);
  { cria as colunas }
  for LIndex := Low(FConfusionMatrix) to High(FConfusionMatrix) do
  begin
    { com "descrição de item" = 0, "acurácia" = N-3, "precisão" = N-2, "Recall" = N-1 e "FScore" = N }
    SetLength(FConfusionMatrix[LIndex], LSize + 5);
  end;
end;

function TConfusionMatrix.GetClasses(const ATestOutputData: IList<TDoubleArray>; out AClasses: array of string): TConfusionMatrix;
var
  LCount, LTotal: Integer;
  LTestValue: string;
  LClasses: array of string;
begin
  Result := Self;
  LCount := ATestOutputData.Count;
  LTotal := 0;
  if LCount > 0 then
  begin
    { calcula }
    LTestValue := ATestOutputData.First.ToString;
    { ajusta o tamanho do array que irá conter as classes }
    SetLength(LClasses, LCount);
    { recupera as classes }
    LClasses[0] := LTestValue;
    LTotal := 1;
    while not ATestOutputData.Eof do
    begin
      LTestValue := ATestOutputData.MoveNext.ToString;
      if (LTestValue <> LClasses[LTotal - 1]) then
      begin
        LClasses[LTotal] := LTestValue;
        Inc(LTotal);
      end;
    end;
  end;
  { ajusta o tamanho }
  SetLength(LClasses, LTotal);
  { ordena }
  TArray.Sort<string>(LClasses, TStringComparer.Ordinal);
  //AClasses := LClasses;
end;

function TConfusionMatrix.Calculate(const ATestOutputData: IList<TDoubleArray>; const APredictedData: IList<TDoubleArray>): IConfusionMatrix;
var
  LClasses: array of string;
begin
  Result := Self;
  Self
    { configura a matrix }
    .Configure(ATestOutputData, APredictedData)
    { recupera as classes }
    .GetClasses(ATestOutputData, LClasses);

  { popula a matriz }

end;

{ TAccuracy }

class function TAccuracy.New: IMetric;
begin
  Result := TAccuracy.Create;
end;

function TAccuracy.Calculate(const ATestOutputData, APredictedData: IList<TDoubleArray>): IMetric;
var
  LCount: Double;
  LTestValue, LPredictValue: string;
begin
  Result := Self;
  FCount := 0;
  LCount := ATestOutputData.Count;
  if LCount > 0 then
  begin
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
  end
  else
  begin
    LCount := 1;
  end;
  { calcula o percentual da acurácia }
  FValue := FCount / LCount;
end;

function TAccuracy.Count: Integer;
begin
  Result := FCount;
end;

function TAccuracy.Value: Double;
begin
  Result := FValue;
end;

end.

