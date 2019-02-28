unit Marvin.Core.IA.Connectionist.Classifier;

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
  { marvin }
  Marvin.Core.InterfacedList,
  Marvin.Core.IA.Connectionist.Activation;

type
  TDoubleArray = array of Double;
  TDoubleMatrix = array of TDoubleArray;

  THelperDoubleArray = record helper for TDoubleArray
    function Clone: TDoubleArray;
    function IsMaxValue(const AValue: Double): Double;
    function ToBinaryValue: TDoubleArray;
    function ToString: string;
  end;

  IClassifier = interface
    ['{FF28CF74-02E0-437A-96A7-F45B404E8439}']
    function ConfigureClassifier: IClassifier;
    function SetHiddenLayerSizes(const AHiddenLayerCount: Word; const AHiddenLayerNeuronsCount: array of Word): IClassifier;
    function SetActivation(const AActivation: IActivation): IClassifier;
    function SetLearningFile(const ALearningFile: string): IClassifier;
    function SetLearning(const ALearning: Double): IClassifier;
    function SetMaxIter(const AMaxIter: Integer): IClassifier;
    function SetMomentum(const AMomentum: Double): IClassifier;
    function Cost: Double;
    function Epochs: Integer;
    function EpochsCovered: Integer;
    { treino }
    function Fit(const ATrainDataInputs: TDoubleArray; const ATrainDataOutputs: TDoubleArray): IClassifier; overload;
    function Fit(const ATrainDataInputs: IList<TDoubleArray>; const ATrainDataOutputs: IList<TDoubleArray>): IClassifier; overload;
    function Fit(const ATrainDataInputs: IList<IList<Double>>; const ATrainDataOutputs: IList<IList<Double>>): IClassifier; overload;
    { teste }
    function Predict(const ATestDataInputs: TDoubleArray; out APredictedDataOutputs: TDoubleArray): IClassifier; overload;
    function Predict(const ATestDataInputs: IList<TDoubleArray>; out APredictedDataOutputs: IList<TDoubleArray>): IClassifier; overload;
    function Predict(const ATestDataInputs: IList<IList<Double>>; out APredictedDataOutputs: IList<IList<Double>>): IClassifier; overload;
  end;

implementation

uses
  { embarcadero }
  System.Math,
  System.SysUtils;

{ THelperDoubleArray }

function THelperDoubleArray.Clone: TDoubleArray;
var
  LIndex: Integer;
begin
  Result := nil;
  SetLength(Result, Length(Self));
  for LIndex := Low(Self) to High(Self) do
  begin
    Result[LIndex] := Self[LIndex];
  end;
end;

function THelperDoubleArray.IsMaxValue(const AValue: Double): Double;
begin
  Result := 0;
  if MaxValue(Self) = AValue then
  begin
    Result := 1;
  end;
end;

function THelperDoubleArray.ToBinaryValue: TDoubleArray;

  procedure LToBinary(const AMax, AMin: Double);
  var
    LIndex: Integer;
  begin
    for LIndex := Low(Self) to High(Self) do
    begin
      if Self[LIndex] = MaxValue(Self) then
        Self[LIndex] := AMax
      else
        Self[LIndex] := AMin;
    end;
  end;

begin
  Result := Self;
  LToBinary(MaxDouble, MinDouble);
  LToBinary(1, 0);
end;

function THelperDoubleArray.ToString: string;
var
  LIndex: Integer;
begin
  Result := EmptyStr;
  for LIndex := Low(Self) to High(Self) do
  begin
    Result := Result + Self[LIndex].ToString + ', ';
  end;
  Result := Result.Trim;
  Delete(Result, Length(Result), 1);
  Result := Format('[%s]', [Result]);
end;

end.
