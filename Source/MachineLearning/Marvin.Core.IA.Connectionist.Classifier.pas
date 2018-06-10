unit Marvin.Core.IA.Connectionist.Classifier;

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
  { embarcadero }
  System.Generics.Collections,
  { marvin }
  Marvin.Core.InterfacedList,
  Marvin.Core.IA.Connectionist.Activation;

type
  TDoubleArray = array of Double;

  THelperDoubleArray = record helper for TDoubleArray
    function Clone: TDoubleArray;
  end;

  IClassifier = interface
    ['{FF28CF74-02E0-437A-96A7-F45B404E8439}']
    function ConfigureClassifier: IClassifier;
    function SetHiddenLayerSizes(const AHiddenLayerSizes: Integer): IClassifier;
    function SetActivation(const AActivation: IActivation): IClassifier;
    function SetLearningFile(const ALearningFile: string): IClassifier;
    function SetLearning(const ALearning: Double): IClassifier;
    function SetMaxIter(const AMaxIter: Integer): IClassifier;
    function SetMomentum(const AMomentum: Double): IClassifier;
    function Cost: Double;
    function Epoch: Integer;
    { treino }
    function Fit(const ATrainDataInputs: TDoubleArray; const ATrainDataOutputs: TDoubleArray): IClassifier; overload;
    function Fit(const ATrainDataInputs: TList<TDoubleArray>; const ATrainDataOutputs: TList<TDoubleArray>): IClassifier; overload;
    function Fit(const ATrainDataInputs: TList<TList<Double>>; const ATrainDataOutputs: TList<TList<Double>>): IClassifier; overload;
    { teste }
    function Predict(const ATestDataInputs: TDoubleArray; out APredictedDataOutputs: TDoubleArray): IClassifier; overload;
    function Predict(const ATestDataInputs: TList<TDoubleArray>; out APredictedDataOutputs: TList<TDoubleArray>): IClassifier; overload;
    function Predict(const ATestDataInputs: TList<TList<Double>>; out APredictedDataOutputs: TList<TList<Double>>): IClassifier; overload;
  end;

implementation

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

end.
