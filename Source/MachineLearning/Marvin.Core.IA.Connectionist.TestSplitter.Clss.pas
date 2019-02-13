unit Marvin.Core.IA.Connectionist.TestSplitter.Clss;

{
  MIT License

  Copyright (c) 2019 Marcus Vinicius D. B. Braga

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
  Marvin.Core.IA.Connectionist.TestSplitter,
  Marvin.Core.IA.Connectionist.Classifier;

type
  TTestSplitter = class(TInterfacedObject, ITestSplitter)
  private
    FInputData: IList<TDoubleArray>;
    FOutputData: IList<TDoubleArray>;
    FTestSize: Double;
    FIsShuffle: Boolean;
  private
    function GetData(const AInputData: IList<TDoubleArray>; const AOutputData: IList<TDoubleArray>): TTestSplitter;
    function Shuffle(const AInputData: IList<TDoubleArray>; const AOutputData: IList<TDoubleArray>): TTestSplitter;
    function Split(const AInputData: IList<TDoubleArray>; const AOutputData: IList<TDoubleArray>;
      const AInputTrainData: IList<TDoubleArray>; const AOutputTrainData: IList<TDoubleArray>; const AInputTestData: IList<TDoubleArray>; const AOutputTestData: IList<TDoubleArray>): TTestSplitter;
  protected
    function ExecuteSplit(out AInputTrainData: IList<TDoubleArray>; out AOutputTrainData: IList<TDoubleArray>; out AInputTestData: IList<TDoubleArray>; out AOutputTestData: IList<TDoubleArray>): ITestSplitter;
  public
    constructor Create(const AInputData: IList<TDoubleArray>; const AOutputData: IList<TDoubleArray>; const ATestSize: Double = 0.3; const AIsShuffle: Boolean = True);
    class function New(const AInputData: IList<TDoubleArray>; const AOutputData: IList<TDoubleArray>; const ATestSize: Double = 0.3; const AIsShuffle: Boolean = True): ITestSplitter;
    destructor Destroy; override;
  end;

implementation

{ TTestSplitter }

constructor TTestSplitter.Create(const AInputData, AOutputData: IList<TDoubleArray>; const ATestSize: Double; const AIsShuffle: Boolean);
begin
  inherited Create;
  FInputData := AInputData;
  FOutputData := AOutputData;
  FTestSize := ATestSize;
  FIsShuffle := AIsShuffle;
end;

class function TTestSplitter.New(const AInputData, AOutputData: IList<TDoubleArray>; const ATestSize: Double; const AIsShuffle: Boolean): ITestSplitter;
begin
  Result := TTestSplitter.Create(AInputData, AOutputData, ATestSize, AIsShuffle);
end;

destructor TTestSplitter.Destroy;
begin
  FInputData := nil;
  FOutputData := nil;
  inherited;
end;

function TTestSplitter.ExecuteSplit(out AInputTrainData: IList<TDoubleArray>; out AOutputTrainData: IList<TDoubleArray>; out AInputTestData: IList<TDoubleArray>; out AOutputTestData: IList<TDoubleArray>): ITestSplitter;
var
  LShuffledInputData: IList<TDoubleArray>;
  LShuffledOutputData: IList<TDoubleArray>;
begin
  Result := Self;
  { inicializa as listas de resposta }
  AInputTrainData := TCustomList<TDoubleArray>.Create;
  AOutputTrainData := TCustomList<TDoubleArray>.Create;
  AInputTestData := TCustomList<TDoubleArray>.Create;
  AOutputTestData := TCustomList<TDoubleArray>.Create;
  { cria as listas embaralhadas }
  LShuffledInputData := TCustomList<TDoubleArray>.Create;
  LShuffledOutputData := TCustomList<TDoubleArray>.Create;

  Self
    { recupera os dados }
    .GetData(LShuffledInputData, LShuffledOutputData)
    { embaralha os dados }
    .Shuffle(LShuffledInputData, LShuffledOutputData)
    { separa os dados }
    .Split(LShuffledInputData, LShuffledOutputData, AInputTrainData, AOutputTrainData, AInputTestData, AOutputTestData);
end;

function TTestSplitter.GetData(const AInputData,
  AOutputData: IList<TDoubleArray>): TTestSplitter;
begin
  Result := Self;
  AInputData.Add(FInputData);
  AOutputData.Add(FOutputData);
end;

function TTestSplitter.Shuffle(const AInputData,
  AOutputData: IList<TDoubleArray>): TTestSplitter;
var
  LIndex, LPosition: Integer;
begin
  Result := Self;
  if FIsShuffle then
  begin
    Randomize;
    for LIndex := AInputData.Count - 1 downto 1 do
    begin
      LPosition := Random(LIndex + 1);
      AInputData.Exchange(LIndex, LPosition);
      AOutputData.Exchange(LIndex, LPosition);
    end;
  end;
end;

function TTestSplitter.Split(const AInputData, AOutputData,
  AInputTrainData, AOutputTrainData, AInputTestData,
  AOutputTestData: IList<TDoubleArray>): TTestSplitter;
var
  LInputs, LOutputs: TDoubleArray;
  LSplitPoint, LIndex: Integer;
begin
  Result := Self;
  LSplitPoint := Trunc(AInputData.Count * (1 - FTestSize)) - 1;
  LIndex := 0;
  LInputs := AInputData.First;
  LOutputs := AOutputData.First;
  while not(AInputData.Eof) do
  begin
    if LIndex <= LSplitPoint then
    begin
      { dados de treino }
      AInputTrainData.Add(LInputs.Clone);
      AOutputTrainData.Add(LOutputs.Clone);
    end
    else
    begin
      { dados de teste }
      AInputTestData.Add(LInputs.Clone);
      AOutputTestData.Add(LOutputs.Clone);
    end;
    LInputs := AInputData.MoveNext;
    LOutputs := AOutputData.MoveNext;
    Inc(LIndex);
  end;
end;

end.

