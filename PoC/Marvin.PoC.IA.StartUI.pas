unit Marvin.PoC.IA.StartUI;

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
  { marvin }
  Marvin.Core.InterfacedList,
  Marvin.Core.IA.Connectionist.Classifier,
  { embarcadero }
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls;

type
  TFormStart = class(TForm)
    PanelToolBar: TPanel;
    ButtonLoadFile: TButton;
    DlgData: TFileOpenDialog;
    pgcInfo: TPageControl;
    TabDataFile: TTabSheet;
    TabTrain: TTabSheet;
    TabTest: TTabSheet;
    MemoData: TMemo;
    procedure ButtonLoadFileClick(Sender: TObject);
  private
    function GetIrisData: TFormStart;
    function ShowData(const AInputData: IList<TDoubleArray>; const AOutputData: IList<TDoubleArray>): TFormStart;
    function ShowOriginalData(const AText: string): TFormStart;
    function ShowConvertedData(const AInputData: IList<TDoubleArray>; const AOutputData: IList<TDoubleArray>): TFormStart;
    function ShowTreinData(const AInputData: IList<TDoubleArray>; const AOutputData: IList<TDoubleArray>): TFormStart;
    function ShowTestData(const AInputData: IList<TDoubleArray>; const AOutputData: IList<TDoubleArray>): TFormStart;
    function ShowPredictedData(const AInputData: IList<TDoubleArray>; const AOutputData: IList<TDoubleArray>; const AFitCost: Double; const APredictCost: Double): TFormStart;
    function ShowResume(const ATestOutputData: IList<TDoubleArray>; const APredictedData: IList<TDoubleArray>): TFormStart;
  public
  end;

var
  FormStart: TFormStart;

implementation

uses
  { marvin }
  Marvin.Core.IA.Connectionist.MLPClassifier.Clss,
  Marvin.PoC.IA.DataConverter,
  Marvin.PoC.IA.DataConverter.Clss,
  Marvin.Core.IA.Connectionist.TestSplitter.Clss;

{$R *.dfm}

procedure TFormStart.ButtonLoadFileClick(Sender: TObject);
var
  LCursor: TCursor;
begin
  LCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    Application.ProcessMessages;
    Self.GetIrisData;
  finally
    Screen.Cursor := LCursor;
  end;
end;

function TFormStart.GetIrisData: TFormStart;
var
  LStream: TStringStream;
  LIrisInputData, LIrisOutputData: IList<TDoubleArray>;
  LTreinInputData, LTreinOutputData: IList<TDoubleArray>;
  LTestInputData, LTestOutputData: IList<TDoubleArray>;
  LPredictedOutputData: IList<TDoubleArray>;
  LMlp: IClassifier;
  LPredictCost, LFitCost: Double;
begin
  Result := Self;
  if DlgData.Execute then
  begin
    LStream := TStringStream.Create('', TEncoding.UTF8);
    try
      { load pure data file }
      LStream.LoadFromFile(DlgData.FileName);

      { transforma os dados originais para o formato compatível e ajustado }
      TIrisDataConverter.New(LStream.DataString).Execute(LIrisInputData, LIrisOutputData);
      { faz o split dos dados para treino e teste }
      TTestSplitter.New(LIrisInputData, LIrisOutputData, 0.3).ExecuteSplit(LTreinInputData, LTreinOutputData, LTestInputData, LTestOutputData);
      { cria o classificardor }
      LMlp := TMLPClassifier.New;
      LFitCost := LMlp.Fit(LTreinInputData, LTreinOutputData).Cost;
      LPredictCost := LMlp.Predict(LTestInputData, LPredictedOutputData).Cost;

      MemoData.Lines.BeginUpdate;
      try
        Self
          { exibe os dados originais }
          .ShowOriginalData(LStream.DataString)
          { exibe os dados convertidos }
          .ShowConvertedData(LIrisInputData, LIrisOutputData)
          { exibe os dados de treino }
          .ShowTreinData(LTreinInputData, LTreinOutputData)
          { exibe os dados de teste }
          .ShowTestData(LTestInputData, LTestOutputData)
          { exibe os dados da classificação }
          .ShowPredictedData(LTestInputData, LPredictedOutputData, LFitCost, LPredictCost)
          { exibe o resumo }
          .ShowResume(LTestOutputData, LPredictedOutputData)
        ;
      finally
        MemoData.Lines.EndUpdate;
      end;
    finally
      LStream.Free;
    end;
  end;
end;

function TFormStart.ShowData(const AInputData, AOutputData: IList<TDoubleArray>): TFormStart;
var
  LInput, LOutput: TDoubleArray;
begin
  Result := Self;
  { recupera os dados }
  LInput := AInputData.First;
  LOutput := AOutputData.First;
  { percorre todos os dados }
  while not (AInputData.Eof) do
  begin
    { converte o resultado as saídas para 0 ou 1 }
    LOutput.ToBinaryValue;
    { exibe }
    MemoData.Lines.Add(Format('Inputs: [%3.8f, %3.8f, %3.8f, %3.8f]; Outputs: [%3.8f, %3.8f, %3.8f]', [
      LInput[0],  LInput[1],  LInput[2], LInput[3],
      LOutput[0], LOutput[1], LOutput[2]
    ]));
    { recupera os dados }
    LInput := AInputData.MoveNext;
    LOutput := AOutputData.MoveNext;
  end;
end;

function TFormStart.ShowOriginalData(const AText: string): TFormStart;
begin
  Result := Self;
  MemoData.Lines.Text := AText;
  MemoData.Lines.Insert(0, Format('IRIS ORIGINAL DATA: (%d)', [MemoData.Lines.Count]));
  MemoData.Lines.Insert(1, '------------------');
  MemoData.Lines.Insert(2, '');
end;

function TFormStart.ShowPredictedData(const AInputData: IList<TDoubleArray>; const AOutputData: IList<TDoubleArray>; const AFitCost: Double; const APredictCost: Double): TFormStart;
begin
  Result := Self;
  MemoData.Lines.Add('');
  MemoData.Lines.Add(Format('IRIS PREDICTED DATA: (%d)', [AInputData.Count]));
  MemoData.Lines.Add('-------------------');
  MemoData.Lines.Add('');
  Self.ShowData(AInputData, AOutputData);
  MemoData.Lines.Add('');
  MemoData.Lines.Add(Format('FIT COST .....: (%2.8f)', [AFitCost]));
  MemoData.Lines.Add(Format('PREDICT COST .: (%2.8f)', [APredictCost]));
end;

function TFormStart.ShowResume(const ATestOutputData: IList<TDoubleArray>; const APredictedData: IList<TDoubleArray>): TFormStart;
const
  LC_CORRECT: string = 'Correct';
  LC_INCORRECT: string = '<-- INCORRECT';
var
  LTestData, LPredictedData: TDoubleArray;
  LResult: string;
begin
  Result := Self;
  MemoData.Lines.Add('');
  MemoData.Lines.Add(Format('RESUME: (%d)', [ATestOutputData.Count]));
  MemoData.Lines.Add('------');
  MemoData.Lines.Add('');

  LTestData := ATestOutputData.First;
  LPredictedData := APredictedData.First;
  while not(ATestOutputData.Eof) do
  begin
    LResult := LC_INCORRECT;
    if SameText(LTestData.ToString, LPredictedData.ToString) then
    begin
      LResult := LC_CORRECT;
    end;
    MemoData.Lines.Add(Format('%d: %s, %s (%s)', [ATestOutputData.Position + 1, LTestData.ToString, LPredictedData.ToString, LResult]));
    { recupera o próximo dado }
    LTestData := ATestOutputData.MoveNext;
    LPredictedData := APredictedData.MoveNext;
  end;
end;

function TFormStart.ShowConvertedData(const AInputData, AOutputData: IList<TDoubleArray>): TFormStart;
begin
  Result := Self;
  MemoData.Lines.Add('');
  MemoData.Lines.Add(Format('IRIS CONVERTED DATA: (%d)', [AInputData.Count]));
  MemoData.Lines.Add('-------------------');
  MemoData.Lines.Add('');
  Self.ShowData(AInputData, AOutputData);
end;

function TFormStart.ShowTestData(const AInputData, AOutputData: IList<TDoubleArray>): TFormStart;
begin
  Result := Self;
  MemoData.Lines.Add('');
  MemoData.Lines.Add(Format('IRIS TEST DATA: (%d)', [AInputData.Count]));
  MemoData.Lines.Add('--------------');
  MemoData.Lines.Add('');
  Self.ShowData(AInputData, AOutputData);
end;

function TFormStart.ShowTreinData(const AInputData, AOutputData: IList<TDoubleArray>): TFormStart;
begin
  Result := Self;
  MemoData.Lines.Add('');
  MemoData.Lines.Add(Format('IRIS TREIN DATA: (%d)', [AInputData.Count]));
  MemoData.Lines.Add('---------------');
  MemoData.Lines.Add('');
  Self.ShowData(AInputData, AOutputData);
end;

end.

