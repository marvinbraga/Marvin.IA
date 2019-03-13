unit Marvin.PoC.IA.StartUI;

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
  {marvin}
  Marvin.Core.InterfacedList,
  Marvin.Core.IA.Connectionist.Classifier,
  Marvin.Utils.VCL.StyleManager,
  {embarcadero}
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.Dialogs,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.ComCtrls,
  VclTee.TeeGDIPlus,
  VclTee.TeEngine,
  VclTee.Series,
  VclTee.TeeProcs,
  VclTee.Chart,
  VCL.WinXCtrls,
  VCL.Menus, VCLTee.TeeSpline;

type
  TFormStart = class(TForm)
    PanelToolBar: TPanel;
    ButtonLoadFile: TButton;
    pgcInfo: TPageControl;
    TabDataFile: TTabSheet;
    TabTrain: TTabSheet;
    TabTest: TTabSheet;
    MemoData: TMemo;
    LabelCabecalhoTreino: TLabel;
    MemoPredict: TMemo;
    LabelCabecalhoTeste: TLabel;
    Label1: TLabel;
    PageControlData: TPageControl;
    TabSheetCollectionData: TTabSheet;
    TabSheetGraphicsData: TTabSheet;
    PanelGraphicsData: TPanel;
    ActivityIndicator: TActivityIndicator;
    PopupMenu: TPopupMenu;
    ItemStyles: TMenuItem;
    PageControlTrain: TPageControl;
    TabSheetTrainData: TTabSheet;
    MemoTrain: TMemo;
    TabSheetTrainGraphic: TTabSheet;
    pnl1: TPanel;
    ChartTrain: TChart;
    LineCost: TLineSeries;
    TimerCost: TTimer;
    Chart1: TChart;
    ProgressBar: TProgressBar;
    Series1: TPointSeries;
    procedure ButtonLoadFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerCostTimer(Sender: TObject);
  private
    FStylesManager: IVclStyleManager;
    FMlp: IClassifier;
    FIsRunning: Boolean;
  private
    function GetIrisData: TFormStart;
    function ShowData(const AMemo: TMemo; const AInputData: IList<TDoubleArray>; const AOutputData: IList<TDoubleArray>): TFormStart;
    function ShowOriginalData(const AText: string): TFormStart;
    function ShowConvertedData(const AInputData: IList<TDoubleArray>; const AOutputData: IList<TDoubleArray>): TFormStart;
    function ShowTreinData(const AInputData: IList<TDoubleArray>; const AOutputData: IList<TDoubleArray>): TFormStart;
    function ShowTestData(const AInputData: IList<TDoubleArray>; const AOutputData: IList<TDoubleArray>): TFormStart;
    function ShowPredictedData(const AInputData: IList<TDoubleArray>; const AOutputData: IList<TDoubleArray>; const AFitCost: Double; const APredictCost: Double): TFormStart;
    function ShowResume(const AMlp: IClassifier; const ATestOutputData: IList<TDoubleArray>; const APredictedData: IList<TDoubleArray>): TFormStart;
    procedure ExecutePredict(const AFileName: string);
    procedure InitProcessIndicators;
    procedure ReleaseProcessIndicators;
  public
  end;

var
  FormStart: TFormStart;

implementation

uses
  System.Threading,
  {marvin}
  Marvin.PoC.IA.DataConverter,
  Marvin.Core.IA.Connectionist.Metric,
  Marvin.PoC.IA.DataConverter.Clss,
  Marvin.Core.IA.Connectionist.ActivateFunction.Clss,
  Marvin.Core.IA.Connectionist.MLPClassifier.Clss,
  Marvin.Core.IA.Connectionist.TestSplitter.Clss,
  Marvin.Core.IA.Connectionist.Metric.Clss;

{$R *.dfm}

procedure TFormStart.ButtonLoadFileClick(Sender: TObject);
begin
  Self.GetIrisData;
end;

function TFormStart.GetIrisData: TFormStart;
var
  LFileName: TFileName;
  LExecute: Boolean;
begin
  Result := Self;
  MemoData.Lines.Clear;
  MemoTrain.Lines.Clear;
  pgcInfo.ActivePage := TabTrain;

{$WARNINGS OFF}
  with TFileOpenDialog.Create(nil) do
  begin
    try
      var LFileType := FileTypes.Add;
      LFileType.FileMask := '*.csv';
      LFileType := FileTypes.Add;
      LFileType.FileMask := '*.*';
      LExecute := Execute;
      if LExecute then
      begin
        LFileName := FileName;
      end;
    finally
      Free;
    end;
  end;
{$WARNINGS ON}
  if LExecute then
  begin
    TTask.Run(
      procedure
      begin
        Self.ExecutePredict(LFileName);
      end);
  end;
end;

procedure TFormStart.InitProcessIndicators;
begin
  ProgressBar.Visible := True;
  ProgressBar.Position := 0;
  ActivityIndicator.BringToFront;
  ActivityIndicator.Animate := True;
  Screen.Cursor := crHourGlass;
  LineCost.Clear;
  TimerCost.Enabled := True;
end;

procedure TFormStart.ReleaseProcessIndicators;
begin
  TimerCost.Enabled := False;
  Screen.Cursor := crDefault;
  ActivityIndicator.Animate := False;
  ActivityIndicator.SendToBack;
  ProgressBar.Visible := False;
end;

function TFormStart.ShowData(const AMemo: TMemo; const AInputData: IList<TDoubleArray>; const AOutputData: IList<TDoubleArray>): TFormStart;
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
    AMemo.Lines.Add(Format('Inputs: [%3.8f, %3.8f, %3.8f, %3.8f]; Outputs: [%3.8f, %3.8f, %3.8f]', [LInput[0], LInput[1], LInput[2], LInput[3], LOutput[0], LOutput[1], LOutput[2]]));
    { recupera os dados }
    LInput := AInputData.MoveNext;
    LOutput := AOutputData.MoveNext;
  end;
end;

function TFormStart.ShowOriginalData(const AText: string): TFormStart;
begin
  Result := Self;
  MemoData.Lines.Text := AText.Trim;
  MemoData.Lines.Insert(0, Format('IRIS ORIGINAL DATA: (%d)', [MemoData.Lines.Count]));
  MemoData.Lines.Insert(1, '------------------');
  MemoData.Lines.Insert(2, '');
end;

function TFormStart.ShowPredictedData(const AInputData: IList<TDoubleArray>; const AOutputData: IList<TDoubleArray>; const AFitCost: Double; const APredictCost: Double): TFormStart;
begin
  Result := Self;
  MemoPredict.Lines.Add('');
  MemoPredict.Lines.Add(Format('IRIS PREDICTED DATA: (%d)', [AInputData.Count]));
  MemoPredict.Lines.Add('-------------------');
  MemoPredict.Lines.Add('');
  Self.ShowData(MemoPredict, AInputData, AOutputData);
  MemoPredict.Lines.Add('');
  MemoPredict.Lines.Add(Format('FIT COST .....: (%2.8f)', [AFitCost]));
  MemoPredict.Lines.Add(Format('PREDICT COST .: (%2.8f)', [APredictCost]));
end;

function TFormStart.ShowResume(const AMlp: IClassifier; const ATestOutputData: IList<TDoubleArray>; const APredictedData: IList<TDoubleArray>): TFormStart;
const
  LC_CORRECT: string = 'Correct';
  LC_INCORRECT: string = '<-- INCORRECT';
var
  LTestData, LPredictedData: TDoubleArray;
  LResult: string;
  LAccuracy: IMetric;
begin
  Result := Self;
  MemoPredict.Lines.Add('');
  MemoPredict.Lines.Add(Format('RESUME: (%d)', [ATestOutputData.Count]));
  MemoPredict.Lines.Add('------');
  MemoPredict.Lines.Add('');

  LTestData := ATestOutputData.First;
  LPredictedData := APredictedData.First;
  while not (ATestOutputData.Eof) do
  begin
    LResult := LC_INCORRECT;
    if SameText(LTestData.ToString, LPredictedData.ToString) then
    begin
      LResult := LC_CORRECT;
    end;
    MemoPredict.Lines.Add(Format('%d: %s, %s (%s)', [ATestOutputData.Position + 1, LTestData.ToString, LPredictedData.ToString, LResult]));
    { recupera o próximo dado }
    LTestData := ATestOutputData.MoveNext;
    LPredictedData := APredictedData.MoveNext;
  end;

  LAccuracy := TAccuracy.New.Calculate(ATestOutputData, APredictedData);
  MemoPredict.Lines.Add('');
  MemoPredict.Lines.Add(Format('Accuracy .....: Count = %d, Value = %2.8f', [LAccuracy.Count, LAccuracy.Value]));
  MemoPredict.Lines.Add(Format('Epochs .......: %d', [AMlp.Epochs]));
  MemoPredict.Lines.Add(Format('Epochs Covered: %d', [AMlp.EpochsCovered]));
end;

procedure TFormStart.ExecutePredict(const AFileName: string);
var
  LStream: TStringStream;
  LIrisInputData, LIrisOutputData: IList<TDoubleArray>;
  LTreinInputData, LTreinOutputData: IList<TDoubleArray>;
  LTestInputData, LTestOutputData: IList<TDoubleArray>;
  LPredictedOutputData: IList<TDoubleArray>;
  LPredictCost, LFitCost: Double;
begin
  TThread.Queue(TThread.CurrentThread,
    procedure
    begin
      Self.InitProcessIndicators;
    end);
  try
    LStream := TStringStream.Create('', TEncoding.UTF8);
    try
      { load pure data file }
      LStream.LoadFromFile(AFileName);
      { transforma os dados originais para o formato compatível e ajustado }
      TIrisDataConverter.New(LStream.DataString).Execute(LIrisInputData, LIrisOutputData);
      { faz o split dos dados para treino e teste }
      TTestSplitter.New(LIrisInputData, LIrisOutputData, 0.3).ExecuteSplit(LTreinInputData, LTreinOutputData, LTestInputData, LTestOutputData);
      { cria o classificardor }
      FMlp := TMLPClassifier.New(TSigmoidActivation.New, [8, 8], 0.9, 0.9, 5000);
      ProgressBar.Max := FMlp.Epochs;
      LFitCost := FMlp.Fit(LTreinInputData, LTreinOutputData).Cost;
      LPredictCost := FMlp.Predict(LTestInputData, LPredictedOutputData).Cost;

      TThread.Queue(TThread.CurrentThread,
        procedure
        begin
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
              .ShowResume(FMlp, LTestOutputData, LPredictedOutputData);
          finally
            MemoData.Lines.EndUpdate;
          end;
        end);
    finally
      LStream.Free;
    end;
  finally
    TThread.Queue(TThread.CurrentThread,
      procedure
      begin
        Self.ReleaseProcessIndicators;
      end);
  end;
end;

procedure TFormStart.FormCreate(Sender: TObject);
begin
  ActivityIndicator.SendToBack;
  FStylesManager := TFactoryVCLStyleManager.New(ItemStyles);
  LineCost.Clear;
  ProgressBar.Visible := False;
  FIsRunning := False;
end;

procedure TFormStart.FormDestroy(Sender: TObject);
begin
  FMlp := nil;
  FStylesManager := nil;
end;

procedure TFormStart.FormResize(Sender: TObject);
begin
  ActivityIndicator.Top := (FormStart.Height - ActivityIndicator.Height) div 2;
  ActivityIndicator.Left := (FormStart.Width - ActivityIndicator.Width) div 2;
end;

function TFormStart.ShowConvertedData(const AInputData, AOutputData: IList<TDoubleArray>): TFormStart;
begin
  Result := Self;
  MemoData.Lines.Add('');
  MemoData.Lines.Add(Format('IRIS CONVERTED DATA: (%d)', [AInputData.Count]));
  MemoData.Lines.Add('-------------------');
  MemoData.Lines.Add('');
  Self.ShowData(MemoData, AInputData, AOutputData);
end;

function TFormStart.ShowTestData(const AInputData, AOutputData: IList<TDoubleArray>): TFormStart;
begin
  Result := Self;
  MemoPredict.Lines.Add('');
  MemoPredict.Lines.Add(Format('IRIS TEST DATA: (%d)', [AInputData.Count]));
  MemoPredict.Lines.Add('--------------');
  MemoPredict.Lines.Add('');
  Self.ShowData(MemoPredict, AInputData, AOutputData);
end;

function TFormStart.ShowTreinData(const AInputData, AOutputData: IList<TDoubleArray>): TFormStart;
begin
  Result := Self;
  MemoTrain.Lines.Add('');
  MemoTrain.Lines.Add(Format('IRIS TREIN DATA: (%d)', [AInputData.Count]));
  MemoTrain.Lines.Add('---------------');
  MemoTrain.Lines.Add('');
  Self.ShowData(MemoTrain, AInputData, AOutputData);
end;

procedure TFormStart.TimerCostTimer(Sender: TObject);
begin
  if not FIsRunning then
  begin
    FIsRunning := True;
    try
      var LEpochs := FMlp.EpochsCovered;
      LineCost.AddXY(LEpochs, FMlp.Cost, '');
      ProgressBar.Position := LEpochs;
      ProgressBar.Update;
    finally
      FIsRunning := False;
    end;
  end;
end;

end.

