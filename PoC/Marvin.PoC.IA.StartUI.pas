unit Marvin.PoC.IA.StartUI;

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
    function ShowOriginalData(const AText: string): TFormStart;
    function ShowConvertedData(const AInputData: IList<TDoubleArray>; const AOutputData: IList<TDoubleArray>): TFormStart;
  public
  end;

var
  FormStart: TFormStart;

implementation

uses
  { marvin }
  Marvin.Core.IA.Connectionist.MLPClassifier.Clss,
  Marvin.PoC.IA.DataConverter,
  Marvin.PoC.IA.DataConverter.Clss;

{$R *.dfm}

procedure TFormStart.ButtonLoadFileClick(Sender: TObject);
begin
  Self.GetIrisData;
end;

function TFormStart.GetIrisData: TFormStart;
var
  LStream: TStringStream;
  LIrisInputData, LIrisOutputData: IList<TDoubleArray>;
begin
  Result := Self;
  if DlgData.Execute then
  begin
    LStream := TStringStream.Create('', TEncoding.UTF8);
    try
      { load pure data file }
      LStream.LoadFromFile(DlgData.FileName);

      { recupera os dados convertidos }
      TIrisDataConverter.New(LStream.DataString).Execute(LIrisInputData, LIrisOutputData);
      { faz o split dos dados para treino e teste }

      MemoData.Lines.BeginUpdate;
      try
        Self
          { exibe os dados originais }
          .ShowOriginalData(LStream.DataString)
          { exibe os dados convertidos }
          .ShowConvertedData(LIrisInputData, LIrisOutputData);
      finally
        MemoData.Lines.EndUpdate;
      end;
    finally
      LStream.Free;
    end;
  end;
end;

function TFormStart.ShowConvertedData(const AInputData,
  AOutputData: IList<TDoubleArray>): TFormStart;
var
  LInput, LOutput: TDoubleArray;
begin
  Result := Self;
  { recupera os dados }
  LInput := AInputData.First;
  LOutput := AOutputData.First;

  MemoData.Lines.Add('');
  MemoData.Lines.Add('IRIS CONVERTED DATA');
  MemoData.Lines.Add('-------------------');
  MemoData.Lines.Add('');
  { percorre todos os dados }
  while not(AInputData.Eof) do
  begin
    { exibe }
    MemoData.Lines.Add(Format('Inputs: [%3.8f, %3.8f, %3.8f, %3.8f]; Outputs: [%3.8f, %3.8f, %3.8f]', [
      LInput[0], LInput[1], LInput[2], LInput[3],
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
  MemoData.Lines.Insert(0, 'IRIS ORIGINAL DATA');
  MemoData.Lines.Insert(1, '------------------');
  MemoData.Lines.Insert(2, '');
end;

end.

