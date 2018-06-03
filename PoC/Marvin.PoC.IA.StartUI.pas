unit Marvin.PoC.IA.StartUI;

interface

uses
  { marvin }
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
  System.Generics.Collections;

{$R *.dfm}

procedure TFormStart.ButtonLoadFileClick(Sender: TObject);
var
  LStream: TStringStream;
  LIrisData: TList<TDoubleArray>;
begin
  if DlgData.Execute then
  begin
    LStream := TStringStream.Create('', TEncoding.UTF8);
    try
      { load pure data file }
      LStream.LoadFromFile(DlgData.FileName);
      MemoData.Lines.Text := LStream.DataString;
      { recupera os dados convertidos }
      TIrisDataConverter.New(LStream.DataString).Execute(LIrisData);
      try
        { faz o split dos dados para treino e teste }

      finally
        LIrisData.Free;
      end;
    finally
      FreeAndNil(LStream);
    end;
  end;
end;

end.

