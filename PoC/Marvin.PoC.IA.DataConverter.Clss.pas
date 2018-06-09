unit Marvin.PoC.IA.DataConverter.Clss;

interface

uses
  System.Classes,
  Marvin.Core.InterfacedList,
  Marvin.Core.IA.Connectionist.Classifier,
  Marvin.PoC.IA.DataConverter;

type
  TIrisDataConverter = class(TInterfacedObject, IDataConverter)
  private
    FData: TStrings;
  protected
    function Execute(out AInputValues: IList<TDoubleArray>; out AOutputValues: IList<TDoubleArray>): IDataConverter;
    class procedure Split(const ARecord: string; const AList: TStringList; const ADelimitador: Char = ';');
  public
    constructor Create(const ADataText: string);
    destructor Destroy; override;
    class function New(const ADataText: string): IDataConverter;
  end;

implementation

uses
  System.SysUtils,
  Marvin.PoC.IA.Iris,
  Marvin.PoC.IA.Iris.Clss;


{ TIrisDataConverter }

constructor TIrisDataConverter.Create(const ADataText: string);
begin
  inherited Create;
  FData := TStringList.Create;
  FData.Text := StringReplace(ADataText, '.', FormatSettings.DecimalSeparator, [rfReplaceAll]);
end;

destructor TIrisDataConverter.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

function TIrisDataConverter.Execute(out AInputValues: IList<TDoubleArray>; out AOutputValues: IList<TDoubleArray>): IDataConverter;
var
  LIndex: Integer;
  LSplitedRecord: TStringList;
  LIrisData: IIrisData;
begin
  Result := Self;
  AInputValues := TCustomList<TDoubleArray>.Create;
  AOutputValues := TCustomList<TDoubleArray>.Create;
  { transforma os dados em valores Double }
  LSplitedRecord := TStringList.Create;
  try
    for LIndex := 0 to FData.Count - 1 do
    begin
      if not(FData[LIndex].Trim.IsEmpty) then
      begin
        { quebra os dados da string }
        TIrisDataConverter.Split(FData[LIndex], LSplitedRecord);
        { cria o dado }
        LIrisData := TIrisData.New(LSplitedRecord[0].ToDouble, LSplitedRecord[1].ToDouble, LSplitedRecord[2].ToDouble, LSplitedRecord[3].ToDouble,
            LSplitedRecord[4]);
        { converte em TDoubleArray }
        AInputValues.Add(LIrisData.GetInputValues);
        AOutputValues.Add(LIrisData.GetOutputValues);
      end;
    end;
  finally
    LSplitedRecord.Free;
  end;
end;

class function TIrisDataConverter.New(const ADataText: string): IDataConverter;
begin
  Result := TIrisDataConverter.Create(ADataText);
end;

class procedure TIrisDataConverter.Split(const ARecord: string; const AList: TStringList; const ADelimitador: Char = ';');
begin
  AList.Text := '';
  AList.Clear;
  AList.StrictDelimiter := True;
  AList.Delimiter := ADelimitador;
  AList.QuoteChar := '"';
  AList.DelimitedText := ARecord;
end;

end.

