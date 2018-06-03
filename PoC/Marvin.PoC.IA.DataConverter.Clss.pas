unit Marvin.PoC.IA.DataConverter.Clss;

interface

uses
  System.Classes,
  System.Generics.Collections,
  Marvin.Core.IA.Connectionist.Classifier,
  Marvin.PoC.IA.DataConverter;

type
  TIrisDataConverter = class(TInterfacedObject, IDataConverter)
  private
    FData: TStrings;
  protected
    function Execute(out AList: TList<TDoubleArray>): IDataConverter;
    class procedure Split(const ARecord: string; const AList: TStringList; const ADelimitador: Char = ';');
  public
    constructor Create(const ADataText: string);
    destructor Destroy; override;
    class function New(const ADataText: string): IDataConverter;
  end;

implementation

uses
  System.SysUtils,
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

function TIrisDataConverter.Execute(out AList: TList<TDoubleArray>): IDataConverter;
var
  LIndex: Integer;
  LSplitedRecord: TStringList;
begin
  Result := Self;
  AList := TList<TDoubleArray>.Create;
  { transforma os dados em valores Double }
  LSplitedRecord := TStringList.Create;
  try
    for LIndex := 0 to FData.Count - 1 do
    begin
      { quebra os dados da string }
      if not(FData[LIndex].Trim.IsEmpty) then
      begin
        TIrisDataConverter.Split(FData[LIndex], LSplitedRecord);
        { converte em TDoubleArray }
        AList.Add(
          TIrisData.New(
            LSplitedRecord[0].ToDouble,
            LSplitedRecord[1].ToDouble,
            LSplitedRecord[2].ToDouble,
            LSplitedRecord[3].ToDouble,
            LSplitedRecord[4])
          .ToDoubleArray);
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

