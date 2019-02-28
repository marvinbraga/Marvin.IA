unit Marvin.PoC.IA.DataConverter.Clss;

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

