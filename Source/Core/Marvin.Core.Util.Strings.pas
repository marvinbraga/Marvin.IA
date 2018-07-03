unit Marvin.Core.Util.Strings;

interface

uses
  System.Classes;

type
  TUtilStrings = class
  public
    class procedure Split(const ALine: string; const AList: TStringList; const ADelimitador: Char = ';'; const AQuoteChar: Char = '"');
  end;

implementation

{ TUtilStrings }

class procedure TUtilStrings.Split(const ALine: string; const AList: TStringList; const ADelimitador, AQuoteChar: Char);
begin
  AList.Text := '';
  AList.Clear;
  AList.StrictDelimiter := True;
  AList.Delimiter := ADelimitador;
  AList.QuoteChar := AQuoteChar;
  AList.DelimitedText := ALine;
end;

end.
