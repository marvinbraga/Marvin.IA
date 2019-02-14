unit Marvin.Utils.VCL.StyleManager;

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
  Vcl.Menus;

type
  IVclStyleManager = interface(IInterface)
    ['{2B441724-C0C1-47D2-80E6-B0D5A7DD69DF}']
  end;

  TFactoryVCLStyleManager = class
  public
    class function New(AMenuItem: TMenuItem): IVclStyleManager;
  end;

implementation

uses
  Winapi.Windows,
  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.Themes,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  System.Generics.Defaults,
  StrUtils,
  IniFiles;

resourcestring
  RS_STYLES_MENU_ITEM = 'Styles';

type
  TVCLStyleManager = class(TInterfacedObject, IVclStyleManager)
  strict private
    FMenuItem: TMenuItem;
    FCaptionMenuItem: string;
  protected
    { métodos }
    procedure LoadVCLStyles;
    procedure InternalStyleMenuClick(ASender: TObject);
    function AdjustCheckedItem(const AStyleName: string): TMenuItem;
    function GetActiveStyle(AIniFile: string; var AActiveStyle: string): Boolean;
    procedure StoreActiveStyle(AIniFile: string; const AActiveStyle: string);
    procedure SetMenuItemCaption(const AStyleName: string);
  public
    constructor Create(AMenuItem: TMenuItem); virtual;
    destructor Destroy; override;
  end;

{ TFactoryVCLStyleManager }

class function TFactoryVCLStyleManager.New(AMenuItem: TMenuItem): IVclStyleManager;
begin
  Result := TVCLStyleManager.Create(AMenuItem);
end;

{ TVCLStyleManager }

constructor TVCLStyleManager.Create(AMenuItem: TMenuItem);
begin
  Assert(Assigned(AMenuItem), 'The Menu Item can not be NIL.');
  FMenuItem := AMenuItem;
  FCaptionMenuItem := FMenuItem.Caption;
  if FCaptionMenuItem.Trim.IsEmpty then
  begin
    FCaptionMenuItem := RS_STYLES_MENU_ITEM;
  end;
  { carrega os estilos }
  Self.LoadVCLStyles;
end;

destructor TVCLStyleManager.Destroy;
begin
  FMenuItem := nil;
  inherited;
end;

function TVCLStyleManager.GetActiveStyle(AIniFile: string; var AActiveStyle: string): Boolean;
var
  LIniFile: TIniFile;
begin
  { cria o arquivo INI }
  LIniFile := TIniFile.Create(AIniFile);
  try
    { verifica se encontrou o estilo }
    Result := LIniFile.ValueExists('Appearances', 'ActiveStyle');
    if Result then
      { recupera o nome do estilo }
      AActiveStyle := LIniFile.ReadString('Appearances', 'ActiveStyle', '');
  finally
    LIniFile.Free;
  end;
end;

procedure TVCLStyleManager.LoadVCLStyles;
var
  LStyleNames: TArray<string>;
  LStylesMenuItem, LItem: TMenuItem;
  LActiveStyle, LStyleName: string;
begin
  { recupera o estilo do arquivo INI e existe um estilo informado }
  if (Self.GetActiveStyle(ChangeFileExt(ParamStr(0), '.ini'), LActiveStyle) and (LActiveStyle <> EmptyStr)) then
  begin
    { atualiza o estilo com o nome recuperado }
    TStyleManager.TrySetStyle(LActiveStyle, False);
    Self.SetMenuItemCaption(LActiveStyle);
  end;

  { recupera os estilos disponíveis }
  LStyleNames := TStyleManager.StyleNames;
  { ordena os nomes dos estilos }
  TArray.Sort<string>(LStyleNames, TStringComparer.Ordinal);
  { percorre os estilos recuperados }
  for LStyleName in LStyleNames do
  begin
    { cria e configura os menus }
    LStylesMenuItem := TMenuItem.Create(FMenuItem);
    LStylesMenuItem.Caption := LStyleName;
    LStylesMenuItem.OnClick := InternalStyleMenuClick;
    FMenuItem.Add(LStylesMenuItem);
    if (LActiveStyle.Trim.IsEmpty) then
    begin
      if LStyleName.Contains('Windows') then
      begin
        LActiveStyle := LStyleName;
      end;
    end;
  end;

  LItem := Self.AdjustCheckedItem(LActiveStyle);
  if Assigned(LItem) then
  begin
    InternalStyleMenuClick(LItem);
  end;
end;

function TVCLStyleManager.AdjustCheckedItem(const AStyleName: string): TMenuItem;
var
  LMenuItem: TMenuItem;
  LStyleName: string;
begin
  Result := nil;
  { percorre todos os itens do menu }
  for LMenuItem in FMenuItem do
  begin
    LStyleName := ReplaceStr(LMenuItem.Caption, '&', '');
    { ajusta o check para o estilo selecionado }
    LMenuItem.Checked := (LStyleName = AStyleName);
    if LMenuItem.Checked then
    begin
      Result := LMenuItem;
    end;
  end;
end;

procedure TVCLStyleManager.SetMenuItemCaption(const AStyleName: string);
begin
  { ajusta o item do menu com o nome do estilo selecionado }
  FMenuItem.Caption := Format('[%S] ' + FCaptionMenuItem, [AStyleName]);
end;

procedure TVCLStyleManager.StoreActiveStyle(AIniFile: string; const AActiveStyle: string);
var
  LIniFile: TIniFile;
begin
  LIniFile := TIniFile.Create(AIniFile);
  try
    { guarda no arquivo INI o nome do estilo selecionado }
    LIniFile.WriteString('Appearances', 'ActiveStyle', AActiveStyle);
  finally
    LIniFile.Free;
  end;
end;

procedure TVCLStyleManager.InternalStyleMenuClick(ASender: TObject);
var
  LMenuItem: TMenuItem;
  LStyleName: string;
begin
  if (ASender is TMenuItem) then
  begin
    LMenuItem := (ASender as TMenuItem);
    { seleciona um novo estilo }
    LStyleName := ReplaceStr(LMenuItem.Caption, '&', '');
    { altera o estilo }
    TStyleManager.TrySetStyle(LStyleName);
    { ajusta o item principal }
    Self.SetMenuItemCaption(LStyleName);
    { seleciona o item do menu }
    Self.AdjustCheckedItem(LStyleName);
    { salva o estilo atual no arquivo INI }
    Self.StoreActiveStyle(ChangeFileExt(ParamStr(0), '.ini'), TStyleManager.ActiveStyle.Name);
  end;
end;

end.

