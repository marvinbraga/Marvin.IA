unit Marvin.Core.InterfacedList;

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
  { embarcadero }
  Generics.Defaults,
  Generics.Collections,
  SysUtils,
  System.Classes;

type
  { exceções }
  EInterfacedListException = class(Exception)
  public
    constructor Create; reintroduce; virtual;
  end;

  { Para referências inválidas }
  EInvalidReferences = class(EInterfacedListException)
  public
    constructor Create; override;
  end;

  { interface de lista }
  IList<T> = interface
    ['{D972F946-A9F1-41AC-96F9-A2E02F095E9A}']
    function Add(const AItem: T): IList<T>; overload;
    function Add(const AItems: IList<T>): IList<T>; overload;
    function Bof: Boolean;
    function Clear: IList<T>;
    function Count: Integer;
    function Delete(const AIndex: Integer): IList<T>;
    function Eof: Boolean;
    function First: T;
    function Get(const AIndex: Integer): T; overload;
    function Get: T; overload;
    function Last: T;
    function MoveNext: T;
    function Exchange(const AIndex1, AIndex2: Integer): IList<T>;
    function Remove(const AItem: T): IList<T>;
    function ToString: string;
    function Position: Integer;
    function Update(const AIndex: Integer; const AItem: T): IList<T>;
  end;

  { lista interfaceada }
  TInterfacedList<T> = class(TList<T>, IInterface)
  private
    FRefCount: Integer;
    procedure FreeObjects;
  public
    { construtores }
    constructor Create; reintroduce; overload;
    destructor Destroy; override;
    { métodos }
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    { IInterface Implementation }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  TCustomList<T> = class(TInterfacedObject, IList<T>)
  private
    FList: TInterfacedList<T>;
    FPosition: Integer;
  protected
    function Add(const AItem: T): IList<T>; overload;
    function Add(const AItems: IList<T>): IList<T>; overload;
    function Bof: Boolean;
    function Clear: IList<T>;
    function Count: Integer;
    function Delete(const AIndex: Integer): IList<T>;
    function Eof: Boolean;
    function First: T;
    function Get(const AIndex: Integer): T; overload;
    function Get: T; overload;
    function Last: T;
    function MoveNext: T;
    function Remove(const AItem: T): IList<T>;
    function Exchange(const AIndex1, AIndex2: Integer): IList<T>;
    function Position: Integer;
    function Update(const AIndex: Integer; const AItem: T): IList<T>;
  public
    constructor Create; reintroduce; overload;
    constructor Create(const AComparer: IComparer<T>); overload;
    destructor Destroy; override;
    function ToString: string; override;
  end;

implementation

uses
  { embarcadero }
  Winapi.Windows;

resourcestring
  RS_EInvalidReferencesMessage = 'Existem referências inválidas na lista.';

{ EInterfacedListException }

constructor EInterfacedListException.Create;
begin
  inherited Create('EInterfacedListException');
end;

{ EInvalidReferences }

constructor EInvalidReferences.Create;
begin
  inherited;
  Self.Message := RS_EInvalidReferencesMessage;
end;

{ TInterfacedList<T> }

procedure TInterfacedList<T>.AfterConstruction;
begin
  { Chama o método da super classe }
  inherited AfterConstruction;
end;

procedure TInterfacedList<T>.BeforeDestruction;
begin
  { Chama o método da super classe }
  inherited;
  { Se o mesmo ainda for diferente de zero }
  if FRefCount <> 0 then
  begin
    { Informa que existem referências inválidas para este objeto }
    raise EInvalidReferences.Create;
  end;
end;

constructor TInterfacedList<T>.Create;
begin
  inherited Create;
end;

destructor TInterfacedList<T>.Destroy;
begin
  Self.FreeObjects;
  inherited;
end;

function TInterfacedList<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := E_NOINTERFACE;
  if GetInterface(IID, Obj) then
  begin
    Result := 0
  end;
end;

function TInterfacedList<T>._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TInterfacedList<T>._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
  begin
    Free;
  end;
end;

procedure TInterfacedList<T>.FreeObjects;
var
  LIndex: Integer;
begin
  if Self.Count > 0 then
  begin
    while Self.Count > 0 do
    begin
      Self.Delete(0);
    end;
  end;
end;

{ TCustomList<T> }

function TCustomList<T>.Add(const AItem: T): IList<T>;
begin
  Result := Self;
  FPosition := FList.Add(AItem);
end;

function TCustomList<T>.Add(const AItems: IList<T>): IList<T>;
var
  LIndex: Integer;
  LItem: T;
begin
  Result := Self;
  for LIndex := 0 to AItems.Count - 1 do
  begin
    LItem := AItems.Get(LIndex);
    Self.Add(LItem);
  end;
end;

function TCustomList<T>.Bof: Boolean;
begin
  Result := (FPosition = 0);
end;

function TCustomList<T>.Clear: IList<T>;
begin
  Result := Self;
  while (FList.Count > 0) do
  begin
    FList.Delete(0);
  end;
end;

function TCustomList<T>.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TCustomList<T>.Create(const AComparer: IComparer<T>);
begin
  inherited Create;
  FList := TInterfacedList<T>.Create(AComparer);
end;

constructor TCustomList<T>.Create;
begin
  inherited Create;
  FList := TInterfacedList<T>.Create;
end;

function TCustomList<T>.Delete(const AIndex: Integer): IList<T>;
begin
  Result := Self;
  FList.Delete(AIndex);
end;

destructor TCustomList<T>.Destroy;
begin
  Self.Clear;
  FreeAndNil(FList);
  inherited;
end;

function TCustomList<T>.Eof: Boolean;
begin
  Result := (FPosition >= Self.Count);
  if Result then
  begin
    FPosition := Self.Count;
  end;
end;

function TCustomList<T>.Exchange(const AIndex1,
  AIndex2: Integer): IList<T>;
begin
  Result := Self;
  FList.Exchange(AIndex1, AIndex2);
end;

function TCustomList<T>.First: T;
begin
  FPosition := 0;
  Result := Self.Get(FPosition);
end;

function TCustomList<T>.Get: T;
begin
  Result := Self.Get(FPosition);
end;

function TCustomList<T>.Get(const AIndex: Integer): T;
begin
  Result := FList.Items[AIndex];
end;

function TCustomList<T>.Last: T;
begin
  FPosition := Self.Count - 1;
  Result := Self.Get(FPosition);
end;

function TCustomList<T>.MoveNext: T;
begin
  Inc(FPosition);
  Result := Default(T);
  if not Self.Eof then
  begin
    Result := Self.Get;
  end;
end;

function TCustomList<T>.Position: Integer;
begin
  Result := FPosition;
end;

function TCustomList<T>.Remove(const AItem: T): IList<T>;
begin
  Result := Self;
  FList.Remove(AItem);
end;

function TCustomList<T>.ToString: string;
begin
  Result := Self.ClassName;
end;

function TCustomList<T>.Update(const AIndex: Integer; const AItem: T): IList<T>;
begin
  Result := Self;
  FList.Items[AIndex] := AItem;
end;

end.
