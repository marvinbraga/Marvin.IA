unit Marvin.Core.IA.Connectionist.Layers.Clss;

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
  System.Generics.Defaults,
  System.Generics.Collections,
  { marvin }
  Marvin.Core.IA.Connectionist.Layers,
  Marvin.Core.IA.Connectionist.Layer,
  Marvin.Core.IA.Connectionist.Layer.Clss,
  Marvin.Core.InterfacedList;

type
  TFactoryLayers = class
  public
    class function New: IList<ILayer>;
  end;

implementation

type
  TLayers = class sealed(TCustomList<ILayer>)
  public
    type
      TLayerComparer = class(TComparer<ILayer>)
      public
        function Compare(ALeft, ARight: ILayer): Integer; reintroduce;
      end;
  public
    constructor Create; reintroduce;
    class function New: IList<ILayer>;
    function ToString: string; override;
  end;

{ TLayers }

constructor TLayers.Create;
begin
  inherited Create(TLayerComparer.Create);
end;

class function TLayers.New: IList<ILayer>;
begin
  Result := TLayers.Create;
end;

function TLayers.ToString: string;
begin
  Result := 'TLayers';
end;

{ TFactoryLayers }

class function TFactoryLayers.New: IList<ILayer>;
begin
  Result := TLayers.New;
end;

{ TLayers.TLayerComparer }

function TLayers.TLayerComparer.Compare(ALeft, ARight: ILayer): Integer;
begin
  Result := -1;
  if ALeft.Id = ARight.Id then
  begin
    Result := 0;
  end;
end;

end.
