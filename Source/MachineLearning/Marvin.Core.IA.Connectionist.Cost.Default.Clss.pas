unit Marvin.Core.IA.Connectionist.Cost.Default.Clss;

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
  { marvin }
  Marvin.Core.IA.Connectionist.Layer,
  Marvin.Core.IA.Connectionist.Cost;

type
  TCostDefault = class(TInterfacedObject, ICost)
  strict private
    FLayer: ILayer;
  protected
    function Calculate: Double;
  public
    constructor Create(const ALayer: ILayer);
    destructor Destroy; override;
    class function New(const ALayer: ILayer): ICost;
  end;

implementation

{ TCostDefault }

function TCostDefault.Calculate: Double;
var
  LIndexNeuron: Integer;
  LCost: Double;
begin
  LCost := 0;
  for LIndexNeuron := 0 to FLayer.Neurons.Count - 1 do
  begin
    { com cada neurônio }
    with FLayer.Neurons.Get(LIndexNeuron) do
    begin
      { calcula o valor para o erro }
      LCost := LCost + Sqr(Target - Value);
    end;
  end;
  { retorna o erro }
  Result := 0.5 * LCost;
end;

constructor TCostDefault.Create(const ALayer: ILayer);
begin
  inherited Create;
  FLayer := ALayer;
end;

destructor TCostDefault.Destroy;
begin
  FLayer := nil;
  inherited;
end;

class function TCostDefault.New(const ALayer: ILayer): ICost;
begin
  Result := TCostDefault.Create(ALayer);
end;

end.

