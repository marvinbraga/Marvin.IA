unit Marvin.Core.IA.Connectionist.Neurons.Clss;

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
  Marvin.Core.IA.Connectionist.Neurons,
  Marvin.Core.IA.Connectionist.Neuron,
  Marvin.Core.IA.Connectionist.Neuron.Clss,
  Marvin.Core.InterfacedList;

type
  TFactoryNeurons = class
  public
    class function New: IList<INeuron>;
  end;

implementation

type
  TNeurons = class sealed(TCustomList<INeuron>)
  private
    type
      TNeuronComparer = class(TComparer<INeuron>)
      public
        function Compare(ALeft, ARight: INeuron): Integer; reintroduce;
      end;
  public
    constructor Create; reintroduce;
    class function New: IList<INeuron>;
    function ToString: string; override;
  end;

{ TNeurons }

constructor TNeurons.Create;
begin
  inherited Create(TNeuronComparer.Create);
end;

class function TNeurons.New: IList<INeuron>;
begin
  Result := TNeurons.Create;
end;

function TNeurons.ToString: string;
begin
  Result := 'TNeurons';
end;

{ TFactoryNeurons }

class function TFactoryNeurons.New: IList<INeuron>;
begin
  Result := TNeurons.New;
end;

{ TNeurons.TNeuronComparer }

function TNeurons.TNeuronComparer.Compare(ALeft, ARight: INeuron): Integer;
begin
  Result := -1;
  if (ALeft.Id = ARight.Id) then
  begin
    Result := 0;
  end;
end;

end.
