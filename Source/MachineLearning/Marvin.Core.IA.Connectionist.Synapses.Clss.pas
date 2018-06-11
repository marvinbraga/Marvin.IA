unit Marvin.Core.IA.Connectionist.Synapses.Clss;

{
  MIT License

  Copyright (c) 2018 Marcus Vinicius D. B. Braga

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
  Marvin.Core.IA.Connectionist.Synapses,
  Marvin.Core.IA.Connectionist.Synapse,
  Marvin.Core.IA.Connectionist.Synapse.Clss,
  Marvin.Core.InterfacedList;

type
  TFactorySynapses = class
  public
    class function New: IList<ISynapse>;
  end;

implementation

type
  TSynapses = class sealed(TCustomList<ISynapse>)
  private
    type
      TSynapseComparer = class(TComparer<ISynapse>)
      public
        function Compare(ALeft, ARight: ISynapse): Integer; reintroduce;
      end;
  public
    constructor Create; reintroduce;
    class function New: IList<ISynapse>;
    function ToString: string; override;
  end;

{ TSynapses }

constructor TSynapses.Create;
begin
  inherited Create(TSynapseComparer.Create);
end;

class function TSynapses.New: IList<ISynapse>;
begin
  Result := TSynapses.Create;
end;

function TSynapses.ToString: string;
begin
  Result := 'TSynapses';
end;

{ TFactorySynapses }

class function TFactorySynapses.New: IList<ISynapse>;
begin
  Result := TSynapses.New;
end;

{ TSynapses.TSynapseComparer }

function TSynapses.TSynapseComparer.Compare(ALeft, ARight: ISynapse): Integer;
begin
  Result := -1;
  if ALeft.Id = ARight.Id then
  begin
    Result := 0;
  end;
end;

end.

