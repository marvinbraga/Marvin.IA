unit Marvin.Core.IA.Connectionist.Cost.Factory.Clss;

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
  Marvin.Core.IA.Connectionist.Cost,
  Marvin.Core.IA.Connectionist.Layer;

type
  TCostType = (Default, MeanAbsoluteError, MeanSquaredError, AbsoluteMedianError);

  TCostTypeHelper = record helper for TCostType
  public
    function New(const ALayer: ILayer): ICost;
  end;

implementation

uses
  Marvin.Core.IA.Connectionist.Cost.Default.Clss,
  Marvin.Core.IA.Connectionist.Exceptions.Clss;

{ TCostTypeHelper }

function TCostTypeHelper.New(const ALayer: ILayer): ICost;
begin
  case Self of
    Default: Result := TCostDefault.New(ALayer);
    MeanAbsoluteError: raise EInvalidFactory.Create;
    MeanSquaredError: raise EInvalidFactory.Create;
    AbsoluteMedianError: raise EInvalidFactory.Create;
  end;
end;

end.
