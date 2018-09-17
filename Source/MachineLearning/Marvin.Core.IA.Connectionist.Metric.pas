unit Marvin.Core.IA.Connectionist.Metric;

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
  { marvin }
  Marvin.Core.InterfacedList,
  Marvin.Core.IA.Connectionist.Classifier;

type
  IMetric = interface
    ['{55E40DD1-5B4A-4000-B7F3-54903C4EC980}']
    function Calculate(const ATestOutputData: IList<TDoubleArray>; const APredictedData: IList<TDoubleArray>): IMetric;
    function Value: Double;
    function Count: Integer;
  end;

  IConfusionMatrix = interface
    ['{9CBC2158-4B90-497B-A57F-E27BBD68FF2A}']
    function Calculate(const ATestOutputData: IList<TDoubleArray>; const APredictedData: IList<TDoubleArray>): IConfusionMatrix;
    function Matrix: TDoubleMatrix;
  end;

implementation

end.