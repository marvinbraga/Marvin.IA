unit Marvin.Core.IA.Connectionist.TestSplitter;

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
  { marvin }
  Marvin.Core.InterfacedList,
  Marvin.Core.IA.Connectionist.Classifier;

type
  ITestSplitter = interface
    ['{BC4B3C35-12B8-4EE1-9349-1F9EA14D84C7}']
    function ExecuteSplit(out AInputTrainData: IList<TDoubleArray>; out AOutputTrainData: IList<TDoubleArray>; out AInputTestData: IList<TDoubleArray>; out AOutputTestData: IList<TDoubleArray>): ITestSplitter;
  end;

implementation

end.
