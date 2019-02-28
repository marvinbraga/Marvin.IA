unit Marvin.PoC.IA.Iris;

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
  Marvin.Core.IA.Connectionist.Classifier;

type
  ISize = interface
    ['{2E5ABDA4-D935-422A-ABA3-94B756BF2740}']
    function GetLength: Double;
    function GetWidth: Double;
    procedure SetWidth(const Value: Double);
    procedure SetLength(const Value: Double);
    property Width: Double read GetWidth write SetWidth;
    property Length: Double read GetLength write SetLength;
  end;

  IIrisData = interface
    ['{1920EDEF-11CB-4126-8F77-45C85E49F243}']
    function Petal: ISize;
    function Sepal: ISize;
    function ClassName: string;
    function ClassValue: Double;
    function GetInputValues: TDoubleArray;
    function GetOutputValues: TDoubleArray;
  end;

implementation

end.
