unit Marvin.Core.IA.Connectionist.Metric.Clss;

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
  System.Classes,
  { marvin }
  Marvin.Core.InterfacedList,
  Marvin.Core.IA.Connectionist.Classifier,
  Marvin.Core.IA.Connectionist.Metric;

implementation

type
  TConfusionMatrix = class(TInterfacedObject, IConfusionMatrix)
  private
    FConfusionMatrix: TDoubleMatrix;
  protected
    function Calculate(const ATestOutputData: IList<TDoubleArray>; const APredictedData: IList<TDoubleArray>): IConfusionMatrix;
    function Matrix: TDoubleMatrix;
  public
    constructor Create;
    class function New: IConfusionMatrix;
    destructor Destroy; override;
  end;

{ TConfusionMatrix }

constructor TConfusionMatrix.Create;
begin
  inherited Create;
end;

destructor TConfusionMatrix.Destroy;
begin
  inherited;
end;

function TConfusionMatrix.Matrix: TDoubleMatrix;
begin
  Result := FConfusionMatrix;
end;

class function TConfusionMatrix.New: IConfusionMatrix;
begin
  Result := TConfusionMatrix.Create;
end;

function TConfusionMatrix.Calculate(const ATestOutputData: IList<TDoubleArray>; const APredictedData: IList<TDoubleArray>): IConfusionMatrix;
begin
  Result := Self;
  { informa o tamanho da matriz }
  SetLength(FConfusionMatrix, Length(ATestOutputData.Get(0)));


end;

end.

