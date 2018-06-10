unit Marvin.Core.IA.Connectionist.Neuron;

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

type
  { interface para neurônio }
  INeuron = interface
    ['{8095EF29-8156-4BFE-B8E9-C05114B56DCD}']
    function Clear: INeuron;
    function Assign(const ANeuron: INeuron): INeuron;
    { properties }
    function SetValue(const AValue: Double): INeuron;
    function SetDelta(const ADelta: Double): INeuron;
    function SetTarget(const ATarget: Double): INeuron;
    function SetMaxValue(const AMaxValue: Double): INeuron;
    function SetMinValue(const AMinValue: Double): INeuron;
    function SetId(const AId: string): INeuron;
    function Value: Double;
    function Delta: Double;
    function Target: Double;
    function MaxValue: Double;
    function MinValue: Double;
    function Id: string;
end;

implementation

end.
