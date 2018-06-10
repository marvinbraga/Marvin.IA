unit Marvin.Core.IA.Connectionist.Synapse;

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
  Marvin.Core.IA.Connectionist.Neuron;

type
  ISynapse = interface
    ['{29979914-B17B-4746-A2DB-30517A76CF5D}']
    function Clear: ISynapse;
    function Assign(const ASynapse: ISynapse): ISynapse;
    function Init: ISynapse;
    function SetDelta(const ADelta: Double): ISynapse;
    function SetValue(const AValue: Double): ISynapse;
    function SetNeuron(const ANeuron: INeuron): ISynapse;
    function SetLinkedNeuron(const ANeuron: INeuron): ISynapse;
    function Delta: Double;
    function Value: Double;
    function Neuron: INeuron;
    function LinkedNeuron: INeuron;
    function Id: string;
  end;

implementation

end.
