unit Marvin.Core.IA.Connectionist.Neuron;

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
