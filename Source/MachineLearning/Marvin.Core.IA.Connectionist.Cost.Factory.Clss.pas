unit Marvin.Core.IA.Connectionist.Cost.Factory.Clss;

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
