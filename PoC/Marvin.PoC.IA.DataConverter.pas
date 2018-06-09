unit Marvin.PoC.IA.DataConverter;

interface

uses
  System.Generics.Collections,
  Marvin.Core.IA.Connectionist.Classifier,
  Marvin.Core.InterfacedList;

type
  IDataConverter = interface
    ['{3E020966-878E-4EE7-9495-508B37FCE12C}']
    function Execute(out AInputValues: IList<TDoubleArray>; out AOutputValues: IList<TDoubleArray>): IDataConverter;
  end;

implementation

end.
