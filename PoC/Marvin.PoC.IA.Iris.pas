unit Marvin.PoC.IA.Iris;

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
