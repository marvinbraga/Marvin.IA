unit Marvin.Core.IA.Connectionist.Exceptions.Clss;

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
  { embarcadero }
  System.SysUtils;

type
  EConnectionistExceptionBase = class(Exception)
  public
    constructor Create; reintroduce; virtual;
  end;

  ELearnFileNotExists = class(EConnectionistExceptionBase)
  public
    constructor Create(const AFileName: string); reintroduce; virtual;
  end;

  ENeuronIsNil = class(EConnectionistExceptionBase)
  public
    constructor Create; override;
  end;

  EInvalidFactory = class(EConnectionistExceptionBase)
  public
    constructor Create; override;
  end;

  EInputDataIsEmpty = class(EConnectionistExceptionBase)
  public
    constructor Create; override;
  end;

  EOutputDataIsEmpty = class(EConnectionistExceptionBase)
  public
    constructor Create; override;
  end;

  EInconsistentData = class(EConnectionistExceptionBase)
  public
    constructor Create; override;
  end;


implementation

{ EConnectionistExceptionBase }

constructor EConnectionistExceptionBase.Create;
begin
  inherited Create('EConnectionistExceptionBase');
end;

{ ELearnFileNotExists }

constructor ELearnFileNotExists.Create(const AFileName: string);
begin
  inherited Create;
  Self.Message := Format('O arquivo IA %S não existe.', [AFileName]);
end;

{ ENeuronIsNil }

constructor ENeuronIsNil.Create;
begin
  inherited;
  Self.Message := 'Neurônio não existe.';
end;

{ EInvalidFactory }

constructor EInvalidFactory.Create;
begin
  inherited;
  Self.Message := 'Factory não implementado.';
end;

{ EInputDataIsEmpty }

constructor EInputDataIsEmpty.Create;
begin
  inherited;
  Self.Message := 'Não existem dados de entrada.';
end;

{ EOutputDataIsEmpty }

constructor EOutputDataIsEmpty.Create;
begin
  inherited;
  Self.Message := 'Não existem dados de saída.';
end;

{ EInconsistentData }

constructor EInconsistentData.Create;
begin
  inherited;
  Self.Message := 'Os dados estão incosistentes.';
end;

end.
