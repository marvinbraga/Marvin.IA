unit Marvin.Core.IA.Connectionist.Layer.Clss;

{
  MIT License

  Copyright (c) 2019 Marcus Vinicius D. B. Braga

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
  System.Generics.Collections,
  { marvin }
  Marvin.Core.InterfacedList,
  Marvin.Core.IA.Connectionist.Layer,
  Marvin.Core.IA.Connectionist.Neuron,
  Marvin.Core.IA.Connectionist.Neurons,
  Marvin.Core.IA.Connectionist.Synapse,
  Marvin.Core.IA.Connectionist.Synapses,
  Marvin.Core.IA.Connectionist.LayerType.Clss,
  Marvin.Core.IA.Connectionist.Neuron.Clss,
  Marvin.Core.IA.Connectionist.Neurons.Clss,
  Marvin.Core.IA.Connectionist.Synapse.Clss,
  Marvin.Core.IA.Connectionist.Synapses.Clss;

type
  TFactoryLayer = class
  public
    class function New(const ALayerType: TLayerType; const ALayerIndex: Integer; const ANeuronsCount: Integer): ILayer;
  end;

  { Classe que implementa uma camada de rede neural artificial. }
  TLayer = class sealed(TInterfacedObject, ILayer)
  strict private
    FId: string;
    FIndex: Integer;
    FLayerType: TLayerType;
    FNeurons: IList<INeuron>;
    FSynapses: IList<ISynapse>;
  protected
    function SetNeurons(const ANeurons: IList<INeuron>): TLayer;
    function SetSynapses(const ASynapses: IList<ISynapse>): TLayer;
    constructor Create(const ALayerType: TLayerType; const ALayerIndex: Integer; const ANeuronsCount: Integer);
  public
    class function NewId: string;
    class function Compare(const Left, Right: ILayer): Integer;
    class function New(const ALayerType: TLayerType; const ALayerIndex: Integer; ANeuronsCount: Integer): ILayer;
    destructor Destroy; override;
    function Clear: ILayer;
    function Assign(const ALayer: ILayer): ILayer;
    function InitSynapses: ILayer;
    function NewNeurons(const ACount: Integer): ILayer;
    function NewSynapse(const ANeuron: INeuron; const ALinkedNeuron: INeuron; const AValue: Double = 0): ISynapse;
    function AddSynapse(const ASynapse: ISynapse): ILayer;
    { dados }
    function LayerType: TLayerType;
    function NeuronsCount: Integer;
    function Neurons: IList<INeuron>;
    function Synapses: IList<ISynapse>;
    function Index: Integer;
    function Id: string;
  end;

implementation

uses
  System.SysUtils;

{ TFactoryLayer }

class function TFactoryLayer.New(const ALayerType: TLayerType; const ALayerIndex: Integer;
  const ANeuronsCount: Integer): ILayer;
begin
  Result := TLayer.New(ALayerType, ALayerIndex, ANeuronsCount);
end;

{ TLayer }

function TLayer.AddSynapse(const ASynapse: ISynapse): ILayer;
begin
  Result := Self;
  FSynapses.Add(ASynapse);
end;

function TLayer.Assign(const ALayer: ILayer): ILayer;
begin
  Result := Self;
  FId := ALayer.Id;
  FLayerType := ALayer.LayerType;
  Self.SetNeurons(ALayer.Neurons).SetSynapses(ALayer.Synapses);
end;

function TLayer.Clear: ILayer;
begin
  Result := Self;
  FId := TLayer.NewId;
  FLayerType := TLayerType.InputLayer;
  FNeurons.Clear;
  FSynapses.Clear;
end;

class function TLayer.Compare(const Left, Right: ILayer): Integer;
begin
  Result := -1;
  if SameText(Left.Id, Right.Id) then
  begin
    Result := 0;
  end;
end;

constructor TLayer.Create(const ALayerType: TLayerType; const ALayerIndex: Integer;
  const ANeuronsCount: Integer);
begin
  inherited Create;
  FNeurons := TFactoryNeurons.New;
  FSynapses := TFactorySynapses.New;
  Self.Clear;
  FLayerType := ALayerType;
  FIndex := ALayerIndex;
  Self.NewNeurons(ANeuronsCount);
end;

destructor TLayer.Destroy;
begin
  FNeurons := nil;
  FSynapses := nil;
  inherited;
end;

function TLayer.Id: string;
begin
  Result := FId;
end;

function TLayer.Index: Integer;
begin
  Result := FIndex;
end;

function TLayer.InitSynapses: ILayer;
var
  LCount: Integer;
begin
  Assert(FSynapses <> nil, 'List não pode ser nil.');
  Result := Self;
  for LCount := 0 to Self.Synapses.Count - 1 do
  begin
    Self.Synapses.Get(LCount).Init;
  end;
end;

function TLayer.LayerType: TLayerType;
begin
  Result := FLayerType;
end;

function TLayer.Neurons: IList<INeuron>;
begin
  Result := FNeurons;
end;

function TLayer.NeuronsCount: Integer;
begin
  Result := FNeurons.Count;
end;

class function TLayer.New(const ALayerType: TLayerType;
  const ALayerIndex: Integer; ANeuronsCount: Integer): ILayer;
begin
  Result := Self.Create(ALayerType, ALayerIndex, ANeuronsCount);
end;

class function TLayer.NewId: string;
var
  LId: TGUID;
begin
  CreateGUID(LId);
  Result := StringReplace(GUIDToString(LId), '{', '', [rfReplaceAll]);
  Result := StringReplace(Result, '}', '', [rfReplaceAll]);
  Result := StringReplace(Result, '-', '', [rfReplaceAll]);
end;

function TLayer.SetNeurons(const ANeurons: IList<INeuron>): TLayer;
var
  LCount: Integer;
begin
  Result := Self;
  FNeurons.Clear;
  for LCount := 0 to ANeurons.Count - 1 do
  begin
    FNeurons.Add(FLayerType.New.Assign(ANeurons.Get(LCount)));
  end;
end;

function TLayer.NewNeurons(const ACount: Integer): ILayer;
var
  LCount: Integer;
begin
  Assert(ACount <> 0, 'ACount não pode ser zero.');
  Result := Self;
  FNeurons.Clear;
  FSynapses.Clear;
  for LCount := 0 to ACount - 1 do
  begin
    FNeurons.Add(FLayerType.New);
  end;
end;

function TLayer.NewSynapse(const ANeuron: INeuron;
  const ALinkedNeuron: INeuron; const AValue: Double): ISynapse;
begin
  Result := TFactorySynapse.New(ANeuron, ALinkedNeuron);
  Result.SetValue(AValue);
  Self.AddSynapse(Result);
end;

function TLayer.SetSynapses(const ASynapses: IList<ISynapse>): TLayer;
var
  LSynapse: ISynapse;
  LCount: Integer;
begin
  Result := Self;
  FSynapses.Clear;
  for LCount := 0 to ASynapses.Count - 1 do
  begin
    LSynapse := ASynapses.Get(LCount);
    FSynapses.Add(
      TFactorySynapse.New(
        TFactoryNeuron.New.Assign(LSynapse.Neuron),
        TFactoryNeuron.New.Assign(LSynapse.LinkedNeuron)
      )
    );
  end;
end;

function TLayer.Synapses: IList<ISynapse>;
begin
  Result := FSynapses;
end;

end.
