unit Marvin.Core.IA.Connectionist.Support.Clss;

interface

uses
  Marvin.Core.IA.Connectionist.Support,
  Marvin.Core.IA.Connectionist.MultiLayerPerceptron;

type
  TSupportSimpleTextFile = class(TInterfacedObject, ISupportMultiLayerPerceptron)
  private
    FMultiLayerPerceptron: IMultiLayerPerceptron;
  protected
    function Save: ISupportMultiLayerPerceptron;
    function Load: ISupportMultiLayerPerceptron;
  public
    constructor Create(const AMultiLayerPerceptron: IMultiLayerPerceptron);
    class function New(const AMultiLayerPerceptron: IMultiLayerPerceptron): ISupportMultiLayerPerceptron;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.IniFiles,
  Marvin.Core.IA.Connectionist.Layer,
  Marvin.Core.IA.Connectionist.Neuron,
  Marvin.Core.IA.Connectionist.Exceptions.Clss;

{ TSupportMultiLayerPerceptron }

class function TSupportSimpleTextFile.New(const AMultiLayerPerceptron: IMultiLayerPerceptron): ISupportMultiLayerPerceptron;
begin
  Result := TSupportSimpleTextFile.Create(AMultiLayerPerceptron);
end;

constructor TSupportSimpleTextFile.Create(const AMultiLayerPerceptron: IMultiLayerPerceptron);
begin
  inherited Create;
  FMultiLayerPerceptron := AMultiLayerPerceptron;
end;

function TSupportSimpleTextFile.Load: ISupportMultiLayerPerceptron;
var
  LFile: TMemIniFile;
  LLayersConfigCount: Integer;
  LIndexLayersConfig, LIndexLayers, LIndexNeurons: Integer;
  LLayer: ILayer;
  LNeuron: INeuron;
  LSynapseData, LSynapseSplited: TStringList;
  LIndexSynapses: Integer;
  LFileName: string;

  procedure LSplit(const ALineData: string; AStringList: TStringList);
  begin
    AStringList.Text := '';
    AStringList.StrictDelimiter := True;
    AStringList.Delimiter := ';';
    AStringList.DelimitedText := ALineData;
  end;

  procedure LGetOrder(var AIndexLayer: Integer; var AIndexNeuron: Integer; const ALineInfo: string);
  var
    LInfo: string;
  begin
    { Retorna apenas os dados }
    LSplit(Copy(ALineInfo, Pos('=', ALineInfo) + 1, Length(ALineInfo)), LSynapseSplited);
    { recupera o posicionamento }
    LInfo := Copy(ALineInfo, 1, Pos('=', ALineInfo) - 1);
    AIndexLayer := StrToInt(Copy(LInfo, 1, Pos('.', LInfo) - 1));
    AIndexNeuron := StrToInt(Copy(LInfo, Pos('.', LInfo) + 1, Length(LInfo)));
  end;

begin
  Result := Self;
  { verifica se o arquivo existe }
  LFileName := ExtractFilePath(ParamStr(0)) + FMultiLayerPerceptron.LearnFile;
  if not (FileExists(LFileName)) then
  begin
    raise ELearnFileNotExists.Create(LFileName);
  end;

  LFile := TMemIniFile.Create(LFileName);
  try
    FMultiLayerPerceptron.Clear;
    FMultiLayerPerceptron
      .SetGama(LFile.ReadFloat('Gama', 'Value', 0))
      .SetLearning(LFile.ReadFloat('Learning', 'Value', 0.9))
      .SetMomentum(LFile.ReadFloat('Momentum', 'Value', 0.5));
    { recupera quantidade de camadas }
    LLayersConfigCount := LFile.ReadInteger('LayersConfig', 'Count', 0);
    { cria a estrutura }
    for LIndexLayersConfig := 0 to LLayersConfigCount - 1 do
    begin
      FMultiLayerPerceptron.AddLayerConfig(LFile.ReadInteger('LayerConfig', LIndexLayersConfig.ToString, 0));
    end;
    { monta a rede }
    FMultiLayerPerceptron.Build;
    { percorre todas as camadas para recuperar os identificadores dos neurônios }
    for LIndexLayers := 0 to FMultiLayerPerceptron.Layers.Count - 1 do
    begin
      { recupera a camada }
      LLayer := FMultiLayerPerceptron.Layers.Get(LIndexLayers);
      for LIndexNeurons := 0 to LLayer.NeuronsCount - 1 do
      begin
        { recuperar o neurônio }
        LNeuron := LLayer.Neurons.Get(LIndexNeurons);
        { informa o id }
        LNeuron.SetId(LFile.ReadString('Neuron', Format('%d.%d', [LIndexLayers, LIndexNeurons]), LNeuron.Id));
      end;
    end;
    { inicializa as sinapses }
    LSynapseData := TStringList.Create;
    LSynapseSplited := TStringList.Create;
    try
      LFile.ReadSectionValues('Synapse', LSynapseData);
      { percorre todas as linhas recuperadas }
      for LIndexSynapses := 0 to LSynapseData.Count - 1 do
      begin
        { recupera as informações da linha }
        LGetOrder(LIndexLayers, LIndexNeurons, LSynapseData[LIndexSynapses]);
        { recupera a camada }
        LLayer := FMultiLayerPerceptron.Layers.Get(LIndexLayers);
        { recupera para a sinapse os valores importados do arquivo }
        FMultiLayerPerceptron.InitSynapse(LLayer, LSynapseSplited[0], StrToFloat(LSynapseSplited[1]), LSynapseSplited[2], LSynapseSplited[3]);
      end;
    finally
      LSynapseSplited.Free;
      LSynapseData.Free;
    end;
  finally
    LFile.Free;
  end;
end;

function TSupportSimpleTextFile.Save: ISupportMultiLayerPerceptron;
var
  LFile: TMemIniFile;
  LCount, LIndexConfig: Integer;
  LLayer: ILayer;
  LNeuron: INeuron;
  LLayerIndex, LNeuronIndex, LSynapseIndex: Integer;
  LCountNeuron: Integer;
  LFileName: string;
begin
  Result := Self;

  { verifica se o arquivo existe }
  LFileName := ExtractFilePath(ParamStr(0)) + FMultiLayerPerceptron.LearnFile;
  LFile := TMemIniFile.Create(LFileName);
  try
    { guarda as propriedades da rede }
    LFile.WriteFloat('Gama', 'Value', FMultiLayerPerceptron.Gama);
    LFile.WriteFloat('Learning', 'Value', FMultiLayerPerceptron.Learning);
    LFile.WriteFloat('Momentum', 'Value', FMultiLayerPerceptron.Momentum);
    LFile.WriteInteger('LayersConfig', 'Count', FMultiLayerPerceptron.LayersConfig.Count);
    { percorre as estruturada rede }
    for LIndexConfig := 0 to FMultiLayerPerceptron.LayersConfig.Count - 1 do
    begin
      LFile.WriteInteger('LayerConfig', LIndexConfig.ToString, FMultiLayerPerceptron.LayersConfig.Items[LIndexConfig]);
    end;
    { salva os identificadores dos neurônios }
    LLayerIndex := 0;
    for LCount := 0 to FMultiLayerPerceptron.Layers.Count - 1 do
    begin
      LLayer := FMultiLayerPerceptron.Layers.Get(LCount);
      LNeuronIndex := 0;
      for LCountNeuron := 0 to LLayer.Neurons.Count - 1 do
      begin
        LNeuron := LLayer.Neurons.Get(LCountNeuron);
        LFile.WriteString('Neuron', Format('%d.%d', [LLayerIndex, LNeuronIndex]), LNeuron.Id);
        Inc(LNeuronIndex);
      end;
      Inc(LLayerIndex);
    end;
    { salva os pesos sinápticos - exceção da camada de saída }
    for LLayerIndex := 0 to FMultiLayerPerceptron.Layers.Count - 2 do
    begin
      LLayer := FMultiLayerPerceptron.Layers.Get(LLayerIndex);
      for LSynapseIndex := 0 to LLayer.Synapses.Count - 1 do
      begin
        with LLayer.Synapses.Get(LSynapseIndex) do
        begin
          LFile.WriteString('Synapse', Format('%d.%d', [LLayerIndex, LSynapseIndex]), Format('%s; %s; %s; %s;', [Id, Value.ToString, Neuron.Id, LinkedNeuron.Id]));
        end;
      end;
    end;
    { salva os dados no arquivo }
    LFile.UpdateFile;
  finally
    LFile.Free;
  end;
end;

end.
