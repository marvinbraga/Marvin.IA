object FormStart: TFormStart
  Left = 0
  Top = 0
  Caption = 'PoC Marvin.IA - Open Machine Learning API - Delphi'
  ClientHeight = 600
  ClientWidth = 952
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Roboto'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = PopupMenu
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pgcInfo: TPageControl
    Left = 0
    Top = 0
    Width = 952
    Height = 600
    ActivePage = TabDataFile
    Align = alClient
    TabOrder = 0
    object TabDataFile: TTabSheet
      Caption = 'Data File'
      object PanelToolBar: TPanel
        Left = 0
        Top = 0
        Width = 944
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        Caption = ' '
        TabOrder = 0
        object Label1: TLabel
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 165
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alLeft
          AutoSize = False
          Caption = '1. Load Data'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 13264128
          Font.Height = -19
          Font.Name = 'Roboto'
          Font.Style = [fsBold]
          ParentFont = False
          Layout = tlCenter
          ExplicitHeight = 25
        end
        object ButtonLoadFile: TButton
          AlignWithMargins = True
          Left = 830
          Top = 4
          Width = 110
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alRight
          Caption = 'Load File...'
          TabOrder = 0
          OnClick = ButtonLoadFileClick
        end
      end
      object PageControlData: TPageControl
        AlignWithMargins = True
        Left = 3
        Top = 44
        Width = 938
        Height = 525
        ActivePage = TabSheetCollectionData
        Align = alClient
        HotTrack = True
        MultiLine = True
        TabOrder = 1
        object TabSheetCollectionData: TTabSheet
          Caption = 'Pure Data'
          object MemoData: TMemo
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 924
            Height = 491
            Align = alClient
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Roboto Mono'
            Font.Style = []
            ParentFont = False
            ScrollBars = ssBoth
            TabOrder = 0
          end
        end
        object TabSheetGraphicsData: TTabSheet
          Caption = 'Graphics Data'
          ImageIndex = 1
          TabVisible = False
          object PanelGraphicsData: TPanel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 924
            Height = 491
            Align = alClient
            BevelOuter = bvNone
            Caption = ' '
            ShowCaption = False
            TabOrder = 0
            object ChartPureData: TChart
              Left = 0
              Top = 16
              Width = 441
              Height = 313
              Title.Text.Strings = (
                'Iris Data')
              Chart3DPercent = 20
              TabOrder = 0
              DefaultCanvas = 'TGDIPlusCanvas'
              PrintMargins = (
                15
                10
                15
                10)
              ColorPaletteIndex = 13
              object Series1: TPointSeries
                Title = 'Iris Setosa'
                ClickableLine = False
                Pointer.InflateMargins = True
                Pointer.Style = psRectangle
                XValues.Name = 'X'
                XValues.Order = loAscending
                YValues.Name = 'Y'
                YValues.Order = loNone
                Data = {
                  00190000000000000000B0834000000000007483400000000000308140000000
                  00009C83400000000000BC81400000000000507E400000000000507E40000000
                  00005480400000000000C4834000000000007C85400000000000748840000000
                  0000E48B400000000000BC8B400000000000208C400000000000CC8F40000000
                  000076914000000000002C90400000000000F48F400000000000DA9140000000
                  0000489240000000000016924000000000004090400000000000D09140000000
                  00008492400000000000DA9140}
                Detail = {0000000000}
              end
              object Series2: TPointSeries
                Title = 'Iris Versicolor'
                ClickableLine = False
                Pointer.InflateMargins = True
                Pointer.Style = psRectangle
                XValues.Name = 'X'
                XValues.Order = loAscending
                YValues.Name = 'Y'
                YValues.Order = loNone
              end
              object Series3: TPointSeries
                Title = 'Iris Virginica'
                ClickableLine = False
                Pointer.InflateMargins = True
                Pointer.Style = psRectangle
                XValues.Name = 'X'
                XValues.Order = loAscending
                YValues.Name = 'Y'
                YValues.Order = loNone
              end
            end
          end
        end
      end
    end
    object TabTrain: TTabSheet
      Caption = 'Train'
      ImageIndex = 1
      object LabelCabecalhoTreino: TLabel
        AlignWithMargins = True
        Left = 8
        Top = 8
        Width = 928
        Height = 23
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alTop
        Caption = '2. Train Results'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 13264128
        Font.Height = -19
        Font.Name = 'Roboto'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitWidth = 136
      end
      object MemoTrain: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 42
        Width = 938
        Height = 527
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Roboto Mono'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabTest: TTabSheet
      Caption = 'Test'
      ImageIndex = 2
      object LabelCabecalhoTeste: TLabel
        AlignWithMargins = True
        Left = 8
        Top = 8
        Width = 928
        Height = 23
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alTop
        Caption = '3. Test Results'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 13264128
        Font.Height = -19
        Font.Name = 'Roboto'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitWidth = 130
      end
      object MemoPredict: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 42
        Width = 938
        Height = 527
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Roboto Mono'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object ActivityIndicator: TActivityIndicator
    Left = 464
    Top = 304
    IndicatorSize = aisLarge
    IndicatorType = aitSectorRing
  end
  object PopupMenu: TPopupMenu
    Left = 880
    Top = 112
    object ItemStyles: TMenuItem
      Caption = 'Styles'
    end
  end
end
