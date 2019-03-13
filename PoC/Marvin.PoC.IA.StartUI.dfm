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
    Top = 3
    Width = 952
    Height = 597
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
        Height = 522
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
            Height = 488
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
            Height = 488
            Align = alClient
            BevelOuter = bvNone
            Caption = ' '
            ShowCaption = False
            TabOrder = 0
            object Chart1: TChart
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 918
              Height = 482
              Border.Color = clGray
              Border.Fill.Color = clGray
              Legend.Visible = False
              Title.Text.Strings = (
                'Train Cost x Epochs Covered')
              BottomAxis.Axis.Color = clGray
              BottomAxis.AxisValuesFormat = '#,##0.##'
              DepthAxis.Axis.Color = 8421440
              DepthTopAxis.Axis.Color = 8421440
              DepthTopAxis.Axis.Width = 0
              LeftAxis.Axis.Color = 8421440
              LeftAxis.Axis.Width = 0
              LeftAxis.AxisValuesFormat = '#,##0.0000'
              RightAxis.Axis.Color = 8421440
              TopAxis.Axis.Color = 8421440
              View3D = False
              Align = alClient
              BevelOuter = bvNone
              Color = 14671839
              TabOrder = 0
              DefaultCanvas = 'TGDIPlusCanvas'
              PrintMargins = (
                15
                23
                15
                23)
              ColorPaletteIndex = 13
              object Series1: TPointSeries
                Title = 'Iris Dataset'
                ClickableLine = False
                Pointer.InflateMargins = True
                Pointer.Pen.Visible = False
                Pointer.Style = psCircle
                XValues.Name = 'X'
                XValues.Order = loNone
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
      object PageControlTrain: TPageControl
        AlignWithMargins = True
        Left = 3
        Top = 42
        Width = 938
        Height = 524
        ActivePage = TabSheetTrainGraphic
        Align = alClient
        HotTrack = True
        MultiLine = True
        TabOrder = 0
        object TabSheetTrainData: TTabSheet
          Caption = 'Train Data'
          object MemoTrain: TMemo
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 924
            Height = 490
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
        object TabSheetTrainGraphic: TTabSheet
          Caption = 'Train Graphics'
          ImageIndex = 1
          object pnl1: TPanel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 924
            Height = 490
            Align = alClient
            BevelOuter = bvNone
            Caption = ' '
            ShowCaption = False
            TabOrder = 0
            object ChartTrain: TChart
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 918
              Height = 484
              Border.Color = clGray
              Border.Fill.Color = clGray
              Legend.Visible = False
              Title.Text.Strings = (
                'Train Cost x Epochs Covered')
              BottomAxis.Axis.Color = clGray
              BottomAxis.AxisValuesFormat = '#,##0.##'
              DepthAxis.Axis.Color = 8421440
              DepthTopAxis.Axis.Color = 8421440
              DepthTopAxis.Axis.Width = 0
              LeftAxis.Axis.Color = 8421440
              LeftAxis.Axis.Width = 0
              LeftAxis.AxisValuesFormat = '#,##0.0000'
              RightAxis.Axis.Color = 8421440
              TopAxis.Axis.Color = 8421440
              View3D = False
              Align = alClient
              BevelOuter = bvNone
              Color = 14671839
              TabOrder = 0
              DefaultCanvas = 'TGDIPlusCanvas'
              PrintMargins = (
                15
                23
                15
                23)
              ColorPaletteIndex = 13
              object LineCost: TLineSeries
                Marks.Emboss.Color = 8487297
                Marks.Font.Color = clSilver
                Marks.Shadow.Color = 8487297
                Marks.BackColor = clBlack
                Marks.Callout.Length = 20
                Marks.Symbol.Visible = True
                Marks.Color = clBlack
                SeriesColor = 16749459
                Shadow.Visible = False
                Title = 'Train Cost'
                ValueFormat = '#,##0.000'
                Brush.BackColor = clDefault
                Dark3D = False
                DrawStyle = dsCurve
                LinePen.Width = 3
                Pointer.InflateMargins = True
                Pointer.Style = psRectangle
                XValues.Name = 'X'
                XValues.Order = loNone
                YValues.Name = 'Y'
                YValues.Order = loNone
                object TSmoothingFunction
                  CalcByValue = False
                  Period = 1.000000000000000000
                  Factor = 8
                end
              end
            end
          end
        end
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
        Height = 524
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
  object ProgressBar: TProgressBar
    Left = 0
    Top = 0
    Width = 952
    Height = 3
    Align = alTop
    Max = 5000
    Position = 5000
    BarColor = 13264128
    TabOrder = 2
  end
  object PopupMenu: TPopupMenu
    Left = 880
    Top = 112
    object ItemStyles: TMenuItem
      Caption = 'Styles'
    end
  end
  object TimerCost: TTimer
    Enabled = False
    Interval = 100
    OnTimer = TimerCostTimer
    Left = 880
    Top = 160
  end
end
