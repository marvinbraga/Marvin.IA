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
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
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
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
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
          end
        end
      end
    end
    object TabTrain: TTabSheet
      Caption = 'Train'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LabelCabecalhoTreino: TLabel
        AlignWithMargins = True
        Left = 8
        Top = 8
        Width = 136
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
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LabelCabecalhoTeste: TLabel
        AlignWithMargins = True
        Left = 8
        Top = 8
        Width = 130
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
