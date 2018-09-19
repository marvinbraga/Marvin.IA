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
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
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
        Height = 33
        Align = alTop
        BevelOuter = bvNone
        Caption = ' '
        TabOrder = 0
        object ButtonLoadFile: TButton
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 110
          Height = 27
          Align = alLeft
          Caption = 'Load File...'
          TabOrder = 0
          OnClick = ButtonLoadFileClick
        end
      end
      object MemoData: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 36
        Width = 938
        Height = 533
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Roboto Mono'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 1
      end
    end
    object TabTrain: TTabSheet
      Caption = 'Train'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object TabTest: TTabSheet
      Caption = 'Test'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
end
