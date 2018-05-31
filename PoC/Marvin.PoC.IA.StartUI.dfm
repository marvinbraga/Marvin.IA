object FormStart: TFormStart
  Left = 0
  Top = 0
  Caption = 'PoC Marvin.IA - Open Machine Learning API - Delphi'
  ClientHeight = 600
  ClientWidth = 725
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
    Width = 725
    Height = 600
    ActivePage = TabDataFile
    Align = alClient
    TabOrder = 0
    ExplicitTop = 65
    ExplicitHeight = 535
    object TabDataFile: TTabSheet
      Caption = 'Data File'
      ExplicitHeight = 507
      object PanelToolBar: TPanel
        Left = 0
        Top = 0
        Width = 717
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
          ExplicitHeight = 59
        end
      end
      object MemoData: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 36
        Width = 711
        Height = 533
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 1
        ExplicitLeft = 264
        ExplicitTop = 240
        ExplicitWidth = 185
        ExplicitHeight = 89
      end
    end
    object TabTrain: TTabSheet
      Caption = 'Train'
      ImageIndex = 1
      ExplicitHeight = 507
    end
    object TabTest: TTabSheet
      Caption = 'Test'
      ImageIndex = 2
      ExplicitHeight = 507
    end
  end
  object DlgData: TFileOpenDialog
    DefaultExtension = '*.csv'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'CSV Files'
        FileMask = '*.csv'
      end
      item
        DisplayName = 'All Files'
        FileMask = '*.*'
      end>
    OkButtonLabel = 'Load File'
    Options = []
    Title = 'Select Data File'
    Left = 680
    Top = 16
  end
end
