object fMain: TfMain
  Left = 0
  Top = 0
  Caption = 'SWIFT MT GUI'
  ClientHeight = 717
  ClientWidth = 964
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlButtons: TPanel
    Left = 0
    Top = 0
    Width = 964
    Height = 41
    Align = alTop
    TabOrder = 0
    object btnLoad: TButton
      Left = 8
      Top = 7
      Width = 100
      Height = 25
      Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100
      TabOrder = 0
      OnClick = btnLoadClick
    end
    object btnEditor: TButton
      Left = 114
      Top = 7
      Width = 100
      Height = 25
      Caption = #1056#1077#1076#1072#1082#1090#1080#1088#1086#1074#1072#1090#1100
      TabOrder = 1
      OnClick = btnEditorClick
    end
    object btnValidate: TButton
      Left = 220
      Top = 7
      Width = 100
      Height = 25
      Caption = #1055#1088#1086#1074#1077#1088#1080#1090#1100
      TabOrder = 2
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 41
    Width = 964
    Height = 676
    Align = alClient
    TabOrder = 1
    object pgcMain: TPageControl
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 956
      Height = 668
      ActivePage = tsView
      Align = alClient
      TabOrder = 0
      object tsText: TTabSheet
        Caption = #1058#1077#1082#1089#1090
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object mmoText: TMemo
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 942
          Height = 634
          Align = alClient
          Color = clBtnFace
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          Lines.Strings = (
            'mmoText')
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
      object tsView: TTabSheet
        Caption = #1056#1077#1076#1072#1082#1090#1086#1088
        ImageIndex = 1
        object scbView: TScrollBox
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 942
          Height = 587
          Align = alClient
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          TabOrder = 0
        end
        object pnlButtonView: TPanel
          AlignWithMargins = True
          Left = 3
          Top = 596
          Width = 942
          Height = 41
          Align = alBottom
          DoubleBuffered = False
          ParentBackground = False
          ParentDoubleBuffered = False
          TabOrder = 1
          object btnApply: TButton
            Left = 8
            Top = 8
            Width = 100
            Height = 25
            Caption = #1055#1088#1080#1084#1077#1085#1080#1090#1100
            TabOrder = 0
          end
          object btnCancel: TButton
            Left = 114
            Top = 8
            Width = 100
            Height = 25
            Caption = #1054#1090#1084#1077#1085#1080#1090#1100
            TabOrder = 1
          end
        end
      end
    end
  end
end
