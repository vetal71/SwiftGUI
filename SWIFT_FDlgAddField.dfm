object FDlgAddField: TFDlgAddField
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #1055#1086#1083#1077' Swift '#1089#1086#1086#1073#1097#1077#1085#1080#1103
  ClientHeight = 214
  ClientWidth = 430
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnlButtons: TPanel
    Left = 0
    Top = 177
    Width = 430
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnOK: TButton
      Left = 5
      Top = 7
      Width = 100
      Height = 25
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 111
      Top = 7
      Width = 100
      Height = 25
      Caption = #1054#1090#1084#1077#1085#1072
      ModalResult = 2
      TabOrder = 1
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 430
    Height = 177
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object lbl3: TLabel
      Left = 5
      Top = 5
      Width = 104
      Height = 13
      Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1087#1086#1083#1103':'
    end
    object lbl4: TLabel
      Left = 121
      Top = 5
      Width = 79
      Height = 13
      Caption = #1047#1085#1072#1095#1077#1085#1080#1077' '#1087#1086#1083#1103':'
    end
    object edtTagName: TEdit
      Left = 5
      Top = 21
      Width = 104
      Height = 21
      TabOrder = 0
    end
    object edtTagValue: TMemo
      Left = 121
      Top = 21
      Width = 300
      Height = 150
      ScrollBars = ssVertical
      TabOrder = 1
    end
  end
end
