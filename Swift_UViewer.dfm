object fSwiftView: TfSwiftView
  Left = 0
  Top = 0
  Width = 830
  Height = 706
  TabOrder = 0
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 830
    Height = 60
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lbl1: TLabel
      Left = 15
      Top = 10
      Width = 80
      Height = 13
      Caption = #1054#1090#1087#1088#1072#1074#1080#1090#1077#1083#1100':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lbl2: TLabel
      Left = 15
      Top = 35
      Width = 74
      Height = 13
      Caption = #1055#1086#1083#1091#1095#1072#1090#1077#1083#1100':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object edtSender: TEdit
      Left = 100
      Top = 7
      Width = 200
      Height = 21
      TabOrder = 0
    end
    object edtReciever: TEdit
      Left = 100
      Top = 32
      Width = 200
      Height = 21
      TabOrder = 1
    end
  end
  object pnlView: TPanel
    Left = 0
    Top = 60
    Width = 830
    Height = 546
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 552
    ExplicitTop = 424
    ExplicitWidth = 185
    ExplicitHeight = 41
    object VST: TVirtualStringTree
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 824
      Height = 540
      Align = alClient
      Header.AutoSizeIndex = 0
      Header.DefaultHeight = 17
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
      TabOrder = 0
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
      TreeOptions.SelectionOptions = [toExtendedFocus]
      OnBeforeCellPaint = VSTBeforeCellPaint
      OnChange = VSTChange
      OnGetText = VSTGetText
      OnPaintText = VSTPaintText
      OnGetNodeDataSize = VSTGetNodeDataSize
      OnInitNode = VSTInitNode
      OnMeasureItem = VSTMeasureItem
      Columns = <
        item
          Position = 0
          Width = 150
          WideText = #1048#1084#1103' '#1087#1086#1083#1103
        end
        item
          Position = 1
          Width = 300
          WideText = #1047#1085#1072#1095#1077#1085#1080#1077
        end>
    end
  end
  object pnlEditor: TPanel
    Left = 0
    Top = 606
    Width = 830
    Height = 100
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
    ExplicitTop = 456
    DesignSize = (
      830
      100)
    object lblTagName: TLabel
      Left = 15
      Top = 10
      Width = 54
      Height = 13
      Caption = #1048#1084#1103' '#1087#1086#1083#1103
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object edtTagValue: TEdit
      Left = 100
      Top = 7
      Width = 722
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Visible = False
      OnChange = mmoTagValueChange
    end
    object mmoTagValue: TMemo
      Left = 100
      Top = 6
      Width = 722
      Height = 89
      Anchors = [akLeft, akTop, akRight]
      Lines.Strings = (
        'mmoTagValue')
      TabOrder = 1
      Visible = False
      OnChange = mmoTagValueChange
    end
  end
end
