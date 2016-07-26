object fSwiftView: TfSwiftView
  Left = 0
  Top = 0
  Width = 830
  Height = 706
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 830
    Height = 60
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblSender: TLabel
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
    object lblReciewer: TLabel
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
      Enabled = False
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
    object VST: TVirtualStringTree
      AlignWithMargins = True
      Left = 30
      Top = 3
      Width = 797
      Height = 540
      Align = alClient
      Header.AutoSizeIndex = 1
      Header.DefaultHeight = 20
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.Height = 20
      Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
      HintAnimation = hatSlide
      HintMode = hmTooltip
      ParentShowHint = False
      PopupMenu = pmView
      ShowHint = True
      TabOrder = 0
      TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
      TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
      OnBeforeCellPaint = VSTBeforeCellPaint
      OnChange = VSTChange
      OnExit = VSTExit
      OnGetText = VSTGetText
      OnPaintText = VSTPaintText
      OnGetNodeDataSize = VSTGetNodeDataSize
      OnInitNode = VSTInitNode
      OnMeasureItem = VSTMeasureItem
      Columns = <
        item
          Position = 0
          Width = 200
          WideText = #1048#1084#1103' '#1087#1086#1083#1103
        end
        item
          Position = 1
          Width = 593
          WideText = #1047#1085#1072#1095#1077#1085#1080#1077
        end>
      WideDefaultText = #1053#1077#1090' '#1076#1072#1085#1085#1099#1093
    end
    object dck97LeftDock: TDock97
      Left = 0
      Top = 0
      Width = 27
      Height = 546
      HelpContext = 18
      Position = dpLeft
      object tlbr: TToolbar97
        Left = 0
        Top = 0
        HelpContext = 19
        Caption = #1044#1077#1081#1089#1090#1074#1080#1103
        CloseButton = False
        DockPos = 0
        TabOrder = 0
        object tbtnAddSeq: TToolbarButton97
          Left = 0
          Top = 0
          Width = 23
          Height = 22
          Hint = #1044#1086#1073#1072#1074#1080#1090#1100' '#1087#1086#1089#1083#1077#1076#1086#1074#1072#1090#1077#1083#1100#1085#1086#1089#1090#1100
          Enabled = False
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
            8888888888888800000888888888887F8808888888888877080888888888887F
            7F0899988888880777087F308880088888889797000880888888888888088000
            097988888888888889F788888888888889998888888888888888888888888888
            8888888888888888888888888888888888888888888888888888}
          GlyphMask.Data = {00000000}
          ParentShowHint = False
          ShowHint = True
          OnClick = miAddSeqClick
        end
        object tbtnDelField: TToolbarButton97
          Left = 0
          Top = 66
          Width = 23
          Height = 22
          Hint = #1059#1076#1072#1083#1080#1090#1100' '#1087#1086#1083#1077
          Enabled = False
          Glyph.Data = {
            EE000000424DEE000000000000007600000028000000100000000F0000000100
            0400000000007800000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
            888888888888888881F88881F8888888888888991F8888889F8888191F888881
            F888888199F88891F8888888191F819F88888888819199F88888888888919F88
            88888888891911F888888888191F89F88888889191F88819F88881919F888881
            9F88891F8888888819F88888888888888888}
          GlyphMask.Data = {00000000}
          ParentShowHint = False
          ShowHint = True
          OnClick = miDelFieldClick
        end
        object tbtnAddField: TToolbarButton97
          Left = 0
          Top = 44
          Width = 23
          Height = 22
          Hint = #1044#1086#1073#1072#1074#1080#1090#1100' '#1087#1086#1083#1077' '#1074' '#1090#1077#1082#1091#1097#1091#1102' ('#1087#1086#1076')'#1087#1086#1089#1083#1077#1076#1086#1074#1072#1090#1077#1083#1100#1085#1086#1089#1090#1100
          Enabled = False
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888877
            7777888888888777F777888888888777F87787778777878FFFF7877788888777
            F877877788888777887788888888887777788777877788888888879788888888
            88888777888888888888888888888888888887B78777888888888BB788888888
            8888877788888888888888888888888888888888888888888888}
          GlyphMask.Data = {00000000}
          OnClick = miAddFieldClick
        end
        object tbtnAddSubSeq: TToolbarButton97
          Left = 0
          Top = 22
          Width = 23
          Height = 22
          Hint = #1044#1086#1073#1072#1074#1080#1090#1100' '#1087#1086#1076#1087#1086#1089#1083#1077#1076#1086#1074#1072#1090#1077#1083#1100#1085#1086#1089#1090#1100' '#1074' '#1090#1077#1082#1091#1097#1091#1102' '#1087#1086#1089#1083#1077#1076#1086#1074#1072#1090#1077#1083#1100#1085#1086#1089#1090#1100
          Enabled = False
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888877
            7777777788888777F777778888888777F877778FFFFFF78FFFF7778FFFFFF778
            F877778FFFFFF777F877778FFFFFF8777278778FFFFFFFF88888778FFFFFFFF8
            8888878FFFFFFFF88888878FFFFFFFF888888788888888888888877777777777
            8888888888888888888888888888888888888888888888888888}
          GlyphMask.Data = {00000000}
          OnClick = miAddSubSeqClick
        end
      end
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
    DesignSize = (
      830
      100)
    object lblTagName: TLabel
      Left = 15
      Top = 7
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
      Top = 1
      Width = 722
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = mmoTagValueChange
    end
    object mmoTagValue: TMemo
      Left = 100
      Top = 1
      Width = 722
      Height = 95
      Anchors = [akLeft, akTop, akRight, akBottom]
      ScrollBars = ssVertical
      TabOrder = 1
      OnChange = mmoTagValueChange
    end
  end
  object pmView: TPopupMenu
    Left = 293
    Top = 111
    object miAddSeq: TMenuItem
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1087#1086#1089#1083#1077#1076#1086#1074#1072#1090#1077#1083#1100#1085#1086#1089#1090#1100
      Enabled = False
      OnClick = miAddSeqClick
    end
    object miAddSubSeq: TMenuItem
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1087#1086#1076#1087#1086#1089#1083#1077#1076#1086#1074#1072#1090#1077#1083#1100#1085#1086#1089#1090#1100
      Enabled = False
      OnClick = miAddSubSeqClick
    end
    object miAddField: TMenuItem
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1087#1086#1083#1077' '#1074' ('#1087#1086#1076')'#1087#1086#1089#1083#1077#1076#1086#1074#1072#1090#1077#1083#1100#1085#1086#1089#1090#1100
      Enabled = False
      OnClick = miAddFieldClick
    end
    object miN2: TMenuItem
      Caption = '-'
    end
    object miDelField: TMenuItem
      Caption = #1059#1076#1072#1083#1080#1090#1100' '#1087#1086#1083#1077
      Enabled = False
      OnClick = miDelFieldClick
    end
  end
end
