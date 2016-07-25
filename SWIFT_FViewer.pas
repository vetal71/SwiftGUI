unit SWIFT_FViewer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, VirtualTrees, SWIFT_UMsgFormat, SWIFT_UUtils,
  Menus;

const
  cSequenceTags: array [0..10] of string = (
    '15A', '15B', '15C', '15D', '15E', '15F', '15G', '15H', '15I', '16R', '16S');

type

  TSwiftData = packed record
    TagName,
    TagValue,
    TagFullName,
    Content: String;
    Editable,
    MultiLine,
    HasDelete: Boolean;
//    LineCount: Integer;
  end;
  PSwiftData = ^TSwiftData;

  TSWiftFieldTYpe = set of (sftSequence, sftSubSequence, sftField);

  TfSwiftView = class(TFrame)
    pnlTop: TPanel;
    pnlView: TPanel;
    pnlEditor: TPanel;
    lblSender: TLabel;
    edtSender: TEdit;
    lblReciewer: TLabel;
    edtReciever: TEdit;
    VST: TVirtualStringTree;
    lblTagName: TLabel;
    edtTagValue: TEdit;
    mmoTagValue: TMemo;
    pmView: TPopupMenu;
    miAddField: TMenuItem;
    miDelField: TMenuItem;
    miAddSeq: TMenuItem;
    miN2: TMenuItem;
    miAddSubSeq: TMenuItem;
    procedure miAddFieldClick(Sender: TObject);
    procedure miAddSeqClick(Sender: TObject);
    procedure miAddSubSeqClick(Sender: TObject);
    procedure miDelFieldClick(Sender: TObject);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VSTMeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; var NodeHeight: Integer);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure mmoTagValueChange(Sender: TObject);
  private
    { Swift сообщение }
    FSwiftMessage: TSwiftMessage;
    FMsgText: string;
    FMsgType: Integer;
    FReadOnlyMsg: Boolean;

    { Getters and Setters }
    procedure SetMsgText(aValue: string);
    function GetMsgText: string;

    procedure UpdateFromNode;
    procedure FillData;
    procedure BuildTree(aBlock: TSwiftBlock4);
    function GetNodeData(const ANode: PVirtualNode): PSwiftData;
    function BuildMessageText: string;
    function AddField(aType: TSWiftFieldTYpe): Boolean;

  public
    property MsgType: Integer read FMsgType write FMsgType;
    property MessageText: string read GetMsgText write SetMsgText;
    property ReadOnlyMsg: Boolean write FReadOnlyMsg default True;
  end;

implementation

uses
  Math, RegularExpressions, StrUtils, SWIFT_FDlgAddField;

{$R *.dfm}

function GetLineCount(aText: string): Integer;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := aText;
    Result := SL.Count;
  finally
    SL.Free;
  end;
end;

{ TODO: Переделать после перехода на настроку полей в базе данных }
function GetContentFields(aFieldsArray: array of TSwiftFieldsRec;
  const aFieldName: string): string;
var
  I: Integer;
begin
  for I := 0 to High(aFieldsArray) do begin
    if (SameText(aFieldsArray[ I ].FullName, aFieldName)) or
       (Pos(aFieldsArray[ I ].FullName, aFieldName) > 0) then begin
      Result := aFieldsArray[ I ].Content;
      Break;
    end else Result := '';
  end;
end;

function TfSwiftView.GetNodeData(const ANode: PVirtualNode): PSwiftData;
begin
  // Для более короткого доступа к данным узла дерева
  Result := VST.GetNodeData(Anode);
end;

procedure TfSwiftView.mmoTagValueChange(Sender: TObject);
var
  Data: PSwiftData;
  SwiftField: TSwiftField;
  IsValid: Boolean;
  OldValue: string;
begin
  Data := GetNodeData(VST.FocusedNode);
//  if SameText(TCustomEdit(Sender).Text, Data.TagValue) then Exit;

// Валидация
  IsValid := False;
  if (FMsgType div 100 = 5) then
    SwiftField := FSwiftMessage.Block4.GetFieldByEx(Data.TagFullName)
  else
    SwiftField := FSwiftMessage.Block4.GetFieldBy(Data.TagName);

  if Assigned(SwiftField) then begin
    OldValue := SwiftField.Tag.Value;
    SwiftField.Tag.Value := TCustomEdit(Sender).Text;
    IsValid := SwiftField.Valid;
  end;

  if IsValid then begin
    TEdit(Sender).Color := clWhite;
    Data^.TagValue := TCustomEdit(Sender).Text;
    VST.Refresh;
  end else begin
    SwiftField.Tag.Value := OldValue;
    TEdit(Sender).Color := clYellow;
  end;
end;

procedure TfSwiftView.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  BgColor: TColor;
begin
  if GetNodeData(Node).Editable then
    BgColor := clWhite
  else
    BgColor := clBtnFace;
  with TargetCanvas do begin
    Brush.Color := BgColor;
    FillRect(CellRect);
  end;
end;

procedure TfSwiftView.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  // при изменении узла дерева
  UpdateFromNode;
end;

procedure TfSwiftView.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TSwiftData);
end;

procedure TfSwiftView.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
begin
  if Column = 0 then
    CellText := GetNodeData(Node).TagName
  else if Column = 1 then
    CellText := GetNodeData(Node).TagValue;
  Sender.Hint := GetNodeData(Node).Content;
end;

procedure TfSwiftView.VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if GetNodeData(Node).MultiLine then
    Include(InitialStates, ivsMultiline)
  else begin
    Exclude(InitialStates, ivsMultiline);
//     НЕ ЗАБЫВАЙТЕ ВЫПОЛНЯТЬ ЭТО:
    Node.States := Node.States - [vsMultiline];
//     Это выключит многострочность для узлов, где она раньше была.
  end;
end;

procedure TfSwiftView.VSTMeasureItem(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
var
  LineCount: Integer;
begin
  if GetNodeData(Node).MultiLine then begin
    LineCount := GetLineCount(GetNodeData(Node).TagValue);
    NodeHeight := VST.ComputeNodeHeight(TargetCanvas, Node, 0) + 4;
    NodeHeight := LineCount * 18;
    NodeHeight := Max(18, NodeHeight);
  end else NodeHeight := 18;
end;

procedure TfSwiftView.VSTPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
//var
//  FontColor: TColor;
begin
  { Переопределение цвета текста при нестандартной заливке }
//  if not GetNodeData(Node).Editable then
//    FontColor := clBlack
//  else
//  FontColor := clBlack;
//  if (vsSelected in Node.States) and (Sender.Focused) then
//    FontColor := clHighlightText;
//  TargetCanvas.Font.Color := FontColor;
end;

function TfSwiftView.GetMsgText: string;
begin
  // TODO Строим текст по дереву
  FMsgText := BuildMessageText;
  Result := FMsgText;
end;

procedure TfSwiftView.FillData;
begin
  // Отправитель и Получатель
  edtSender.Text   := FSwiftMessage.Block1.Sender;
  edtReciever.Text := FSwiftMessage.Block2.Reciever;

  // строим дерево полей
  BuildTree(FSwiftMessage.Block4);
  VST.FullExpand(nil);
end;

function TfSwiftView.AddField(aType: TSWiftFieldTYpe): Boolean;
var
  FDlg: TFDlgAddField;
  Data: PSwiftData;
begin
  case aType of
  sftSequence: ;
  sftSubSequence: ;
  sftField: ;
  end;
  // Добавление поля
  FDlg := TFDlgAddField.Create(Owner);
  try
    with FDlg do begin
      if ShowModal = mrOk then begin
        if edtTagName.Text = '' then
          raise Exception.Create('Наименование поля не должно быть пустым!');

        Data^.TagName     := edtTagName.Text;
        Data^.Editable    := True;
        VST.AddChild(VST.FocusedNode, Data);
      end;
    end;
  finally
    FDlg.Free;
  end;
end;

function TfSwiftView.BuildMessageText: string;
var
  CurNode: PVirtualNode;
  eBuffer: TSWIFTBuilder;
  sHeader: string;
  Data: PSwiftData;
begin
  Result := '';
  eBuffer := TSWIFTBuilder.Create();
  try
    sHeader := Format('{1:%s}{2:%s}', [FSwiftMessage.Block1.Text,
                                       FSwiftMessage.Block2.Text]);
    sHeader := sHeader + Format('{3:%s}', [FSwiftMessage.Blocks[2].Text]);
    // строим текст блока 4
    eBuffer.AppendLine(sHeader + '{4:');
    CurNode := VST.GetFirst;
    while Assigned(CurNode) do begin
      // обработка узлов дерева
      Data := GetNodeData(CurNode);
      eBuffer.AppendLineFmt(':%s:%s', [Data.TagName, Data.TagValue]);
      CurNode := VST.GetNext(CurNode);
    end;
    eBuffer.Append('-}'#03);
    Result := eBuffer.ToString();
  finally
    eBuffer.Free();
  end;
end;

procedure TfSwiftView.BuildTree(aBlock: TSwiftBlock4);
var
  eTmpTag: TSwiftTag;
  I: Integer;
  eNode, eSeqNode, eSubNode, eRoot: PVirtualNode;
  eIsEditable: Boolean;
  eData: PSwiftData;

  function IsEditable(aName: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    if Length(cEditableFields) = 0 then Exit;

    for I := 0 to High(cEditableFields) do
      if TRegEx.IsMatch(aName, cEditableFields[ I ]) then begin
        Result := True;
        Break;
      end;
  end;

  function GetLineCountFromContent(aContent: string): Integer;
  var
    PosAsterisc: Integer;
    s: string;
  begin
    Result := 1;
    if aContent = '' then Exit;
    PosAsterisc := Pos('*', aContent);
    if PosAsterisc = 0 then Exit;
    if PosAsterisc > 2 then begin
      s := Copy(aContent, PosAsterisc - 2, 2);
      if CharInSet(s[1], ['0'..'9']) then
        Result := StrToIntDef(s, 1)
      else
        Result := StrToIntDef(s[2], 1);
    end else begin
      Result := StrToIntDef(aContent[1], 1);
    end;
  end;

begin
  with VST do begin
    NodeDataSize := SizeOf(TSwiftData);
    aBlock.RecreateTags;
    eRoot := RootNode;
    eNode    := nil;
    eSeqNode := nil;
    eSubNode := nil;
    for I := 0 to aBlock.Count - 1 do begin
      eTmpTag := aBlock.Tags[ I ];

      // последовательность в корень
      if MatchText(eTmpTag.Name, cSequenceTags) then begin
        // начало последовательности
        if (MatchText(eTmpTag.Name, ['15A', '15B', '16R'])) then begin
          if (eSeqNode <> nil) then begin
            eSubNode := AddChild(eSeqNode);
            eNode    := eSubNode;
          end else begin
            eSeqNode := AddChild(eRoot);
            eNode    := eSeqNode;
          end;
        end;
        // конец последовательности
        if (SameText(eTmpTag.Name, '16S')) then begin
          if (eSubNode <> nil) then begin
            // добавим
            eNode    := AddChild(eSubNode);
            eSubNode := nil;
          end else if (eSeqNode <> nil) then begin
            eNode    := AddChild(eSeqNode);
            eSeqNode := nil;
          end;
        end;
      end else begin

        if (eSubNode <> nil) then begin
          // добавим
          eNode    := AddChild(eSubNode);
        end else if (eSeqNode <> nil) then begin
          eNode    := AddChild(eSeqNode);
        end;
      end;
      eData := GetNodeData(eNode);
      eData^.TagName     := eTmpTag.Name;
      eData^.TagValue    := eTmpTag.Value;
      eData^.TagFullName := eTmpTag.FullName;
      eData^.Editable    := (not FReadOnlyMsg) and (IsEditable(eTmpTag.FullName));
      { TODO: Замена после перехода на БД }
      case FMsgType of
      518:     eData^.Content := GetContentFields(cSwiftFieldsFour518, eTmpTag.FullName);
      541,543: eData^.Content := GetContentFields(cSwiftFieldsFour541, eTmpTag.FullName);
      end;
      eData^.MultiLine   := (Pos(#$D#$A, eTmpTag.Value) > 0) or
                            (GetLineCountFromContent(eData.Content) > 1);

      eData^.HasDelete   := False;
    end;
  end;
end;

procedure TfSwiftView.miAddFieldClick(Sender: TObject);
begin
  //
end;

procedure TfSwiftView.miAddSeqClick(Sender: TObject);
begin
  //
end;

procedure TfSwiftView.miAddSubSeqClick(Sender: TObject);
begin
  //
end;

procedure TfSwiftView.miDelFieldClick(Sender: TObject);
begin
  // Удаление поля
end;

procedure TfSwiftView.SetMsgText(aValue: string);
begin
  FMsgText := aValue;
  if FMsgText > '' then begin
    FSwiftMessage := TSwift.Load(FMsgType, FMsgText);
    if Assigned(FSwiftMessage) then FillData;
  end;
end;

procedure TfSwiftView.UpdateFromNode;
var
  Data: PSwiftData;
begin
  with VST do
  if FocusedNode = nil then begin
    PnlEditor.Visible  := False;
  end else begin
    Data := GetNodeData(FocusedNode);
    PnlEditor.Visible  := True;
    lblTagName.Caption := Data.TagName;
    lblTagName.Enabled := True;
    miDelField.Enabled := Data.HasDelete;
    if Data.MultiLine then begin
      mmoTagValue.Text     := Data.TagValue;
      mmoTagValue.Visible  := True;
      edtTagValue.Visible  := False;
      mmoTagValue.Enabled  := Data.Editable;
    end else begin
      edtTagValue.Text     := Data.TagValue;
      mmoTagValue.Visible  := False;
      edtTagValue.Visible  := True;
      edtTagValue.Enabled  := Data.Editable;
    end;
  end;
end;

end.
