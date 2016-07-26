unit SWIFT_FViewer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, VirtualTrees, SWIFT_UMsgFormat, SWIFT_UUtils,
  Menus, TB97Ctls, TB97, TB97Tlbr;

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
    dck97LeftDock: TDock97;
    tlbr: TToolbar97;
    tbtnAddSeq: TToolbarButton97;
    tbtnDelField: TToolbarButton97;
    tbtnAddField: TToolbarButton97;
    tbtnAddSubSeq: TToolbarButton97;
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
    procedure VSTExit(Sender: TObject);
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
    function AddField(aNode: PVirtualNode; aFieldType: Integer): PVirtualNode;

    function Allowed: Boolean;

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

// Валидация
  IsValid := True;
  if (FMsgType div 100 = 5) then
    SwiftField := FSwiftMessage.Block4.GetFieldByEx(Data.TagFullName)
  else
    SwiftField := FSwiftMessage.Block4.GetFieldBy(Data.TagName);

  if not Assigned(SwiftField) then Exit;

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
  edtReciever.Enabled := not FReadOnlyMsg;

  // строим дерево полей
  BuildTree(FSwiftMessage.Block4);
  VST.FullExpand(nil);
end;

function TfSwiftView.AddField(aNode: PVirtualNode; aFieldType: Integer): PVirtualNode;
var
  FDlg: TFDlgAddField;
  Data: PSwiftData;
  Node, SeqNode, SubNode: PVirtualNode;
  FullName, Qualifier: string;
begin
  // Добавление поля
  Result := nil;
  FDlg := TFDlgAddField.Create(Owner);
  try
    with FDlg do begin
      ActiveControl := edtTagName;
      if (aFieldType in [0, 1]) and (FMsgType div 100 = 5) then begin
        edtTagName.Text    := '16R';
        edtTagName.Enabled := False;
        ActiveControl := edtTagValue;
      end;

      if ShowModal = mrOk then begin
        if edtTagName.Text = '' then
          raise Exception.Create('Наименование поля не должно быть пустым!');
        if (aFieldType in [0, 1]) and (edtTagValue.Text = '') then
          raise Exception.Create('Значение поля для (под)последовательности не должно быть пустым!');

        // последовательность
        if aFieldType in [0, 1] then begin
          // проверка
          if not MatchText(edtTagValue.Text,
            ['GENL','LINK','CONFDET','CONFPRTY','FIA','SETDET','SETPRTY',
             'CSHPRTY','AMT','OTHRPRTY','REPO','TRADDET','FIAC','BREAK']) then
            raise Exception.CreateFmt('%s - Неверное наименование (под)последовательности!!', [ edtTagValue.Text ]);
          // последовательность добавляем последней в корень
          if aFieldType = 0 then
            Result := VST.InsertNode(aNode, amAddChildLast);
          // подпоследовательность добавляем перед последним полем
          if aFieldType = 1 then
            Result := VST.InsertNode(aNode.LastChild, amInsertBefore);
          Data := GetNodeData(Result);
          if Assigned(Data) then begin
            Data^.TagName     := edtTagName.Text;
            Data^.TagValue    := UpperCase(edtTagValue.Text);
            Data^.Editable    := False;
            if aFieldType = 0 then
              Data^.TagFullName := Format('%s.%s', [Data^.TagValue, Data^.TagName]);
            if aFieldType = 1 then
              Data^.TagFullName := Format('%s.%s.%s', [
                GetNodeData(aNode).TagValue, Data^.TagValue, Data^.TagName]);
          end;
          // Для последовательности необходимо закрытие
          Node :=  VST.InsertNode(Result, amAddChildLast);
          Data := GetNodeData(Node);
          if Assigned(Data) then begin
            Data^.TagName     := '16S';
            Data^.TagValue    := UpperCase(edtTagValue.Text);
            Data^.Editable    := False;
            if aFieldType = 0 then
              Data^.TagFullName := Format('%s.%s', [Data^.TagValue, Data^.TagName]);
            if aFieldType = 1 then
              Data^.TagFullName := Format('%s.%s.%s', [
                GetNodeData(aNode).TagValue, Data^.TagValue, Data^.TagName]);
          end;
        end;

        // поле
        if aFieldType = 2 then begin
          if (Length(edtTagName.Text) > 3) or
             (not TRegEx.IsMatch(edtTagName.Text, '\A(\d{1,2})([A-Z]{1})?\Z'))  then
            raise Exception.CreateFmt('%s - Неверное наименование поля!', [ edtTagName.Text ]);
          FullName := '';
          // поле добавляем перед последним узлом
          Result := VST.InsertNode(aNode.LastChild, amInsertBefore);
          Data := GetNodeData(Result);
          if Assigned(Data) then begin
            Data^.TagName     := edtTagName.Text;
            Data^.TagValue    := edtTagValue.Text;
            SeqNode := aNode.Parent;
            SubNode := aNode;
            Qualifier := GetBetweenEx(':', '/', edtTagValue.Text);
            if (Assigned(SeqNode)) and (SeqNode <> VST.RootNode) then
              FullName := GetNodeData(SeqNode).TagValue;
            FullName := FullName + '.' + GetNodeData(aNode).TagValue;
            FullName := FullName + '.' + edtTagName.Text;
            if Qualifier > '' then
              FullName := FullName + '.' + Qualifier;
            Data^.TagFullName := FullName;
            Data^.Editable    := True;
          end;
        end;
        // Tag = :<имя поля>:<значение поля>
        FSwiftMessage.Block4.AddTag(TSwiftTag.Create(
          Format(':%s:%s', [Data^.TagName, Data^.TagValue])));
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
  sHeader, eReciever: string;
  Data: PSwiftData;

begin
  Result := '';
  eBuffer := TSWIFTBuilder.Create();
  try
    sHeader := Format('{1:%s}', [FSwiftMessage.Block1.Text]);
    eReciever := edtReciever.Text;
    eReciever := Format('{2:I%d%sX%sN}',
                  [FMsgType,
                   Copy(eReciever, 1, 8),
                   StrRightPad(Copy(eReciever, 9, 3), 3, 'X')]);

    sHeader := sHeader + eReciever;

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
  eIsEditable, IsSequence: Boolean;
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
      IsSequence := False;
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
        IsSequence := True;
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
      {$IFDEF DEBUG}
      eData^.Editable    := (not IsSequence);
      {$ELSE}
      eData^.Editable    := (not FReadOnlyMsg) and (IsEditable(eTmpTag.FullName));
      {$ENDIF}
      { TODO: Замена после перехода на БД }
      case FMsgType of
      518:     eData^.Content := GetContentFields(cSwiftFieldsFour518, eTmpTag.FullName);
      541,543: eData^.Content := GetContentFields(cSwiftFieldsFour541, eTmpTag.FullName);
      end;
      eData^.MultiLine   := (Pos(#$D#$A, eTmpTag.Value) > 0) or
                            (GetLineCountFromContent(eData.Content) > 1);

      eData^.HasDelete   := False;
    end;
    VST.FocusedNode := VST.RootNode.FirstChild;
  end;
end;

function TfSwiftView.Allowed: Boolean;
begin
  Result := True;
  if not Assigned(VST.FocusedNode) then begin
    MessageDlg('Вы должны установить фокус на запись!', mtInformation, [mbOK], 0);
    Result := False;
  end;
end;

procedure TfSwiftView.miAddFieldClick(Sender: TObject);
var
  NewNode: PVirtualNode;
begin
  if not Allowed then Exit;
  if Assigned(VST.FocusedNode) then begin
    if GetNodeData(VST.FocusedNode).TagName = '16R' then
      NewNode := AddField(VST.FocusedNode, 2)
    else
      NewNode := AddField(VST.FocusedNode.Parent, 2);
  end;
  if Assigned(NewNode) then
    VST.FocusedNode := NewNode;
end;

procedure TfSwiftView.miAddSeqClick(Sender: TObject);
var
  NewNode: PVirtualNode;
begin
  if not Allowed then Exit;
  NewNode := AddField(VST.RootNode, 0);
  if Assigned(NewNode) then begin
    VST.FocusedNode := NewNode;
    VST.FullExpand(NewNode);
  end;
end;

procedure TfSwiftView.miAddSubSeqClick(Sender: TObject);
var
  NewNode: PVirtualNode;
begin
  if not Allowed then Exit;
  if (VST.FocusedNode.Parent <> VST.RootNode) then
    NewNode := AddField(VST.FocusedNode.Parent, 1);
  if Assigned(NewNode) then begin
    VST.FocusedNode := NewNode;
    VST.FullExpand(NewNode);
  end;
end;

procedure TfSwiftView.miDelFieldClick(Sender: TObject);
var
  ConfirmMsg: string;
begin
  if not Allowed then Exit;
  // Удаление поля
  if MessageDlg('Вы действительно хотите удалить запись?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    if (GetNodeData(VST.FocusedNode).TagName = '16R') then begin
      if MessageDlg('Будут удалены все дочерние записи. Согласны?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        VST.DeleteChildren(VST.FocusedNode);
    end;
    VST.DeleteNode(VST.FocusedNode);
  end;
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

    // Доступность кнопок и пунктов popup меню
    if not FReadOnlyMsg then begin
      tbtnAddSeq.Enabled    := True;
      tbtnAddSubSeq.Enabled := True;
      tbtnAddField.Enabled  := True;
      tbtnDelField.Enabled  := True;
      miAddSeq.Enabled      := True;
      miAddSubSeq.Enabled   := True;
      miAddField.Enabled    := True;
      miDelField.Enabled    := True;
    end;

    Data := GetNodeData(FocusedNode);
    PnlEditor.Visible  := True;
    lblTagName.Caption := Data.TagName;
    lblTagName.Enabled := True;
    pnlEditor.Height   := 30;
    if Data.MultiLine then begin
      mmoTagValue.Text     := Data.TagValue;
      mmoTagValue.Visible  := True;
      edtTagValue.Visible  := False;
      mmoTagValue.Enabled  := Data.Editable;
      pnlEditor.Height     := 100;
    end else begin
      edtTagValue.Text     := Data.TagValue;
      mmoTagValue.Visible  := False;
      edtTagValue.Visible  := True;
      edtTagValue.Enabled  := Data.Editable;
    end;
  end;
end;

procedure TfSwiftView.VSTExit(Sender: TObject);
begin
  tbtnAddSeq.Enabled    := False;
  tbtnAddSubSeq.Enabled := False;
  tbtnAddField.Enabled  := False;
  tbtnDelField.Enabled  := False;
  miAddSeq.Enabled      := False;
  miAddSubSeq.Enabled   := False;
  miAddField.Enabled    := False;
  miDelField.Enabled    := False;
end;

end.
