unit SWIFT_UPanel__;

interface

uses Windows, Controls, Graphics, SysUtils, Classes, ExtCtrls, StdCtrls,
     {SWIFT_UTypes,} VirtualTrees, SWIFT_UMsgFormat, SWIFT_UUtils, StrUtils,
     RegularExpressions;


const
  cSequenceTags: array [0..10] of string = (
    '15A', '15B', '15C', '15D', '15E', '15F', '15G', '15H', '15I', '16R', '16S');

  const cSequence518: array [0..4] of string = (
    'GENL', 'CONFDET', 'SETDET', 'OTHRPRTY', 'REPO');
  const cSubSequence518: array [0..5] of string = (
    'LINK', 'CONFPRTY', 'FIA', 'SETPRTY', 'CSHPRTY', 'AMT');

type

  TSwiftData = packed record
    TagName, TagValue: String;
    Editable, MultiLine: Boolean;
  end;
  PSwiftData = ^TSwiftData;

  TSwiftViewPanel = class(TCustomPanel)
  private
    { Панель ОТправитель-Получатель }
    FPnlTop: TPanel;
    FSenderEdit: TEdit;
    FSenderLabel: TLabel;
    FRecieverEdit: TEdit;
    FRecieverLabel: TLabel;
    { Панель редактирования поля }
    FPnlEditor: TPanel;
    FFieldLabel: TLabel;
    FFieldEdit: TEdit;
    FFieldMemo: TMemo;

    { панель представления }
    FPnlView: TPanel;
//    FTrvItems: TSwiftTreeView;
    FTrvItems: TVirtualStringTree;
    FIsMultiLine: Boolean;

    { Swift сообщение }
    FSwiftMessage: TSwiftMessage;
    FMsgText: string;
    FMsgType: Integer;

    procedure SetMsgText(aValue: string);
    function GetMsgText: string;

    procedure InitControls;
    procedure FillData;
    procedure BuildTree(aBlock: TSwiftBlock4);

    { События VirtualTreeView }
    procedure InitTreeNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure MeasureItem(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
    procedure PaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure BeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode;
      CellRect: TRect; var ContentRect: TRect);
    procedure GetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure GetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure TreeViewChange(Sender: TBaseVirtualTree; Node: PVirtualNode);

    procedure UpdateFromNode;
    function GetNodeData(const ANode: PVirtualNode): PSwiftData;

  protected
    procedure CreateWnd; override;
  public
    constructor Create(aOwner:TComponent); override;
    destructor Destroy; override;

    property MsgText: string read GetMsgText write SetMsgText;
    property MsgType: Integer read FMsgType write FMsgType;

  end;

implementation

uses
  Math, Dialogs;

function TSwiftViewPanel.GetNodeData(const ANode: PVirtualNode): PSwiftData;
begin
  // Для более короткого доступа к данным узла дерева
  Result := FTrvItems.GetNodeData(Anode);
end;

procedure TSwiftViewPanel.GetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TSwiftData);
end;

procedure TSwiftViewPanel.GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
begin
  if Column = 0 then
    CellText := GetNodeData(Node).TagName
  else if Column = 1 then
    CellText := GetNodeData(Node).TagValue
end;

procedure TSwiftViewPanel.PaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  FontColor: TColor;
begin
  if GetNodeData(Node).Editable then FontColor := clWhite else FontColor := clBlack;
  if (vsSelected in Node.States) and (Sender.Focused) then
    FontColor := clHighlightText;
  TargetCanvas.Font.Color := FontColor;
end;

procedure TSwiftViewPanel.BeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode;
  CellRect: TRect; var ContentRect: TRect);
var
  BgColor: TColor;
begin
  if GetNodeData(Node).Editable then BgColor := clWhite else BgColor := clBtnFace;
  with TargetCanvas do begin
    Brush.Color := BgColor;
    FillRect(CellRect);
  end;
end;

//------------------------------------------------------------------------------
// Включение/Выключение многострочного режима
//------------------------------------------------------------------------------
procedure TSwiftViewPanel.InitTreeNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if GetNodeData(Node).MultiLine then
    Include(InitialStates, ivsMultiline)
  else begin
    Exclude(InitialStates, ivsMultiline);
    // НЕ ЗАБЫВАЙТЕ ВЫПОЛНЯТЬ ЭТО:
    Node.States := Node.States - [vsMultiline];
    // Это выключит многострочность для узлов, где она раньше была.
  end;
end;

//---------------------------------------------------------------------------
// Подсчет высоты узлов в соответствии с высотой переносимого текста
//---------------------------------------------------------------------------
procedure TSwiftViewPanel.MeasureItem(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
begin
  if GetNodeData(Node).MultiLine then begin
    NodeHeight := FTrvItems.ComputeNodeHeight(TargetCanvas, Node, 0) + 4;
    NodeHeight := Max(18, NodeHeight);
  end else NodeHeight := 18;
end;


procedure TSwiftViewPanel.FillData;
begin
  // Отправитель и Получатель
  FSenderEdit.Text   := FSwiftMessage.Block1.Sender;
  FRecieverEdit.Text := FSwiftMessage.Block2.Reciever;

  // строим дерево полей
  BuildTree(FSwiftMessage.Block4);
  FTrvItems.FullExpand(nil);
end;

procedure TSwiftViewPanel.BuildTree(aBlock: TSwiftBlock4);
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

begin
  with FTrvItems do begin
    aBlock.RecreateTags;
    eRoot := RootNode;
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
      eData^.TagName   := eTmpTag.Name;
      eData^.TagValue  := eTmpTag.Value;
      eData^.Editable  := IsEditable(eTmpTag.FullName);
      eData^.MultiLine := Pos(#$D#$A, eTmpTag.Value) > 0;
    end;
  end;
end;

procedure TSwiftViewPanel.SetMsgText(aValue: string);
begin
  FMsgText := aValue;
  if FMsgText > '' then begin
    FSwiftMessage := TSwift.Load(FMsgType, FMsgText);
    if Assigned(FSwiftMessage) then
      FillData;
  end;
end;

procedure TSwiftViewPanel.TreeViewChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  // при изменении узла дерева
  UpdateFromNode;
end;

procedure TSwiftViewPanel.UpdateFromNode;
var
  Data: PSwiftData;
begin
  with FTrvItems do
  if FocusedNode = nil then begin
    FFieldLabel.Caption := '';
    FFieldLabel.Enabled := False;
    FFieldEdit.Text     := '';
    FFieldEdit.Enabled  := False;
    FPnlEditor.Visible  := False;
  end else begin
    Data := GetNodeData(FocusedNode);
    FPnlEditor.Visible  := True;
    FFieldLabel.Caption := Data.TagName;
    FFieldLabel.Enabled := True;
    if Data.MultiLine then begin
      FFieldMemo.Text     := Data.TagValue;
      FFieldMemo.Visible  := True;
      FFieldEdit.Visible  := False;
      FFieldMemo.Enabled  := Data.Editable;
    end else begin
      FFieldEdit.Text     := Data.TagValue;
      FFieldMemo.Visible  := False;
      FFieldEdit.Visible  := True;
      FFieldEdit.Enabled  := Data.Editable;
    end;
  end;
end;

function TSwiftViewPanel.GetMsgText: string;
begin
  { TODO: Собираем текст по дереву }
  Result := FMsgText;
end;

procedure TSwiftViewPanel.InitControls;
var
  EditorLeft: Integer;
begin
  { панель Отправитель - Получатель }
  FpnlTop:=TPanel.Create(Self);
  with FpnlTop do begin
    Parent     := Self;
    Height     := 60;
    Align      := alTop;
    BevelOuter := bvNone;
    BevelInner := bvLowered;
  end;
  { Отправитель }
  FSenderLabel := TLabel.Create(Self);
  with FSenderLabel do begin
    Parent  := FPnlTop;
    Caption := 'Отправитель:';
    SetBounds( 15, 10, 80, 13 );
    Font.Style := [fsBold];
  end;
  FSenderEdit := TEdit.Create(Self);
  EditorLeft  := FSenderLabel.Left + FSenderLabel.Width + 5;
  with FSenderEdit do begin
    Parent := FPnlTop;
    SetBounds( EditorLeft,
               7, 250, 21 );
    Hint := 'Swift код отправителя сообщения';
    Enabled := False;
  end;
  { Получатель }
  FRecieverLabel := TLabel.Create(Self);
  with FRecieverLabel do begin
    Parent  := FPnlTop;
    Caption := 'Получатель:';
    SetBounds( 15, 34, 80, 13 );
    Font.Style := [fsBold];
  end;
  FRecieverEdit := TEdit.Create(Self);
  with FRecieverEdit do begin
    Parent := FPnlTop;
    SetBounds( FSenderEdit.Left,
               31, 250, 21 );
    Hint := 'Swift код получателя сообщения';
    Enabled := False;
  end;

  { Панель редактора }
  FPnlEditor := TPanel.Create(Self);
  with FPnlEditor do begin
    Parent     := Self;
    Height     := 100;
    Align      := alBottom;
    BevelOuter := bvNone;
    BevelInner := bvLowered;
    Visible    := False;
  end;
  FFieldLabel := TLabel.Create(Self);
  with FFieldLabel do begin
    Parent  := FPnlEditor;
    Caption := 'Имя поля:';
    SetBounds( 15, 10, 80, 13 );
    Font.Style := [fsBold];
  end;
  FFieldEdit := TEdit.Create(Self);
  with FFieldEdit do begin
    Parent := FPnlEditor;
    SetBounds( FSenderEdit.Left,
               7, FPnlEditor.Width - FSenderEdit.Left - 5, 21 );
    Anchors := [akTop, akLeft, akRight];
    Enabled := True;
    Visible := True;
  end;
  FFieldMemo := TMemo.Create(Self);
  with FFieldMemo do begin
    Parent := FPnlEditor;
    SetBounds( FSenderEdit.Left, 7,
               FPnlEditor.Width - FSenderEdit.Left - 5, FPnlEditor.Height - 10 );
    ScrollBars := ssVertical;
    Anchors := [akTop, akLeft, akRight];
    Enabled := True;
    Visible := False;
  end;

  FPnlView := TPanel.Create(Self);
  with FPnlView do begin
    Parent     := Self;
    Align      := alClient;
    BevelOuter := bvNone;
    BevelInner := bvLowered;
  end;

//  FtrvItems := TSwiftTreeView.Create(Self);
  FTrvItems := TVirtualStringTree.Create(Self);
  with FtrvItems do  begin
    Parent := Self;
    Align  := alClient;
    NodeDataSize := SizeOf(TSwiftData);
  end;
end;

constructor TSwiftViewPanel.Create(aOwner:TComponent);
begin
  inherited Create(aOwner);
  BevelOuter:=bvNone;
  Caption:='';
  { Создание контролов }
  InitControls;

  { События }
  with FTrvItems do begin
    OnChange          := TreeViewChange;
    OnInitNode        := InitTreeNode;
    OnMeasureItem     := MeasureItem;
    OnPaintText       := PaintText;
    OnBeforeCellPaint := BeforeCellPaint;

    OnGetNodeDataSize := GetNodeDataSize;
    OnGetText         := GetText;
  end;
end;

procedure TSwiftViewPanel.CreateWnd;
begin
  inherited CreateWnd;

  with FtrvItems, TreeOptions do begin
    PaintOptions:=PaintOptions+[toShowButtons,        // display collapse/expand
                      toShowHorzGridLines,  // display horizontal lines
                      toShowRoot,           // show lines also at root level
                      toShowTreeLines,      // display tree lines to show
                                            // hierarchy of nodes
                                            // buttons left to a node
                      toShowVertGridLines]; // display vertical lines
                                            // (depending on columns) to
                                            // simulate a grid
//    MiscOptions      := MiscOptions + [toEditable];
    SelectionOptions := SelectionOptions + [toExtendedFocus];
                                            // to simulate a grid
    with Header do begin
      Height     := 18;
      Options    := Options + [hoVisible];
      Background := clBtnFace;
      AutoSize   := True;
      with Columns.Add do begin
        Text  := 'Имя поля';
        Width := 100;
      end;
      with Columns.Add do begin
        Text  := 'Значение';
        Width := 300;
      end;
    end;
  end;
end;

destructor TSwiftViewPanel.Destroy;
begin
  inherited;
end;

end.
