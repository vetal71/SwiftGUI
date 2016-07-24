unit SWG_FMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, VirtualTrees;

type
  TfMain = class(TForm)
    pnlButtons: TPanel;
    pnlMain: TPanel;
    pgcMain: TPageControl;
    tsText: TTabSheet;
    tsView: TTabSheet;
    mmoText: TMemo;
    scbView: TScrollBox;
    btnLoad: TButton;
    btnEditor: TButton;
    pnlButtonView: TPanel;
    btnApply: TButton;
    btnCancel: TButton;
    btnValidate: TButton;
    procedure btnLoadClick(Sender: TObject);
    procedure btnEditorClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function CreateSwiftView: Boolean;
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

uses
  Swift_UViewer;

{$R *.dfm}

function TfMain.CreateSwiftView: Boolean;
var
  eSwiftPnl: TfSwiftView;
begin
  eSwiftPnl := TfSwiftView.Create(Owner);
  with eSwiftPnl do begin
    Parent  := scbView;
    Align   := alClient;
    MsgType := 518;
    MsgText := mmoText.Text;
  end;
  Result := True;
end;


procedure TfMain.FormShow(Sender: TObject);
begin
  mmoText.Lines.LoadFromFile(ExtractFilePath( ParamStr(0)) + 'MT518Test.txt');
end;

procedure TfMain.btnEditorClick(Sender: TObject);
begin
  if CreateSwiftView then pgcMain.ActivePage := tsView
  else
    ShowMessage('Ќе удалось представить текст swift сообщени€ дл€ редактировани€');
end;

procedure TfMain.btnLoadClick(Sender: TObject);
begin
  // загрузка файла
  with TOpenDialog.Create(Application) do begin
    Filter := 'Text|*.txt';
    InitialDir := ExtractFilePath( ParamStr(0) + '\Samples\' );
    if Execute then
      mmoText.Lines.LoadFromFile(FileName);

    Free;
  end;
end;

end.
