unit SWIFT_FDlgAddField;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TFDlgAddField = class(TForm)
    pnlButtons: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    pnlMain: TPanel;
    lbl3: TLabel;
    edtTagName: TEdit;
    lbl4: TLabel;
    edtTagValue: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FDlgAddField: TFDlgAddField;

implementation

{$R *.dfm}

end.
