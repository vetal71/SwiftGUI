program SwiftGUI;

uses
  Forms,
  SWG_FMain in 'SWG_FMain.pas' {fMain},
  SWIFT_FViewer in 'SWIFT_FViewer.pas' {fSwiftView: TFrame},
  SWIFT_FDlgAddField in 'SWIFT_FDlgAddField.pas' {FDlgAddField};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfMain, fMain);
  Application.CreateForm(TFDlgAddField, FDlgAddField);
  Application.Run;
end.
