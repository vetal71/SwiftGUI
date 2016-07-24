program SwiftGUI;

uses
  Forms,
  SWG_FMain in 'SWG_FMain.pas' {fMain},
  Swift_UViewer in 'Swift_UViewer.pas' {fSwiftView: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
