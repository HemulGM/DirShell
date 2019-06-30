program ShellDir;

uses
  Vcl.Forms,
  ShellDir.Form in 'ShellDir.Form.pas' {FormPanel},
  ShellDir.Settings in 'ShellDir.Settings.pas' {FormMain},
  ShellDir.Manager in 'ShellDir.Manager.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.ShowMainForm := False;
  Application.CreateForm(TFormMain, FormMain);
  Manager.Run;
  Application.Run;
end.
