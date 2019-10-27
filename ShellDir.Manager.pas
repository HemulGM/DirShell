unit ShellDir.Manager;

interface

uses
  Vcl.Forms, System.Classes, System.SysUtils, Vcl.Dialogs, Winapi.Windows,
  System.Generics.Collections, ShellDir.Form, ShellDir.Settings,
  HGM.Common.Settings, Vcl.Controls;

type
  TListOfWindow = class(TList<TFormPanel>)
    function Add: TFormPanel;
  end;

  TManager = class
  private
    FWindows: TListOfWindow;
    FSettings: TSettingsReg;
    function GetGUID: string;
    procedure SetSettings(const Value: TSettingsReg);
  public
    constructor Create;
    destructor Destroy;
    procedure Run;
    procedure Load;
    procedure Save;
    procedure SetFormsOnTaskBar(Value: Boolean);
    property Settings: TSettingsReg read FSettings write SetSettings;
  end;

var
  Manager: TManager;

implementation

{ TManager }

constructor TManager.Create;
begin
  FWindows := TListOfWindow.Create;
  FSettings := TSettingsReg.Create(rrHKCU, 'Software\ShellDir');
end;

destructor TManager.Destroy;
begin
  FWindows.Clear;
  FWindows.Free;
end;

function TManager.GetGUID: string;
var
  FG: Integer;
begin
  FG := FSettings.GetInt('General', 'GUID', 0);
  Inc(FG);
  FSettings.SetInt('General', 'GUID', FG);
  Result := 'Window_' + FG.ToString;
end;

procedure TManager.Load;
var
  List: TStringList;
  i: Integer;
begin
  List := TStringList.Create;

  if FSettings.GetSections('Windows', List) then
  begin
    for i := 0 to List.Count - 1 do
    begin
      FWindows.Add.ID := List[i];
    end;
  end;
  List.Free;
  if FWindows.Count = 0 then
  begin
    FWindows.Add.ID := GetGUID;
  end;
end;

procedure TManager.Run;
var
  i: Integer;
begin
  Load;
  for i := 0 to FWindows.Count - 1 do
  begin
    FWindows[i].Show;
  end;
end;

procedure TManager.Save;
begin

end;

procedure ChangeAppWindow(const Handle: THandle; const SetAppWindow, RestoreVisibility: Boolean);
var
  Style: Integer;
  WasVisible, WasIconic: Boolean;
begin
  Style := GetWindowLong(Handle, GWL_EXSTYLE);
  if (SetAppWindow and (Style and WS_EX_APPWINDOW = 0)) or (not SetAppWindow and (Style and WS_EX_APPWINDOW = WS_EX_APPWINDOW)) then
  begin
    WasIconic := Winapi.Windows.IsIconic(Handle);
    WasVisible := IsWindowVisible(Handle);
    if WasVisible or WasIconic then
      ShowWindow(Handle, SW_HIDE);
    if SetAppWindow then
      SetWindowLong(Handle, GWL_EXSTYLE, Style or WS_EX_APPWINDOW)
    else
      SetWindowLong(Handle, GWL_EXSTYLE, Style and not WS_EX_APPWINDOW);
    if (RestoreVisibility and WasVisible) or WasIconic then
    begin
      if WasIconic then
        ShowWindow(Handle, SW_MINIMIZE)
      else
        ShowWindow(Handle, SW_SHOW);
    end;
  end;
end;

procedure TManager.SetFormsOnTaskBar(Value: Boolean);
var
  i: Integer;
begin
  for i := 0 to FWindows.Count - 1 do
  begin
    if Value then
    begin
      ChangeAppWindow(FWindows[i].Handle, False, False);
    end
    else
    begin
      ChangeAppWindow(FWindows[i].Handle, True, True);
    end;
      // Recreate Main form to ensure correct owner
    FWindows[i].Perform(CM_RECREATEWND, 0, 0);
    if Value then
    begin
      SetWindowLong(FWindows[i].Handle, GWL_EXSTYLE, GetWindowLong(FWindows[i].Handle, GWL_EXSTYLE) or WS_EX_TOOLWINDOW);
      SetWindowText(FWindows[i].Handle, nil)
    end
    else
    begin
      SetWindowLong(FWindows[i].Handle, GWL_EXSTYLE, GetWindowLong(FWindows[i].Handle, GWL_EXSTYLE) and not WS_EX_TOOLWINDOW);
      SetWindowText(FWindows[i].Handle, FWindows[i].Caption);
    end;
  end;
end;

procedure TManager.SetSettings(const Value: TSettingsReg);
begin
  FSettings := Value;
end;

{ TListOfWindow }

function TListOfWindow.Add: TFormPanel;
begin
  Result := TFormPanel.Create(Application);
  inherited Add(Result);
end;

initialization
  Manager := TManager.Create;

finalization
  Manager.Free;

end.

