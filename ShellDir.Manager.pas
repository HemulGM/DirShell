unit ShellDir.Manager;

interface

uses
  Vcl.Forms, System.Classes, System.SysUtils, Vcl.Dialogs, System.Generics.Collections, ShellDir.Form, ShellDir.Settings, HGM.Common.Settings;

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
var FG: Integer;
begin
  FG := FSettings.GetInt('General', 'GUID', 0);
  Inc(FG);
  FSettings.SetInt('General', 'GUID', FG);
  Result := 'Window_'+FG.ToString;
end;

procedure TManager.Load;
var List: TStringList;
    i: Integer;
begin
  List := TStringList.Create;

  if FSettings.GetSections('Windows', List) then
  begin
    for i := 0 to List.Count-1 do
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
  for i := 0 to FWindows.Count-1 do
  begin
    FWindows[i].Show;
  end;
end;

procedure TManager.Save;
begin

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
