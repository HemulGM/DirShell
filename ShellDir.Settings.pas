unit ShellDir.Settings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Menus, Vcl.StdCtrls,
  System.ImageList, Vcl.ImgList, HGM.Button;

type
  TFormMain = class(TForm)
    TrayIcon: TTrayIcon;
    PopupMenuTray: TPopupMenu;
    MenuItemQuit: TMenuItem;
    CheckBoxFlat1: TCheckBoxFlat;
    ImageList24: TImageList;
    MenuItemSettings: TMenuItem;
    N1: TMenuItem;
    CheckBoxShowOnTaskbar: TCheckBoxFlat;
    CheckBoxFlat3: TCheckBoxFlat;
    procedure MenuItemQuitClick(Sender: TObject);
    procedure MenuItemSettingsClick(Sender: TObject);
    procedure CheckBoxShowOnTaskbarClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.CheckBoxShowOnTaskbarClick(Sender: TObject);
begin
  Application.MainFormOnTaskBar := CheckBoxShowOnTaskbar.Checked;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caNone;
  Hide;
end;

procedure TFormMain.MenuItemQuitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFormMain.MenuItemSettingsClick(Sender: TObject);
begin
  Show;
  BringToFront;
end;

end.
