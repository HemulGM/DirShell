unit ShellDir.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, acPNG,
  Vcl.ExtCtrls, System.Generics.Collections, System.Generics.Defaults,
  System.ImageList, Vcl.ImgList, Direct2D, D2D1, Vcl.StdCtrls, HGM.Button,
  System.Win.TaskbarCore, Vcl.Taskbar, Winapi.Dwmapi;

type
  TShellItem = record
    FileName: string;
    DisplayText: string;
    IconIndex: Integer;
  end;

  TShellItems = class(TList<TShellItem>)
  end;

  TBMPImages = TList<ID2D1Bitmap>;

  TFormPanel = class(TForm)
    ImageListIcons: TImageList;
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
  private
    FVertOffset: Integer;
    FColCount: Integer;
    FIconSpaceW: Double;
    FItems: TShellItems;
    FMouseCord: TPoint;
    FItemUnderMouse: Integer;
    FID: string;
    FImages: TBMPImages;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetID(const Value: string);
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure WndProc(var Message: TMessage); override;
  public
    procedure UpdateGrid;
    procedure Save;
    procedure Load;
    function AddFile(FileName: string): Integer;
    property ID: string read FID write SetID;
  end;

const
  IconSpaceW = 5;
  IconSpaceH = 10;
  IconWidth = 20 + 32 + 20;
  IconHeight = 10 + 32 + 10 + 20 + 10;
  MainOffset: Integer = 0;

var
  FormPanel: TFormPanel;

  //C:\Users\Геннадий\Desktop\Графика

implementation

uses
  Math, ShellDir.Manager, ShellApi;

{$R *.dfm}

function GetFileIcon(const FileName: TFileName; Size: Integer; IL: TImageList): Integer;
var
  Icon: TIcon;
  Icon32, Icon16, IcEx: HICON;
  i: word;
begin
  Result := -1;
  try
    IcEx := ExtractAssociatedIcon(0, PChar(FileName), i);
    if IcEx > 0 then
    begin
      Icon := TIcon.Create;
      if Integer(ExtractIconEx(PWideChar(FileName), i, Icon32, Icon16, 1)) > 0 then
        case Size of
          16:
            IcEx := Icon16;
          32:
            IcEx := Icon32;
        end;
      Icon.Handle := IcEx;
      Result := IL.AddIcon(Icon);
      Icon.Free;
    end;
  except
    on E: Exception do
      Exit;
  end;
end;

procedure TFormPanel.FormActivate(Sender: TObject);
begin
  //SendToBack;
end;

function TFormPanel.AddFile(FileName: string): Integer;
var
  Item: TShellItem;
begin
  Item.FileName := FileName;
  Item.DisplayText := ExtractFileName(FileName);
  Item.IconIndex := GetFileIcon(FileName, 32, ImageListIcons);
  Result := FItems.Add(Item);
  FItems.Sort(TComparer<TShellItem>.Construct(
    function(const Left, Right: TShellItem): Integer
    begin
      Result := AnsiCompareStr(Left.DisplayText, Right.DisplayText);
    end));
end;

procedure TFormPanel.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_BORDER or WS_THICKFRAME;
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

procedure TFormPanel.WndProc(var Message: TMessage);
var
  i, Amount, NameSize: integer;
  FileName: PChar;
  FN: string;
begin
  if Message.Msg = WM_DROPFILES then
  begin
    Amount := DragQueryFile(Message.WParam, $FFFFFFFF, FileName, 255);
    for i := 0 to (Amount - 1) do
    begin
      NameSize := DragQueryFile(Message.WParam, i, nil, 0) + 1;
      FileName := StrAlloc(NameSize);
      DragQueryFile(Message.WParam, i, FileName, NameSize);
      FN := StrPas(FileName);
      AddFile(FN);
      StrDispose(FileName);
    end;
    DragFinish(Message.WParam);
  end;
  inherited WndProc(Message);
end;

procedure TFormPanel.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited;
end;

procedure TFormPanel.FormCreate(Sender: TObject);
var
  i: Integer;
  Item: TShellItem;
begin
  FImages := TBMPImages.Create;
  DragAcceptFiles(Handle, True);
  FVertOffset := 0;
  FColCount := 1;
  FIconSpaceW := IconSpaceW;
  FItems := TShellItems.Create;
  FormResize(nil);
end;

procedure TFormPanel.FormDeactivate(Sender: TObject);
begin
  //SendToBack;
end;

procedure TFormPanel.FormDestroy(Sender: TObject);
begin
  FImages.Clear;
  FImages.Free;
  FItems.Clear;
  FItems.Free;
end;

procedure TFormPanel.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FItemUnderMouse < 0 then
  begin
    ReleaseCapture;
    SendMessage(Handle, WM_SYSCOMMAND, 61458, 0);
  end;
end;

procedure TFormPanel.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FMouseCord := TPoint.Create(X, Y);
  Repaint;
  if FItemUnderMouse >= 0 then
    Cursor := crHandPoint
  else
    Cursor := crDefault;
end;

procedure TFormPanel.FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;
  FVertOffset := FVertOffset - 20;
  UpdateGrid;
  Repaint;
end;

procedure TFormPanel.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;
  FVertOffset := FVertOffset + 20;
  UpdateGrid;
  Repaint;
end;

procedure TFormPanel.FormPaint(Sender: TObject);
var
  i: Integer;
  TxtRect, ItemRect, IconRect: TRect;
  C, R, W: Integer;
  S: string;
  Icon: TIcon;
  txSize: TSize;
  PaintStruct: TPaintStruct;
begin
  BeginPaint(Handle, PaintStruct);
  with Canvas {TDirect2DCanvas.Create(Canvas, ClientRect)} do
  begin
    //BeginDraw;
    //RenderTarget.SetAntialiasMode(D2D1_ANTIALIAS_MODE_ALIASED);
    FItemUnderMouse := -1;
    //Pen.Brush.Handle.SetOpacity(0.5);
    for i := 0 to FItems.Count - 1 do
    begin
      C := i mod FColCount;
      R := i div FColCount;
      if FColCount > 1 then
        ItemRect := TRect.Create(TPoint.Create(Round(C * (IconWidth + FIconSpaceW) + FIconSpaceW), R * (IconHeight + IconSpaceH) + 10), IconWidth, IconHeight)
      else
        ItemRect := TRect.Create(TPoint.Create(ClientWidth div 2 - IconWidth div 2, R * (IconHeight + IconSpaceH) + 10), IconWidth, IconHeight);
      ItemRect.Offset(0, FVertOffset);

      //Выделение элемента
      if PtInRect(ItemRect, FMouseCord) and MouseInClient then
      begin
        FItemUnderMouse := i;
        Brush.Style := bsSolid;
        Brush.Color := clWhite;
        Pen.Color := clWhite;
        //Brush.Handle.SetOpacity(0.2);
        RoundRect(ItemRect, 2, 2);
      end;

    //end;
  //end;
  //with Canvas do
  //begin
      Font.Name := 'Segoe UI Light';
      Font.Size := 10;
      Pen.Style := psClear;
      Brush.Style := bsClear;
    //for i := 0 to FItems.Count - 1 do
    //begin
    {  C := i mod FColCount;
      R := i div FColCount;
      if FColCount > 1 then
        ItemRect := TRect.Create(TPoint.Create(Round(C * (IconWidth + FIconSpaceW) + FIconSpaceW), R * (IconHeight + IconSpaceH) + 10), IconWidth, IconHeight)
      else
        ItemRect := TRect.Create(TPoint.Create(ClientWidth div 2 - IconWidth div 2, R * (IconHeight + IconSpaceH) + 10), IconWidth, IconHeight);
      ItemRect.Offset(0, FVertOffset); }
    //Иконка
      IconRect := ItemRect;
      IconRect.Height := ItemRect.Height - 25;
      Icon := TIcon.Create;
      ImageListIcons.GetIcon(FItems[i].IconIndex, Icon);
      //ImageListIcons.Draw(Canvas, IconRect.CenterPoint.X - 32 div 2, IconRect.CenterPoint.Y - 32 div 2, FItems[i].IconIndex, True);
      Draw(IconRect.CenterPoint.X - 32 div 2, IconRect.CenterPoint.Y - 32 div 2, Icon);
      Icon.Free;

    //Подпись
      S := FItems[i].DisplayText;

      TxtRect := ItemRect;
      TxtRect.Height := 38;
      TxtRect.Offset(0, IconHeight - TxtRect.Height);
      W := Min(ItemRect.Width - 10, TextWidth(S)) + 8;
      TxtRect.Left := TxtRect.Left + ItemRect.Width div 2 - W div 2;
      TxtRect.Width := W;
           {
      //Выделение текста
      if PtInRect(ItemRect, FMouseCord) and MouseInClient then
      begin
        Pen.Style := psClear;
        Brush.Style := bsSolid;
        Brush.Color := $00363535;
        //Brush.Handle.SetOpacity(1);
        RoundRect(TxtRect, 4, 4);
      end;  }
    //Font.Brush.Handle.SetOpacity(0.9);
      Font.Color := $00888888;
      TxtRect.Offset(-1, 0);
      TextRect(TxtRect, S, [tfVerticalCenter, tfWordBreak, tfCenter, tfEndEllipsis]);
      TxtRect.Offset(+2, 0);
      TextRect(TxtRect, S, [tfVerticalCenter, tfWordBreak, tfCenter, tfEndEllipsis]);
      TxtRect.Offset(-1, -1);
      TextRect(TxtRect, S, [tfVerticalCenter, tfWordBreak, tfCenter, tfEndEllipsis]);
      TxtRect.Offset(0, +2);
      TextRect(TxtRect, S, [tfVerticalCenter, tfWordBreak, tfCenter, tfEndEllipsis]);

      TxtRect.Offset(0, -1);
    //Font.Brush.Handle.SetOpacity(1);
      Font.Color := clWhite;
      TextRect(TxtRect, S, [tfVerticalCenter, tfWordBreak, tfCenter, tfEndEllipsis]);
    end;
    //EndDraw;
    //Free;
  end;
  EndPaint(Handle, PaintStruct);
end;

procedure TFormPanel.FormResize(Sender: TObject);
begin
  FColCount := (ClientWidth - IconSpaceW) div (IconWidth + IconSpaceW);
  FIconSpaceW := (ClientWidth - FColCount * IconWidth) / (FColCount + 1);
  UpdateGrid;
  Invalidate;
end;

procedure TFormPanel.Load;
begin

end;

procedure TFormPanel.Save;
begin

end;

procedure TFormPanel.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TFormPanel.UpdateGrid;
begin
  FVertOffset := Min(0, Max(FVertOffset, -((FItems.Count - 1) div FColCount) * (IconHeight + IconSpaceH)));
end;

end.

