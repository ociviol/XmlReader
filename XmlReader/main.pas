unit main;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$MODE Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Windows, Messages, Menus, ExtCtrls, Grids, ufxmlviewer;

const
  S_CAPTION = 'XML Editor 2.0';

type

  { TFXmlViewer }

  TFXmlViewer = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    mnuOpenXml: TMenuItem;
    mnuQuit: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    N1: TMenuItem;
    About1: TMenuItem;
    Loadfromclipboard1: TMenuItem;
    Copytoclipboard1: TMenuItem;
    PageControl1: TPageControl;
    New1: TMenuItem;
    mnuClose1: TMenuItem;
    PopupMenu1: TPopupMenu;
    mnuClose2: TMenuItem;
    Edit1: TMenuItem;
    mnuTabs1: TMenuItem;
    N2: TMenuItem;
    Save1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure mnuOpenXmlClick(Sender: TObject);
    procedure mnuQuitClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Loadfromclipboard1Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure Copytoclipboard1Click(Sender: TObject);
    procedure SaveDialog1Show(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure mnutab1Click(Sender: TObject);
    procedure mnuClose1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Save1Click(Sender: TObject);
  private
    { private declarations }
    FFilename: String;
    procedure LoadFile(filename: String);
    function GetActiveXmlView: TFxmlView;
    // procedure Doload2(var Msg : TMessage); message WM_USER+1;
    //procedure ReceiveData_Handler(var msg: TWMCopyData); message WM_COPYDATA;
    //procedure DoLoad(var msg: TMessage); message WM_USER + 2;
    function AddNewXmlreader: TFxmlView;
    procedure SetFrameBounds(aFrame: TFxmlView);
    property ActiveXmlview: TFxmlView read GetActiveXmlView;
  public
    { public declarations }
  end;

var
  FXmlViewer: TFXmlViewer;

implementation

{$R *.lfm}

uses
  Inifiles, Clipbrd;
{ TFXmlViewer }

function TFXmlViewer.GetActiveXmlView: TFxmlView;
begin
  // set new active xml object
  result := TFxmlView(PageControl1.activepage.Controls[0]);
end;

procedure TFXmlViewer.LoadFile(filename: String);
begin
  with ActiveXmlview do
    try
      FFilename := filename;
      LoadFromFile(filename);
      PageControl1.activepage.Caption := ExtractFileName(filename);
      self.Caption := S_CAPTION;
    Except
      on e: Exception do
        ShowMessage(e.message);
    end;
end;

procedure TFXmlViewer.mnuClose1Click(Sender: TObject);
var
  t: TTabSheet;
begin
  if PageControl1.PageCount = 0 then
    Exit;

  t := PageControl1.activepage;
  // delete XML Viewer
  TFxmlView(t.Controls[0]).Free;
  // delete tabsheet
  t.Free;
end;

procedure TFXmlViewer.mnuOpenXmlClick(Sender: TObject);
begin
  with OpenDialog1 do
    if Execute then
      LoadFile(filename);
end;

procedure TFXmlViewer.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  i: Integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TFxmlView then
      if TFxmlView(Components[i]).Modified then
        case MessageDlg(Format('file %s has changed, save changes ?',
          [TFxmlView(Components[i]).XmlDoc.filename]), mtConfirmation,
          [mbYes, mbNo, MbCancel], 0) of
          mrYes:
            TFxmlView(Components[i]).Save;

          mrNo:
            Continue;

          mrCancel:
            begin
              CanClose := False;
              Exit;
            end;
        end;

  CanClose := True;
end;

procedure TFXmlViewer.FormCreate(Sender: TObject);
var
  s: string;
begin
  s := Application.Exename;
  s := copy(s, 1, Length(s) - 3) + 'ini';

  with TInifile.Create(s) do
    try
      if ReadBool('bounds', 'Maxed', False) then
        WindowState := wsMaximized
      else
      begin
        Width := ReadInteger('bounds', 'Width', 1130);
        Height := ReadInteger('bounds', 'Height', 800);
      end;

      AddNewXmlreader;
    finally
      Free;
    end;
  Caption := S_CAPTION;
end;

procedure TFXmlViewer.FormDestroy(Sender: TObject);
var
  s: string;
begin
  s := Application.Exename;
  s := copy(s, 1, Length(s) - 3) + 'ini';

  with TInifile.Create(s) do
    try
      if WindowState = wsMaximized then
        WriteBool('bounds', 'Maxed', True)
      else if WindowState <> wsMinimized then
      begin
        WriteBool('bounds', 'Maxed', False);
        WriteInteger('bounds', 'Width', Width);
        WriteInteger('bounds', 'Height', Height);
      end;
      if Assigned(ActiveXmlview) then
        with ActiveXmlview do
        begin
          WriteInteger('bounds', 'PnlAttrib', PnlAttrib.Width);
          WriteInteger('bounds', 'Panel4', Panel4.Height);
          WriteInteger('bounds', 'Col1', AttrGrid.ColWidths[0]);
          WriteInteger('bounds', 'Col2', AttrGrid.ColWidths[1]);
          //WriteBool('ScriptMemo', 'Wordwrap', SynMemo1.WordWrap);
        end;
    finally
      Free;
    end;
end;

procedure TFXmlViewer.FormShow(Sender: TObject);
begin
  if ParamCount > 0 then
    PostMessage(handle, WM_USER + 2, 0, 0);
end;

function TFXmlViewer.AddNewXmlreader: TFxmlView;
var
  TabSheet: TTabSheet;
begin
  result := TFxmlView.Create(self);

  TabSheet := TTabSheet.Create(PageControl1);
  TabSheet.PageControl := PageControl1;
  TabSheet.Caption := 'Untitled';

  with result do
  begin
    name := 'fmXmlView' + inttostr(PageControl1.PageCount + 1);
    parent := TabSheet;
    Align := alClient;
    // TreeView1.PopupMenu := PopupMenu1;
    // SynMemo1.PopupMenu  := PopupMenu2;
  end;
  PageControl1.ActivePageIndex := PageControl1.PageCount - 1;
  SetFrameBounds(result);
end;

procedure TFXmlViewer.SetFrameBounds(aFrame: TFxmlView);
var
  s: string;
begin
  s := Application.Exename;
  s := copy(s, 1, Length(s) - 3) + 'ini';

  with TInifile.Create(s) do
    try
      with aFrame do
      begin
        PnlAttrib.Width := ReadInteger('bounds', 'PnlAttrib', 445);
        Panel4.Height := ReadInteger('bounds', 'Panel4', 252);
        AttrGrid.ColWidths[0] := ReadInteger('bounds', 'Col1', 200);
        AttrGrid.ColWidths[1] := ReadInteger('bounds', 'Col2', 200);
        AttrGrid.Cells[0, 0] := 'Attribute';
        AttrGrid.Cells[1, 0] := 'Value';
        //SynMemo1.WordWrap := ReadBool('ScriptMemo', 'Wordwrap', False);
      end;
    finally
      Free;
    end;
end;

procedure TFXmlViewer.About1Click(Sender: TObject);
begin
  ShowMessage(S_CAPTION + ' ® O.Civiol 2014');
end;

procedure TFXmlViewer.Copytoclipboard1Click(Sender: TObject);
begin
  Clipboard.AsText := ActiveXmlview.XmlDoc.AsString;
end;
{
procedure TFXmlViewer.DoLoad(var msg: TMessage);
begin
  LoadFile(ParamStr(1));
end;

procedure TFXmlViewer.ReceiveData_Handler(var msg: TWMCopyData);
var
  s : string;
begin
  BringToFront;
  AddNewXmlreader;
  s := PAnsiChar(msg.CopyDataStruct.lpData);
  LoadFile(s);
end;
}
procedure TFXmlViewer.MenuItem1Click(Sender: TObject);
begin
  Loadfromclipboard1.Enabled := Clipboard.HasFormat(CF_TEXT);
  Copytoclipboard1.Enabled := Assigned(ActiveXmlview.XmlDoc.DocumentElement);
  if Assigned(ActiveXmlview.XmlDoc.DocumentElement) then
    Save1.Enabled := ActiveXmlview.Modified;
end;

procedure TFXmlViewer.MenuItem2Click(Sender: TObject);
begin
  with SaveDialog1 do
    if Execute then
      ActiveXmlview.XmlDoc.SaveToFile(filename);
end;

procedure TFXmlViewer.mnuQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TFXmlViewer.mnutab1Click(Sender: TObject);
begin
  mnuClose1.Enabled := (PageControl1.PageCount > 1);
  mnuClose2.Enabled := (PageControl1.PageCount > 1);
end;

procedure TFXmlViewer.New1Click(Sender: TObject);
begin
  AddNewXmlreader;
end;

procedure TFXmlViewer.Save1Click(Sender: TObject);
begin
  ActiveXmlview.Save;
end;

procedure TFXmlViewer.SaveDialog1Show(Sender: TObject);
begin
  SaveDialog1.InitialDir := ExtractFilePath(FFilename);
end;

procedure TFXmlViewer.Loadfromclipboard1Click(Sender: TObject);
begin
  if Assigned(ActiveXmlview.XmlDoc.DocumentElement) then
    AddNewXmlreader;
  ActiveXmlview.LoadFromText(Clipboard.AsText);
end;

end.