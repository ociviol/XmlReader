program XmlReaderLinux;

{$MODE Delphi}

uses
  Forms, FileUtil,
  LCLIntf, LCLType, LMessages, Interfaces,
  Messages,
  SysUtils,
  Inifiles,
  main in 'main.pas',
  ufxmlviewer in '..\..\CAFE\CPXml\ufxmlviewer.pas' {FxmlView: TFrame},
  uxmldoc in '..\..\CAFE\CPXml\uxmldoc.pas',
  VariantUtils in '..\..\CAFE\VariantUtils.pas';

{$R *.res}

//var
  //PrevWindow: HWND;
  //S: AnsiString;
  //CData: TCopyDataStruct;

begin
  (*
  if OpenMutex(MUTEX_ALL_ACCESS, False, 'CPXmlViewer') <> 0 then
  begin
    PrevWindow := FindWindow(nil, S_CAPTION);

    if IsWindow(PrevWindow) then
    begin
      if FileExistsUTF8(ParamStr(1)) { *Converted from FileExists* } then
      begin
        S := ParamStr(1);
        CData.dwData := 0;
        CData.lpData := PAnsiChar(S);
        CData.cbData := 1 + Length(S);

        SendMessage(PrevWindow, WM_COPYDATA, 0, DWORD(@CData));
      end;
    end;
  end
  else
 *)
  begin
    //CreateMutex(nil, False, 'CPXmlViewer');
    Application.Initialize;
    Application.Title := 'Simple XmlReader';
    Application.CreateForm(TFXmlViewer, FXmlViewer);
    Application.Run;
  end;

end.
