unit uAbout;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Label2Click(Sender: TObject);
  private
  public
  end;


implementation

{$R *.lfm}

uses
  LclIntf,
  Utils.SoftwareVersion;

{ TfrmAbout }

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  Label1.Caption := GetFileVersionInternalName + ' ' +
                    GetFileVersion + ' © ' + GetFileVersionCopyright;
end;

procedure TfrmAbout.Label2Click(Sender: TObject);
begin
  OpenUrl('https://ollivierciviolsoftware.wordpress.com/xmlreader/');
end;

end.

