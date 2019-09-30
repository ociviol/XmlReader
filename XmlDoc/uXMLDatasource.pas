unit uXMLDatasource;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
interface

uses
  Windows, Classes, SysUtils, contnrs, UXmlDoc,
  MSAccess, cxCustomData, cxGridTableView, cxTLData;

type
  { XML CUSTOM DATASOURCE }

  TXMLTvDataSource = class(TcxTreeListCustomDataSource)
  private
    FXmlDoc: TXMLDoc;
  protected
    function GetChildCount(AParentHandle: TcxDataRecordHandle): integer; override;
    function GetChildRecordHandle(AParentHandle: TcxDataRecordHandle; AChildIndex: integer): TcxDataRecordHandle; override;
    function GetRootRecordHandle: TcxDataRecordHandle; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant; override;
  public
    constructor Create(ADoc: TXMLDoc);
  end;

  TXMLDataSource = class(TcxCustomDataSource)
  private
    FXmlElement : TXMLElement;
  public
    constructor Create(AElement: TXMLElement);
    function GetItemHandle(AItemIndex: integer): TcxDataItemHandle; override;
    function GetRecordCount: integer; override;
    function GetRecordHandle(ARecordIndex: integer): TcxDataRecordHandle; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant; override;
    procedure SetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle; const AValue: Variant); override;
  end;

implementation


constructor TXMLTvDataSource.Create(ADoc: TXMLDoc);
begin
  FXmldoc := aDoc;
end;

function TXMLTvDataSource.GetChildCount(AParentHandle: TcxDataRecordHandle): integer;
begin
  if not Assigned(AParentHandle) then
    result := FXmlDoc.DocumentElement.NbElements
  else
    result := TXMLElement(AParentHandle).NbElements;
end;

function TXMLTvDataSource.GetChildRecordHandle(AParentHandle: TcxDataRecordHandle; AChildIndex: integer): TcxDataRecordHandle;
begin
  result := TcxDataRecordHandle(TXMLElement(AParentHandle).Elements[AChildIndex]);
end;

function TXMLTvDataSource.GetRootRecordHandle: TcxDataRecordHandle;
begin
  result := TcxDataRecordHandle(FXmlDoc.DocumentElement);
end;

function TXMLTvDataSource.GetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant;
begin
  result := TXMLElement(ARecordHandle).TagName;
end;


{  TXMLDATASOURCE }

constructor TXMLDataSource.Create(AElement: TXMLElement);
begin
  FXmlElement := aElement;
end;

function TXMLDataSource.GetItemHandle(AItemIndex: integer): TcxDataItemHandle;
begin
  result := TcxDataItemHandle(AItemIndex);
end;

function TXMLDataSource.GetRecordCount: integer;
begin
  result := FXmlElement.NbElements;
end;

function TXMLDataSource.GetRecordHandle(ARecordIndex: integer): TcxDataRecordHandle;
begin
  result := TcxDataItemHandle(FXmlElement.Elements[ARecordIndex]);
end;

function TXMLDataSource.GetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant;
begin
  with TXMLElement(ARecordHandle) do
    result := TcxDataItemHandle(GetAttribute(GetAttributeName(AItemHandle)));
end;

procedure TXMLDataSource.SetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle; const AValue: Variant);
begin
  with TXMLElement(ARecordHandle) do
    TcxDataItemHandle(SetAttribute(GetAttributeName(AItemHandle), AValue));
end;


end.
