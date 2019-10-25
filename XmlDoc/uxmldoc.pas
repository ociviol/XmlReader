unit uxmldoc;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$MODE Delphi}


//
// XmlDoc : XMlDocument, Ollivier Civiol 2014
//

interface

{$define O_INLINE}

uses
  Classes, SysUtils, OXmlReadWrite, Variants;

const
  XML_CDATA   = 'CDATA';
  XML_COMMENT = 'COMMENT';
  XML_DOCTYPE = 'DOCTYPE';

type
  TXMLDoc = Class;

  { TXMLElement }

  TXMLElement = Class(TObject)
  private
    FElements : TList;
    FAttributes: TStringList;
    FTagName : String;
    FText: String;
    FLine : int64;
    FOwner : TXMLDoc;
    FParent : TXMLElement;
    FTokenType: TXMLReaderTokenType;
    function GetIndex: Integer;
    procedure SetTagName(aName : String);
  protected
    function GetElement(index : integer):TXMLElement; overload;
    function GetAttribute(index : integer):string; overload;
    function GetAttributeValue(index : integer):string;
    function GetShortAttributeValue(index : integer):string;

    function  GetNbElements:integer; inline;
    function  GetNbAttributes:integer; inline;
    function  GetAttribList:String;
    function  GetElemValue:String; inline;
    procedure SetElementValue(aValue : String);
    function  GetAttribs:String;
    function  GetLevel:Integer;
    function  GetText:String;
    procedure SetText(aText : String);
    procedure DoBeforeNodeChange;
    procedure DoAfterNodeChange;
    procedure DoOnAddChildNode(aChildNode : TXMLElement);
    function GetNodePath:String;
    function  GetLine:Int64; inline;
    procedure SetLine(aLine : int64);
    //function  GetTagName:String;
    function  GetElements:TList; inline;
    procedure StartLoad(Token: PXMLReaderToken; Reader : TXMLReader); 
    procedure Load(var Token: PXMLReaderToken; Reader : TXMLReader);
    procedure Save(Writer : TXMLWriter; omitXMLDeclaration : Boolean = False);
    function  Locate(aElem : TXMLElement) : Integer;
    function  GetParent:TXMLElement; inline;
    procedure MoveTo(aNewParent : TXMLElement);
    property Attribute[index : integer] : string read GetAttribute;
  public
    constructor Create; overload;
    constructor Create(aOwner, aParent : TObject); overload;
    constructor Create(aParent : TXMLElement; aName, aText : String; TokenType: TXMLReaderTokenType; aLine : int64); overload;
    destructor Destroy; override;

    procedure Assign(aElem : TXMLElement);
    procedure AddText(txt : String);
    procedure Clear;
    procedure ClearChildren;
    function  FirstChild : TXMLElement;
    function  NextSibling : TXMLElement;
    function  AddChildNode(aName : String):TXMLElement;
    procedure DeleteChildNode(aNode : TXMLElement);
    function  InsertNode(aNode : TXMLElement):TXMLElement;
    procedure SetValueByElement(aNodeName, aValue : String);
    function  NodeByAttributeValue(ANodeName, AAttributeName, AValue: string):TXMLElement;
    procedure RemoveChild(aNode : TXMLElement);
    function  SelectSingleNode(aNodeName : String):TXMLElement;
    function  SelectNodes(aName : String) : TList;
    procedure SwapElements(aFirst, aSecond : Integer);
    // CP added functions
    function  GetAttributeName(index : integer):string; inline;
    procedure AddAttrib(attr : String); inline;
    function  GetAttribute(AName: string): variant; overload;
    procedure RemoveAttribute(AName: string); overload;
    procedure RemoveAttribute(index: integer); overload;
    function  GetAttributeStr(AName: string): string;
    function  GetAttributeBool(AName: string): boolean;
    //procedure GetAttributeHex(AName: string; var aArr: TArray<Byte>);
    function  GetAttributeInt(AName: string): Int64;
    procedure SetAttribute(AName: string; AValue: Variant);
    procedure SetAttributeBool(AName: string; AValue: Boolean);
    //procedure SetAttributeHex(AName: string; aArr : TArray<Byte>);
    function  GetNode(aNodeName : String; bAutoCreate : Boolean = True):TXMLElement;
    function  GetValueByElement(ANodeName: string; var AValue: string): boolean; overload;
    function  GetValueByElement(ANodeName: string): string; overload;
    function  GetElementByValue(AElementName, AValue: string): TXMLElement;
    function  GetNodeByElementValue(ANodeName, AElementName, AValue: string; bAutoCreate : Boolean = True): TXMLElement;
    function  GetNodeByAttributeValue(ANodeName, AAttributeName, AValue: string; bAutoCreate : Boolean = True): TXMLElement;
    // compare node structures
    function  Compare(AElement: TXMLElement): boolean;

    property Level : integer read GetLevel;
    ///
    property Line : int64 read GetLine write SetLine;
    // TagName, NodeName : same difference
    ///
    property TagName : String read FTagName write SetTagName;
    property NodeName : String read FTagName write SetTagName;
    property Text : String read GetText write SetText;
    property NbElements : integer read GetNbElements;
    property NbAttributes : integer read GetNbAttributes;
    property Elements[index : integer] : TXMLElement read GetElement;
    property Index:Integer read GetIndex;
    // tweek to offer MSXML compatibility
    //property ChildNodes : TList read GetElements;
    property Attribs : String read GetAttribs;
    property AttributeValue[index : integer] : String read GetAttributeValue;
    property AttributeName[index : integer] : string read GetAttributeName;
    property ShortAttributeValue[index : integer] : string read GetShortAttributeValue;
    property Attributes : TStringList read FAttributes;
    property Path : String read GetNodePath;
    property Parent : TXMLElement read GetParent;
    // tweek to offer MSXML compatibility
    property nodeTypedValue : String read GetElemValue write SetElementValue;
    property TokenType: TXMLReaderTokenType read FTokenType;
    property Owner : TXMLDoc read FOwner;
  end;

  TNodeChangeEvent  = procedure (aNode : TXMLElement) of object;
  TNodeChildEvent   = procedure (aNode, aChildNode : TXMLElement) of object;

  TXMLDoc = Class(TObject)
  private
    FElement : TXMLElement;
    //XMLReader : TXMLReader;
    //XMLWriter : TXMLWriter;
    FFilename : String;
    FBeforeNodeChange : TNodeChangeEvent;
    FAfterNodeChange  : TNodeChangeEvent;
    FOnAddChildNode   : TNodeChildEvent;
    FOnDeleteNode     : TNodeChildEvent;
    FomitXMLDeclaration : Boolean;
    FModified : Boolean;
    procedure LoadData(XMLReader : TXMLReader);
    function  GetReader:TXMLReader;
    function  GetWriter:TXMLWriter;
  protected
    function  GetDocumentElement:TXMLElement;
    function  GetAsString:String;
    procedure SetAsString(aString : String);
    function  GetElement:TXMLElement;
    function  GetomitXMLDeclaration:Boolean;
    procedure SetomitXMLDeclaration(aVal : Boolean);
    function  GetFilename:String;
    procedure SetModified(Sender : TObject);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Clear;

    function  CreateNewDocumentElement(aName : String):TXMLElement;
    procedure MoveElement(aElement, aNewParent : TXMLElement);
    procedure loadXML(aXMLStr : String);
    procedure LoadFromFile(afilename : String);
    procedure SaveToFile(afilename : String);
    procedure SaveToStream(aStream : TStream; {bWriteBom : Boolean = True; }aOmitXMLDeclaration : boolean = False);
    procedure LoadFromStream(aStream : TStream);
    function  GetNodeFromPath(aPath : String):TXMLElement;

    property  DocumentElement : TXMLElement read GetDocumentElement;
    property  Element : TXMLElement read GetElement;
    property  Filename : String read GetFilename;
    property  AsString : String read GetAsString write SetAsString;
    property  XML : String read GetAsString;
    property  omitXMLDeclaration : Boolean read GetomitXMLDeclaration write SetomitXMLDeclaration;
    // Callback events
    property  BeforeNodeChange : TNodeChangeEvent read FBeforeNodeChange write FBeforeNodeChange;
    property  AfterNodeChange  : TNodeChangeEvent read FAfterNodeChange write FAfterNodeChange;
    property  OnAddChildNode   : TNodeChildEvent read FOnAddChildNode write FOnAddChildNode;
    property  OnDeleteChildNode : TNodeChildEvent read FOnDeleteNode write FOnDeleteNode;
    property  Modified : Boolean Read FModified;
  end;
  {
  // tweek to offer MSXML compatibility
  TListEx = Class(TListExIntf)
  private
    function Getcount : integer;
    function GetElement(index : integer):TXMLElement;
  public
    property length : Integer read Getcount;
    property Item[index : integer] : TXMLElement read GetElement;
  End;

  TXMLElementList = Class(TListEx);
  IXMLDOMNodeList = TXMLElementList;
  }

implementation

uses OTextReadWrite, VariantUtils;

{
function TListEx.Getcount : Integer;
begin
  result := Count;
End;

function TListEx.GetElement(index : integer):TXMLElement;
begin
  result := Items[index];
end;
}

constructor TXMLElement.Create;
begin
  if FOwner = nil then
    Raise Exception.Create('Owner not initialized !');

  inherited Create;
end;

constructor TXMLElement.Create(aOwner, aParent : TObject);
begin
  FOwner := TXMLDoc(aOwner);
  FParent := TXMLElement(aParent);
  Create;
  FLine := 0;
  FElements := TList.Create;
  FAttributes := TStringList.Create;
  FAttributes.OnChange := Owner.SetModified;
  //FText := TStringList.Create;
  FText := '';
  FTokenType := rtOpenElement;
end;

constructor TXMLElement.Create(aParent : TXMLElement; aName, aText : String; TokenType: TXMLReaderTokenType; aLine : int64);
begin
  Create(aParent.FOwner, aParent);
  FTagName := aName;
  FText := aText;
  FTokenType := TokenType;
  FLine := aLine;
end;

destructor TXMLElement.Destroy;
begin
  Clear;
  FElements.Free;
  FAttributes.Free;
  //FText.Free;
  inherited Destroy;
end;

procedure TXMLElement.Assign(aElem : TXMLElement);
var
  i : Integer;
  Elem : TXMLElement;
begin
  // assign child elements
  for i := 0 to aElem.NbElements - 1 do
  begin
    Elem := AddChildNode(TXMlelement(aElem.FElements[i]).TagName);
    Elem.Assign(aElem.FElements[i]);
  end;

  // assign attribs
  FAttributes.Text := aElem.Attributes.Text;
  FText := aElem.Text;
  FTokenType := aElem.TokenType;
end;

procedure TXMLElement.Clear;
begin
  ClearChildren;
  FElements.Clear;
  FAttributes.Clear;
  FText := '';
end;

procedure TXMLElement.ClearChildren;
var
  i : integer;
begin
  for i:=0 to NbElements-1 do
    TXMlelement(FElements[i]).Free;
end;

function TXMLElement.FirstChild : TXMLElement;
begin
  result := nil;
  if NbElements > 0 then
    result := FElements[0];
end;

function TXMLElement.Locate(aElem : TXMLElement) : Integer;
var
  i : Integer;
begin
  result := -1;
  for i := 0 to NbElements - 1 do
    if FElements[i] = aElem then
    begin
      result := i;
      exit;
    end;
end;

function TXMLElement.GetParent:TXMLElement;
begin
  result := FParent;
end;

procedure TXMLElement.MoveTo(aNewParent : TXMLElement);
var
  i : Integer;
begin
  with Parent do
    for i := 0 to FElements.count - 1 do
      if FElements[i] = Self then
      begin
        FElements.Delete(i);
        Break;
      end;

  FParent := aNewParent;
  aNewParent.FElements.Add(Self);
end;

function TXMLElement.NextSibling : TXMLElement;
var
  i : integer;
begin
  result := nil;
  i := TXMLElement(Parent).Locate(Self);
  if (i>=0) and (i < Parent.NbElements-1) then
    result := Parent.FElements[i+1];
end;

function TXMLElement.AddChildNode(aName : String):TXMLElement;
// aTokenType : TXMLReaderTokenType = rtOpenElement):TXMLElement;
begin
  Felements.Add(TXMLElement.Create(FOwner, Self));
  result := FElements[Felements.count-1];
  with TXMLElement(result) do
  begin
    FTagName := aName;
    FTokenType := rtOpenElement; //aTokenType;
  end;
  DoOnAddChildNode(result);
  Owner.SetModified(Self);
end;

procedure TXMLElement.DeleteChildNode(aNode : TXMLElement);
var
  i : Integer;
begin
  for I := 0 to NbElements-1 do
    if FElements[i] = aNode then
    begin
      if Assigned(Owner.OnDeleteChildNode) then
        Owner.OnDeleteChildNode(aNode, FElements[i]);

      FElements.Delete(i);
      aNode.Free;
      Owner.SetModified(Self);
      exit;
    end;
end;

function TXMLElement.InsertNode(aNode : TXMLElement):TXMLElement;
begin
  result := AddChildNode(aNode.TagName);
  result.Assign(aNode)
end;

function TXMLElement.SelectSingleNode(aNodeName : String):TXMLElement;
var
  i : integer;
begin
  result := nil;

  for i := 0 to NbElements - 1 do
    if TXMlelement(FElements[i]).TagName = aNodeName then
    begin
      result := FElements[i];
      exit;
    end;
end;

procedure TXMLElement.RemoveChild(aNode : TXMLElement);
var
  i : integer;
begin
  for i := 0 to FElements.Count - 1 do
    if FElements[i] = aNode then
    begin
      FElements.Delete(i);
      aNode.Free;
      exit;
    end;
end;

function TXMLElement.SelectNodes(aName : String) : TList;
var
  i : Integer;
begin
  result := TList.Create;
  with result do
    for I := 0 to NbElements - 1 do
      if TXMlelement(FElements[i]).TagName = aName then
        Add(FElements[i]);
end;

procedure TXMLElement.SwapElements(aFirst, aSecond: Integer);
var
  Node : TObject;
begin
  Node := FElements[aFirst];
  FElements[aFirst] := FElements[aSecond];
  FElements[aSecond] := Node;
  Owner.SetModified(Self);
end;

function TXMLElement.GetAttribute(AName: string): variant;
var
  s : String;
begin
  result := Null;

  if AName = '' then exit;

  result := FAttributes.Values[AName];
  if result <> '' then
  begin
    s := FAttributes.Values[AName];
    // check for dates
    if (Length(s) = 23) then
    begin
      if (s[5]='-') and (s[11]='T') then
      begin
        s := StringReplace(s, '-', '/', [rfReplaceAll]);
        s[11] := ' ';
        s :=
          copy(s, 9, 2)+'/'+copy(s, 6, 2)+'/'+copy(s, 1, 4) +
          copy(s, 11, length(s));;

        try
          result := StrToDateTime(s);
        except
          result := 0;
        end;
      end
      else
        result := FAttributes.Values[AName];
    end
    else
      result := FAttributes.Values[AName];
  end;
end;

procedure TXMLElement.RemoveAttribute(index: integer);
begin
  with FAttributes do
    if index < Count then
      Delete(index);
end;

procedure TXMLElement.RemoveAttribute(AName: string);
var
  i : Integer;
begin
  with FAttributes do
    for i := 0 to Count - 1 do
      if Strings[i] = aName then
      begin
        Delete(i);
        Exit;
      end;

end;

function TXMLElement.GetAttributeStr(AName: string): string;
begin
  result := '';
  result := FAttributes.Values[AName];
end;

function TXMLElement.GetAttributeBool(AName: string): boolean;
var
  a: string;
begin
  result := false;
  a := FAttributes.Values[AName];
  if a <> '' then
    result := (a = '1') or (a = 'true');
end;

{
procedure TXMLElement.GetAttributeHex(AName: string; var aArr: TArray<Byte>);
const
  Convert: array['0'..'f'] of SmallInt =
    ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15);
var
  I,
  j, o : Integer;
  aHex : string;
begin
  aHex := GetAttribute(aNAme);
  I := Length(aHex);
  j := 1;
  o := 0;
  SetLength(aArr, length(aHex) div 2);
  while I > 0 do
  begin
    if (CharInSet(aHex[j], [':'..'@']) or CharInSet(aHex[j], ['G'..#96])) or
       (CharInSet(aHex[j+1], [':'..'@']) or CharInSet(aHex[j+1], ['G'..#96])) then
       Break;
    if not CharInSet(aHex[j], ['0'..'f']) or not CharInSet(aHex[j+1], ['0'..'f']) then Break;
    aArr[O] := byte((Convert[AnsiChar(aHex[j])] shl 4) + Convert[AnsiChar(aHex[j+1])]);
    Inc(O);
    Inc(j, 2);
    Dec(I, 2);
  end;
end;

procedure TXMLElement.SetAttributeHex(AName: string; aArr : TArray<Byte>);
const
  Convert: array[0..15] of WideChar = '0123456789ABCDEF';
var
  I: Integer;
  s : string;
begin
  s := '';
  for I := Low(aArr) to High(aArr) do
  begin
    s := s + Convert[Byte(aArr[I]) shr 4];
    s := s + Convert[Byte(aArr[I]) and $F];
  end;
  SetAttribute(AName, s);
end;
}

function TXMLElement.GetAttributeInt(AName: string): Int64;
begin
  result := VarToIntDef(FAttributes.Values[AName], 0);
end;

procedure TXMLElement.SetAttributeBool(AName: string; AValue: Boolean);
begin
  if AValue then SetAttribute(AName, 'true')
            else SetAttribute(AName, 'false');
end;

function TXMLElement.GetNode(aNodeName : String; bAutoCreate : Boolean = True):TXMLElement;
begin
  result := selectSingleNode(ANodeName);
  if (result = nil) and (bAutoCreate) then
    result := AddChildNode(ANodeName);
end;

function TXMLElement.GetValueByElement(ANodeName: string; var AValue: string): boolean;
var
  n: TXMLElement;
begin
  n := selectSingleNode(ANodeName);
  result := n <> nil;
  if result then AValue := n.text;
end;

function TXMLElement.GetValueByElement(ANodeName: string): string;
begin
  result := '';
  GetValueByElement(ANodeName, result);
end;

function TXMLElement.GetElementByValue(AElementName, AValue: string): TXMLElement;
var
  i: integer;
begin
  result := nil;

  for i := 0 to NbElements -1 do
    if TXMLElement(FElements[i]).Text = AValue then
    begin
      result := FElements[i];
      break;
    end;

  if result = nil then
  begin
    result := AddChildNode(AElementName);
    result.text := AValue;
  end;
end;

function TXMLElement.GetNodeByElementValue(ANodeName, AElementName, AValue: string; bAutoCreate : Boolean = True): TXMLElement;
var
  i: integer;
  n: TXMLElement;
begin
  result := nil;

  for i := 0 to NbElements -1 do
  begin
    n := FElements[i];
    if SameText(n.GetValueByElement(AElementName), AValue) then
    begin
      result := n;
      break;
    end;
  end;
  if (result = nil) and bAutoCreate then
  begin
    result := AddChildNode(ANodeName);
    n := result.GetNode(AElementName);
    n.Text := AValue;
  end;
end;

function TXMLElement.GetNodeByAttributeValue(ANodeName, AAttributeName, AValue: string; bAutoCreate : Boolean = True): TXMLElement;
var
  i: integer;
  n: TXMLElement;
begin
  result := nil;

  for i := 0 to NbElements -1 do
  begin
    n := FElements[i];
    if SameText(n.GetAttribute(AAttributeName), AValue) then
    begin
      result := n;
      break;
    end;
  end;
  if (result = nil) and bAutoCreate then
  begin
    result := AddChildNode(ANodeName);
    result.SetAttribute(AAttributeName, AValue);
  end;
end;

function TXMLElement.Compare(AElement : TXMLElement): boolean;
var
  i, j : integer;
  Match: Boolean;
begin
  Result := True;
  Match := False;
  
  if (AElement = nil) OR
     (TagName <> AElement.TagName) OR
     (Attribs <> AElement.Attribs) OR
     (NbElements <> AElement.NbElements) then
  begin
    Result := False;
    exit;
  end;

  for i := 0 to NbElements - 1 do
  begin
    for j := 0 to AElement.NbElements - 1 do
    begin
      Match := False;
      if (TXMLElement(FElements[i]).TagName = TXMLElement(AElement.FElements[j]).TagName) AND
         (TXMLElement(FElements[i]).Attribs = TXMLElement(AElement.FElements[j]).Attribs) then
      begin
        Match := True;
        Result := TXMLElement(FElements[i]).Compare(AElement.FElements[j]);
        if not Result then
          exit
        else
          break;
      end;
    end;
    if not Match then
    begin
      Result := False;
      exit;
    end;
  end;
end;

procedure TXMLElement.SetAttribute(AName: string; AValue: Variant);
var
  s : string;
begin
  if not VarIsNull(AValue) then
    if VarType(AValue) = VarDate then
    begin
      s := FormatDateTime('yyyy-mm-dd', TDateTime(AValue)) + 'T' +
           FormatDateTime('hh:nn:ss.zzz', TDateTime(AValue));

      FAttributes.Values[aName] := s;
    end
    else
      FAttributes.Values[aName] := AValue;
end;


procedure TXMLElement.SetValueByElement(aNodeName, aValue : String);
var
  i : integer;
begin
  for i := 0 to NbElements - 1 do
    if TXMLElement(FElements[i]).TagName = anodeName then
    begin
      TXMLElement(FElements[i]).Text := aValue;
      Exit;
    end;

  AddChildNode(aNodeName).Text := aValue;
end;

function TXMLElement.NodeByAttributeValue(ANodeName, AAttributeName,
  AValue: string): TXMLElement;
var
  i,j : integer;
begin
  for i := 0 to NbElements - 1 do
    for j := 0 to TXMLElement(FElements[i]).Attributes.Count - 1 do
      if TXMLElement(FElements[i]).Attributes.Values[AAttributeName] = AValue then
      begin
        result := FElements[i];
        exit;
      end;
  // not found : create
  result := AddChildNode(aNodeName);
  TXMLElement(result).Attributes.Values[aAttributeName] := aValue;
end;

procedure TXMLElement.Save(Writer : TXMLWriter; omitXMLDeclaration : Boolean = False);
var
  i : integer;
  s : string;

  procedure TabIt;
  var
    ii : integer;
  begin
    for ii:=1 to Level-1 do
      Writer.RawText(#9);
  end;

begin
  // tabs to nicely format the xml but not
  // for xml fragment
  if not omitXMLDeclaration then
    TabIt;

  // special case for the XML document declaration
  if ((Level = 0) and (TagName = 'xml')) and
     (not omitXMLDeclaration) then
  begin
    Writer.OpenXMLDeclaration;
    FAttributes.Values['encoding'] := Writer.Encoding.EncodingName;

    for i := 0 to NbAttributes-1 do
      Writer.Attribute(FAttributes.Names[i], FAttributes.ValueFromIndex[i]);

    Writer.FinishOpenXMLDeclaration;
  end
  // child elements
  else
    case FTokenType of
      rtCData :   //cdata: <![CDATA[value]]>
        Writer.CData(Text);

      rtComment: //comment: <!--value-->
        Writer.Comment(Text);

      rtProcessingInstruction: //custom processing instruction: <?target content?>
        Writer.ProcessingInstruction(TagName, Text);

      rtDocType :       //docty
        Writer.DocType(Text);

      rtOpenElement :
        begin
          Writer.OpenElement(TagName);

          if NbAttributes > 0 then
            for i := 0 to NbAttributes-1 do
              Writer.Attribute(FAttributes.Names[i], FAttributes.ValueFromIndex[i]);

          if (NbElements > 0) or (Text <> '') then
            Writer.FinishOpenElement(TagName)
          else
            Writer.FinishOpenElementClose(TagName);

          if Text <> '' then
          begin
            s := Text;
            //Writer.RawText(s);
            Writer.Text(s, false);
            // format XML only if not XML Fragment
            if not omitXMLDeclaration then
              Writer.RawText(#13#10);
            Writer.CloseElement(TagName, True);
          end;
        end;
    end;

  // format XML only if not XML Fragment
  if not omitXMLDeclaration then
    Writer.RawText(#13#10);

  // process chid elements
  for i := 0 to NbElements -1 do
    TXMLElement(FElements[i]).Save(Writer, omitXMLDeclaration);

  // close element except for top level XML declaration
  if (NbElements > 0) and (Level > 0)then
  begin
    // format XML only if not XML Fragment
    if not omitXMLDeclaration then
      TabIt;

    Writer.CloseElement(TagName, True);
    
    // format XML only if not XML Fragment
    if not omitXMLDeclaration then
      Writer.RawText(#13#10);
  end;
end;

procedure TXMLElement.StartLoad(Token: PXMLReaderToken; Reader : TXMLReader);
begin
  FTagName := Token^.TokenName;
  Load(Token, Reader);
end;

procedure TXMLElement.Load(var  Token: PXMLReaderToken; Reader : TXMLReader);
begin
  repeat
    case Token^.TokenType of
      rtDocumentStart : ;//start of reading
      rtXMLDeclarationAttribute :
         FAttributes.Add(Token^.TokenName+'='+Token^.TokenValue) ;//attribute in an xml declaration: name="value"

      rtCData :                                      //cdata: <![CDATA[value]]>
         Felements.Add(TXMLElement.Create(Self, XML_CDATA, Token^.TokenValue, Token^.TokenType, Reader.Line));

      rtComment :               //comment: <!--value-->
         Felements.Add(TXMLElement.Create(Self, XML_COMMENT, Token^.TokenValue, Token^.TokenType, Reader.Line));

      rtProcessingInstruction : //custom processing instruction: <?target content?>
         Felements.Add(TXMLElement.Create(Self, Token^.TokenName, Token^.TokenValue, Token^.TokenType, Reader.Line));

      rtDocType :                                       //docty
         Felements.Add(TXMLElement.Create(Self, XML_DOCTYPE, Token^.TokenValue, Token^.TokenType, Reader.Line));

      rtOpenElement :                                  //open element: <name
         begin
           Felements.Add(TXMLElement.Create(FOwner, Self));
           with TXMLElement(FElements[Felements.count-1]) do
           begin
             FTagName := Token^.TokenName;
             FTokenType := Token^.TokenType;
             Reader.ReadNextToken(Token);
             FLine := Reader.Line;
             Load(Token, Reader);
           end;
         end;

      rtAttribute :
           FAttributes.Add(Token^.TokenName+'='+Token^.TokenValue);  //attribute: name="value"

      rtFinishOpenElement :      ;//open element finished but not closed: <node ... ">"
      rtFinishOpenElementClose :
           exit;                //open element finished and closed: <node ... "/>"

      rtFinishXMLDeclarationClose,
      rtCloseElement :
          if Token^.TokenName = FTagName then exit;      //close element: "</node>"

      rtText :
       begin
          AddText(Token^.TokenValue);
       end;

      rtEntityReference :  ;//&name; (value = the dereferenced entity value)
    end;

  until not Reader.ReadNextToken(Token);

end;

procedure TXMLElement.AddText(txt : String);
begin
  FText := StringReplace(txt, #13#13#10, #13#10, [rfReplaceAll]);
end;

procedure TXMLElement.SetTagName(aName : String);
begin
  if FTagName <> aName then
  begin
    FTagName := aName;
    Owner.SetModified(Self);
  end;
end;

function TXMLElement.GetIndex: Integer;
var
  i : integer;
begin
  result := -1;
  if Assigned(Parent) then
    for i:=0 to Parent.NbElements-1 do
      if Parent.Elements[i] = Self then
      begin
        result := i;
        break;
      end;
end;

function TXMLElement.GetElement(index : integer):TXMLElement;
begin
  result := TXMLElement(FElements[index]);
end;

function TXMLElement.GetAttribute(index : integer):string;
begin
  result := FAttributes[index];
end;

function TXMLElement.GetAttributeValue(index : integer):string;
begin
  result := GetAttribute(AttributeName[index]);
end;

function TXMLElement.GetShortAttributeValue(index : integer):string;
  function FormatNumber(aNum : integer):string;
  begin
    if aNum > 10240000 then
      result := IntToStr(aNum div 10240)+' mb'
    else
    if aNum > 102400 then
      result := IntToStr(aNum div 1024)+' kb'
    else
      result := IntToStr(aNum)+' b';

    result := '(' + Result + ')';
  end;
begin
  result := GetAttributeValue(index);
  result := Copy(result, 1, 10) + '... ' + FormatNumber(Length(result));
end;

function TXMLElement.GetAttributeName(index : integer):string;
begin
  if index < FAttributes.Count then
    result := FAttributes.Names[index];
end;

function TXMLElement.GetNbElements:integer;
begin
  result := FElements.Count;
end;

function TXMLElement.GetNbAttributes:integer;
begin
  result := FAttributes.Count;
end;

function TXMLElement.GetAttribList:String;
var
  i : integer;
begin
  result := '';
  for i:=0 to FAttributes.Count-1 do
    result := result + FAttributes.ValueFromIndex[i] + ';';
  Setlength(result, length(result)-1);
end;

function TXMLElement.GetAttribs:String;
var
  i : integer;
begin
  result := '';
  if NbAttributes > 0 then
  begin
    for i:=0 to NbAttributes-1 do
      if Length(Attribute[i]) > 50 then
        result := result + AttributeName[i]+'='+ShortAttributeValue[i] + ' '
      else
        result := result + Attribute[i] + ' ';
    Setlength(result, length(result)-1);
  end;
end;

function  TXMLElement.GetLevel:Integer;
begin
  result := 0;
  if assigned(self) and Assigned(Self.parent) then
    result := parent.level + 1;
end;

function TXMLElement.GetText:String;
begin
  result := Trim(FText);
end;

procedure TXMLElement.SetText(aText : String);
begin
  DoBeforeNodeChange;
  if FText <> aText then
  begin
    FText := aText;
    Owner.SetModified(Self);
  end;
  DoAfterNodeChange;
end;

function TXMLElement.GetNodePath:String;
var
  n : TXMLElement;
begin
  result := '';
  n := Self;

  repeat
    result := '/'+ n.TagName + result;
    n := TXMLElement(n.Parent);
  until n.level = 0;

end;

function TXMLElement.GetLine:Int64;
begin
  result := FLine;
end;

procedure TXMLElement.SetLine(aLine : int64);
begin
  FLine := aLine;
end;

function TXMLElement.GetElements:TList;
begin
  result := FElements;
end;
{
function TXMLElement.GetValue(aName : String):String;
var
  i : integer;
begin
  result := '';
  if Name = aName then
  begin
    if Text <> '' then;
      result := Text;
  end
  else
    for i:=0 to NbElements-1 do
    begin
      result := TXMLElement(FElements[i]).GetValue(aName);
      if result <> '' then exit;
    end;
end;
}

procedure TXMLElement.DoBeforeNodeChange;
begin
  if Assigned(FOwner.BeforeNodeChange) then
    FOwner.BeforeNodeChange(Self);
end;

procedure TXMLElement.DoAfterNodeChange;
begin
  if Assigned(FOwner.AfterNodeChange) then
    FOwner.AfterNodeChange(Self);
end;

procedure TXMLElement.DoOnAddChildNode(aChildNode : TXMLElement);
begin
  if Assigned(FOwner.OnAddChildNode) then
    TXMLElement(Self).FOwner.OnAddChildNode(Self, aChildNode);
end;

function TXMLElement.GetElemValue:String;
begin
  result := Trim(Text);
end;

procedure TXMLElement.SetElementValue(aValue : String);
begin
  Text := aValue;
end;
{
procedure TXMLElement.SetValue(aName, Avalue : String);
var
  i : integer;
begin
  if Name = aName then
    Text := aValue
  else
    for i:=0 to NbElements-1 do
      TXMLElement(FElements[i]).SetValue(aName, aValue);
end;
}

procedure TXMLElement.AddAttrib(attr : String);
begin
  FAttributes.Add(attr);
end;




//
//  TXMLDOC
//



constructor TXMLDoc.Create;
begin
  inherited Create;

  FomitXMLDeclaration := False;
  FElement := TXMLElement.Create(Self, nil);
  FElement.FTagName := 'xml';
  FElement.AddAttrib('version=1.0');
  FModified := False;
end;

destructor TXMLDoc.Destroy;
begin
  FElement.Free;
  inherited Destroy;
end;

procedure TXMLDoc.Clear;
begin
  FElement.Clear;
end;

function TXMLDoc.CreateNewDocumentElement(aName : String):TXMLElement;
begin
  if FElement.NbElements > 0 then
    FElement.ClearChildren;

  Result := FElement.AddChildNode(aName)
end;

function TXMLDoc.GetDocumentElement:TXMLElement;
begin
  result := nil;
  if FElement.NbElements > 0 then
    result := TXMLElement(FElement.FElements[0]);
end;

procedure TXMLDoc.MoveElement(aElement, aNewParent : TXMLElement);
begin
  aElement.MoveTo(aNewParent);
  SetModified(Self);
end;

procedure TXMLDoc.LoadData(XMLReader : TXMLReader);
var
  Token: PXMLReaderToken;
begin
  Token:=nil;
  with XMLReader do
  begin
    ReadNextToken(Token);

    if not Assigned(Token) then
      raise Exception.Create('Token not found, invalid XML file !');

    repeat
      case Token^.TokenType of
        rtOpenXMLDeclaration:
          FElement.StartLoad(Token, XMLReader);

        rtXMLDeclarationAttribute:
           FElement.AddAttrib(Token^.TokenName+'='+Token^.TokenValue);

        rtText :
          FElement.AddTExt(Token^.TokenValue);

        rtOpenElement:
          FElement.Load(Token, XMLReader);
      end;
    until not ReadNextToken(Token);
  end;
end;

procedure TXMLDoc.LoadFromFile(afilename : String);
var
  x : TXMLReader;
begin
  FFilename := aFilename;
  x := GetReader;
  with x do
  try
    InitFile(FFilename);
    Clear;
    LoadData(x);
    FModified := False;
  finally
    Free;
  end;
end;

procedure TXMLDoc.loadXML(aXMLStr : String);
begin
  AsString := aXMLStr;
  FModified := False;
end;

procedure TXMLDoc.SaveToStream(aStream : TStream; {bWriteBom : Boolean = True; }aOmitXMLDeclaration : boolean = False);
var
  x : TXMLWriter;
begin
  x := GetWriter;
  //x.WriteBOM := bWriteBom;
  with x do
  try
    InitStream(aStream);
    DefaultIndentLevel := 4;

    if aOmitXMLDeclaration then
      TXMLElement(DocumentElement).Save(x, aOmitXMLDeclaration)
    else
      FElement.Save(x, aOmitXMLDeclaration);

    FModified := False;
  finally
    Free;
  end;
end;

procedure TXMLDoc.SaveToFile(afilename : String);
var
  x : TXMLWriter;
begin
  x := GetWriter;
  with x do
  try
    InitFile(aFilename);
    DefaultIndentLevel := 4;

    if FomitXMLDeclaration then
      TXMLElement(DocumentElement).Save(x)
    else
      FElement.Save(x, FomitXMLDeclaration);

    FFilename := aFilename;
    FModified := False;
  finally
    Free;
  end;
end;


procedure TXMLDoc.LoadFromStream(aStream : TStream);
var
  x : TXMLReader;
begin
  x := GetReader;
  with x do
  try
    InitStream(aStream);
    Clear;
    LoadData(x);
    FModified := False;
  finally
    Free;
  end;
end;

function TXMLDoc.GetNodeFromPath(aPath : String):TXMLElement;
var
  List: TStrings;
  i : integer;
begin
  result := DocumentElement;
  if aPath[1] <> '/' then
    raise Exception.Create('Invalid node path format !');
    
  List := TStringList.Create;
  try
    ExtractStrings(['/'], [], PChar(aPath), List);
    // special case for 1st node
    if Result = nil then
      Result := CreateNewDocumentElement(List[0]);

    with List do
      if Count > 0 then
        for i := 1 to Count - 1 do
          Result := Result.GetNode(Strings[i]);
  finally
    List.Free;
  end;
end;

function TXMLDoc.GetAsString:String;
var
  ss: TStringStream;
begin
  Result := '';
  ss := TStringStream.Create('');
  with ss do
  try
    Self.SaveToStream(ss, {False,} True);
    position := 0;
    Result := SS.DataString;
  finally
    free;
  end;
end;

procedure TXMLDoc.SetAsString(aString : String);
var
  x : TXMLReader;
begin
  x := GetReader;
  with x do
  try
    InitXML(aString);
    Clear;
    LoadData(x);
    FModified := False;
  finally
    Free;
  end;
end;

function TXMLDoc.GetElement:TXMLElement;
begin
  result := Felement;
end;

function  TXMLDoc.GetomitXMLDeclaration:Boolean;
begin
  result := FomitXMLDeclaration;
end;

procedure TXMLDoc.SetomitXMLDeclaration(aVal : Boolean);
begin
  FomitXMLDeclaration := aVal;
end;

function TXMLDoc.GetFilename:String;
begin
  result := FFilename;
end;

procedure TXMLDoc.SetModified(Sender : TObject);
begin
  FModified := True;
end;

function TXMLDoc.GetReader:TXMLReader;
begin
  result := TXMLReader.Create;
  with result.ReaderSettings do
  begin
    ErrorHandling := ehRaise;
    StrictXML := True;
  end;
end;

function TXMLDoc.GetWriter:TXMLWriter;
begin
  result := TXMLWriter.Create;
  with result do
  begin
    OwnsEncoding := False;
    //Encoding := TEncoding.Unicode;
    WriterSettings.WriteBOM :=  True;
  end;
end;



end.

