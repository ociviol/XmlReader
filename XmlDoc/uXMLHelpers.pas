unit uXMLHelpers;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
interface

uses
  SysUtils, Variants, Classes, ComCtrls,
  Controls, Grids,
  uxmldoc;
  //MSAccess, dxmdaset;

const
  CS_DATASETS = 'DataSets';


type
  TXMLProgress = procedure(aMin, aMax, aPosition : Integer) of object;
  {
  TXMLQUery = CLass helper for TMSQuery
  public
    procedure SaveToXml(aXMLDoc : TXMLDoc; OnProgress : TXMLProgress = nil);
  End;

  TXMLDataSet = class helper for TdxMemData
  public
    procedure SaveToXml(aXMLDoc : TXMLDoc; OnProgress : TXMLProgress = nil);
    procedure LoadFromXml(aNode : TXmlElement; OnProgress : TXMLProgress = nil);
  end;
  }
  TTreeView = Class(ComCtrls.TTreeView)
  private
    function GetSelectedXML:TXMLElement; inline;
  protected
  public
    procedure Init(zNode : TXmlElement; OnProgress : TXMLProgress = nil);
    procedure AddTreeNodes(Elem: TXmlElement; ANode: TTreeNode; OnProgress : TXMLProgress = nil);
    property  SelectedXML : TXMLElement read GetSelectedXML;
  End;

  TStringGrid = class(Grids.TStringGrid)
  private
    FNode : TXMLElement;
  protected
    procedure Resize; override;
  public
    procedure Init(aNode : TXmlElement; OnProgress : TXMLProgress = nil);
    property Node : TXMLElement read FNode;
  end;

implementation

uses
  Forms, Dialogs;

{
type
  DataTypeInfo = record
    Name : string;
    DataType : TFieldType;
  end;

const
  DataTypes : array[0..49] of DataTypeInfo =
        (
          (Name:'ftUnknown'; DAtaType:ftUnknown),
          (Name:'ftString'; DAtaType:ftString),
          (Name:'ftSmallint'; DAtaType:ftSmallint),
          (Name:'ftInteger'; DAtaType:ftInteger),
          (Name:'ftWord'; DAtaType:ftWord),
          (Name:'ftBoolean'; DAtaType:ftBoolean),
          (Name:'ftFloat'; DAtaType:ftFloat),
          (Name:'ftCurrency'; DAtaType:ftCurrency),
          (Name:'ftBCD'; DAtaType:ftBCD),
          (Name:'ftDate'; DAtaType:ftDate),
          (Name:'ftTime'; DAtaType:ftTime),
          (Name:'ftDateTime'; DAtaType:ftDateTime),
          (Name:'ftBytes'; DAtaType:ftBytes),
          (Name:'ftVarBytes'; DAtaType:ftVarBytes),
          (Name:'ftAutoInc'; DAtaType:ftAutoInc),
          (Name:'ftBlob'; DAtaType:ftBlob),
          (Name:'ftMemo'; DAtaType:ftMemo),
          (Name:'ftGraphic'; DAtaType:ftGraphic),
          (Name:'ftFmtMemo'; DAtaType:ftFmtMemo),
          (Name:'ftParadoxOle'; DAtaType:ftParadoxOle),
          (Name:'ftDBaseOle'; DAtaType:ftDBaseOle),
          (Name:'ftTypedBinary'; DAtaType:ftTypedBinary),
          (Name:'ftCursor'; DAtaType:ftCursor),
          (Name:'ftFixedChar'; DAtaType:ftFixedChar),
          (Name:'ftWideString'; DAtaType:ftWideString),
          (Name:'ftLargeint'; DAtaType:ftLargeint),
          (Name:'ftADT'; DAtaType:ftADT),
          (Name:'ftArray'; DAtaType:ftArray),
          (Name:'ftReference'; DAtaType:ftReference),
          (Name:'ftDataSet'; DAtaType:ftDataSet),
          (Name:'ftOraBlob'; DAtaType:ftOraBlob),
          (Name:'ftOraClob'; DAtaType:ftOraClob),
          (Name:'ftVariant'; DAtaType:ftVariant),
          (Name:'ftInterface'; DAtaType:ftInterface),
          (Name:'ftIDispatch'; DAtaType:ftIDispatch),
          (Name:'ftGuid'; DAtaType:ftGuid),
          (Name:'ftTimeStamp'; DAtaType:ftTimeStamp),
          (Name:'ftFMTBcd'; DAtaType:ftFMTBcd),
          (Name:'ftFixedWideChar'; DAtaType:ftFixedWideChar),
          (Name:'ftWideMemo'; DAtaType:ftWideMemo),
          (Name:'ftOraTimeStamp'; DAtaType:ftOraTimeStamp),
          (Name:'ftOraInterval'; DAtaType:ftOraInterval),
          (Name:'ftLongWord'; DAtaType:ftLongWord),
          (Name:'ftShortint'; DAtaType:ftShortint),
          (Name:'ftByte'; DAtaType:ftByte),
          (Name:'ftConnection'; DAtaType:ftConnection),
          (Name:'ftParams'; DAtaType:ftParams),
          (Name:'ftStream'; DAtaType:ftStream),
          (Name:'ftTimeStampOffset'; DAtaType:ftTimeStampOffset),
          (Name:'ftObject'; DAtaType:ftObject)
         );


function StringToType(aType : String):TFieldType;
var
  z : integer;
begin
  result := ftString;

  for z := Low(DataTypes) to High(DataTypes) do
    if DataTypes[z].Name = aType then
    begin
      result := DataTypes[z].DataType;
      exit;
    end;
end;

function TypeToString(aType : TFieldType):string;
var
  z : integer;
begin
  for z := Low(DataTypes) to High(DataTypes) do
    if DataTypes[z].DataType = aType then
    begin
      result := DataTypes[z].Name;
      exit;
    end;
end;

procedure DatasetToXml(aDataSet : TDataSet; aNode : TXMLElement; OnProgress : TXMLProgress = nil);
var
  m, p, j : integer;
  bm : TArray<Byte>;
begin
  // save field names
  with aDataSet do
  begin
    DisableControls;
    try
      bm := GetBookmark;
      Last;
      m := RecordCount;
      First;

      // save fields
      with aNode.AddChildNode('Fields') do
        for j := 0 to FieldCount - 1 do
          with AddChildNode('Field') do
          begin
            SetAttribute('name', Fields[j].FieldName);
            SetAttribute('type', TypeToString(Fields[j].DataType));
            SetAttributeBool('isblob', Fields[j].IsBlob);
            SetAttribute('displayname', Fields[j].DisplayName);
            SetAttribute('editmask', Fields[j].EditMask);
            SetAttributeBool('isindexed', Fields[j].IsIndexField);
            SetAttribute('size', Fields[j].Size);
            SetAttribute('DisplayLabel', Fields[j].DisplayLabel);
            SetAttribute('DisplayWidth', Fields[j].DisplayWidth);

          end;

      // save data
      p := 0;
      with aNode.AddChildNode('Rows') do
        while not eof do
        begin
          with AddChildNode('Row') do
            for j := 0 to FieldCount - 1 do
              with Fields[j] do
                if (not IsNull) then
                  if IsBlob then
                    SetAttributeHex(FieldName, AsBytes)
                  else
                    SetAttribute(FieldName, AsVariant);

          Next;
          if Assigned(OnProgress) then
            OnProgress(0, m, p);
          Inc(p);
        end;
    finally
      if Assigned(OnProgress) then
        OnProgress(0, 0, 0);
      GotoBookmark(bm);
      EnableControls;
    end;
  end;
end;

procedure TXMLQUery.SaveToXml(aXMLDoc : TXMLDoc; OnProgress : TXMLProgress = nil);
var
  aNode : TXMLElement;
  s : string;
begin
  // save field names
  aNode := aXmlDoc.GetNodeFromPath('/'+CS_DATASETS);
  // get table name from select
  s := inputbox('Dataset Name', 'Choose a name the dataset', '_'+name);
  aNode := aNode.GetNode(s);
  DatasetToXml(self, aNode, OnProgress);
end;

procedure TXMLDataSet.SaveToXml(aXMLDoc : TXmlDoc; OnProgress : TXMLProgress = nil);
var
  aNode : TXMLElement;
begin
  aNode := aXmlDoc.GetNodeFromPath('/'+CS_DATASETS);
  aNode := aNode.GetNode(Name);
  DatasetToXml(self, aNode, OnProgress);
end;

procedure TXMLDataSet.LoadFromXml(aNode : TXmlElement; OnProgress : TXMLProgress = nil);
var
  i, j, m, p : integer;
  AField: TField;
  aArr : TArray<Byte>;
begin

  DisableControls;
  try
    Close;
    while Fields.Count > 0 do
      Fields[0].Free;

    name := aNode.TagName;
    FieldDefs.Clear;

    // create fields
    with aNode.GetNode('Fields') do
      for I := 0 to NbElements - 1 do
        with Elements[i] do
          with FieldDefs.AddFieldDef do
          begin
            Name := GetAttribute('name');
            DataType := StringToType(GetAttribute('type'));
            DisplayName := GetAttribute('displayname');
            Size := GetAttribute('size');

            with CreateField(Self) do
            begin
              Tag := Integer(GetAttributeBool('isblob'));
              DisplayLabel := GetAttribute('DisplayLabel');
              DisplayWidth := GetAttribute('DisplayWidth');
              EditMask := GetAttribute('EditMask');
            end;
          end;

    Open;

    // load data
    with aNode.GetNode('Rows') do
    begin
      p := 0;
      m := NbElements;
      for I := 0 to NbElements - 1 do
        with Elements[i] do
        begin
          Append;

          for J := 0 to Attributes.Count - 1 do
          begin
            AField := FieldByName(AttributeName[J]);
            // blobs
            if Boolean(AField.Tag) then
            begin
              GetattributeHex(AttributeName[J], aArr);
              AField.AsBytes := aArr;
            end
            else
              AField.AsVariant := GetAttribute(AttributeName[J]);
          end;

          Post;
          if Assigned(OnProgress) then
            OnProgress(0, m, p);
          inc(p);
        end;
    end;

  finally
    if Assigned(OnProgress) then
      OnProgress(0, 0, 0);
    First;
    EnableControls;
  end;

end;
}

function MakeNodeText(aElement: TXmlElement): String;
var
  theattribs: string;
begin
  // create node name
  result := aElement.TagName;
 // append attributes to name
  theattribs := aElement.Attribs;
  if theattribs <> '' then
    result := result + ' (' + theattribs + ')';
end;
  
procedure TTreeView.AddTreeNodes(Elem: TXmlElement; ANode: TTreeNode; OnProgress : TXMLProgress = nil);
var
  i: Integer;
  t: TTreeNode;
  s: String;
begin
  with Elem do
  begin
    for i := 0 to NbElements - 1 do
    begin
      // create node name
      s := MakeNodeText(Elements[i]);
      // add node
      t := Items.AddChildObject(ANode, s, Elements[i]);
      // add text if any
      if Elements[i].Text <> '' then
        Items.AddChildObject(t, Elements[i].Text, Elements[i]);
      // if childnodes then add them
      if Elements[i].NbElements > 0 then
        AddTreeNodes(Elements[i], t);
    end;
  end;
end;

function TTreeView.GetSelectedXML:TXMLElement;
begin
  result := TXMLElement(Selected.Data);
end;

procedure TTreeView.Init(zNode : TXmlElement; OnProgress : TXMLProgress = nil);
var
  attrib, ss: string;

  procedure ExpandTree(ANode: TTreeNode; aMaxLvl: Integer);
  var
    tn: TTreeNode;
  begin
    if ANode.Level > aMaxLvl then
      Exit;

    ANode.Expand(False);
    tn := ANode.GetFirstChild;
    if Assigned(tn) then
      repeat
        tn.Expand(False);
        if tn.HasChildren then
          ExpandTree(tn, aMaxLvl);
        tn := tn.GetNextChild(tn);
      until tn = nil;
  end;
 
begin
  if zNode.NbElements > 0 then
    With Items do
      try
        Screen.Cursor := crHourGlass;
        BeginUpdate;
        Clear;

        ss := zNode.TagName;
        attrib := zNode.Attribs;
        if attrib <> '' then
          ss := ss + ' (' + attrib + ')';

        AddTreeNodes(zNode, AddChildObject(nil, ss, zNode), OnProgress);
        // expand
        ExpandTree(Items[0], 1);
        Selected := Items[0].GetFirstChild;
        Selected.MakeVisible;
      finally
        EndUpdate;
        Screen.Cursor := crDefault;
      end;
end;

procedure TStringGrid.Resize;
var
  i, z : Integer;
begin
  inherited;
  
  if Colcount < 2 then Exit;
  // first col = 1/3
  ColWidths[0] := width div 3;
  // other cols
  z := ((Width - ColWidths[0]) - 20) div (ColCount - 1);
  for i := 1 to Colcount - 1  do
  begin
    ColWidths[i] := z;
  end; 
end;

procedure TStringGrid.Init(aNode : TXmlElement; OnProgress : TXMLProgress = nil);
var
  i: Integer;

  procedure ClearGrid;
  begin
    RowCount := 0;
    FixedRows := 0;
    RowCount := 1;
    ColCount := 2;
    FixedCols := 1;
  end;

begin
  FNode := aNode;
  ClearGrid;
  RowCount := aNode.Attributes.Count;

  if Assigned(aNode) then
  begin
    Enabled := aNode.TagName <> 'xml';
    // attribs
    if aNode.NbAttributes > 0 then
    begin
      RowCount := aNode.NbAttributes + 1;
      with aNode do
        for i := 0 to NbAttributes - 1 do
        begin
          Cells[0, i] := AttributeName[i];
          if Length(AttributeValue[i]) > 50 then
            Cells[1, i] := ShortAttributeValue[i]
          else
            Cells[1, i] := AttributeValue[i];

        end;
    end;
  end;
end;


end.
