unit ufxmlviewer;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$MODE Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, ComCtrls,
  Grids, Menus, uxmldoc, Clipbrd, SynEditHighlighter,
  SynHighlighterSQL, SynEdit, SynMemo, Dialogs, uXMLHelpers;

type

  { TFxmlView }

  TFxmlView = class(TFrame)
    btnApply: TButton;
    btnSearchTag: TButton;
    btnSearchText: TButton;
    edtTag: TEdit;
    edtText: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    mnuCopy: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pnlAttrib: TPanel;
    PopupMenu1: TPopupMenu;
    TreeView1: TTreeView;
    StatusBar1: TStatusBar;
    Splitter1: TSplitter;
    Panel4: TPanel;
    AttrGrid: TStringGrid;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Panel5: TPanel;
    edtSearchMemo: TEdit;
    btnSearchMemo: TButton;
    SynMemo1: TSynMemo;
    SynSQLSyn1: TSynSQLSyn;
    PopupMenu2: TPopupMenu;
    mnuCopyMemo: TMenuItem;
    Wordwrap1: TMenuItem;
    mnuCopyNodePath: TMenuItem;
    MnuInsert: TMenuItem;
    N1: TMenuItem;
    mnuDeleteNode: TMenuItem;
    SaveDialog1: TSaveDialog;
    PopupMenu3: TPopupMenu;
    mnuAddAttribute: TMenuItem;
    mnuRemoveAttribute: TMenuItem;
    mnuDuplicateNode: TMenuItem;
    mnuRenameNode: TMenuItem;
    mnuCopyNode: TMenuItem;
    N2: TMenuItem;
    mnuPasteNode: TMenuItem;
    procedure edtTagEnter(Sender: TObject);
    procedure edtTextEnter(Sender: TObject);
    procedure mnuCopyClick(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure edtTagChange(Sender: TObject);
    procedure btnSearchTagClick(Sender: TObject);
    procedure edtTextChange(Sender: TObject);
    procedure btnSearchTextClick(Sender: TObject);
    procedure edtSearchMemoChange(Sender: TObject);
    procedure btnSearchMemoClick(Sender: TObject);
    procedure edtSearchMemoEnter(Sender: TObject);
    procedure mnuCopyMemoClick(Sender: TObject);
    procedure PopupMenu2Popup(Sender: TObject);
    procedure Wordwrap1Click(Sender: TObject);
    procedure mnuCopyNodePathClick(Sender: TObject);
    //procedure AttrGridSetEditText(Sender: TObject; ACol, ARow: Integer;
    //  const Value: string);
    procedure AttrGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure MnuInsertClick(Sender: TObject);
    procedure mnuDeleteNodeClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure mnuAddAttributeClick(Sender: TObject);
    procedure mnuRemoveAttributeClick(Sender: TObject);
    procedure PopupMenu3Popup(Sender: TObject);
    procedure mnuDuplicateNodeClick(Sender: TObject);
    procedure mnuRenameNodeClick(Sender: TObject);
    procedure TreeView1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TreeView1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure mnuCopyNodeClick(Sender: TObject);
    procedure mnuPasteNodeClick(Sender: TObject);
    procedure AttrGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private
    { private declarations }
    FXmlDoc: TXMLDoc;
    Procedure FillTreeView;
    function GetNodeByText(AValue: String; ANode: TTreeNode; AVisible: Boolean)
      : TTreeNode;
    function GetNodeByText2(AValue: String; ANode: TTreeNode; AVisible: Boolean)
      : TTreeNode;
    function MakeNodeText(aElement: TXmlElement): String;
    function GetModified:Boolean;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(filename: String);
    procedure LoadFromText(aText: WideString);
    procedure Save;
    property XmlDoc: TXMLDoc read FXmlDoc;
    property Modified: Boolean read GetModified;
  end;

implementation

{$R *.lfm}

constructor TFxmlView.Create(aOwner: TComponent);
var
  gr: TGridRect;
begin
  inherited Create(aOwner);

  gr := AttrGrid.Selection;
  gr.Left := 1;
  gr.right := 1;
  AttrGrid.Selection := gr;

  FXmlDoc := TXMLDoc.Create;
  FillTreeView;
end;

destructor TFxmlView.Destroy;
begin
  FXmlDoc.Free;
  inherited Destroy;
end;

function TFxmlView.GetModified:Boolean;
begin
  Result := FXmlDoc.Modified;
end;

procedure TFxmlView.LoadFromText(aText: WideString);
begin
  Screen.Cursor := CrHourglass;
  try
    StatusBar1.SimpleText := 'Loading data ...';
    Update;
    FXmlDoc.AsString := aText;
    FillTreeView;
  finally
    StatusBar1.SimpleText := 'Done.';
    Screen.Cursor := CrDefault;
  end;
end;

procedure TFxmlView.Save;
begin
  if not FileExists(XmlDoc.filename) { *Converted from FileExists* } then
  begin
    with SaveDialog1 do
      if Execute then
        XmlDoc.SaveToFile(Files[0]);
  end
  else
    XmlDoc.SaveToFile(XmlDoc.filename);
end;

procedure TFxmlView.mnuCopyMemoClick(Sender: TObject);
begin
  with SynMemo1 do
  begin
    if (SelStart <> SelEnd) then
      Clipboard.AsText := Copy(Lines.Text, SelStart, SelEnd)
    else
      Clipboard.AsText := Lines.Text;
  end;
end;

procedure TFxmlView.mnuCopyNodeClick(Sender: TObject);
begin
  Clipboard.AsText := 'XMLNODE:'+inttostr(Integer(TreeView1.Selected.Data));
end;

procedure TFxmlView.mnuCopyNodePathClick(Sender: TObject);
begin
  with TreeView1 do
    if Assigned(Selected) then
      Clipboard.AsText := TXmlElement(Selected.Data).Path;
end;

procedure TFxmlView.LoadFromFile(filename: String);
begin
  Screen.Cursor := CrHourglass;
  try
    StatusBar1.SimpleText := 'Loading file ...';
    Update;
    FXmlDoc.LoadFromFile(filename);
    FillTreeView;
  finally
    StatusBar1.SimpleText := 'Done.';
    Screen.Cursor := CrDefault;
  end;
end;

procedure TFxmlView.mnuAddAttributeClick(Sender: TObject);
begin
  with AttrGrid do
    RowCount := RowCount + 1;
end;

procedure TFxmlView.mnuPasteNodeClick(Sender: TObject);
var
  s : String;
  aNode : TXmlElement;
  Node : TTreeNode;
begin
  screen.Cursor := crHourglass;
  try
    s := ClipBoard.AsText;
    s := copy(s ,9, length(s));
    aNode := TXMLElement(StrToInt(s));
    aNode := TXMLElement(Treeview1.Selected.Data).InsertNode(aNode);
    Node := TreeView1.Items.AddChildObject(Treeview1.Selected, aNode.TagName, aNode);
    TreeView1.AddTreeNodes(aNode, Node);
  finally
    screen.Cursor := crDefault;
  end;
end;

procedure TFxmlView.mnuRemoveAttributeClick(Sender: TObject);
begin;
end;

procedure TFxmlView.mnuRenameNodeClick(Sender: TObject);
var
  aElem: TXmlElement;
  s: string;
begin
  aElem := TXmlElement(TreeView1.Selected.Data);
  s := InputBox('Rename node', 'Type a new name for the node', aElem.TagName);
  if s <> aElem.TagName then
  begin
    aElem.TagName := s;
    TreeView1.Selected.Text := MakeNodeText(aElem);
  end;
end;

procedure TFxmlView.mnuCopyClick(Sender: TObject);
begin
  Clipboard.AsText := FXmlDoc.AsString;
end;

procedure TFxmlView.PopupMenu1Popup(Sender: TObject);
begin
  mnuDeleteNode.Enabled := Assigned(TreeView1.Selected);
  mnuDuplicateNode.Enabled := Assigned(TreeView1.Selected);
  mnuCopyNode.Enabled := Assigned(TreeView1.Selected);
  mnuPasteNode.Enabled := pos('XMLNODE:', Clipboard.AsText)>0;

  if Assigned(TreeView1.Selected) then
  begin
    mnuDuplicateNode.Enabled := TreeView1.Selected.Level > 0;
    mnuCopyNode.Enabled :=  TreeView1.Selected.Level > 0;
    mnuDeleteNode.Enabled := TreeView1.Selected.Level > 0;
  end;
end;

procedure TFxmlView.PopupMenu2Popup(Sender: TObject);
begin
  mnuCopyMemo.Enabled := SynMemo1.Lines.Text <> '';
  //Wordwrap1.Checked := SynMemo1.WordWrap;
end;

procedure TFxmlView.PopupMenu3Popup(Sender: TObject);
begin
  mnuAddAttribute.Enabled := Assigned(TreeView1.Selected);
  mnuRemoveAttribute.Enabled := mnuAddAttribute.Enabled;
end;

procedure TFxmlView.TreeView1Change(Sender: TObject; Node: TTreeNode);
var
  Ex: TXmlElement;
begin
  Ex := TXmlElement(TreeView1.Selected.Data);
  SynMemo1.Clear;

  if Assigned(Ex) then
  begin
    SynMemo1.Enabled := Ex.Text <> '';
    edtSearchMemo.Enabled := Ex.Text <> '';
    btnSearchMemo.Enabled := Ex.Text <> '';
    AttrGrid.Enabled := Ex.TagName <> 'xml';
    // attribs
    if Ex.NbAttributes > 0 then
    begin
      pnlAttrib.Visible := True;
      btnApply.Visible := True;
      btnApply.Enabled := False;

      AttrGrid.Init(Ex);
    end;
    // text
    if Ex.Text <> '' then
      SynMemo1.Lines.Text := Ex.Text;
  end;
end;

procedure TFxmlView.TreeView1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  aElem, aDest: TXmlElement;
  ANode: TTreeNode;

  function MoveNode(TargetNode, SourceNode: TTreeNode): TTreeNode;
  var
    i: Integer;
  begin
    with TreeView1 do
    begin
      result := Items.AddChildObject(TargetNode, SourceNode.Text, SourceNode.Data);
      for i := 0 to SourceNode.Count - 1 do
        MoveNode(result, SourceNode.ItemS[i]);
    end;
  end;

begin
  aElem := TXmlElement(TreeView1.Selected.Data);
  aDest := TXmlElement(TreeView1.GetNodeAt(X, Y).Data);

  XmlDoc.MoveElement(aElem, aDest);
  TreeView1.Items.BeginUpdate;
  try
    ANode := MoveNode(TreeView1.GetNodeAt(X, Y), TreeView1.Selected);
    TreeView1.Selected.Delete;
    TreeView1.Selected := ANode;
    ANode.Expand(False);
  finally
    TreeView1.Items.EndUpdate;
  end;
end;

procedure TFxmlView.TreeView1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  aDest: TTreeNode;
begin
  if Sender is TTreeView then
  begin
    Accept := True;
    aDest := TreeView1.GetNodeAt(X, Y);
    if Assigned(aDest) then
      Accept := (aDest.Level > 0) and
                (aDest <> Treeview1.Selected.Parent) and
                (Treeview1.Selected <> aDest);
  end
  else
    Accept := False;
end;

{
  procedure TFxmlView.TreeView1CustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
  var
  aRect : TRect;
  w1: Integer;
  DrawRect: TRect;
  s : string;
  begin
  aRect := Node.DisplayRect(True);
  w1 := Sender.Canvas.TextWidth('X');
  if Sender.Canvas.TextWidth(Node.Text) > (aRect.Right - aRect.Left) then
  s := WrapText(Node.Text, #13#10, [' '], (aRect.Right - aRect.Left) div w1);
  // calc rectangle
  DrawText(Sender.Canvas.Handle, PChar(s), Length(s), DrawRect,
  DT_CALCRECT or DT_NOCLIP or DT_WORDBREAK);

  Sender.Canvas.FillRect(aRect);
  Sender.Canvas.TextOut(ARect.Left, ARect.Top, s);
  DefaultDraw := True;
  end;
}

procedure TFxmlView.Wordwrap1Click(Sender: TObject);
begin
  //SynMemo1.WordWrap := not SynMemo1.WordWrap;
end;

function TFxmlView.MakeNodeText(aElement: TXmlElement): String;
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

procedure TFxmlView.mnuDeleteNodeClick(Sender: TObject);
var
  aElement: TXmlElement;
begin
  aElement := TXmlElement(TreeView1.Selected.Data);
  if Assigned(aElement) then
  begin
    TreeView1.Items.BeginUpdate;
    try
      aElement.Parent.DeleteChildNode(aElement);
      TreeView1.Items.Delete(TreeView1.Selected);
    finally
      TreeView1.Items.EndUpdate;
    end;
  end;
end;

procedure TFxmlView.mnuDuplicateNodeClick(Sender: TObject);
var
  aElement, aNewElem: TXmlElement;
  ANode: TTreeNode;
begin
  aElement := TXmlElement(TreeView1.Selected.Data);
  if Assigned(aElement) then
  begin
    TreeView1.Items.BeginUpdate;
    try
      aNewElem := aElement.Parent.AddChildNode(aElement.TagName);
      aNewElem.Assign(aElement);
      ANode := TreeView1.Items.AddChildObject(TreeView1.Selected.Parent,
        aNewElem.TagName, aNewElem);
      TreeView1.AddTreeNodes(aNewElem, ANode);
      ANode.Expand(False);
      ANode.Text := MakeNodeText(aNewElem);
    finally
      TreeView1.Items.EndUpdate;
    end;
  end;
end;

procedure TFxmlView.MnuInsertClick(Sender: TObject);
var
  aElement: TXmlElement;
  ANode: TTreeNode;
begin
  if Assigned(TreeView1.Selected) then
  begin
    aElement := TXmlElement(TreeView1.Selected.Data);
    with aElement, TreeView1 do
    begin
      aElement := aElement.AddChildNode('New');
      ANode := Items.AddChildObject(Selected, 'New', aElement);
      Selected.Expand(True);
      Selected := ANode;
    end;
  end
  else
  begin
    aElement := XmlDoc.DocumentElement;
    if not Assigned(aElement) then
      XmlDoc.CreateNewDocumentElement('Document');

    // TreeView1.Items.AddChildObject(nil, 'Document', aElement).Expand(True);
    FillTreeView;
  end;
end;

procedure TFxmlView.FillTreeView;
begin
  TreeView1.Init(FXmlDoc.Element);
end;

function TFxmlView.GetNodeByText(AValue: String; ANode: TTreeNode;
  AVisible: Boolean): TTreeNode;
var
  Node: TTreeNode;
begin
  result := nil;
  if TreeView1.Items.Count = 0 then
    Exit;
  if ANode = nil then
    Node := TreeView1.Items[0]
  else
    Node := ANode.GetNext;
  while Node <> nil do
  begin
    if Copy(UpperCase(Node.Text), 1, length(AValue)) = UpperCase(AValue) then
    begin
      result := Node;
      if AVisible then
      begin
        TreeView1.Selected := result;
        result.MakeVisible;
        TreeView1Change(TreeView1, result);
        SetFocus;
      end;
      Break;
    end;
    Node := Node.GetNext;
  end;
end;

function TFxmlView.GetNodeByText2(AValue: String; ANode: TTreeNode;
  AVisible: Boolean): TTreeNode;
var
  Node: TTreeNode;
begin
  result := nil;
  if TreeView1.Items.Count = 0 then
    Exit;
  if ANode = nil then
    Node := TreeView1.Items[0]
  else
    Node := ANode.GetNext;
  while Node <> nil do
  begin
    if Pos(UpperCase(AValue), UpperCase(Node.Text)) > 0 then
    begin
      result := Node;
      if AVisible then
      begin
        TreeView1.Selected := result;
        result.MakeVisible;
        TreeView1Change(TreeView1, result);
        SetFocus;
      end;
      Break;
    end;
    Node := Node.GetNext;
  end;
end;

procedure TFxmlView.edtSearchMemoChange(Sender: TObject);
begin
  btnSearchMemo.Enabled := edtSearchMemo.Text <> '';
end;

procedure TFxmlView.edtSearchMemoEnter(Sender: TObject);
begin
  btnSearchMemo.Default := True;
end;

procedure TFxmlView.edtTagChange(Sender: TObject);
begin
  btnSearchTag.Enabled := (edtTag.Text <> '') and
    Assigned(FXmlDoc.DocumentElement);
end;

procedure TFxmlView.edtTagEnter(Sender: TObject);
begin
  btnSearchText.Default := False;
  btnSearchTag.Default := True;
end;

procedure TFxmlView.AttrGridSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  CanSelect := Assigned(TreeView1.Selected);
  if CanSelect then
    CanSelect := TreeView1.SelectedXML.TagName <> 'xml';
end;

procedure TFxmlView.AttrGridSetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
begin
 if ACol > 0 then
   with AttrGrid, Node do
      if Cells[0, ARow] <> '' then
        if GetAttribute(Cells[0, ARow]) <> Cells[1, ARow] then
        begin
          SetAttribute(Cells[0, ARow], Cells[1, ARow]);
          TreeView1.Selected.Text := MakeNodeText(TreeView1.Selected.Data);
        end;
end;

procedure TFxmlView.btnSearchMemoClick(Sender: TObject);
var
  s: String;
  i: Integer;
begin
  if SynMemo1.SelStart > 0 then
  begin
    s := SynMemo1.Lines.Text;
    s := Copy(s, SynMemo1.SelEnd + 1, length(s));
    i := Pos(edtSearchMemo.Text, s);
    if i > 0 then
      Inc(i, SynMemo1.SelEnd);
  end
  else
    i := Pos(edtSearchMemo.Text, SynMemo1.Lines.Text);

  if i > 0 then
  begin
    SynMemo1.SelStart := i - 1;
    SynMemo1.SelEnd := (i - 1) + length(edtSearchMemo.Text);
  end;
end;

procedure TFxmlView.btnSearchTagClick(Sender: TObject);
begin
  GetNodeByText(edtTag.Text, TreeView1.Selected, True);
end;

procedure TFxmlView.edtTextChange(Sender: TObject);
begin
  btnSearchText.Enabled := (edtText.Text <> '') and
    Assigned(FXmlDoc.DocumentElement);
end;

procedure TFxmlView.btnSearchTextClick(Sender: TObject);
begin
  GetNodeByText2(edtText.Text, TreeView1.Selected, True);
end;

procedure TFxmlView.edtTextEnter(Sender: TObject);
begin
  btnSearchTag.Default := False;
  btnSearchText.Default := True;
end;

end.