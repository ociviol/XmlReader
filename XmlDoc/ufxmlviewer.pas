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
  Grids, Menus, uxmldoc, Clipbrd, SynEditHighlighter, Graphics,
  SynHighlighterSQL, SynEdit, Dialogs, ActnList, uXMLHelpers;

type

  { TFxmlView }

  TFxmlView = class(TFrame)
    ActionRenameNode: TAction;
    ActionDuplicateNode: TAction;
    ActionDeleteNode: TAction;
    ActionInsertNode: TAction;
    ActionPasteNode: TAction;
    ActionCopyNode: TAction;
    ActionCopyNodePathAsText: TAction;
    ActionCopyXmlAsText: TAction;
    ActionMoveDown: TAction;
    ActionMoveUp: TAction;
    ActionList1: TActionList;
    btnApply: TButton;
    btnSearchTag: TButton;
    btnSearchText: TButton;
    edtTag: TEdit;
    edtText: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    N3: TMenuItem;
    mnuCopy: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel6: TPanel;
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
    procedure ActionCopyNodeExecute(Sender: TObject);
    procedure ActionCopyNodePathAsTextExecute(Sender: TObject);
    procedure ActionCopyXmlAsTextExecute(Sender: TObject);
    procedure ActionDeleteNodeExecute(Sender: TObject);
    procedure ActionDuplicateNodeExecute(Sender: TObject);
    procedure ActionInsertNodeExecute(Sender: TObject);
    procedure ActionMoveDownExecute(Sender: TObject);
    procedure ActionMoveUpExecute(Sender: TObject);
    procedure ActionPasteNodeExecute(Sender: TObject);
    procedure ActionRenameNodeExecute(Sender: TObject);
    procedure AttrGridEditingDone(Sender: TObject);
    procedure edtTagEnter(Sender: TObject);
    procedure edtTextEnter(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
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
    procedure TreeView1CustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure Wordwrap1Click(Sender: TObject);
    procedure AttrGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure mnuAddAttributeClick(Sender: TObject);
    procedure mnuRemoveAttributeClick(Sender: TObject);
    procedure PopupMenu3Popup(Sender: TObject);
    procedure TreeView1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TreeView1DragDrop(Sender, Source: TObject; X, Y: Integer);
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
    procedure EnabledActions;
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

procedure TFxmlView.EnabledActions;
begin
  ActionMoveDown.Enabled:= (Assigned(TreeView1.Selected) and Assigned(TreeView1.Selected.Data)) and
      (TXMLElement(TreeView1.Selected.Data).Index < TXMLElement(TreeView1.Selected.Data).Parent.NbElements-1);
  ActionMoveUp.Enabled:=(Assigned(TreeView1.Selected) and Assigned(TreeView1.Selected.Data)) and
                        (TXMLElement(TreeView1.Selected.Data).Index > 0);

  ActionRenameNode.Enabled := Assigned(TreeView1.Selected);
  ActionDuplicateNode.Enabled := Assigned(TreeView1.Selected);
  ActionDeleteNode.Enabled := Assigned(TreeView1.Selected);
  ActionInsertNode.Enabled := Assigned(TreeView1.Selected);
  ActionPasteNode.Enabled := pos('XMLNODE:', Clipboard.AsText)>0;
  ActionCopyNode.Enabled := Assigned(TreeView1.Selected);
  ActionCopyNodePathAsText.Enabled := Assigned(TreeView1.Selected);
  ActionCopyXmlAsText.Enabled := Assigned(TreeView1.Selected);

  //mnuDeleteNode.Enabled := Assigned(TreeView1.Selected);
  //mnuDuplicateNode.Enabled := Assigned(TreeView1.Selected);
  //mnuCopyNode.Enabled := Assigned(TreeView1.Selected);
  //mnuPasteNode.Enabled := pos('XMLNODE:', Clipboard.AsText)>0;

  if Assigned(TreeView1.Selected) then
  begin
    //mnuDuplicateNode.Enabled := TreeView1.Selected.Level > 0;
    ActionDuplicateNode.Enabled := TreeView1.Selected.Level > 0;
    //mnuCopyNode.Enabled :=  TreeView1.Selected.Level > 0;
    ActionCopyNode.Enabled := TreeView1.Selected.Level > 0;
    //mnuDeleteNode.Enabled := TreeView1.Selected.Level > 0;
    ActionDeleteNode.Enabled := TreeView1.Selected.Level > 0;
    ActionRenameNode.Enabled := TreeView1.Selected.Level > 0;
    ActionCopyNodePathAsText.Enabled := TreeView1.Selected.Level > 0;
    ActionCopyXmlAsText.Enabled := TreeView1.Selected.Level > 0;
  end;
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
  Clipboard.AsText := Memo1.SelText;
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
var
  attr, val : string;
begin
  if Assigned(TreeView1.Selected.Data) then
  begin
    attr := InputBox('New attribute name', 'Type in new attribute name', '');
    if attr <> '' then
      val := InputBox('New attribute value', 'Type in new attribute value', '');

    if val <> '' then
    begin
      TXmlElement(TreeView1.Selected.Data).AddAttrib(attr + '=' + val);
      AttrGrid.Init(TXmlElement(TreeView1.Selected.Data));
    end;
  end;
end;

procedure TFxmlView.mnuRemoveAttributeClick(Sender: TObject);
begin;
end;

procedure TFxmlView.PopupMenu1Popup(Sender: TObject);
begin
  EnabledActions;
end;

procedure TFxmlView.PopupMenu2Popup(Sender: TObject);
begin
  mnuCopyMemo.Enabled := Memo1.SelLength > 0;
  //Wordwrap1.Checked := SynMemo1.WordWrap;
end;

procedure TFxmlView.TreeView1CustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  with Sender.Canvas do
  if TXmlElement(Node.Data).Text.Length > 0 then
    Font.Style := [fsBold]
  else
    Font.Style := [];
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
  if Assigned(TreeView1.Selected) then
    if Assigned(TreeView1.Selected.Data) then
    begin
      Ex := TXmlElement(TreeView1.Selected.Data);

      Memo1.Clear;
      AttrGrid.ClearGrid;
      Memo1.Enabled := Assigned(Ex);

      if TreeView1.Selected.Level > 1 then
        if Assigned(Ex) and (TreeView1.Selected.Level > 0) then
        begin
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
            Memo1.Lines.Text := Ex.Text;
        end;
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

procedure TFxmlView.AttrGridEditingDone(Sender: TObject);
begin
  if (AttrGrid.Col > 0) and (AttrGrid.Row > 0) then
    with AttrGrid, Node do
       if Cells[0, Row] <> '' then
         if GetAttribute(Cells[0, Row]) <> Cells[1, Row] then
         begin
           SetAttribute(Cells[0, Row], Cells[1, Row]);
           TreeView1.Selected.Text := MakeNodeText(TreeView1.Selected.Data);
         end;
end;

procedure TFxmlView.ActionMoveUpExecute(Sender: TObject);
var
  Node, Dad : TXMLElement;
  i, p : integer;
begin
  if Assigned(TreeView1.Selected) and Assigned(TreeView1.Selected.Data) then
  begin
    Node := TXMLElement(TreeView1.Selected.Data);
    Dad := Node.Parent;
    p := -1;
    for i:=0 to Dad.NbElements-1 do
      if Dad.Elements[i] = Node then
      begin
        p := i;
        break;
      end;
    // found ?
    if p > 0 then
      Dad.SwapElements(p, p-1);
  end;
  FillTreeView;
end;

procedure TFxmlView.ActionPasteNodeExecute(Sender: TObject);
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

procedure TFxmlView.ActionRenameNodeExecute(Sender: TObject);
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

procedure TFxmlView.ActionMoveDownExecute(Sender: TObject);
var
  Node, Dad : TXMLElement;
  i, p : integer;
begin
  if Assigned(TreeView1.Selected) and Assigned(TreeView1.Selected.Data) then
  begin
    Node := TXMLElement(TreeView1.Selected.Data);
    Dad := Node.Parent;
    p := -1;
    for i:=0 to Dad.NbElements-1 do
      if Dad.Elements[i] = Node then
      begin
        p := i;
        break;
      end;
    // found ?
    if (p >= 0) and (p < Dad.NbElements-1) then
      Dad.SwapElements(p, p+1);
  end;
  FillTreeView;
end;

procedure TFxmlView.ActionCopyXmlAsTextExecute(Sender: TObject);
begin
  Clipboard.AsText := FXmlDoc.AsString;
end;

procedure TFxmlView.ActionDeleteNodeExecute(Sender: TObject);
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

procedure TFxmlView.ActionDuplicateNodeExecute(Sender: TObject);
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

procedure TFxmlView.ActionInsertNodeExecute(Sender: TObject);
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

procedure TFxmlView.ActionCopyNodePathAsTextExecute(Sender: TObject);
begin
  with TreeView1 do
    if Assigned(Selected) then
      Clipboard.AsText := TXmlElement(Selected.Data).Path;
end;

procedure TFxmlView.ActionCopyNodeExecute(Sender: TObject);
begin
  Clipboard.AsText := 'XMLNODE:'+inttostr(Integer(TreeView1.Selected.Data));
end;

procedure TFxmlView.AttrGridSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  CanSelect := Assigned(TreeView1.Selected);
  if CanSelect then
  begin
    CanSelect := (TreeView1.SelectedXML.TagName <> 'xml');
    if aRow >0 then
      CanSelect := (AttrGrid.Cells[0, aRow] <> '')
  end;
end;

procedure TFxmlView.btnSearchMemoClick(Sender: TObject);
var
  s: String;
  i: Integer;
begin
  if Memo1.SelStart > 0 then
  begin
    s := Memo1.Lines.Text;
    s := Memo1.SelText;
    i := Pos(edtSearchMemo.Text, s);
    if i > 0 then
      Inc(i, Memo1.SelStart + Memo1.SelLength);
  end
  else
    i := Pos(edtSearchMemo.Text, Memo1.Lines.Text);

  if i > 0 then
  begin
    Memo1.SelStart := i - 1;
    Memo1.SelLength := length(edtSearchMemo.Text);
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

procedure TFxmlView.Memo1Change(Sender: TObject);
var
  Ex: TXmlElement;
begin
  if Assigned(TreeView1.Selected) then
  begin
    Ex := TXmlElement(TreeView1.Selected.Data);
    if ASsigned(Ex) then
    begin
      Ex.Text :=  Memo1.Text;
      edtSearchMemo.Enabled := Ex.Text <> '';
      btnSearchMemo.Enabled := Ex.Text <> '';
    end;
  end;
end;

end.
