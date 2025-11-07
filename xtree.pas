(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XTree;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}


interface

uses
  Classes, SysUtils,TypInfo, NodeUtils,StringUtils,EventsInterface, Events,
  {$ifndef JScript}
  fpjson, jsonparser,LazLogger,
  LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls,ComCtrls,
  Propedits,RTTICtrls,
  LazsUtils,
  {$else}
  HTMLUtils,
  {$endif}
  WrapperPanel;

type TTreeNodeHint = function(TreeLabelStr:String):String of object;

{$ifndef JScript}
type TXTree=class;
//type TTreeNodeDropAccepted = function(myTree:TObject; SourceName,SrcText,DstText:String):Boolean of object;
{$else}
//type TTreeNodeDropAccepted = function(myTree:TObject; SourceName,SrcText,DstText:String):Boolean of object;

const  TreeNodeHighlightColor:String = '#ffff00';          //yellow


function addTreeStyles(dummy:string):string;
function addTreeNode(WrapperNodeId,NameSpace,parentName,NameOfDetailsList,SummaryText,HasChildren,color,isopen:string;
                 NodeHintFunc:TTreeNodeHint;Draggable:Boolean):string;
function addnode(WrapperNodeId,NameSpace,ParentName,currentNodeTree,IdOfNodeBeingAdded:string;
                 OpenToLevel,level:integer;normalColor:String;NodeHintFunc:TTreeNodeHint;Draggable:Boolean):string;
function SetOpenStatusOfNodeChildren(NodeName,NameOfSelectedNode:string;level:integer):boolean;
function GetTreeRootID(NodeID:String):String;
function deselectNodeChildren(NodeName,normalColor:String):string;
function clearNodeSelectedMarker(NameOfDetailsList,normalColor:String):string;
function OpenAndScrollToSelectedNode(NameOfDetailsList:string):string;
procedure clearTreeNode(parentName:string);
procedure HandleTreeNodeClick(WrapperNodeId,NameSpace,SelectedNodeObjectId:String);
procedure DragStart(nodeId,NameSpace,NameOfDetailsList,SummaryText:String);
function HandleTreeNodeDragOver(ob:TObject;DestTreeId,NameSpace,DstText:string): Boolean;
procedure HandleTreeNodeDrop(ob:TObject;DestTreeId,NameSpace,DstText:string);
function NodeIdFromText(TreeNode:TDataNode;NodeText:String):String;
procedure SetDraggableAttribute(NodeName,draggable:string);

{$endif}




{$ifndef JScript}
type
PNodeDataRec = ^TNodeDataRec;
TNodeDataRec = record
  ID:String;
  NodeHint: string;
  OriginalText:String;
end;


type TMyTreeView = class(TTreeView)
  public
     var
  lastHintNode : TTreeNode;
  IsDropTarget:Boolean;
  DropTargetNode:TTreeNode;


  procedure CustomDrawTreeNode(Sender: TCustomTreeView;Node: TTreeNode; State: TCustomDrawState;var DefaultDraw: Boolean) ;
  procedure TreeSelectedNodeChange(Sender: TObject; Node: TTreeNode);
  procedure TreeEditingEnd(Sender:TObject; Node:TTreeNode; Cancel:Boolean);
  procedure HandleClick(Sender: TObject) ;
  procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer) ;
  procedure HandleDragDrop(Sender, Source: TObject; X, Y: Integer);
  procedure HandleDragOver(Sender, Source: TObject; X, Y: Integer;
                                   State: TDragState; var Accept: Boolean);
  procedure HandleMouseLeave(Sender: TObject) ;
  procedure KillEditing(Sender: TObject; Node:TTreeNode; xx:Boolean) ;
  function NodeHint(tn: TTreeNode): string;
  function AddATreeNode(ParentNode:TTreeNode; nodeText,TreeName:string):TTreeNode;
  procedure PopulateMeFromJSONData(NodeTreeString,RootName:String;openlvl:integer);
  procedure selectNodeByText(NodeText:String);
  procedure selectNodeByTextPath(PathText:String);
  procedure selectNodeById(NodeId:String);
  function AddNewNode(NodeItems:TTreeNodes;ParentNode:TTreeNode; NodeString,TreeName:string):TTreeNode;
  function AddChildren(jData: TJSONData; ParentNode:TTreeNode;RootName:String):TTreeNode;
  procedure ExpandTreeNodes(Level: Integer);
  function BuildExpandedText(thisNode:TTreeNode):String;
  procedure SetNodeHint(Sender: TObject; Shift: TShiftState; X, Y: Integer) ;
  function BuildTreeDataString(FromNode:TTreeNode;str:String):String;
  function FindNodeByText(AValue:String):TTreeNode;
  function FindNodeByExpandedText(AValue:String):TTreeNode;
  function NodeTextIsUnique(NodeText:String):Boolean;
  function FindNodeById(NodeId:String):TTreeNode;
end;
{$endif}


type
  TXTree = class(TWrapperPanel)
  private
    { Private declarations }
    fAllowDrop:Boolean;
    fTreeNodeHint:TTreeNodeHint;
    fSelectedNodeId:String;

    {$ifndef JScript}
    fHandleClick:TEventHandler;
    fHandleChange:TEventHandler;
    fHandleDragStart:TEventHandler;
    fHandleDrop:TEventHandler;
    fDropAccepted:TEventHandler;

    procedure TreeClick(Sender:TObject);
    {$else}
    fNodeBeingDragged:String;
    {$endif}

    procedure SetMyEventTypes;

    function GetTreeData:string;
    function GetSelectedNodeText:string;
    function GetSelectedTextPath:String;
    function GetTreeWidth:string;
    function GetTreeHeight:string;
    function GetReadOnly:Boolean;
    function GetDraggable:Boolean;
    function GetopenTolevel:Integer;

    procedure SetTreeData(NodeTreeString:string);
    procedure SetSelectedNodeText(AValue:string);
    procedure SetTreeWidth(AValue:string);
    procedure SetTreeHeight(AValue:string);
    procedure SetReadOnly(AValue:Boolean);
    procedure SetDraggable(AValue:Boolean);
    procedure SetOpenToLevel(AValue:Integer);

  protected
    { Protected declarations }
    {$ifndef JScript}
    procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
    property ParentColor;
    {$endif}
  public
    { Public declarations }
    {$ifndef JScript}
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent;IsDynamic:Boolean); override;
    {$else}
    constructor Create(MyForm:TForm;NodeName,NameSpace:String);
    {$endif}

    procedure DeSelectNode;
    function InsertNewSiblingNode(NodeText:String):String;
    function InsertNewChildNode(NodeText:String):String;
    procedure DeleteSelectedNode;
    procedure MoveNode(SourceNodeText,DestNodeText:String);
    function NodeTextIsUnique(NodeText:String):Boolean;
    function BuildTreeDataString:String;
    procedure SelectTreeNodeById(NodeId:string);
    procedure SetSelectedNodeId(AValue:String);
    procedure SelectTreeNodeByPath(NodePath:string);
    {$ifndef JScript}
    procedure SelectTreeNodeByText(NodeText:string);
    {$else}
    function GetChildNodes(nodeId:String):TStringArray;
    function TextOfNode(nodeId:String):String;
    function NumChildNodes(nodeid:String):integer;
    procedure SetNodeText(nodeid,newtext:String);
    function getParentOfNode(nodeid:String):String;
    {$endif}
  published
    { Published declarations }

    // Properties defined for this class...
    property SelectedNodeText: String read GetSelectedNodeText write SetSelectedNodeText;
    property SelectedTextPath:String read GetSelectedTextPath;   //read-only property
    property TreeData: String read GetTreeData write SetTreeData;
    property TreeHeight: String read GetTreeHeight write SetTreeHeight;
    property TreeWidth: String read GetTreeWidth write SetTreeWidth;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Draggable: Boolean read GetDraggable write SetDraggable;
    property OpenToLevel: Integer read GetOpenToLevel write SetOpenToLevel;
    property AllowDrop:Boolean read fAllowDrop write fAllowDrop;

    // these properties will appear as events on Lazarus IDE...
    property TreeNodeHintFunc: TTreeNodeHint read FTreeNodeHint write FTreeNodeHint;

    {$ifndef JScript}
    // Events to be visible in Lazarus IDE
    property HandleClick: TEventHandler read FHandleClick write FHandleClick;
    property HandleTreeNodeClick: TEventHandler read FHandleChange write FHandleChange;
    property HandleDragStart: TEventHandler read FHandleDragStart write FHandleDragStart;
    property HandleDrop: TEventHandler read FHandleDrop write FHandleDrop;
    property HandleDropAccepted: TEventHandler read FDropAccepted write FDropAccepted;
    {$else}
    property NodeBeingDragged: String read fNodeBeingDragged write fNodeBeingDragged;   // NodeBeingDragged is a SUMMARY item in the HTML tree structure
    {$endif}
    property SelectedNodeId: String read fSelectedNodeId write SetSelectedNodeId;       // SelectedNodeId is a SUMMARY item in the HTML tree structure

end;


{$ifndef JScript}
//var
//    TreeNodeIdCounter:longint;

procedure Register;
{$endif}


var
  DraggingTree:TXTree;

implementation

const MyNodeType='TXTree';


const
  ExampleNodeTree = '["Root",["Item1","Level2 Item"],"Item2","Item3",["Item4","Child Item"],"Item5"]';

var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXTree.SetMyEventTypes;
begin
  MyEventTypes.Add('Created');
  MyEventTypes.Add('Click');
  MyEventTypes.Add('TreeNodeClick');
  MyEventTypes.Add('DragStart');
  MyEventTypes.Add('Drop');
  MyEventTypes.Add('DropAccepted');
end;

{$ifndef JScript}
procedure Register;
begin
  {$I xtree_icon.lrs}
  RegisterComponents('XComponents',[TXTree]);

  // inherited from TWrapperPanel, not required here
  RegisterPropertyEditor(TypeInfo(TColor), TXTree, 'BgColor', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(TXPropertyLink), TXTree, 'Link', THiddenPropertyEditor);
end;

function GetNewNodeId(ParentNode:TTreeNode; TreeName:String):String;
var
  str:String;
  PNode,sibNode:TTreeNode;
  c:integer;
begin
  //TreeNodeIdCounter:=TreeNodeIdCounter+1;
  //result:=inttostr(TreeNodeIdCounter);
  pNode:=ParentNode;
  if pNode<>nil then
  begin
    sibNode:=pNode.GetLastChild;
    if sibNode<>nil then
      c:=sibNode.Index+1
    else
      c:=0;
  end
  else
    c:=0;
  str:=intToStr(c);
  while (pNode<>nil) do
  begin
    c:=PNode.Index;  // num within sibling set??
    str:=intToStr(c)+'_'+str;
    pNode:=pNode.Parent;
  end;
  result:=TreeName+str;
end;

constructor TXTree.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXTree.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXTree.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  myControl:=TmyTreeView.Create(self);
  myControl.Parent:=self;


  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];

  TmyTreeView(myControl).DragMode:=dmAutomatic;
  TmyTreeView(myControl).IsDropTarget:=false;
  TmyTreeView(myControl).ShowHint:=true;

  //myControl.Enabled:=true;      //????
  TmyTreeView(myControl).ReadOnly:=false;      //????
  //TmyTreeView(myControl).ReadOnly:=true;     // prevent editing nodes.   !!Lazarus. doesn't work
  //TmyTreeView(myControl).OnEditing:=@TmyTreeView(myControl).KillEditing;
  TmyTreeView(myControl).OnEditingEnd:=@TmyTreeView(myControl).KillEditing;

  TmyTreeView(myControl).OnEditingEnd:=@TmyTreeView(myControl).TreeEditingEnd;
  TmyTreeView(myControl).OnChange := @TmyTreeView(myControl).TreeSelectedNodeChange;        // NB. async
  TmyTreeView(myControl).OnCustomDrawItem:=@TmyTreeView(myControl).CustomDrawTreeNode;
  TmyTreeView(myControl).OnClick := @TmyTreeView(myControl).HandleClick;
  TmyTreeView(myControl).OnMouseMove := @TmyTreeView(myControl).HandleMouseMove;           // set node hint; DragStart
  TmyTreeView(myControl).OnMouseLeave:=@TmyTreeView(myControl).HandleMouseLeave;           // IsDropTarget:=false
  TmyTreeView(myControl).OnDragDrop := @TmyTreeView(myControl).HandleDragDrop;             // 'Drop' - stop drag mode
  TmyTreeView(myControl).OnDragOver := @TmyTreeView(myControl).HandleDragOver;             // is drop allowed

  TmyTreeView(myControl).Options:=TTreeView(myControl).Options - [tvoThemedDraw];
  TmyTreeView(myControl).SelectionColor:=clYellow;  // only works when options are set as above

  self.SetMyEventTypes;

  CreateComponentDataNode2(self,MyNodeType,myDefaultAttribs, self.myEventTypes, TheOwner,IsDynamic);

  self.ParentColor:=true;
  // Setting IsContainer false will prevent Laz IDE dropping new child controls into this one.
  self.IsContainer:=false;

  AddLabel(myControl,self.LabelText);

end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXTree',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;

function TmyTreeView.NodeHint(tn: TTreeNode): string;
var
  MyRecPtr: PNodeDataRec;
begin
  if tn<>nil then
  begin
    MyRecPtr:=tn.Data;
  end;
  if (MyRecPtr<>nil)
  then
    try
    result:=MyRecPtr^.NodeHint
    except
    result:='';
    end
  else
    result:='';
end;

procedure TmyTreeView.TreeEditingEnd(Sender:TObject; Node:TTreeNode; Cancel:Boolean);
begin
//  Node.Text:=self.MakeTextUnique(Node.Text);
end;

procedure TmyTreeView.HandleDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Src, Dst: TTreeNode;
  e:TeventStatus;
  DestName:String;
  DestNode:TDataNode;
  //values:TNodeEventValue;
begin
  Src := TmyTreeView(Source).Selected;
  Dst := TmyTreeView(Sender).GetNodeAt(X,Y);
  DestName:=TTreeView(Sender).parent.Name;

  if (Src<>nil)
  and (Dst<>nil) then
  begin
    TmyTreeView(Sender).DropTargetNode:=Dst;
    e:=TEventStatus.Create('Drop',DestName);
    DestNode:=TXTree(self.parent).myNode;
    //values:=TNodeEventValue.Create;
    e.DstText:=Dst.Text;
    //values.myTree:=TXTree(self.Parent);
    e.SrcText:=Src.Text;
    e.SourceName:=TmyTreeView(Source).Parent.Name;
    e.myTreeNode:=DestNode;
    //e.ValueObject:=values;
    CallHandleEvent(e,'Drop',Dst.Text,Sender);
  end;
  self.IsDropTarget:=false;
end;

procedure TmyTreeView.HandleDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  Src,DestTreeNode: TTreeNode;
  DestNode:TDataNode;
  SourceName, DestName, DstText:string;
  i:integer;
  e:TEventStatus;
//  ob:TNodeEventValue;
begin
  // Sender is TTreeView
  if (Source is TTreeView) then
  begin

    SourceName:=TTreeView(Source).parent.Name;         //!!!!namespace????
    DestName:=TTreeView(Sender).parent.Name;

    DestTreeNode := TTreeNode(TTreeView(Sender).GetNodeAt(X, Y)) ;
    Src := TTreeView(Source).Selected;

    if (Src<>nil) then
    begin
      Accept:=true;
      // Decide whether a drop is allowed here
      // 1) is there a function? (in the form, or registered, or in user code)
      // 2) execute the function - send in SourceName,Src.Text,DstText
      // 3) get the result (e.ReturnString)
      if (DestTreeNode<>nil) then
      begin
        DstText:=DestTreeNode.Text;
        DestNode:=TXTree(self.parent).myNode;
        e:=TEventStatus.Create('DropAccepted',DestName);
        //ob:=TNodeEventValue.Create;
        e.DstText:=DstText;
        //ob.myTree:=TXTree(self.Parent);
        e.SrcText:=Src.Text;
        e.SourceName:=SourceName;
        e.myTreeNode:=DestTreeNode;
        //e.ValueObject:=ob;
        FindAndRunEventHandler(e,'DropAccepted',DestName,'',DestNode);   //bypasses event trappers
        Accept := MyStrToBool(e.ReturnString);
      end;

      if (Src<>nil)
      and (self.IsDropTarget = false)
      then
      begin
        if (Source=Sender) then
          CallHandleEvent('DragStart',Src.Text,Sender);    // pick up source node for dragging
        if (Accept) then
        begin
          self.IsDropTarget:=true;
        end;
      end;

      if (Accept)
      and (DestTreeNode<>nil) then
      begin
        // mark the node as available for drop
        if DestTreeNode.HasChildren then
           TTreeView(Sender).SetInsertMark(DestTreeNode,tvimAsFirstChild)
        else
           TTreeView(Sender).SetInsertMark(DestTreeNode,tvimAsPrevSibling);
        TTreeView(Sender).Update;
      end;
    end;
  end;
end;

procedure TmyTreeView.SetNodeHint(Sender: TObject; Shift: TShiftState; X, Y: Integer) ;  //
var
  tree: TmyTreeView;
  hoverNode: TTreeNode;
  hitTest : THitTests;
begin
  tree:=TmyTreeView(Sender);

  hoverNode := TTreeNode(tree.GetNodeAt(X, Y)) ;
  if hovernode<>nil then
  begin
    hitTest := tree.GetHitTestInfoAt(X, Y) ;

    if (tree.lastHintNode <> hoverNode) then
    begin
      Application.CancelHint;

      if (hitTest <= [htOnItem, htOnIcon, htOnLabel, htOnStateIcon]) then
      begin
        tree.lastHintNode := hoverNode;
        tree.Hint := tree.NodeHint(hoverNode) ;
      end;
    end;
  end;
end;

procedure TmyTreeView.HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer) ;
begin
  if  not (ssLeft in Shift)
  and (self.IsDropTarget = false)   // do nothing if a drag/drop is in progress
  then
  begin
    if (X>-1) and (Y>-1) then
      SetNodeHint(Sender,Shift,X,Y);
  end;
end;

procedure TmyTreeView.KillEditing(Sender: TObject; Node:TTreeNode; xx:Boolean) ;
var
  MyRecPtr:PNodeDataRec;
begin
  if self.ReadOnly then
  begin
    MyRecPtr:=Node.Data;
    if MyRecPtr<>nil then
      Node.Text:=MyRecPtr^.OriginalText;
  end;
end;

procedure TmyTreeView.HandleClick(Sender: TObject) ;
begin
    self.IsDropTarget:=false;
end;

procedure TmyTreeView.HandleMouseLeave(Sender: TObject) ;
begin
  self.IsDropTarget:=false;

end;


procedure TmyTreeView.selectNodeByText(NodeText:String);
var
  TargetNode:TTreeNode;
begin
  if NodeText<>'' then
  begin
    TargetNode:=TTreeNode(self.Items.FindNodeWithText(NodeText));
    if TargetNode<>nil then
    begin
      if TargetNode<>self.Selected then
        self.Select(TargetNode);        // In Lazarus this will automatically open any parent nodes   //  this fires a change event????
    end;
  end
  else
    self.ClearSelection();
end;

procedure TmyTreeView.selectNodeByTextPath(PathText:String);
// path is '/' delimited
var
  TargetNode:TTreeNode;
begin
  if PathText<>'' then
  begin
    TargetNode:=TTreeNode(self.Items.FindNodeWithTextPath(PathText));
    if TargetNode<>nil then
    begin
      if TargetNode<>self.Selected then
        self.Select(TargetNode);        // In Lazarus this will automatically open any parent nodes   //  this fires a change event????
    end;
  end
  else
    self.ClearSelection();
end;


procedure TmyTreeView.selectNodeById(NodeId:String);
var
  TargetNode:TTreeNode;
begin
  if NodeId<>'' then
  begin
    TargetNode:=self.FindNodeById(NodeId);
    if TargetNode<>nil then
    begin
      if TargetNode<>self.Selected then
        self.Select(TargetNode);        // In Lazarus this will automatically open any parent nodes
    end;
  end
  else
    self.ClearSelection();
end;

procedure TmyTreeView.TreeSelectedNodeChange(Sender: TObject; Node: TTreeNode);
var
  outer:TObject;
  NameSpace,nodeName,nodeID:String;
  pth:String;
  MyRecPtr: PNodeDataRec;
begin
    if Assigned(Node) then
    begin
      // added 09/06/19 (so that selectednodetext is always set automatically.  previously had to use the TreeNodeClick event)
      nodeName := FindNodeId(Sender,outer,NameSpace);
      if (outer<>nil)
      and (outer is TXTree) then
      begin
        MyRecPtr:=Node.Data;
        NodeID:=MyRecPtr^.ID;
        TXTree(outer).myNode.SetAttributeValue('SelectedNodeText',Node.Text);
        pth:=TXTree(outer).SelectedTextPath;   // this sets the value for attribute 'SelectedTextPath'
        CallHandleEvent('TreeNodeClick',NodeID,Sender);
      end;
    end;
end;

function TmyTreeView.FindNodeByText(AValue:String):TTreeNode;
var
  i:integer;
  FoundNode:TTreeNode;
  function SearchChildNodes(StartNode:TTreeNode;AValue:String):TTreeNode;
  var
    j:integer;
    FoundNode:TTreeNode;
  begin
    foundNode:=nil;
    j:=0;
    while j < StartNode.Count - 1 do
    begin
      if StartNode.Items[j].Text = AValue then
        FoundNode:= StartNode.Items[j]
      else
        foundNode:=SearchChildNodes(StartNode.Items[j],AValue);
      if foundNode<>nil then
        j:=StartNode.Count;

      j:=j+1;
    end;
    result:=foundNode;
  end;

begin
  foundNode:=nil;
  i:=0;
  while i < self.Items.Count do
  begin
    if self.Items[i].Text = AValue then
      FoundNode:= self.Items[i]
    else
      foundNode:=SearchChildNodes(self.Items[i],AValue);
    if foundNode<>nil then
      i:=self.Items.Count;

    i:=i+1;
  end;
  result:=foundNode;
end;

function TmyTreeView.FindNodeByExpandedText(AValue:String):TTreeNode;
var
  i:integer;
  FoundNode:TTreeNode;
  function SearchChildNodes(StartNode:TTreeNode;AValue:String):TTreeNode;
  var
    j:integer;
    FoundNode:TTreeNode;
  begin
    j:=0;
    while j < StartNode.Count - 1 do
    begin
      if self.Items[j].GetTextPath = AValue then
        FoundNode:= StartNode.Items[j]
      else
        foundNode:=SearchChildNodes(StartNode.Items[j],AValue);
      if foundNode<>nil then
        j:=StartNode.Count;

      j:=j+1;
    end;
    result:=foundNode;
  end;

begin
  foundNode:=nil;
  i:=0;
  while i < self.Items.Count do
  begin
    if self.Items[i].GetTextPath = AValue then
      FoundNode:= self.Items[i]
    else
      foundNode:=SearchChildNodes(self.Items[i],AValue);
    if foundNode<>nil then
      i:=self.Items.Count;

    i:=i+1;
  end;
  result:=foundNode;
end;

function TmyTreeView.BuildExpandedText(thisNode:TTreeNode):String;
var
  i:integer;
  str:String;
  parentNode:TTreeNode;
begin
  str:=inttostr(thisNode.index)+'~:~'+thisNode.Text;
  parentNode:=thisNode;
  while parentNode.Parent<>nil do
  begin
    parentNode:=parentNode.Parent;
    i:=parentNode.Index;
    str:=inttostr(i)+'~:~'+parentNode.Text+'~/~'+str;
  end;
  result:=str;
end;

function TmyTreeView.NodeTextIsUnique(NodeText:String):Boolean;
var
  tn:TTreeNode;
begin
  tn:=self.FindNodeByText(NodeText);
  if tn<>nil then
    result:=false
  else
    result:=true;
end;

function TmyTreeView.FindNodeById(NodeId:String):TTreeNode;
  function GetNodeId(ANode:TTreeNode):String;
  var
    MyRecPtr:PNodeDataRec;
  begin
    MyRecPtr:=ANode.Data;
    result:=MyRecPtr^.ID;
  end;
var
  myItems:TTreeNodes;
  myId:String;
  nextNode:TTreeNode;
begin
  myItems:=self.Items;
  nextNode := myItems.GetFirstNode;
  myId:=GetNodeId(nextNode);
  while ((nextNode<>nil) and (myId <> NodeId)) do
  begin
    nextNode := nextNode.GetNext;
    if nextNode<>nil then
      myId:=GetNodeId(nextNode);
  end;
  result:=nextNode;
end;


procedure TmyTreeView.CustomDrawTreeNode(Sender: TCustomTreeView;Node: TTreeNode; State: TCustomDrawState;var DefaultDraw: Boolean) ;
begin
  DefaultDraw:=true;
end;

function TmyTreeView.AddNewNode(NodeItems:TTreeNodes;ParentNode:TTreeNode; NodeString,TreeName:string):TTreeNode;
var
  NewTreeNode:TTreeNode;
  myHint,NodeId:String;
  MyRecPtr: PNodeDataRec;
begin

  New(MyRecPtr);
  MyRecPtr^.ID:=GetNewNodeId(ParentNode,TreeName);
  if ParentNode<>nil then
    NewTreeNode:=NodeItems.AddChildObject(ParentNode,NodeString,MyRecPtr)
  else
  begin
    NewTreeNode:=NodeItems.Add(nil,NodeString);  //root node
    NewTreeNode.Data:=MyRecPtr;
  end;


  // Create the Hint text for this node, if specified.
  if (self.Parent<>nil) and (self.Parent is TXTree) then
    if TXTree(self.Parent).TreeNodeHintFunc<>nil then

    MyRecPtr^.NodeHint := TXTree(self.Parent).TreeNodeHintFunc(NodeString);

  MyRecPtr^.OriginalText := NewTreeNode.Text;   // save node text to workaround user edits (can't disable editing!!!!)
  result:=NewTreeNode;
end;

procedure TMyTreeView.ExpandTreeNodes(Level: Integer);
var
  I: Integer;
  Nodes:TTreeNodes;
begin
  Nodes:=self.Items;
  Nodes.BeginUpdate;
  try
    for I := 0 to Nodes.Count - 1 do
      if Nodes[I].Level < Level then
        Nodes[I].Expand(False);
  finally
    Nodes.EndUpdate;
  end;
end;

function TmyTreeView.BuildTreeDataString(FromNode:TTreeNode;str:String):String;
var
  myTree:TmyTreeView;
  ANode:TTreeNode;
  i:integer;
begin
  myTree:= self;

  ANode := FromNode;
  if ANode.HasChildren then
  begin
    str:=str+'[';
    str:=str+'"'+ANode.Text+'"';
    ANode:=ANode.GetFirstChild;
    if ANode<>nil then
    begin
      str:=str+',';
      str:=BuildTreeDataString(ANode,str);
      repeat
        ANode := ANode.getNextSibling;
        if ANode<>nil then
        begin
          str:=str+',';
          str:=BuildTreeDataString(ANode,str);
        end;
      until ANode = nil;
    end;
    str:=str+']';
  end
  else
  begin
    // no children
    str:=str+'"'+ANode.Text+'"';
  end;
  result:=str;
end;

function TMyTreeView.AddATreeNode(ParentNode:TTreeNode; nodeText,TreeName:string):TTreeNode;
begin
  result:= AddNewNode(self.Items,ParentNode, nodeText,TreeName);
end;
function TMyTreeView.AddChildren(jData: TJSONData; ParentNode:TTreeNode;RootName:String):TTreeNode;
var
  i:integer;
  jItem : TJSONData;
  NewNode, tmpNode:TTreeNode;
  object_type, nodename, tmp:string;
begin
  i:=0;
  if jData<>nil then
    while i < jData.Count do
    begin
      jItem:=jData.Items[i];
      tmp:=jItem.asJSON;
      //showmessage('add node '+jItem.AsJSON);
      object_type := GetEnumName(TypeInfo(TJSONtype), Ord(jItem.JSONType));
      // expecting first item is always a string
      if (i=0) and (object_type='jtString') then
      begin
        nodename:=jItem.AsString;
        NewNode:=self.AddATreeNode(ParentNode,jItem.AsString,RootName);
      end
      else if (object_type='jtString') then
      begin
        tmpNode:=self.AddATreeNode(NewNode,jItem.AsString,RootName);
      end
      else if object_type='jtArray' then
      begin
        self.AddChildren(jItem,NewNode,RootName);
      end;
      i:=i+1;
    end;
end;

procedure TMyTreeView.PopulateMeFromJSONData(NodeTreeString,RootName:String;openlvl:integer);
var
   jData,JItem : TJSONData;
   i:integer;
   object_type, nodename, tmp:string;
   newnode,parentnode:TTreeNode;
begin
  parentnode:=nil;
  with self.Items do
  begin
    Clear; { remove any existing nodes }

    try
      jData := GetJSON(NodeTreeString);
    except
      on E: Exception do
      begin
        showmessage(RootName+'  JSON error: '+e.Message);
        jData := nil;
      end;
    end;

    if jData<>nil then
    begin
      i:=0;
      // everything at this level is a child of the root node
      while i < jData.Count do
      begin
        jItem := jData.Items[i];
        tmp:=jItem.asJSON;
        object_type := GetEnumName(TypeInfo(TJSONtype), Ord(jItem.JSONType));
        if object_type='jtString' then
        begin
          nodename:=jItem.asString;
          newnode:=self.AddATreeNode(parentnode,nodename,RootName);
          if i=0 then parentnode:=newnode;
        end
        else if object_type='jtArray' then
        begin
          // first element in an array is the node, and following elements are node children
          self.AddChildren(jItem,parentnode,RootName);
        end;
        i:=i+1;
      end;
      self.ExpandTreeNodes(openlvl);
      jData.Free;
    end;
  end;
end;

procedure TXTree.TreeClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;

function TXTree.NodeTextIsUnique(NodeText:String):Boolean;
begin
  result:=TMyTreeView(self.myControl).NodeTextIsUnique(NodeText);
end;

function TXTree.BuildTreeDataString:String;
begin
  result:=TMyTreeView(self.myControl).BuildTreeDataString(TMyTreeView(self.myControl).Items[0],'');
end;

procedure TXTree.SetTreeWidth(AValue:string);
var
  tc:TControl;
begin
  tc:=self.myControl;
  myNode.SetAttributeValue('TreeWidth',AValue);
  SetHeightWidth(self.myNode,tc,'TreeWidth','TreeHeight');
end;

procedure TXTree.SetTreeHeight(AValue:string);
var
  tc:TControl;
begin
  tc:=self.myControl;
  myNode.SetAttributeValue('TreeHeight',AValue);
  SetHeightWidth(self.myNode,tc,'TreeWidth','TreeHeight');
end;

procedure TXTree.SelectTreeNodeByText(NodeText:string);
var
   ATreeView:TMyTreeView;
begin
   // select the required node in the tree
   //showmessage('select node with text: '+NodeText);
   ATreeView:=TMyTreeView(self.myControl);
   ATreeView.selectNodeByText(NodeText);
end;

procedure TXTree.SelectTreeNodeById(NodeId:string);
var
   ATreeView:TMyTreeView;
begin
   // select the required node in the tree
   //showmessage('select node with text: '+NodeText);
   ATreeView:=TMyTreeView(self.myControl);
   ATreeView.selectNodeById(NodeId);
end;
{$else}

function addTreeStyles(dummy:string):string;
begin

asm
  try{
      // ----------------------------------------check if the style has already been set
      var x = document.getElementsByTagName("STYLE");
      var StyleIsSet = false;
      if (x.length>0){
        for (var i=0; i<x.length; i++){
          var y= x[i].innerHTML;
          if (y.indexOf("summary.hasChildren") !=-1) { StyleIsSet =true}
        }
      }
      if (StyleIsSet == false){
         //<-------------- Styles to switch the child marker on and off on the summary line ---------------
         //var styletext = '<style> summary.noChildren::-webkit-details-marker { display:none; } </style>'
         var styletext = '<style> summary.noChildren { list-style: none;} </style>'
                       + '<style> summary.hasChildren {color:black; }</style> ';
         //----------------------------- now append the style declarations to the head of the HTML page
         document.head.innerHTML = document.head.innerHTML+styletext;
      }
 }catch(err) {alert('Error in XTree.addTreeStyles '+ err.message);}
end;

end;

procedure HandleTreeNodeDrop(ob:TObject;DestTreeId,NameSpace,DstText:string);
var
  thisXTree:TXTree;
  thisNode:TDataNode;
  e:TEventStatus;
  //NodeObj:TNodeEventValue;
begin
//asm
//alert('HandleTreeNodeDrop ob.id='+ob.id+' DestTreeId='+DestTreeId+' DstText='+DstText);
//end;
  //ob:object raising the event (a 'Summary' node)
  if DraggingTree<>nil then
  begin
    thisNode:=FindDataNodeById(SystemNodeTree,DestTreeId,NameSpace,true);
    thisXTree:=TXTree(thisNode.ScreenObject);
    begin
      //NodeObj:=TNodeEventValue.Create;
      e:=TEventStatus.Create('Drop',thisNode.NodeName);
      e.NameSpace:=NameSpace;
      e.DstText:=DstText;
      //NodeObj.myTree:=thisXTree;
      e.SrcText:=DraggingTree.NodeBeingDragged;
      e.SourceName:=DraggingTree.myNode.NodeName;
      e.myTreeNode:=ob;
      //e.ValueObject:=NodeObj;

      asm
      pas.Events.handleEvent(e, 'Drop' ,ob.id,NameSpace ,DstText);
      //pas.Events.handleEvent(e, 'Drop' ,ob.id,NameSpace ,ob.id);
      end;

    end;
  end;
end;

function HandleTreeNodeDragOver(ob:TObject;DestTreeId,NameSpace,DstText:string):Boolean;
var
  thisXTree:TXTree;
  thisNode:TDataNode;
  e:TEventStatus;
//  NodeObj:TNodeEventValue;
begin
//if DraggingTree=nil then showmessage('HandleTreeNodeDragOver DestTreeId='+DestTreeId+' DstText='+DstText+' DraggingTree=nil')
//else showmessage('HandleTreeNodeDragOver DestTreeId='+DestTreeId+' DstText='+DstText+' DraggingTree is set');
  //ob:object raising the event (a 'Summary' node)
  // DstText ..... text of node in the tree that is being dragged over
  result:=true;
  if DraggingTree<>nil then
  begin
    thisNode:=FindDataNodeById(SystemNodeTree,DestTreeId,NameSpace,true);
    thisXTree:=TXTree(thisNode.ScreenObject);
    // Is drop allowed?
    // 1) is there a function? (in the form, or registered, or in user code)
    // 2) execute the function - send in SourceName,Src.Text,DstText
    // 3) get the result (e.ReturnString)
    begin
      //NodeObj:=TNodeEventValue.Create;
      e:=TEventStatus.Create('DropAccepted',thisNode.NodeName);
      e.NameSpace:=NameSpace;
      e.DstText:=DstText;
      //NodeObj.myTree:=thisXTree;
      e.SrcText:=DraggingTree.NodeBeingDragged;
      e.SourceName:=DraggingTree.myNode.NodeName;
      e.myTreeNode:=ob;
      //e.ValueObject:=NodeObj;
      FindAndRunEventHandler(e,'DropAccepted',thisNode.NodeName,'',thisXTree);     //bypasses event trappers
      result := MyStrToBool(e.ReturnString);
    end;
  end;
end;

procedure DragStart(nodeId,NameSpace,NameOfDetailsList,SummaryText:String);
var
  treeNode:TDataNode;
  myXTree:TXTree;
begin
 // showmessage('DragStart.  NodeId='+NameSpace+'.'+nodeId);
  treeNode:=FindDataNodeById(SystemNodeTree,nodeId,NameSpace,true);
  myXTree:=TXTree(treenode.ScreenObject);
  DraggingTree:=myXTree;
  myXTree.NodeBeingDragged:=SummaryText;     //?NameOfDetailsList!!!!
  //showmessage('drag node '+SummaryText);
  asm
  pas.Events.handleEvent( null,'DragStart' ,NameOfDetailsList,NameSpace,SummaryText);
  end;
end;

//  HTML Tree Structure is...
// wrapper div id is (eg A=<NodeName>Node0)
//      A
//        B
//        C
//      D
//      E
//        F
//          G
//
//     <DIV  id=AOuterDiv >                     (eg <NodeName>Node0OuterDiv)
//        <DETAILS id=A >
//          <SUMMARY  id=ASummary />
//          <DIV  id=BOuterDiv >
//            <DETAILS id=B >
//              <SUMMARY  id=BSummary /></DETAILS>
//          </DIV>
//          <DIV  id=COuterDiv >
//            <DETAILS id=C >
//              <SUMMARY  id=CSummary /></DETAILS>
//          </DIV>
//       </DETAILS>
//     <DIV  id=DOuterDiv >
//        <DETAILS id=D >
//          <SUMMARY  id=DSummary /></DETAILS>
//     </DIV>
//     <DIV  id=EOuterDiv >
//        <DETAILS id=E >
//          <SUMMARY  id=ESummary />
//          <DIV  id=FOuterDiv >
//            <DETAILS id=F >
//              <SUMMARY  id=FSummary />
//              <DIV  id=GOuterDiv >
//                <DETAILS id=G >
//                  <SUMMARY  id=GSummary />
//                </DETAILS>
//              </DIV>
//            </DETAILS>
//          </DIV>
//        </DETAILS>
//     </DIV>


//................................................... add Tree Node .......
function addTreeNode(WrapperNodeId,NameSpace,parentName,NameOfDetailsList,
                     SummaryText,HasChildren,color,isopen:string;NodeHintFunc:TTreeNodeHint;Draggable:Boolean):string;
var
  newnodeid:String;
begin

asm
 try{

  //console.log('addTreeNode '+WrapperNodeId+' parentName='+parentName+' SummaryText='+SummaryText);

  var SystemNodeText='';
  if (Array.isArray(SummaryText)){
    SystemNodeText=SummaryText[0];
    }
  else {
    SystemNodeText=SummaryText;
    }
 //console.log('addTreeNode '+WrapperNodeId+' parentName='+parentName+' nodetext='+SystemNodeText);

  var myHint='';
  if (NodeHintFunc!=null) {
     myHint = NodeHintFunc(SystemNodeText);
     //alert('Hint '+myHint);
     }

//if (NameSpace!='') {alert('NS='+NameSpace+' addTreeNode '+NameOfDetailsList+' to '+parentName);}
  var parent = (document.getElementById(parentName));
  if (parent==null) {alert('addTreeNode cannot find parent '+parentName);}
  else
  {
  var div = document.createElement("div");
  div.style.width  = parent.width;
  div.style.marginLeft = "25px";
  div.id = NameOfDetailsList+'OuterDiv';

  var dragstartstring='event.stopPropagation(); pas.XTree.DragStart("'+WrapperNodeId+'","'+NameSpace+'","'+NameOfDetailsList+'" ,"'+SummaryText+'"); ';
  var dragOverEventString='if (pas.XTree.HandleTreeNodeDragOver(event.target,"'+WrapperNodeId+'","'+NameSpace+'","'+SummaryText+'")==true) {event.preventDefault();};';
  var dropEventString='event.stopPropagation(); pas.XTree.HandleTreeNodeDrop(event.target,"'+WrapperNodeId+'","'+NameSpace+'","'+SummaryText+'"); ';
  var dragEventString =  'draggable="true" '
    +" ondragstart = '"+dragstartstring+"' "
    +" ondragover = '"+dragOverEventString+"' "
    +" ondrop =  '"+dropEventString+"' ";

  var ClickEventString = 'event.stopPropagation(); '+
                         'pas.XTree.HandleTreeNodeClick("'+WrapperNodeId+'","'+NameSpace+'","'+NameOfDetailsList+'Summary"); '+
                         'pas.Events.handleEvent(null, "TreeNodeClick" ,"'+NameOfDetailsList+'","'+NameSpace+'" ,"'+NameOfDetailsList+'"); '+
                         '';
  if (HasChildren==true){
    if(isopen==true){
      div.innerHTML = "<details open id="+NameOfDetailsList+"><summary id="+NameOfDetailsList+"Summary "
                      +dragEventString
                      +"  onclick ='"+ClickEventString
                      +"' class='widgetinner "+NameSpace+WrapperNodeId+" hasChildren ' style='background-color:"+color+";'>"+SummaryText+"</summary></details>";}
      else {
      div.innerHTML = "<details  id="+NameOfDetailsList+"><summary id="+NameOfDetailsList+"Summary "
                      +dragEventString
                      +" onclick ='" +ClickEventString
                      +"' class='widgetinner "+NameSpace+WrapperNodeId+" hasChildren ' style='background-color:"+color+";'>"+SummaryText+"</summary></details>";
    }
  }
  else {
    div.innerHTML = "<details id = "+NameOfDetailsList+" ><summary id="+NameOfDetailsList+"Summary "
                     +dragEventString
                     +" onclick ='"+ClickEventString
                     +"'  class='widgetinner "+NameSpace+WrapperNodeId+" noChildren' style='background-color:"+color+";' >"+SummaryText+"</summary></details>";
  }
   div.title = myHint;

   parent.appendChild(div);

   newnodeid = NameOfDetailsList;
  }
 }catch(err) {alert('Error in XTree.addTreeNode '+ err.message);}
end;

result:=newnodeid;

end;

procedure clearTreeNode(parentName:string);
begin
asm
try {
 parent = (document.getElementById(parentName));
 if (parent!=null) {
   //alert('clearTreeNode '+parentName);
   parent.innerHTML = '';}
}catch(err){alert('Error in XTree.clearTreeNode '+ err.message);}
end;
end;

function addnode(WrapperNodeId,NameSpace,ParentName,currentNodeTree,IdOfNodeBeingAdded:string;
                 OpenToLevel,level:integer;normalColor:string;NodeHintFunc:TTreeNodeHint;Draggable:Boolean):string;
var
  newnodeid:String;
begin

asm
   try {
       //console.log('addnode: '+WrapperNodeId+' parentname:'+ParentName+'  newnode='+IdOfNodeBeingAdded);
       var isopen=false;
       if (level<OpenToLevel) {isopen=true;}  //by default just show the first n levels of the tree
       var localcurrentNodeTree=currentNodeTree;

       if ((localcurrentNodeTree.length>1)&&(Array.isArray(localcurrentNodeTree))){
       // This node has children so create the parent node
         newnodeid=pas.XTree.addTreeNode(WrapperNodeId,NameSpace,ParentName,IdOfNodeBeingAdded,localcurrentNodeTree[0],
                                      true,normalColor,isopen,NodeHintFunc,Draggable);

       // then make the recursive call for each of its children
       for (var i=1; i<localcurrentNodeTree.length; i++){
         var NameOfChildNode = IdOfNodeBeingAdded+'_'+(i-1)
         pas.XTree.addnode(WrapperNodeId,NameSpace,IdOfNodeBeingAdded,localcurrentNodeTree[i],
                           NameOfChildNode,OpenToLevel,level+1,normalColor,NodeHintFunc,Draggable);}
       }
       else {
       // This node does not have children so just create the node
       //if (currentNodeTree=='StyleRule') {alert('addTreeNode: '+WrapperNodeId+' '+ParentName+' '+localcurrentNodeTree+' newnode='+IdOfNodeBeingAdded);}
         newnodeid=pas.XTree.addTreeNode(WrapperNodeId,NameSpace,ParentName,IdOfNodeBeingAdded,localcurrentNodeTree,
                                      false,normalColor,true,NodeHintFunc,Draggable);
       //if (currentNodeTree=='StyleRule') {alert('node added');}
       }
    }catch(err){alert('Error in XTree.addnode '+ err.message);}
end;

result:=newnodeid;

end;

function GetPathToNode(TreeName,NameOfSelectedNode:string):string;
var containsSelectedNode:boolean;
  str:String;
begin

  asm
    //console.log('GetPathToNode '+NameOfSelectedNode+'. Tree='+TreeName);
    try{
    var thisNode=document.getElementById(NameOfSelectedNode);
    var parent=thisNode;

    do
    {
      var idx='';
      parent = parent.parentNode;     // should be a DETAILS element
      //console.log('parentid='+parent.id+' tag='+parent.tagName);
      if (parent.tagName=="DETAILS") {
        var list = parent.getElementsByTagName("SUMMARY");
        // should contain just one SUMMARY element
        if (list.length>0) {
          //console.log('SUMMARY found');
          var summ=list[0];
          var localDiv=parent.parentNode;
          var parentDiv=localDiv.parentNode;
          var DList = parentDiv.children;
          // find the sibling number of this summary element
          var i=0;
          var j=-1;
          do {
            // just count the DIV children up to this node (localDiv)
            if (DList[i].tagName=='DIV') {j=j+1;}
            if (DList[i]==localDiv) {
                idx=j+'~:~';
                i=DList.length;
              }
            i=i+1;
          }
          while (i<DList.length);

          if (str!="") {str=idx+summ.innerHTML+"~/~"+str;}
          else {str=idx+summ.innerHTML;}

        }
      }
    }
    while ((parent.id!=TreeName)&&(parent.id!=TreeName+"Contents")&&(thisNode.parentNode!=null)&&(thisNode.parentNode!=undefined))
    //console.log('final str='+str);
    }catch(err) { alert(err.message+'  in XTree.GetPathToNode'); }
  end;

  result:=  str;
end;

function SetOpenStatusOfNodeChildren(NodeName,NameOfSelectedNode:string;level:integer):boolean;
var containsSelectedNode:boolean;
begin
  //showmessage('SetOpenStatusOfNodeChildren');
  asm
    try{
      containsSelectedNode = false;
      if (NodeName!='') {
         var parentNode=document.getElementById(NodeName);
         if (parentNode != null){
         if (parentNode.children.length>0) {
            for (var i=0; i<parentNode.children.length; i++) {
              var TempContainsSelectedNode = pas.XTree.SetOpenStatusOfNodeChildren( parentNode.children[i].id,NameOfSelectedNode,level+1);
              if (TempContainsSelectedNode==true){containsSelectedNode = true};
              if ( parentNode.children[i].id== NameOfSelectedNode ){containsSelectedNode = true};
            }
            var test = parentNode.getAttributeNode("open");
            if ((containsSelectedNode == true)||(level < 1) )
              {parentNode.setAttribute("open", "") }
         } }
      }
    }catch(err) { alert(err.message+'  in XTree.SetOpenStatusOfNodeChildren'); }
  end;

  result:=  containsSelectedNode;
end;

function GetTreeRootID(NodeID:String):String;
begin
  asm
    try{
    //alert('GetTreeRootID. NodeID='+NodeID);
        var TreeRootID = "";
        var instring = pas.StringUtils.TrimWhiteSpace(NodeID);
        var EndOfTreeRootID = pas.StringUtils.FoundString(instring,"Node" );
        for (var i=0; i<EndOfTreeRootID-1; i++){ TreeRootID = TreeRootID+instring[i];}
     }catch(err) { alert(err.message+'  in XTree.GetTreeRootID'); }
     return TreeRootID;
  end;
end;

function OpenAndScrollToSelectedNode(NameOfDetailsList:string):string;
begin
  asm
    try{
    //  var TreeRootID=pas.XTree.GetTreeRootID(NameOfDetailsList)+'ContentsScroll'; // the tree nodes container
      var TreeRootID=pas.XTree.GetTreeRootID(NameOfDetailsList)+'Node'; // the tree nodes container
      var root = (document.getElementById(TreeRootID));
      pas.XTree.SetOpenStatusOfNodeChildren(root.id,NameOfDetailsList,0);
      var SelectedNode=document.getElementById(NameOfDetailsList);
      var AlignToTop = false;
      SelectedNode.scrollIntoView(AlignToTop);
    }catch(err) { alert(err.message+'  in XTree.OpenAndScrollToSelectedNode'); }
  end;
end;



function deselectNodeChildren(NodeName,normalColor:string):string;
begin
  asm
    try{
      if (NodeName!='') {
      var parentNode=document.getElementById(NodeName);
      parentNode.style.background=normalColor;

      if (parentNode.children.length>0) {
        for (var i=0; i<parentNode.children.length; i++) {
          pas.XTree.deselectNodeChildren( parentNode.children[i].id,normalColor);
        }
      }
      }
    }catch(err) { alert(err.message+'  in XTree.deselectNodeChildren'); }
  end;
end;

procedure SetDraggableAttribute(NodeName,draggable:string);
begin
  asm
    try{
      if (NodeName!='') {
      var parentNode=document.getElementById(NodeName);
      var d = draggable.toLowerCase();
      if (parentNode.tagName=='SUMMARY') {parentNode.setAttribute('draggable',d);}

      if (parentNode.children.length>0) {
        for (var i=0; i<parentNode.children.length; i++) {
          pas.XTree.SetDraggableAttribute( parentNode.children[i].id,d);
        }
      }
      }
    }catch(err) { alert(err.message+'  in XTree.SetDraggableAttribute'); }
  end;
end;

function clearNodeSelectedMarker(NameOfDetailsList,normalColor:string):string;
begin
  asm
    try{
//      alert('clearNodeSelectedMarker. NameOfDetailsList='+NameOfDetailsList+' normalColor='+normalColor);
      // go down the tree from the root clearing the selected colour

      var TreeRootID=pas.XTree.GetTreeRootID(NameOfDetailsList)+'Node';  // the tree nodes container
      //alert('tree root id='+TreeRootID);
      var root = (document.getElementById(TreeRootID));
      if (root==null) {
        alert('root is null ('+TreeRootID+')')}
      else {
        pas.XTree.deselectNodeChildren(root.id,normalColor); }

    }catch(err) { alert(err.message+'  in XTree.clearNodeSelectedMarker'); }
  end;
end;

function NodeIdFromText(TreeNode:TDataNode;NodeText:String):String;      //!!!!namespace???
var
   TreeObject:TObject;
   FoundId:String;
begin
  TreeObject:=TreeNode;
  asm
  //alert('NodeIdFromText.  NodeName='+TreeNode.NameSpace+'.'+TreeNode.NodeName+' Text='+NodeText);
  var ob = document.getElementById(TreeObject.NameSpace+TreeObject.NodeName+'Node');  // the tree nodes container
    if (ob!=null) {
            function checkChildren(obj) {
               for (var i=0; i<obj.children.length; i++) {
                 if (obj.children[i].innerHTML==NodeText) {
                   //alert('found '+NodeText+' at '+ obj.children[i].id);   //Summary level
                   //return obj.id;                                         //id is name minus 'Summary' suffix
                   return obj.children[i].id;                               //id is name including 'Summary' suffix
                 }
                 else {
                   var tmp=checkChildren(obj.children[i]);
                   if (tmp!='') {return tmp;}
                 }
               }
               return '';
            }
      //alert('top text is '+ ob.innerHTML);
      if (ob.innerHTML==NodeText) {
        //alert('top found '+NodeText+' at '+ ob.id);
        FoundId=ob.id;
      }
      else {
        FoundId=checkChildren(ob);
      }
    }
  end;
  result:=FoundId;
end;

function NodeTextFromId(TreeNode:TDataNode;NodeId:String):String;
var
   TreeObject:TObject;
   FoundText:String;
begin
  TreeObject:=TreeNode;
  asm
 // var ob = document.getElementById(TreeObject.NameSpace+TreeObject.NodeName+'ContentsScroll');  // the tree nodes container
  var ob = document.getElementById(TreeObject.NameSpace+TreeObject.NodeName+'Node');  // the tree nodes container
    if (ob!=null) {
       var node = document.getElementById(NodeId);
      // NB. the node object should be a <summary> object, where innerHTML is simply the visible node text.
      //alert('node '+NodeId+' text is '+ node.innerHTML);
      FoundText=node.innerHTML;
    }
  end;
  result:=FoundText;
end;


constructor TXTree.Create(MyForm:TForm;NodeName,NameSpace:String);
begin
  inherited Create(NodeName,NameSpace);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=false;

  SetNodePropDefaults(self,myDefaultAttribs);
end;


function CreateWidget(MyNode,
                 ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NodeTree,SelectedNodeString,LabelText,LabelPos,normalColor:string;
  Ht,Wd:String;
  NodeName:string;
  NodeHintFunc:TTreeNodeHint;
  openlvl:integer;
begin
  //showmessage('TXTree CreateWidget '+NameSpace+'.'+ScreenObjectName);

  NodeTree:=MyNode.GetAttribute('TreeData',true).AttribValue;
  if NodeTree='' then NodeTree:=ExampleNodeTree;
  //SelectedNodeString:=MyNode.GetAttribute('SelectedNodeText',true).AttribValue;
  LabelText:= MyNode.getAttribute('LabelText',true).AttribValue;
  LabelPos:= UpperCase(MyNode.getAttribute('LabelPos',true).AttribValue);
  openlvl:= StrToInt(MyNode.getAttribute('OpenToLevel',true).AttribValue);
  normalColor:= MyNode.getAttribute('BgColor',true).AttribValue;
  Wd:=' width:100%; ';
  Ht:=' height:100%; ';
  NodeName:=MyNode.NodeName;

  NodeHintFunc:=TXTree(MyNode).TreeNodeHintFunc;
  //if NodeHintFunc<>nil then showmessage('found node hint func for tree '+ScreenObjectname);

  asm
    try{
    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,NameSpace,$impl.MyNodeType,position);
    var wrapperid = NameSpace+ScreenObjectName;
    var wrapper=document.getElementById(wrapperid);

    var MyObjectName=wrapperid+'Contents';
    var localcontainer = document.createElement("div");
    localcontainer.id = MyObjectName;
    localcontainer.style.display="inline-block;";
    localcontainer.style.height="100%";
    localcontainer.style.width="100%";
    wrapper.appendChild(localcontainer);

    pas.XTree.addTreeStyles();// this style script removes the arrowhead on all nodes that do not have children


    // put in a scrollbox to contain all the tree nodes
    //console.log('tree container id='+wrapperid+'Node');
    var ContainerString = '<div style = "overflow:scroll; display:inline-block;'+
                                'background-color:'+normalColor+';'+Ht+Wd+'" '+
                                'id='+wrapperid+'Node class="'+NameSpace+ScreenObjectName+'"></div> ';
    localcontainer.insertAdjacentHTML( 'beforeend', ContainerString );

    var TreeName = wrapperid+'Node';
    var localNodeTree =JSON.parse(NodeTree);
    //console.log('NodeName='+NodeName+' Treename='+TreeName);

    pas.XTree.addnode(NodeName,NameSpace,TreeName,localNodeTree,(TreeName+'0'),
                      openlvl,0,normalColor,NodeHintFunc,true) ;


    var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'">'+LabelText+'</label>';
    if (LabelPos=='LEFT') {
      wrapper.insertAdjacentHTML( 'afterbegin', labelstring );
    }
    else {
      wrapper.insertAdjacentHTML( 'beforeend', labelstring );
      }

    }
    catch(err) { alert(err.message+'  in XTree.CreateWidget');}

  end;

  MyNode.ScreenObject:=MyNode;

  RefreshComponentProps(myNode);

  //showmessage('TXTree CreateWidget Done');
  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXTree.Create(MyForm,NodeName,NameSpace));
end;

procedure HandleTreeNodeClick(WrapperNodeId,NameSpace,SelectedNodeObjectId:String);
var
  myNode:TDataNode;
begin
   myNode:=FindDataNodeById(SystemNodetree,WrapperNodeId,NameSpace,true);
   TXTree(myNode).SelectedNodeId:=SelectedNodeObjectId;
end;

procedure TXTree.SelectTreeNodeById(NodeId:string);
var
   normalColor:string;
   ParentId:String;    // id minus the Summary suffix
   CurrentSelection:String;
begin
  //showmessage('SelectTreeNodeById '+NodeId);
   CurrentSelection:=self.SelectedNodeText;

   normalColor:=self.BgColor;
   ParentId:=myStringReplace(NodeId,'Summary','',-1,-1);
   //showmessage('parentid='+ParentId);
   asm
   try{
   //alert('SelectTreeNodeById NodeId='+ NodeId+' TreeName='+this.NodeName);
   if (NodeId!='') {
     // NodeId is the name (id) of a tree node
     var myself = document.getElementById(ParentId);
     if (myself!=null)
     {
     var HTMLString = myself.innerHTML;
     var hasChildren = pas.StringUtils.FoundString(HTMLString,"hasChildren");

     // go down the tree from the root clearing the selected colour
     pas.XTree.clearNodeSelectedMarker(ParentId,normalColor);  // clear old selected nodes if selecting a new one

     pas.XTree.OpenAndScrollToSelectedNode(ParentId);

     //alert('SelectTreeNodeById 1. looking for '+NodeId);
     // Highlight this selected node in yellow
     var mySummary=document.getElementById(NodeId);
     mySummary.style.background=pas.XTree.TreeNodeHighlightColor;

     if (hasChildren>0)   // toggle the open attribute
     {
         //alert('has children');
         var test = myself.getAttributeNode("open");
         //alert('test='+test);
         if (test == null){
            myself.setAttribute("open", "");
            } else {
            myself.removeAttribute("open");}
      }

      //**** have to do this to force the document to repaint this element (eg if node has been opened)
       HTMLString = myself.innerHTML;
       myself.innerHTML = HTMLString;
       }
     }
     else {
       // go down the tree from the root clearing the selected colour
       pas.XTree.deselectNodeChildren(this.NodeName+'Node',normalColor);
     }
     }catch(err) { alert(err.message+'  in XTree.SelectTreeNodeById'); }
   end;

end;

function TXTree.NodeTextIsUnique(NodeText:String):Boolean;
var
   FoundId:String;
begin
  asm
 //   var ob = document.getElementById(this.NameSpace+this.NodeName+'ContentsScroll');  // the tree nodes container
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Node');  // the tree nodes container
    if (ob!=null) {
            function checkChildren(obj) {
               for (var i=0; i<obj.children.length; i++) {
                 if (obj.children[i].innerHTML==NodeText) {
                   //alert('found '+NodeText+' at '+ obj.children[i].id);   //Summary level
                   //return obj.id;                                         //id is name minus 'Summary' suffix
                   return obj.children[i].id;                                         //id is name including 'Summary' suffix
                 }
                 else {
                   var tmp=checkChildren(obj.children[i]);
                   if (tmp!='') {return tmp;}
                 }
               }
               return '';
            }

      if (ob.innerHTML==NodeText) {
         FoundId=ob.id;
      }
      else {
        FoundId=checkChildren(ob);
      }
    }
  end;
  if FoundId <> '' then
    result:=false
  else
    result:=true;
end;

function TXTree.GetChildNodes(nodeId:String):TStringArray;
var
  nodes:TStringArray;
begin
  setLength(nodes,0);
  asm
    //alert('GetChildNodes for '+nodeId);
    try {
    var ob = document.getElementById(nodeId);  //nodeId should be a 'DETAILS' node
    if (ob!=null) {
            function fetchChildren(obj) {
              //alert('GetChildNodes.fetchChildren of '+obj.id+' tag='+obj.tagName);

              if ((obj.tagName!='DETAILS')) {return;}

              // DETAILS tag...
               if (obj.children.length==0) {
                 return;
               }
               else
               {
               var hasChildNodes = ((obj.tagName=='DETAILS')&&(pas.HTMLUtils.ContainsChildWithTag(obj,'DIV')));
               if (hasChildNodes) {
                 for (var i=0; i<obj.children.length; i++) {
                   //alert('found child '+obj.children[i].id);
                     if (obj.children[i].tagName=='DIV') {
                       //alert('child is div.');
                       var childDiv=obj.children[i];
                       var dtl = childDiv.getElementsByTagName("DETAILS");
                       nodes.push(dtl[0].id);
                       }
                   }
               }
              }
            }
    fetchChildren(ob);
    }
    else
      alert('GetChildNodes. '+nodeId+' not found');

    }catch(err) {alert('Error in XTree.GetChildNodes '+ err.message);}
  end;

  result:=nodes;
end;

function TXTree.NumChildNodes(nodeid:String):integer;
var
  nodes:TStringArray;
  num:integer;
begin
  nodes:=self.GetChildNodes(nodeId);
  num:=length(nodes);
  result:=num;
end;

function TXTree.GetParentOfNode(nodeid:String):String;
var
  ParentId:String;
begin
asm
//alert('GetParentOfNode '+nodeid);
  var ob=document.getElementById(nodeid);
  // expected to be a SUMMARY element
  // parent will be a DETAILS element
  // parent will be a DIV element
  // parent will be the actual node parent - another DETAILS element
  if (ob.tagName=='SUMMARY') {ob=ob.parentNode;}
  if (ob.tagName=='DETAILS') {
    ob=ob.parentNode;
    if (ob.tagName=='DIV') {ob=ob.parentNode;}
    ParentId=ob.id;
  }
end;
  result:=ParentId;
end;


function TXTree.TextOfNode(nodeId:String):String;
var
  str:string;
begin
  asm
    //alert('GetChildNodes for '+nodeId);
    try {
    var ob = document.getElementById(nodeId);  //nodeId should be a 'DETAILS' node
    if (ob!=null) {
      var summNode=ob.children[0];
      str=summNode.innerHTML;
    }
    }catch(err) {alert('Error in XTree.GetChildNodes '+ err.message);}
  end;
  result:=str;
end;

function TXTree.BuildTreeDataString:String;
var
  str:String;
begin
  str:='';
  asm
    //alert('treedata for '+this.NodeName);
 //   var ob = document.getElementById(this.NameSpace+this.NodeName+'ContentsScroll');  // the tree nodes top container
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Node');  // the tree nodes top container
    if (ob!=null) {
            function fetchChildren(obj,str) {
              //alert('fetchChildren of '+obj.id+' tag='+obj.tagName);
              if (obj.tagName=='SUMMARY') {return str + '"'+obj.innerHTML+'"';}
              if ((obj.tagName!='DETAILS')&&(obj.tagName!='DIV')) {return str;}

              // DETAILS or DIV tag...
               if (obj.children.length==0) {
                 return str;
               }
               else
               {
               var hasChildNodes = ((obj.tagName=='DETAILS')&&(pas.HTMLUtils.ContainsChildWithTag(obj,'DIV')));
               if (hasChildNodes) {str=str+'[';}
               for (var i=0; i<obj.children.length; i++) {
                  if (i>0) {str = str + ','; }
                  str=fetchChildren(obj.children[i],str);
                 }
               if (hasChildNodes) {str=str+']';}
               }
               return str;
            }
    str=fetchChildren(ob,str);
    }
    else
      alert('ob not found');
  end;
  //showmessage(str);
  result:=str;
end;

procedure TXTree.SetTreeWidth(AValue:string);
begin
  //showmessage('tree width='+AValue);
  myNode.SetAttributeValue('TreeWidth',AValue);
  asm
  var ob = document.getElementById(this.NameSpace+this.NodeName);
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
  end;
end;

procedure TXTree.SetTreeHeight(AValue:string);
begin
  myNode.SetAttributeValue('TreeHeight',AValue);
  asm
  var ob = document.getElementById(this.NameSpace+this.NodeName);
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'H',AValue);
  end;
end;

procedure TXTree.SetNodeText(nodeid,newtext:String);
begin
  asm
  var ob = document.getElementById(nodeid);     // should be a SUMMARY element
  ob.innerHTML=newtext;
  end;
  self.TreeData:=self.buildTreeDataString;
end;

{$endif}

procedure TXTree.SetSelectedNodeId(AValue:string);
var
  nodeId:String;
  nodeText,pth:String;
  {$ifndef JScript}
  ANode:TTreeNode;
  {$endif}
begin
  nodeText:='';
  nodeId:=AValue;
  if nodeId<>'' then
  begin
    {$ifndef JScript}
    ANode:=TmyTreeView(self.myControl).FindNodeById(nodeId);
    nodeText:=ANode.Text;
    {$else}
    // make sure the id is a SUMMARY element.
    // (might send in a DETAILS id.  SUMMARY is first child of DETAILS)
    asm
    //console.log('SetSelectedNodeId '+AValue);
    var ob=document.getElementById(nodeId);
    if (ob!=null) {
    if (ob.tagName=='DETAILS') {
      ob=ob.getElementsByTagName('SUMMARY')[0];
      nodeId=ob.id;
      }
    nodeText=ob.innerHTML;
    }
    else {alert('cannot find node '+nodeId);}

    end;
    {$endif}
  end;

  fSelectedNodeId:=nodeId;
  myNode.SetAttributeValue('SelectedNodeText',nodeText);

  pth:=self.SelectedTextPath;   // this sets the value for attribute 'SelectedTextPath'

  self.SelectTreeNodeById(nodeId);
end;

procedure TXTree.DeSelectNode;
var
   normalColor:string;
begin
  {$ifndef JScript}
  TTreeView(myControl).Selected:=nil;
  {$else}
  normalColor:=self.BgColor;
  deselectNodeChildren(self.NameSpace+self.NodeName,normalColor);
  {$endif}
end;


function TXTree.InsertNewSiblingNode(NodeText:String):String;
// insert new sibling with the selected node
{$ifndef JScript}
var
  myTree:TmyTreeView;
begin
  result:='';
  myTree:=TmyTreeView(self.myControl);
  if myTree.Selected<>nil then
  begin
    if myTree.Selected<>myTree.Items[0] then
    begin
      myTree.Selected:=myTree.Items.Add(myTree.Selected,NodeText);    //add new node as sibling
      self.TreeData:=self.buildTreeDataString;
    end
    else
      showmessage('There should only be one root node - please insert as child');
  end
  else
    showmessage('Please select a tree node first');
{$else}
var
  localSelectedNodeId,tmp:String;
  localDraggable:Boolean;
  newnodeid:String;
begin
  localSelectedNodeId :=self.SelectedNodeId;
  localDraggable:=self.Draggable;
  result:='';
  if localSelectedNodeId<>'' then
  begin
    //showmessage('InsertNewSiblingNode.  SelectedNodeId='+localSelectedNodeId);
    asm
    //alert('WrapperNodeId='+this.NodeName+' localSelectedNodeId='+localSelectedNodeId+' this.SelectedNodeId='+this.SelectedNodeId);  //!!!! why is this.SelectedNodeId undefined??
      var ob = document.getElementById(localSelectedNodeId);
      // find parent of selected node
      var parentnode=pas.HTMLUtils.getParentByTagName(this.NameSpace+this.NodeName,ob.parentNode,'details');
      if (parentnode!=null) {
         //alert('found parent '+parentnode.id+' (tag is '+parentnode.tagName+')');
         var idx=this.NumChildNodes(parentnode.id);
         newnodeid=pas.XTree.addnode(this.NodeName,this.NameSpace,parentnode.id,NodeText,
                                       (parentnode.id+'_'+(idx)),           //!! need the same id that will be produced when tree is rebuilt
                                       1,0,this.BgColor,null,localDraggable);
      }
        else {alert('parentnode not found');}
    end;
    self.TreeData:=self.buildTreeDataString;
  end
  else
     showmessage('Please select a tree node first');

  result:=newnodeid;
{$endif}
end;

function TXTree.InsertNewChildNode(NodeText:String):String;
{$ifndef JScript}
var
  myTree:TmyTreeView;
begin
  result:='';
  myTree:=TmyTreeView(self.myControl);
  if myTree.Selected<>nil then
  begin
    myTree.Selected:=myTree.Items.AddChild(myTree.Selected,NodeText);    //add new node as child
    result:=NodeText;//!!!! need a unique nodeid here
    self.TreeData:=self.buildTreeDataString;
  end
  else
    showmessage('Please select a tree node first');
{$else}
var
  localSelectedNodeId:String;
  localDraggable:Boolean;
  newnodeid:String;
begin
  localSelectedNodeId :=self.SelectedNodeId;
  localDraggable:=self.Draggable;
  newnodeid:='';
  if localSelectedNodeId<>'' then
  begin
    //if NodeText='StyleRule' then showmessage('InsertNewChildNode.  NodeText='+NodeText+' SelectedNodeId='+localSelectedNodeId);
    asm
      //if (NodeText=='StyleRule') {alert('WrapperNodeId='+this.NodeName+' localSelectedNodeId='+localSelectedNodeId);}
      var ob = document.getElementById(localSelectedNodeId);
      if (ob!=null) {
        if (ob.tagName=='SUMMARY') {ob = ob.parentNode;}  // ob should be the DETAILS element for this node.
        // find existing child nodes
        var idx=this.NumChildNodes(ob.id);
        //if (NodeText=='StyleRule') {alert('adding under parent '+ob.id+' (tag is '+ob.tagName+')');}
        newnodeid=pas.XTree.addnode(this.NodeName,this.NameSpace,ob.id,NodeText,
                                    (ob.id+'_'+(idx)),                        //!! need the same id that will be produced when tree is rebuilt
                                    1,0,this.BgColor,null,localDraggable);
        ob=document.getElementById(newnodeid);
        if (ob.tagName=='SUMMARY') {ob = ob.parentNode;}  // ob should be the DETAILS element for this node.
        newnodeid=ob.id;
      }
      else {alert('node '+localSelectedNodeId+' not found'); }
    end;
    self.TreeData:=self.buildTreeDataString;
  end
  else
     showmessage('Please select a tree node first');
  result:=newnodeid;    //return the id of the new DETAILS element
{$endif}
end;

procedure TXTree.DeleteSelectedNode;
{$ifndef JScript}
var
  myTree:TmyTreeView;
  tmp:String;
begin
myTree:=TmyTreeView(self.myControl);
if myTree.Selected<>nil then
begin
  if myTree.Selected<>myTree.Items[0] then
  begin
    myTree.Items.Delete(myTree.Selected);
    self.TreeData:=self.buildTreeDataString;
  end
  else
    showmessage('DeleteSelectedNode: Cannot delete the root node');
end
else
  showmessage('DeleteSelectedNode: Please select a tree node first');
{$else}
var
  localSelectedNodeId:String;
begin
  localSelectedNodeId :=self.SelectedNodeId;
  if localSelectedNodeId<>'' then
  begin
    asm
      var ob = document.getElementById(localSelectedNodeId);
      // delete the HTML node that is the DIV parent of the selected node
      var divp = pas.HTMLUtils.getParentByTagName(this.NameSpace+this.NodeName,ob,'div');
      divp.remove();
    end;
    self.TreeData:=self.buildTreeDataString;
  end
  else
    showmessage('DeleteSelectedNode: Please select a tree node first');
{$endif}
end;

procedure TXTree.MoveNode(SourceNodeText,DestNodeText:String);     //!!!! do this with nodeId...
var
  {$ifndef JScript}
  Src,dst,parentNode,newNode:TTreeNode;
  myTree:TMyTreeView;
  {$endif}
  str:String;
begin
  {$ifndef JScript}
  myTree:=TmyTreeView(self.myControl);
  src:=myTree.FindNodeByText(SourceNodeText);
  dst:=myTree.FindNodeByText(DestNodeText);
  //showmessage('src='+src.Text+' dst='+dst.Text+' ins='+myTree.InsertMarkNode.Text);
  // move the source node to new destination
  if (myTree.InsertMarkType = tvimAsNextSibling)
  or (myTree.InsertMarkType = tvimAsPrevSibling) then
    parentNode:=dst.Parent
  else if (myTree.InsertMarkType = tvimAsFirstChild) then
    parentnode:=dst
  else
    parentNode:=dst.Parent;
  myTree.InsertMarkType:=tvimNone;

  newNode:=myTree.AddNewNode(myTree.Items,parentnode,Src.Text,self.myNode.NodeName+'Node');
  myTree.Items.Delete(Src);

{$else}
  self.SelectedNodeText:=SourceNodeText;
  self.DeleteSelectedNode;
  self.SelectedNodeText:=DestNodeText;
  self.InsertNewSiblingNode(SourceNodeText);
{$endif}
  self.SelectedNodeText:=SourceNodeText;
  str:=self.BuildTreeDataString;
  self.TreeData:=str;
end;

function TXTree.GetTreeData:string;
begin
  result:=MyNode.getAttribute('TreeData',true).AttribValue;
end;
function TXTree.GetSelectedNodeText:string;
begin
  result:=MyNode.getAttribute('SelectedNodeText',true).AttribValue;
end;
function TXTree.GetTreeHeight:string;
begin
  result:=MyNode.getAttribute('TreeHeight',true).AttribValue;
end;
function TXTree.GetTreeWidth:string;
begin
  result:=MyNode.getAttribute('TreeWidth',true).AttribValue;
end;
function TXTree.GetReadOnly:Boolean;
begin
  result:=MyStrToBool(MyNode.getAttribute('ReadOnly',true).AttribValue);
end;
function TXTree.GetDraggable:Boolean;
begin
  result:=MyStrToBool(MyNode.getAttribute('Draggable',true).AttribValue);
end;
function TXTree.GetOpenToLevel:Integer;
var
  str:string;
begin
  str:=MyNode.getAttribute('OpenToLevel',true).AttribValue;
  if str<>'' then
    result:=StrToInt(str)
  else
    result:=0;
end;
function TXTree.GetSelectedTextPath:String;
var
  pth:string;
  localSelectedNodeId,tmp:String;
begin
  pth:='';
  {$ifndef JScript}
  if TmyTreeView(myControl).Selected<>nil then
  begin
    //pth:=TmyTreeView(myControl).Selected.GetTextPath;
    pth:=TmyTreeView(myControl).BuildExpandedText(TmyTreeView(myControl).Selected);
  end;
  {$else}
    // build the text path from the defined nodes
    localSelectedNodeId :=self.SelectedNodeId;
    //asm console.log('TXTree.GetSelectedTextPath localSelectedNodeId='+localSelectedNodeId); end;
    if localSelectedNodeId<>'' then
    begin
      pth:=GetPathToNode(self.NodeName,localSelectedNodeId);
    end
    else
      pth:='';
    //asm console.log('pth='+pth); end;
  {$endif}

  myNode.setAttributeValue('SelectedTextPath',pth,'String');
  result:=pth;
end;

procedure TXTree.SetTreeData(NodeTreeString:String);
var
  SelectedId:string;
  myNodeName,myNameSpace:String;
  NodeHintFunc:TTreeNodeHint;
  localDraggable:Boolean;
  openlvl:integer;
begin
  myNode.setAttributeValue('TreeData',NodeTreeString,'TreeString');
  openlvl:=self.OpenToLevel;
  {$ifndef JScript}
  TmyTreeView(myControl).PopulateMeFromJSONData(NodeTreeString,myNode.NodeName+'Node',openlvl);
  {$else}
    if trim(NodeTreeString) = '' then
      NodeTreeString:=ExampleNodeTree;
    localDraggable:=self.Draggable;
    selectedId:=self.SelectedNodeId;
    myNodeName:=self.NodeName;
    myNameSpace:=self.NameSpace;
 //   clearTreeNode(myNameSpace+myNodeName+'ContentsScroll');
    clearTreeNode(myNameSpace+myNodeName+'Node');
    NodeHintFunc:=TXTree(MyNode).TreeNodeHintFunc;
    asm
    try {
    // fudge... (problem with newline embedded)....
    var CleanValue = '';
    for ( var c = 0; c < NodeTreeString.length; c++ ) {
      if (NodeTreeString.charCodeAt(c) != 10) {CleanValue = CleanValue + NodeTreeString[c];}
      else {CleanValue = CleanValue + '\\n';}
      }


    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
      //var TreeName = myNameSpace+myNodeName+'ContentsScroll';
      var ParentName = myNameSpace+myNodeName+'Node';
      var localNodeTree =JSON.parse(CleanValue);
      //alert('localNodeTree ='+localNodeTree);
      //alert('SetTreeData '+TreeName+' Namespace='+myNameSpace);
      pas.XTree.addnode(myNodeName,myNameSpace,ParentName,localNodeTree,
                        ParentName+'0',
                        openlvl,0,this.BgColor,NodeHintFunc,localDraggable) ;
    }
    }
    catch(err) {  alert("Error in TXTree.SetTreeData: "+ err.message); };

    end;
    self.HTMLClasses:=self.HTMLClasses;
  {$endif}
  self.SelectedNodeText:='';
end;

procedure TXTree.SetReadOnly(AValue:Boolean);
begin
  myNode.SetAttributeValue('ReadOnly',myBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  TmyTreeView(myControl).ReadOnly:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
      ob.readOnly = AValue  }
  end;
  {$endif}
end;

procedure TXTree.SetDraggable(AValue:Boolean);
begin
  myNode.SetAttributeValue('Draggable',myBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  if AValue=true then
    TmyTreeView(myControl).DragMode:=dmAutomatic
  else
    TmyTreeView(myControl).DragMode:=dmManual;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
      //draggable attribute must be set for all the tree nodes (with summary tag)
      pas.XTree.SetDraggableAttribute(ob.id, AValue.toString());
      }
  end;
  {$endif}
end;

procedure TXTree.SetOpenToLevel(AValue:Integer);
var
  oldval:integer;
begin
  oldval:=self.OpenToLevel;
  myNode.SetAttributeValue('OpenToLevel',IntToStr(AValue),'Integer');
  {$ifndef JScript}
  TMyTreeView(self.myControl).ExpandTreeNodes(AValue);
  {$else}
  // takes effect next time the tree is rebuilt   ?!!!!
  if oldval<>AValue then
  begin
    self.TreeData:=self.TreeData;
  end;
  {$endif}
end;

procedure TXTree.SetSelectedNodeText(AValue:string);
var
  NodeId,pth:String;
begin
  if AValue<>self.SelectedNodeText then
  begin
    if AValue<>'' then
    begin
      {$ifndef JScript}
      myNode.SetAttributeValue('SelectedNodeText',AValue);
      self.SelectTreeNodeByText(AValue);                   // selects node with this text....!!!! node texts must be unique
      {$else}
      //asm console.log('SetSelectedNodeText '+AValue); end;
      NodeId:=NodeIdFromText(self,AValue);
      self.SelectedNodeId:=NodeId;
      pth:=self.SelectedTextPath;   // this sets the value for attribute 'SelectedTextPath'
      {$endif}
    end
    else
    begin
      myNode.SetAttributeValue('SelectedNodeText',AValue);
      {$ifndef JScript}
      self.SelectTreeNodeByText(AValue);                   // clears the current selection
      {$endif}
    end;
  end;
end;

procedure TXTree.SelectTreeNodeByPath(NodePath:string);
var
  TreeName,NodeId,pth:String;
  bits,bits2:TStringList;
  i:integer;
begin
  // path is delimited with embedded indices, same as format of self.SelectedTextPath.
  // This must be split into elements
  {$ifndef JScript}
  TMyTreeView(self.myControl).selectNodeByTextPath(NodePath);  // (Windows) this selects the node in the datatree component, if changed.
  {$else}
  // find the relevant div element in the HTML tree
  //asm console.log('SelectTreeNodeByPath Looking for nodepath '+NodePath);  end;
  TreeName:=self.NodeName;
  NodeId:=TreeName+'Node0';
  bits:=stringsplit(NodePath,'~/~',false);
  for i:=1 to bits.count-1 do      // skip the root node element
  begin
    bits2 := stringsplit(bits[i],'~:~',false);
    if bits2.count>1 then
    begin
      NodeId:=NodeId + '_' + bits2[0];
    end;
  end;
  //asm console.log('Looking for div id '+NodeId);  end;
  self.SelectedNodeId:=NodeId;
  pth:=self.SelectedTextPath;   // this sets the value for attribute 'SelectedTextPath'

  bits.Free;
  bits2.Free;
  {$endif}
end;

begin
  // this is the set of node attributes that each XTree instance will have.
  AddWrapperDefaultAttribs(myDefaultAttribs);
  AddDefaultAttribute(myDefaultAttribs,'TreeWidth','String','200','',false);
  AddDefaultAttribute(myDefaultAttribs,'TreeHeight','String','250','',false);
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','Top','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelText','String','Tree View','',false);
  AddDefaultAttribute(myDefaultAttribs,'BgColor','Color','#FFFFFF','',false);
  AddDefaultAttribute(myDefaultAttribs,'ReadOnly','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'Draggable','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'SelectedNodeText','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'SelectedTextPath','String','','',true,false);
  AddDefaultAttribute(myDefaultAttribs,'OpenToLevel','Integer','1','',false);
  AddDefaultAttribute(myDefaultAttribs,'TreeData','Integer',ExampleNodeTree,'',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);


  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  AddAttribOptions(MyNodeType,'LabelPos',LabelPosOptions);
  {$ifndef JScript}
  RegisterClass(TXTree);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
{$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}
  SuppressDesignerProperty('TXTree','BgColor');
  SuppressDesignerProperty(MyNodeType,'ContainerHeight');
  SuppressDesignerProperty(MyNodeType,'ContainerWidth');
end.
