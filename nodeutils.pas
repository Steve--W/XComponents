(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit NodeUtils;
interface
uses
  Classes,  SysUtils, StringUtils, TypInfo,
{$ifndef JScript}
  Forms,Controls, StdCtrls, ExtCtrls, Dialogs, Clipbrd, ProjectIntf, LazIDEIntf, LazLogger,
 // UtilsJSCompile,
  {$ifdef Chromium}
  uCEFApplication,
  {$endif}
{$else}
{$endif}
  EventsInterface
  ;

type TIntArray = Array of integer;
type TDataNode = class;  //forward
type TNodeAttribute = class;  //forward

type
TAttribSourceRec = record
    InputNodeName:String;
    InputNameSpace:String;
    InputAttribName:String;
    InputValue:String;
  end;
type TEventHandler = procedure(e:TEventStatus;nodeID:AnsiString;myValue:AnsiString) of object;
type TEventHandlerRec = class(TObject)
  public
  TheHandler:TEventHandler;   // for event handler functions that exist in the compiled project
  TheCode:String;             // for dynamically created XComponents with user-entered event code
  InitCode:String;            // for dynamically created XComponents with user-entered event code
  ReadOnlyInterface:Boolean; // for TXComposite components only.
  EventHint:String;          // for TXCompositeIntf components only.
end;
type TEventHandlers = Array of TEventHandlerRec;
type TGenericHandler = function(MyEventType,myValue:string;myNode:TDataNode):Boolean of object;
{$ifdef windows}
   {$ifdef Win64}
type   WinSizeDependentInt = int64;
   {$else}
type  WinSizeDependentInt = integer;
   {$endif}
{$else}
   {$ifdef CPU64}
type   WinSizeDependentInt = int64;
   {$else}
type  WinSizeDependentInt = integer;
   {$endif}
{$endif}

{$ifndef JScript}
type TAddComponentFunc = function(ParentNode:TDataNode;ObjectName,NameSpace:String;position:integer;Alignment:String): TDataNode;
{$else}
type TAddComponentFunc = function(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:String;position:integer;Alignment:String): TDataNode;

   // dummy 'TForm' object for Javascript widget creation
   type TForm = class(TObject)
         private
           fName:String;
         published
           property Name:String read fName write fName;
         end;
   type TColor = String;
{$endif}

 type TCreateInObFunc = function(MyForm:TForm;NodeName,NameSpace:String):TObject;

 type TNodeFuncsLookup = record
                         NodeType:String;
                         ScreenObjFunctionPtr:TAddComponentFunc;
                         InObFunctionPtr:TCreateInObFunc;
                      end;
 type TNodeFuncsTable = Array of TNodeFuncsLookup;

 type   TDefaultAttribute = Record
          AttribName:string;
          AttribType:string;
          AttribValue:string;
          AttribReadOnly:boolean;
          AttribHint:string;
          AttribIncludeInSave:Boolean;
      end;
 type  TDefaultAttributesArray = Array of TDefaultAttribute;
 type TDefaultAttribsByType = record
          NodeType:String;
          DefaultAttribs:TDefaultAttributesArray;
 end;
 type TDefaultAttribsTable = Array of TDefaultAttribsByType;

 type   TNodeAttribute = class(TObject)
   public
         AttribName:string;
         AttribType:string;
         AttribValue:string;
         AttribReadOnly:boolean;
         AttribSource:TAttribSourceRec;
         AttribHint:String;        // populated only for TXCompositeIntf nodes; otherwise hints come from the defaultattribs.

         OwnerNode:TDataNode;
         constructor Create(InNode:TDataNode; AttrName:String);
     end;
type  TNodeAttributesArray = Array of TNodeAttribute;

type TChildNodesArray = Array of  TDataNode;
type TNodesArray = Array of TDataNode;


type TComponentTag = class
       public
         HH,WW:string;
       end;


//----------------- TDataNode Definition ---------------------------------------
// The data nodes store the definition of the user interface, including the object inspectors,
// plus all available resources.
// The tree of data nodes is the data which is stored when a system is saved.

{$ifndef JScript}
type   TDataNode = class(TPersistent)
{$else}
type   TDataNode = class(TForm)         // <- TObject
{$endif}
       private
       public
          NodeType:String;
          NodeClass:String;
          NodeName:String;
          NameSpace:String;                              // non-blank for encapsulated components (name of top parent node of component)
          IsDynamic:Boolean;                             // true if created at runtime
          ScreenObject:TObject;                          // the actual visible component
          MyForm:TForm;                                  // the owning 'form' - object which contains event handlers

          NodeAttributes:TNodeAttributesArray;
          ChildNodes:TChildNodesArray;
          myEventTypes:TStringList;
          myEventHandlers:TEventHandlers;

          NodeParent:TDataNode;

          constructor Create(MyClass,MyName,MyNameSpace,MyType:string;NodeIsDynamic:Boolean=false);
          procedure DeleteMe;
          function HasAttribute(AttrName:string):Boolean;
          function GetAttribute(AttrName:string;AllowSpace:boolean):TNodeAttribute;
          function GetAttributeAnyCase(AttrName:string;AllowSpace:boolean):TNodeAttribute;   overload;
          function GetAttributeAnyCase(AttrName:string):TNodeAttribute;   overload;
          procedure AddAttribute(AttributeName,AttributeType,AttributeValue:string;AttributeReadOnly:boolean);
          procedure SetAttributeValue(AttrName:string;const NewValue,AttrType:string;AttribReadOnly:Boolean);  overload;
          procedure SetAttributeValue(AttrName:string;const NewValue,AttrType:string);  overload;
          procedure SetAttributeValue(AttrName:string;const NewValue:string); overload;
          procedure SetAttributeSource(AttrName:string;SourceNodeName,SourceNameSpace,SourceAttribName:string);
          function GetChildIndex(ChildNode:TDataNode):integer;
          procedure RemoveChildNode(ChildNode:TDataNode);
          function FindRegisteredEvent(EventType:string):TEventHandler;
          procedure RegisterEvent(EventType:String;TheHandler:TEventHandler);
          function HasUserEventCode(EventType:String):Boolean;
          function EventNum(EventType:String):integer;
          function GetEvent(EventType:String):TEventHandlerRec;
          function GetEventCode(EventType:String):String;
          function GetEventInitCode(EventType:String):String;
          function GetEventROI(EventType:String):Boolean;
          procedure InitialiseEventHandlers;
          procedure AddEvent(EventType,MainCode,InitCode:String;ReadOnly:Boolean=false;EventHint:String='');
          procedure DeleteEvent(EventType:String);
          procedure DeleteAttribute(AttributeName:string);
          procedure DebugEvents(sometext:String);
          procedure LogAttributes(sometext:String);
       end;

const AlignmentOptions:Array[0..4] of string = ('Left','Right','Centre','Top','Bottom');
const LabelPosOptions:Array[0..3] of string = ('Top','Bottom','Left','Right');
const LanguageOptions:Array[0..1] of string = ('Pascal','Python');
const ScrollBarsOptions:Array[0..2] of string = ('Bottom','Right','Both');
type TAttribOptionsRec = record
      ComponentType:String;
      AttribName:String;
      Options:TStringList;
end;
type TAttribOptionsArray = array of TAttribOptionsRec;

type TExclusionAttrib = record
  NodeType : string;
  AttribName : string;
  TargetAttrib:String;
end;
type TExclusionAttribs = array of TExclusionAttrib;

{$ifdef JScript}
type TInterfaceObject = class(TDataNode)
private

published
  myNode:TDataNode;          // self-reference for compatibility with Laz component
end;


{$endif}

function SubstituteSpecials(instring:string):string;
function UnSubstituteSpecials(instring:string):string;
function CreateFormNode(myForm:TForm):TDataNode;
function AddFormToNodeTree(myForm:TForm):TdataNode;
function InitialiseCodeTree:TdataNode;
procedure AddDefaultsToTable(MyNodeType:String;myDefaultAttribs:TDefaultAttributesArray);
function GetDefaultAttrib(NodeType,AttrName:String):TDefaultAttribute;
function IsADefaultAttrib(NodeType,AttrName:String):Boolean;
procedure AddDefaultAttribs(myComponent:TObject;NewNode:TDataNode;defaultAttribs:TDefaultAttributesArray);
procedure AddDefaultAttribute(var Attribs:TDefaultAttributesArray;AttrName,AttrType,AttrValue,AttrHint:String; ro:Boolean);
procedure AddDefaultAttribute(var Attribs:TDefaultAttributesArray;AttrName,AttrType,AttrValue,AttrHint:String; ro,incl:Boolean);
procedure AddExclusionAttribToTable(NodeType,AttribName,TargetAttrib:String);
function GetDefaultAttribs(NodeType:String):TDefaultAttributesArray;
procedure AddAttrib(var AttrParams:TNodeAttributesArray;attrName,attrType,attrValue:string;attrReadOnly:Boolean);
procedure AddChildToParentNode(var ParentNode, ChildNode:TDataNode; position:integer);
function NodeTreeToXML(CurrentItem,ParentNode:TDataNode;DynamicOnly,QuotedString:Boolean):String;
function FindDataNodeById(InTree:TDataNode; ScreenObjectID,NameSpace:String;showerror:boolean):TDataNode;
function FindParentOfNode(InTree:TDataNode;targetNode:TdataNode;showerror:Boolean=false):TDataNode; // overload;
function FindAncestorByType(childNode:TdataNode;NodeType:String;showerror:Boolean=false):TDataNode;
procedure ReParentNode(MyNode,NewParent:TDataNode);
procedure DeleteNode(ParentNode,MyNode:TDataNode);
function CopyNode(SourceNode:TDataNode;DrillDown:Boolean):TDataNode;
function AddChildToDataNode(ParentNode:TDataNode;MyClass,MyName,MyType,Namespace:string;MyAttributes:TNodeAttributesArray;
                           position:integer):TDataNode;
function LookupComponentFunc(NodeType:string):TAddComponentFunc;
function NodeIsDescendantOf(ThisNode:TDataNode;AncestorName:string):integer;
function NodeIsInXForm(ThisNode:TDataNode):Boolean;
procedure DeleteNodeChildren(ParentNode:TDataNode);
function InsertSystemNode(ParentNode,SourceNode:TDataNode;Position:integer):TDataNode;
function NodeNameIsUnique(myNode:TDataNode;NodeName,Namespace:string; showerror:Boolean):Boolean;
function SetUniqueName(myNode:TDataNode;Formname:String;const NewName: TComponentName):TComponentName;
procedure InitSystemNodetree;
procedure ClearAllDynamicNodes(StartNode:TDataNode);
procedure NilScreenObject(MyNode:TdataNode);
function AlignmentResetInvalidCombinations(OldAlignment,myName,myClass:string;ParentAlignChildrenVertical,IsContainer,HasSibs:Boolean):string;
procedure SortAttribs(var Attribs: TNodeAttributesArray);
procedure AddAttribOptions(ComponentType,AttribName:string;Options:array of string);
function LookupAttribOptions(ComponentType,AttribName:string):TstringList;
function DeleteAttribOptions(ComponentType,AttribName:string):TstringList;
function XMLToNodeTree(XMLString:String; UIParentNode:TDataNode; ExpandingComposite:Boolean=false):String;
procedure SetXObjectProperty(myObj:TObject;targetNode:TdataNode;PropName,AttrType,newValue:String);


procedure EditNodeAttributeValue(targetNode:TDataNode; SourceAttrib:TNodeAttribute;AddIfMissing:Boolean);
procedure EditAttributeValue(NodeNameToEdit,NameSpace:String; SourceAttrib:TNodeAttribute;AddIfMissing:Boolean=false); overload;
procedure EditAttributeValue(NodeNameToEdit,NameSpace,AttrNameToEdit,newValue:String;AddIfMissing:Boolean=false;Context:TDataNode=nil); overload;
procedure EditAttributeValue(NodeToEdit:TDataNode;AttrNameToEdit,newValue:String;AddIfMissing:Boolean=false);
{$ifndef JScript}
procedure EditAttributeValue(NodeNameToEdit,NameSpace,AttrNameToEdit,newValue:PChar); overload;
procedure EditAttributeValue(NodeToEdit:TDataNode;AttrNameToEdit,newValue:PChar); overload;
{$else}
procedure EditAttributeValue2(NodeNameToEdit,NameSpace,AttrNameToEdit,newValue:String);
{$endif}

procedure EditAttributeValueIndexed(NodeNameToEdit,NameSpace,AttrNameToEdit:String;newValue:TStringArray; x,y:integer);

function FindInterfaceNode(StartNode:TDataNode;NameSpace:String;PropName:String):TdataNode;

procedure PushSourceToAttributes(FromNode:TDataNode; FromAttrib:TNodeAttribute);
procedure PushAllSourcesToAttributes;
procedure PushNodeSourcesToAttributes(FromNode:TDataNode);
procedure myCopyToClip(stringname,instring:string);
function FindNodesOfType(StartNode:TdataNode;NodeType:String;InNameSpace:Boolean=false;NameSpace:String=''):TNodesArray;
procedure SetNodePropDefaults(myNode:TDataNode;defaultAttribs:TDefaultAttributesArray);
procedure UnSuspendFrames(StartNode:TdataNode);

{$ifndef JScript}
Procedure SaveSystemToIncFile;
procedure AddNodeFuncLookup(NodeType:string;ScreenObjFunc:TAddComponentFunc);
procedure SetAlignProperty(MyObj,MyParent:TControl);   overload;
procedure SetAlignProperty(ParentNode, MyNode:TDataNode); overload;
function FindOuterParentOf(InnerControl:TControl):TControl;
procedure CreateComponentDataNode2(myComponent:TObject;myType:String; defaultAttribs:TDefaultAttributesArray; eventTypes:TStringList; myOwner:TObject; IsDynamic:Boolean);
function CreateDynamicLazWidget(TypeName:String;ParentForm:TForm;ParentNode:TDataNode;NodeName,NameSpace,Alignment:string;position:integer):TDataNode;
procedure InitialiseXComponentsProject;
procedure ResequenceNVNodes;

{$else}
procedure RefreshComponentProps(myComponent:TDataNode);
procedure InitFormObject(myForm:TForm;NodeName:String);
procedure AddNodeFuncLookup(NodeType:string;InObFuncPtr:TCreateInObFunc;ScreenObjFunc:TAddComponentFunc);
procedure SetInterfaceProperty(myName,NameSpace,PropName,NewValue:string);
function mygetClipboardData(stringname:string):string;
function FinishHTMLPasteAction(myValue:string):string;
{$endif}

type TSourcedAttrib = record
      TheAttribute:TNodeAttribute;
      TheNode:TDataNode;
      SourceNode:TDataNode;
      InProgress:Boolean;
end;

var MainForm:TForm;
SuppressEvents:Boolean;
SuppressUserEvents:Boolean;
AttribOptionsArray:TAttribOptionsArray;
ProjectDirectory:String;
DefaultAttribsByType:TDefaultAttribsTable;
SourcedAttribs:array of  TSourcedAttrib;
ExclusionAttribs:TExclusionAttribs;

//var
//debugfind:Boolean;

{$ifndef JScript}
MainFormTopControl:TWinControl;
{$else}


type TMethod = record
  Code : Pointer;       //CodePointer;
  Data : Pointer;
end;

function CreateInterfaceObject(MyForm:TForm;NodeType, NodeName,NameSpace:String):TObject;

// this variable contains the system description to be loaded at startup
var LoadedSystemString:String;

{$endif}



const SystemRootName= 'UIRootNode';
const CodeRootName= 'CodeUnits';
const PortraitMode=false;

var SystemNodeTree,UIRootNode,CodeRootNode, DMRoot:TDataNode;
var StartingUp:Boolean;

var   NodeFuncsLookup:TNodeFuncsTable;             // lookup table used in data node creation

const EventAttributeDelimiter='|@|';
const EventListdelimiter='|^|';
const EventHistorydelimiter='|~|';
const delimiterBetweenAttribsAndEvents = '|@@|';
const attributeListdelimiter='|;'  ;
const NameValuePairdelimiter= '|=' ;
const AttribBitsDelimiter= '|{'   ;
const DataDelimiter= '|}'   ;

const StartXMLString =  '<';
const ToggleFlagMarker = '/';
const EndXMLString =  '>';

// available types for component attributes
const AttributeTypes:String = '["String","Integer","Boolean","Color","TableString","TreeString"]';


implementation
{$ifndef JScript}
uses LazsUtils,WrapperPanel, XForm, XBitMap, XIFrame, XTable, Events, PasteDialogUnit, CompilerLogUnit;
{$else}
uses WrapperPanel, XForm, XButton, XBitMap, XTable, PasteDialogUnit, HTMLUtils;
{$endif}

constructor TNodeAttribute.Create(InNode:TDataNode; AttrName:String);
begin
  self.OwnerNode:=InNode;
  self.AttribName:=AttrName;
  self.AttribReadOnly:=false;
  self.AttribType:='String';
  self.AttribValue:='';
  self.AttribHint:='';
end;

constructor TDataNode.Create(MyClass,MyName,MyNameSpace,MyType:string;NodeIsDynamic:Boolean=false);
var
  NodeString:string;
begin
  SetLength(self.ChildNodes,0) ;
  SetLength(self.NodeAttributes,0);
  self.myEventTypes:=TStringList.Create;
  SetLength(self.myEventHandlers,0);

  {$ifdef JScript}
  asm
  if ((MyNameSpace==null)||(MyNameSpace==undefined))
    {MyNameSpace='';}
  end;
  {$endif}
  self.NodeClass:=MyClass;
  self.NameSpace:=MyNameSpace;
  self.NodeName:=MyName;
  self.NodeType:=MyType;
  self.IsDynamic:=NodeIsDynamic;

end;


procedure TDataNode.DeleteMe;
begin
  self.ScreenObject:=nil;
  if assigned(self) then
    self.Destroy;

end;

function TDataNode.HasAttribute(AttrName:string):Boolean;
var
  found:Boolean;
  i:integer;
begin
  found:=false;
  for i:=0 to length(self.NodeAttributes)-1 do
    if self.NodeAttributes[i].AttribName=AttrName then
      found:=true;
  result:=found;
end;

function TDataNode.GetAttribute(AttrName:string;AllowSpace:boolean):TNodeAttribute;
var
  i:integer;
  foundAttrib:TNodeAttribute;
  myAttribs:TNodeAttributesArray;
begin
  i:=0;
  foundAttrib:=nil;
  myAttribs:=self.NodeAttributes;
  while i < length(myAttribs) do
  begin
    if self.NodeAttributes[i].AttribName=AttrName then
    begin
      foundAttrib:=self.NodeAttributes[i];
      i:= length(myAttribs);
    end;
    i:=i+1;
  end;
  if foundAttrib=nil then
    if AllowSpace then
    begin
      foundAttrib:=TNodeAttribute.Create(self,AttrName);
      SetLength(self.NodeAttributes,length(self.NodeAttributes)+1);
      self.NodeAttributes[length(self.NodeAttributes)-1]:=foundAttrib;
    end
    else
    begin
      showmessage('Attribute '+AttrName+' not found in node '+self.NodeName);
    end;
  result:=foundAttrib;
end;

function TDataNode.GetAttributeAnyCase(AttrName:string;AllowSpace:boolean):TNodeAttribute;
var
  i:integer;
  foundAttrib:TNodeAttribute;
  myAttribs:TNodeAttributesArray;
begin
  i:=0;
  foundAttrib:=nil;
  myAttribs:=self.NodeAttributes;
  while i < length(myAttribs) do
  begin
    if trim(uppercase(self.NodeAttributes[i].AttribName))=trim(uppercase(AttrName)) then
    begin
      foundAttrib:=self.NodeAttributes[i];
      i:= length(myAttribs);
    end;
    i:=i+1;
  end;
  if foundAttrib=nil then
    if AllowSpace then
    begin
      foundAttrib:=TNodeAttribute.Create(self,AttrName);
      SetLength(self.NodeAttributes,length(self.NodeAttributes)+1);
      self.NodeAttributes[length(self.NodeAttributes)-1]:=foundAttrib;
    end
    else
    begin
      showmessage('Attribute '+AttrName+' not found in node '+self.NodeName);
    end;
  result:=foundAttrib;
end;

function TDataNode.GetAttributeAnyCase(AttrName:string):TNodeAttribute;
begin
  result:=self.GetAttributeAnyCase(AttrName,false);
end;

function GetDefaultAttribs(NodeType:String):TDefaultAttributesArray;
var
  i,j:integer;
  empty:TDefaultAttributesArray;
  thisrec:TDefaultAttribsByType;
begin
  setLength(empty,0);
  result:=empty;
  i:=0;
  while i<length(DefaultAttribsByType) do
  begin
    if DefaultAttribsByType[i].NodeType=NodeType then
    begin
      thisrec:=DefaultAttribsByType[i];
      result:=thisrec.DefaultAttribs;
      i:=length(DefaultAttribsByType);
    end;
    i:=i+1;
  end;
end;

function GetDefaultAttrib(NodeType,AttrName:String):TDefaultAttribute;
var
  i,j:integer;
  dummyrec:TDefaultAttribute;
  thisrec:TDefaultAttribsByType;
begin
  dummyrec.AttribName:='';
  result:=dummyrec;
  i:=0;
  while i<length(DefaultAttribsByType) do
  begin
    if DefaultAttribsByType[i].NodeType=NodeType then
    begin
      j:=0;
      thisrec:=DefaultAttribsByType[i];
      while j<length(thisrec.DefaultAttribs) do
      begin
        if DefaultAttribsByType[i].DefaultAttribs[j].AttribName=AttrName then
        begin
          result:=DefaultAttribsByType[i].DefaultAttribs[j];
          j:=length(DefaultAttribsByType[i].DefaultAttribs);
        end;
        j:=j+1;
      end;
      i:=length(DefaultAttribsByType);
    end;
    i:=i+1;
  end;
end;

function IsADefaultAttrib(NodeType,AttrName:String):Boolean;
var
  i,j:integer;
  thisrec:TDefaultAttribsByType;
begin
  result:=false;

  if AttrName='ParentName' then
    result:=true;

  if result=false then
  begin
    i:=0;
    while i<length(DefaultAttribsByType) do
    begin
      if DefaultAttribsByType[i].NodeType=NodeType then
      begin
        j:=0;
        thisrec:=DefaultAttribsByType[i];
        while j<length(thisrec.DefaultAttribs) do
        begin
          if DefaultAttribsByType[i].DefaultAttribs[j].AttribName=AttrName then
          begin
            result:=true;
            j:=length(DefaultAttribsByType[i].DefaultAttribs);
          end;
          j:=j+1;
        end;
        i:=length(DefaultAttribsByType);
      end;
      i:=i+1;
    end;
  end;
end;

procedure AddDefaultsToTable(MyNodeType:String;myDefaultAttribs:TDefaultAttributesArray);
begin
  SetLength(DefaultAttribsByType,length(DefaultAttribsByType)+1);
  DefaultAttribsByType[length(DefaultAttribsByType)-1].NodeType:=MyNodeType;
  DefaultAttribsByType[length(DefaultAttribsByType)-1].DefaultAttribs:=myDefaultAttribs;
end;

procedure AddDefaultAttribute(var Attribs:TDefaultAttributesArray;AttrName,AttrType,AttrValue,AttrHint:String; ro,incl:Boolean);
var
  numAttributes:Integer;
begin
   numAttributes:=Length(Attribs);
   SetLength( Attribs,numAttributes+1);
   Attribs[numAttributes].AttribName := AttrName ;
   Attribs[numAttributes].AttribValue := AttrValue;
   Attribs[numAttributes].AttribType := AttrType ;
   Attribs[numAttributes].AttribReadOnly := ro;
   Attribs[numAttributes].AttribHint := AttrHint ;
   Attribs[numAttributes].AttribIncludeInSave:=incl;
end;
procedure AddDefaultAttribute(var Attribs:TDefaultAttributesArray;AttrName,AttrType,AttrValue,AttrHint:String; ro:Boolean);
begin
  AddDefaultAttribute(Attribs,AttrName,AttrType,AttrValue,AttrHint,ro,true);
end;

procedure AddExclusionAttribToTable(NodeType,AttribName,TargetAttrib:String);
begin
  SetLength(ExclusionAttribs,length(ExclusionAttribs)+1);
  ExclusionAttribs[length(ExclusionAttribs)-1].NodeType:=NodeType;
  ExclusionAttribs[length(ExclusionAttribs)-1].AttribName:=AttribName;
  ExclusionAttribs[length(ExclusionAttribs)-1].TargetAttrib:=TargetAttrib;
end;

procedure TDataNode.AddAttribute(AttributeName,AttributeType,AttributeValue:string;AttributeReadOnly:boolean);
var
  numAttributes:Integer;
  myAttributes:TNodeAttributesArray;
  newAttrib:TNodeAttribute;
begin
  //showmessage('adding '+AttributeName+' '+AttributeValue);
    myAttributes:= self.NodeAttributes;
    numAttributes:=Length(myAttributes);
    SetLength( self.NodeAttributes,numAttributes+1);
    self.NodeAttributes[numAttributes]:=TNodeAttribute.Create(self,AttributeName);
    self.NodeAttributes[numAttributes].AttribValue := AttributeValue;
    self.NodeAttributes[numAttributes].AttribType := AttributeType ;
    self.NodeAttributes[numAttributes].AttribReadOnly := AttributeReadOnly;

    SortAttribs(self.NodeAttributes);
end;

procedure TDataNode.DeleteAttribute(AttributeName:String);
var
  i:integer;
  found:boolean;
begin
  for i:=0 to length(self.NodeAttributes)-1 do
  begin
    if self.NodeAttributes[i].AttribName=AttributeName then
    begin
      found:=true;
      TNodeAttribute(self.NodeAttributes[i]).Free;
    end;
    if found then
    begin
      if i<length(self.NodeAttributes)-1 then
        self.NodeAttributes[i]:=self.NodeAttributes[i+1];
    end;
  end;
  if found then
    SetLength( self.NodeAttributes,length(self.NodeAttributes)-1);
end;


procedure TDataNode.SetAttributeValue(AttrName:string;const NewValue,AttrType:string;AttribReadOnly:Boolean); // overload;
var
  foundAttrib:TNodeAttribute;
begin
  foundAttrib:=self.GetAttribute(AttrName,true);
  //this may have created a new attribute (default type String), so fix the type.
  if AttrType<>'' then
     foundAttrib.AttribType:=AttrType;

  foundAttrib.AttribValue:=NewValue;
  foundAttrib.AttribReadOnly:=AttribReadOnly;

end;

procedure TDataNode.SetAttributeValue(AttrName:string;const NewValue,AttrType:string); // overload;
var
  foundAttrib:TNodeAttribute;
begin
  foundAttrib:=self.GetAttribute(AttrName,true);
  //this may have created a new attribute (default type String), so fix the type.
  if AttrType<>'' then
     foundAttrib.AttribType:=AttrType;
  foundAttrib.AttribValue:=NewValue;
end;

procedure TDataNode.SetAttributeValue(AttrName:string;const NewValue:string); //overload;
begin
  self.SetAttributeValue(AttrName,NewValue,'');
end;

procedure TDataNode.SetAttributeSource(AttrName:string;SourceNodeName,SourceNameSpace,SourceAttribName:string);
var
  foundAttrib:TNodeAttribute;
  i:integer;
begin
  //if no attribute here, add one
  foundAttrib:=self.GetAttribute(AttrName,true);

//  SourceNode:=FindData
//  SourceAttrib:=???.GetAttribute(AttrName,true);
  foundAttrib.AttribSource.InputNodeName:=SourceNodeName;
  foundAttrib.AttribSource.InputNameSpace:=SourceNameSpace;
  foundAttrib.AttribSource.InputAttribName:=SourceAttribName;

  if SourceNodeName='' then
  begin
    foundAttrib.AttribSource.InputValue:='';
  end;
  // now have to put it back in the array
  (*
  found:=false;
  i:=0;
  while i < length(self.NodeAttributes) do
  begin
    if self.NodeAttributes[i].AttribName=AttrName then
    begin
      self.NodeAttributes[i]:=foundAttrib;
      i:= length(self.NodeAttributes);
      found:=true;
    end;
    i:=i+1;
  end;
  *)
end;

function TDataNode.GetChildIndex(ChildNode:TDataNode):integer;
var
  i:integer;
  mychildren:TChildNodesArray;
begin
  result:=-1;
  mychildren:=self.ChildNodes;
  for i:=0 to length(mychildren)-1 do
  begin
     if mychildren[i].NodeName=ChildNode.NodeName then
       result:=i;
  end;
end;

procedure TDataNode.RemoveChildNode(ChildNode:TDataNode);
var
  i,l:integer;
  found:Boolean;
  mychildren:TChildNodesArray;
begin
  found:=false;
  mychildren:=self.ChildNodes;
  l:=length(mychildren);
  i:=0;
  while (i < length(mychildren)) do
  begin
     if found=false then
     begin
       if mychildren[i]=ChildNode then
       begin
         found:=true;
       end;
     end;
     if (found) and (i<length(mychildren)) then
       mychildren[i]:=mychildren[i+1];
     i:=i+1;
  end;
  if found then
    setlength(mychildren,l-1);
  self.ChildNodes:=mychildren;
end;

procedure TDataNode.AddEvent(EventType,MainCode,InitCode:String;ReadOnly:Boolean=false;EventHint:String='');
var
  j:integer;
begin
  self.myEventTypes.Add(EventType);
  setlength(self.myEventHandlers,self.myEventTypes.Count);
  j:=self.myEventTypes.Count-1;
  self.myEventHandlers[j]:=TEventHandlerRec.Create;
  self.myEventHandlers[j].TheCode:=MainCode;
  self.myEventHandlers[j].InitCode:=InitCode;
  self.myEventHandlers[j].ReadOnlyInterface:=ReadOnly;
  self.myEventHandlers[j].TheHandler:=nil;
  self.myEventHandlers[j].EventHint:=EventHint;
end;

procedure TDataNode.DeleteEvent(EventType:String);
var
  j,i:integer;
begin
  j:= self.myEventTypes.IndexOf(EventType);
  for i:=self.myEventTypes.Count-1 downto j+1 do
  begin
    self.myEventTypes[i-1]:=self.myEventTypes[i];
    self.myEventHandlers[i-1]:=self.myEventHandlers[i];
  end;
  self.myEventTypes.Delete(self.myEventTypes.Count-1);
  setlength(self.myEventHandlers,self.myEventTypes.Count);
end;

function TDataNode.EventNum(EventType:String):integer;
var
  i:integer;
begin
  result:=-1;
  for i:=0 to self.myEventTypes.Count-1 do
  begin
     if self.myEventTypes[i] = EventType then
         result:=i;
  end;
end;


function TDataNode.FindRegisteredEvent(EventType:string):TEventHandler;
var
  i:integer;
begin
  result:=nil;
  if length(self.myEventHandlers) < self.myEventTypes.Count then
    setlength(self.myEventHandlers,self.myEventTypes.Count);
  for i:=0 to self.myEventTypes.Count-1 do
  begin
    if self.myEventHandlers[i]=nil then
      self.myEventHandlers[i]:=TEventHandlerRec.Create;
    if self.myEventTypes[i] = EventType then
      result:=self.myEventHandlers[i].TheHandler;
  end;
end;

procedure TDataNode.RegisterEvent(EventType:String;TheHandler:TEventHandler);
var
  i:integer;
begin
  if length(self.myEventHandlers) < self.myEventTypes.Count then
    setlength(self.myEventHandlers,self.myEventTypes.Count);
  for i:=0 to self.myEventTypes.Count-1 do
  begin
     if self.myEventTypes[i] = EventType then
     begin
       if self.myEventHandlers[i]=nil then
         self.myEventHandlers[i]:=TEventHandlerRec.Create;
       self.myEventHandlers[i].TheHandler := TheHandler;
       //showmessage('registered event '+EventType+' for '+self.NodeName);
     end;
  end;
end;

function TDataNode.HasUserEventCode(EventType:String):Boolean;
var
  i:integer;
begin
  result:=false;
  for i:=0 to length(myEventHandlers)-1 do
  begin
     if self.myEventTypes[i] = EventType then
       if self.myEventHandlers[i]<>nil then
         if trim(self.myEventHandlers[i].TheCode)<>'' then
           result:=true;
  end;
end;

function TDataNode.GetEvent(EventType:String):TEventHandlerRec;
var
  i:integer;
begin
  result:=nil;
  for i:=0 to length(myEventHandlers)-1 do
  begin
     if self.myEventTypes[i] = EventType then
       result:=self.myEventHandlers[i];
  end;
end;

function TDataNode.GetEventCode(EventType:String):String;
var
  i:integer;
begin
  result:='';
  for i:=0 to length(myEventHandlers)-1 do
  begin
     if self.myEventTypes[i] = EventType then
       if self.myEventHandlers[i]<>nil then
         result:=self.myEventHandlers[i].TheCode;
  end;
end;

function TDataNode.GetEventInitCode(EventType:String):String;
var
  i:integer;
begin
  result:='';
  for i:=0 to length(myEventHandlers)-1 do
  begin
     if self.myEventTypes[i] = EventType then
       if self.myEventHandlers[i]<>nil then
         result:=self.myEventHandlers[i].InitCode;
  end;
end;

function TDataNode.GetEventROI(EventType:String):Boolean;
var
  i:integer;
begin
  result:=false;
  for i:=0 to length(myEventHandlers)-1 do
  begin
     if self.myEventTypes[i] = EventType then
       result:=self.myEventHandlers[i].ReadOnlyInterface;
  end;
end;

procedure TDataNode.InitialiseEventHandlers;
var
  i:integer;
begin
  SetLength(self.myEventHandlers,self.myEventTypes.Count);
  for i:=0 to self.myEventTypes.Count-1 do
  begin
    if self.myEventHandlers[i]=nil then
      self.myEventHandlers[i]:=TEventHandlerRec.Create;
  end;
end;

procedure SetNodePropDefaults(myNode:TDataNode;defaultAttribs:TDefaultAttributesArray);
var
  i:integer;
begin
  for i:=0 to length(defaultAttribs)-1 do
  begin
    if myNode.HasAttribute(defaultAttribs[i].AttribName)=false then
      myNode.AddAttribute(defaultAttribs[i].AttribName,defaultAttribs[i].AttribType,defaultAttribs[i].AttribValue,defaultAttribs[i].AttribReadOnly);
  end;
end;

procedure AddDefaultAttribs(myComponent:TObject;NewNode:TDataNode;defaultAttribs:TDefaultAttributesArray);
var
  i:integer;
begin
  SetNodePropDefaults(NewNode,defaultAttribs);
  for i:=0 to length(defaultAttribs)-1 do
  begin
    if defaultAttribs[i].AttribReadOnly=false then
    begin
      //{$ifndef JScript}
      //if NewNode.NodeType='TXIFrame' then debugln('AddDefaultAttribs '+defaultAttribs[i].AttribName+' '+defaultAttribs[i].AttribValue);
      //{$endif}
      SetXObjectProperty(myComponent,NewNode,defaultAttribs[i].AttribName,defaultAttribs[i].AttribType,defaultAttribs[i].AttribValue);
    end;
  end;
end;

{$ifndef JScript}
function CreateDynamicLazWidget(TypeName:String;ParentForm:TForm;ParentNode:TDataNode;NodeName,NameSpace,Alignment:string;position:integer):TDataNode;
var
  NewNode,FormNode:TDataNode;
  NewWidget:TControl;
  supp:Boolean;
begin
  //Create Widget (also creates datanode)
  supp:=SuppressEvents;
  SuppressEvents:=true;
  NewWidget:=TControlClass(getclass(TypeName)).create(ParentForm);
  SetStrProp(NewWidget,'Name',NameSpace+NodeName);         // for uniqueness, prefix namespace
  NewNode:=TDataNode(GetObjectProp(NewWidget,'myNode'));
  NewNode.NameSpace:=NameSpace;
  NewNode.NodeName:=NodeName;

  if (TypeName<>'TXMainMenu')
  and (NewNode.NodeClass<>'NV') then
  begin
    SetStrProp(NewWidget,'Alignment',Alignment);
    InsertUnderParent(TControl(NewWidget),TWinControl(ParentNode.ScreenObject), position);
    AddChildToParentNode(ParentNode,NewNode,position);
  end
  else
  begin
    if (TypeName='TXMainMenu') then
    begin
      FormNode:=TDataNode(GetObjectProp(ParentForm,'myNode'));
      AddChildToParentNode(FormNode,NewNode,position);
    end
    else
    begin
      // NV component
      AddChildToParentNode(ParentNode,NewNode,position);
    end;
  end;
  SuppressEvents:=supp;
  result:=NewNode;
end;

procedure CreateComponentDataNode2(myComponent:TObject;myType:String; defaultAttribs:TDefaultAttributesArray; eventTypes:TStringList; myOwner:TObject; IsDynamic:Boolean);
var
  NewNode:TDataNode;
begin
  NewNode:=TDataNode.Create('UI','','',myType,false);       // name is set later
  NewNode.ScreenObject:=myComponent;
  if eventTypes=nil then
  begin
    NewNode.myEventTypes:=TStringList.Create;
  end
  else
    NewNode.myEventTypes:=eventTypes;
  NewNode.InitialiseEventHandlers;
  //showmessage('node '+myName+' event count set to '+ inttostr(EventTypes.Count));
  NewNode.MyForm:=TForm(myOwner);
  NewNode.IsDynamic:=IsDynamic;

  if myComponent<>nil then
    SetObjectProp(myComponent,'myNode',NewNode);

  AddDefaultAttribs(myComponent,NewNode,defaultAttribs);

  // temporarily set as child of root node, so that name uniqueness checks can be done during design
  AddChildToParentNode(SystemNodetree,NewNode,-1);

end;

{$else}
procedure RefreshComponentProps(myComponent:TDataNode);
var
  i:integer;
  DfltAttrib:TDefaultAttribute;
begin
  for i:=0 to length(myComponent.NodeAttributes)-1 do
  begin
    DfltAttrib:=GetDefaultAttrib(myComponent.NodeType,myComponent.NodeAttributes[i].AttribName);
    if ((myComponent.NodeType='TXForm')
      or ((DfltAttrib.AttribName<>'')
        and (DfltAttrib.AttribIncludeInSave = true)))      // other attribs have derived values, so don't need explicit reset
    and (myComponent.NodeAttributes[i].AttribReadOnly = false)
    then
    begin
      SetXObjectProperty(myComponent,myComponent,myComponent.NodeAttributes[i].AttribName,
                         myComponent.NodeAttributes[i].AttribType,myComponent.NodeAttributes[i].AttribValue);
    end;
  end;
end;


{$endif}

//function ScanChildrenForNode(CurrentItem:TDataNode;targetNode:TdataNode;var FoundParent:TDataNode; var position:Integer):TDataNode;
//var FoundItem,TempItem:TDataNode;
//    TempArrayOfChildren:TChildNodesArray;
//    NumChildren,i:integer;
//begin
//   FoundItem:=nil;
//   FoundParent:=nil;
//   if CurrentItem = targetNode
//   then
//   begin
//     FoundItem:= CurrentItem
//   end
//   else
//   begin
//      TempArrayOfChildren:= CurrentItem.ChildNodes;
//      NumChildren:=Length(TempArrayOfChildren);
//      i:=0;
//      while i < NumChildren do
//      begin
//         if FoundItem=nil then  // object has not been found so keep looking
//         begin
//           //showmessage('parent='+CurrentItem.NodeName+' i='+inttostr(i));
//            TempItem := CurrentItem.ChildNodes[i];
//            //showmessage('TempItem='+TempItem.NodeName);
//            if TempItem = targetNode
//            then
//            begin
//              FoundItem:= TempItem;
//              FoundParent:=CurrentItem;
//              position:=i;
//              i:=NumChildren;
//            end
//            else
//              FoundItem:= ScanChildrenForNode(TempItem,targetNode,FoundParent,position);
//         end;
//         i:=i+1;
//      end;
//   end;
//   result:=FoundItem;
//end;


function ScanChildrenForNodeByName(CurrentItem:TDataNode;ScreenObjectID,NameSpace:String;var FoundParent:TDataNode):TDataNode;
var FoundItem,TempItem:TDataNode;
    NumChildren,i:integer;
begin
   FoundItem:=nil;
   FoundParent:=nil;
   if CurrentItem<>nil then
   begin
     //showmessage('ScanChildrenForNodeByName currentitem '+CurrentItem.NameSpace+'.'+CurrentItem.NodeName);
     {$ifdef JScript}
     asm
     if ((NameSpace==undefined)||(NameSpace==null)) {alert('NameSpace is undefined. Looking for '+ScreenObjectID); }
     if ((CurrentItem.NodeName==undefined)||(CurrentItem.NodeName==null)) {alert('node has undefined NodeName'); }
     if ((CurrentItem.NameSpace==undefined)||(CurrentItem.NameSpace==null)) {alert('node '+CurrentItem.NodeName+' has undefined namespace'); }
     end;
     {$endif}
     if (Trim(Uppercase(CurrentItem.NodeName)) = Trim(Uppercase(ScreenObjectID)))
     and (Trim(Uppercase(CurrentItem.NameSpace)) = Trim(Uppercase(NameSpace)))
     then
     begin
       FoundItem:= CurrentItem
     end
     else
     begin
        NumChildren:=Length(CurrentItem.ChildNodes);
        i:=0;
        while i < NumChildren do
        begin
           if FoundItem=nil then  // object has not been found so keep looking
           begin
              TempItem := CurrentItem.ChildNodes[i];
              {$ifdef JScript}
              asm
              if ((TempItem.NodeName==undefined)||(TempItem.NodeName==null)) {alert('node has undefined NodeName'); }
              if ((TempItem.NameSpace==undefined)||(TempItem.NameSpace==null)) {alert('node '+TempItem.NodeName+' has undefined namespace'); }
              end;
              {$endif}
              if (Trim(Uppercase(TempItem.NodeName)) = Trim(Uppercase(ScreenObjectID)))
              and (Trim(Uppercase(TempItem.NameSpace)) = Trim(Uppercase(NameSpace)))
              then
              begin
                if (TempItem.NodeParent<>CurrentItem) then
                  showmessage('oops - NodeParent not correctly set for node '+NameSpace+'.'+ScreenObjectID);
                FoundItem:= TempItem;
                FoundParent:=CurrentItem;
                i:=NumChildren;
              end
              else
                FoundItem:= ScanChildrenForNodeByName(TempItem,ScreenObjectID,Namespace,FoundParent);
           end;
           i:=i+1;
        end;
     end;
   end
   else
   begin
      showmessage('null datanode discovered in ScanChildrenForNodeByName while looking for '+ScreenObjectID);
   end;
   result:=FoundItem;
end;


function FindDataNodeById(InTree:TDataNode; ScreenObjectID,NameSpace:String;showerror:boolean):TDataNode;
var
  FoundItem, TempItem, FoundParent :TDataNode;
begin
   if trim(ScreenObjectID)='' then
     showmessage('FindDataNodeById: oops no id');
   FoundItem:=nil;
   FoundParent:=nil;
   TempItem:=ScanChildrenForNodeByName(InTree,ScreenObjectID,Namespace,FoundParent);

   if TempItem<>nil
   then
   begin
       FoundItem:= TempItem ;
   end
   else
   begin
      if showerror then
        showmessage('Error in NodeUtils.FindDataNodeById >'+NameSpace+','+ScreenObjectID+'< not found');
   end;
   result:=FoundItem;
end;


function FindParentOfNode(InTree:TDataNode;targetNode:TdataNode;showerror:Boolean=false):TDataNode; // overload;
var
  FoundItem, TempItem, FoundParent :TDataNode;
  position:integer;
begin
  FoundItem:=nil;
  //showmessage('FindParentOfNode '+ScreenObjectID+' in tree '+InTree.NodeName);
   if targetNode.NodeParent<>nil then
     FoundItem:=targetNode.NodeParent
   else
   begin
     if targetNode.Nodename<>'ApplicationRoot' then
       showMessage('oops: node '+TargetNode.NodeType+'('+TargetNode.NodeName+') has no "NodeParent" set');
//   FoundItem:=nil;
//   TempItem:=nil;
//   FoundParent:=nil;
//   TempItem:=ScanChildrenForNode(InTree,targetNode,FoundParent,position);
//
//   if (TempItem<>nil) and (FoundParent<>nil) then
//   begin
//       FoundItem:= FoundParent ;
//   end
//   else
//     if showError then
//       showmessage('Error in Nodeutils.FindParentOfNode >'+targetNode.NodeType+'('+targetNode.NodeName+')'+' < not found');
   end;

   result:=FoundItem;
end;

function FindAncestorByType(childNode:TdataNode;NodeType:String;showerror:Boolean=false):TDataNode;
var
  TempItem, FoundParent :TDataNode;
begin
   TempItem:=childNode;
   FoundParent:=nil;
   while (FoundParent=nil)
   and (TempItem.NodeParent<>nil) do
   begin
     tempitem:=tempitem.NodeParent;
      if (tempitem.NodeType = NodeType) then
        FoundParent:=tempitem;
   end;

   if (FoundParent=nil) then
     if showError then
       showmessage('Error in Nodeutils.FindAncestorByType. >'+NodeType+'< ancestor of >'+childNode.NodeName+'< not found');
   result:=FoundParent;
end;

function MakeAttrib(attrName,attrType,attrValue:string;attrReadOnly:Boolean):TNodeAttribute;
var
  newAttrib:TNodeAttribute;
begin
  newAttrib:=TNodeAttribute.Create(nil,attrName);
  newAttrib.AttribType:=attrType;
  newAttrib.AttribValue:=attrValue;
  newAttrib.AttribReadOnly:=AttrReadOnly;
  result:=newAttrib;
end;

procedure AddAttrib(var AttrParams:TNodeAttributesArray;attrName,attrType,attrValue:string;attrReadOnly:Boolean);
var
  i:integer;
begin
  i:=Length(AttrParams);
  setlength(AttrParams,i+1);
  AttrParams[i]:=MakeAttrib(attrName,attrType,attrValue,attrReadOnly);
end;

function SubstituteSpecials(instring:string):string;
var
  tempstr:string;
begin
  tempstr:=instring;
  tempstr:=myStringReplace(tempstr,LineEnding,'&crlf;',-1,-1);
  tempstr:=myStringReplace(tempstr,chr(10),'&crlf;',-1,-1);
  tempstr:=myStringReplace(tempstr,'<','&lt;',-1,-1);
  tempstr:=myStringReplace(tempstr,'>','&gt;',-1,-1);
  tempstr:=myStringReplace(tempstr,'''','&apos;',-1,-1);
  tempstr:=myStringReplace(tempstr,'"','&quot;',-1,-1);
  tempstr:=myStringReplace(tempstr,'\n','&bksln;',-1,-1);
  tempstr:=myStringReplace(tempstr,'\','&bksl;',-1,-1);
  tempstr:=myStringReplace(tempstr,EventAttributeDelimiter,'&eadlm;',-1,-1);
  tempstr:=myStringReplace(tempstr,EventListdelimiter,'&eldlm;',-1,-1);
  tempstr:=myStringReplace(tempstr,delimiterBetweenAttribsAndEvents,'&aedlm;',-1,-1);
  tempstr:=myStringReplace(tempstr,attributeListdelimiter,'&aldlm;',-1,-1);
  tempstr:=myStringReplace(tempstr,EventListdelimiter,'&eldlm;',-1,-1);
  tempstr:=myStringReplace(tempstr,AttribBitsDelimiter,'&abdlm;',-1,-1);
  tempstr:=myStringReplace(tempstr,DataDelimiter,'&akdlm;',-1,-1);
  tempstr:=myStringReplace(tempstr,'https:','&hhttppss;',-1,-1);
  tempstr:=myStringReplace(tempstr,'http:','&hhttpp;',-1,-1);
  result:=tempstr;
end;
function SubstituteSpecials2(instring:string):string;
var
  tempstr:string;
begin
  tempstr:=instring;
  tempstr:=myStringReplace(tempstr,LineEnding,'&crlf;',-1,-1);
  tempstr:=myStringReplace(tempstr,chr(10),'&crlf;',-1,-1);
  tempstr:=myStringReplace(tempstr,'<','&lt;',-1,-1);
  tempstr:=myStringReplace(tempstr,'>','&gt;',-1,-1);
  tempstr:=myStringReplace(tempstr,'''','&apos;',-1,-1);
  tempstr:=myStringReplace(tempstr,'"','&quot;',-1,-1);
  tempstr:=myStringReplace(tempstr,'\n','&bksln;',-1,-1);
  tempstr:=myStringReplace(tempstr,'\','&bksl;',-1,-1);
  tempstr:=myStringReplace(tempstr,EventAttributeDelimiter,'&eadlm;',-1,-1);
  tempstr:=myStringReplace(tempstr,EventListdelimiter,'&eldlm;',-1,-1);
  tempstr:=myStringReplace(tempstr,delimiterBetweenAttribsAndEvents,'&aedlm;',-1,-1);
  tempstr:=myStringReplace(tempstr,attributeListdelimiter,'&aldlm;',-1,-1);
  tempstr:=myStringReplace(tempstr,EventListdelimiter,'&eldlm;',-1,-1);
  tempstr:=myStringReplace(tempstr,AttribBitsDelimiter,'&abdlm;',-1,-1);
  tempstr:=myStringReplace(tempstr,DataDelimiter,'&akdlm;',-1,-1);
  tempstr:=myStringReplace(tempstr,'https:','&hhttppss;',-1,-1);
  tempstr:=myStringReplace(tempstr,'http:','&hhttpp;',-1,-1);
  result:=tempstr;
end;
function UnSubstituteSpecials(instring:string):string;
var
  tempstr:string;
begin
  tempstr:=instring;
  tempstr:=myStringReplace(tempstr,'&lt;','<',-1,-1);
  tempstr:=myStringReplace(tempstr,'&gt;','>',-1,-1);
  tempstr:=myStringReplace(tempstr,'&apos;','''',-1,-1);
  tempstr:=myStringReplace(tempstr,'&quot;','"',-1,-1);
  tempstr:=myStringReplace(tempstr,'&bksln;','\n',-1,-1);
  tempstr:=myStringReplace(tempstr,'&bksl;','\',-1,-1);
  tempstr:=myStringReplace(tempstr,'&eadlm;',EventAttributeDelimiter,-1,-1);
  tempstr:=myStringReplace(tempstr,'&eldlm;',EventListdelimiter,-1,-1);
  tempstr:=myStringReplace(tempstr,'&aedlm;',delimiterBetweenAttribsAndEvents,-1,-1);
  tempstr:=myStringReplace(tempstr,'&aldlm;',attributeListdelimiter,-1,-1);
  tempstr:=myStringReplace(tempstr,'&eldlm;',EventListdelimiter,-1,-1);
  tempstr:=myStringReplace(tempstr,'&abdlm;',AttribBitsDelimiter,-1,-1);
  tempstr:=myStringReplace(tempstr,'&akdlm;',DataDelimiter,-1,-1);
  tempstr:=myStringReplace(tempstr,'&hhttppss;','https:',-1,-1);
  tempstr:=myStringReplace(tempstr,'&hhttpp;','http:',-1,-1);
  tempstr:=myStringReplace(tempstr,'&crlf;',LineEnding,-1,-1);
  result:=tempstr;
end;

function IsExcluded(ThisNode:TDataNode; ThisAttrib:TNodeAttribute):Boolean;
var
  i:integer;
  incl:Boolean;
begin
  result:=false;
  for i := 0 to length(ExclusionAttribs)-1 do
  begin
    if (ExclusionAttribs[i].NodeType = ThisNode.NodeType)
    and (ExclusionAttribs[i].TargetAttrib = ThisAttrib.AttribName)
    then
    begin
      incl := myStrToBool(ThisNode.GetAttribute(ExclusionAttribs[i].AttribName,false).AttribValue);
      result:=not incl;
    end;
  end;
end;

function NodeTreeToXML(CurrentItem,ParentNode:TDataNode;DynamicOnly,QuotedString:Boolean):String;
// Recursive
var
  XMLString,ParentName:String;
  i,numchildren,numAttributes,numEvents:integer;
  CurrentChildNodes:TChildNodesArray;
  myAttribs:TNodeAttributesArray;
  AQuote1,AQuote2:string;
  DfltAttrib:TDefaultAttribute;
  strg:string;

procedure AppendAttribute;
begin
  DfltAttrib:=GetDefaultAttrib(CurrentItem.NodeType,CurrentItem.NodeAttributes[i].AttribName);
  if DfltAttrib.AttribName='' then
    if (CurrentItem.NodeClass<>'DM') then
      DfltAttrib.AttribIncludeInSave:=true
    else
      DfltAttrib.AttribIncludeInSave:=false;
  if (FindSuppressedProperty(CurrentItem.NodeType,CurrentItem.NodeAttributes[i].AttribName)<0)
  and (DfltAttrib.AttribIncludeInSave = true)
  and (IsExcluded(CurrentItem,CurrentItem.NodeAttributes[i]) = false)
  and (CurrentItem.NodeAttributes[i].AttribName<>'ParentName')        // is re-generated on load
  and ((CurrentItem.NodeAttributes[i].AttribName<>'XMLString')
       or ((CurrentItem.NodeAttributes[i].AttribName='XMLString') and (CurrentItem.IsDynamic=false))) then
  begin
    XMLString:=XMLString
                     + CurrentItem.NodeAttributes[i].AttribName
                     + AttribBitsDelimiter+' '+CurrentItem.NodeAttributes[i].AttribType
                     + AttribBitsDelimiter+ SubstituteSpecials(CurrentItem.NodeAttributes[i].AttribValue)
                     + AttribBitsDelimiter+ myBoolToStr(CurrentItem.NodeAttributes[i].AttribReadOnly)
                     + AttribBitsDelimiter+ CurrentItem.NodeAttributes[i].AttribSource.InputNodeName
                                          + '.'+CurrentItem.NodeAttributes[i].AttribSource.InputAttribName;
    if (CurrentItem.NodeType = 'TXCompositeIntf') then
      XMLString:=XMLString
                     + AttribBitsDelimiter+CurrentItem.NodeAttributes[i].AttribHint;
    XMLString:=XMLString
                     + attributeListdelimiter;
  end;
end;

begin

  XMLString:='';
  if CurrentItem.NodeName='ResourceRoot' then
    XMLString:='';
  if QuotedString then
    XMLString:='';

  if QuotedString then
  begin
     AQuote1 := '''';
     AQuote2 := ''' +'
  end
  else
  begin
    AQuote1:='';
    AQuote2:='';
  end;

  if ParentNode<>nil then
     ParentName:=ParentNode.Nodename;

  if ((CurrentItem.NodeClass='Root')
  or (CurrentItem.NodeClass='UI')
  or (CurrentItem.NodeClass='NV')
  or (CurrentItem.NodeClass='SVG')
  or (CurrentItem.NodeClass='Code')
  or (CurrentItem.NodeClass='DM')
  // special case (for XIDE) include the set of composites in the resources tree
  or ((CurrentItem.NodeType='TXComposite') and (CurrentItem.NodeClass='RUI'))
  or ((CurrentItem.NodeName='Composites') and (CurrentItem.NodeClass='RUI'))
  )
  and (CurrentItem.NodeName<>'XGPUCodeEditorForm')
  and (CurrentItem.NodeName<>'PasteDialog')
  and (CurrentItem.NodeName<>'CompilerLogForm')then
  begin
    if ((CurrentItem.IsDynamic = true)
      or ((DynamicOnly=false) and (CurrentItem.NameSpace='')))
    and (CurrentItem.NodeName<>'Composites')
    then
    begin

      XMLString:=AQuote2 + LineEnding + AQuote1 + StartXMLString+CurrentItem.NodeType+attributeListdelimiter;
      XMLString:=XMLString+' Class '+NameValuePairdelimiter + CurrentItem.NodeClass + attributeListdelimiter;
      XMLString:=XMLString+' Name '+NameValuePairdelimiter + CurrentItem.NodeName + attributeListdelimiter;
      XMLString:=XMLString+' NameSpace '+NameValuePairdelimiter + CurrentItem.NameSpace + attributeListdelimiter;

      myAttribs:= CurrentItem.NodeAttributes;
      numAttributes:=length(myAttribs);

      for i:=0 to numAttributes-1 do
      if CurrentItem.NodeAttributes[i].AttribName='OptionList' then
      begin
        AppendAttribute;
      end;
      for i:=0 to numAttributes-1 do
      if (CurrentItem.NodeAttributes[i].AttribName<>'')
      and (CurrentItem.NodeAttributes[i].AttribName<>'OptionList') then
      begin
        AppendAttribute;
      end;

      // add the ParentName attribute
      XMLString:=XMLString
                       + 'ParentName'
                       + AttribBitsDelimiter+' String'
                       + AttribBitsDelimiter+ SubstituteSpecials(ParentName)
                       + AttribBitsDelimiter+ 'True'
                       + AttribBitsDelimiter+ '.'
                       + attributeListdelimiter;



      //......Event Handlers (for dynamic components)
      if (CurrentItem.IsDynamic) or (CurrentItem=UIRootNode) then
      begin
        XMLString:=XMLString+delimiterBetweenAttribsAndEvents;
        if CurrentItem.myEventTypes.count>0 then
          if CurrentItem.myEventTypes.count<>length(CurrentItem.myEventHandlers) then
          begin
             CurrentItem.InitialiseEventHandlers;
          end;
        for i:=0 to CurrentItem.myEventTypes.count-1 do
        begin
          if CurrentItem.myEventHandlers[i]=nil then
          begin
            showmessage('oops event handler nil for '+CurrentItem.NodeName+' event:'+ CurrentItem.myEventTypes[i]);
          end;
          // build the full event string
          XMLString:=XMLString
                           + CurrentItem.myEventTypes[i]
                           + AttribBitsDelimiter+SubstituteSpecials(trim(CurrentItem.myEventHandlers[i].TheCode))
                           + AttribBitsDelimiter+SubstituteSpecials(trim(CurrentItem.myEventHandlers[i].InitCode));
          if (CurrentItem.NodeType = 'TXCompositeIntf')
          or (CurrentItem.NodeType = 'TXComposite') then
            XMLString:=XMLString
                           + AttribBitsDelimiter+myBoolToStr(CurrentItem.myEventHandlers[i].ReadOnlyInterface);
          if (CurrentItem.NodeType = 'TXCompositeIntf') then
            XMLString:=XMLString
                           + AttribBitsDelimiter+CurrentItem.myEventHandlers[i].EventHint;
          XMLString:=XMLString
                           + attributeListdelimiter;
        end;
      end;

      XMLString:=XMLString+EndXMLString;
    end;

    CurrentChildNodes:= CurrentItem.ChildNodes;
    numchildren:=length( CurrentChildNodes);
    for i:=0 to numchildren-1 do
       XMLString:=XMLString+NodeTreeToXML(CurrentItem.ChildNodes[i],CurrentItem,DynamicOnly,QuotedString);

    XMLString:=XMLString+StartXMLString+ToggleFlagMarker+CurrentItem.NodeType+EndXMLString;      //add '</abcd>'
  end;
  result:=( XMLString);
end;

{$ifndef JScript}
function NodeTreeToInterfaceString(CurrentItem:TDataNode;MainFormName:String):String;
var
  n,i:integer;
  resultString:string;
begin
  if (CurrentItem.IsDynamic=false) then  // item is declared in the form (not created dynamically)
  begin
    if (CurrentItem.NodeName<>SystemRootName)
    and ((CurrentItem.NodeClass='UI') or (CurrentItem.NodeClass='NV'))
    and (CurrentItem.NodeType<>'TXForm')
    and (CurrentItem.NodeType<>'')
    and (CurrentItem.MyForm<>nil) then
    begin
       // eg.   ButtonName := TXButton(CreateInterfaceObject(MyForm,'TXButton','ButtonName'));
      resultString:=CurrentItem.MyForm.Name+'.'+CurrentItem.NodeName +
                    ':= '+CurrentItem.NodeType
                    +'(CreateInterfaceObject('+CurrentItem.MyForm.Name+','''+CurrentItem.NodeType+''','''+CurrentItem.NodeName+''',''''));'
                    +LineEnding;

      if CurrentItem.NodeType='TXTree' then
      begin
         // has a function been declared to create hints for nodes in this tree?
         // If so, add code to assign the function to the new node in the javascript object
         if CurrentItem.MyForm.MethodAddress(CurrentItem.NodeName+'TreeNodeHintFunc')<>nil then
           resultString:=resultString +
                         CurrentItem.MyForm.Name+'.'+CurrentItem.NodeName+'.TreeNodeHintFunc:=@'+
                                             CurrentItem.MyForm.Name+'.'+CurrentItem.NodeName+'TreeNodeHintFunc;'
                                             +LineEnding;
         // ditto for a dropaccepted function...
         if CurrentItem.MyForm.MethodAddress(CurrentItem.NodeName+'TreeNodeDropAccepted')<>nil then
           resultString:=resultString +
                         CurrentItem.MyForm.Name+'.'+CurrentItem.NodeName+'.TreeNodeDropAccepted:=@'+
                                             CurrentItem.MyForm.Name+'.'+CurrentItem.NodeName+'TreeNodeDropAccepted;'
                                             +LineEnding;
      end;
    end
    else if (CurrentItem.NodeType='TXForm')
         and (CurrentItem.NodeClass='UI')
         and (CurrentItem.NodeName<>'XGPUCodeEditorForm') then
    begin
      resultString:=LineEnding + CurrentItem.NodeName + ':= T'+CurrentItem.NodeName+'.Create; ' +LineEnding +
                    'InitFormObject('+CurrentItem.NodeName+','''+CurrentItem.NodeName+''');' +LineEnding;
      if CurrentItem.NodeName=MainFormName then
        resultString:=resultString+'MainForm:='+CurrentItem.NodeName+';';
    end;

    if (CurrentItem.NodeName<>'XGPUCodeEditorForm') then
    begin
      n:=length( CurrentItem.ChildNodes);
      for i:=0 to n-1 do
         resultString:=resultString+NodeTreeToInterfaceString(CurrentItem.ChildNodes[i],MainFormName);
    end;
  end;
  result:=( resultString);
end;

Procedure SaveSystemToIncFile;
var
  systemstring,fullstring:string;
  interfaceString:string;
  i:integer;
begin
  interfaceString:=NodeTreeToInterfaceString(SystemNodeTree,MainForm.Name);
  WriteToFile(ProjectDirectory+'tempinc/systemintface.inc',interfaceString);
  systemstring:= NodeTreeToXML(SystemNodeTree,nil,false,true);
  if DMRoot<>nil then
    for i:=0 to length(DMRoot.ChildNodes)-1 do
      systemstring:=systemstring+NodeTreeToXML(DMRoot.ChildNodes[i],DMRoot,false,true);

  fullstring:= systemstring;

  WriteToFile(ProjectDirectory+'tempinc/systemnodetree.inc','LoadedSystemString:=''*'';LoadedSystemString := '''+fullstring+''';');
end;

procedure AddNodeFuncLookup(NodeType:string;ScreenObjFunc:TAddComponentFunc);
var
    myRec:TNodeFuncsLookup;
    l:integer;
begin
  if NodeType='' then EXIT;
  l:=Length(NodeFuncsLookup);
  SetLength(NodeFuncsLookup,l+1);
  myRec.NodeType:=NodeType;
  myRec.ScreenObjFunctionPtr:=ScreenObjFunc;     // To create a Lazarus 'X' screen object of this type dynamically
  NodeFuncsLookup[l]:=myRec;
end;
{$else}
procedure AddNodeFuncLookup(NodeType:string;InObFuncPtr:TCreateInObFunc;ScreenObjFunc:TAddComponentFunc);
var
    myRec:TNodeFuncsLookup;
    l:integer;
begin
  if NodeType='' then EXIT;
  l:=Length(NodeFuncsLookup);
  SetLength(NodeFuncsLookup,l+1);
  myRec.NodeType:=NodeType;
  myRec.InObFunctionPtr:=InObFuncPtr;            // To create an interface object of this type (not relevant for Lazarus runtime)
  myRec.ScreenObjFunctionPtr:=ScreenObjFunc;     // To create a screen object of this type (not relevant for Lazarus runtime)
  NodeFuncsLookup[l]:=myRec;
end;
{$endif}

function LookupComponentFunc(NodeType:string):TAddComponentFunc;
var
  i:integer;
begin
  i:=0;
  result:=nil;
  while i < length(NodeFuncsLookup) do
  begin
    if NodeFuncsLookup[i].NodeType=NodeType then
    begin
      result:= NodeFuncsLookup[i].ScreenObjFunctionPtr;
      i:=length(NodeFuncsLookup);
    end;
    i:=i+1;
  end;
end;

function NodeIsDescendantOf(ThisNode:TDataNode;AncestorName:string):integer;
var
  myresult:integer;
  parentNode,CurrentNode:TDataNode;
  done:boolean;
begin
  myresult:=-1;
  if ThisNode<>nil then
  begin
  if ThisNode.NodeName = AncestorName then
    myresult:=0
  else
  begin
    done:=false;
    CurrentNode:=ThisNode;
    while done=false do
    begin
      if CurrentNode.NodeName=SystemRootName then
      begin
        done:=true;
        myresult:=-1;
      end
      else
      begin
        ParentNode:=FindParentOfNode(SystemNodeTree,CurrentNode);
        if (ParentNode<>nil) then
        begin
           myresult:=myresult+1;
           if ParentNode.NodeName=AncestorName then
           begin
             done:=true;
           end
           else
             CurrentNode:=ParentNode;
        end
        else
        begin
          done:=true;
          myresult:=-1;
        end;
      end;

    end;
  end;

  end;
  result:=myresult;
end;

function NodeIsInXForm(ThisNode:TDataNode):Boolean;
var
  myresult:Boolean;
  parentNode,CurrentNode:TDataNode;
  done:boolean;
begin
  myresult:=false;
  if (ThisNode.NodeType = 'TXForm')
  and (ThisNode.IsDynamic = true)
  and (ThisNode.ScreenObject<>MainForm) then
    myresult:=true
  else
  begin
    done:=false;
    CurrentNode:=ThisNode;
    while done=false do
    begin
      if CurrentNode.NodeName=SystemRootName then
      begin
        done:=true;
        myresult:=false;
      end
      else
      begin
        ParentNode:=FindParentOfNode(SystemNodeTree,CurrentNode);
        if (ParentNode<>nil) then
        begin
           if (ParentNode.NodeType='TXForm')
           and (ParentNode.IsDynamic = true)
           and (ParentNode.ScreenObject<>MainForm) then
           begin
             done:=true;
             myresult:=true;
           end
           else
             CurrentNode:=ParentNode;
        end
        else
        begin
          done:=true;
          myresult:=false;
        end;
      end;

    end;
  end;
  result:=myresult;
end;

{$ifndef JScript}
procedure SetAlignProperty(MyObj,MyParent:TControl);
var
  ParentAlignChildrenVertical:Boolean;
  PropInfo: PPropInfo;
begin
  if (MyObj<>nil) and (MyParent<>nil)
  and (HasProperty(MyObj,'Align')) then
  begin
    // Sort out alignment within the parent control...
    ParentAlignChildrenVertical := GetBooleanProperty(MyParent, 'AlignChildrenVertical');

    if (ParentAlignChildrenVertical) then
      MyObj.Align:=alTop
    else
      MyObj.Align:=alLeft;

  end;
end;
procedure SetAlignProperty(ParentNode, MyNode:TDataNode);
var
  MyObj,MyParent:TControl;
  ParentAlignChildrenVertical:Boolean;
  PropInfo: PPropInfo;
begin
  // Sort out alignment within the parent control...
  MyObj:=TControl(MyNode.ScreenObject);
  MyParent:=TControl(ParentNode.ScreenObject);
  SetAlignProperty(MyObj,MyParent);
end;
{$endif}

function AlignmentResetInvalidCombinations(OldAlignment,myName,myClass:string;ParentAlignChildrenVertical,IsContainer,HasSibs:Boolean):string;
var
  MyAlignment:String;
  ShowMessages:Boolean;
begin
  ShowMessages:=true;
  MyAlignment:=OldAlignment;

  if (MyAlignment='') then
  begin
    showMessages:=false;
    if ParentAlignChildrenVertical=true then
      MyAlignment:='Left'
    else
      MyAlignment:='Top';
  end;

  // reset invalid combinations
  if (MyAlignment='Right') or (MyAlignment='Left') then
  begin
    if ParentAlignChildrenVertical=false then
    // in a horizontal list, Right/left are invalid
    begin
      //if showMessages then showmessage('Alignment for '+myClass+'('+myName+') in a horizontal list is reset from '+MyAlignment+' to Top');
      MyAlignment:='Top';
    end;
  end
  else if (MyAlignment='Top') or (MyAlignment='Bottom') then
  begin
    if ParentAlignChildrenVertical=true then
    // in a vertical list, Top/Bottom are invalid
    begin
      //if showMessages then showmessage('Alignment for '+myClass+'('+myName+') in a vertical list is reset from '+MyAlignment+' to Left');
      MyAlignment:='Left';
    end;
  end;


  // For containers in a vertical list, we can only set Right/Centre if this is the ONLY child of its parent
  if ((MyAlignment='Right') or (MyAlignment='Centre'))
  and (IsContainer) then
    if (HasSibs) then
    begin
      MyAlignment:='Left';
      if showMessages
      and (not StartingUp) then
        showmessage('Place this '+myClass+'('+myName+') inside its own parent VBox to set Alignment Right or Centre');
    end;
  // For containers in a horizontal list, we can only set Bottom/Centre if this is the ONLY child of its parent
  if ((MyAlignment='Bottom') or (MyAlignment='Centre'))
  and (IsContainer) then
    if (HasSibs) then
    begin
      MyAlignment:='Top';
      if showMessages
      and (not StartingUp) then
        showmessage('Place this '+myClass+'('+myName+') inside its own parent HBox to set Alignment Bottom or Centre');
    end;


  result:=MyAlignment;
end;

procedure AddChildToParentNode(var ParentNode, ChildNode:TDataNode; position:integer);
var numchildren:integer;
    i:integer;
    pn:TDataNode;
begin
  // remove the node from its existing parent, if any
  if ChildNode.NodeName<>'' then
  begin
    pn:=ChildNode.NodeParent;
    if pn<>nil then
    begin
      pn.RemoveChildNode(ChildNode);
    end;
  end;
  ChildNode.NodeParent:=ParentNode;

  if ParentNode<>nil then
  begin
    // add the node to its new parent
    numchildren:= Length(ParentNode.ChildNodes);
    SetLength(ParentNode.ChildNodes,numchildren+1) ;

    if position=-1 then
    begin
      ParentNode.ChildNodes[numchildren]:=ChildNode;
    end
    else
    begin
      for i:=numchildren downto position+1 do
      begin
        ParentNode.ChildNodes[i]:=ParentNode.ChildNodes[i-1];
      end;
      ParentNode.ChildNodes[position]:=ChildNode;
    end;
    {$ifndef JScript}
    if ChildNode.NodeClass='UI' then
      SetAlignProperty(ParentNode,ChildNode);
    {$endif}

    ChildNode.SetAttributeValue( 'ParentName', ParentNode.NodeName);
  end;
end;

function NodeNameIsUnique(myNode:TDataNode;NodeName,Namespace:string; showerror:Boolean):Boolean;
var
  myresult:Boolean;
  founditem:TDataNode;
begin
  myresult:=true;
  founditem:=FindDataNodeById(SystemNodeTree,NodeName,NameSpace,false);
  if (founditem<>nil) and (foundItem<>myNode) and (founditem.NodeName=NodeName) then
  begin
    if showerror then
      ShowMessage('Error. Name >'+NodeName+'< is not unique when creating a new object' );
    myresult:=false;
  end;
  result:=myresult;
end;

function SetUniqueName(myNode:TDataNode;FormName:String;const NewName: TComponentName):TComponentName;
var
  ApplyName:TComponentName;
  i:integer;
begin
  ApplyName:=NewName;
  i:=1;
  // additional check - component name must be unique in the tree of data nodes for the whole
  // application (ie. not just within a form).  Check here and suffix name if necessary.
    while NodeNameIsUnique(myNode,ApplyName,myNode.NameSpace,false) = false do
    begin
      ApplyName:=FormName + NewName + inttostr(i);    //!!!! when nodes are deleted in IDE, have to destroy the data node and screenobject!!!!
      i:=i+1;
    end;
  result:=ApplyName;
end;

function AddChildToDataNode(ParentNode:TDataNode;MyClass,MyName,MyType,Namespace:string;MyAttributes:TNodeAttributesArray;
                           position:integer):TDataNode;
var numchildren:integer;
    newNode:TDataNode;
    i,j:integer;
begin

  if NodeNameIsUnique(nil,MyName,Namespace,true) then
  begin
    numchildren:= Length(ParentNode.ChildNodes);
    SetLength(ParentNode.ChildNodes,numchildren+1) ;

    newNode:=TDataNode.Create(MyClass,MyName,NameSpace,MyType);
    newNode.NodeAttributes:=MyAttributes;
    newNode.NodeParent:=ParentNode;
    newNode.AddAttribute('ParentName','String', ParentNode.NodeName,true);
    for i:=0 to length(MyAttributes)-1 do
    begin
      MyAttributes[i].OwnerNode:=newNode;
    end;

    if position=-1 then
    begin
      ParentNode.ChildNodes[numchildren]:=newNode;
      result:=  ParentNode.ChildNodes[numchildren];
    end
    else
    begin
       for i:=numchildren downto position+1 do
       begin
          ParentNode.ChildNodes[i]:=ParentNode.ChildNodes[i-1];
       end;
       ParentNode.ChildNodes[position]:=newNode;
       result:=  ParentNode.ChildNodes[position];
    end;

  end
  else
    result:=nil;
end;

function CopyEventHandlers(DestNode,SourceNode:TDataNode;AddIfMissing:Boolean):boolean;
var
  i,j,k:integer;
  ok:Boolean;
begin
  if DestNode<>nil then
  begin
    j:=length(SourceNode.myEventHandlers);
    k:= SourceNode.myEventTypes.Count;
    //{$ifdef JScript} asm console.log('CopyEventHanfdlers. length(SourceNode.myEventHandlers)='+j+' SourceNode.myEventTypes.Count='+k); end; {$endif}
    SourceNode.InitialiseEventHandlers;
    //if length(SourceNode.myEventHandlers) < SourceNode.myEventTypes.Count then
    //for i:=length(SourceNode.myEventHandlers) to SourceNode.myEventTypes.Count-1 do
    //  SourceNode.AddEvent(SourceNode.myEventTypes[i],'','');

    setlength(DestNode.myEventHandlers,Destnode.myEventTypes.Count);
    for i:=0 to SourceNode.myEventTypes.Count-1 do
    begin
      j := DestNode.myEventTypes.IndexOf(SourceNode.myEventTypes[i]);
      if (j<0) then
      begin
        if AddIfMissing then
        begin
          DestNode.AddEvent(SourceNode.myEventTypes[i],
                            SourceNode.myEventHandlers[i].TheCode,
                            SourceNode.myEventHandlers[i].InitCode,
                            SourceNode.myEventHandlers[i].ReadOnlyInterface,
                            SourceNode.myEventHandlers[i].EventHint);
        end
        else
          ok:=false;
      end
      else
      begin
        if Destnode.myEventHandlers[j]=nil then
          Destnode.myEventHandlers[j]:=TEventHandlerRec.Create;
        Destnode.myEventHandlers[j].TheCode:=SourceNode.myEventHandlers[i].TheCode;
        Destnode.myEventHandlers[j].InitCode:=SourceNode.myEventHandlers[i].InitCode;
        Destnode.myEventHandlers[j].ReadOnlyInterface:=SourceNode.myEventHandlers[i].ReadOnlyInterface;
        Destnode.myEventHandlers[j].EventHint:=SourceNode.myEventHandlers[i].EventHint;
      end;
    end;

    result:=ok;
  end
  else
    result:=false;
end;

function CopyNode(SourceNode:TDataNode;DrillDown:Boolean):TDataNode;
// recursive
var
  NewNode:TDataNode;
  myAttribs:TNodeAttributesArray;
  myEventHandlers:TEventHandlers;
  i:integer;
begin
  setlength(myAttribs,length(SourceNode.NodeAttributes));
  for i:=0 to length(SourceNode.NodeAttributes)-1 do
     myAttribs[i]:=SourceNode.NodeAttributes[i];

  NewNode:=TDataNode.Create(SourceNode.NodeClass, SourceNode.NodeName,SourceNode.NameSpace,SourceNode.NodeType);
  NewNode.IsDynamic:=true;
  NewNode.NodeAttributes:=myAttribs;

  NewNode.myEventTypes:=TStringList.Create;
  for i:=0 to SourceNode.myEventTypes.count-1 do
  begin
     NewNode.AddEvent(SourceNode.myEventTypes[i],'','');
  end;
  CopyEventHandlers(NewNode,SourceNode,(SourceNode.NodeType='TXCompositeIntf'));

  setlength(NewNode.ChildNodes,0);
  if DrillDown then
  begin
    setlength(NewNode.ChildNodes,length(SourceNode.ChildNodes));
    for i:=0 to length(SourceNode.ChildNodes)-1 do
      NewNode.ChildNodes[i]:=CopyNode(SourceNode.ChildNodes[i],DrillDown);
  end;

  result:=NewNode;
end;

function InsertSystemNode(ParentNode,SourceNode:TDataNode;Position:integer):TDataNode;
// Dynamic runtime creation of a new component
var
    myparent,myself:TDataNode;
    i:integer;
    DfltAttrib:TDefaultAttribute;
begin
  myparent:=ParentNode;
  if (myParent<>nil)
  and (myparent.NodeName<>'')
  then
  begin
    if (SourceNode.NodeClass = 'UI')
    or (SourceNode.NodeClass = 'NV')
    or (SourceNode.NodeClass = 'DM')
    or (SourceNode.NodeClass = 'SVG') then
    begin
      // create the screen object and data node...
      begin
        if (SourceNode.NodeClass <> 'DM') then
        begin
          myself:=AddDynamicWidget(SourceNode.NodeType,ParentNode.MyForm,ParentNode,SourceNode.NodeName,SourceNode.NameSpace,'Left',position);
          myself.NameSpace:=SourceNode.NameSpace;
          CopyEventHandlers(myself,SourceNode,((SourceNode.NodeType='TXCompositeIntf')or(SourceNode.NodeType='TXComposite')));
          for i:=0 to length(SourceNode.NodeAttributes)-1 do
          begin
            DfltAttrib:=GetDefaultAttrib(SourceNode.NodeType,SourceNode.NodeAttributes[i].AttribName);
            if (SourceNode.NodeAttributes[i].AttribReadOnly=false)
            or (DfltAttrib.AttribIncludeInSave=true)            //!! ??
            or (SourceNode.NodeType='TXComposite')
            or (SourceNode.NodeType='TXCompositeIntf') then
            begin
              if SourceNode.NodeAttributes[i].AttribName<>'ParentName' then
              begin
                EditNodeAttributeValue(myself,SourceNode.NodeAttributes[i],true);
                if (SourceNode.NodeType='TXCompositeIntf')
                and (SourceNode.NodeAttributes[i].AttribHint<>'') then
                  myself.GetAttribute(SourceNode.NodeAttributes[i].AttribName,false).AttribHint:=SourceNode.NodeAttributes[i].AttribHint;
              end;
            end;
          end;
        end
        else
        begin
          myself:=SourceNode;
          AddChildToParentNode(ParentNode,myself,position);
        end;

        // now insert any child nodes
        for i:=0 to length(SourceNode.ChildNodes)-1 do
          InsertSystemNode(myself,SourceNode.ChildNodes[i],-1);
      end;
    end
    else if (SourceNode.NodeClass = 'Code')
      or (SourceNode.NodeClass = 'RUI') then        // for composite resources  ????
    begin
      myself:=SourceNode;
      myself.IsDynamic:=true;

      AddChildToParentNode(myparent,myself,position);
    end;
  end;
  //{$ifdef JScript} asm console.log('InsertSystemNode '+SourceNode.NodeType+' done'); end; {$endif}

  result:=myself;
end;

function AttribsFromXML(attributeList:TStringList;offset:integer;NodeClass,NodeType:String;var ParentName:String;NameSpace:String):TNodeAttributesArray;
var
  myAttribs:TNodeAttributesArray;
  dfltAttribs:TDefaultAttributesArray;
  LinkSet,AttribBits, sourceBits:TStringList;
  i,j:integer;
  tmp,anm:String;
  found:boolean;
begin
  //showmessage('AttribsFromXML. count='+inttostr(attributeList.count)+' offset='+inttostr(offset));
  setlength(myAttribs,0);
  j:=-1;
  for i:=offset to attributeList.count-1 do
  begin
    AttribBits :=  stringsplit(attributeList[i],AttribBitsDelimiter);
    anm:=TrimWhiteSpace(AttribBits[0]);
    // if this attribute is not listed in defaults for this nodetype, then ignore it. (unless it's a composite)
    if ((IsADefaultAttrib(NodeType,anm))
    or (NodeType='TXComposite')
    or (NodeType='TXCompositeIntf')
    or (NodeType='TXForm')
    or (NodeClass<>'UI'))
    and (AttribBits.Count>3) then      //name, type, value, readonly
    begin
      j:=j+1;
      setlength(myAttribs,j+1);
      myAttribs[j]:=TNodeAttribute.Create(nil,anm);
      myAttribs[j].AttribType:=TrimWhiteSpace(AttribBits[1]);
      myAttribs[j].AttribValue:=  UnSubstituteSpecials(AttribBits[2]);
      myAttribs[j].AttribReadOnly:=myStrToBool(TrimWhiteSpace(AttribBits[3]));
      if AttribBits.Count>4 then       // ..... , source
      begin
        tmp:=TrimWhiteSpace(AttribBits[4]);
        sourceBits:= stringsplit(tmp,'.');
        if sourceBits.Count>0 then
          if sourceBits[0]<>'' then
          begin
            myAttribs[j].AttribSource.InputNodeName:=sourceBits[0];
            if sourceBits.Count>1 then
              myAttribs[j].AttribSource.InputAttribName:=sourceBits[1];
  //          showmessage('AttribsFromXML found source data '+ sourceBits[0]+' '+sourceBits[1]+' for '+AttribBits[0]);
            myAttribs[j].AttribSource.InputNameSpace:=NameSpace;
          end;
      end;
      if AttribBits.Count>5 then       // ..... , hint
      begin
        tmp:=AttribBits[5];
        myAttribs[j].AttribHint:=tmp;
      end;
      if myAttribs[j].AttribName='ParentName' then
        ParentName:=AttribBits[2];
    end;
  end;

  // add any missing attribs (defined in default attribs)
  dfltAttribs := GetDefaultAttribs(NodeType);
  for i:=0 to length(dfltAttribs)-1 do
  if dfltAttribs[i].AttribIncludeInSave  then           // ???? mustn't do this for 'derived' attributes, like table numrows.
  begin
    j:=0;
    found:=false;
    while j < length(myAttribs) do
    begin
      if myAttribs[j].AttribName = dfltAttribs[i].AttribName then
      begin
        found:=true;
        j:=length(myAttribs);
      end;
      j:=j+1;
    end;
    if (not found)
    and (FindSuppressedProperty(NodeType,dfltAttribs[i].AttribName)<0) then
    begin
      setlength(myAttribs,length(myAttribs)+1);
      myAttribs[length(myAttribs)-1]:=TNodeAttribute.Create(nil,dfltAttribs[i].AttribName);
      myAttribs[length(myAttribs)-1].AttribType:=dfltAttribs[i].AttribType;
      myAttribs[length(myAttribs)-1].AttribValue:=dfltAttribs[i].AttribValue;
      myAttribs[length(myAttribs)-1].AttribReadOnly:=dfltAttribs[i].AttribReadOnly;
      myAttribs[length(myAttribs)-1].AttribHint:=dfltAttribs[i].AttribHint;
    end;
  end;


  result:=myAttribs;
end;

function EventsFromXML(eventsList:TStringList; var EventNames:TStringList):TEventHandlers;
var
  myEvents:TEventHandlers;
  AttribBits:TStringList;
  i:integer;
begin
  //showmessage('EventsFromXML. count='+inttostr(eventsList.count));
  setlength(myEvents,eventsList.count);
  for i:=0 to eventsList.count-1 do
  begin
    myEvents[i]:=TEventHandlerRec.Create;
    AttribBits :=  stringsplit(eventsList[i],AttribBitsDelimiter);
    EventNames.Add(TrimWhiteSpace(AttribBits[0]));
    myEvents[i].TheCode:=UnSubstituteSpecials(AttribBits[1]);
    if AttribBits.Count>2 then
    begin
      myEvents[i].InitCode:=UnSubstituteSpecials(AttribBits[2]);
    end;
    if AttribBits.Count>3 then
    begin
      myEvents[i].ReadOnlyInterface:=MyStrToBool(AttribBits[3]);
    end;
    if AttribBits.Count>4 then
    begin
      myEvents[i].EventHint:=AttribBits[4];
    end;
  end;

  result:=myEvents;
end;

procedure UpdateEvents(EventNames:TStringList;SourceHandlers:TEventHandlers;TargetNode:TDataNode);
var
  i,j:integer;
  found:Boolean;
  foundEvent:TEventHandlerRec;
begin
  for i:=0 to EventNames.Count-1 do
  begin
    found:=false;
    //showmessage('TargetNode '+TargetNode.NodeName+' eventTypes '+inttostr(TargetNode.myEventTypes.Count)+' handlers '+inttostr(length(TargetNode.myEventHandlers)));
    //setlength(TargetNode.myEventHandlers,TargetNode.myEventTypes.Count);
    foundEvent:=TargetNode.GetEvent(EventNames[i]);
    if foundEvent<>nil then
    begin
        found:=true;
        foundEvent.InitCode:=SourceHandlers[i].InitCode;
        foundEvent.TheCode:=SourceHandlers[i].TheCode;
        foundEvent.TheHandler:=nil;
    end
    else
    begin
      TargetNode.AddEvent(EventNames[i],SourceHandlers[i].TheCode,SourceHandlers[i].InitCode);
    end;
  end;
end;

function BuildSourceNodeFromXML(XMLString:String; var ParentName,NewNameSpace:String;ExpandingComposite:Boolean; var NodeClass:String):TDataNode;
var
     ScreenObjectName,ScreenObjectType,NameSpace:string;
     AttribsEvents:TStringList;
     attributeList:TStringList;
     EventsList, EventNames:TStringList;
     NameValuePair:TStringList;
     i,pos,offset:integer;
     myAttribs:TNodeAttributesArray;
     myEventHandlers:TEventHandlers;
     ParentNode:TDataNode;
     mynode,SourceNode, foundNode:TDataNode;
     NodeString,tmp:String;
begin
   result:=nil;
  //  ShowMessage('BuildSourceNodeFromXML  : '+XMLString);

   NodeString:=XMLString;

   attribsEvents:= stringsplit(NodeString,delimiterBetweenAttribsAndEvents);
   attributeList:= stringsplit(attribsEvents[0],attributeListdelimiter);
   if attribsEvents.Count>1 then
     EventsList:= stringsplit(attribsEvents[1],attributeListdelimiter)
   else
     EventsList:=TStringList.Create;

   // first find the node class, type and name
   ScreenObjectType:=attributeList[0];
   if ScreenObjectType='RawUnit' then ScreenObjectType:='PasUnit';
   if ScreenObjectType='DMClassRef' then ScreenObjectType:='DMContains';
   if ScreenObjectType='DMPkg' then
   begin
     ScreenObjectType:='';
     EXIT;
   end;

   NameValuePair :=  stringsplit(attributeList[1],NameValuePairdelimiter);
   NodeClass := TrimWhiteSpace(NameValuePair[1]);

   NameValuePair :=  stringsplit(attributeList[2],NameValuePairdelimiter);
   ScreenObjectName := TrimWhiteSpace(NameValuePair[1]);

   offset:=3;
   NameSpace:='';
   if FoundString(attributeList[3],'NameSpace')>0 then
   begin
     NameValuePair :=  stringsplit(attributeList[3],NameValuePairdelimiter);
     if NameValuePair.Count>1 then
       NameSpace := TrimWhiteSpace(NameValuePair[1])
     else
       NameSpace:='';
     offset:=4;
   end;
   NameSpace:=NewNameSpace+NameSpace;

   if (ExpandingComposite)
   and ((NodeClass = 'RUI') or (NodeClass  = 'Root')) then
   begin
     EXIT;  // return nil
   end;

   //showmessage(ScreenObjectType+' '+NodeClass+' '+ScreenObjectName);
   if ScreenObjectType='TXIFrame' then
   begin
     ScreenObjectType:='';
     ScreenObjectType:='TXIFrame';
   end;

   if (NodeClass <> 'Root') then                     // these already exist
   begin
     myAttribs := AttribsFromXML(attributeList,offset,NodeClass,ScreenObjectType,ParentName,NameSpace);
     {$ifndef JScript}
     foundNode:=FindDataNodeById(SystemNodeTree,ScreenObjectName,Namespace,false);
     if (foundNode=nil)
     // existing node....if it's the same element, we will just update it.
     or ((ScreenObjectType = foundNode.NodeType)
       and (NodeClass = foundNode.NodeClass)
       and (ParentName = FindParentOfNode(systemnodetree,foundNode).NodeName)
       )
     then
     {$endif}
     begin
       EventNames:=TStringList.Create;
       myEventHandlers := EventsFromXML(EventsList,EventNames);

       if foundNode=nil then
         myNode:=TDataNode.Create(NodeClass,ScreenObjectName,NameSpace,ScreenObjectType,true)
       else
         myNode:=foundNode;
       myNode.NodeAttributes:=myAttribs;

       i:=length(myNode.NodeAttributes);

       myNode.myEventTypes:=EventNames;
       myNode.myEventHandlers:=myEventHandlers;
       result:=myNode;
     {$ifndef JScript}
     end
     else
     begin
       showmessage('node name '+ScreenObjectName+' is not unique - cannot load');
     {$endif}
     end;
   end
   else  // 'Root' nodes...
   begin
     // update attributes for existing 'Root' node (eg. to capture DeploymentMode)
     myNode:=FindDataNodeById(SystemNodeTree,ScreenObjectName,'',false);
     if myNode<>nil then
     begin
       myAttribs := AttribsFromXML(attributeList,offset,NodeClass,ScreenObjectType,ParentName,'');
       for i:=0 to length(myAttribs)-1 do
       begin
         myNode.SetAttributeValue(myAttribs[i].AttribName,myAttribs[i].AttribValue);
       end;
       if myNode.NodeName = SystemRootName then
       begin
         EventNames:=TStringList.Create;
         myEventHandlers := EventsFromXML(EventsList,EventNames);
         UpdateEvents(EventNames,myEventHandlers,myNode);
       end;

     end;
   end;
end;

function addComponentFromXML(XMLString:String; DefaultParent:TDataNode; BaseNameSpace:String; ExpandingComposite:Boolean=false):Boolean;
 var
     ParentName,mf,ScreenObjectName,ScreenObjectType,NodeClass,tmp,NewNameSpace:string;
     i,pos:integer;
     ParentNode:TDataNode;
     mynode,SourceNode:TDataNode;
     myAttrib:TNodeAttribute;
     ok:Boolean;
{$ifdef JScript}
     fn:TAddComponentFunc;
     myDynamicWrapper:TWrapperPanel;
{$endif}

begin
  result:=true;
  //{$ifdef JScript} asm console.log('addComponentFromXML  : '+XMLString); end;{$endif}

  //loading a composite component direct from xml file: BaseNameSpace is '';  SourceNode.nameSpace is 'whatever'
  //expanding a new composite component from resource:  BaseNameSpace is 'parentname';  SourceNode.namespace is 'parentname'+'whatever'

 SourceNode:=BuildSourceNodeFromXML(XMLString, ParentName, BaseNameSpace, ExpandingComposite,NodeClass);

 if (SourceNode=nil)
 and (ExpandingComposite)
 and (NodeClass='RUI') then
   result:=false; //stop

 if SourceNode<>nil then
 begin
   NewNameSpace:=SourceNode.NameSpace;
   //showmessage('Building from SourceNode '+SourceNode.NodeType+' '+SourceNode.NameSpace+':'+SourceNode.NodeName);
   if (Not ExpandingComposite)
   or ((SourceNode.NodeType<>'TXMenuItem')
      and (SourceNode.NodeClass<>'RUI')) then
   begin
     ScreenObjectName:=SourceNode.NodeName;
     ScreenObjectType:=SourceNode.NodeType;
     ParentNode:=nil;

     if (SourceNode.NodeClass='Code')
     and (ExpandingComposite) then                  // code units loaded from composites - add to the composite node
     begin
       ParentNode:=DefaultParent;
     end
     else if ParentName<>'' then
     begin
       if SourceNode.NodeClass<>'DM' then
         ParentNode:=FindDataNodeByID(SystemNodeTree,ParentName,BaseNameSpace,false)
       else
       begin
         ParentNode:=FindDataNodeByID(DMRoot,ParentName,'',false);
         if ParentNode=nil then
           ParentNode:=DMRoot;
       end;

       //parent might be in same namespace
       if (ParentNode=nil) and (SourceNode.NameSpace<>'') then
         ParentNode:=FindDataNodeByID(SystemNodeTree,ParentName,SourceNode.NameSpace,false);
       //parent might be a composite within a namespace
       if (ParentNode=nil)
       and (SourceNode.NameSpace<>'') then
       begin
         // trim the parentname off the end of the namespace
         i:=FoundString(SourceNode.NameSpace,ParentName);
         if (i>1) and (i = length(SourceNode.NameSpace)-length(ParentName)+1) then
         begin
           tmp:=Copy(SourceNode.NameSpace,1,i-1);
           ParentNode:=FindDataNodeByID(SystemNodeTree,ParentName,tmp,false);
         end;
       end;
     end;

     if ParentNode=nil then
       ParentNode:=DefaultParent;   //UIRootNode;  // (eg. for TXForms)
     if (BaseNameSpace<>'')
     and (Parentname<>'CodeUnits')
     and (ParentNode.NameSpace<>BaseNameSpace)
     and (ParentNode.NodeType<>'TXComposite') then
         ParentNode:=DefaultParent;
     if (SourceNode.NodeClass='NV') and (SourceNode.NameSpace='') then
       ParentNode:=DefaultParent;

     // If the object was defined in a Lazarus form at design time,
     // the necessary data node (interface object) should already have been created.
     // Find it, add it to the identified parent, and set the relevant attributes.
     myNode:=nil;
     if SourceNode.NameSpace='' then
       myNode:=FindDataNodeById(SystemNodeTree,SourceNode.NodeName,'',false);

     {$ifndef JScript}
     // If the object was created dynamically at run time, however, then we now need to
     // create a data node for it.

     if myNode=nil then
     begin
       myNode:=SourceNode;
       // Create a screen object by running the registered instantiation function for the node type
       InsertSystemNode(ParentNode,myNode,-1);
     end
     else
     begin
       ReparentNode(myNode,ParentNode);
       pos:=ParentNode.GetChildIndex(myNode);
       if (ParentNode.ScreenObject<>nil) then
         InsertUnderParent(TControl(MyNode.ScreenObject),TWinControl(ParentNode.ScreenObject),pos);
       for i:=0 to length(SourceNode.NodeAttributes)-1 do
         if SourceNode.NodeAttributes[i].AttribReadOnly=false then        //!! ? should this be just the 'includeinsave' setting ????
         begin
           EditAttributeValue(myNode,SourceNode.NodeAttributes[i].AttribName,SourceNode.NodeAttributes[i].AttribValue);     //!!!! are we doing this twice??
         end;
       if myNode.IsDynamic then
       begin
         myNode.myEventTypes:=SourceNode.myEventTypes;
         ok:=CopyEventHandlers(myNode,SourceNode,true);
         if ok=false then
           showmessage('Component '+mynode.NodeName+' has mismatched event types - check user events code');
       end;
     end;
     {$else}
     //ShowMessage('addComponentFromXML  : '+XMLString);
     if myNode=nil then
     begin
       if ((SourceNode.NodeClass='UI') and (SourceNode.NodeType<>''))
       or ((SourceNode.NodeClass='NV') and (SourceNode.NodeType<>''))
       or (SourceNode.NodeClass = 'SVG') then
       begin
         //asm console.log('building dynamic node '+ScreenObjectType+' '+ScreenObjectName+' '+NewNameSpace); end;
         myDynamicWrapper:=TWrapperPanel(CreateInterfaceObject(ParentNode.MyForm,ScreenObjectType,ScreenObjectName,NewNameSpace));
         if myDynamicWrapper<>nil then
         begin
           myNode:=TDataNode(myDynamicWrapper);
           myDynamicWrapper.myNode:=myNode;

           myNode.myEventTypes:=SourceNode.myEventTypes;
           myNode.IsDynamic:=true;
           if SourceNode.myEventTypes.count<>length(SourceNode.myEventHandlers) then
             showmessage('Component '+SourceNode.NodeName+' has mismatched event types - check user events code')
           else
             myNode.myEventHandlers:=SourceNode.myEventHandlers;
         end;
         if mynode=nil then showmessage('mynode is nil in addComponentFromXML');
       end
       else if (SourceNode.NodeClass='Code')
         or (SourceNode.NodeClass='RUI')
         or (SourceNode.NodeClass='DM') then
       begin
         myNode:=SourceNode;
       end;
     end;
     if myNode<>nil then
     begin
       //asm console.log('created node.  Name='+mynode.NameSpace+'.'+mynode.NodeName+' dynamic='+mynode.IsDynamic); end;

       ReparentNode(myNode,ParentNode);

       if ((SourceNode.NodeClass='UI') and (SourceNode.NodeType<>''))
       or ((SourceNode.NodeClass='NV') and (SourceNode.NodeType<>''))
       or (SourceNode.NodeClass = 'SVG') then
       begin
         for i:=0 to length(SourceNode.NodeAttributes)-1 do
           if (SourceNode.NodeAttributes[i]<>nil)
           and (SourceNode.NodeAttributes[i].AttribName<>'') then
           begin
             myAttrib:=mynode.GetAttribute(SourceNode.NodeAttributes[i].AttribName,true);
             myAttrib.AttribType:=SourceNode.NodeAttributes[i].AttribType;
             myAttrib.AttribValue:=SourceNode.NodeAttributes[i].AttribValue;
             myAttrib.AttribHint:=SourceNode.NodeAttributes[i].AttribHint;
             myAttrib.AttribReadOnly:=SourceNode.NodeAttributes[i].AttribReadOnly;
             myAttrib.AttribSource:=SourceNode.NodeAttributes[i].AttribSource;
             //mynode.SetAttributeValue(SourceNode.NodeAttributes[i].AttribName,SourceNode.NodeAttributes[i].AttribValue);
             //!! ???? should we be checking here for 'non-default' attributes??

           end;
         if MainForm<>nil then
           mf:=MainForm.Name
         else
           mf:='.';

         pos:=-1;
         asm
           if (ScreenObjectName!=mf) {
             // object may already exist if this is a system re-load, so delete the old one.
             var ob = document.getElementById(NewNameSpace+ScreenObjectName);
             if (ob!=null) {
               //alert('looking for inner component of parent '+ParentNode.NodeName);
               var Parent = pas.HTMLUtils.ScreenObjectInnerComponent(ParentNode);
               if (Parent!=null) {
                  //console.log('removing object '+NewNameSpace+ScreenObjectName+' while loading '+XMLString);
                  pos=Array.from(ob.parentNode.children).indexOf(ob);
                  Parent.removeChild(ob); }
             }
           }
         end;
         // Create a screen object by running the registered instantiation function for the node type
         fn:=LookupComponentFunc(SourceNode.NodeType);
         fn(myNode,ParentNode,SourceNode.NodeName,NewNameSpace,pos,'Left');        //!! set initial alignment from source?
       end;

       if myNode.HasAttribute('SuspendRefresh') then
         myNode.SetAttributeValue('SuspendRefresh','False');
     end;
     {$endif}
    end;
  end;

end;
{$ifndef JScript}
procedure myCopyToClip(stringname,instring:string);
begin
  Clipboard.AsText :=instring;
  Showmessage('The '+stringname+' has been saved to the Clipboard');
end;


{$else}

function mygetClipboardData(stringname:string):string;
begin
  // set 'normal' appearance for paste dialog form
  PasteDialogUnit.PasteDoneBtn.IsVisible:=false;
  PasteDialogUnit.PasteLabel.LabelCaption:='Waiting for a copy/paste action';
  PasteDialogUnit.PasteTarget.IsVisible:=true;

  OpenModal('PasteDialog');

  asm
  try {
         var pasteTarget = document.getElementById('PasteTargetContents');
         pasteTarget.spellcheck=false;

         pasteTarget.value= '';
         pasteTarget.style.height =  '19px';
         pasteTarget.style.width =  '100px';
         pasteTarget.focus();
         alert('After closing this message box, Confirm Paste from clipboard by hitting "Ctrl-V"  Any other action will abandon this paste operation');

     } catch(err) { alert(err.message+'  in NodeUtils.myGetClipboardData'); }
  end;

  Result :=  'Wait for Dialog To Close';
end;

function closeClipboardPasteDialog(myValue:String):string;
begin
      asm
      try {
             myTimeout(function() {
                try {
                 var pasteTarget = document.getElementById('PasteTargetContents');
                  var PasteString = myValue;
                  // myValue should hold the pasted data direct from the clipboard.
                  // However if it's blank, revert to the text content of pasteTarget...
                  if (PasteString=='') {PasteString = pasteTarget.value};

                  pas.XForm.CloseModal('PasteDialog','');

                 if (pas.PasteDialogUnit.CompletionEvent!=null) {
                    //alert('call completion event '+pas.PasteDialogUnit.CompletionEvent.EventType+' '+pas.PasteDialogUnit.CompletionEvent.NodeId);
                    pas.PasteDialogUnit.CompletionEvent.ReturnString=PasteString;
                    //console.log('calling handleevent '+ pas.PasteDialogUnit.CompletionEvent.EventType);
                    //console.log(pas.PasteDialogUnit.CompletionEvent);
                    pas.Events.handleEvent(pas.PasteDialogUnit.CompletionEvent,
                                           pas.PasteDialogUnit.CompletionEvent.EventType,
                                           pas.PasteDialogUnit.CompletionEvent.NodeId,
                                           pas.PasteDialogUnit.CompletionEvent.NameSpace,
                                           PasteString,'');
                    pas.PasteDialogUnit.CompletionEvent=null;
                  }
                } catch(err) { alert(err.message+'  in NodeUtils.closeClipboardPasteDialog'); }
             }, 10);
         } catch(err) { alert(err.message+'  in NodeUtils.closeClipboardPasteDialog'); }
      end;

      Result :=  '';
end;

function FinishHTMLPasteAction(myValue:string):string;
begin
    // HTML only
    closeClipboardPasteDialog(myValue);

end;


procedure myCopyToClip(stringname,instring:string);
begin

   asm
   try {

   // Register a copy handler which will directly add the text to the DataTransfer object.
     function copyHandler(event) {
       if (event.clipboardData) {
         event.clipboardData.setData("text/plain", instring);
       }
       else {
         // Internet Explorer is special, and needs to be given both a different
         // MIME type, and have the method called on a different object.
         clipboardData.setData("Text", instring);
       }
       event.preventDefault();
     }
   document.addEventListener("copy", copyHandler);

   // Create a dummy text area and select it, as Safari, Chrome and Edge require the
   // text area to be selected to trigger an execCommand("copy") event.
   var textField=document.getElementById('NodeUtilsCopyArea');
   if (textField==null) {
     textField = document.createElement('textarea');
     textField.id = 'NodeUtilsCopyArea';
     document.body.appendChild(textField);
     }
   textField.value = 'a';             //something very short (quick)
   textField.focus();
   textField.select();

   // Trigger the copy command,
   var successful = document.execCommand("copy");

   // and then remove the event listener
   document.removeEventListener("copy", copyHandler);
   textField.remove();

   var msg = successful ? 'successful' : 'unsuccessful';
   alert('Saving the '+stringname+' to the Clipboard was ' + msg);


    } catch(err) { alert(err.message+'  in NodeUtils.myCopyToClip'); }
end;

end;



//procedure TInterfaceObject.SetLink(const AValue: TXPropertyLink);
//begin
//  if FLink=AValue then exit;
//  FLink:=AValue;
//end;
//
//function LinkToStr(ALink:TXPropertyLink):string;
//begin
//  result:=ALink.TIObjectName;
//  result:=result + AttribLinkDelimiter + ALink.TIPropertyName;
//end;
//
//
//procedure TInterfaceObject.LinkLoadFromProperty(Sender: TObject);
//begin
//  if Sender=nil then ;
//
//  //writeln('TTICustomEdit.LinkLoadFromProperty A ',Name,
//  //  ' FLink.GetAsText=',FLink.GetAsText,' Text=',Text,
//  //  ' PropName=',FLink.TIPropertyName);
//  //showmessage('loadfromproperty');
//
//  SetAttributeValue('Link',LinkToStr(Link));
//
//end;
//
//procedure TInterfaceObject.LinkSaveToProperty(Sender: TObject);
//begin
//end;

procedure SetInterfaceProperty(myName,NameSpace,PropName,NewValue:string);
// set the property value on the interface object
var
  myObj:TObject;
  MyPropType:TTypeKind;
  valueStr:String;
  i:NativeInt;
begin
  valueStr:=NewValue;

  myObj:=TObject(FindDataNodeById(SystemNodeTree,myName,NameSpace,false));
  if myObj<>nil then
  begin
    //showmessage('setintfprop. name='+NameSpace+' '+myname+' prop='+PropName+' value=>'+NewValue+'<');
    MyPropType := PropType(myObj, PropName);
    if MyPropType = tkString then
    begin
      //showmessage('set string prop '+PropName+' value='+NewValue);
      SetStringProp(myObj,PropName,NewValue);
    end
    else if MyPropType = tkBool then
    begin
      //showmessage('set boolean prop '+PropName);
      SetBoolProp(myObj,PropName,myStrToBool(NewValue));
    end
    else if MyPropType = tkInteger then
    begin
      i:= StrToInt(valueStr);
      //showmessage('int value = '+IntToStr(i));
      SetNativeIntProp(myObj,PropName,i);
    end
    else
      showmessage('SetInterfaceProperty.  Need to handle property type for '+PropName);
  end
  else
    showmessage('SetInterfaceProperty cannot find node '+myName);
end;

function LookupNodeInObFunc(NodeType:string):TCreateInObFunc;
var
  i:integer;
begin
  i:=0;
  while i < length(NodeFuncsLookup) do
  begin
    if NodeFuncsLookup[i].NodeType=NodeType then
    begin
      result:= NodeFuncsLookup[i].InObFunctionPtr;
      i:=length(NodeFuncsLookup);
    end;
    i:=i+1;
  end;
end;

procedure InitFormObject(myForm:TForm;NodeName:String);
// Called on loading the system (statements in the file systemintface.inc)
// This generates a 'Form' object of type TXForm for the JS/HTML system.
var
  myNode:TDataNode;
begin
  myForm.Name:=NodeName;
  myNode:=AddFormToNodeTree(myForm);
end;

function CreateInterfaceObject(MyForm:TForm;NodeType, NodeName,NameSpace:String):TObject;
var
  myObj:TObject;
  inobFn:TCreateInObFunc;
  mynode:TDataNode;
begin
  //showmessage('CreateInterfaceObject '+NodeType+' '+NodeName);
  // Find the interface object and link to this node
  // Object creation procs are stored in NodeFuncLookup.
  // Find the creation func for this node type, and execute it.
  inobFn:=LookupNodeInObFunc(NodeType);
  if inobFn=nil then
    showmessage('no interface object creation function for '+myNode.NodeName)
  else
  begin
    //showmessage('function found');
    myObj:=inobFn(MyForm,NodeName,NameSpace);
  end;
  result:=myObj;
end;
{$endif}



function CreateFormNode(myForm:TForm):TDataNode;
var
  myNode:TDataNode;
begin
  {$ifndef JScript}
  myNode:=nil;
  myNode:=TDataNode.Create('UI',myForm.Name,'','TXForm',false);      //!!!!namespace
  myNode.ScreenObject:=myForm;
  myNode.MyForm:=myForm;
  if myForm<>MainForm then
  begin
    myNode.SetAttributeValue('Top',inttostr(myForm.Top));
    myNode.SetAttributeValue('Left',inttostr(myForm.Left));
    myNode.SetAttributeValue('Height',inttostr(myForm.Height));
    myNode.SetAttributeValue('Width',inttostr(myForm.Width));
    myNode.SetAttributeValue('Caption',myForm.Caption);
    myNode.SetAttributeValue('BgColor',ColorToHexRGB(myForm.Color));
  end;
  {$else}
  SetLength(TXForm(myForm).ChildNodes,0) ;
  SetLength(TXForm(myForm).NodeAttributes,0);
//  TXForm(myForm).myEventTypes:=TStringList.Create;
  SetLength(TXForm(myForm).myEventHandlers,0);
  TXForm(myForm).IsDynamic:=false;
  TXForm(myForm).NodeName:=myForm.Name;
  TXForm(myForm).NodeClass:='UI';
  TXForm(myForm).NodeType:='TXForm';
  TXForm(myForm).ScreenObject:=myForm;
  TXForm(myForm).MyForm:=myForm;
  myNode:=TXForm(myForm);
  {$endif}
  TXForm(myForm).myNode:=myNode;
  TXForm(myForm).SetFormEventTypes;

  result:=myNode;
end;

function AddFormToNodeTree(myForm:TForm):TdataNode;
var
  myNode:TDataNode;
begin
  myNode:=CreateFormNode(myForm);
  AddChildToParentNode(UIRootNode,myNode,-1);

  result:=myNode;
end;

function InitialiseCodeTree:TdataNode;
var
  myNode:TDataNode;
begin
  myNode:=nil;
  myNode:=TDataNode.Create('Code','CodeUnits','','Root',false);

  AddChildToParentNode(SystemNodeTree,myNode,-1);

  result:=myNode;
end;


procedure ReParentNode(MyNode,NewParent:TDataNode);
var
  OldParent:TDataNode;
begin
  // called during load from XML
//  {$ifndef JScript}
//  OldParent:=FindParentOfNode(SystemNodeTree,MyNode);
//  {$else}
//  OldParent:=FindParentOfNodeByName(SystemNodeTree,MyNode.NodeName,MyNode.NameSpace,false);
//  {$endif}
  OldParent:=MyNode.NodeParent;
  if (OldParent<>NewParent) then
  begin
    if (OldParent<>nil) then
    begin
      OldParent.RemoveChildNode(MyNode);
    end;
    AddChildToParentNode(NewParent,MyNode,-1);
  end;
end;

function FindNodesOfType(StartNode:TDataNode;NodeType:String;InNameSpace:Boolean=false;NameSpace:String=''):TNodesArray;
var
  newNodes,descendants:TNodesArray;
  i,j,l:integer;
begin
  setlength(newNodes,0);
  for i:=0 to length(StartNode.ChildNodes)-1 do
  begin
    if (StartNode.ChildNodes[i].NameSpace = NameSpace)
    or (InNameSpace=false) then
    begin
      if StartNode.ChildNodes[i].NodeType=NodeType then
      begin
        setlength(newNodes,length(NewNodes)+1);
        newNodes[length(NewNodes)-1]:=StartNode.ChildNodes[i];
      end;
      descendants:=FindNodesOfType(StartNode.ChildNodes[i],NodeType,InNameSpace,NameSpace);
    end;
    l:=length(newNodes);
    setLength(newNodes,l+length(descendants));
    for j:=0 to length(descendants)-1 do
    begin
      newNodes[l+j]:=descendants[j];
    end;
  end;
  result:=newNodes;
end;

procedure ClearAllDynamicNodes(StartNode:TDataNode);
var
  i:integer;
begin
  if StartNode=UIRootNode then
  begin
     for i:=0 to length(StartNode.myEventHandlers)-1 do
     begin
       if StartNode.myEventHandlers[i]=nil then
         StartNode.myEventHandlers[i]:=TEventHandlerRec.Create;
       StartNode.myEventHandlers[i].InitCode:='';
       StartNode.myEventHandlers[i].TheCode:='';
       StartNode.myEventHandlers[i].TheHandler:=nil;
       StartNode.myEventHandlers[i].ReadOnlyInterface:=false;
       StartNode.myEventHandlers[i].EventHint:='';
     end;
  end;
  for i:=length(StartNode.ChildNodes)-1 downto 0 do
  begin
    ClearAllDynamicNodes(StartNode.ChildNodes[i]);
    if StartNode.ChildNodes[i].IsDynamic then
      DeleteNode(StartNode,StartNode.ChildNodes[i]);
  end;
end;

procedure NilScreenObject(MyNode:TdataNode);
var
  i:integer;
begin
  if (MyNode.ScreenObject<>nil) then
  begin
    MyNode.ScreenObject:=nil;
  end;


  for i:=0 to length(MyNode.ChildNodes)-1 do
  begin
     NilScreenObject(MyNode.ChildNodes[i]);
  end;
end;

procedure SetXObjectProperty(myObj:TObject;targetNode:TdataNode;PropName,AttrType,newValue:String);
var
  pType:TTypeKind;
  tf:Boolean;
  int:integer;
  arr:TStringArray;
  parr:Pointer;
begin
  if (myObj<>nil)
  and (IsPublishedProp(myObj,PropName)) then
  begin
    pType := PropType(myObj,PropName);
    {$ifndef JScript}
    if pType in [tkSString,tkLString,tkAString,tkWString] then
    begin
      SetStrProp(myObj,PropName,newValue);
    end
    else if pType in [tkInteger] then
    begin
        //!! problem if the property is a TColor.....strtoint no good (eg. value is #FF2299)
        if AttrType='Color' then
        begin
          SetBooleanProperty(myObj,'ParentColor',false) ;
          SetOrdProp(myObj,PropName,HexRGBToColor(newValue));
        end
        else if newValue<>'' then
          SetOrdProp(myObj,PropName,strtoint(newValue));
    end
    else if pType in [tkBool] then
    begin
       tf:=MyStrToBool(newValue);
       int:=integer(tf);
       SetOrdProp(myObj,PropName,int);
    end
    {$ifdef windows}            // SetDynArrayProp missing .... check versions tbd!!!!
    else if pType in [tkDynArray] then
    begin
      arr:=CommaListToStringArray(newValue);
      parr:=Pointer(arr);
      SetDynArrayProp(myObj,PropName,parr);
    end
    {$endif}
    {$else}
    if pType = tkString then
    begin
      SetStringProp(myObj,PropName,NewValue);
    end
    else if pType = tkInteger then
    begin
      //showmessage('setting integer property');
      SetNativeIntProp(myObj,PropName,strtoint(newValue));
    end
    else if pType = tkBool then
    begin
      SetBoolProp(myObj,PropName,myStrToBool(newValue));
    end
    else if pType = tkDynArray then
    begin
      //showmessage('... property '+PropName+' needs tkDynArray type');       //!!!!
      targetNode.SetAttributeValue(PropName,newValue);
    end
    {$endif}
    else
      showmessage('EditAttributeValue. ** Need to handle property type for '+targetNode.NodeName+'.'+PropName);
  end;
end;


function FindInterfaceNode(StartNode:TDataNode;NameSpace:String;PropName:String):TdataNode;
var
  SearchNode, InterfaceNode:TDataNode;
  i:integer;
begin
  // find an interface node within this namespace, with the required attribute
  InterfaceNode:=nil;
  if (StartNode.NodeType='TXCompositeIntf')
  and (StartNode.NameSpace = NameSpace)
  and (StartNode.HasAttribute(PropName)) then
  begin
    InterfaceNode:=StartNode;
  end
  else
  begin
     i:=0;
     while (i < length(StartNode.ChildNodes))
     and (InterfaceNode = nil) do
     begin
       SearchNode:=StartNode.ChildNodes[i];
       InterfaceNode := FindInterfaceNode(SearchNode,NameSpace,PropName);
       i:=i+1;
     end;
   end;
   result:=InterfaceNode;
end;

function AttributeValuesEquivalent(Value1,Value2,AttribType:String):Boolean;
begin
  if AttribType='String' then
    result:=(Value1=Value2)
  else if (AttribType='Boolean') or (AttribType='Color') then
    result:=(UpperCase(Value1)=UpperCase(Value2))
  else if AttribType='Integer' then
    result:=(strtoint(Value1)=strtoint(Value2))
  else
    result:=(Value1=Value2);
end;

procedure PushThisAttributeValue(i:integer;SourceNode:TDataNode);
var
  TargetAttrib,SourceAttrib:TNodeAttribute;
  TargetNode:TDataNode;
begin
  TargetNode:=SourcedAttribs[i].TheNode;
  SourcedAttribs[i].InProgress:=true;
  SourceAttrib:=SourceNode.GetAttribute(SourcedAttribs[i].TheAttribute.AttribSource.InputAttribName,false);
  TargetAttrib:=SourcedAttribs[i].TheAttribute;
  //TargetAttrib.AttribReadOnly:=SourceAttrib.AttribReadOnly;
  TargetAttrib.AttribValue:=SourceAttrib.AttribValue;
  EditNodeAttributeValue(TargetNode,TargetAttrib,false);
  //EditNodeAttributeValue will change the associated screen object property.
  //...which will fire 'change' events on the object
  //...and will also call PushSourceToAttributes to daisy-chain the change to target nodes.

  SourcedAttribs[i].InProgress:=false;
end;

procedure PushSourceToAttributes(FromNode:TDataNode; FromAttrib:TNodeAttribute);
var
  i:integer;
  //TargetNode:TDataNode;

begin
  // use the 'quick list' of sourced attributes (SourcedAttribs)...
  // eg. built on entry to run mode (XIDE project)
  // record:  TSourcedAttrib
  if (not StartingUp)
  and (length(SourcedAttribs)>0) then
  begin
    for i:=0 to length(SourcedAttribs)-1 do
    begin
      //TargetNode:=SourcedAttribs[i].TheNode;
      //refresh the current value of the target attribute in the SourcedAttribs list...
      //SourcedAttribs[i].TheAttribute.AttribValue:=TargetAttrib.AttribValue;

      if (SourcedAttribs[i].TheAttribute.AttribSource.InputNodeName = FromNode.NodeName)
      and (SourcedAttribs[i].TheAttribute.AttribSource.InputNameSpace = FromNode.NameSpace)
      and (SourcedAttribs[i].TheAttribute.AttribSource.InputAttribName = FromAttrib.AttribName)
      and (not AttributeValuesEquivalent(SourcedAttribs[i].TheAttribute.AttribValue,FromAttrib.AttribValue,SourcedAttribs[i].TheAttribute.AttribType))
      and (SourcedAttribs[i].InProgress=false)
      then
      begin
        PushThisAttributeValue(i,FromNode);
      end;
    end;
  end;
end;

procedure PushAllSourcesToAttributes;
var
  i,iteration:integer;
  TargetAttrib,SourceAttrib:TNodeAttribute;
  SourceNode,TargetNode:TDataNode;
  Done:boolean;

  function SelfRefFound(thisAttrib:TNodeAttribute):Boolean;
  var
    testAttr:TNodeAttribute;
    source:TAttribSourceRec;
    sourceNode:TDataNode;
    sourceAttr:TNodeAttribute;
  begin
    result:=false;
    testAttr:=thisAttrib;
    while (testAttr<>sourceAttr)
    and (testAttr<>nil) do
    begin
      source:=testAttr.AttribSource;
      sourceNode:=FindDataNodeById(SystemNodeTree,source.InputNodeName,source.InputNameSpace,true);
      if sourceNode<>nil then
      begin
        sourceAttr:=sourceNode.GetAttribute(source.InputAttribName,false);
        if testAttr=sourceAttr then
        begin
          result:=true;
          EXIT;
        end;
        testAttr:=sourceAttr;
      end
      else
        EXIT;
    end;
  end;

begin
  // use the 'quick list' of sourced attributes (SourcedAttribs)...
  // eg. built on entry to run mode (XIDE project)
  // record:  TSourcedAttrib

  // look for circular references...
  for i:=0 to length(SourcedAttribs)-1 do
  begin
    if SelfRefFound(SourcedAttribs[i].TheAttribute) then
    begin
      showmessage('There is a circular reference in sourced attributes');
      EXIT;
    end;
  end;

  iteration:=0;
  if length(SourcedAttribs)>0 then
  begin
    //showmessage('PushAllSourcesToAttributes');
    Done:=false;
    iteration:=iteration+1;
    if iteration<50 then          // fail-safe?
    while Done=false do
    begin
      Done:=true;
      for i:=0 to length(SourcedAttribs)-1 do
      begin
        begin
          //SourcedAttribs[i] is the attribute which contains a source reference
          TargetNode:=SourcedAttribs[i].TheNode;
          TargetAttrib:=SourcedAttribs[i].TheAttribute;
          //refresh the current value of the target attribute in the SourcedAttribs list...
          TargetAttrib.AttribValue:=TargetNode.GetAttribute(TargetAttrib.AttribName,false).AttribValue;

          SourcedAttribs[i].InProgress:=true;     //prevent circular updates

          if SourcedAttribs[i].SourceNode<>nil then
          begin
            SourceNode:=SourcedAttribs[i].SourceNode;
            SourceAttrib:=SourceNode.GetAttribute(TargetAttrib.AttribSource.InputAttribName,false);
            if not AttributeValuesEquivalent(TargetAttrib.AttribValue,SourceAttrib.AttribValue,TargetAttrib.AttribType) then
            begin
              Done:=false;
              //showmessage('PushAllSourcesToAttributes  src='+SourceNode.NodeName+' '+SourceAttrib.AttribName);
              PushThisAttributeValue(i,SourceNode);
            end;
          end;
          SourcedAttribs[i].InProgress:=false;
        end;
      end;

    end;
  end;
end;

procedure PushNodeSourcesToAttributes(FromNode:TDataNode);
var
  i:integer;
  TargetAttrib,SourceAttrib:TNodeAttribute;
  SourceNode,TargetNode:TDataNode;
begin
  // use the 'quick list' of sourced attributes (SourcedAttribs)...
  // eg. built on entry to run mode (XIDE project)
  // record:  TSourcedAttrib
  if length(SourcedAttribs)>0 then
  begin
    for i:=0 to length(SourcedAttribs)-1 do
    begin
      begin
        TargetNode:=SourcedAttribs[i].TheNode;
        //refresh the current value of the target attribute in the SourcedAttribs list...
        SourcedAttribs[i].TheAttribute.AttribValue:=TargetNode.GetAttribute(SourcedAttribs[i].TheAttribute.AttribName,false).AttribValue;

        //SourcedAttribs[i] is the attribute which contains a source reference
        if SourcedAttribs[i].SourceNode = FromNode then
        begin
          PushThisAttributeValue(i,FromNode);
        end;
      end;
    end;
  end;
end;


procedure EditNodeAttributeValue(targetNode:TDataNode; SourceAttrib:TNodeAttribute;AddIfMissing:Boolean);
var
    targetAttrib,SourcedAttrib:TNodeAttribute;
    myObj:TObject;
    tmp,AttrNameToEdit,newValue:String;
    IsReadOnly:Boolean;
    i:integer;
begin
  AttrNameToEdit:=SourceAttrib.AttribName;
  newValue:=SourceAttrib.AttribValue;
  IsReadOnly:=SourceAttrib.AttribReadOnly;
  targetAttrib:=targetNode.GetAttributeAnyCase(AttrNameToEdit,AddIfMissing);
  if (targetAttrib<>nil) then
  begin
    targetAttrib.AttribReadOnly:=IsReadOnly;
    if (SourceAttrib.AttribType<>'String')
    and (targetAttrib.AttribType='String') then
      targetAttrib.AttribType:=SourceAttrib.AttribType;

    {$ifndef JScript}
    if targetNode.ScreenObject=nil then
      myObj:=targetNode
    else
      myObj:=targetNode.ScreenObject;
    {$else}
    myObj:=targetNode;
    {$endif}

    if (myObj<>nil)
    //and (IsReadOnly=false)           //????
    and (IsPublishedProp(myObj,AttrNameToEdit)) then
    begin
      SetXObjectProperty(myObj,targetNode,AttrNameToEdit,targetAttrib.AttribType,newValue);
    end
    else
    begin
      // no screen or interface object associated with this node - just set the node attribute value
      targetNode.SetAttributeValue(AttrNameToEdit,newValue,targetAttrib.AttribType,IsReadOnly);
    end;

    // If this is a dynamic node, then the attribute source may also need to be set.
    if targetNode.IsDynamic=true then
    begin
      if SourceAttrib.AttribSource.InputNodeName<>'' then
      begin
        SourcedAttrib:=TargetNode.GetAttribute(SourceAttrib.AttribName,false);
        //for i:=0 to length(TargetNode.NodeAttributes)-1 do
        //  if TargetNode.NodeAttributes[i].AttribName = SourceAttrib.AttribName then
        //    TargetNode.NodeAttributes[i].AttribSource:=SourceAttrib.AttribSource;
        SourcedAttrib.AttribSource:=SourceAttrib.AttribSource;
      end;
      // If this is a dynamic node, then this may be a source for other node attribute(s).
      // Find them, and push the new value.
      PushSourceToAttributes(targetNode,targetAttrib);
    end;

  end;
end;

procedure EditAttributeValue(NodeNameToEdit,NameSpace:String; SourceAttrib:TNodeAttribute;AddIfMissing:Boolean=false);
var
  targetNode:TDataNode;
begin
  targetNode:=FindDataNodeById(SystemNodetree,NodeNameToEdit,NameSpace,true);
  if targetNode<>nil then
  begin
    EditNodeAttributeValue(targetNode,SourceAttrib,AddIfMissing);
  end;
end;

procedure EditAttributeValue(NodeToEdit:TDataNode;AttrNameToEdit,newValue:String;AddIfMissing:Boolean=false);
var
  SourceAttrib,NodeAttrib:TNodeAttribute;
begin
  NodeAttrib:=NodeToEdit.GetAttributeAnyCase(AttrNameToEdit,AddIfMissing);
  if NodeAttrib<>nil then
  begin
    SourceAttrib:=TNodeAttribute.Create(nil,AttrNameToEdit);
    SourceAttrib.AttribValue:=newValue;
    SourceAttrib.AttribType:=NodeAttrib.AttribType;
    SourceAttrib.AttribReadOnly:=NodeAttrib.AttribReadOnly;
    EditNodeAttributeValue(NodeToEdit,SourceAttrib,AddIfMissing);
    SourceAttrib.Free;
  end;
end;

procedure EditAttributeValue(NodeNameToEdit,NameSpace:String; AttrNameToEdit,newValue:String;AddIfMissing:Boolean=false;Context:TDataNode=nil);
var
  targetNode:TDataNode;
begin
  if Context=nil then
    targetNode:=FindDataNodeById(SystemNodetree,NodeNameToEdit,NameSpace,true)
  else
    targetNode:=FindDataNodeById(Context,NodeNameToEdit,NameSpace,true);
  if targetNode<>nil then
    EditAttributeValue(targetNode,AttrNameToEdit,newValue,AddIfMissing);
end;

{$ifndef JScript}
procedure EditAttributeValue(NodeNameToEdit,NameSpace,AttrNameToEdit,newValue:PChar);
begin
  EditAttributeValue(NodeNameToEdit,NameSpace,AttrNameToEdit,StrPas(newValue),false);
end;
procedure EditAttributeValue(NodeToEdit:TDataNode;AttrNameToEdit,newValue:PChar);
begin
  EditAttributeValue(NodeToEdit,AttrNameToEdit,StrPas(newValue),false);
end;
{$else}
procedure EditAttributeValue2(NodeNameToEdit,NameSpace,AttrNameToEdit,newValue:String); // for direct calls via JS (doesn't handle overloads)
begin
  EditAttributeValue(NodeNameToEdit,NameSpace,AttrNameToEdit,newValue,false);
end;
{$endif}


procedure EditAttributeValueIndexed(NodeNameToEdit,NameSpace,AttrNameToEdit:String;newValue:TStringArray; x,y:integer);
var
  targetNode:TDataNode;
  targetAttrib:TNodeAttribute;
  myObj:TObject;
  pType:TTypeKind;
  tmp:String;
  arr:TStringArray;
  parr:Pointer;
begin
  targetNode:=FindDataNodeById(SystemNodetree,NodeNameToEdit,NameSpace,true);
  if targetNode<>nil then
  begin
    targetAttrib:=targetNode.GetAttributeAnyCase(AttrNameToEdit);
    if targetAttrib<>nil then
    begin
      AttrNameToEdit:=targetAttrib.AttribName;

      if targetNode.ScreenObject=nil then
        myObj:=targetNode
      else
        myObj:=targetNode.ScreenObject;

      if (myObj<>nil) then
      //and (IsPublishedProp(myObj,AttrNameToEdit)) then
      begin
       // pType := PropType(myObj,AttrNameToEdit);
       // if pType in [tkDynArray] then
        begin
          // For efficiency, updating just the widget property here (NOT the node attribute value)
          if (myObj is TXBitMap) then
            if AttrNameToEdit='MapPixelArray' then
              TXBitMap(myObj).SetMapPixelArraySection(newValue,x,y);
        end;
      //  else
      //    showmessage('EditAttributeValueIndexed. ** Property type is not an array - '+NodeNameToEdit+'.'+AttrNameToEdit);
      end;

    end;

  end;
end;


{$ifndef JScript}


function FindOuterParentOf(InnerControl:TControl):TControl;
var
  innerName:String;
begin
  innerName:=InnerControl.Name;
  if (InnerControl.Name<>'') and (FoundString(InnerControl.Name,'Contents')=0) then
    result:=InnerControl
  else if InnerControl.Parent<>nil then
    result:=FindOuterParentOf(InnerControl.Parent)
  else
    result:=nil;
end;



{$Else}
//function DeleteScreenObject(MyNode:TDataNode):string;
//var
//    ObjName:string;
//begin
//   ObjName:=MyNode.NameSpace+MyNode.NodeName;
//
//  asm
//    try{
//    var ThisObject = document.getElementById(ObjName);
//    if (ThisObject!=null) {
//       ThisObject.parentNode.removeChild(ThisObject);
//      }
//    }catch(err) { alert(err.message+' in NodeUtils.DeleteScreenObject');}
//  end;
//
//  NilScreenObject(MyNode);
//
//end;
{$endif}

procedure DeleteNodeChildren(ParentNode:TDataNode);
var
    i:integer;
    handlers:TEventHandlers;
begin
  //ShowMessage('dnc '+ParentNode.NameSpace+' '+ParentNode.NodeName);
  if ParentNode=nil then
    ShowMessage('parentnode nil in DeleteNodeChildren')
  else
  begin
     // delete the child screenobjects
     for i:=0 to length(ParentNode.ChildNodes)-1 do
     begin
        //ShowMessage('i='+inttostr(i)+' deleting children of node '+ParentNode.ChildNodes[i].NodeName);
        setlength(handlers,0);
        ParentNode.ChildNodes[i].MyEventHandlers:=handlers;
        DeleteNodeChildren(ParentNode.ChildNodes[i]);
        //ShowMessage('i='+inttostr(i)+' deleting screenobject '+ParentNode.ChildNodes[i].NodeName);
        DeleteScreenObject(ParentNode.ChildNodes[i]);
        //ShowMessage('i='+inttostr(i)+' deleting node '+ParentNode.ChildNodes[i].NodeName);
        ParentNode.ChildNodes[i].DeleteMe;
     end;
     // Clear out all the child nodes
     setlength(ParentNode.ChildNodes,0);
   end;
end;

procedure DeleteNode(ParentNode,MyNode:TDataNode);
var
    handlers:TEventHandlers;
    i,j:integer;
    supp:Boolean;
begin
  supp:=suppressEvents;
  SuppressEvents:=true;
  setlength(handlers,0);
  MyNode.MyEventHandlers:=handlers;
  // delete my child nodes
  DeleteNodeChildren(MyNode);
  // delete my screen object
  DeleteScreenObject(MyNode);
  // remove me from my parent
  if ParentNode=nil then
    ParentNode:=FindParentOfnode(SystemNodeTree,MyNode);
  if ParentNode<>nil then
  begin
    ParentNode.RemoveChildNode(MyNode);
  end;

  MyNode.DeleteMe;
  SuppressEvents:=supp;
end;

procedure UnSuspendFrames(StartNode:TdataNode);
var
  i:integer;
  glb:Boolean;
begin
  if StartNode.IsDynamic then
    if StartNode.HasAttribute('SuspendRefresh') then
      EditAttributeValue(StartNode.NodeName,StartNode.NameSpace,'SuspendRefresh','False');
  for i:=0 to length(StartNode.ChildNodes)-1 do
    UnSuspendFrames(StartNode.ChildNodes[i]);
end;

function checkData(SystemDescription:string):boolean;
var teststring,teststring2,sys:string;    // this checks a longer string after un encryption than the function isvalidsystemdata
   i:integer;
   MatchFound:boolean;
begin
  MatchFound:=true;
  sys:=trim(SystemDescription);
  teststring :='<Root|; Class |=R';
  for i :=1 to Length(teststring) do
  begin
     if (Sys[i]<> teststring[i])
     then  MatchFound:=false;
  end;
  if MatchFound=false then
  begin
    MatchFound:=true;
    teststring :='<TXComposite';
    for i :=1 to Length(teststring) do
    begin
       if (Sys[i]<> teststring[i])
       then  MatchFound:=false;
    end;
  end;
  result:=MatchFound;
end;


function XMLToNodeTree(XMLString:String;UIParentNode:TDataNode; ExpandingComposite:Boolean=false):String;
var
  i:integer;
  TempChar,NextChar,NewString,RootNodeName,NewNameSpace:String;
  BracesToggleFlag:boolean;
  glb,ok:Boolean;
begin

  if checkData(XMLString)= true then
  begin

    StartingUp:=true;
   {$ifdef JScript}
   RootNodeName:=SystemRootName;
   asm
      var ob=document.getElementById(RootNodeName);
      $mod.SystemNodeTree.ScreenObject=ob;
    end;

    {$endif}

    if not ExpandingComposite then
      NewNameSpace:=''
    else
      NewNameSpace:=UIParentNode.NodeName;

    NewString:='';
    i:=0;
    while i<Length(XMLString) do
    begin
      i:=i+1;
      TempChar:=XMLString[i];
      if ( TempChar='<') then  // start recording this node string unless it is a closing "</ mytype>"
      begin
        Tempchar:='';// do not save the '<' char to the string being parsed
        BracesToggleFlag:=true;
        NextChar:= XMLString[i+1];
        if NextChar = '/' then BracesToggleFlag:=false;
      end;
      if  TempChar = '>' then  //stop recording this node string and process it before moving on to the next item
      begin
        if (BracesToggleFlag=true) then
        begin
          ok:=addComponentFromXML(newstring,UIParentNode,NewNameSpace,ExpandingComposite);// process the record string if this was a leading xml string such as "< type; name= ''; attr_1 = '???' etc>"
          //{$ifdef JScript} asm console.log('done addComponentFromXML for '+NewString);end; {$endif}
          if not ok then
            i:=length(XMLString);   //stop
        end;
        BracesToggleFlag:=false;
        newstring:='';
      end;
      if BracesToggleFlag=true
      then
      begin
        newstring:=newstring+TempChar;
      end;
    end;


//ShowMessage('all components added');
    StartingUp:=false;

  end
  else
  begin
    ShowMessage('Error .....Unable to load system data');
    showmessage(XMLString);
  end;

end;

function CompareAttribName(const Attr1, Attr2: TNodeAttribute): Integer;
begin
  Result := CompareText(Attr1.AttribName, Attr2.AttribName);
end;

procedure SwapAttrib(var Attr1, Attr2: TNodeAttribute);
var
  temp: TNodeAttribute;
begin
  temp := Attr1;
  Attr1 := Attr2;
  Attr2 := temp;
end;

procedure SortAttribs(var Attribs: TNodeAttributesArray);
var
  i, n: Integer;
  Swapped: Boolean;
begin
  n := Length(Attribs);
  repeat
    Swapped := False;
    for i := 1 to n-1 do begin
      if CompareAttribName(Attribs[i-1], Attribs[i])>0 then begin
        SwapAttrib(Attribs[i-1], Attribs[i]);
        Swapped := True;
      end;
    end;
    dec(n);
  until not Swapped;
end;

procedure AddAttribOptions(ComponentType,AttribName:string;Options:array of string);
var
  AttribOptionsRec:TAttribOptionsRec;
  i:integer;
  found:Boolean;
begin
  AttribOptionsRec.ComponentType:=ComponentType;
  AttribOptionsRec.AttribName:=AttribName;
  AttribOptionsRec.Options:=TStringList.Create;
  for i:=0 to length(options)-1 do
    AttribOptionsRec.Options.Add(Options[i]);
  // does this already exist?
  found:=false;
  i:=0;
  while i < length(AttribOptionsArray) do
  begin
    if (AttribOptionsArray[i].ComponentType=ComponentType)
    and (AttribOptionsArray[i].AttribName=AttribName) then
    begin
      AttribOptionsArray[i]:=AttribOptionsRec;
      found:=true;
      i:= length(AttribOptionsArray);
    end;
    i:=i+1;
  end;
  if not found then
  begin
    setlength(AttribOptionsArray,length(AttribOptionsArray)+1);
    AttribOptionsArray[length(AttribOptionsArray)-1]:=AttribOptionsRec;
  end;
end;

function LookupAttribOptions(ComponentType,AttribName:string):TstringList;
var
  i:integer;
begin
  i:=0;
  result:=nil;
  while i < length(AttribOptionsArray) do
  begin
    if (AttribOptionsArray[i].ComponentType=ComponentType)
    and (AttribOptionsArray[i].AttribName=AttribName)  then
    begin
      result:= AttribOptionsArray[i].Options;
      i:=length(AttribOptionsArray);
    end;
    i:=i+1;
  end;
end;

function DeleteAttribOptions(ComponentType,AttribName:string):TstringList;
var
  i,j:integer;
begin
  i:=0;
  result:=nil;
  while i < length(AttribOptionsArray) do
  begin
    if (AttribOptionsArray[i].ComponentType=ComponentType)
    and (AttribOptionsArray[i].AttribName=AttribName)  then
    begin
      for j:=i to length(AttribOptionsArray)-2 do
        AttribOptionsArray[j]:=AttribOptionsArray[j+1];
      i:=length(AttribOptionsArray);
      setlength(AttribOptionsArray,length(AttribOptionsArray)-1);
    end;
    i:=i+1;
  end;
end;


procedure InitSystemNodetree;
begin
  SystemNodetree:=TDataNode.Create('Root','ApplicationRoot','','Root',false);
  // create a parent node for all UI nodes
  UIRootNode:=TDataNode.Create('Root',SystemRootName,'','Root',false);
  AddChildToParentNode(SystemNodeTree,UIRootNode,-1);
  // create a parent node for all Code nodes
  CodeRootNode:=TDataNode.Create('Code',CodeRootName,'','Root',false);
  AddChildToParentNode(SystemNodeTree,CodeRootNode,-1);
  DMRoot:=TDataNode.Create('Root','DMRoot','','Root',false);
  AddChildToParentNode(SystemNodeTree,DMRoot,-1);
end;

{$ifndef JScript}
procedure ResequenceNVNodes;
var
  i,j,k:integer;
  NVNodes,UINodes:TChildNodesArray;
begin
  // ensure all the NV (non-visual) nodes are positioned in the node tree AFTER all the forms.
  setlength(UINodes,0);
  setlength(NVNodes,0);
  j:=0;
  k:=0;
  for i:=0 to length(UIRootNode.ChildNodes)-1 do
  begin
    if UIRootNode.ChildNodes[i].NodeClass='NV' then
    begin
      setlength(NVNodes,j+1);
      NVNodes[j]:=UIRootNode.ChildNodes[i];
      j:=j+1;
    end
    else
    begin
      setlength(UINodes,k+1);
      UINodes[k]:=UIRootNode.ChildNodes[i];
      k:=k+1;
    end;
  end;
  setlength(UIRootNode.ChildNodes,j+k);
  for i:=0 to k-1 do
    UIRootNode.ChildNodes[i]:=UINodes[i];
  for i:=0 to j-1 do
    UIRootNode.ChildNodes[k+i]:=NVNodes[i];
  setlength(UINodes,0);
  setlength(NVNodes,0);
end;

procedure InitialiseXComponentsProject;
begin
  // PasteDialog and CompilerLog forms are in XComponents units - need to instantiate the forms here.
  CompilerLogUnit.SetupCompilerLogForm;
  PasteDialogUnit.SetupPasteDialogForm;
end;
{$endif}

procedure TDataNode.DebugEvents(sometext:String);
var
  i:integer;
  txt:string;
begin
  {$ifdef JScript}
  txt:='';
  asm console.log(sometext); end;
  for i:=0 to self.myEventTypes.count-1 do
    txt:=txt + inttostr(i)+ ' >'+ self.myEventTypes[i] + '<  ';
  asm console.log(txt); end;
  txt:='';
  for i:=0 to length(self.myEventHandlers)-1 do
  begin
    txt:=txt+inttostr(i)+':';
    if self.myEventHandlers[i]<>nil then
      txt:=txt + '*  '
    else
      txt:=txt + 'nil  ';
  end;
  asm console.log(txt); end;
  {$endif}
end;
procedure TDataNode.LogAttributes(sometext:String);
var
  i:integer;
  txt:string;
begin
  {$ifdef JScript}
  txt:='';
  asm console.log(sometext); end;
  for i:=0 to length(self.NodeAttributes)-1 do
  begin
    txt:='  '+inttostr(i)+ ': >'+ self.NodeAttributes[i].AttribName + '<  '+self.NodeAttributes[i].AttribValue + ' '+mybooltostr(self.NodeAttributes[i].AttribReadOnly);
    asm console.log(txt); end;
  end;
  {$endif}
end;

//-------------------------------------------------------------------------------------------
begin
  SetLength(SourcedAttribs,0);
  InitSystemNodeTree;
  SuppressEvents:=false;
  SuppressUserEvents:=true;

  {$ifndef JScript}
  if not DirectoryExists('tempinc') then
    CreateDir('tempinc');
  {$endif}
end.

