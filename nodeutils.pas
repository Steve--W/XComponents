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
  Forms,Controls, StdCtrls, ExtCtrls, Dialogs, Clipbrd, ProjectIntf, LazIDEIntf,
 // UtilsJSCompile,
  {$ifdef Chromium}
  uCEFApplication,
  {$endif}
{$else}
{$endif}
  EventsInterface
  ;

type TDataNode = class;  //forward

type TCodeInputRec = record
    InputNodeName:String;
    InputAttribName:String;
    InputSynonym:String;
    InputValue:String;
  end;
type TCodeInputs = Array of TCodeInputRec;
type TEventHandler = procedure(e:TEventStatus;nodeID:AnsiString;myValue:AnsiString) of object;
type TEventHandlerRec = record
  TheHandler:TEventHandler;   // for event handler functions that exist in the compiled project
  TheCode:String;             // for dynamically created XComponents with user-entered event code
  InitCode:String;            // for dynamically created XComponents with user-entered event code
end;
type TEventHandlers = Array of TEventHandlerRec;
type TGenericHandler = function(MyEventType,myValue:string;myNode:TDataNode):Boolean of object;
   {$ifdef Win64}
type   WinSizeDependentInt = int64;
   {$else}
type  WinSizeDependentInt = integer;
   {$endif}

{$ifndef JScript}
type TAddComponentFunc = function(ParentNode:TDataNode;ObjectName:String;position:integer;Alignment:String): TDataNode;
{$else}
type TAddComponentFunc = function(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer;Alignment:String): TDataNode;

//type TXPropertyLink = class(TPersistent)
//  FTIObjectName:String;
//  FTIObject:TObject;
//  FTIPropertyName:String;
//  published
//    property TIObjectName:String read FTIObjectName write FTIObjectName;
//    property TIObject:TObject read FTIObject write FTIObject;
//    property TIPropertyName:String read FTIPropertyName write FTIPropertyName;
//end;

   // dummy 'TForm' object for Javascript widget creation
   type TForm = class(TObject)
         private
           fName:String;
         published
           property Name:String read fName write fName;
         end;
   type TColor = String;
{$endif}

 type TCreateInObFunc = function(MyForm:TForm;NodeName:String):TObject;

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

 type   TNodeAttribute = Record
         AttribName:string;
         AttribType:string;
         AttribValue:string;
         AttribReadOnly:boolean;
         AttribSource:TCodeInputRec;
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
       public
          NodeName:String;
          NodeType:String;
          NodeClass:String;
          IsDynamic:Boolean;                             // true if
          ScreenObject:TObject;                          // the actual visible component
          MyForm:TForm;                                  // the owning 'form' - object which contains event handlers

          NodeAttributes:TNodeAttributesArray;
          ChildNodes:TChildNodesArray;
          myEventTypes:TStringList;
          myEventHandlers:TEventHandlers;

          constructor Create(MyClass,MyName,MyType:string;NodeIsDynamic:Boolean=false);
          procedure DeleteMe;
          function HasAttribute(AttrName:string):Boolean;
          function GetAttribute(AttrName:string;AllowSpace:boolean):TNodeAttribute;
          function GetAttributeAnyCase(AttrName:string;AllowSpace:boolean):TNodeAttribute;   overload;
          function GetAttributeAnyCase(AttrName:string):TNodeAttribute;   overload;
          procedure AddAttribute(AttributeName,AttributeType,AttributeValue:string;AttributeReadOnly:boolean);
          procedure SetAttributeValue(AttrName:string;NewValue,AttrType:string;AttribReadOnly:Boolean);  overload;
          procedure SetAttributeValue(AttrName:string;NewValue,AttrType:string);  overload;
          procedure SetAttributeValue(AttrName:string;NewValue:string); overload;
          procedure SetAttributeSource(AttrName:string;SourceNodeName,SourceAttribName:string);
          function GetChildIndex(ChildNode:TDataNode):integer;
          procedure RemoveChildNode(ChildNode:TDataNode);
          function FindRegisteredEvent(EventType:string):TEventHandler;
          procedure RegisterEvent(EventType:String;TheHandler:TEventHandler);
          function HasUserEventCode(EventType:String):Boolean;
          function EventNum(EventType:String):integer;
          function GetEventCode(EventType:String):String;
          function GetEventInitCode(EventType:String):String;
       end;

const AlignmentOptions:Array[0..4] of string = ('Left','Right','Centre','Top','Bottom');
const LabelPosOptions:Array[0..3] of string = ('Top','Bottom','Left','Right');
const ScrollBarsOptions:Array[0..2] of string = ('Bottom','Right','Both');
type TAttribOptionsRec = record
      ComponentType:String;
      AttribName:String;
      Options:TStringList;
end;
type TAttribOptionsArray = array of TAttribOptionsRec;

{$ifdef JScript}
type TInterfaceObject = class(TDataNode)
private
//  FLink:TXpropertyLink;
published
  myNode:TDataNode;          // self-reference for compatibility with Laz component
//  procedure SetLink(const AValue: TXPropertyLink);
//  procedure LinkLoadFromProperty(Sender: TObject); virtual;
//  procedure LinkSaveToProperty(Sender: TObject); virtual;

//  property Link: TXPropertyLink read FLink write SetLink;
end;


{$endif}

function SubstituteSpecials(instring:string):string;
function UnSubstituteSpecials(instring:string):string;
function CreateFormNode(myForm:TForm):TDataNode;
function AddFormToNodeTree(myForm:TForm):TdataNode;
function InitialiseCodeTree:TdataNode;
procedure AddDefaultsToTable(MyNodeType:String;myDefaultAttribs:TDefaultAttributesArray);
function GetDefaultAttrib(NodeType,AttrName:String):TDefaultAttribute;
procedure AddDefaultAttribs(myComponent:TObject;NewNode:TDataNode;defaultAttribs:TDefaultAttributesArray);
procedure AddDefaultAttribute(var Attribs:TDefaultAttributesArray;AttrName,AttrType,AttrValue,AttrHint:String; ro:Boolean);
procedure AddDefaultAttribute(var Attribs:TDefaultAttributesArray;AttrName,AttrType,AttrValue,AttrHint:String; ro,incl:Boolean);
procedure AddAttrib(var AttrParams:TNodeAttributesArray;attrName,attrType,attrValue:string;attrReadOnly:Boolean);
procedure AddChildToParentNode(var ParentNode, ChildNode:TDataNode; position:integer);
function StringToCodeInputs(InputString:String):TCodeInputs;
function CodeInputsToString(myInputs:TCodeInputs):String;
function NodeTreeToXML(CurrentItem,ParentNode:TDataNode;DynamicOnly,QuotedString:Boolean):String;
Procedure SaveSystem(ToClip:Boolean);
function FindDataNodeById(InTree:TDataNode; ScreenObjectID:String;showerror:boolean):TDataNode;
function FindParentOfNode(InTree:TDataNode;targetNode:TdataNode;showerror:Boolean;var position:Integer):TDataNode;  overload;
function FindParentOfNode(InTree:TDataNode;targetNode:TdataNode):TDataNode;  overload;
function FindParentOfNodeByName(InTree:TDataNode;ScreenObjectID:String;showerror:Boolean;var position:Integer):TDataNode;  overload;
function FindParentOfNodeByName(InTree:TDataNode;ScreenObjectID:String;showError:Boolean):TDataNode; overload;
function FindParentOfNodeByName(InTree:TDataNode;ScreenObjectID:String):TDataNode; overload;
procedure ReParentNode(MyNode,NewParent:TDataNode);
procedure DeleteNode(ParentNode,MyNode:TDataNode);
function CopyNode(SourceNode:TDataNode):TDataNode;
function AddChildToDataNode(ParentNode:TDataNode;MyClass,MyName,MyType:string;MyAttributes:TNodeAttributesArray;
                           position:integer):TDataNode;
function LookupComponentFunc(NodeType:string):TAddComponentFunc;
function NodeIsDescendantOf(ThisNode:TDataNode;AncestorName:string):integer;
function NodeIsInXForm(ThisNode:TDataNode):Boolean;
//procedure Initialiselinks(StartNode:TDataNode);
procedure DeleteNodeChildren(ParentNode:TDataNode);
function InsertSystemNode(ParentNode,SourceNode:TDataNode;Position:integer):TDataNode;
procedure ClearAttribs(var AttrParams:TNodeAttributesArray);
function NodeNameIsUnique(myNode:TDataNode;NodeName:string; showerror:Boolean):Boolean;
function SetUniqueName(myNode:TDataNode;Formname:String;const NewName: TComponentName):TComponentName;
procedure InitSystemNodetree;
procedure ClearAllDynamicNodes(StartNode:TDataNode);
procedure NilScreenObject(MyNode:TdataNode);
function AlignmentResetInvalidCombinations(OldAlignment,myName,myClass:string;ParentAlignChildrenVertical,IsContainer,HasSibs:Boolean):string;
procedure SortAttribs(var Attribs: TNodeAttributesArray);
procedure AddAttribOptions(ComponentType,AttribName:string;Options:array of string);
function LookupAttribOptions(ComponentType,AttribName:string):TstringList;
function XMLToNodeTree(XMLString:String):String;
procedure SetXObjectProperty(myObj:TObject;targetNode:TdataNode;PropName,AttrType,newValue:String);
procedure EditAttributeValue(NodeNameToEdit:String; SourceAttrib:TNodeAttribute;AddIfMissing:Boolean); overload;
procedure EditAttributeValue(NodeNameToEdit,AttrNameToEdit,newValue:String;AddIfMissing:Boolean); overload;
procedure EditAttributeValue(NodeNameToEdit,AttrNameToEdit,newValue:String); overload;
{$ifndef JScript}
procedure EditAttributeValue(NodeNameToEdit,AttrNameToEdit,newValue:PChar); overload;
{$endif}
procedure EditAttributeValue2(NodeNameToEdit,AttrNameToEdit,newValue:String);
procedure EditAttributeValueIndexed(NodeNameToEdit,AttrNameToEdit:String;newValue:TStringArray; x,y:integer);
procedure myCopyToClip(stringname,instring:string);
function FindNodesOfType(StartNode:TdataNode;NodeType:String):TNodesArray;
procedure SetNodePropDefaults(myNode:TDataNode;defaultAttribs:TDefaultAttributesArray);
procedure RefreshComponentProps(myComponent:TDataNode);
procedure UnSuspendFrames(StartNode:TdataNode);

{$ifndef JScript}
procedure AddNodeFuncLookup(NodeType:string;ScreenObjFunc:TAddComponentFunc);
procedure SetAlignProperty(MyObj,MyParent:TControl);   overload;
procedure SetAlignProperty(ParentNode, MyNode:TDataNode); overload;
function FindOuterParentOf(InnerControl:TControl):TControl;
procedure CreateComponentDataNode2(myComponent:TObject;myType:String; defaultAttribs:TDefaultAttributesArray; eventTypes:TStringList; myOwner:TObject; IsDynamic:Boolean);
function CreateDynamicLazWidget(TypeName:String;ParentForm:TForm;ParentNode:TDataNode;NodeName,Alignment:string;position:integer):TDataNode;
procedure InitialiseXComponentsProject;

{$else}
procedure InitFormObject(myForm:TForm;NodeName:String);
procedure AddNodeFuncLookup(NodeType:string;InObFuncPtr:TCreateInObFunc;ScreenObjFunc:TAddComponentFunc);
//procedure PushTolinks(AObject:TObject; PropName:string; PropValue:String; StartNode:TDataNode);
procedure SetInterfaceProperty(myName,PropName,NewValue:string);
function mygetClipboardData(stringname:string):string;
function FinishHTMLPasteAction(myValue:string):string;
{$endif}

var MainForm:TForm;
SuppressEvents:Boolean;
AttribOptionsArray:TAttribOptionsArray;
ProjectDirectory:String;
DefaultAttribsByType:TDefaultAttribsTable;


{$ifndef JScript}
MainFormTopControl:TWinControl;
{$else}


type TMethod = record
  Code : Pointer;       //CodePointer;
  Data : Pointer;
end;

function CreateInterfaceObject(MyForm:TForm;NodeType, NodeName:String):TObject;

// this variable contains the system description to be loaded at startup
var LoadedSystemString:String;

{$endif}



const SystemRootName= 'UIRootNode';
const CodeRootName= 'CodeUnits';
const PortraitMode=false;

var SystemNodeTree,UIRootNode,CodeRootNode:TDataNode;
var StartingUp:Boolean;

var   NodeFuncsLookup:TNodeFuncsTable;             // lookup table used in data node creation

const EventAttributeDelimiter='|@|';
const EventListdelimiter='|^|';
const EventHistorydelimiter='|~|';
const delimiterBetweenAttribsAndEvents = '|@@|';
const attributeListdelimiter='|;'  ;
const NameValuePairdelimiter= '|=' ;
const AttribBitsDelimiter= '|{'   ;
const AttribLinkDelimiter= '|}'   ;

const StartXMLString =  '<';
const ToggleFlagMarker = '/';
const EndXMLString =  '>';

// available types for component attributes
const AttributeTypes:String = '["String","Integer","Boolean","Color","TableString","TreeString"]';


implementation
{$ifndef JScript}
uses LazsUtils,WrapperPanel, XForm, XBitMap, XGPUCanvas, XIFrame, Events, PasteDialogUnit, CompilerLogUnit;
{$else}
uses WrapperPanel, XForm, XButton, XBitMap, XGPUCanvas, PasteDialogUnit;
{$endif}



constructor TDataNode.Create(MyClass,MyName,MyType:string;NodeIsDynamic:Boolean=false);
var
  NodeString:string;
begin
  SetLength(self.ChildNodes,0) ;
  SetLength(self.NodeAttributes,0);
  self.myEventTypes:=TStringList.Create;
  SetLength(self.myEventHandlers,0);

  self.NodeClass:=MyClass;
  self.NodeName:=MyName;
  self.NodeType:=MyType;
  self.IsDynamic:=NodeIsDynamic;

end;


procedure TDataNode.DeleteMe;
var
  NodeString:string;
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
  foundAttrib.AttribName:='';
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
  if foundAttrib.AttribName<>AttrName then
    if AllowSpace then
    begin
      foundAttrib.AttribName:=AttrName;
      foundAttrib.AttribType:='String';
      foundAttrib.AttribValue:='';
      foundAttrib.AttribReadOnly:=false;
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
  foundAttrib.AttribName:='';
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
  if trim(uppercase(foundAttrib.AttribName))<>trim(uppercase(AttrName)) then
    if AllowSpace then
    begin
      foundAttrib.AttribName:=AttrName;
      foundAttrib.AttribType:='String';
      foundAttrib.AttribValue:='';
      foundAttrib.AttribReadOnly:=false;
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

procedure TDataNode.AddAttribute(AttributeName,AttributeType,AttributeValue:string;AttributeReadOnly:boolean);
var
  numAttributes:Integer;
  myAttributes:TNodeAttributesArray;
begin
  //showmessage('adding '+AttributeName+' '+AttributeValue);
    myAttributes:= self.NodeAttributes;
    numAttributes:=Length(myAttributes);
    SetLength( self.NodeAttributes,numAttributes+1);
    self.NodeAttributes[numAttributes].AttribName := AttributeName ;
    self.NodeAttributes[numAttributes].AttribValue := AttributeValue;
    self.NodeAttributes[numAttributes].AttribType := AttributeType ;
    self.NodeAttributes[numAttributes].AttribReadOnly := AttributeReadOnly;

    SortAttribs(self.NodeAttributes);
end;

procedure TDataNode.SetAttributeValue(AttrName:string;NewValue,AttrType:string;AttribReadOnly:Boolean); // overload;
var
  foundAttrib:TNodeAttribute;
  myAttribs:TNodeAttributesArray;
  i:integer;
  found:Boolean;
begin
  foundAttrib:=self.GetAttribute(AttrName,true);
  //this may have created a new attribute (default type String), so fix the type.
  if AttrType<>'' then
     foundAttrib.AttribType:=AttrType;

  foundAttrib.AttribValue:=NewValue;

  // now have to put it back in the array
  found:=false;
  myAttribs:=self.NodeAttributes;
  i:=0;
  while i < length(myAttribs) do
  begin
    if self.NodeAttributes[i].AttribName=AttrName then
    begin
      self.NodeAttributes[i]:=foundAttrib;
      i:= length(myAttribs);
      found:=true;
    end;
    i:=i+1;
  end;
  if not found then
  begin
    self.AddAttribute(foundAttrib.AttribName, foundAttrib.AttribType, FoundAttrib.AttribValue, AttribReadOnly);
  end;
end;

procedure TDataNode.SetAttributeValue(AttrName:string;NewValue,AttrType:string); // overload;
begin
  self.SetAttributeValue(AttrName,NewValue,AttrType,false);
end;

procedure TDataNode.SetAttributeValue(AttrName:string;NewValue:string); //overload;
begin
  self.SetAttributeValue(AttrName,NewValue,'');
end;

procedure TDataNode.SetAttributeSource(AttrName:string;SourceNodeName,SourceAttribName:string);
var
  foundAttrib:TNodeAttribute;
  myAttribs:TNodeAttributesArray;
  i:integer;
  found:Boolean;
begin
  foundAttrib:=self.GetAttribute(AttrName,true);
  //if no attribute here, add one
  if (foundAttrib.AttribName='') then
    if (SourceNodeName <> '') then
    begin
      showmessage('Unable to set source for missing attribute '+self.NodeName+'.'+AttrName);
      EXIT;
    end;

  foundAttrib.AttribSource.InputNodeName:=SourceNodeName;
  foundAttrib.AttribSource.InputAttribName:=SourceAttribName;

  if SourceNodeName='' then
  begin
    foundAttrib.AttribSource.InputSynonym:='';
    foundAttrib.AttribSource.InputValue:='';
  end;
  // now have to put it back in the array
  found:=false;
  myAttribs:=self.NodeAttributes;
  i:=0;
  while i < length(myAttribs) do
  begin
    if self.NodeAttributes[i].AttribName=AttrName then
    begin
      self.NodeAttributes[i]:=foundAttrib;
      i:= length(myAttribs);
      found:=true;
    end;
    i:=i+1;
  end;
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
     //showmessage('FindRegisteredEvent '+EventType);
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
       if trim(self.myEventHandlers[i].TheCode)<>'' then
         result:=true;
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
       if trim(self.myEventHandlers[i].TheCode)<>'' then
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
       if trim(self.myEventHandlers[i].InitCode)<>'' then
         result:=self.myEventHandlers[i].InitCode;
  end;
end;

procedure ClearAttribs(var AttrParams:TNodeAttributesArray);
begin
  setlength(AttrParams,0);
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
procedure RefreshComponentProps(myComponent:TDataNode);
var
  i:integer;
begin
  for i:=0 to length(myComponent.NodeAttributes)-1 do
  begin
    if myComponent.NodeAttributes[i].AttribReadOnly = false then
    begin
      SetXObjectProperty(myComponent,myComponent,myComponent.NodeAttributes[i].AttribName,myComponent.NodeAttributes[i].AttribType,myComponent.NodeAttributes[i].AttribValue);
    end;
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
      SetXObjectProperty(myComponent,NewNode,defaultAttribs[i].AttribName,defaultAttribs[i].AttribType,defaultAttribs[i].AttribValue);
    end;
  end;
end;

{$ifndef JScript}
function CreateDynamicLazWidget(TypeName:String;ParentForm:TForm;ParentNode:TDataNode;NodeName,Alignment:string;position:integer):TDataNode;
var
  NewNode,FormNode:TDataNode;
  NewWidget:TControl;
  supp:Boolean;
begin
  //Create Widget (also creates datanode)
  supp:=SuppressEvents;
  SuppressEvents:=true;
  NewWidget:=TControlClass(getclass(TypeName)).create(ParentForm);
  SetStrProp(NewWidget,'Name',NodeName);
  NewNode:=TDataNode(GetObjectProp(NewWidget,'myNode'));

  if (TypeName<>'TXMainMenu')
  and (NewNode.NodeClass<>'NV') then
  begin
    SetStrProp(NewWidget,'Alignment',Alignment);
    InsertUnderParent(TControl(NewWidget),TWinControl(ParentNode.ScreenObject), position);
    AddChildToParentNode(ParentNode,NewNode,position);
  end
  else
  begin
    FormNode:=TDataNode(GetObjectProp(ParentForm,'myNode'));



    AddChildToParentNode(FormNode,NewNode,position);
  end;
  SuppressEvents:=supp;
  result:=NewNode;
end;

procedure CreateComponentDataNode2(myComponent:TObject;myType:String; defaultAttribs:TDefaultAttributesArray; eventTypes:TStringList; myOwner:TObject; IsDynamic:Boolean);
var
  NewNode:TDataNode;
begin
  NewNode:=TDataNode.Create('UI','',myType,false);       // name is set later
  NewNode.ScreenObject:=myComponent;
  NewNode.myEventTypes:=eventTypes;
  SetLength(NewNode.myEventHandlers,EventTypes.Count);
  //showmessage('node '+myName+' event count set to '+ inttostr(EventTypes.Count));
  NewNode.MyForm:=TForm(myOwner);
  NewNode.IsDynamic:=IsDynamic;

  SetObjectProp(myComponent,'myNode',NewNode);

  AddDefaultAttribs(myComponent,NewNode,defaultAttribs);

  // temporarily set as child of root node, so that name uniqueness checks can be done during design
  AddChildToParentNode(SystemNodetree,NewNode,-1);

end;

{$else}

{$endif}

function ScanChildrenForNode(CurrentItem:TDataNode;targetNode:TdataNode;var FoundParent:TDataNode; var position:Integer):TDataNode;
var FoundItem,TempItem:TDataNode;
    TempArrayOfChildren:TChildNodesArray;
    NumChildren,i:integer;
begin
   FoundItem:=nil;
   FoundParent:=nil;
   if CurrentItem = targetNode
   then
   begin
     FoundItem:= CurrentItem
   end
   else
   begin
      TempArrayOfChildren:= CurrentItem.ChildNodes;
      NumChildren:=Length(TempArrayOfChildren);
      i:=0;
      while i < NumChildren do
      begin
         if FoundItem=nil then  // object has not been found so keep looking
         begin
           //showmessage('parent='+CurrentItem.NodeName+' i='+inttostr(i));
            TempItem := CurrentItem.ChildNodes[i];
            //showmessage('TempItem='+TempItem.NodeName);
            if TempItem = targetNode
            then
            begin
              FoundItem:= TempItem;
              FoundParent:=CurrentItem;
              position:=i;
              i:=NumChildren;
            end
            else
              FoundItem:= ScanChildrenForNode(TempItem,targetNode,FoundParent,position);
         end;
         i:=i+1;
      end;
   end;
   result:=FoundItem;
end;


function ScanChildrenForNodeByName(CurrentItem:TDataNode;ScreenObjectID:String;var FoundParent:TDataNode; var position:Integer):TDataNode;
var FoundItem,TempItem:TDataNode;
    TempArrayOfChildren:TChildNodesArray;
    NumChildren,i:integer;
begin
   FoundItem:=nil;
   FoundParent:=nil;
   if Trim(Uppercase(CurrentItem.NodeName)) = Trim(Uppercase(ScreenObjectID))
   then
   begin
     FoundItem:= CurrentItem
   end
   else
   begin
      TempArrayOfChildren:= CurrentItem.ChildNodes;
      NumChildren:=Length(TempArrayOfChildren);
      i:=0;
      while i < NumChildren do
      begin
         if FoundItem=nil then  // object has not been found so keep looking
         begin
            TempItem := CurrentItem.ChildNodes[i];
            if Trim(Uppercase(TempItem.NodeName)) = Trim(Uppercase(ScreenObjectID))
            then
            begin
              FoundItem:= TempItem;
             FoundParent:=CurrentItem;
              position:=i;
              i:=NumChildren;
            end
            else
              FoundItem:= ScanChildrenForNodeByName(TempItem,ScreenObjectID,FoundParent,position);
         end;
         i:=i+1;
      end;
   end;
   result:=FoundItem;
end;



function FindDataNodeById(InTree:TDataNode; ScreenObjectID:String;showerror:boolean):TDataNode;
var
  FoundItem, TempItem, FoundParent :TDataNode;
  pos:Integer;
begin
   pos:=-1;
   if trim(ScreenObjectID)='' then
     showmessage('oops no id');
   FoundItem:=nil;
   FoundParent:=nil;
   TempItem:=ScanChildrenForNodeByName(InTree,ScreenObjectID,FoundParent,pos);

   if TempItem<>nil
   then
   begin
       FoundItem:= TempItem ;
   end
   else
   begin
      if showerror then
        showmessage('Error in NodeUtils.FindDataNodeById >'+ScreenObjectID+'< not found');
   end;
   result:=FoundItem;
end;

function FindParentOfNode(InTree:TDataNode;targetNode:TdataNode;showerror:Boolean;var position:Integer):TDataNode;  overload;
var
  FoundItem, TempItem, FoundParent :TDataNode;
begin
  //showmessage('FindParentOfNode '+ScreenObjectID+' in tree '+InTree.NodeName);
   FoundItem:=nil;
   TempItem:=nil;
   FoundParent:=nil;
   position:=-1;
   TempItem:=ScanChildrenForNode(InTree,targetNode,FoundParent,position);

   if (TempItem<>nil) and (FoundParent<>nil) then
   begin
       FoundItem:= FoundParent ;
   end
   else
     if showError then
       showmessage('Error in Nodeutils.FindParentOfNode >'+targetNode.NodeType+'('+targetNode.NodeName+')'+' < not found');

   result:=FoundItem;
end;
function FindParentOfNode(InTree:TDataNode;targetNode:TdataNode):TDataNode;  overload;
var
  pos:integer;
begin
  result:=FindParentOfNode(InTree,targetNode,false,pos)
end;

function FindParentOfNodeByName(InTree:TDataNode;ScreenObjectID:String;showerror:Boolean;var position:Integer):TDataNode;
var
  FoundItem, TempItem, FoundParent :TDataNode;
begin
  //showmessage('FindParentOfNode '+ScreenObjectID+' in tree '+InTree.NodeName);
   FoundItem:=nil;
   TempItem:=nil;
   FoundParent:=nil;
   position:=-1;
   TempItem:=ScanChildrenForNodeByName(InTree,ScreenObjectID,FoundParent,position);

   if (TempItem<>nil) and (FoundParent<>nil) then
   begin
       FoundItem:= FoundParent ;
   end
   else
     if showError then
       showmessage('Error in Nodeutils.FindParentOfNode >'+ScreenObjectID+'< not found');
   result:=FoundItem;
end;
function FindParentOfNodeByName(InTree:TDataNode;ScreenObjectID:String;showerror:Boolean):TDataNode;
var
  pos:Integer;
begin
  result:=FindParentOfNodeByName(InTree,ScreenObjectID,false,pos);
end;
function FindParentOfNodeByName(InTree:TDataNode;ScreenObjectID:String):TDataNode;
var
  pos:Integer;
begin
   result:=FindParentOfNodeByName(InTree,ScreenObjectID,false,pos);
end;

function MakeAttrib(attrName,attrType,attrValue:string;attrReadOnly:Boolean):TNodeAttribute;
var
  newAttrib:TNodeAttribute;
begin
  newAttrib.AttribName:=attrName;
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
  tempstr:=myStringReplace(tempstr,'<','&lt;',-1,-1);
  tempstr:=myStringReplace(tempstr,'>','&gt;',-1,-1);
  tempstr:=myStringReplace(tempstr,'''','&apos;',-1,-1);
  tempstr:=myStringReplace(tempstr,'"','&quot;',-1,-1);
  tempstr:=myStringReplace(tempstr,'\n','&bksln;',-1,-1);
  tempstr:=myStringReplace(tempstr,'\','&bksl;',-1,-1);
  tempstr:=myStringReplace(tempstr,LineEnding,'&crlf;',-1,-1);
  tempstr:=myStringReplace(tempstr,chr(10),'&crlf;',-1,-1);
  tempstr:=myStringReplace(tempstr,EventAttributeDelimiter,'&eadlm;',-1,-1);
  tempstr:=myStringReplace(tempstr,EventListdelimiter,'&eldlm;',-1,-1);
  tempstr:=myStringReplace(tempstr,delimiterBetweenAttribsAndEvents,'&aedlm;',-1,-1);
  tempstr:=myStringReplace(tempstr,attributeListdelimiter,'&aldlm;',-1,-1);
  tempstr:=myStringReplace(tempstr,EventListdelimiter,'&eldlm;',-1,-1);
  tempstr:=myStringReplace(tempstr,AttribBitsDelimiter,'&abdlm;',-1,-1);
  tempstr:=myStringReplace(tempstr,AttribLinkDelimiter,'&akdlm;',-1,-1);
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
  tempstr:=myStringReplace(tempstr,'&crlf;',LineEnding,-1,-1);
  tempstr:=myStringReplace(tempstr,'&eadlm;',EventAttributeDelimiter,-1,-1);
  tempstr:=myStringReplace(tempstr,'&eldlm;',EventListdelimiter,-1,-1);
  tempstr:=myStringReplace(tempstr,'&aedlm;',delimiterBetweenAttribsAndEvents,-1,-1);
  tempstr:=myStringReplace(tempstr,'&aldlm;',attributeListdelimiter,-1,-1);
  tempstr:=myStringReplace(tempstr,'&eldlm;',EventListdelimiter,-1,-1);
  tempstr:=myStringReplace(tempstr,'&abdlm;',AttribBitsDelimiter,-1,-1);
  tempstr:=myStringReplace(tempstr,'&akdlm;',AttribLinkDelimiter,-1,-1);
  result:=tempstr;
end;

function StringToCodeInputs(InputString:String):TCodeInputs;
var
  myInputs:TCodeInputs;
  Inputs,InputBits:TStringList;
  InputStr:String;
  j,k:integer;
begin
   setLength(myInputs,0);
   if InputString<>'' then
   begin
     Inputs:=stringsplit(Inputstring,AttribLinkDelimiter);
     // each input has NodeName (which is a declared unit or function)
     // and AttribName (if NodeName is a screen component).
     setlength(myInputs,0);
     k:=0;
     for j:=0 to Inputs.Count-1 do
     begin
        // format of string is SynonymName.NodeName.AttribName
        InputStr:=Inputs[j];
        InputBits:=stringsplit(InputStr,'.');
        if trim(InputBits[0])<>'' then  //skip any undefined inputs (must have synonym)
        begin
          setlength(myInputs,k+1);
          myInputs[k].InputSynonym:=InputBits[0];
          myInputs[k].InputNodeName:=InputBits[1];
          if InputBits.Count>2 then
            myInputs[k].InputAttribName:=InputBits[2];
          k:=k+1;
        end;
     end;
   end;
   result:=myInputs;
end;

function CodeInputsToString(myInputs:TCodeInputs):String;
var
  j:integer;
  InputsString:String;
begin
  InputsString:='';
  for j:=0 to length(myInputs)-1 do
  begin
    InputsString:=InputsString
                  + myInputs[j].InputSynonym
                  + '.' + myInputs[j].InputNodeName
                  + '.' + myInputs[j].InputAttribName
                  + AttribLinkDelimiter;
  end;
  result:=InputsString;
end;

function NodeTreeToXML(CurrentItem,ParentNode:TDataNode;DynamicOnly,QuotedString:Boolean):String;
// Recursive
var
  XMLString,ParentName:String;
  i,j,numchildren,numAttributes,numEvents:integer;
  CurrentChildNodes:TChildNodesArray;
  myAttribs:TNodeAttributesArray;
  InputsString,AQuote1,AQuote2:string;
  DfltAttrib:TDefaultAttribute;
begin
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
  or (CurrentItem.NodeClass='Code'))
  and (CurrentItem.NodeName<>'XGPUCodeEditorForm')
  and (CurrentItem.NodeName<>'PasteDialog')
  and (CurrentItem.NodeName<>'CompilerLogForm')then
  begin
    if ((CurrentItem.IsDynamic = true) or (DynamicOnly=false) ) then
    begin
      XMLString:=AQuote2 + LineEnding + AQuote1 + StartXMLString+CurrentItem.NodeType+attributeListdelimiter;
      XMLString:=XMLString+' Class '+NameValuePairdelimiter + CurrentItem.NodeClass + attributeListdelimiter;
      XMLString:=XMLString+' Name '+NameValuePairdelimiter + CurrentItem.NodeName + attributeListdelimiter;

      myAttribs:= CurrentItem.NodeAttributes;
      numAttributes:=length(myAttribs);
      for i:=0 to numAttributes-1 do
      begin
        DfltAttrib:=GetDefaultAttrib(CurrentItem.NodeType,CurrentItem.NodeAttributes[i].AttribName);
        if DfltAttrib.AttribName='' then
          DfltAttrib.AttribIncludeInSave:=true;
        if (FindSuppressedProperty(CurrentItem.NodeType,CurrentItem.NodeAttributes[i].AttribName)<0)
        and (DfltAttrib.AttribIncludeInSave = true)
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
                                                + '.'+CurrentItem.NodeAttributes[i].AttribSource.InputAttribName
                           + attributeListdelimiter;
        end;
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
             setlength(CurrentItem.myEventHandlers,CurrentItem.myEventTypes.count);
        for i:=0 to CurrentItem.myEventTypes.count-1 do
        begin
          // build the full event string
          XMLString:=XMLString
                           + CurrentItem.myEventTypes[i]
                           + AttribBitsDelimiter+SubstituteSpecials(trim(CurrentItem.myEventHandlers[i].TheCode))
                           + AttribBitsDelimiter+SubstituteSpecials(trim(CurrentItem.myEventHandlers[i].InitCode))
                        //   + InputsString
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
                    +'(CreateInterfaceObject('+CurrentItem.MyForm.Name+','''+CurrentItem.NodeType+''','''+CurrentItem.NodeName+'''));'
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
{$endif}

Procedure SaveSystem(ToClip:Boolean);
var
  systemstring,eventstring,fullstring:string;
  interfaceString:string;
begin
  {$ifndef JScript}
  interfaceString:=NodeTreeToInterfaceString(SystemNodeTree,MainForm.Name);
  WriteToFile(ProjectDirectory+'tempinc/systemintface.inc',interfaceString);
  {$endif}
  systemstring:= NodeTreeToXML(SystemNodeTree,nil,false,(not ToClip));
  fullstring:= systemstring;

  if ToClip then
  begin
    {$ifndef JScript}
    myCopyToClip('System',fullstring );
    {$endif}
  end
  else
  begin
    {$ifndef JScript}
    WriteToFile(ProjectDirectory+'tempinc/systemnodetree.inc','LoadedSystemString:=''*'';LoadedSystemString := '''+fullstring+''';');
    {$endif}
  end;
end;

{$ifndef JScript}
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
        //ParentNode:=FindParentOfNodeByName(SystemNodeTree,CurrentNode.NodeName);
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
        //ParentNode:=FindParentOfNodeByName(SystemNodeTree,CurrentNode.NodeName);
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
    //pn:= FindParentOfNode(SystemNodeTree,ChildNode.NodeName,false);
    pn:= FindParentOfNode(SystemNodeTree,ChildNode);
    if pn<>nil then
    begin
      pn.RemoveChildNode(ChildNode);
    end;
  end;

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

function NodeNameIsUnique(myNode:TDataNode;NodeName:string; showerror:Boolean):Boolean;
var
  myresult:Boolean;
  founditem:TDataNode;
begin
  myresult:=true;
  founditem:=FindDataNodeById(SystemNodeTree,NodeName,false);
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
    while NodeNameIsUnique(myNode,ApplyName,false) = false do
    begin
      ApplyName:=FormName + NewName + inttostr(i);    //!!!! when nodes are deleted in IDE, have to destroy the data node and screenobject!!!!
      i:=i+1;
    end;
  result:=ApplyName;
end;

function AddChildToDataNode(ParentNode:TDataNode;MyClass,MyName,MyType:string;MyAttributes:TNodeAttributesArray;
                           position:integer):TDataNode;
var numchildren:integer;
    tempDataNodeArray:TChildNodesArray;
    newNode:TDataNode;
    i:integer;
begin

  if NodeNameIsUnique(nil,MyName,true) then
  begin
    tempDataNodeArray:= ParentNode.ChildNodes;
    numchildren:= Length(tempDataNodeArray);
    SetLength(ParentNode.ChildNodes,numchildren+1) ;

    newNode:=TDataNode.Create(MyClass,MyName,MyType);
    newNode.NodeAttributes:=MyAttributes;

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

procedure CopyEventHandlers(myNode:TDataNode;myEventHandlers:TEventHandlers);
var
  i:integer;
begin
  setlength(myNode.myEventHandlers,mynode.myEventTypes.Count);
  for i:=0 to length(myEventHandlers)-1 do
  begin
    if trim(myEventHandlers[i].TheCode)<>'' then
    begin
      mynode.myEventHandlers[i].TheCode:=myEventHandlers[i].TheCode;
      mynode.myEventHandlers[i].InitCode:=myEventHandlers[i].InitCode;
    end;
  end;

end;

function CopyNode(SourceNode:TDataNode):TDataNode;
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

  NewNode:=TDataNode.Create(SourceNode.NodeClass, SourceNode.NodeName,SourceNode.NodeType);
  NewNode.IsDynamic:=true;
  NewNode.NodeAttributes:=myAttribs;

  NewNode.myEventTypes:=TStringList.Create;
  SetLength(NewNode.myEventHandlers,SourceNode.myEventTypes.Count);
  //showmessage('node '+sourcenode.NodeName+' event count set to '+ inttostr(NewNode.myEventTypes.Count));
  for i:=0 to SourceNode.myEventTypes.count-1 do
  begin
     NewNode.myEventTypes.Add(SourceNode.myEventTypes[i]);
     NewNode.myEventHandlers[i].TheHandler:=nil;
  end;
  CopyEventHandlers(NewNode,SourceNode.myEventHandlers);

  setlength(NewNode.ChildNodes,length(SourceNode.ChildNodes));
  for i:=0 to length(SourceNode.ChildNodes)-1 do
    NewNode.ChildNodes[i]:=CopyNode(SourceNode.ChildNodes[i]);

  result:=NewNode;
end;

function InsertSystemNode(ParentNode,SourceNode:TDataNode;Position:integer):TDataNode;
// Dynamic runtime creation of a new component
var
    myparent,myself:TDataNode;
    i:integer;
begin
  //showmessage('InsertSystemNode '+SourceNode.NodeType+' parent='+ParentNode.Nodename);

  myparent:=ParentNode;
  if (myParent<>nil)
  and (myparent.NodeName<>'')
  then
  begin
    if (SourceNode.NodeClass = 'UI')
    or (SourceNode.NodeClass = 'NV')
    or (SourceNode.NodeClass = 'SVG') then
    begin
      // create the screen object and data node...
      begin
        myself:=AddDynamicWidget(SourceNode.NodeType,ParentNode.MyForm,ParentNode,SourceNode.NodeName,'Left',position);
        CopyEventHandlers(myself,SourceNode.myEventHandlers);
        for i:=0 to length(SourceNode.NodeAttributes)-1 do
          if SourceNode.NodeAttributes[i].AttribReadOnly=false then
          begin
            EditAttributeValue(SourceNode.NodeName,SourceNode.NodeAttributes[i],true);
          end;


        // now insert any child nodes
        for i:=0 to length(SourceNode.ChildNodes)-1 do
          InsertSystemNode(myself,SourceNode.ChildNodes[i],-1);
      end;
    end
    else if SourceNode.NodeClass = 'Code' then
    begin
      myself:=SourceNode;
      myself.IsDynamic:=true;

      AddChildToParentNode(myparent,myself,position);
    end;
  end;

  result:=myself;
end;

//function AttribsFromXML(attributeList:TStringList;offset:integer;var ParentName:String;var NewLink:TXPropertyLink):TNodeAttributesArray;
function AttribsFromXML(attributeList:TStringList;offset:integer;var ParentName:String):TNodeAttributesArray;
var
  myAttribs:TNodeAttributesArray;
  LinkSet,AttribBits, sourceBits:TStringList;
  i:integer;
  tmp:String;
begin
  //showmessage('AttribsFromXML. count='+inttostr(attributeList.count)+' offset='+inttostr(offset));
  setlength(myAttribs,attributeList.count-offset);
  for i:=offset to attributeList.count-1 do
  begin
    AttribBits :=  stringsplit(attributeList[i],AttribBitsDelimiter);
    myAttribs[i-offset].AttribName:=TrimWhiteSpace(AttribBits[0]);
    myAttribs[i-offset].AttribType:=TrimWhiteSpace(AttribBits[1]);
    myAttribs[i-offset].AttribValue:=  UnSubstituteSpecials(AttribBits[2]);
    myAttribs[i-offset].AttribReadOnly:=myStrToBool(TrimWhiteSpace(AttribBits[3]));
    if AttribBits.Count>4 then
    begin
      tmp:=TrimWhiteSpace(AttribBits[4]);
      sourceBits:= stringsplit(tmp,'.');
      if sourceBits.Count>0 then
        if sourceBits[0]<>'' then
        begin
          myAttribs[i-offset].AttribSource.InputNodeName:=sourceBits[0];
          if sourceBits.Count>1 then
            myAttribs[i-offset].AttribSource.InputAttribName:=sourceBits[1];
        end;
    end;

    if myAttribs[i-offset].AttribName='ParentName' then
      ParentName:=AttribBits[2];
//    if myAttribs[i-offset].AttribName='Link' then
//    begin
//      LinkSet:= stringsplit(myAttribs[i-offset].AttribValue,AttribLinkDelimiter);
//      if LinkSet.count>1 then
//      begin
//       // showmessage('creating link for '+LinkSet[0]+' '+LinkSet[1]);
//        NewLink:=TXPropertyLink.Create;
//        NewLink.TIObjectName := LinkSet[0];
//        NewLink.TIPropertyName := LinkSet[1];
//      end;
//    end;
  end;

  result:=myAttribs;
end;

function EventsFromXML(eventsList:TStringList; var EventNames:TStringList):TEventHandlers;
var
  myEvents:TEventHandlers;
  AttribBits:TStringList;
  i:integer;
  t1,t2,tmp:string;
begin
  //showmessage('EventsFromXML. count='+inttostr(eventsList.count));
  setlength(myEvents,eventsList.count);
  for i:=0 to eventsList.count-1 do
  begin
    t1:=eventsList[i];
    AttribBits :=  stringsplit(eventsList[i],AttribBitsDelimiter);
    t2:=AttribBits[0];
    tmp:=AttribBits[1];
    EventNames.Add(TrimWhiteSpace(AttribBits[0]));
    myEvents[i].TheCode:=UnSubstituteSpecials(AttribBits[1]);
    if AttribBits.Count>2 then
    begin
      tmp:=AttribBits[2];
      myEvents[i].InitCode:=UnSubstituteSpecials(AttribBits[2]);
    end;
  end;

  result:=myEvents;
end;

function BuildSourceNodeFromXML(XMLString:String; var ParentName:String):TDataNode;
var
     ScreenObjectName,NodeClass,ScreenObjectType:string;
     AttribsEvents:TStringList;
     attributeList:TStringList;
     EventsList, EventNames:TStringList;
     NameValuePair:TStringList;
     i,pos:integer;
     myAttribs:TNodeAttributesArray;
     myEventHandlers:TEventHandlers;
     ParentNode:TDataNode;
     mynode,SourceNode:TDataNode;
     tmp,NodeString:String;
     //NewLink:TXPropertyLink;
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

   NameValuePair :=  stringsplit(attributeList[1],NameValuePairdelimiter);
   NodeClass := TrimWhiteSpace(NameValuePair[1]);

   NameValuePair :=  stringsplit(attributeList[2],NameValuePairdelimiter);
   ScreenObjectName := TrimWhiteSpace(NameValuePair[1]);

   //showmessage(ScreenObjectType+' '+NodeClass+' '+ScreenObjectName);

   if (NodeClass <> 'Root') then                     // these already exist
   begin
     {$ifndef JScript}
     if (FindDataNodeById(SystemNodeTree,ScreenObjectName,false)=nil) then
     {$endif}
     begin
       //myAttribs := AttribsFromXML(attributeList,3,ParentName,NewLink);
       myAttribs := AttribsFromXML(attributeList,3,ParentName);
       EventNames:=TStringList.Create;
       myEventHandlers := EventsFromXML(EventsList,EventNames);

       if ScreenObjectType='TXButton' then
       for i:=0 to length(MyEventHandlers)-1 do
       begin
         tmp:=MyEventHandlers[i].TheCode;
         tmp:=MyEventHandlers[i].InitCode;
         tmp:='';
       end;

       myNode:=TDataNode.Create(NodeClass,ScreenObjectName,ScreenObjectType,true);
       myNode.NodeAttributes:=myAttribs;
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
   else
   begin
     // update attributes for existing root nodes (eg. to capture DeploymentMode)
     myNode:=FindDataNodeById(SystemNodeTree,ScreenObjectName,false);
     if myNode<>nil then
     begin
       //myAttribs := AttribsFromXML(attributeList,3,ParentName,NewLink);
       myAttribs := AttribsFromXML(attributeList,3,ParentName);
       for i:=0 to length(myAttribs)-1 do
       begin
         myNode.SetAttributeValue(myAttribs[i].AttribName,myAttribs[i].AttribValue);
       end;
       EventNames:=TStringList.Create;
       myEventHandlers := EventsFromXML(EventsList,EventNames);
       myNode.myEventTypes:=EventNames;
       myNode.myEventHandlers:=myEventHandlers;
     end;
   end;
end;

{$ifndef JScript}
procedure myCopyToClip(stringname,instring:string);
begin
  Clipboard.AsText :=instring;
  Showmessage('The '+stringname+' has been saved to the Clipboard');
end;

function addComponentFromXML(XMLString:String):string;
 var
     ParentName:string;
     i,pos:integer;
     ParentNode:TDataNode;
     mynode,SourceNode:TDataNode;
//     NewLink:TXPropertyLink;
  begin
    //ShowMessage('addComponentFromXML  : '+XMLString);

   SourceNode:=BuildSourceNodeFromXML(XMLString, ParentName);

   if SourceNode<>nil then
   begin
     ParentNode:=nil;
     if ParentName<>'' then
       ParentNode:=FindDataNodeByID(SystemNodeTree,ParentName,false);
     if ParentNode=nil then
       ParentNode:=UIRootNode;  // (eg. for TXForms)


     // If the object was defined in a Lazarus form at design time,
     // the necessary data node (interface object) should already have been created.
     // Find it, add it to the identified parent, and set the relevant attributes.
     myNode:=FindDataNodeById(SystemNodeTree,SourceNode.NodeName,false);

     // If the object was created dynamically at run time, however, then we now need to
     // create a data node for it.
     if myNode=nil then
     begin
       //showmessage('creating dynamic component '+ScreenObjectType+'; '+ NodeClass+'; '+ScreenObjectName);
       myNode:=SourceNode;
       // Create a screen object by running the registered instantiation function for the node type
       InsertSystemNode(ParentNode,myNode,-1);
     end
     else
     begin
       ReparentNode(myNode,ParentNode);
       if (ParentNode.ScreenObject<>nil) then
         InsertUnderParent(TControl(MyNode.ScreenObject),TWinControl(ParentNode.ScreenObject),-1);
       for i:=0 to length(SourceNode.NodeAttributes)-1 do
       begin
         mynode.SetAttributeValue(SourceNode.NodeAttributes[i].AttribName,SourceNode.NodeAttributes[i].AttribValue);
       end;
       myNode.myEventTypes:=SourceNode.myEventTypes;
       CopyEventHandlers(myNode,SourceNode.myEventHandlers);
     end;


   end;

 end;

{$else}

function mygetClipboardData(stringname:string):string;
begin
  // set 'normal' appearance for paste dialog form
  PasteDialogUnit.PasteDoneBtn.IsVisible:=false;
  PasteDialogUnit.PasteLabel.LabelCaption:='Waiting for a copy/paste action';
  PasteDialogUnit.PasteTarget.IsVisible:=true;

  // showmessage('mygetClipboardData');
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
      //alert('closeClipboardPasteDialog '+myValue);
      try {
             setTimeout(function() {
                try {
                 var pasteTarget = document.getElementById('PasteTargetContents');
                  var PasteString = myValue;
                  // myValue should hold the pasted data direct from the clipboard.
                  // However if it's blank, revert to the text content of pasteTarget...
                  if (PasteString=='') {PasteString = pasteTarget.value};

                  pas.XForm.CloseModal('PasteDialog');

                 if (pas.PasteDialogUnit.CompletionEvent!=null) {
                    //alert('call completion event '+pas.PasteDialogUnit.CompletionEvent.EventType+' '+pas.PasteDialogUnit.CompletionEvent.NodeId);
                    pas.PasteDialogUnit.CompletionEvent.ReturnString=PasteString;
                    pas.Events.handleEvent(pas.PasteDialogUnit.CompletionEvent,
                                           pas.PasteDialogUnit.CompletionEvent.EventType,
                                           pas.PasteDialogUnit.CompletionEvent.NodeId,
                                           PasteString,'');
                    pas.PasteDialogUnit.CompletionEvent=null;
                  }
                } catch(err) { alert(err.message+'  in NodeUtils.setTimeout'); }
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

procedure SetInterfaceProperty(myName,PropName,NewValue:string);
// set the property value on the interface object
var
  myObj:TObject;
  MyPropType:TTypeKind;
  valueStr:String;
  i:NativeInt;
begin
  valueStr:=NewValue;

  //showmessage('setintfprop. name='+myname+' prop='+PropName+' value=>'+NewValue+'<');
  myObj:=TObject(FindDataNodeById(SystemNodeTree,myName,false));
  if myObj<>nil then
  begin
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

function CreateInterfaceObject(MyForm:TForm;NodeType, NodeName:String):TObject;
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
    myObj:=inobFn(MyForm,NodeName);
  end;
  result:=myObj;
end;

function addComponentFromXML(XMLString:String):string;
 var
     ParentName,mf,ScreenObjectName,ScreenObjectType:string;
     i,l:integer;
     ParentNode, SourceNode:TDataNode;
     mynode:TDataNode;
     fn:TAddComponentFunc;
//     NewLink:TXPropertyLink;
     myDynamicWrapper:TWrapperPanel;

  begin
    //ShowMessage('addComponentFromXML  : '+XMLString);
    SourceNode:=BuildSourceNodeFromXML(XMLString, ParentName);


    if SourceNode<>nil then
    begin
       ScreenObjectName:=SourceNode.NodeName;
       ScreenObjectType:=SourceNode.NodeType;
       if ParentName='' then
       begin
         showmessage('parentname is blank for '+ScreenObjectName);
         ParentNode:=SystemNodeTree;
       end
       else
         ParentNode:=FindDataNodeByID(SystemNodeTree,ParentName,true);
       //if parentnode<>nil then showmessage('found parent '+ParentName +' as '+ParentNode.NodeName);

       // If the object was defined in a Lazarus form at design time,
       // the necessary data node (interface object) should already have been created.
       // Find it, add it to the identified parent, and set the relevant attributes.
       myNode:=FindDataNodeById(SystemNodeTree,ScreenObjectName,false);

       // If the object was created dynamically at run time, however, then we still need to
       // create an interface object / data node for it.
       if myNode=nil then
       begin
         if ((SourceNode.NodeClass='UI') and (SourceNode.NodeType<>''))
         or ((SourceNode.NodeClass='NV') and (SourceNode.NodeType<>''))
         or (SourceNode.NodeClass = 'SVG') then
         begin
           //showmessage('building dynamic node '+ScreenObjectType+' '+ScreenObjectName);
           myDynamicWrapper:=TWrapperPanel(CreateInterfaceObject(ParentNode.MyForm,ScreenObjectType,ScreenObjectName));
           if myDynamicWrapper<>nil then
           begin
             myNode:=TDataNode(myDynamicWrapper);
             myDynamicWrapper.myNode:=myNode;

             if myNode=nil then showmessage('myNode is nil');
             myNode.IsDynamic:=true;
             myNode.myEventTypes:=SourceNode.myEventTypes;
             myNode.myEventHandlers:=SourceNode.myEventHandlers;
           end;
//           if myDynamicWrapper<>nil then showmessage('AddComponentFromXML created dynamic node '+myNode.NodeName);
           if mynode=nil then showmessage('mynode is nil in addComponentFromXML');
         end
         else if SourceNode.NodeClass='Code' then
         begin
           myNode:=SourceNode;
         end;
       end;

       ReparentNode(myNode,ParentNode);

      if ((SourceNode.NodeClass='UI') and (SourceNode.NodeType<>''))
      or ((SourceNode.NodeClass='NV') and (SourceNode.NodeType<>''))
      or (SourceNode.NodeClass = 'SVG') then
      begin
       //showmessage('setting attribute values');
       for i:=0 to length(SourceNode.NodeAttributes)-1 do
       begin
         //showmessage(SourceNode.NodeAttributes[i].AttribName);
         mynode.SetAttributeValue(SourceNode.NodeAttributes[i].AttribName,SourceNode.NodeAttributes[i].AttribValue);
       end;
//       if myNode is TInterfaceObject then
//         TInterfaceObject(myNode).Link := NewLink;
       if MainForm<>nil then
         mf:=MainForm.Name
       else
         mf:='.';
       asm
         if (ScreenObjectName!=mf) {
           // object may already exist if this is a system re-load, so delete the old one.
           var ob = document.getElementById(ScreenObjectName);
           if (ob!=null) {
              var Parent = pas.HTMLUtils.ScreenObjectInnerComponent(ParentNode);
             if (Parent!=null) {
                Parent.removeChild(ob); }
           }
         }
       end;
       // Create a screen object by running the registered instantiation function for the node type
       //showmessage('calling widget func');
       fn:=LookupComponentFunc(SourceNode.NodeType);
       fn(myNode,ParentNode,SourceNode.NodeName,-1,'Left');        //!! set initial alignment from source?
     end ;

     if myNode.HasAttribute('SuspendRefresh') then
       myNode.SetAttributeValue('SuspendRefresh','False');

   end;
 end;
{$endif}

function CreateFormNode(myForm:TForm):TDataNode;
var
  myNode:TDataNode;
begin
  {$ifndef JScript}
  myNode:=nil;
  myNode:=TDataNode.Create('UI',myForm.Name,'TXForm',false);
  myNode.ScreenObject:=myForm;
  myNode.MyForm:=myForm;
  if myForm<>MainForm then
  begin
    myNode.SetAttributeValue('Top',inttostr(myForm.Top));
    myNode.SetAttributeValue('Left',inttostr(myForm.Left));
    myNode.SetAttributeValue('Height',inttostr(myForm.Height));
    myNode.SetAttributeValue('Width',inttostr(myForm.Width));
    myNode.SetAttributeValue('Caption',myForm.Caption);
  end;
  {$else}
  SetLength(TXForm(myForm).ChildNodes,0) ;
  SetLength(TXForm(myForm).NodeAttributes,0);
  TXForm(myForm).myEventTypes:=TStringList.Create;
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
  myNode:=TDataNode.Create('Code','CodeUnits','Root',false);

  AddChildToParentNode(SystemNodeTree,myNode,-1);

  result:=myNode;
end;


procedure ReParentNode(MyNode,NewParent:TDataNode);
var
  OldParent:TDataNode;
  pos:integer;
begin
  // called during load from XML
  {$ifndef JScript}
  OldParent:=FindParentOfNode(SystemNodeTree,MyNode);
  {$else}
  OldParent:=FindParentOfNodeByName(SystemNodeTree,MyNode.NodeName,false,pos);
  {$endif}
  if OldParent<>nil then
  begin
    OldParent.RemoveChildNode(MyNode);
  end;
  AddChildToParentNode(NewParent,MyNode,-1);
end;

function FindNodesOfType(StartNode:TDataNode;NodeType:String):TNodesArray;
var
  newNodes,descendants:TNodesArray;
  i,j,l:integer;
begin
  setlength(newNodes,0);
  for i:=0 to length(StartNode.ChildNodes)-1 do
  begin
    if StartNode.ChildNodes[i].NodeType=NodeType then
    begin
      setlength(newNodes,length(NewNodes)+1);
      newNodes[length(NewNodes)-1]:=StartNode.ChildNodes[i];
    end;
    descendants:=FindNodesOfType(StartNode.ChildNodes[i],NodeType);
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
       StartNode.myEventHandlers[i].InitCode:='';
       StartNode.myEventHandlers[i].TheCode:='';
       StartNode.myEventHandlers[i].TheHandler:=nil;
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
    else if pType in [tkDynArray] then
    begin
      arr:=CommaListToStringArray(newValue);
      parr:=Pointer(arr);
      SetDynArrayProp(myObj,PropName,parr);
    end
    {$else}
    if pType = tkString then
    begin
      //showmessage('setting string property');
      SetStringProp(myObj,PropName,NewValue);
    end
    else if pType = tkInteger then
    begin
      //showmessage('setting integer property');
      SetNativeIntProp(myObj,PropName,strtoint(newValue));
    end
    else if pType = tkBool then
      SetBoolProp(myObj,PropName,myStrToBool(NewValue))
    else if pType = tkDynArray then
    begin
      //showmessage('arg... property '+PropName+' needs tkDynArray type');       //!!!!
      targetNode.SetAttributeValue(PropName,newValue);
    end
    {$endif}
    else
      showmessage('EditAttributeValue. ** Need to handle property type for '+targetNode.NodeName+'.'+PropName);
  end;
end;


procedure EditAttributeValue(NodeNameToEdit:String; SourceAttrib:TNodeAttribute;AddIfMissing:Boolean);
var
  targetNode:TDataNode;
  targetAttrib:TNodeAttribute;
  myObj:TObject;
  tmp,AttrNameToEdit,newValue:String;
  IsReadOnly:Boolean;
begin
  targetNode:=FindDataNodeById(SystemNodetree,NodeNameToEdit,true);
  if targetNode<>nil then
  begin
    AttrNameToEdit:=SourceAttrib.AttribName;
    newValue:=SourceAttrib.AttribValue;
    IsReadOnly:=SourceAttrib.AttribReadOnly;
    targetAttrib:=targetNode.GetAttributeAnyCase(AttrNameToEdit,AddIfMissing);
    if (targetAttrib.AttribName<>'') then
    begin
      AttrNameToEdit:=targetAttrib.AttribName;
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
      and (IsReadOnly=false)
      and (IsPublishedProp(myObj,AttrNameToEdit)) then
      begin
        SetXObjectProperty(myObj,targetNode,AttrNameToEdit,targetAttrib.AttribType,newValue);
      end
      else
      begin
        // no screen or interface object associated with this node - just set the node attribute value
        targetNode.SetAttributeValue(AttrNameToEdit,newValue);
      end;

    end;

  end;
end;
procedure EditAttributeValue(NodeNameToEdit,AttrNameToEdit,newValue:String;AddIfMissing:Boolean);
var
  SourceAttrib:TNodeAttribute;
begin
  SourceAttrib.AttribName:=AttrNameToEdit;
  SourceAttrib.AttribValue:=newValue;
  SourceAttrib.AttribType:='String';
  SourceAttrib.AttribReadOnly:=false;
  EditAttributeValue(NodeNameToEdit,SourceAttrib,AddIfMissing);
end;
procedure EditAttributeValue(NodeNameToEdit,AttrNameToEdit,newValue:String);
var
  SourceAttrib:TNodeAttribute;
begin
  SourceAttrib.AttribName:=AttrNameToEdit;
  SourceAttrib.AttribValue:=newValue;
  SourceAttrib.AttribType:='String';
  SourceAttrib.AttribReadOnly:=false;
  EditAttributeValue(NodeNameToEdit,SourceAttrib,false);
end;
{$ifndef JScript}
procedure EditAttributeValue(NodeNameToEdit,AttrNameToEdit,newValue:PChar);
var
  SourceAttrib:TNodeAttribute;
begin
  SourceAttrib.AttribName:=AttrNameToEdit;
  SourceAttrib.AttribValue:=StrPas(newValue);
  SourceAttrib.AttribType:='String';
  SourceAttrib.AttribReadOnly:=false;
  EditAttributeValue(NodeNameToEdit,SourceAttrib,false);
end;
{$endif}
procedure EditAttributeValue2(NodeNameToEdit,AttrNameToEdit,newValue:String); // for direct calls via JS (doesn't handle overloads)
begin
  EditAttributeValue(NodeNameToEdit,AttrNameToEdit,newValue);
end;


procedure EditAttributeValueIndexed(NodeNameToEdit,AttrNameToEdit:String;newValue:TStringArray; x,y:integer);
var
  targetNode:TDataNode;
  targetAttrib:TNodeAttribute;
  myObj:TObject;
  pType:TTypeKind;
  tmp:String;
  arr:TStringArray;
  parr:Pointer;
begin
  targetNode:=FindDataNodeById(SystemNodetree,NodeNameToEdit,true);
  if targetNode<>nil then
  begin
    targetAttrib:=targetNode.GetAttributeAnyCase(AttrNameToEdit);
    if targetAttrib.AttribName<>'' then
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
function DeleteScreenObject(MyNode:TDataNode):string;
var
    ObjName:string;
begin
   ObjName:=MyNode.NodeName;

  asm
    try{
    var ThisObject = document.getElementById(ObjName);
    if (ThisObject!=null) {
       ThisObject.parentNode.removeChild(ThisObject);
      }
    }catch(err) { alert(err.message+' in NodeUtils.DeleteScreenObject');}
  end;

  NilScreenObject(MyNode);

end;
{$endif}

procedure DeleteNodeChildren(ParentNode:TDataNode);
var
    i:integer;
    handlers:TEventHandlers;
begin
//ShowMessage('dnc '+ParentNode.NodeName);
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
      EditAttributeValue(StartNode.NodeName,'SuspendRefresh','False');
  for i:=0 to length(StartNode.ChildNodes)-1 do
    UnSuspendFrames(StartNode.ChildNodes[i]);
end;

function checkData(SystemDescription:string):boolean;
var teststring,teststring2,sys:string;    // this checs a longer string after un encryption than the function isvalidsystemdata
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
  result:=MatchFound;
end;


function XMLToNodeTree(XMLString:String):String;
var
  i:integer;
  TempChar,NextChar,NewString,RootNodeName:String;
  BracesToggleFlag:boolean;
  glb:Boolean;
begin

  if checkData(XMLString)= true then
  begin

    StartingUp:=true;
   {$ifdef JScript}
//   showmessage('XMLToNodeTree. Node '+SystemRootName+' has '+inttostr(length(systemnodetree.childnodes))+' children');
   RootNodeName:=SystemRootName;
   asm
      var ob=document.getElementById(RootNodeName);
      $mod.SystemNodeTree.ScreenObject=ob;
    end;

    {$endif}

    NewString:='';
    for i:=1 to Length(XMLString) do
    begin
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
          addComponentFromXML(newstring);// process the record string if this was a leading xml string such as "< type; name= ''; attr_1 = '???' etc>"
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

//    InitialiseLinks(SystemNodeTree);


//ShowMessage('all components added');
    StartingUp:=false;

  end
  else
    ShowMessage('Error .....Unable to load data');

end;

(*
{$ifndef JScript}
procedure Initialiselinks(StartNode:TDataNode);
var
  i:integer;
  targetNode:TDataNode;
  myLink:TXPropertyLink;
begin
  // system has been loaded.  Look for links specified in all nodes and
  // locate the named target nodes.
 //showmessage('init links '+StartNode.NodeName);

  //!!!!  TBA ?

  for i := 0 to length(StartNode.ChildNodes) -1 do
  begin
//     showmessage('child '+inttostr(i)+' of '+StartNode.NodeName+' - '+StartNode.ChildNodes[i].NodeName);
     Initialiselinks(StartNode.ChildNodes[i]);
  end;
end;
{$else}
procedure Initialiselinks(StartNode:TDataNode);
var
  i:integer;
  targetNode:TDataNode;
  myLink:TXPropertyLink;
begin
  // system has been loaded.  Look for links specified in all nodes and
  // locate the named target nodes.
 //showmessage('init links '+StartNode.NodeName);
  if (StartNode is TInterfaceObject) then
    if TInterfaceObject(StartNode).Link <> nil then
    begin
      myLink:=TInterfaceObject(StartNode).Link;
      if myLink.TIObjectName<>'' then
      begin
        targetNode:=FindDataNodeById(SystemNodeTree,myLink.TIObjectName,false);
        if targetNode<>nil then
           myLink.TIObject := targetNode
        else
           showmessage('Initialiselinks.  Node is nil. '+myLink.TIObjectName);
      end;
    end;
  for i := 0 to length(StartNode.ChildNodes) -1 do
  begin
//     showmessage('child '+inttostr(i)+' of '+StartNode.NodeName+' - '+StartNode.ChildNodes[i].NodeName);
     Initialiselinks(StartNode.ChildNodes[i]);
  end;
end;

procedure PushTolinks(AObject:TObject; PropName:string; PropValue:String; StartNode:TDataNode);
var
  i:integer;
  MyPropType:TTypeKind;
begin
  // a component property has changed.  Look for links in other components that
  // reference this property, and update those values.
  // Propname is the property in AObject that has changed.
 //showmessage('PushTolinks 1. '+StartNode.NodeName);
  if (StartNode is TInterfaceObject) then
    if TInterfaceObject(StartNode).Link <> nil then
      if (TInterfaceObject(StartNode).Link.FTIObject = AObject)
      and (TInterfaceObject(StartNode).Link.TIPropertyName = PropName) then
      begin
        MyPropType := PropType(AObject, PropName);
        if MyPropType = tkString then
          SetStringProp(AObject,PropName,PropValue)
        else if MyPropType = tkBool then
          SetBoolProp(AObject,PropName,myStrToBool(PropValue))
        else
          showmessage('PushTolinks.  Need to handle property type for '+PropName);
      end;
  //showmessage('PushTolinks 2');
  //for i := 0 to length(TDataNode(AObject).ChildNodes) -1 do
  for i := 0 to length(StartNode.ChildNodes) -1 do
     PushToLinks(AObject,PropName,PropValue,StartNode.ChildNodes[i]);
  //showmessage('PushTolinks done');
end;

{$endif}
*)

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
begin
  AttribOptionsRec.ComponentType:=ComponentType;
  AttribOptionsRec.AttribName:=AttribName;
  AttribOptionsRec.Options:=TStringList.Create;
  for i:=0 to length(options)-1 do
    AttribOptionsRec.Options.Add(Options[i]);
  setlength(AttribOptionsArray,length(AttribOptionsArray)+1);
  AttribOptionsArray[length(AttribOptionsArray)-1]:=AttribOptionsRec;
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


procedure InitSystemNodetree;
begin
  SystemNodetree:=TDataNode.Create('Root','ApplicationRoot','Root',false);
  // create a parent node for all UI nodes
  UIRootNode:=TDataNode.Create('Root',SystemRootName,'Root',false);
  AddChildToParentNode(SystemNodeTree,UIRootNode,-1);
  // create a parent node for all Code nodes
  CodeRootNode:=TDataNode.Create('Code',CodeRootName,'Root',false);
  AddChildToParentNode(SystemNodeTree,CodeRootNode,-1);
end;

{$ifndef JScript}
procedure InitialiseXComponentsProject;
begin
  // PasteDialog and CompilerLog forms are in XComponents units - need to instantiate the forms here.
  PasteDialogUnit.SetupPasteDialogForm;
  CompilerLogUnit.SetupCompilerLogForm;
  {$ifdef Chromium}
  // !!!!cef issue. all of this has to be done in the main project unit, else it doesn't work at all...
  //GlobalCEFApp.OnProcessMessageReceived := @GlobalCEFProc.GlobalCEFApp_OnProcessMessageReceived;
//  GlobalCEFApp.OnProcessMessageReceived := @GlbObject.GlobalCEFApp_OnProcessMessageReceived;
//  if GlbObject<>nil then
//    GlobalCEFApp.OnProcessMessageReceived := @GlbObject.GlobalCEFApp_OnProcessMessageReceived
//  else
//    showmessage('GlbObject is nil');
  {$endif}
end;
{$endif}



//-------------------------------------------------------------------------------------------
begin
//  SetLength(DefaultAttribsByType,0);
  InitSystemNodeTree;
  SuppressEvents:=false;

  {$ifndef JScript}
  if not DirectoryExists('tempinc') then
    CreateDir('tempinc');
  {$endif}
end.
