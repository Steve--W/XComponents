(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit Events;
interface
uses
  SysUtils,Classes, StringUtils, NodeUtils, EventsInterface,
  {$ifndef JScript}
  LCLType, FileUtil, DateTimePicker,  Forms, Controls,
  Graphics, Dialogs, StdCtrls, ExtCtrls, Menus,   ColorBox,Clipbrd,
  ComCtrls, Spin,lclintf , EditBtn, Dynlibs,
  LazsUtils;
  {$Else}
  HTMLUtils;
  {$endif}

type
{$ifndef JScript}
  TEventClass = class
    procedure HandleResizeComponent(Sender:TObject);
    procedure DoEventLater(Data: PtrInt);
  end;


type
  // record type for queued async functions
  TQueueRec = record
    QEventType: string;
    QNodeId: string;
    QEventValue: string;
  end;
  PQueueRec = ^TQueueRec;

function ReloadDll(DllFileName:String):Boolean;
procedure CallHandleEvent(e:TEventStatus;EventType,MyValue:string; MyControl:TObject);  overload;
procedure CallHandleEvent(EventType,MyValue:string; MyControl:TObject);  overload;
procedure CallHandleEventlater(EventType,MyValue:string; MyControl:TObject);
procedure HandleEventLater(e:TEventStatus;EventType,NodeId,MyValue:string);

var   EventCode : TEventClass;   // contains event handling procedures
      MyLibC: TLibHandle= dynlibs.NilHandle;

{$ifndef JScript}
var mmi : IInterface;    // type is  IMyMethodInterface
{$endif}

{$else}
function FindEventFunction(myName,EventType:string;MyNode:TDataNode;DoBind:Boolean):TObject;
{$endif}

var DllName:String;

procedure handleEvent(e:TEventStatus;MyEventType,nodeID,myValue,PropName:string);     overload;
procedure handleEvent(e:TEventStatus;MyEventType,nodeID,myValue:string);     overload;
procedure handleEvent(MyEventType,nodeID,myValue:string);     overload;
{$ifndef JScript}
procedure handleEvent(e:TEventStatus;MyEventType,nodeID,myValue:PChar);     overload;
{$endif}

implementation
{$ifndef JScript}

uses XIFrame, XSVGContainer;

function FindNodeId(MyControl:TObject; var outer:TObject):String;
var
  nodeID:string;
begin
  if (MyControl is TControl)
  and (TControl(MyControl).Parent<>nil) then
  begin
    // showmessage(EventType);
    outer:= FindOuterParentOf(TControl(MyControl));
    if outer<>nil then
      nodeID:= TControl(outer).Name;
  end
  {$ifdef Chromium}
  else if (MyControl is TXSVGWidget) then
  begin
    outer:=MyControl;
    nodeId:=TDataNode(MyControl).NodeName;
  end
  {$endif}
  else if (MyControl is TMenuItem) then
  begin
    outer:=MyControl;
    nodeID:= TMenuItem(outer).Name;
  end;
  result:=nodeID;
end;

procedure CallHandleEvent(e:TEventStatus;EventType,MyValue:string; MyControl:TObject);
var
  nodeID:string;
  outer:TObject;
begin
   if (not SuppressEvents)
   and (MyControl<>nil) then
   begin
     nodeID := FindNodeId(MyControl,outer);

     if outer<>nil then
     begin
       handleEvent(e,EventType,nodeID,MyValue);
     end;

   end;
end;
procedure CallHandleEvent(EventType,MyValue:string; MyControl:TObject);
begin
  CallHandleEvent(nil,EventType,MyValue,MyControl);
end;
procedure TEventClass.DoEventLater(Data: PtrInt);
var
  ReceivedQueueRec: TQueueRec;
begin
  ReceivedQueueRec := PQueueRec(Data)^;

  HandleEvent(nil,ReceivedQueueRec.QEventType,ReceivedQueueRec.QNodeId,ReceivedQueueRec.QeventValue);

end;

procedure CallHandleEventLater(EventType,MyValue:string; MyControl:TObject);
var
  outer:TObject;
  QueueRecToSend: PQueueRec;
  nodeID:String;
begin
  nodeID := FindNodeId(MyControl,outer);

   if outer<>nil then
   begin

     New(QueueRecToSend);
     QueueRecToSend^.QEventType:=EventType;
     QueueRecToSend^.QeventValue:=MyValue;
     QueueRecToSend^.QnodeID := FindNodeId(MyControl,outer);
     Application.QueueAsyncCall(@EventCode.DoEventLater, PtrInt(QueueRecToSend)); // put msg into queue that will be processed from the main thread after all other messages

  end;
end;
procedure HandleEventLater(e:TEventStatus;EventType,NodeId,MyValue:string);
var
  QueueRecToSend: PQueueRec;
begin
  New(QueueRecToSend);
  QueueRecToSend^.QEventType:=EventType;
  QueueRecToSend^.QeventValue:=MyValue;
  QueueRecToSend^.QnodeID := NodeId;
  Application.QueueAsyncCall(@EventCode.DoEventLater, PtrInt(QueueRecToSend)); // put msg into queue that will be processed from the main thread after all other messages
end;

procedure TEventClass.HandleResizeComponent(Sender:TObject);
var
    myComponent,pr:TControl;
begin
   myComponent:= TControl(Sender);
   CheckPercentageSizing(myComponent);
end;


{$Else}

implementation

{$endif}

{$ifndef JScript}
function ExecDLLInitFunc:String;
type
   TMyFunc=procedure(mmi : IInterface); stdcall;
var
   fn: TMyFunc;
begin
  if MyLibC<>dynlibs.NilHandle then
  begin

    fn:= TMyFunc(GetProcedureAddress(MyLibC,'SetDllContext'));
    if Assigned(fn) then
    begin
      fn (mmi);   //Executes the dll function SetDllContext
    end
    else
      result:='dll Function SetDllContext not found'   ;
  end;
end;


function ReloadDll(DllFileName:String):Boolean;
var
  S:String;
begin
  if MyLibC <>  DynLibs.NilHandle then
    if FreeLibrary(MyLibC) then
      MyLibC:= DynLibs.NilHandle;  //Unload the lib, if already loaded

  MyLibC := LoadLibrary(ProjectDirectory+'resources/project/'+DllFileName);

  if MyLibC = dynlibs.NilHandle then
  begin
    S:=GetLoadErrorStr;
    result:=false;
  end
  else
    ExecDLLInitFunc;
end;

function ExecDLLEventFunc(e:TEventStatus;procname:String;Params:TstringList):String;
type
   TMyFunc=procedure(e:TEventStatus;nodeID:AnsiString;myValue:AnsiString); stdcall;     // same as TEventHandler
var
   fn: TMyFunc;
   myNode:TDataNode;
begin
  if MyLibC = dynlibs.NilHandle then
  {$ifdef windows}
    ReloadDll(DllName+'.'+SharedSuffix);
  {$endif}
  {$ifdef linux}
    ReloadDll('lib'+DllName+'.'+SharedSuffix);
  {$endif}

  if MyLibC<>dynlibs.NilHandle then
  begin

    fn:= TMyFunc(GetProcedureAddress(MyLibC,procname));
    if Assigned(fn) then
      if Params.Count=2 then
      begin
        try
          fn (e,Params[0],Params[1]);   //Executes the function
        except
          on ex:exception do
            showmessage('Error raised in event function '+procname+': '+ex.Message);
        end;
      end
      else
        showmessage('func '+procname+' need to handle parameters in ExecDLLEventFunc')
    else
      result:='Function '+procname+' not found in ExecDLLEventFunc'   ;

  end;
end;

procedure RunComponentEvent(e:TEventStatus;myName,EventType:string;MyNode:TDataNode;MyValue:string);
var
  m: TMethod;
  params:TStringList;
begin
  m.Code:=nil;
  //find existing event handler (in the form) for a component created in Lazarus IDE with compiled events...
  if MyNode.MyForm<>nil then
    m.Code := MyNode.MyForm.MethodAddress(myName+'Handle'+EventType);
  if m.Code=nil then
  begin
    // the component may have been created programatically at run-time, while the event handling function already exists in the project,
    // in which case look for a registered event.
    m := TMethod(MyNode.FindRegisteredEvent(EventType));
  end;
  if m.Code=nil then
  begin

    if ((MyNode.IsDynamic) or (myNode=UIRootNode))
    and (MyNode.HasUserEventCode(EventType)) then
    begin
      // third option - a dynamically created component with dynamically created event code (eg. created in XIDE project)...
      // in this case the code will have been compiled into the events dll
      params:=TStringList.Create;
      params.Add(myname);
      params.Add(myValue);
      ExecDLLEventFunc(e,myName+'Handle'+EventType,params);
      params.Free;
    end
    else
       EXIT;                 // or no handler has been defined
  end
  else
  begin
    m.Data := pointer(MyNode.MyForm); //store pointer to form object instance
    TEventHandler(m)(e,myName,myValue);
  end;
end;
{$else}
function FindEventFunction(myName,EventType:string;MyNode:TDataNode;DoBind:Boolean):TObject;
var
  UnitName:String;
  fn:TObject;
begin
  UnitName:=MainUnitname+'Events';
  fn:=nil;
asm
try {
//alert('FindEventFunction NodeName='+MyNode.NodeName);

  fn=null;
  var handlerName=myName+'Handle'+EventType;

  // FIRST ..... Look for a compiled handler function in the Form ....
  if (MyNode.MyForm!=null) {
    fn = MyNode.MyForm[handlerName];
    if (fn!=null) {
      fn = fn.bind(MyNode.MyForm);     // so that the 'this' context will be preserved
    }
  }

  // SECOND .....
  if (fn==null) {
  // the component may have been created dynamically at run-time.
  // in which case look for a registered event.
    fn = MyNode.FindRegisteredEvent(EventType);
  }

  // THIRD ......
  if (fn==null) {
  // the component may have been created dynamically at run-time
  // with dynamically added event code (eg. using the XIDE project)
  // in which case look for the event handler in module XIDEMainEvents

    //alert('RunComponentEvent looking in dynamic events unit '+UnitName+' for '+handlerName);
    var mdl=pas[UnitName];
    if ((mdl!=null)&&(mdl!=undefined)) {
      //alert('found module '+UnitName);
        fn = mdl[ handlerName];
        if (fn!=null) {
          //alert('found function '+handlerName);
          if (DoBind) {
          fn = fn.bind(mdl); }    // so that the 'this' context will be preserved
        }
      }
    }

    if (fn==undefined) {fn=null;}

}catch(err) { alert(err.message+'  in Events.FindEventFunction '+myName+' '+EventType);}
end;
result:=fn;
end;

procedure RunComponentEvent(e:TEventStatus;myName,EventType:string;MyNode:TDataNode;MyValue:string);
var
  UnitName:String;
  fn:TObject;
begin
 // showmessage('RunComponentEvent '+EventType+' '+MyNode.NodeName);
  UnitName:=MainUnitname+'Events';
  fn:=FindEventFunction(myName,EventType,MyNode,true);

  asm
  try {
      // Execute the function, if found....
    if (fn!=null)  {
    fn(e,myName,MyValue);
     // alert('function done.');
  }
  }catch(err) { alert(err.message+'  in Events.RunComponentEvent '+myName+' '+EventType);}
  end;

end;
{$endif}

function ExecuteEventTrappers(e:TEventStatus;MyEventType,nodeID,myValue:string;myNode:TDataNode): String ;
 var
    i,j,NumHandlers:integer;
    trappers:TNodesArray;
    newe:TEventStatus;
 begin
   //ShowMessage('ExecuteEventTrappers. '+MyEventType+' NodeId='+nodeID+' value='+myValue);

   trappers:=FindNodesOfType(SystemNodeTree,'TXTrapEvents');

   for i:=0 to length(trappers)-1 do
   if trappers[i].NodeClass='NV' then    // exclude 'RNV' class
   begin
     NumHandlers:= trappers[i].MyEventTypes.count;
     for j:=0 to NumHandlers - 1 do
     begin
         // Execute the registered event handler if it exists
       //ShowMessage('ExecuteEventTrapper. '+trappers[i].NodeName+' '+MyEventType+' NodeId='+nodeID+' value='+myValue);
         newe:=TEventStatus.Create(MyEventType,nodeID);
         newe.eventValue:=myValue;
         RunComponentEvent(newe,trappers[i].NodeName,trappers[i].MyEventTypes[j],trappers[i],'');
         e.ContinueAfterTrappers := newe.ContinueAfterTrappers;
     end;
   end;

end;

function ExecuteEventHandlers(e:TEventStatus;MyEventType,nodeID,myValue:string;myNode:TDataNode): String ;
  var
     i,NumHandlers:integer;
  begin
    //ShowMessage('ExecuteEventHandlers. '+MyEventType+' NodeId='+nodeID+' value='+myValue);
    NumHandlers:= myNode.MyEventTypes.count;
    for i:=0 to NumHandlers - 1 do
    begin
      if  myNode.MyEventTypes[i]=MyEventType then
      begin
        // Execute the registered event handler if it exists
        RunComponentEvent(e,nodeID,MyEventType,myNode,myValue);
      end;
    end;
end;

procedure  handleEvent(e:TEventStatus;MyEventType,nodeID,myValue,PropName:string);
var
  CurrentNode :TDataNode;
  m: TMethod;
  {$ifndef JScript}
  sc:TCursor;
  {$endif}
begin
//  if MyEventType='notnow' then EXIT;

  if StartingUp = false then
  begin

    if e=nil then
    begin
      e:=TEventStatus.Create(MyEventType,nodeID);
    end;


   //ShowMessage('handle event...'+MyEventType+' '+nodeID);

     // Identify the system node.
     // nodeID is the screen object name (nodename in the TDataNode tree)
     //   **** exception - HTML TreeNode events send in the name of the node object
     {$ifndef JScript}
     sc:=Screen.Cursor;
     Screen.Cursor := crHourglass;
     CurrentNode:=FindDataNodeById(SystemNodeTree,nodeID,false);
     {$else}
     if (MyEventType = 'TreeNodeClick')
     or (MyEventType = 'DragStart')
     or (MyEventType = 'Drop')
     then
     begin
       // try to get the system node name from the tree node name...
       CurrentNode:=GetDataNodeFromTreeNode(nodeID);
     end
     else
       CurrentNode:=FindDataNodeById(SystemNodeTree,nodeID,false);
     {$endif}

     //if CurrentNode=nil then
     //  ShowMessage('handleEvent.'+MyEventType+'   Cannot find node '+nodeID)
     //else
     //  showmessage('node found');

    if (CurrentNode<>nil)
    and (MainForm<>nil) then
    begin
 //       {$ifdef JScript}
 //       // discover links that the new value needs pushing to
 //       if (PropName<>'') and (PropName<>'undefined') then
 //       begin
 //         //showmessage('propname=>'+PropName+'<');
 //         PushToLinks(CurrentNode,PropName,myValue, SystemNodeTree);  // propname is the changed property in CurrentNode
 //       end;
 //       {$endif}

        // Look for XTrapEvents components.  Each may have a 'HandleAny' process defined.
         if e.InitRunning=false then
           // don't re-trap any event that is entering the second phase (after async function)
           ExecuteEventTrappers(e,MyEventType,CurrentNode.nodeName,myValue,CurrentNode)
         else
           e.ContinueAfterTrappers:=true;

         if e.ContinueAfterTrappers then
         begin
            // run the specific event handler defined in the form for this component and event type (if any)
            //showmessage('calling ExecuteEventHandlers');
            ExecuteEventHandlers(e,MyEventType,CurrentNode.nodeName,myValue,CurrentNode) ;
         end;

     end;
     {$ifndef JScript}
     Screen.Cursor:=sc;
     {$endif}
   end;
end;

procedure  handleEvent(e:TEventStatus;MyEventType,nodeID,myValue:string);
begin
  handleEvent(e,MyEventType,nodeID,myValue,'');
end;
procedure  handleEvent(MyEventType,nodeID,myValue:string);
begin
  handleEvent(nil,MyEventType,nodeID,myValue,'');
end;
{$ifndef JScript}
procedure  handleEvent(e:TEventStatus;MyEventType,nodeID,myValue:PChar);
begin
  handleEvent(e,MyEventType,nodeID,StrPas(myValue),'');
end;
{$endif}
//======================================================================================================
//============================================  End of Common Events Code ================================
//======================================================================================================
Begin
 {$ifndef JScript}
  EventCode := TEventClass.Create;
{$endif}

end.