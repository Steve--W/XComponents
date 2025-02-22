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
  SysUtils,Classes, StringUtils, NodeUtils, EventsInterface,TypInfo,
  {$ifndef JScript}
  LCLType, {FileUtil,} DateTimePicker,  Forms, Controls,
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
    QNameSpace: string;
    QEventValue: string;
  end;
  PQueueRec = ^TQueueRec;

function ReloadDll(DllFileName:String):Boolean;
function CallHandleEvent(e:TEventStatus;EventType,MyValue:string; MyControl:TObject):Boolean;  overload;
function CallHandleEvent(EventType,MyValue:string; MyControl:TObject):Boolean;  overload;
procedure CallHandleEventlater(EventType,MyValue:string; MyControl:TObject);
procedure HandleEventLater(e:TEventStatus;EventType,NodeId,NameSpace,MyValue:string);

var   EventCode : TEventClass;   // contains event handling procedures
      MyLibC: TLibHandle= dynlibs.NilHandle;
      ConsoleString:String;
      ConsoleNode:TDataNode;
      FPCAvailable:Boolean;

var mmi : IInterface;    // type is  IMyMethodInterface

{$else}
function FindEventFunction(NameSpace,myName,EventType:string;MyNode:TDataNode;DoBind:Boolean):TObject;
{$endif}

var
  DllName:String;

function FindAndRunEventHandler(e:TEventStatus;MyEventType,nodeID,myValue:string;myNode:TDataNode):Boolean;
function handleEvent(e:TEventStatus;MyEventType,nodeID,NameSpace,myValue,PropName:string):Boolean;  overload;
function handleEvent(e:TEventStatus;MyEventType,nodeID,NameSpace,myValue:string):Boolean;           overload;
function handleEvent(MyEventType,nodeID,NameSpace,myValue:string):Boolean;                          overload;
{$ifndef JScript}
function FindNodeId(MyControl:TObject; var outer:TObject; var NameSpace:String):String;
{$endif}

procedure ExecuteEventHandler(e:TEventStatus;NodeId: String; myValue: String; initfunc,mainfunc:THandler;InitPyCode,MainPyCode:String);
procedure RunTheEventFunc(e:TEventStatus;NodeId: String; myValue: String; fn:THandler;PyCode:String);

implementation
uses XTabControl, XComboBox, XForm,
{$ifndef JScript}
XIFrame, XSVGContainer,
{$endif}
Xtree;

{$ifndef JScript}
function FindNodeId(MyControl:TObject; var outer:TObject; var NameSpace:String):String;
var
  nodeID:string;
  componentNode:TDataNode;
begin
  if (MyControl is TControl)
  and (TControl(MyControl).Parent<>nil) then
  begin
    outer:= FindOuterParentOf(TControl(MyControl));
    if (outer<>nil) and (HasProperty(outer,'myNode')) then
    begin
      ComponentNode:=TDataNode(GetObjectProp(outer,'MyNode'));
      nodeID:=ComponentNode.NodeName;
      NameSpace:=ComponentNode.NameSpace;
    end;
  end
  {$ifdef Chromium}
  else if (MyControl is TXSVGWidget) then
  begin
    outer:=MyControl;
    nodeId:=TDataNode(MyControl).NodeName;
    NameSpace:=TDataNode(MyControl).NameSpace;
  end
  {$endif}
  else if (MyControl is TMenuItem) then
  begin
    outer:=MyControl;
    nodeID:= TMenuItem(outer).Name;
    Namespace:='';
  end
  else if (MyControl is TComponent) then    //!! mynode exists???
  begin
    outer:=MyControl;
    nodeID:= TComponent(outer).Name;
    Namespace:='';
  end;
  result:=nodeID;
end;

function CallHandleEvent(e:TEventStatus;EventType,MyValue:string; MyControl:TObject):Boolean;
var
  nodeID,NameSpace:string;
  outer:TObject;
  ok:Boolean;
begin
   ok:=true;
   if (not SuppressEvents)
   and (MyControl<>nil) then
   begin
     nodeID := FindNodeId(MyControl,outer,NameSpace);

     if outer<>nil then
     begin
       ok:=handleEvent(e,EventType,nodeID,NameSpace,MyValue);
     end;

   end;
   result:=ok;
end;
function CallHandleEvent(EventType,MyValue:string; MyControl:TObject):Boolean;
begin
  result:=CallHandleEvent(nil,EventType,MyValue,MyControl);
end;
procedure TEventClass.DoEventLater(Data: PtrInt);
var
  ReceivedQueueRec: TQueueRec;
begin
  ReceivedQueueRec := PQueueRec(Data)^;

  HandleEvent(nil,ReceivedQueueRec.QEventType,ReceivedQueueRec.QNodeId,ReceivedQueueRec.QNameSpace,ReceivedQueueRec.QeventValue);

end;

procedure CallHandleEventLater(EventType,MyValue:string; MyControl:TObject);
var
  outer:TObject;
  QueueRecToSend: PQueueRec;
  nodeID,NameSpace:String;
begin
  nodeID := FindNodeId(MyControl,outer,NameSpace);

   if outer<>nil then
   begin

     New(QueueRecToSend);
     QueueRecToSend^.QEventType:=EventType;
     QueueRecToSend^.QeventValue:=MyValue;
     QueueRecToSend^.QnodeID := nodeID;
     QueueRecToSend^.QNameSpace := NameSpace;
     Application.QueueAsyncCall(@EventCode.DoEventLater, PtrInt(QueueRecToSend)); // put msg into queue that will be processed from the main thread after all other messages

  end;
end;
procedure HandleEventLater(e:TEventStatus;EventType,NodeId,NameSpace,MyValue:string);
var
  QueueRecToSend: PQueueRec;
begin
  New(QueueRecToSend);
  QueueRecToSend^.QEventType:=EventType;
  QueueRecToSend^.QeventValue:=MyValue;
  QueueRecToSend^.QnodeID := NodeId;
  QueueRecToSend^.QNameSpace := NameSpace;
  Application.QueueAsyncCall(@EventCode.DoEventLater, PtrInt(QueueRecToSend)); // put msg into queue that will be processed from the main thread after all other messages
end;

procedure TEventClass.HandleResizeComponent(Sender:TObject);
var
    myComponent,pr:TControl;
begin
   myComponent:= TControl(Sender);
   CheckPercentageSizing(myComponent);
end;


//{$Else}
//
//implementation
//
{$endif}

{$ifndef JScript}
function ExecDLLInitFunc:String;
type
   TMyFunc=procedure(mmi : IInterface; evh:TExEvHandler); stdcall;
var
   fn: TMyFunc;
begin
  if MyLibC<>dynlibs.NilHandle then
  begin

    fn:= TMyFunc(GetProcedureAddress(MyLibC,'SetDllContext'));
    if Assigned(fn) then
    begin
      fn (mmi,ExecuteEventHandlerFunc);   //Executes the dll function SetDllContext
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


function ExecDLLEventFunc(e:TEventStatus;procname:String;Params:TstringList):Boolean;
type
   TMyFunc=procedure(e:TEventStatus;nodeID:AnsiString;myValue:AnsiString); stdcall;     // same as TEventHandler
var
   fn: TMyFunc;
   myNode:TDataNode;
   ok:Boolean;
begin
  ok:=true;
  if MyLibC = dynlibs.NilHandle then
  {$ifdef windows}
    ReloadDll(DllName+'.'+SharedSuffix);
  {$endif}
  {$ifdef linux}
    ReloadDll('lib'+DllName+'.'+SharedSuffix);
  {$endif}

  if MyLibC<>dynlibs.NilHandle then
  begin
    glbEvent:=e;
    fn:= TMyFunc(GetProcedureAddress(MyLibC,procname));
    if Assigned(fn) then
      if Params.Count=2 then
      begin
        try
          fn (e,Params[0],Params[1]);   //Executes the event handler function
        except
          on ex:exception do
            showmessage('Error raised in event function '+procname+': '+ex.Message);
        end;
      end
      else
        showmessage('func '+procname+' need to handle parameters in ExecDLLEventFunc')
    else
    begin
      ok:=false;
      showmessage('Function '+procname+' not found in ExecDLLEventFunc')   ;
    end;

  end
  else ok:=false;
  result:=ok;
end;

function RunComponentEvent(e:TEventStatus;myName,NameSpace,EventType:string;MyNode:TDataNode;MyValue:string):Boolean;
var
  m: TMethod;
  params:TStringList;
  Event:TEventHandlerRec;
  HandlerFound:Boolean;
begin
  HandlerFound:=false;
  m.Code:=nil;
  //find existing event handler (in the form) for a component created in Lazarus IDE with compiled events...
  if MyNode.MyForm<>nil then
  begin
    m.Data := pointer(MyNode.MyForm); //store pointer to form instance  (self of the function)
    m.Code := MyNode.MyForm.MethodAddress(myName+'Handle'+EventType);
  end;
  if m.Code=nil then
  begin
    // the component may have been created programatically at run-time, while the event handling function already exists in the project,
    // in which case look for a registered event.
    m := TMethod(MyNode.FindRegisteredEvent(EventType));
  end;
  if m.Code=nil then
  begin

    if ((MyNode.IsDynamic) or (myNode=UIRootNode))
    and (MyNode.HasUserEventCode(EventType))
    and (not SuppressUserEvents) then
    begin
      Event:= MyNode.GetEvent(EventType);
      // third option - a dynamically created component with dynamically created event code (eg. created in XIDE project)...
      // in this case the code will have been compiled into the events dll
      if Event.EventLanguage = 'Pascal' then
      begin
        params:=TStringList.Create;
        params.Add(myname);
        params.Add(myValue);
        HandlerFound:=ExecDLLEventFunc(e,e.NameSpace+myName+'Handle'+EventType,params);
        params.Free;
        if not HandlerFound then
          if not FPCAvailable then
            ShowMessage('Warning: Cannot execute Pascal code. FPC unavaliable (check Settings)');
      end
      else if Event.EventLanguage = 'Python' then
      begin
        m := TMethod(TXForm(MainForm).MyNode.FindRegisteredEvent('PythonEvent'));  // a generic function to find and execute python script
        if m.Code=nil then EXIT(false);
        if m.Data=nil then
          m.Data := pointer(TXForm(MainForm).MyNode.ScreenObject); //store pointer to form object

        TEventHandler(m)(e,myName,myValue);
      end;
    end
    else
       EXIT(false);                 // or no handler has been defined
  end
  else
  begin
    HandlerFound:=true;
    if m.Data=nil then
      m.Data := pointer(MyNode.ScreenObject); //store pointer to object instance  (self of the function)
    TEventHandler(m)(e,myName,myValue);
  end;
  if (ConsoleString<>'') and (ConsoleNode<>nil) then
  begin
    ConsoleString:=ConsoleNode.GetAttribute('ItemValue',false).AttribValue + ConsoleString;
    EditAttributeValue(ConsoleNode,'ItemValue',PChar(ConsoleString));
    ConsoleString:='';
  end;
  result:=HandlerFound;

end;
{$else}
function FindEventFunction(NameSpace,myName,EventType:string;MyNode:TDataNode;DoBind:Boolean):TObject;
var
  UnitName,Language:String;
  AllowUserEvents:Boolean;
  fn:TObject;
  Event:TEventHandlerRec;
begin
  UnitName:=MainUnitname+'Events';
  fn:=nil;
  AllowUserEvents:=not SuppressUserEvents;
  Event:=MyNode.GetEvent(EventType);
  if Event<>nil then
    Language:=Event.EventLanguage
  else
    Language:='Pascal';
asm
try {
//console.log('FindEventFunction NS='+NameSpace+' myName='+myName+' MyNode='+MyNode.NameSpace+'.'+MyNode.NodeName);
//console.log('FindEventFunction '+EventType+' MyNode='+MyNode.NameSpace+'.'+MyNode.NodeName);
  fn=null;
  var handlerName=NameSpace+myName+'Handle'+EventType;

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
  if ((fn==null)&&(AllowUserEvents==true)) {
  if (Language == 'Pascal') {
  // the component may have been created dynamically at run-time
  // with dynamically added event code (eg. using the XIDE project)
  // in which case look for the event handler in module XIDEMainEvents
    if (NameSpace!='') {UnitName = NameSpace;}
    //console.log('FindEventFunction looking in dynamic events unit '+UnitName+' for '+handlerName);
    var mdl=pas[UnitName];
    if ((mdl!=null)&&(mdl!=undefined)) {
        fn = mdl[ handlerName];
        if (fn!=null) {
          if (DoBind) {
          fn = fn.bind(mdl); }    // so that the 'this' context will be preserved
        }
      }
    } }

    // FOURTH...
    if ((fn==null)&&(AllowUserEvents==true)) {
    // if we are running with Python events, look for the generic handler
    // registered with the main form...
      if (Language == 'Python') {
        fn = pas.NodeUtils.MainForm.myNode.FindRegisteredEvent('PythonEvent');
        }
    }


//    // FIFTH ......
//    if ((fn==null)&&(AllowUserEvents==true)) {
//    // the event we seek may be a thread event, for a TXThreads component.
//    // These events are compiled into a separate unit (MainUnitName+'EventsThreads')
//
//      //alert('FindEventFunction looking in thread events unit '+UnitName+'Threads for '+handlerName);
//      var mdl=pas[UnitName+'Threads'];
//      if ((mdl!=null)&&(mdl!=undefined)) {
//        //alert('found module '+UnitName);
//          fn = mdl[ handlerName];
//          if (fn!=null) {
//            //alert('found function '+handlerName);
//            if (DoBind) {
//            fn = fn.bind(mdl); }    // so that the 'this' context will be preserved
//          }
//        }
 //     }
    if (fn==undefined) {fn=null;}

}catch(err) { alert(err.message+'  in Events.FindEventFunction '+myName+' '+EventType);}
end;
result:=fn;
end;

function RunComponentEvent(e:TEventStatus;myName,NameSpace,EventType:string;MyNode:TDataNode;MyValue:string):Boolean;
var
  UnitName:String;
  fn:TObject;
begin
  glbEvent:=e;
  UnitName:=MainUnitname+'Events';
  fn:=FindEventFunction(NameSpace,myName,EventType,MyNode,true);

  asm
  //console.log('RunComponentEvent '+EventType+' '+myName+' '+MyNode.NodeName);
  try {
    // Execute the function, if found....
    if (fn!=null)  {
      fn(e,myName,MyValue);
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
         // nodeid and namespace provide the original 'event node' for use in the trapper function
         newe.eventValue:=myValue;
         newe.NameSpace:=e.NameSpace;
         RunComponentEvent(newe,trappers[i].NodeName,trappers[i].NameSpace,trappers[i].MyEventTypes[j],trappers[i],'');
         e.ContinueAfterTrappers := newe.ContinueAfterTrappers;
     end;
   end;

end;

procedure RunCompositeEvent(e:TEventStatus;myName,NameSpace,EventType:string;MyNode:TDataNode;MyValue:string);
var
  roi:Boolean;
  nodes:TNodesArray;
  i:integer;
  newe:TEventStatus;
begin
  roi:=myNode.GetEventROI(EventType);
  if roi=false then
    // run event as normal
    RunComponentEvent(e,myName,e.NameSpace,EventType,myNode,myValue)
  else
  begin
    // find the underlying event within the composite element
    nodes:=FindNodesOfType(myNode,'TXCompositeIntf',true,myNode.NodeName);
    i:=0;
    while i < length(nodes) do
    begin
      if nodes[i].HasUserEventCode(EventType) then
      begin
        newe:=TEventStatus.Create(EventType,nodes[i].NodeName);
        newe.eventValue:=myValue;
        newe.NameSpace:=nodes[i].NameSpace;
        RunComponentEvent(newe,nodes[i].NodeName,nodes[i].NameSpace,EventType,nodes[i],myValue);
        i:=length(nodes);
      end;
      i:=i+1;
    end;
  end;
end;

procedure RunInterfaceEvent(e:TEventStatus;myName,NameSpace,EventType:string;MyNode:TDataNode;MyValue:string);
var
  roi:Boolean;
  newe:TEventStatus;
  CompNode:TDataNode;
begin
  roi:=myNode.GetEventROI(EventType);
  if roi=true then
    // run event as normal
    RunComponentEvent(e,myName,e.NameSpace,EventType,myNode,myValue)
  else
  begin
    // find the containing composite element
    CompNode:=FindDataNodeByID(SystemNodeTree,Namespace,'',true);
    if CompNode<>nil then
    begin
      if CompNode.HasUserEventCode(EventType) then
      begin
        newe:=TEventStatus.Create(EventType,CompNode.NodeName);
        newe.eventValue:=myValue;
        newe.NameSpace:=CompNode.NameSpace;
        RunComponentEvent(newe,CompNode.NodeName,CompNode.NameSpace,EventType,CompNode,myValue);
      end;
    end;
  end;
end;

function FindAndRunEventHandler(e:TEventStatus;MyEventType,nodeID,myValue:string;myNode:TDataNode):Boolean;
  var
     i,NumHandlers:integer;
     found:Boolean;
  begin
    found:=true;
    //ShowMessage('FindAndRunEventHandler. '+MyEventType+' NodeId='+nodeID+' value='+myValue);
    NumHandlers:= myNode.MyEventTypes.count;
    for i:=0 to NumHandlers - 1 do
    begin
      if  myNode.MyEventTypes[i]=MyEventType then
      begin
        // Execute the registered event handler if it exists
        if (myNode.NodeType<>'TXComposite')
        and (myNode.NodeType<>'TXCompositeIntf') then
        begin
          found:=RunComponentEvent(e,nodeID,e.NameSpace,MyEventType,myNode,myValue);
        end
        else if (myNode.NodeType='TXComposite') then
        begin
          // for a composite, if the event is 'ReadOnlyInterface', then execute the corresponding
          // event on a TXCompositeIntf node, within the composite element instead.
          RunCompositeEvent(e,nodeID,e.NameSpace,MyEventType,myNode,myValue);
        end
        else if (myNode.NodeType='TXCompositeIntf') then
        begin
          // for a composite interface, if the event is NOT 'ReadOnlyInterface', then execute the corresponding
          // event on the containing TXComposite node instead.
          RunInterfaceEvent(e,nodeID,e.NameSpace,MyEventType,myNode,myValue);
        end;
      end;
    end;
    result:=found;
end;

function  handleEvent(e:TEventStatus;MyEventType,nodeID,NameSpace,myValue,PropName:string):Boolean;
var
  CurrentNode :TDataNode;
  m: TMethod;
  {$ifndef JScript}
  sc:TCursor;
  {$endif}
  found:Boolean;
begin
  found:=true;
  if (StartingUp = false)
  and (not SuppressEvents) //!! check this !!!!
  then
  begin

    if e=nil then
    begin
      e:=TEventStatus.Create(MyEventType,nodeID);
      e.NameSpace:=NameSpace;
    end;

     // Identify the system node.
     // nodeID is the screen object name (nodename in the TDataNode tree)
     //   **** exception - HTML TreeNode events send in the name of the treenode object
     {$ifndef JScript}
     sc:=Screen.Cursor;
     Screen.Cursor := crHourglass;
     CurrentNode:=FindDataNodeById(SystemNodeTree,nodeID,NameSpace,false);
     {$else}
     if (MyEventType = 'TreeNodeClick')
     or (MyEventType = 'DragStart')
     or (MyEventType = 'Drop')
     then
     begin
       // try to get the system node name from the treenode name...
       CurrentNode:=GetDataNodeFromTreeNode(nodeID,NameSpace);
     end
     else
       //asm console.log('handleEvent '+MyEventType+' '+nodeID+' '+NameSpace); end;
       CurrentNode:=FindDataNodeById(SystemNodeTree,nodeID,NameSpace,false);
     {$endif}

    if (CurrentNode<>nil)
    and (MainForm<>nil) then
    begin

        // Look for XTrapEvents components.  Each may have a 'HandleAny' process defined.
         if e.InitRunning=false then
           // don't re-trap any event that is entering the second phase (after async function)
           ExecuteEventTrappers(e,MyEventType,CurrentNode.nodeName,myValue,CurrentNode)
         else
           e.ContinueAfterTrappers:=true;

         if e.ContinueAfterTrappers then
         begin
            // run the specific event handler defined in the form for this component and event type (if any)
            //showmessage('calling FindAndRunEventHandler');
            found:=FindAndRunEventHandler(e,MyEventType,CurrentNode.nodeName,myValue,CurrentNode) ;
         end;

     end;

     {$ifndef JScript}
     Screen.Cursor:=sc;
     {$endif}
   end;
  result:=found;
end;

function  handleEvent(e:TEventStatus;MyEventType,nodeID,NameSpace,myValue:string):Boolean;
begin
  result:=handleEvent(e,MyEventType,nodeID,NameSpace,myValue,'');
end;
function  handleEvent(MyEventType,nodeID,NameSpace,myValue:string):Boolean;
begin
  result:=handleEvent(nil,MyEventType,nodeID,NameSpace,myValue,'');
end;


////// procedure called from generated events unit, to execute user's event handlers and to
////// control async functions with the init/main code sections in the event
// initfunc and mainfunc point to Pascal functions to be executed.
// If events are in Python mode, initfunc and mainfunc are null, with the Python scripts in InitPyCode,MainPyCode
procedure RunTheEventFunc(e:TEventStatus;NodeId: String; myValue: String; fn:THandler;PyCode:String);
var
  bits:TStringList;
  str:string;
  i:integer;
begin
  if fn<>nil then  // (Pascal) execute a function in the dll
    fn(e,NodeId,myValue)
  else
  begin  // run python script
    bits:=TStringlist.Create;
    bits.text:=PyCode;
    glbEvent:=e;
{$ifdef JScript}
    // refresh the global 'e' object in the Python (Pyodide) environment
    bits.insert(0,'e = pas.EventsInterface.glbEvent');
    //bits.insert(1,'print("python e.ReturnString:",e.ReturnString)');
    //bits.insert(1,'print("python e:",e)');
{$else}
    PassEToPythonFunc(e);
    bits.Insert(0,'e = PyInterfaceE.Value');
{$endif}
    RunPyScriptFunc(bits,NodeId+' '+e.EventType);
    bits.Free;
{$ifndef JScript}
    if e<>nil then
      e.ReturnString:=GetEFromPythonFunc();
{$endif}
  end;
end;

procedure ExecuteEventHandler(e:TEventStatus;NodeId: String; myValue: String; initfunc,mainfunc:THandler;InitPyCode,MainPyCode:String);
var
  asyncWaiting:boolean;
begin
  asyncWaiting := false;
  if (e<>nil) then
    asyncWaiting := e.EventHasWaitingAsyncProcs;
  {$ifndef JScript}
  EventsNameSpace:=PChar(e.NameSpace);
  {$else}
  //asm console.log('ExecuteEventHandler code:'+InitPyCode+MainPyCode); end;
  EventsNameSpace:=e.NameSpace;
  {$endif}
  if ((e=nil) or (e.InitDone=false))
  and (not asyncWaiting) then
  begin
    if (e=nil) then
    begin
      e:=TEventStatus.Create(e.eventType,NodeId);
      e.NameSpace:='';
    end;
    e.InitRunning:=true;
    e.InitDone:=true;
    RunTheEventFunc(e,NodeId,myValue,initfunc,InitPyCode);
    e.InitRunning:=false;
  end;
  if e.AsyncProcsRunning.Count = 1 then
    e.ClearAsync('ShowBusy');
  if e.EventHasWaitingAsyncProcs = false then
  begin
    {$ifndef JScript}
    RunTheEventFunc(e,NodeId,myValue,mainfunc,MainPyCode);
    {$else}
    if (e.eventType = 'DropAccepted')
    or (e.eventType = 'DragStart') then
      RunTheEventFunc(e,NodeId,myValue,mainfunc,MainPyCode)    // must do these synchronously
    else
    begin
      /// timeout/job-queue so that any changes made in the 'init' secton will be refreshed on screen
      if mainfunc<>nil then
      begin
        asm
        console.log('timeout for main (pascal events)...');
        myTimeout(mainfunc,5,'Event Main',0,e,NodeId,myValue,MainPyCode);
        end;
      end
      else
      begin
        asm
        console.log('timeout for main (python events)...');
        myTimeout(pas.Events.RunTheEventFunc,5,'Event Main',0,e,NodeId,myValue,null,MainPyCode);
        end;
      end;
    end;
    {$endif}
  end;
end;


//======================================================================================================
//============================================  End of Common Events Code ================================
//======================================================================================================
Begin
  ExecuteEventHandlerFunc:=@ExecuteEventHandler;
 {$ifndef JScript}
  EventCode := TEventClass.Create;
{$endif}

end.
