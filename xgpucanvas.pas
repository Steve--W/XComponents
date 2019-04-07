(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XGPUCanvas;

{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
    Classes, SysUtils, TypInfo, StringUtils, NodeUtils, XIFrame, Math,
    UtilsJSCompile, XForm, XCode, XButton, XVBox, XTabControl, XMemo, EventsInterface,
  {$ifndef JScript}
    LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, Propedits, RTTICtrls,
    LazsUtils, LCLIntf,
    FileUtil,  LCLType, gettext,
    {$ifdef Chromium}
    uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFConstants, uCEFTypes, uCEFChromiumEvents, uCEFRenderProcessHandler,
    uCEFWinControl, uCEFProcessMessage, uCEFApplication,
    {$endif}
  {$else}
    webfilecache, pas2jswebcompiler,
    HTMLUtils,
  {$endif}
    WrapperPanel, Events;

{$ifndef JScript}
procedure Register;
{$endif}

//{$ifdef JScript}
//type TGPUMessage = record
//  objid:String;
//  mtype:String;
//  pName:String;
//  pValue:TNumArray;
//end;
//procedure HandleGPUMessage(gpumsg:TGPUMessage);
//{$endif}

type TGPUNumParam = record
  ParamName:String;
  ParamValue:TNumArray;
  end;
type TGPUNumParams = Array of TGPUNumParam;
type TGPUImgParam = record
  ParamName:String;
  ParamValue:TImgArray;
  end;
type TGPUImgParams = Array of TGPUImgParam;
type TGPUIntConst = record
  ConstName:String;
  ConstValue:integer;
  end;
type TGPUIntConsts = Array of TGPUIntConst;

type
  TXGPUCanvas = class(TXIFrame)
  private
    { Private declarations }
    ParamNumArray:TGPUNumParams;
    ParamImgArray:TGPUImgParams;
    ConstIntArray:TGPUIntConsts;
    {$ifndef JScript}
    fHandleOnNewFrame:TEventHandler;
    {$endif}

    function GetAnimationCode:string;
    function GetActive:Boolean;
    function GetAnimated:Boolean;
    function GetParamNumList:string;
    function GetConstIntList:string;
    function GetParamImgList:string;
    function GetMaxIterations:integer;
    function GetStartIteration:integer;
    function GetNumFrames:integer;
    function GetMaxFramesPerSec:integer;

    procedure SetAnimationCode(AValue:string);
    procedure SetActive(AValue:Boolean);
    procedure SetAnimated(AValue:Boolean);
    procedure SetParamNumList(AValue:string);
    procedure SetConstIntList(AValue:string);
    procedure SetParamImgList(AValue:string);
    procedure SetMaxIterations(AValue:integer);
    procedure SetStartIteration(AValue:integer);
    procedure SetNumFrames(AValue:integer);
    procedure SetMaxFramesPerSec(AValue:integer);

    procedure SetMyEventTypes;  override;
    procedure SetPropertyDefaults;
    procedure StartMyGPU;
    procedure StopMyGPU;
    function BuildPascalAnimationUnit(Compiler:TObject):String;
    function CompileGPUToJS(var GPUJSOutput:String):Boolean;
    function GPUJSHeader:String;
    function GPUJSFooter:String;
    function GPUJSAnimationFooter:String;
    procedure setupGPUPage;

    {$ifndef JScript}
    procedure DoGPUCanvasConstructor;
    procedure EditorResize(Sender: TObject);
    {$endif}
//    function SetParamsFromMessage(msg:String):TNumArray;

  protected
    { Protected declarations }
  public
    { Public declarations }
    GeneratedHTML:String;
    {$ifndef JScript}
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent;IsDynamic:Boolean); override;
    destructor Destroy; override;
    procedure ReLoadURL; override;
    {$else}
    constructor Create(MyForm:TForm;NodeName:String);  override;
    {$endif}
    function FullParamList:String;
    function FullXMLString:String;
    function GetParamNumValue(pName:String):TNumArray;
    function GetConstIntValue(pName:String):integer;
    function GetParamImgValue(pName:String):TImgArray;
    procedure SetParamNumValue(pName:String;pValue:TNumArray;ForwardToWidget:Boolean);
    procedure SetConstIntValue(pName:String;pValue:integer);
    procedure SetParamImgValue(pName:String;pValue:TImgArray;ForwardToWidget:Boolean);
//    function ParamNumArrayToString:String;

  //property myNode;
published
    { Published declarations }

    // Properties defined for this class...
    property Active: Boolean read GetActive write SetActive;
    property Animated: Boolean read GetAnimated write SetAnimated;
    property AnimationCode: String read GetAnimationCode write SetAnimationCode;
    property ParamNumList: String read GetParamNumList write SetParamNumList;
    property ConstIntList: String read GetConstIntList write SetConstIntList;
    property ParamImgList: String read GetParamImgList write SetParamImgList;
    property MaxIterations: integer read GetMaxIterations write SetMaxIterations;
    property StartIteration: integer read GetStartIteration write SetStartIteration;
    property NumFrames: integer read GetNumFrames write SetNumFrames;
    property MaxFramesPerSec: integer read GetMaxFramesPerSec write SetMaxFramesPerSec;

    {$ifndef JScript}
    // Events to be visible in Lazarus IDE
    property HandleOnNewFrame: TEventHandler read FHandleOnNewFrame write FHandleOnNewFrame;
    {$endif}
  end;

type  TGPUEventClass = class
    procedure CloseCodeEditor(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure GPUCodeEditHandleClickMessage(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure LaunchGPUHTML(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  end;

//{$ifndef JScript}
//const
//  XGPUCANVAS_GETPARAMS = 'getgpuparams';
//  XGPUCANVAS_SEND_PARAMS  = 'sendgpuparams';
//{$endif}

var
  GPUEvents:TGPUEventClass;
  GPUEditorForm:TXForm;
  GPUEditorTabControl:TXTabControl;
  GPUCodeEditor:TXCode;
  GPUMemo:TXMemo;
  EditingGPUNode:TDataNode;
  GPUEditorMode:String;


implementation

const MyNodeType='TXGPUCanvas';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXGPUCanvas.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
  MyEventTypes.Add('OnStart');
 // MyEventTypes.Add('OnNewFrame');
end;

{$ifndef JScript}
//{$ifdef Chromium}
//procedure TXGPUCanvas.ChromiumProcessMessageReceived(
//  Sender: TObject; const Browser: ICefBrowser;
//  sourceProcess: TCefProcessId;
//  const message: ICefProcessMessage; out Result: Boolean);
//var
//  ParamText:String;
//begin
//  case message.Name of
//    XGPUCANVAS_SEND_PARAMS:
//    begin
//      ParamText := message.ArgumentList.GetString(0);
//      SetParamsFromMessage(ParamText);
//    end
//  else
//    inherited;
//  end;
//end;
//
//{$endif}
procedure TXGPUCanvas.EditorResize(Sender: TObject);
begin
  CheckPercentageSizing(GPUCodeEditor);
end;

{$endif}


procedure TXGPUCanvas.SetPropertyDefaults;
var
  FormNode,TabControlNode,TabPageNode1,TabPageNode2,EditorNode,VBNode,BtnNode:TDataNode;
  MemoNode,VBNode2,BtnNode2:TDataNode;
  DoneBtn,LaunchBtn:TXButton;
  VB,vb2:TXVBox;
  tmp:String;
begin
  {$ifndef JScript}
  if not (csDesigning in ComponentState) then
  {$endif}
  begin
  // Create the popup form for editing the GPU animation code block...
  if GPUEditorForm=nil then
  begin
    {$ifndef JScript}
    GPUEditorForm:=TXForm.CreateNew(MainForm);
    GPUEditorForm.Name:='XGPUCodeEditorForm';
    GPUEditorForm.OnResize:=@EditorResize;
    FormNode:=CreateFormNode(GPUEditorForm);
    {$else}
    FormNode:=AddDynamicWidget('TXForm',nil,nil,'XGPUCodeEditorForm','Left',-1);
    GPUEditorForm:=TXForm(FormNode);
    GPUEditorForm.Name:='XGPUCodeEditorForm';
    {$endif}
    GPUEditorForm.Caption:='XGPUCanvas Animation Code Editor';
    GPUEditorForm.Top:=100;
    GPUEditorForm.Left:=100;
    GPUEditorForm.Height:=500;
    GPUEditorForm.Width:=800;
    FormNode.IsDynamic:=false;

    addchildtoparentnode(SystemNodeTree,FormNode,-1);   //!!!! check this doesn't upset things...don't want these in a systemsave...

    TabControlNode:=AddDynamicWidget('TXTabControl',GPUEditorForm,FormNode,'XGPUTabControl','Left',-1);
    TabControlNode.IsDynamic:=false;
    GPUEditorTabControl:=TXTabControl(TabControlNode.ScreenObject);
    GPUEditorTabControl.ContainerHeight:='100%';
    GPUEditorTabControl.ContainerWidth:='100%';

    TabPageNode1:=AddDynamicWidget('TXTabSheet',GPUEditorForm,TabControlNode,'XGPUTabSheet1','Left',-1);
    TabPageNode2:=AddDynamicWidget('TXTabSheet',GPUEditorForm,TabControlNode,'XGPUTabSheet2','Left',-1);
    TabPageNode1.IsDynamic:=false;
    TabPageNode2.IsDynamic:=false;
    TXTabSheet(TabPageNode1.ScreenObject).Caption:='Frame Animation Code';
    TXTabSheet(TabPageNode2.ScreenObject).Caption:='Generated HTML';
    TXTabControl(TabControlNode.ScreenObject).TabIndex:=0;

    VBNode:=AddDynamicWidget('TXVBox',GPUEditorForm,TabPageNode1,'XGPUVBox','Left',-1);
    VB:=TXVBox(VBNode.ScreenObject);
    VB.ContainerHeight:='100%';
    VB.Border:=false;
    VBNode.IsDynamic:=false;

    EditorNode:=AddDynamicWidget('TXCode',GPUEditorForm,VBNode,'XGPUCodeEditor','Left',-1);
    GPUCodeEditor:=TXCode(EditorNode.ScreenObject);
    GPUCodeEditor.ContainerHeight:='90%';
    GPUCodeEditor.ContainerWidth:='100%';
    GPUCodeEditor.MessagesHeight:='30%';
    GPUCodeEditor.LabelText:='';
    GPUCodeEditor.myNode.registerEvent('ClickMessage',@GPUEvents.GPUCodeEditHandleClickMessage);
    EditorNode.IsDynamic:=false;

    BtnNode:=AddDynamicWidget('TXButton',GPUEditorForm,VBNode,'XGPUDoneBtn','Left',-1);
    DoneBtn:=TXButton(BtnNode.ScreenObject);
    DoneBtn.Caption:='Done';
    DoneBtn.myNode.registerEvent('ButtonClick',@GPUEvents.CloseCodeEditor);
    BtnNode.IsDynamic:=false;

    VBNode2:=AddDynamicWidget('TXVBox',GPUEditorForm,TabPageNode2,'XGPUVBox2','Left',-1);
    VB2:=TXVBox(VBNode2.ScreenObject);
    VB2.ContainerHeight:='100%';
    VB2.Border:=false;
    VBNode2.IsDynamic:=false;

    MemoNode:=AddDynamicWidget('TXMemo',GPUEditorForm,VBNode2,'XGPUHTMLMemo','Left',-1);
    GPUMemo:=TXMemo(MemoNode.ScreenObject);
    GPUMemo.MemoHeight:='85%';
    GPUMemo.MemoWidth:='100%';
    GPUMemo.LabelPos:='Top';
    GPUMemo.LabelText:='HTML generated at the last GPU activation';
    MemoNode.IsDynamic:=false;

    BtnNode2:=AddDynamicWidget('TXButton',GPUEditorForm,VBNode2,'XGPULaunchBtn','Left',-1);
    LaunchBtn:=TXButton(BtnNode2.ScreenObject);
    LaunchBtn.Caption:='Launch HTML in browser';
    LaunchBtn.myNode.registerEvent('ButtonClick',@GPUEvents.LaunchGPUHTML);
    LaunchBtn.Hint:='Launch the generated GPU HTML in a separate browser page to aid diagnostics';
    BtnNode2.IsDynamic:=false;
  end;

  end;
end;

//function TXGPUCanvas.ParamNumArrayToString:String;    // (used for debugging)
//var
//  i,j:integer;
//  str:String;
//begin
//  str:='[';
//  for i:=0 to length(ParamNumArray)-1 do
//  begin
//    if i>0 then str:=str+',';
//    str:=str+ParamNumArray[i].ParamName+':[';
//    for j:=0 to length(ParamNumArray[i].ParamValue)-1 do
//    begin
//      if j>0 then str:=str+',';
//      str:=str+FloatToStr(ParamNumArray[i].ParamValue[j]);
//    end;
//    str:=str+']';
//  end;
//  str:=str+']';
//  result:=str;
//end;

{$ifndef JScript}
procedure Register;
begin
  {$I Icons/XGPUCanvas.lrs}
  {$ifdef Chromium}
  RegisterComponents('XComponents',[TXGPUCanvas]);
  {$endif}
end;


function CreateGPUCanvasWidget(ParentNode:TDataNode;ScreenObjectName:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXGPUCanvas',ParentNode.MyForm,ParentNode,ScreenObjectName,Alignment,position);
  result:=NewNode;
end;

procedure TXGPUCanvas.DoGPUCanvasConstructor;
begin
  self.IsContainer:=false;
  self.myNode.NodeType:='TXGPUCanvas';
  SetLength(ParamNumArray,0);
  AddDefaultAttribs(self,self.myNode,mydefaultAttribs);

  SetPropertyDefaults;
end;

constructor TXGPUCanvas.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoGPUCanvasConstructor;
end;

constructor TXGPUCanvas.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoGPUCanvasConstructor;
end;

destructor TXGPUCanvas.Destroy;
begin
  if (not (csDesigning in componentState)) then
  begin
    if Active then
    begin
      self.StopMyGPU;
    end;
    myControl.Free;
  end;
  inherited Destroy;
end;

procedure TXGPUCanvas.ReLoadURL;
begin
  if (self.SuspendRefresh)
  {$ifndef JScript}
  or (GlobalSuppressFrameDisplay)
  {$endif}
  then
    EXIT;

  if (StartingUp=false)
  and (self.Active) then
    self.StartMyGPU;
end;


{$else} //JScript

constructor TXGPUCanvas.Create(MyForm:TForm;NodeName:String);
begin
  inherited Create(MyForm,NodeName);
  self.NodeType:='TXGPUCanvas';
  self.IsContainer:=false;
  SetLength(ParamNumArray,0);

  SetNodePropDefaults(self,myDefaultAttribs);
  SetPropertyDefaults;
end;

function CreateGPUCanvasWidget(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer;Alignment:String):TDataNode;
var
  NewWidget:TXGPUCanvas;
  h,w:integer;
  scr:String;
begin
  DoCreateFrameWidget(MyNode, ParentNode,ScreenObjectName,position);
  NewWidget:=TXGPUCanvas(myNode);

  RefreshComponentProps(myNode);

  // refresh the actual h/w attributes
  h:=NewWidget.ActualHeight;
  w:=NewWidget.ActualWidth;

  result:=myNode;
end;


function CreateinterfaceObjGPU(MyForm:TForm;NodeName:String):TObject;
begin
  result:=TObject(TXGPUCanvas.Create(MyForm,NodeName));
end;

//procedure HandleGPUMessage(gpumsg:TGPUMessage);
//var
//  ItemNode:TdataNode;
//begin
//  if (gpumsg.objid<>'') then
//  begin
//    //showmessage('HandleMessage XGPUCanvas: '+gpumsg.objid+' '+gpumsg.mtype+' '+NumArrayToJSONString(gpumsg.pvalue));
//     //this is a notification sent out from within a GPU frame.
//     ItemNode:=findDataNodeById(systemnodetree,gpumsg.objid,false);
//     if ItemNode<>nil then
//     begin
//       // set the ParamArray values from the message
//       TXGPUCanvas(ItemNode).SetParamNumValue(gpumsg.pName, gpumsg.pValue,false);     //!!!! + int + img
////      showmessage(TXGPUCanvas(ItemNode).ParamNumArrayToString);
//       handleEvent('OnNewFrame',ItemNode.NodeName, '');
//     end;
//  end;
//
//end;

{$endif}
(*
function TXGPUCanvas.SetParamsFromMessage(msg:String):TNumArray;
// msg has format 'params =[[n,n,n],[n,n,n]....]
//                'ints =[[i,i,i],[i,i,i]....]
//                'strs =[[s,s,s],[s,s,s]....]'
var
  numstr,intstr,imgstr, str, lastbit:string;
  n,i,s,j,k:integer;
  sets:TStringList;
  bits:TStringList;
  bits2:TStringList;
begin
  str:=msg;
  n:=FoundString(msg,'nums =');
  i:=FoundString(msg,'ints =');
  s:=FoundString(msg,'strs =');
  if (n>0)
  and (i>0)
  and (s>0) then
  begin
    imgstr:=str;
    Delete(imgstr,1,s+5);

    Delete(str,s,length(str)-s);
    intstr:=str;
    Delete(intstr,1,i+5);

    Delete(str,i,length(str)-i);
    numstr:=str;
    Delete(numstr,1,i+5);

    i:=FoundString(numstr,'[');
    if i>0 then
    begin
      Delete(numstr,1,i+1);             //  'n,n,n],[n,n,n]....]'
      if numstr<>'' then
      begin
        bits:=stringsplit(numstr,'],[');  //  'n,n,n /   n,n,n  / ....   ]]'
        lastbit:=bits[bits.count-1];
        if length(lastbit)>1 then
        begin
          Delete(lastbit,length(lastbit)-1,2);
          bits[bits.count-1]:=lastbit;
          for j:=0 to bits.count-1 do
          begin
            bits2:=stringsplit(bits[j],',');
            for k:=0 to bits2.count-1 do
              self.ParamNumArray[j].ParamValue[k]:=StrToFloat(bits2[k]);
          end;
        end;
      end;
    end;

    i:=FoundString(imgstr,'[');
    if i>0 then
    begin
      Delete(imgstr,1,i+1);             //  'i,i,i],[i,i,i]....]'
      if imgstr<>'' then
      begin
        bits:=stringsplit(imgstr,'],[');  //  'i,i,i /   i,i,i  / ....   ]]'
        lastbit:=bits[bits.count-1];
        if length(lastbit)>1 then
        begin
          Delete(lastbit,length(lastbit)-1,2);
          bits[bits.count-1]:=lastbit;
          for j:=0 to bits.count-1 do
          begin
            bits2:=stringsplit(bits[j],',');
            for k:=0 to bits2.count-1 do
              self.ParamImgArray[j].ParamValue[k]:='';  //!!!! ??
          end;
        end;
      end;
    end;



  end;

end;
*)

function TXGPUCanvas.GetAnimationCode:string;
begin
  result:=myNode.getAttribute('AnimationCode',true).AttribValue;
end;
function TXGPUCanvas.GetActive:Boolean;
begin
  result:=myStrToBool(myNode.getAttribute('Active',true).AttribValue);
end;
function TXGPUCanvas.GetAnimated:Boolean;
begin
  result:=myStrToBool(myNode.getAttribute('Animated',true).AttribValue);
end;
function TXGPUCanvas.GetParamNumList:string;
begin
  result:=myNode.getAttribute('ParamNumList',true).AttribValue;
end;
function TXGPUCanvas.GetConstIntList:string;
begin
  result:=myNode.getAttribute('ConstIntList',true).AttribValue;
end;
function TXGPUCanvas.GetParamImgList:string;
begin
  result:=myNode.getAttribute('ParamImgList',true).AttribValue;
end;
function TXGPUCanvas.GetMaxIterations:integer;
begin
  result:=StrToInt(myNode.getAttribute('MaxIterations',true).AttribValue);
end;
function TXGPUCanvas.GetStartIteration:integer;
begin
  result:=StrToInt(myNode.getAttribute('StartIteration',true).AttribValue);
end;
function TXGPUCanvas.GetNumFrames:integer;
begin
  result:=StrToInt(myNode.getAttribute('NumFrames',true).AttribValue);
end;
function TXGPUCanvas.GetMaxFramesPerSec:integer;
begin
  result:=StrToInt(myNode.getAttribute('MaxFramesPerSec',true).AttribValue);
end;

function TXGPUCanvas.FullParamList:String;
var
  plist:String;
begin
  plist:='';
  if self.ParamNumList<>'' then plist:=','+self.ParamNumList;
 // if self.ParamIntList<>'' then plist:=plist+','+self.ParamIntList;
  if self.ParamImgList<>'' then plist:=plist+','+self.ParamImgList;
  result:=plist;
end;

function TXGPUCanvas.GPUJSHeader:String;
var
  str,vstr,cma, plist:String;
  i,j:integer;
  vn:TNumArray;
  vi:TIntArray;
  vb:TImgArray;
begin
  str:=
//  'document.domain = "/abc"; ' + LineEnding
  'document.title = "'+myNode.NodeName+' '+myNode.NodeType+'"; ' + LineEnding
  +'/*/ ------------------------------------ Initialise the GPU ---------------------------------/*/ ' + LineEnding
  +'     const '+self.MyNode.NodeName+' = new GPU({mode: ''webgl''});   '+LineEnding;
 // +'     const '+self.MyNode.NodeName+' = new GPU({mode: ''cpu''});   '+LineEnding;

  str:= str + '/*/ -------------------------------- Initialise Parameters List -------------------------/*/ ' + LineEnding;
  for i:=0 to length(self.ParamNumArray)-1 do
  begin
    vn:=ParamNumArray[i].ParamValue;
    vstr:='[';
    for j:=0 to length(vn)-1 do
    begin
      if j>0 then vstr:=vstr+',';
      vstr:=vstr+floattostr(vn[j]);
    end;
    vstr:=vstr+']';
    str:=str+'   var '+ParamNumArray[i].ParamName+' = '+vstr+';' +LineEnding;
  end;
  for i:=0 to length(self.ParamImgArray)-1 do
  begin
    vb:=ParamImgArray[i].ParamValue;
    vstr:='[';
    for j:=0 to length(vb)-1 do
    begin
      if j>0 then vstr:=vstr+',';
      vstr:=vstr+vb[j];
    end;
    vstr:=vstr+']';
    str:=str+'   var '+ParamImgArray[i].ParamName+' = '+vstr+';' +LineEnding;
  end;

  str:=str
  +'  ParamsAsString=function() { '+LineEnding
  // numeric parameters
  +'     var s="nums =["; '+LineEnding;
  cma:='""';
  for i:=0 to length(self.ParamNumArray)-1 do
  begin
    str:=str+'s = s+'+cma+'+"["+'+ParamNumArray[i].ParamName+'.toString()+"]";'+LineEnding ;       //eg. s = s+","+"["+p1.toString()+"]";
    cma:='","';
  end;
  str:=str+'    s=s+"]";'+LineEnding
  // image parameters
  +'     s=s+" imgs =["; '+LineEnding;
  cma:='""';
  for i:=0 to length(self.ParamImgArray)-1 do
  begin
    str:=str+'    s = s+'+cma+'+"["+'+ParamImgArray[i].ParamName+'.toString()+"]";'+LineEnding ;       //eg. s = s+","+"["+p1.toString()+"]";
    cma:='","';
  end;
  str:=str+'    s=s+"]";'+LineEnding;

  str:=str+'    return(s);'+LineEnding;
  str:=str+'}'+LineEnding;


//  {$ifdef JScript}
//  str:=str
//  +'  function PostParamMessages(objid) {'  + LineEnding
//  +'     var pv=[0]; '  + LineEnding;
//  for i:=0 to length(self.ParamNumArray)-1 do
//  begin
//  str:=str
//  +'          pv='+ParamNumArray[i].ParamName+';'  + LineEnding
////   +'          alert("posting msg. pvalue="+pv);'  + LineEnding
//  +'          parent.postMessage({"objid":objid,"mtype":"FrameDone","pName":"'+ParamNumArray[i].ParamName+'","pValue":pv},"*"); ' + LineEnding;
//  end;
//  //!!!! Add integers and images.....
//  str:=str+'}'+LineEnding;
//  {$endif}

  plist:=self.FullParamList;
  str:=str
 +'     /*/------------ start of create Kernel routine -------/*/ ' + LineEnding
 +'     var '+self.MyNode.NodeName+'CanvasRenderFn = '+self.MyNode.NodeName+'.createKernel(function(AnimationCounterValue'+plist
 +') { ' + LineEnding
 +'       var r = 0  ;      ' + LineEnding
 +'       var g = 0  ; /*/--initalise the default colour for the GPUCanvas pixel in r,g,b,a format --/*/  ' + LineEnding
 +'       var b = 0  ;   ' + LineEnding
 +'       var a = 1  ;  ' + LineEnding;
  result:=str;
end;
function TXGPUCanvas.GPUJSFooter:String;
var
  h,w,i:integer;
  cma,str,plist:String;
begin
  {$ifndef JScript}
  h:=trunc(self.Height * 0.8);
  w:=trunc(self.Width * 0.95);
  {$else}
  asm
    var ob=document.getElementById(this.NodeName);
    if (ob!=null) {
      //alert('found ob for h/w calc');
      var style = window.getComputedStyle(ob);
      var hh = style.height;
      h = parseInt(hh, 10);
      h = Math.trunc( h*0.85 );
      var ww = style.width;
      w = parseInt(ww, 10);
      w = Math.trunc( w*0.9 );
    }
    else {h=301; w=301;}
    if (w==undefined) {w=302;}
    if (h==undefined) {h=302;}
    //alert('w='+w+' h='+h);
  end;
  {$endif}

  plist:=self.FullParamList;
  str:=    '         /*/-------- Standard Javascript to place the GPU Canvas on the web page------------------/*/     ' + LineEnding
  +'             this.color((r),(g),(b),(a));        ' + LineEnding
  +'     })  '+LineEnding
  +'  /*/------- end of create Kernel routine ---------------/*/ ' + LineEnding
  +'  ' + LineEnding
  +'       .setOutput(['+intToStr(w)+','+ intToStr(h)+'])              ' + LineEnding
  +'       .setLoopMaxIterations(['+IntToStr(self.MaxIterations)+'])   ' + LineEnding
  +'       .setGraphical(true)                                         ' + LineEnding;

  // integer parameters are loaded as constants
  for i:=0 to length(self.ConstIntArray)-1 do
  begin
    str:=str
    +'       .setConstants({'+self.ConstIntArray[i].ConstName+': '+inttostr(self.ConstIntArray[i].ConstValue)+'}); ' + LineEnding;
  end;

  str:=str
  +'     /*/-------------------Run the Graphics code and place it on the web page----------------------/*/    ' + LineEnding
  +'     var AnimationCounterValue='+IntToStr(self.StartIteration)+'; '                                         +LineEnding
  +'     var AnimationCounterMax='+IntToStr(self.MaxIterations)+'; '                                         +LineEnding
  +'     '+self.MyNode.NodeName+'CanvasRenderFn(AnimationCounterValue'+plist+');               ' + LineEnding
  +'     var '+self.MyNode.NodeName+'BrowserCanvas = '+self.MyNode.NodeName+'CanvasRenderFn.getCanvas();                    ' + LineEnding
  +'     document.getElementsByTagName("body")[0].appendChild('+self.MyNode.NodeName+'BrowserCanvas);                ' + LineEnding ;

  str:=str
  +'  function GetMessage(msg) {'  + LineEnding
  +'    alert("msg="+msg);  '  + LineEnding
  +'}'  + LineEnding ;

  str:=str
  +'  function RunCode(theCode) {'  + LineEnding
  +'    eval(theCode);  '  + LineEnding
  +'}'  + LineEnding ;


  {$ifdef JScript}
//    // handle an inbound message of format:{"objid":<id>, "mtype":"SetParam", "pName":<pName>, "pValue":<pValue>}
    str:=str
    +'  window.addEventListener("message", function(ev) { '+Lineending
                  + 'if (ev.data.objid!=undefined) { '  +LineEnding
 //                 + '  alert("handle GPU inbound message "+ev.data.objid+"  "+ev.data.mtype); '+LineEnding
                  + '  if (ev.data.mtype=="SetNumParam") {    '+lineEnding
//                 + '  alert(ev.data.pName+"=["+ev.data.pValue.toString()+"];"); '+LineEnding
                  + '  try {   '+lineEnding
                  + '    eval(ev.data.pName+"=["+ev.data.pValue.toString()+"];");  '+lineEnding
                  + '   }catch(err){alert(err.message);}  '+lineEnding
                  + '  } '+LineEnding
                  + '  if (ev.data.mtype=="execCode") {    '+lineEnding
//                  + '  alert("handle execCode message "+ev.data.code); '+LineEnding
                  + '  try {   '+lineEnding
                  + '    eval(ev.data.code);  '+lineEnding
                  + '   }catch(err){alert(err.message);}  '+lineEnding
                  + '  } '+LineEnding
                  + '} '+lineEnding
                  + '} );'  +LineEnding;
  {$endif}
  result:=str;

end;

function GetParam(NodeName:String; pindex:integer):Float;
begin
  showMessage('GetParam');
  result:=0;
end;

function TXGPUCanvas.GPUJSAnimationFooter:String;
var
  str,cma,plist:String;
begin

  plist:=self.FullParamList;

  str:=     '  /*/------------------ Now Animate the Graphics ------------------------------------------------/*/   ' + LineEnding;


// The FrameDone function is called after each frame is finished.
// It is an async function, so that inbound messages are picked up, and we can use postMessage, eg. for passing parameter
// values back out from the IFrame into the project environment.
  str:=str
  +'  function FrameDone() {'  + LineEnding
  +'    return new Promise(resolve => { '  + LineEnding
  +'  }); } '+ LineEnding
  +'  function FetchParamValuesFromParent()  {' +LineEnding
 // +'    alert("param 0 is "+parent.window.pas.XGPUCanvas.GetParam('''+myNode.NodeName+''',0)); '+LineEnding
 // +'    alert("param 0 is "+parent.testnum); '+LineEnding
  +'  }  '+LineEnding;

//   {$ifndef JScript}
//   +'        var msg = ParamsAsString(); ' + LineEnding
//  // +'        alert("changing title to:"+msg); ' + LineEnding
//  // +'        document.title = "params ="+msg; ' + LineEnding
//  {$ifdef Chromium}
//  +'           var ob=getcomponent("paramString"); ' + LineEnding
//  +'           if (ob!=null) {ob.innerHTML=msg;} ' + LineEnding
//  // change the document title to trigger a cef titlechange event...
//  +'           document.title = "'+myNode.NodeName+' "+AnimationCounterValue;'  + LineEnding
//  {$else}
//  // running in external browser page - using polling to get title changes
//  {$endif}
//   {$else}
//   //  +'      alert("posting FrameDone message");'  + LineEnding
//   //+'          PostParamMessages("'+self.MyNode.NodeName+'");'  +LineEnding
//   {$endif}
//   +'    }); '  + LineEnding
//   +'  } ' + LineEnding;
//
    str:=str
   +'  async function DoFrame() {  '  + LineEnding
   +'          '+self.MyNode.NodeName+'CanvasRenderFn(AnimationCounterValue'+plist+'); ' + LineEnding
   +'            AnimationCounterValue = AnimationCounterValue +1; '  + LineEnding
   +'            if (AnimationCounterValue > AnimationCounterMax) {AnimationCounterValue = 0};  '  + LineEnding
   {$ifdef JScript}
   +'            try {    '  + LineEnding
   +'              FetchParamValuesFromParent();  '  + LineEnding
   +'            }catch(err){alert(err.message); clearInterval(AnimationFrameID);}  '  + LineEnding
   {$endif}
   +'            var xx = await FrameDone() ; '  + LineEnding
   +'  } '  + LineEnding;

   str:=str
   +'  var AnimationFrameID;  '  + LineEnding
   +'  var GPUIntervalRunner;  '  + LineEnding
   +'  function animate(timestamp){  '  + LineEnding
   +'    GPUIntervalRunner=setInterval(DoFrame, 1000/'+IntToStr(self.MaxFramesPerSec)+'); ' + LineEnding
   +'    } '  + LineEnding
   +'  AnimationFrameID=requestAnimationFrame(animate) ; '  + LineEnding;

  result:=str;
end;

function TXGPUCanvas.BuildPascalAnimationUnit(Compiler:TObject):String;
// Wrap the user-supplied code from the AnimationCode property in a unit, for compilation to JavaScript by pas2js.
var
  PascalHeader:TStringList;
//  UserCode:String;
  UserCodeParameterList:String;
  TheAnimationCode:TStringList;
  vstr:String;
  v:TNumArray;
  i,j:integer;
begin
  PascalHeader:=TStringList.Create;
  PascalHeader.Add(' unit GPUCode; ');
  PascalHeader.Add('interface');
  PascalHeader.Add('uses Classes, SysUtils;');
  PascalHeader.Add(' type ');
  PascalHeader.Add('     TNumArray = array of real;');
  PascalHeader.Add('     TImgArray = array of string;');
  PascalHeader.Add('     TConstantsRecord=record');
  for i:=0 to length(self.ConstIntArray)-1 do
  begin
   PascalHeader.Add('       const '+self.ConstIntArray[i].ConstName+':integer='+inttostr(self.ConstIntArray[i].ConstValue)+';');
  end;
  PascalHeader.Add('     end;');
  PascalHeader.Add('     TXThread = record	');
  PascalHeader.Add('       x,y,z:integer;	');
  PascalHeader.Add('     end;	');
  PascalHeader.Add('     TGPUThread = class	');
  PascalHeader.Add('        thread:TXThread;	');
  PascalHeader.Add('        constants:TConstantsRecord;     	');
  PascalHeader.Add('        procedure color(r,g,b,a:real); virtual; abstract;	');
  PascalHeader.Add('     end;	');
  PascalHeader.Add('     TFuncNotSupported = record	');
  PascalHeader.Add('          dummy:string; 	');
  PascalHeader.Add('     end;	');
  PascalHeader.Add('     TFuncNotSupported_Use_log = record	');
  PascalHeader.Add('          dummy:string;	');
  PascalHeader.Add('     end; 	');
  PascalHeader.Add('     TFuncNotSupported_Use_atan = record	');
  PascalHeader.Add('          dummy:string;	');
  PascalHeader.Add('     end;	');
  PascalHeader.Add('   implementation	');
  PascalHeader.Add('   function ln(b:TFuncNotSupported_Use_Log):TFuncNotSupported_Use_Log;	');
  PascalHeader.Add('   begin	');
  PascalHeader.Add('    asm alert(" This function is not supported by GPUJS"); end;	');
  PascalHeader.Add('   end;	');
  PascalHeader.Add('   function arctan(b:TFuncNotSupported_Use_atan):TFuncNotSupported_Use_atan;	');
  PascalHeader.Add('   begin	');
  PascalHeader.Add('    asm alert(" This function is not supported by GPUJS"); end;	');
  PascalHeader.Add('   end;	');
  PascalHeader.Add('   function chr(b:TFuncNotSupported):TFuncNotSupported;	');
  PascalHeader.Add('   begin	');
  PascalHeader.Add('    asm alert(" This function is not supported by GPUJS"); end;	');
  PascalHeader.Add('   end;	');
  PascalHeader.Add('   function Ord(  X: TFuncNotSupported):TFuncNotSupported;	');
  PascalHeader.Add('   begin	');
  PascalHeader.Add('    asm alert(" This function is not supported by GPUJS"); end;	');
  PascalHeader.Add('   end;	');
  PascalHeader.Add('   function pred( X: TFuncNotSupported):TFuncNotSupported;	');
  PascalHeader.Add('   begin	');
  PascalHeader.Add('    asm alert(" This function is not supported by GPUJS"); end;	');
  PascalHeader.Add('   end;	');
  PascalHeader.Add('   function Succ( X: TFuncNotSupported):TFuncNotSupported;	');
  PascalHeader.Add('   begin	');
  PascalHeader.Add('    asm alert(" This function is not supported by GPUJS"); end;	');
  PascalHeader.Add('   end;	');
  PascalHeader.Add('   function trunc( d: TFuncNotSupported):TFuncNotSupported;	');
  PascalHeader.Add('   begin	');
  PascalHeader.Add('    asm alert(" This function is not supported by GPUJS"); end;	');
  PascalHeader.Add('   end;	');
  PascalHeader.Add('   function sqr(d: TFuncNotSupported):TFuncNotSupported;	');
  PascalHeader.Add('   begin	');
  PascalHeader.Add('    asm alert(" This function is not supported by GPUJS"); end;	');
  PascalHeader.Add('   end;	');
  PascalHeader.Add('   	');

  PascalHeader.Add('// -------------- Declare Parameters List (for compilation) ---------- ');
  for i:=0 to length(self.ParamNumArray)-1 do
  begin
    PascalHeader.Add('   var '+ParamNumArray[i].ParamName+':TNumArray;');
  end;
  for i:=0 to length(self.ParamImgArray)-1 do
  begin
    PascalHeader.Add('   var '+ParamImgArray[i].ParamName+':TImgArray;');
  end;

  PascalHeader.Add('   procedure PascalVersionOfGPUCode(AnimationCounterValue:integer');
  if UserCodeParameterList<>'' then
    PascalHeader.Add(';'+UserCodeParameterList);
  PascalHeader.Add(');	');

  PascalHeader.Add('   var  this:TGPUThread;	');
  PascalHeader.Add('        r,g,b,a:real;	');
  PascalHeader.Add('        zzzzz1:integer;') ;          // used as position marker in resulting JS script
  //.............. user code block goes here .................
  TheAnimationCode:=TStringList.Create;
  TheAnimationCode.Text:=self.AnimationCode;
  WriteIncFile(Compiler,myNode.NodeName,'','',PascalHeader,TheAnimationCode);

  //..........................................................
  PascalHeader.Add('var zzzzz2:integer;');              // used as position marker in resulting JS script


  PascalHeader.Add('begin');
  PascalHeader.Add('end.');

  result:=PascalHeader.Text;
end;


function TXGPUCanvas.CompileGPUToJS(var GPUJSOutput:String):Boolean;
var
  ProgPath, PasFileName,ObjectFileName,ExeFileName:String;
  UnitString:String;
  Res,ok:Boolean;
  args:TStringList;
  prog:TStringList;
  {$ifdef JScript}
  lWebFS : TPas2JSWebFS;
  {$endif}
begin


  {$ifndef JScript}
  UnitString:=BuildPascalAnimationUnit(nil);

  ProgPath:=ExtractFilePath(Application.ExeName);
  prog:=TStringList.Create;
  prog.Text:=UnitString;

  // clean up from previous runs
  PASFileName:= self.myNode.NodeName+'.pas';
  DeleteFile(ProgPath+PASFileName);
  ObjectFileName:= self.myNode.NodeName+'.o';
  DeleteFile(ProgPath+ObjectFileName);
  ExeFileName:= self.myNode.NodeName+'.js';
  DeleteFile(ProgPath+ExeFileName);

 // GPUCodeEditor.ItemValue:=PascalCode.Text;
  // save the text to be compiled to the .pas file
  prog.SaveToFile(ProgPath+PASFileName);
  prog.Free;
  ok:=false;

  GPUCodeEditor.MessageLines:='';

  CompileMyProgram(self.myNode.NodeName,ProgPath,'',GPUCodeEditor,true);
  // and do checking for errors...
  // if the JS file exists, run on browser...
  if FileExists(ProjectDirectory+self.myNode.NodeName+'.js') then
  begin
    ok:=true;
    // and get the GPUJSOutput...
    GPUJSOutput:=ReadFile(ProjectDirectory+self.myNode.NodeName+'.js');
//    EditAttributeValue('XMemo1','ItemValue',GPUJSOutput);        //!!!! temporary for debugging
  end
  else
  begin
    GPUCodeEditor.ItemValue:=UnitString;
    GPUCodeEditor.MessagesHeight:='30%';
    EditingGPUNode:=self.myNode;
    GPUEditorMode:='Unit';
    GPUCodeEditor.ReadOnly:=true;
    GPUEditorForm.Showing:='Modal';
  end;

  {$else}
  //showmessage('starting compile section...');
  //with MyWebCompiler do
  begin
    UnitString:=BuildPascalAnimationUnit(MyWebCompiler.Compiler);

    MyWebCompiler.MyCodeEditor:=GPUCodeEditor;
    MyWebCompiler.Compiler.Log.OnLog:=@MyWebCompiler.DoLog;
    MyWebCompiler.Compiler.WebFS.LoadBaseURL:='';

    //showmessage('FirstUnitName='+FirstUnitName);
    if MyWebCompiler.MyCodeEditor<>nil then
      MyWebCompiler.myCodeEditor.ItemValue:=UnitString;

    Res:=False;

    // Load up the RTL sources that are required for the compilation...
    lWebFS:=MyWebCompiler.Compiler.WebFS;
    LoadRTLFilesForPas2JS(lWebFS);

    lWebFS.SetFileContent(self.NodeName+'.pas',UnitString);
//    showmessage('done main file save');
    args:=TStringList.Create;
    try
      Args.Add('-vwnhe');
      Args.Add('-O-');           //  Disable optimizations
      Args.Add('-Jc');           //  Write all JavaScript concatenated into the output file
      Args.Add('-Jirtl.js-');         //  Remove a JS file.
      Args.Add('-dJScript');
      Args.Add(self.NodeName+'.pas');

      //........................................................
      MyWebCompiler.Compiler.Run('','',Args,True);
      Res:=MyWebCompiler.Compiler.ExitCode=0;
      //........................................................

    finally
     Args.Free;
    end;

    EditAttributeValue('XMemo2','ItemValue',UnitString);        //!!!! temporary for debugging

    if res=true then
    begin
      //showmessage('compiler all done');
      GPUJSOutput:=MyWebCompiler.Compiler.WebFS.GetFileContent(self.NodeName+'.js');
    end
    else
    begin
      GPUCodeEditor.ItemValue:=UnitString;
      EditingGPUNode:=self.myNode;
      GPUCodeEditor.ReadOnly:=true;
      GPUEditorMode:='Unit';
      GPUEditorForm.Showing:='Modal';
    end;
  end;

  EditAttributeValue('XMemo1','ItemValue',GPUJSOutput);        //!!!! temporary for debugging

  ok:=res;
  if res=true then
  begin
  end;

//  showmessage('RunParser done.  Output='+GPUJSOutput);

  {$endif}

  prog.free;

  //....decide if there are errors or not .......
  //if ok=false then showmessage('Compilation failed')
  //else showmessage('Compilation successful');
  result:=ok;
end;

function TXGPUCanvas.FullXMLString:String;
var
  PasString:String;
  Pas2jsOutput:String;
  Pas2JSTrimmed,tmp:String;
  FullString:String;
  tmpList:TStringList;
  ok:Boolean;
  i,j:integer;
begin
  {$ifndef JScript}
  if not (csDesigning in ComponentState) then
  {$endif}
  begin
    PasString:=myNode.getAttribute('AnimationCode',true).AttribValue;
    if PasString<>'' then
    begin

      // Compile the Pascal code
      ok:=CompileGPUToJS(Pas2jsOutput);

      if ok then
      // Extract the bit of this that we need
      begin
        tmpList:=TStringList.Create;
        tmpList.Text:=Pas2JSOutput;
        while (tmpList.Count>0) and (FoundString(tmpList[0],'zzzzz1')<1) do
           tmpList.Delete(0);
  //      showmessage('after search for zzzzz1 '+inttostr(tmpList.Count)+' lines left');
        if tmpList.Count>0 then
        begin
          // delete the zzzzz1 line
          tmpList.Delete(0);
          i:=0;
          while i<tmpList.Count do
          begin
            if FoundString(tmpList[i],'zzzzz2')>0 then
            begin
  //            showmessage('found zzzzz2 at line '+inttostr(i));
              j:=i-1;                      // need to delete the preceding '}' as well
              i:=tmpList.Count;
            end;
            i:=i+1;
          end;
          while j<tmpList.Count do
            tmpList.Delete(j);
        end;
        Pas2JSTrimmed:=tmpList.Text;
        tmpList.Free;
      end;
    end;
    //showmessage('trimmed='+Pas2JSTrimmed);

    // Now filter out any qualifiers that may have been added by pas2js
    // eg. parameter variable P1 will have translated to $impl.P1
    //
    Pas2JSTrimmed := myStringReplace(Pas2JSTrimmed,'$impl.','',-1,-1);
    Pas2JSTrimmed := myStringReplace(Pas2JSTrimmed,'This.','this.',-1,-1);
    Pas2JSTrimmed := myStringReplace(Pas2JSTrimmed,'Math.','',-1,-1);

    // Look for 'for' loops.  Insert 'var' ahead of the loop variable.   eg. for (var i=0; i<something; i++)
    // NB. for loops MUST also be coded using a constant as the comparison element.
    Pas2JSTrimmed := myStringReplace(Pas2JSTrimmed,' for (',' for (var ',-1,-1);

    //!!!! check for GPU invalid chars (eg $) here and throw error????
    // also refs to rtl.

    // and wrap it with some the GPU JS...
    FullString:= GPUJSHeader +
                 Pas2JSTrimmed +
                 GPUJSFooter;
    if self.Animated then
      FullString:=FullString + GPUJSAnimationFooter;


    tmp:=UnSubstituteSpecials(gpujs);

    FullString:=
      '<!DOCTYPE html>' +  LineEnding
      +'<html>' +  LineEnding
      +'<body  style="margin:0px; font:normal 12px Verdana, Arial, sans-serif;">' +  LineEnding
//      +'<h2 id="myHeader">Testing GPU Canvas Component</h2> ' +  LineEnding
      +'<script>'+tmp+'</script>  ' +  LineEnding
      +'<div  id="GPUCanvas" > '+  LineEnding
      +'<script>'+  LineEnding
      +FullString +  LineEnding
      +'</script>' +  LineEnding
      +'</div> '+  LineEnding
      +'<div id="paramString" style="display:none" ></div>' + LineEnding
      +'</body> ' +  LineEnding
      +'</html> '+  LineEnding;

    //EditAttributeValue('XMemo1','ItemValue',FullString);        //!!!! temporary for debugging
    self.GeneratedHTML:=FullString;

  end;
  result:=FullString;
end;

procedure TGPUEventClass.CloseCodeEditor(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
  // careful here... if the compile has failed the editor may be showing the whole gpu code unit,
  // and not just the user code from the AnimationCode property.
  GPUEditorForm.Showing:='No';
  TXGPUCanvas(EditingGPUNode.ScreenObject).Active:=false;

  if GPUEditorMode='Animation' then
  begin
    // Update the property value ...         !!!! BUT NOT IF THE WHOLE UNIT IS ON DISPLAY>>>>>
    EditAttributeValue(EditingGPUNode.NodeName,'AnimationCode',GPUCodeEditor.ItemValue);
  end;
end;

procedure TGPUEventClass.GPUCodeEditHandleClickMessage(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var linenumber,targetLine:integer;
    SelectedLine,FileName,CharPos:string;
    FoundLineNum:Boolean;
    tmp1:TStringList;
    LineNum:String;
    Messages:TStringList;
begin
   //showmessage('GPUCodeEditHandleClickMessage '+ nodeID + ' '+myValue);
  {$ifndef JScript}
  linenumber:=GPUCodeEditor.TheMessages.CaretPos.Y;
  SelectedLine:= GPUCodeEditor.TheMessages.lines[linenumber];
  GPUCodeEditor.GetFileNameLineNumAndCharPos(FoundLineNum,SelectedLine, '(',FileName,LineNum,CharPos );
  {$else}
  // Find the message line thats been clicked on
  try
  linenumber:=StrToInt(myValue);
  except
    On E : EConvertError do
    EXIT;
  end;
  Messages:=TStringList.Create;
  Messages.Text:=GPUCodeEditor.MessageLines;
  if linenumber>Messages.Count then
    EXIT;
  SelectedLine:= Messages[linenumber-1];
  Messages.Free;

  // Find the indicated line number from the message
  GPUCodeEditor.GetFileNameLineNumAndCharPos(FoundLineNum,SelectedLine, '(',FileName,LineNum,CharPos );

  //showmessage('FileName='+FileName+' Indicated Linenum is '+linenum);
  {$endif}

  //load the indicated file from the tempinc folder into the code edit box
  if trim(FileName)<>'' then
  begin
    {$ifndef JScript}
      tmp1:=LoadIncludeFile(nil,FileName,'');
    {$else}
      tmp1:=LoadIncludeFile(myWebCompiler.Compiler,FileName,'');
    {$endif}

      if tmp1.Count>0 then
      begin

      GPUCodeEditor.ItemValue:=tmp1.Text;
      GPUCodeEditor.ReadOnly:=false;
      GPUEditorMode:='Animation';

      FreeAndNil(tmp1);

      // set cursor position
      if (LineNum<>'') and (CharPos<>'') then
      begin
        targetLine:=StrToInt(linenum);
        GPUCodeEditor.GoToLineCharPos(targetLine,StrToInt(CharPos))
      end;
    end
    else
    begin
      showmessage('This file is not available for edit');
    end;

   end;
end;

procedure TGPUEventClass.LaunchGPUHTML(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var
  myHTML:String;
begin
  if GPUMemo.ItemValue<>'' then
  begin
    myHTML:=GPUMemo.ItemValue;
    TXGPUCanvas(EditingGPUNode.ScreenObject).LaunchHTML('Data',myHTML,'GPU Diagnostic');
  end;
end;

procedure TXGPUCanvas.setupGPUPage;
var
  GPUString:String;
begin
  // called from StartMyGPU
  GPUString:=self.FullXMLString;
  self.HTMLSource:= GPUString;
end;

function TXGPUCanvas.GetParamNumValue(pName:String):TNumArray;
var
  i:integer;
  tmp:String;
  pval:TNumArray;
begin
  for i:=0 to length(ParamNumArray)-1 do
    if ParamNumArray[i].ParamName=pName then
    begin
      pval:=ParamNumArray[i].ParamValue;
    end;
  result:=pval;
end;

function TXGPUCanvas.GetConstIntValue(pName:String):integer;
var
  i:integer;
  tmp:String;
  pval:integer;
begin
  for i:=0 to length(ConstIntArray)-1 do
    if ConstIntArray[i].ConstName=pName then
    begin
      pval:=ConstIntArray[i].ConstValue;
    end;
  result:=pval;
end;

function TXGPUCanvas.GetParamImgValue(pName:String):TImgArray;
var
  i:integer;
  tmp:String;
  pval:TImgArray;
begin
  for i:=0 to length(ParamImgArray)-1 do
    if ParamImgArray[i].ParamName=pName then
    begin
      pval:=ParamImgArray[i].ParamValue;
    end;
  result:=pval;
end;

procedure TXGPUCanvas.SetParamNumValue(pName:String;pValue:TNumArray;ForwardToWidget:Boolean);
var
  i,j:integer;
  tmp:String;
  myurl:string;
begin
  for i:=0 to length(ParamNumArray)-1 do
    if uppercase(ParamNumArray[i].ParamName)=uppercase(pName) then
    begin
      SetLength(ParamNumArray[i].ParamValue,length(pValue));
      for j:=0 to length(pValue)-1 do
        ParamNumArray[i].ParamValue[j]:=pValue[j];
      //ParamNumArray[i].ParamValue:=pValue;          // causing errors...?

      if (ForwardToWidget)
      and (self.Active)
      and (self.HTMLSource<>'')
      and (self.HTMLSource<>'about:blank') then
      begin
        {$ifndef JScript}
        {$ifdef Chromium}
        myurl:= myChromium.Browser.MainFrame.GetURL();
        {$else}
        //!!!!
        myurl:='';
        {$endif}
        tmp:=NumArrayToJSONString(pValue);
        //tmp:='alert("updating gpu parameter '+pName+'"); RunCode("'+pName+'='+tmp+';")';
        tmp:='RunCode("'+pName+'='+tmp+';")';
        {$ifdef Chromium}
        myChromium.Browser.MainFrame.ExecuteJavaScript(tmp, myurl, 0);
        {$else}
        //!!!!
        {$endif}
        {$else}
        asm
          var ob=document.getElementById(this.NodeName+'Contents');
          if (ob!=null) {
            //alert('found iframe. posting param message');
            ob.contentWindow.postMessage({"objid":this.NodeName, "mtype":"SetNumParam", "pName":pName, "pValue":pValue},"*");
            }
        end;
        {$endif}
      end;

    end;
end;

procedure TXGPUCanvas.SetConstIntValue(pName:String;pValue:integer);
var
  i:integer;
  tmp:String;
  myurl:string;
begin
  for i:=0 to length(ConstIntArray)-1 do
    if uppercase(ConstIntArray[i].ConstName)=uppercase(pName) then
    begin
      ConstIntArray[i].ConstValue:=pValue;

    end;
end;

procedure TXGPUCanvas.SetParamImgValue(pName:String;pValue:TImgArray;ForwardToWidget:Boolean);
var
  i,j:integer;
  tmp:String;
  myurl:string;
begin
  for i:=0 to length(ParamImgArray)-1 do
    if uppercase(ParamImgArray[i].ParamName)=uppercase(pName) then
    begin
      SetLength(ParamImgArray[i].ParamValue,length(pValue));
      for j:=0 to length(pValue)-1 do
        ParamImgArray[i].ParamValue[j]:=pValue[j];
      //ParamImgArray[i].ParamValue:=pValue;        // causing errors...?

      if (ForwardToWidget)
      and (self.Active)
      and (self.HTMLSource<>'')
      and (self.HTMLSource<>'about:blank') then
      begin
        {$ifndef JScript}
        {$ifdef Chromium}
        myurl:= myChromium.Browser.MainFrame.GetURL();
        {$else}
        //!!!!
        {$endif}
        tmp:=ImgArrayToJSONString(pValue);
        tmp:='RunCode("'+pName+'='+tmp+';")';
        //showmessage('update img param: '+tmp);
        {$ifdef Chromium}
        myChromium.Browser.MainFrame.ExecuteJavaScript(tmp, myurl, 0);
        {$else}
        //!!!!
        {$endif}
        {$else}
        asm
          var ob=document.getElementById(this.NodeName+'Contents');
          if (ob!=null) {
            //alert('found iframe. posting param message');
            ob.contentWindow.postMessage({"objid":this.NodeName, "mtype":"SetImgParam", "pName":pName, "pValue":pValue},"*");
            }
        end;
        {$endif}
      end;
    end;
end;

procedure TXGPUCanvas.SetAnimationCode(AValue:string);
var
  GPUString:string;
  FullString:String;
begin
  {$ifndef JScript}
  if not (csDesigning in componentState) then
  {$endif}
  begin
    GPUString:=AValue;

    //showmessage('Frame setXMLString '+SVGString);
    myNode.SetAttributeValue('AnimationCode',GPUString);

    if (self.Active=true)
    and (not StartingUp) then
    begin
      FullString:=self.FullXMLString;
      myNode.SetAttributeValue('HTMLSource',FullString);
      showmessage('SetAnimationCode RedisplayFrame');
      RedisplayFrame;

    end;
  end;
end;

procedure TXGPUCanvas.StartMyGPU;
var
  h,w:integer;
begin
  // refresh the actual h/w attributes
  h:=self.ActualHeight;
  w:=self.ActualWidth;

  {$ifndef JScript}
  if not (csDesigning in componentState) then
  {$endif}
  begin
    HandleEvent('OnStart',self.myNode.NodeName,'');
    SetupGPUPage;
  end;
end;

procedure TXGPUCanvas.StopMyGPU;
var
  tmp,doJS,myurl:String;
begin
  {$ifndef JScript}
  if not (csDesigning in componentState) then
  {$endif}
  begin
    // stop the gpu loop
    doJS:='clearInterval(GPUIntervalRunner);';

    {$ifndef JScript}
    {$ifdef Chromium}
    if myChromium<>nil then
      if myChromium.Browser<>nil then
      begin
        myurl:= myChromium.Browser.MainFrame.GetURL();
        tmp:='RunCode("'+doJS+'")';
        myChromium.Browser.MainFrame.ExecuteJavaScript(tmp, myurl, 0);
      end;
    {$else}
    //!!!!
    {$endif}
    {$else}
    asm
    var ob=document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
      ob.contentWindow.postMessage({"objid":this.NodeName, "mtype":"execCode", "code":doJS},"*");
      }
    end;
    {$endif}

    self.HTMLSource:='';   //about:blank??

  end;
end;

//------------------------------- SetActive ----------------------------
// Setting the Active flag will build and run the GPU code inside the IFrame
//----------------------------------------------------------------------
procedure TXGPUCanvas.SetActive(AValue:Boolean);
begin
  if myNode<>nil then
  begin
    myNode.SetAttributeValue('Active',myBoolToStr(AValue),'Boolean');
    if (StartingUp=false) and (AValue=true) then
    begin
      self.StartMyGPU;
    end
    else
      self.StopMyGPU;
  end;
end;


procedure TXGPUCanvas.SetAnimated(AValue:Boolean);
begin
  myNode.SetAttributeValue('Animated',myBoolToStr(AValue),'Boolean');
end;
procedure TXGPUCanvas.SetParamNumList(AValue:string);
var
  pNames:TStringList;
  i:integer;
begin
  myNode.SetAttributeValue('ParamNumList',AValue,'String');
  SetLength(ParamNumArray,0);

  //use this comma-delimited list to initialise ParamArray.
  if AValue<>'' then
  begin
    pNames:=TStringList.Create;
    pNames.StrictDelimiter:=true;
    pNames.LineBreak:=',';
    pNames.Text:=AValue;
    SetLength(ParamNumArray,pNames.Count);
    for i:=0 to pNames.Count-1 do
    begin
      ParamNumArray[i].ParamName:=pNames[i];
      SetLength(ParamNumArray[i].ParamValue,1);
      ParamNumArray[i].ParamValue[0]:=0;
    end;
    pNames.Free;
  end;

end;
procedure TXGPUCanvas.SetConstIntList(AValue:string);
var
  pNames:TStringList;
  i:integer;
begin
  myNode.SetAttributeValue('ConstIntList',AValue,'String');
  SetLength(ConstIntArray,0);

  //use this comma-delimited list to initialise ParamArray.
  if AValue<>'' then
  begin
    pNames:=TStringList.Create;
    pNames.StrictDelimiter:=true;
    pNames.LineBreak:=',';
    pNames.Text:=AValue;
    SetLength(ConstIntArray,pNames.Count);
    for i:=0 to pNames.Count-1 do
    begin
      ConstIntArray[i].ConstName:=pNames[i];
      ConstIntArray[i].ConstValue:=0;
    end;
    pNames.Free;
  end;

end;

procedure TXGPUCanvas.SetParamImgList(AValue:string);
var
  pNames:TStringList;
  i:integer;
begin
  myNode.SetAttributeValue('ParamImgList',AValue,'String');
  SetLength(ParamImgArray,0);

  //use this comma-delimited list to initialise ParamArray.
  if AValue<>'' then
  begin
    pNames:=TStringList.Create;
    pNames.StrictDelimiter:=true;
    pNames.LineBreak:=',';
    pNames.Text:=AValue;

    SetLength(ParamImgArray,pNames.Count);
    for i:=0 to pNames.Count-1 do
    begin
      ParamImgArray[i].ParamName:=pNames[i];
      SetLength(ParamImgArray[i].ParamValue,1);
      //ParamImgArray[i].ParamValue[0]:=0;       //!!!!?
    end;
    pNames.Free;
  end;

end;

procedure TXGPUCanvas.SetMaxIterations(AValue:integer);
begin
  myNode.SetAttributeValue('MaxIterations',IntToStr(AValue),'String');
end;
procedure TXGPUCanvas.SetStartIteration(AValue:integer);
begin
  myNode.SetAttributeValue('StartIteration',IntToStr(AValue),'String');
end;
procedure TXGPUCanvas.SetNumFrames(AValue:integer);
begin
  myNode.SetAttributeValue('NumFrames',IntToStr(AValue),'String');
end;
procedure TXGPUCanvas.SetMaxFramesPerSec(AValue:integer);
begin
  myNode.SetAttributeValue('MaxFramesPerSec',IntToStr(AValue),'String');
end;


begin
  // this is the set of node attributes that each GPUCanvas instance will have (added to the set inherited from TXIFrame).
  AddDefaultAttribute(myDefaultAttribs,'ActualHeight','Integer','','',true,false);
  AddDefaultAttribute(myDefaultAttribs,'ActualWidth','Integer','','',true,false);
  AddDefaultAttribute(myDefaultAttribs,'Alignment','String','Left','',false);
  AddDefaultAttribute(myDefaultAttribs,'Hint','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'IsVisible','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'FrameWidth','String','300','',false);
  AddDefaultAttribute(myDefaultAttribs,'FrameHeight','String','300','',false);
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','Top','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelText','String','GPU Canvas','',false);
  AddDefaultAttribute(myDefaultAttribs,'HTMLSource','String','','',false,false);
  AddDefaultAttribute(myDefaultAttribs,'Active','Boolean','False','',false,false);
  AddDefaultAttribute(myDefaultAttribs,'Animated','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'ParamNumList','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'ParamImgList','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'ConstIntList','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'MaxIterations','Integer','512','',false);
  AddDefaultAttribute(myDefaultAttribs,'StartIteration','Integer','1','',false);
  AddDefaultAttribute(myDefaultAttribs,'NumFrames','Integer','100','',false);
  AddDefaultAttribute(myDefaultAttribs,'MaxFramesPerSec','Integer','15','',false);
  AddDefaultAttribute(myDefaultAttribs,'AnimationCode','String','','',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  GPUEvents:=TGPUEventClass.Create;
  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  AddAttribOptions(MyNodeType,'LabelPos',LabelPosOptions);
  {$IFndef JScript}
  RegisterClass(TXGPUCanvas);
  AddNodeFuncLookup(MyNodeType,@CreateGPUCanvasWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateInterfaceObjGPU,@CreateGPUCanvasWidget);
  {$endif}
  SuppressDesignerProperty(MyNodeType,'ContainerHeight');
  SuppressDesignerProperty(MyNodeType,'ContainerWidth');
  SuppressDesignerProperty(MyNodeType,'SuspendRefresh');
  SuppressDesignerProperty(MyNodeType,'BgColor');
  SuppressDesignerProperty(MyNodeType,'HTMLSource');


end.
