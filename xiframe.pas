(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XIFrame;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}
(*
In the windows desktop environment.....
if the CEF4 Chromium embedded widget tools are installed, then in the Windows environment, an IFrame
will be created as an embedded Chromium browser.  (compiler directive '-dChromium' is set).
If CEF4 is not available, a TXIFrame will be displayed by launching a separate browser page for each IFrame.
The project runtime then loosely communicates with the resulting browser page by running a polling timer which
finds and fetches the titles of opened windows.

In the JS/browser environment.....
This creates an HTML element of type iframe.
postMessage is then used to communicate between the parent window and the iframe.
*)

interface
uses
  Classes, SysUtils, TypInfo, StringUtils, NodeUtils,
  WrapperPanel, Events,
  {$ifndef JScript}
  LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, Propedits, RTTICtrls,
  LazsUtils, LCLIntf, LazLogger,
  {$if defined ( windows)}
  Windows, UTF8Process,
  {$else}
  process,
  {$endif}
  LazHelpHTML,
    {$ifdef Chromium}
    uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFConstants, uCEFTypes,
    uCEFWinControl,
    {$endif}
  {$else}
  HTMLUtils,
  {$endif}
  XForm;


{$ifndef JScript}
  procedure Register;
  {$if defined ( windows)}
  function EnumWindowsProc(WHandle: HWND; LParM: LParam): LongBool;StdCall;
  function GetTitleOfSelectedWindow(AHandle: HWND): string;
  {$endif}
{$else}
function CreateBasicIFrame(NameSpace,NodeName,MyObjectName:String):TObject;
procedure DoCreateFrameWidget(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer);
var dummyvar:integer;
{$endif}

procedure ResetHWAttributes(myNode:TDataNode);


{$ifndef JScript}

type
  TXIFrame = class(TWrapperPanel)
  private
    { Private declarations }
    fHandleClick:TEventHandler;
    fFrameTitle:String;

    function GetHTMLSource:string;
    function GetFrameWidth:string;
    function GetFrameHeight:string;
    function GetActualWidth:integer;
    function GetActualHeight:integer;
    function GetSuspendRefresh:Boolean;

    procedure SetHTMLSource(AValue:string);
    procedure SetFrameWidth(AValue:string);
    procedure SetFrameHeight(AValue:string);
    procedure SetIsSelected(AValue: Boolean); override;
    {$ifdef Chromium}
    procedure DoReloadTimerThing(sender:TObject);
    {$endif}
   procedure Loaded; override;
    procedure ParentWindowClick(Sender:TObject);
    procedure ApplyHTMLSource(AValue:String);


  protected
    { Protected declarations }
    procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
    {$if defined ( windows)}
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    {$ENDIF}
  public
    { Public declarations }
    BrowserLaunched:Boolean;
      {$if defined ( windows)}
      BrowserHandle:HWND;
      {$ELSE}
      BrowserHandle:string;
      {$ENDIF}

    lastmessage:String;
    pollingTimer:TTimer;
      {$ifdef Chromium}
      myStartupTimer:TTimer;
      myReloadTimer:TTimer;
      myChromium:TChromium;
      brw:iCefBrowser;
      ForceReload:Boolean;
      {$endif}
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent;IsDynamic:Boolean); override;
    destructor Destroy; override;
    procedure DoPollingTimer(Sender: TObject);     virtual;

      {$ifdef Chromium}
      procedure myChromiumAfterCreated(Sender: TObject; const browser: ICefBrowser);  virtual;
      procedure myChromiumBeforeClose(Sender: TObject; const browser: ICefBrowser);
      //TOnBeforePopup = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean) of object;
      procedure myChromiumBeforePopup(Sender: TObject;
          const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
          targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
          userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
          var windowInfo: TCefWindowInfo; var client: ICefClient;
          var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean;
          var Result: Boolean);
      procedure myChromiumClose(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
      procedure CEFLoaded(Sender: TObject; const browser: ICefBrowser; const TheFrame:ICefFrame; z:Longint);
      procedure TitleChange(Sender: TObject;const cefBrowser:ICefBrowser;const NewTitle:UString) ;  virtual;

      // You have to handle this two messages to call NotifyMoveOrResizeStarted or some page elements will be misaligned.
      procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
      procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
      // You also have to handle these two messages to set GlobalCEFApp.OsmodalLoop
      procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
      procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;

      procedure BrowserCreatedMsg(var aMessage: TMessage);  message CEF_AFTERCREATED;
      procedure BrowserDestroyMsg(var aMessage : TMessage); message CEF_DESTROY;
      procedure PendingInvalidateMsg(var aMessage : TMessage); message CEF_PENDINGINVALIDATE;

      procedure DoCreateBrowser(sender:TObject);
      procedure Wobble;
      {$endif}

    procedure SetSuspendRefresh(AValue: Boolean); virtual;
    procedure ReLoadURL;  virtual;
    procedure RePaint; override;
    procedure LoadDataURL(DataString:String);
    procedure RunJavaScript(JSString:String);
    procedure SetMyEventTypes;  virtual;
    procedure RedisplayFrame;
      {$if defined ( windows)}
      procedure WinlaunchBrowser(URL:String);    //TXIFrame.LaunchBrowser(URL:String);
      {$endif}
    procedure LaunchHTML(URLType,myURL,title:String);
    Procedure LaunchBrowserAbs(URLString:String);
    Procedure LaunchBrowserData(URLString:String);
    function CheckForNewMessage(expectedRoot:String):string;
    procedure CloseBrowserWindow;

    property FrameTitle:String read fFrameTitle write fFrameTitle;

  published
    // Properties defined for this class...
    property HTMLSource: String read GetHTMLSource write SetHTMLSource;
    property FrameHeight: String read GetFrameHeight write SetFrameHeight;
    property FrameWidth: String read GetFrameWidth write SetFrameWidth;
    property SuspendRefresh: Boolean read GetSuspendRefresh write SetSuspendRefresh;
    property ActualHeight:integer read GetActualHeight;
    property ActualWidth:integer read GetActualWidth;

    // Events to be visible in Lazarus IDE
    property HandleClick: TEventHandler read FHandleClick write FHandleClick;

  end;

{$else}

type
  TXIFrame = class(TWrapperPanel)
  private

    function GetHTMLSource:string;
    function GetFrameWidth:string;
    function GetFrameHeight:string;
    function GetActualWidth:integer;
    function GetActualHeight:integer;
    function GetSuspendRefresh:Boolean;

    procedure SetHTMLSource(AValue:string);
    procedure SetFrameWidth(AValue:string);
    procedure SetFrameHeight(AValue:string);

  protected
    { Protected declarations }
  public
    { Public declarations }
    BrowserHandle:longWord;
    BrowserPage:TObject;

    constructor Create(MyForm:TForm;NodeName,NameSpace:String); virtual;

    procedure SetSuspendRefresh(AValue: Boolean); virtual;
    procedure SetMyEventTypes;  virtual;
    procedure LoadDataURL(DataString:String);
    procedure RunJavaScript(JSString:String);
    procedure RedisplayFrame;
    procedure LaunchHTML(URLType,myURL,title:String);
    procedure CloseBrowserWindow;
    procedure ApplyHTMLSource(AValue:String);

  published
    { Published declarations }
    property HTMLSource: String read GetHTMLSource write SetHTMLSource;
    property FrameHeight: String read GetFrameHeight write SetFrameHeight;
    property FrameWidth: String read GetFrameWidth write SetFrameWidth;
    property SuspendRefresh: Boolean read GetSuspendRefresh write SetSuspendRefresh;
    property ActualHeight:integer read GetActualHeight;
    property ActualWidth:integer read GetActualWidth;

  end;

{$endif}


{$ifndef JScript}
{$if defined ( windows)}
{$else}
function myGetWindowNumericID(myWindowName:String):string;
Function myGetWindowTitle(WindowID:string):string;
{$endif}
var
  GlobalSuppressFrameDisplay:Boolean;     // to suppress launching of IFrame elements in external browser pages
{$endif}

implementation
{$ifndef JScript}
{$ifdef Chromium}
uses
  uCEFApplication;
{$endif}
{$endif}


var
IFrameDefaultAttribs:TDefaultAttributesArray;


procedure TXIFrame.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
  MyEventTypes.Add('IFrameExternalBrowserClosed');       // applicable when launched in external browser page only (eg. when CEF unavailable)
end;

{$ifndef JScript}
procedure Register;
begin
{$I xiframe_icon.lrs}
{$ifdef Chromium}
  RegisterComponents('XComponents',[TXIFrame]);
 {$endif}
    // inherited from TWrapperPanel, not required here
    RegisterPropertyEditor(TypeInfo(TColor), TXIFrame, 'BgColor', THiddenPropertyEditor);
    //RegisterPropertyEditor(TypeInfo(String), TXIFrame, 'LabelPos', THiddenPropertyEditor);
    //RegisterPropertyEditor(TypeInfo(String), TXIFrame, 'LabelText', THiddenPropertyEditor);
  end;

  constructor TXIFrame.Create(TheOwner:TComponent);
  begin
//    debugln('TXIFrame.Create(TheOwner:TComponent)');
    inherited Create(TheOwner,false);
    DoConstructor(TheOwner,false);
  end;

  constructor TXIFrame.Create(TheOwner:TComponent;IsDynamic:Boolean);
  begin
//    debugln('hello2');
    inherited Create(TheOwner,IsDynamic);
    DoConstructor(TheOwner,IsDynamic);
  end;

  procedure TXIFrame.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
  var
    tmp:TDataNode;
    myMemo:TMemo;
  begin
    BrowserLaunched:=false;
    pollingTimer:=TTimer.Create(self);
    pollingTimer.Interval:=500;
    pollingTimer.Enabled:=false;
    pollingTimer.OnTimer:=@self.DoPollingTimer;

    {$ifdef Chromium}
     self.ForceReload:=false;
     self.myReloadTimer:=TTimer.Create(self);
     myReloadTimer.Enabled:=false;
     myReloadTimer.Interval:=200;
     myReloadTimer.OnTimer:=@self.DoReloadTimerThing;

     myChromium:=TChromium.Create(self);
     myChromium.OnAfterCreated:=@self.myChromiumAfterCreated;
     myChromium.OnBeforeClose:=@self.myChromiumBeforeClose;
     myChromium.OnBeforePopup:=@self.myChromiumBeforePopup;
     myChromium.OnClose:=@self.myChromiumClose;
     myChromium.OnLoadEnd:=@self.CEFLoaded;


     myControl:=TCEFWindowParent.Create(self);
     TCEFWindowParent(myControl).OnClick:=@self.ParentWindowClick;
     myControl.Parent:=self;

     if (not (csDesigning in componentState))
     and (not (csLoading in componentState)) then
     begin
       self.myStartupTimer:=TTimer.Create(self);
       myStartupTimer.Enabled:=true;
       myStartupTimer.Interval:=200;
       myStartupTimer.OnTimer:=@self.DoCreateBrowser;
     end;
    {$else}
    // cef unavailable.  Create a 'placeholder' panel
    myControl:=TPanel.Create(self);
    myControl.Parent:=self;
    mymemo:=TMemo.Create(myControl);
    myMemo.Parent:=TPanel(myControl);
    myMemo.Align:=alClient;
    myMemo.Text:='Chromium Embedded Framework is unavailable - this content will be sent to local browser (tested for Chrome only). '
               + LineEnding
               + LineEnding
               + 'You may need to close old browser pages manually after content is updated through XIDE';
    myMemo.Enabled:=false;
    myMemo.Font.Color:=clGray;
    {$endif}


     myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
     // Make sure the embedded component can not be selected/deleted within the IDE
     myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];

    self.SetMyEventTypes;
    CreateComponentDataNode2(self,'TXIFrame',IFrameDefaultAttribs, self.myEventTypes, TheOwner,IsDynamic);

    tmp:=self.myNode;
    self.ParentColor:=true;
    // Setting IsContainer false will prevent Lazarus IDE designer dropping new child controls into this one.
    self.IsContainer:=false;

    AddLabel(myControl);

  end;

destructor TXIFrame.Destroy;
begin
  CloseBrowserWindow;
  inherited Destroy;
end;

procedure TXIframe.ParentWindowClick(Sender:TObject);
begin
  //showmessage('parent window click');
  CallHandleEvent('Click','',self);
end;

function CreateIFWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  //debugln('CreateIFWidget 1');
  NewNode:=CreateDynamicLazWidget('TXIFrame',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  //debugln('CreateIFWidget 2');
  result:=NewNode;
end;


{$ifdef Chromium}

procedure TXIFrame.myChromiumAfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
   if (not (csDesigning in componentState))
   and (not (csLoading in componentState)) then
  // Now the browser is fully initialized we can send a message to the main form to load the initial page.
     PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TXIFrame.myChromiumBeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
end;

procedure TXIFrame.myChromiumBeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean;
  var Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TXIFrame.CEFLoaded(Sender: TObject; const browser: ICefBrowser; const TheFrame:ICefFrame; z:Longint);
begin
end;


procedure TXIFrame.myChromiumClose(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
begin
   if (not (csDesigning in componentState))
   and (not (csLoading in componentState)) then
    PostMessage(Handle, CEF_DESTROY, 0, 0);
  //Result := True;
  aAction:=cbaClose;
end;
procedure TXIFrame.BrowserCreatedMsg(var aMessage: TMessage);
var
  glb:Boolean;
begin
  //showmessage(self.Name+' browser created');
  self.ForceReload:=true;
  self.HTMLSource:=self.HTMLSource;
  self.ForceReload:=false;
end;
procedure TXIFrame.BrowserDestroyMsg(var aMessage: TMessage);
begin
  //CEFWindowParent1.Free;
end;
procedure TXIFrame.PendingInvalidateMsg(var aMessage : TMessage);
begin
  self.Invalidate;
  TCEFWindowParent(myControl).InvalidateChildren;
end;


procedure TXIFrame.DoReloadTimerThing(sender:TObject);
begin
  if (not (csDesigning in componentState))
  and (not (csLoading in componentState)) then
    if (myChromium<>nil) and (myChromium.Initialized) then
    begin
      myReloadTimer.Enabled := false;
      if self.myNode.NodeType='TXSVGContainer' then
        self.ReLoadURL;    // svg reload
    end;
end;

procedure TXIFrame.DoCreateBrowser(sender:TObject);
var
  nm:String;
begin
  if (not (csDesigning in componentState))
  and (not (csLoading in componentState)) then
    if GetCurrentThreadID = MainThreadID  then
    begin
      if (myControl=nil)
      then
        myStartupTimer.Enabled := True
      else
      begin
        nm:=self.myNode.NodeName;
        if (myChromium<>nil)
        and not(myChromium.Initialized)
        and not(myChromium.CreateBrowser(TCEFWindowParent(myControl)))  then
          myStartupTimer.Enabled := True
        else
          myStartupTimer.Enabled := False;
      end;
    end
    else
      myStartupTimer.Enabled := False;
end;

procedure TXIFrame.TitleChange(Sender: TObject;const cefBrowser:ICefBrowser;const NewTitle:UString) ;
begin

end;

{$endif}


{$if defined ( windows)}
procedure TXIFrame.WinLaunchBrowser(URL:String);
var
  v: THTMLBrowserHelpViewer;
  BrowserPath, BrowserParams: string;
  p: LongInt;
  BrowserProcess: TProcessUTF8;
begin
  // procedure to open a fresh browser instance on the desktop, and load the URL
  //!! if 2 of these in quick succession, we still get one browser instance with 2 tabs...
  v:=THTMLBrowserHelpViewer.Create(nil);
  try
    v.FindDefaultBrowser(BrowserPath,BrowserParams);

//    URL:='http://www.lazarus.freepascal.org';
    p:=System.Pos('%s', BrowserParams);
    System.Delete(BrowserParams,p,2);
    System.Insert(URL,BrowserParams,p);

    // start browser
    BrowserParams:='--new-window "'+URL+'"';       //!! works for Chrome...
    BrowserProcess:=TProcessUTF8.Create(nil);
    try
      //BrowserProcess.CommandLine:='"start" "xxx" '+BrowserPath+' '+BrowserParams;
      //BrowserProcess.CommandLine:=BrowserPath+' "xxx" '+BrowserParams;
      BrowserProcess.CommandLine:=BrowserPath+BrowserParams;
      BrowserProcess.Execute;
    finally
      BrowserProcess.Free;
    end;
  finally
    v.Free;
  end;
end;
(*
procedure testlaunch2(URL: string);
// Apparently you need to pass your URL inside ", like "www.lazarus.freepascal.org"
var
  Browser, Params: string;
begin
  FindDefaultBrowser(Browser, Params);
  with TProcess.Create(nil) do
  try
    Executable := Browser;
    Params:=Format(Params, [URL]);
    Params:=copy(Params,3,length(Params)-3); // remove "", the new version of TProcess.Parameters does that itself
    Parameters.Add(Params);
    Options := [poNoConsole];
    Execute;
  finally
    Free;
  end;
end;
*)

// loaded from http://lazplanet.blogspot.com/2013/04/get-all-running-windows-titles.html
// modified to set the windows handle for the TXIFrame external browser
function EnumWindowsProc(WHandle: HWND; LParM: LParam): LongBool;StdCall; export;
var Title:array[0..128] of char;
    sTitle:STRING ;
    mySelf:TXIframe;
begin
 Result:=True;
 GetWindowText(wHandle, Title,128);
 sTitle:=Title;
 mySelf:=TXIFrame(pointer(LParM));
 if (pos( myself.myNode.NodeType, sTitle)>0)and(pos( myself.myNode.NodeName, sTitle)>0)  then
   if IsWindowVisible(wHandle) then
   begin
     myself.BrowserHandle:=WHandle;
   end;
end;

function GetTitleOfSelectedWindow(AHandle: HWND): string;
var
  ATitle: string;
  ALen: Integer;
begin
  Result := '';
  if AHandle <> 0 then begin
    ALen := GetWindowTextLength(AHandle) + 1;
    SetLength(ATitle, ALen);
    GetWindowText(AHandle, PChar(ATitle), ALen);
    result := Trim(ATitle);
  end;
end;
{$ELSE}
function mylistWindows:string;
var outputstring:string;
    Success:boolean;
begin
  outputstring :='***';
  Success:=RunCommand('sudo wmctrl -l',outputstring);
  //if Success = false
  //then showmessage('Error --- Failed to list current window --- is wmctrl installed?') ;
  mylistWindows:= outputstring;
end;

procedure mySetWindowTitle(WindowID,NewName:string);
var Success:boolean;
    inputstring,outputstring:string;
begin
  WindowID:=trim(WindowID);
  if (pos( '0x', WindowID) = 1   )
  then  inputstring :='sudo wmctrl -r -i '+WindowID+'  '+ ' -N '+ ' "'+NewName+'"'     // ID is a hex number
  else  inputstring :='sudo wmctrl -r   "'+WindowID+'"  '+ ' -N '+ ' "'+NewName+'"';   // ID is a name
  outputstring :='***';
  Success:=RunCommand(inputstring,outputstring);
  if Success = false
  then showmessage('Error --- Failed to set title of window >'+WindowID+'< to >'+inputstring+'<--- is wmctrl installed?') ;
end;

function myGetWindowNumericID(myWindowName:String):string;
var tempstringlist:TStringList;
    i,WindowidStart:integer;
    windowid,tempstring:string ;
begin
  windowid:='Window named >'+myWindowName+'< not found';
  tempstringlist:=TStringlist.create;
  tempstringlist.Duplicates := dupIgnore;
  tempstringlist.Sorted := False;
  tempstringlist.Delimiter := chr(13);
  tempstringlist.Text:=mylistWindows;
  if tempstringlist.count > 0 then
  for i:=0 to  tempstringlist.count - 1 do
  begin
    if pos(myWindowName,tempstringlist[i])>0 then
    begin
       WindowidStart:=pos('0x',tempstringlist[i]) ;
       if WindowidStart > 0
       then windowid:= copy( tempstringlist[i],WindowidStart,10)
       else showmessage('Window ID not found in >'+tempstringlist[i]+'<');
    end;
  end;
  tempstringlist.free;
  myGetWindowNumericID:=windowid;
end;

Function myGetWindowTitle(WindowID:string):string;
var tempstringlist:TStringList;
    i,WindowTitleStart,temppos:integer;
    windowTitle,tempstring:string ;
begin
  windowTitle:=' ';
  tempstringlist:=TStringlist.create;
  tempstringlist.Duplicates := dupIgnore;
  tempstringlist.Sorted := False;
  tempstringlist.Delimiter := chr(13);
  tempstringlist.Text:=mylistWindows;
  if tempstringlist.count > 0 then
  for i:=0 to  tempstringlist.count - 1 do
  begin
    temppos:= pos(WindowID,tempstringlist[i]);
    if temppos>0 then
    begin
       WindowTitleStart:=temppos+11 ;
       windowTitle:= copy( tempstringlist[i],WindowTitleStart,999999) ;
    end;
  end;
  tempstringlist.free;
  myGetWindowTitle:=windowTitle;
end;

{$ENDIF}

function TXIframe.CheckForNewMessage(expectedRoot:String):string;
var currentmessage, startstr:string;
    myPtr:Longint;
begin
  result:='';
{$if defined ( windows)}
  // this assumes the window title includes the string "TXIFrame"
  // this puts the handle of the browser in "BrowserHandle"
  myPtr:=  WinSizeDependentInt(self);
  BrowserHandle:=0;
  EnumWindows(@EnumWindowsProc,myPtr);
  if BrowserHandle>0 then
    // now find the current value of the title
    currentmessage:=GetTitleOfSelectedWindow(BrowserHandle);
{$ELSE}
  BrowserHandle:= myGetWindowNumericID('TXIFrame');
  currentmessage:=myGetWindowTitle(BrowserHandle);
{$ENDIF}
  startstr:=currentMessage.Substring(0,length(expectedRoot));
  if startstr=expectedRoot then
  begin
    if lastmessage <>  currentmessage then
    begin
      result := currentmessage;
    end
    else result := '';
    lastmessage := currentmessage;
  end;
end;

procedure TXIFrame.DoPollingTimer(Sender: TObject);
var message:string;
begin
   message:=CheckForNewMessage('');
   if (length( message)>0) then
   begin
     PollingTimer.Interval:=500;
     BrowserLaunched:=false;
   end;
   {$if defined ( windows)}
   if BrowserHandle<1 then
   {$else}
   if BrowserHandle='' then
   {$endif}
   begin
     // window must have been closed    (or not open yet....)
     if BrowserLaunched=false then
       TTimer(sender).Enabled:=false;
   end;
end;

Procedure TXIFrame.LaunchBrowserAbs(URLString:String);
begin
  {$if defined ( windows)}
  if BrowserHandle<1 then
  {$else}
  if BrowserHandle='' then
  {$endif}
    // try not to start multiple browser pages
    begin
      self.LaunchHTML('Abs',URLString,'TXIFrame');
      self.BrowserLaunched:=true;
      PollingTimer.Interval:=4000;
      PollingTimer.enabled:=true;
      lastmessage:='';
    end;
end;
Procedure TXIFrame.LaunchBrowserData(URLString:String);
begin
  {$if defined ( windows)}
  if BrowserHandle<1 then
  {$else}
  if BrowserHandle='' then
  {$endif}
    // try not to start multiple browser pages
    begin
      self.LaunchHTML('Data',URLString,'TXIFrame');
      self.BrowserLaunched:=true;
      PollingTimer.Interval:=4000;
      PollingTimer.enabled:=true;
      lastmessage:='';
    end;
end;


procedure TXIFrame.RePaint;
begin
  self.Invalidate;
end;

procedure TXIframe.Loaded;
begin
  // runtime, after component is loaded from lfm file
  inherited Loaded;

  if myNode<>nil then
    if myNode.HasAttribute('SuspendRefresh') then
      myNode.SetAttributeValue('SuspendRefresh','False');

  {$ifdef Chromium}
  // Load the browser (later)
  if not (csDesigning in componentState) then
    self.myReloadTimer.Enabled := True;
  {$endif}
end;


{$if defined ( windows)}
procedure TXIFrame.WMPaint(var Message: TWMPaint);
var
  MCanvas:TControlCanvas;
  DrawBounds: TRect;
  h,w:integer;
begin
  inherited;
  // refresh the actual h/w attributes
  h:=self.ActualHeight;
  w:=self.ActualWidth;

  {$ifdef Chromium}
  // refresh the frame contents on timer
  if (not (csDesigning in componentState))
  and (not (csLoading in componentState)) then
    myReloadTimer.Enabled:=true;
  {$endif}
end;
{$endif}

procedure TXIFrame.ReLoadURL;
begin
//  myChromium.Reload;     //this makes it go blank!!!!
  ApplyHTMLSource(self.HTMLSource);
end;

{$ifdef Chromium}
procedure TXIFrame.WMMove(var aMessage: TWMMove);
begin
  inherited;
  if (not (csDesigning in componentState))
  and (not (csLoading in componentState)) then
    if (myChromium <> nil) then myChromium.NotifyMoveOrResizeStarted;
end;

procedure TXIFrame.WMMoving(var aMessage: TMessage);
begin
  inherited;
  if (not (csDesigning in componentState))
  and (not (csLoading in componentState)) then
    if (myChromium <> nil) then myChromium.NotifyMoveOrResizeStarted;
end;

procedure TXIFrame.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;
  if (not (csDesigning in componentState))
  and (not (csLoading in componentState)) then
    if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TXIFrame.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;
  if (not (csDesigning in componentState))
  and (not (csLoading in componentState)) then
    if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;
{$endif}

procedure TXIframe.SetIsSelected(AValue: Boolean);
var
  oldval:Boolean;
begin
  oldval:= fIsSelected;
  if AValue<>oldval then
    begin
      fIsSelected:=AValue;
      ShowHideSelectedBorder(self.myNode,AValue);
      Repaint;
    end;
end;



//'''''''''''''''''''''''''''''''''''''''''''''''''''
{$else} //JScript


function CreateBasicIFrame(NameSpace,NodeName,MyObjectName:String):TObject;
var
  ob:TObject;
begin
asm
  try {
  function resized(ob){
  // refresh the actual h/w attributes
  var h=ob.ActualHeight;
  var w=ob.ActualWidth;
  }
  ob=null;
  var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'"></label>';
  var FrameString = '<iframe  id='+MyObjectName+' name="'+MyObjectName+ '" '+
                          'scrolling="no" '+
                          'src="" '+
                          'title="" '+
                          'style="height:100%;width:100%;border: 1px solid #444444;" '+
                          'onresize="resized(this);" '+
                          '>'+
                          '</iframe>';
  var HTMLString = labelstring+FrameString;



  var wrapper=document.getElementById(NameSpace+NodeName);
  wrapper.innerHTML= HTMLString;

  //............ Set an event for title change ...................

  var Iframe = document.getElementById(MyObjectName);

//  // create an observer instance
//  var observer = new MutationObserver(function(mutationsList) {
//      for(var mutation of mutationsList) {
//        if ((mutation.type == "attributes")
//        && (mutation.attributeName == "title")) {
//           alert('title mutation detected');
//           pas.Events.handleEvent(null,'Click',NodeName, mutation.oldValue);
//         }
//      }
//   });
//
//   // pass in the target node, as well as the observer options
//  //!!!! observer target has to be the document>Title within the IFrame (not the Iframe's title attribute)....
//        // this one works on the iframe title correctly....
//      observer.observe(Iframe, {subtree: true, characterData: true,
//                                attributes: true //configure it to listen to attribute changes
//                               });
//  }

// ... attempt to reset the default 8px margin that appears in the iframe document body ...(unsuccessful)
//  function restyle() {
//     var body = Iframe.contentDocument.body;
//     body.style.padding = 0;
//     body.style.margin = 0;
//  }
//
//  Iframe.onload = restyle;
//  restyle();
//

  ob = Iframe;
  } catch(err){alert(err.message+'  in CreateBasicIFrame');}
end;
result:=ob;
end;


procedure DoCreateFrameWidget(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer);
var
  NodeType,NameSpace:String;
  ht,wd:integer;
begin
  NodeType:=MyNode.NodeType;
  NameSpace:=MyNode.NameSpace;
    asm
      try{

      var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,NameSpace,NodeType,position);

      var Iframe = pas.XIFrame.CreateBasicIFrame(NameSpace,ScreenObjectName,NameSpace+ScreenObjectName+'Contents');

      }
      catch(err) { alert(err.message+'  in XIFrame.DoCreateFrameWidget');}

    end;

    MyNode.ScreenObject:=MyNode;

    RefreshComponentProps(myNode);

    // refresh the actual h/w attributes
    ht:=TXIFrame(myNode).ActualHeight;
    wd:=TXIFrame(myNode).ActualWidth;
end;

  constructor TXIFrame.Create(MyForm:TForm;NodeName,NameSpace:String);
  begin
    inherited Create(NodeName,NameSpace);
    self.NodeType:='TXIFrame';
    self.MyForm:=MyForm;

    self.SetMyEventTypes;
    self.IsContainer:=false;

    SetNodePropDefaults(self,IFrameDefaultAttribs);
  end;


  function CreateIFWidget(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
  begin
    DoCreateFrameWidget(MyNode, ParentNode,ScreenObjectName,position);
    TXIFrame(myNode).HTMLSource:=TXIFrame(myNode).HTMLSource;

    result:=myNode;
  end;

  function CreateinterfaceObjIF(MyForm:TForm;NodeName,NameSpace:String):TObject;
  begin
    result:=TObject(TXIFrame.Create(MyForm,NodeName,NameSpace));
  end;

{$endif}

procedure TXIFrame.CloseBrowserWindow;
begin
  {$ifndef JScript}
  {$if defined ( windows)}
  if self.BrowserHandle>0 then
    PostMessage(BrowserHandle, WM_CLOSE, 0, 0);   //!! sadly this closes the whole browser (so is ok if we open all frames in sep instances)
  self.BrowserHandle:=0;
  {$else}
  //!!!!
  if self.BrowserHandle<>'' then
    begin
    end;
  self.BrowserHandle:='';
  {$endif}
  {$else}
  //!!!!    ??
  {$endif}
end;

  procedure TXIFrame.LoadDataURL(DataString:String);
  var
    tmp:string;
  begin
    if DataString<>'' then
    begin
      {$IFndef JScript}
        {$ifdef Chromium}
        if (myChromium<>nil)
        and (myChromium.Browser<>nil) then
        begin
          tmp:=myChromium.Browser.MainFrame.GetUrl;
          myChromium.Browser.MainFrame.LoadString(DataString, 'data:text/html');        //wobble??
 //         //self.Wobble;
        end;
        {$else}
        if (self.SuspendRefresh=false)
        and (not GlobalSuppressFrameDisplay)
        and (DataString<>'') then
        begin
           CloseBrowserWindow;
           launchBrowserData(DataString);
        end;
        {$endif}
      {$else}
      myNode.SetAttributeValue('HTMLSource',DataString);
      //showmessage('calling RedisplayFrame');
      RedisplayFrame;
      {$endif}
    end;
  end;

  procedure TXIFrame.RunJavaScript(JSString:String);
  var
  myurl:string;
  begin
    //NB code size is a max of 255 chars ?
  {$IFndef JScript}
    {$ifdef Chromium}
       myurl:= myChromium.Browser.MainFrame.GetURL();
       myChromium.Browser.MainFrame.ExecuteJavaScript(JSString, myurl, 0);
    {$else}
    //!!!!
    {$endif}
    {$else}
    asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
    alert('!!!! need to write code to handle this in TXIFrame.RunJavaScript (send message into iframe???)');
    }
    end;
    {$endif}
  end;

function TXIFrame.GetFrameHeight:string;
begin
  result:=MyNode.getAttribute('FrameHeight',true).AttribValue;
end;
function TXIFrame.GetFrameWidth:string;
begin
  result:=MyNode.getAttribute('FrameWidth',true).AttribValue;
end;

function TXIFrame.GetSuspendRefresh:Boolean;
begin
  result:=myStrToBool(MyNode.getAttribute('SuspendRefresh',true).AttribValue);
end;
function TXIFrame.GetHTMLSource:String;
begin
  result:=MyNode.getAttribute('HTMLSource',true).AttribValue;
end;

procedure TXIFrame.ApplyHTMLSource(AValue:String);
begin
  if ((FoundString(AValue,'://') > 0) and (FoundString(AValue,'http') = 1))
  or (FoundString(AValue,'//') = 1)
  or (FoundString(AValue,'about:') = 1) then
  // eg. URL is absolute; either "http://example.com" or "//example.com"
  begin
    //showmessage('found absolute url '+AValue);
    {$IFndef JScript}
    {$ifdef Chromium}
    if myChromium<>nil then
    begin
      myChromium.LoadURL(UTF8Decode(AValue));
    end;
    {$else}
    begin
      ClosebrowserWindow;
      LaunchBrowserAbs(AValue);
    end;
    {$endif}
    {$else}
    asm
      var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
      if (ob!=null) {
        ob.src = AValue;
      }
    end;
    {$endif}

  end
  else
  begin
    // url is a 'data' type...
    //showmessage('setting data URL'+AValue);
    // data url
    {$ifndef JScript}
    //if (IsChanged)
    {$ifdef windows}
    if (self.BrowserHandle<1)
    {$else}
    if (self.BrowserHandle='')
    {$endif}
    then
    {$endif}
      self.LoadDataURL(AValue);
  end;

end;

procedure TXIFrame.SetHTMLSource(AValue:string);
var
  IsChanged:Boolean;
  oldval:String;
begin
  IsChanged:=false;
  oldval:=myNode.GetAttribute('HTMLSource',true).AttribValue;
 // LazsUtils.writeTofile('old.txt',oldval);
 // LazsUtils.writeTOfile('new.txt',AValue);
  if AValue<>myNode.GetAttribute('HTMLSource',true).AttribValue then
    IsChanged:=true;
  {$ifndef JScript}
  {$ifdef Chromium}
  if self.ForceReload=true then
    IsChanged:=true;
  {$endif}
  {$endif}
  myNode.SetAttributeValue('HTMLSource',AValue);

  if (self.SuspendRefresh)
  {$ifndef JScript}
  //or (GlobalSuppressFrameDisplay)
  or (csLoading in componentState)
  or (csDesigning in componentState)
  {$endif}
  then
    EXIT;

  if IsChanged then
    ApplyHTMLSource(AValue);

// example of 'embedded' URL's that work inside a browser iframe element.
// https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d37273.69524119164!2d-0.4477915275058492!3d54.275548127073684!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x487f254b19e38abb%3A0x69546c0179a47ac7!2sScarborough!5e0!3m2!1sen!2suk!4v1557476265485!5m2!1sen!2suk
// https://www.google.com/maps/embed
// http://www.bbc.co.uk/news/technology-35731734/embed
// http://www.bbc.co.uk/news/embed/
end;
procedure TXIFrame.SetSuspendRefresh(AValue: Boolean);
var
  tmp:String;
begin
  if (myNode<>nil)
  {$ifndef JScript}
  and ((not (csLoading in componentState)) or (AValue=true))
  {$endif}
  then
  begin
    myNode.SetAttributeValue('SuspendRefresh',myBoolToStr(AValue),'Boolean');
    if AValue=false then
      {$ifndef JScript}
      if (not (csLoading in componentState))
      and (not (csDesigning in componentState))
      and (self.myNode.MyForm<>nil) then
        tmp:=TXForm(self.myNode.MyForm).Showing;

      if (not (csLoading in componentState))
      and (not (csDesigning in componentState))
      // if the frame is on a visible form...
      and (self.myNode.MyForm<>nil)
      and (TXForm(self.myNode.MyForm).Showing <> 'No') then
      {$endif}
        self.RedisplayFrame;
  end;
end;

procedure TXIFrame.SetFrameWidth(AValue:string);
var
  w:integer;
{$ifndef JScript}
  tc:TControl;
{$endif}
begin
  myNode.SetAttributeValue('FrameWidth',AValue);
  {$ifndef JScript}
  tc:=self.myControl;
  SetHeightWidth(self.myNode,tc,'FrameWidth','FrameHeight');
  {$else}
  asm
  var ob = document.getElementById(this.NameSpace+this.NodeName);
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
  end;
  {$endif}
  // refresh the actualwidth attribute
  w:=self.ActualWidth;
end;

procedure TXIFrame.SetFrameHeight(AValue:string);
var
  h:integer;
{$ifndef JScript}
  tc:TControl;
{$endif}
begin
  myNode.SetAttributeValue('FrameHeight',AValue);
  {$ifndef JScript}
  tc:=self.myControl;
  SetHeightWidth(self.myNode,tc,'FrameWidth','FrameHeight');
  {$else}
  asm
  var ob = document.getElementById(this.NameSpace+this.NodeName);
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'H',AValue);
  end;
  {$endif}
  // refresh the actualheight attribute
  h:=self.ActualHeight;
end;

{$ifndef JScript}
{$ifdef Chromium}
procedure TXIFrame.Wobble;
begin
  // do the following to 'wobble' the frame display, forcing (hopefully) a repaint!
  ShowHideSelectedBorder(self.myNode,(not self.IsSelected));
  Repaint;
  ShowHideSelectedBorder(self.myNode,self.IsSelected);
  Repaint;
end;
{$endif}
{$endif}


procedure TXIFrame.RedisplayFrame;
var
  sup,AbsoluteURI:Boolean;
  SourceString:String;
begin
  // nudge an IFrame component into re-displaying (eg after content has changed)
  if (self.SuspendRefresh)
  {$ifndef JScript}
  //or (GlobalSuppressFrameDisplay)
  or (csLoading in componentState)
  or (csDesigning in componentState)
  {$endif}
  then
    EXIT;

  SourceString:=myNode.GetAttribute('HTMLSource',false).AttribValue;
  {$ifndef JScript}
  if myControl<>nil then
  begin
    {$ifdef Chromium}
    if (myChromium<>nil) and (myChromium.Initialized) then
    begin
      self.ReLoadURL;
      // do the following to 'wobble' the frame display, forcing (hopefully) a repaint!
      self.Wobble;
    end;
    {$else}
    self.myNode.SetAttributeValue('HTMLSource','');
    self.HTMLSource:=SourceString;
    {$endif}
  end;
  {$else}
  if ((FoundString(SourceString,'://') > 0) and (FoundString(SourceString,'http') = 1))
  or (FoundString(SourceString,'//') = 1)
  or (FoundString(SourceString,'about:') = 1) then
  // eg. URL is absolute; either "http://example.com" or "//example.com"
    AbsoluteURI:=true
  else
    AbsoluteURI:=false;
  sup:=StartingUp;
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
       // SADLY.....Once instantiated, the frame will not refresh when the src attribute is changed.
      // So, we will have to delete the frame object and re-create with its new src.
      var myFrame = ob; // get frame
      var originalId = myFrame.id; // retain the original id of the frame
      var newFrameId = myFrame.id + new Date().getTime(); // create a new temporary id
      var newIframe = pas.XIFrame.CreateBasicIFrame(this.NameSpace,this.NodeName,newFrameId);
      newIframe.id = originalId; // change id back
      ob = document.getElementById(originalId);
      //alert('refreshing frame canvas '+SourceString);
      if (AbsoluteURI)
      {
        var uri=SourceString;
      }
      else
      {
        uri='data:text/html,   ' + encodeURIComponent(SourceString);
      }
      ob.src=uri;

    }
  end;
  self.LabelText:=self.LabelText;




  {$endif}
end;

function TXIFrame.GetActualHeight:integer;
// NB. this is a read-only attribute (no setter)
var
  h:integer;
begin
  {$ifndef JScript}
  h:=myControl.Height;
  {$else}
  h:=GetCurrentHeight(self.NodeName);
  {$endif}
  myNode.SetAttributeValue('ActualHeight',inttostr(h),'Integer',true);     // just so the attribute exists
  result:=h;
end;
function TXIFrame.GetActualWidth:integer;
// NB. this is a read-only attribute (no setter)
var
  wd:integer;
begin
  {$ifndef JScript}
  wd:=myControl.Width;
  {$else}
  wd:=GetCurrentWidth(self.NodeName);
  {$endif}
  myNode.SetAttributeValue('ActualWidth',inttostr(wd),'Integer',true);     // just so the attribute exists
  result:=wd;
end;

procedure TXIFrame.LaunchHTML(URLType,myURL,title:String);
var
  filename:String;
begin
    {$ifndef JScript}
    filename:=ProjectDirectory+MainUnitName+myNode.NodeName+'.html';
    LazsUtils.WriteToFile(filename,myURL);
    filename:='file://'+filename;
    // open in the default browser.
    if URLType='Data' then
    begin
      {$if defined ( windows)}
      WinLaunchBrowser(filename);
      {$else}
      OpenDocument(filename);
      {$endif}
    end
    else
    begin
      {$if defined ( windows)}
      WinLaunchBrowser(myURL);
      {$else}
      OpenURL(myURL);
      {$endif}
    end;
    {$else}
    asm
    var objid=this.NodeName;
    //alert('open window with name '+objid);
    var win=window.open("",objid,"");                  // third (blank) parameter makes a new window
    win.document.write(myURL);
    this.BrowserPage=win;
    win.onunload = function(event) {win.opener.postMessage({"objid":win.name, "NameSpace":"", "mtype":"titleChange", "mdata":""},"*"); };
    end;
    {$endif}

end;



procedure ResetHWAttributes(myNode:TDataNode);
var
  h,w:integer;
begin
  h:=TXIFrame(myNode.ScreenObject).ActualHeight;
  w:=TXIFrame(myNode.ScreenObject).ActualWidth;
  //showmessage('ResetHWAttributes w='+inttostr(w));
end;

begin
  {$ifndef JScript}
//  {$ifndef Chromium}
  GlobalSuppressFrameDisplay:=true;   // necessary for cef4, when creating new iframe from IDE menu, ....or loading in a Laz project?
//  {$endif}
  {$endif}
  // this is the set of node attributes that each XIFrame instance will have.
  AddWrapperDefaultAttribs(IFrameDefaultAttribs);
  AddDefaultAttribute(IFrameDefaultAttribs,'SuspendRefresh','Boolean','False','',false);
  AddDefaultAttribute(IFrameDefaultAttribs,'ActualHeight','Integer','','',true,false);
  AddDefaultAttribute(IFrameDefaultAttribs,'ActualWidth','Integer','','',true,false);
  AddDefaultAttribute(IFrameDefaultAttribs,'FrameWidth','String','300','',false);
  AddDefaultAttribute(IFrameDefaultAttribs,'FrameHeight','String','300','',false);
  AddDefaultAttribute(IFrameDefaultAttribs,'Border','Boolean','True','',false);
  AddDefaultAttribute(IFrameDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(IFrameDefaultAttribs,'LabelPos','String','Top','',false);
  AddDefaultAttribute(IFrameDefaultAttribs,'LabelText','String','IFrame','',false);
  AddDefaultAttribute(IFrameDefaultAttribs,'BgColor','Color','#FFFFFF','',false);
  AddDefaultAttribute(IFrameDefaultAttribs,'HTMLSource','String','','',false);
  AddDefaultsToTable('TXIFrame',IFrameDefaultAttribs);


  AddAttribOptions('TXIFrame','Alignment',AlignmentOptions);
  AddAttribOptions('TXIFrame','LabelPos',LabelPosOptions);

  {$IFndef JScript}
  Classes.RegisterClass(TXIFrame);
  AddNodeFuncLookup('TXIFrame',@CreateIFWidget);
  {$else}
  AddNodeFuncLookup('TXIFrame',@CreateInterfaceObjIF,@CreateIFWidget);
  {$endif}

  SuppressDesignerProperty('TXIFrame','ContainerHeight');
  SuppressDesignerProperty('TXIFrame','ContainerWidth');

end.

