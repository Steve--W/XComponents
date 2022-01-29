(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XHTMLEditor;

{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
    Classes, SysUtils, TypInfo, StringUtils, NodeUtils, XIFrame,
    UtilsJSCompile, XForm, XCode, XButton, XVBox, XTabControl, XMemo, EventsInterface,
    {$ifndef JScript}
    LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, Propedits, RTTICtrls,
    LazsUtils, LCLIntf,
    LCLType, gettext,
    {$if defined (windows)}
    Windows,
    {$ENDIF}
    {$ifdef Chromium}
    uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFConstants, uCEFTypes, uCEFChromiumEvents, uCEFRenderProcessHandler,
    uCEFWinControl, uCEFProcessMessage, uCEFApplication,
    {$endif}
  {$else}
  HTMLUtils,
  {$endif}
  WrapperPanel, Events;

{$ifndef JScript}
procedure Register;
{$endif}

{$ifdef JScript}
type TXHTMLMessage = record
  objid:String;
  NameSpace:String;
  mtype:String;
  mdata:String;
end;
procedure HandleTXHTMLMessage(msg:TXHTMLMessage);
{$endif}
{$ifndef JScript}
const
  XHTMLEDITOR_GETTEXT = 'geteditedtext';
  XHTMLEDITOR_SEND_TEXT  = 'sendeditedtext';
  {$endif}

type


  TXHTMLEditor = class(TXIFrame)
  private
    fHandleChange:TEventHandler;

    function GetIsEmbedded:Boolean;
    function GetIsEditable:Boolean;
    function GetShowing:Boolean;
    function GetSourceText:String;
    function GetHeaderHTML:String;
    function GetFooterHTML:String;

    procedure SetIsEmbedded(AValue:Boolean);
    procedure SetIsEditable(AValue:Boolean);
    procedure SetShowing(AValue:Boolean);
    procedure SetSourceText(AValue:String);
    procedure SetHeaderHTML(AValue:String);
    procedure SetFooterHTML(AValue:String);

  protected

    Procedure PopUpBrowser;
  public
    {$ifndef JScript}
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent;IsDynamic:Boolean); override;
    procedure DoHTMLEditorConstructor;
    procedure DoPollingTimer(Sender: TObject);  override;
    procedure ReLoadURL; override;
    {$ifdef Chromium}
    procedure TitleChange(Sender: TObject;const cefBrowser:ICefBrowser;const NewTitle:UString) ;  override;
    procedure myChromiumAfterCreated(Sender: TObject; const browser: ICefBrowser); override;

    //  TOnProcessMessageReceived       = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId; const message: ICefProcessMessage; out Result: Boolean) of object;
    procedure ChromiumProcessMessageReceived(
      Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; sourceProcess: TCefProcessId;
      const message: ICefProcessMessage; out Result: Boolean);
    {$endif}


    {$else}    // JScript
    constructor Create(MyForm:TForm;NodeName,NameSpace:String);  override;
    {$endif}
    function CreateTextURL:tstringlist;
    function ExtractTextFromTitle(message:String):String;
    procedure SetMyEventTypes; override;


  published
    property IsEmbedded: Boolean read GetIsEmbedded write SetIsEmbedded;
    property IsEditable: Boolean read GetIsEditable write SetIsEditable;
    property Showing: Boolean read GetShowing write SetShowing;
    property SourceText: String read GetSourceText write SetSourceText;
    property HeaderHTML: String read GetHeaderHTML write SetHeaderHTML;
    property FooterHTML: String read GetFooterHTML write SetFooterHTML;

    // Events to be visible in Lazarus IDE
    property HandleChange: TEventHandler read FHandleChange write FHandleChange;
  end;

   procedure dotest;

implementation

const MyNodeType='TXHTMLEditor';
var
  myDefaultAttribs:TDefaultAttributesArray;

  TEST:String;

procedure TXHTMLEditor.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
  MyEventTypes.Add('Change');
  MyEventTypes.Add('HTMLEditorBrowserClosed');   // applicable when launched in external browser page only (eg. when CEF unavailable)
end;

function  TXHTMLEditor.ExtractTextFromTitle(message:String):String;
var
  str:String;
  StartPos,EndPos:integer;
begin
  str:=message;
  StartPos:=Pos('Z!Z!Z',str);
  if StartPos>0 then
  begin
    str:=copy(str, StartPos+5 ,999999);

    EndPos:=Pos('Z!Z!Z',str);
    if endpos>0      // end marker might be missing if the OS has truncated the title string -- now checked for in the browser code
    then
      str:=copy(str,1 ,EndPos-1);
  end
  else
    str:='Z!Z!Z';
  result:=str;
end;


{$ifndef JScript}
{$ifdef Chromium}
// Handler for messages sent OUT of the Cef browser
procedure TXHTMLEditor.ChromiumProcessMessageReceived(
  Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage; out Result: Boolean);
var
  NewText:String;
begin
  case message.Name of
    XHTMLEDITOR_SEND_TEXT:
    begin
      NewText := message.ArgumentList.GetString(0);
      //just set attribute here
      self.myNode.SetAttributeValue('SourceText',NewText);
      //event here (eg) to refresh ob inspector
      if (StartingUp=false)
      and (XForm.TXForm(self.myNode.MyForm).Showing<>'No') then
        CallHandleEventLater('Change',NewText,self.myControl);

    end
  else
    inherited;
  end;
end;

{$endif}

procedure Register;
begin
  {$I Icons/XHTMLEdit.lrs}
  {$ifdef Chromium}
  RegisterComponents('XComponents',[TXHTMLEditor]);
  {$endif}
end;

function CreateHTMLEditorWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXHTMLEditor',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;

procedure TXHTMLEditor.DoHTMLEditorConstructor;
begin
  {$ifdef Chromium}
  myChromium.OnProcessMessageReceived:=@self.ChromiumProcessMessageReceived;
  myChromium.OnTitleChange:=@self.TitleChange;
  {$endif}
  pollingTimer.OnTimer:=@self.DoPollingTimer;

  self.IsContainer:=false;
  self.myNode.NodeType:='TXHTMLEditor';

  AddDefaultAttribs(self,self.myNode,mydefaultAttribs);

end;

constructor TXHTMLEditor.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoHTMLEditorConstructor;
end;

constructor TXHTMLEditor.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoHTMLEditorConstructor;
end;

{$ifdef Chromium}
procedure TXHTMLEditor.myChromiumAfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  if not (csDesigning in componentState) then
  begin
  // Now the browser is fully initialized we can send a message to the main form to load the initial page.
    PostMessage(Handle, CEF_AFTERCREATED, 0, 0);

  end;
end;

procedure TXHTMLEditor.TitleChange(Sender: TObject;const cefBrowser:ICefBrowser;const NewTitle:UString) ;
var
  params:TStringList;
  c:integer;
  NewText:String;
  TempMsg : ICefProcessMessage;
  //newval:ICefValue;
begin
  // User has pressed 'Save'.
  if (not (csDesigning in componentState))
  and (not StartingUp)
  and (self.myNode<>nil)
  then
  begin
    // Send a cef message to fetch the new value of the text
    // (Use the ArgumentList property if you need to pass some parameters.)
    TempMsg := TCefProcessMessageRef.New(XHTMLEDITOR_GETTEXT);
    //TempMsg.ArgumentList.SetValue(0,newval);              //  msg.ArgumentList.SetString(0, txt);

    myChromium.SendProcessMessage(PID_RENDERER, TempMsg);

  end;
end;
{$endif}

function checkTitle(txt:String):Boolean;
var
    j,k:Integer;
begin
  j:=txt.IndexOf('Z!Z!Z');
  k:=txt.IndexOf('Z!Z!Z',j+5);
  if (j>0) and (k>0) then
  begin
    result:=true;
  end
  else
  begin
    showmessage('Error ---- There is a problem saving the edited text (text too long ?)  ---- Save your work to the clipboard, and Quit ');
    result:= false;
  end
end;

procedure TXHTMLEditor.DoPollingTimer(Sender: TObject);
var message,teststr:string;
begin
   teststr:='TXHTMLEditor ' + self.myNode.NodeName + 'Z!Z!Z';
   message:=CheckForNewMessage(teststr);

   if (length( message)>0) then
   begin
     if CheckTitle(message)=true then
     begin
       PollingTimer.Interval:=500;
       BrowserLaunched:=false;
       // top and tail the string to get just the text (this also does a truncation check)
       message:=ExtractTextFromTitle(message);
       if message<>'Z!Z!Z' then
       begin
         // now save the returned text to the SourceText property
         SourceText:= message;
       end;
     end;
   end;
   {$if defined ( windows)}
   if BrowserHandle<1 then
   {$else}
   if BrowserHandle='' then
   {$endif}
   begin
     // window must have been closed    (or not open yet....)
     if self.BrowserLaunched=false then
     begin
       PollingTimer.Enabled:=false;
       self.Showing:=false;
       CallHandleEventLater('HTMLEditorBrowserClosed','',self);
     end;
   end;
end;

procedure TXHTMLEditor.ReLoadURL;
begin
  self.SourceText:=self.SourceText;
end;

{$else} //JScript  ...............................

constructor TXHTMLEditor.Create(MyForm:TForm;NodeName,NameSpace:String);
begin
  inherited Create(MyForm,NodeName,NameSpace);
  self.NodeType:='TXHTMLEditor';
  self.IsContainer:=false;

  SetNodePropDefaults(self,myDefaultAttribs);
end;

function CreateHTMLEditorWidget(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewWidget:TXHTMLEditor;
  h,w:integer;
begin
//showmessage('TXHTMLEditor CreateWidget. '+MyNode.NodeName);
  DoCreateFrameWidget(MyNode, ParentNode,ScreenObjectName,position);

  NewWidget:=TXHTMLEditor(myNode);

  // refresh the actual h/w attributes
  h:=NewWidget.ActualHeight;
  w:=NewWidget.ActualWidth;

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXHTMLEditor.Create(MyForm,NodeName,NameSpace));
end;

procedure HandleTXHTMLMessage(msg:TXHTMLMessage);
var
  ItemNode:TdataNode;
  message:string;
begin
  if (msg.objid<>'') then
  begin
    //showmessage('HandleTXHTMLMessage: '+msg.objid+' '+msg.mtype);
     //this is a notification sent out from within a HTMLEditor frame.
     ItemNode:=findDataNodeById(systemnodetree,msg.objid,msg.NameSpace,false);
     if ItemNode<>nil then
     begin
        if msg.mtype='titleChange' then
        begin
           //showmessage('message is titleChange');
           message:=msg.mdata;
           if message<>'' then
           begin
             message:= TXHTMLEditor(ItemNode).ExtractTextFromTitle(message);
             //showmessage(message);
             // now save the help text to the SourceText property
             if (length(trim(message))>0) then // and (StartPos>0)
             begin
                TXHTMLEditor(ItemNode).SourceText:= message;
                //event here (eg) to refresh ob inspector
                if StartingUp=false then
                  HandleEvent('Change',ItemNode.NodeName,msg.NameSpace,message);
             end;
           end
           else     // window was closed
           begin
             TXHTMLEditor(ItemNode).Showing:=false;
             HandleEvent('HTMLEditorBrowserClosed',ItemNode.NodeName,msg.NameSpace,'');
           end;
        end;
     end;
  end;
end;

{$endif}

function TXHTMLEditor.GetIsEmbedded:Boolean;
begin
  result:=myStrToBool(myNode.getAttribute('IsEmbedded',true).AttribValue);
end;
function TXHTMLEditor.GetIsEditable:Boolean;
begin
  result:=myStrToBool(myNode.getAttribute('IsEditable',true).AttribValue);
end;
function TXHTMLEditor.GetSourceText:String;
begin
  result:=myNode.getAttribute('SourceText',true).AttribValue;
end;
function TXHTMLEditor.GetHeaderHTML:String;
begin
  result:=myNode.getAttribute('HeaderHTML',true).AttribValue;
end;
function TXHTMLEditor.GetFooterHTML:String;
begin
  result:=myNode.getAttribute('FooterHTML',true).AttribValue;
end;
function TXHTMLEditor.GetShowing:Boolean;
begin
  result:=myStrToBool(myNode.getAttribute('GetShowing',true).AttribValue);
end;

procedure TXHTMLEditor.SetIsEmbedded(AValue:Boolean);
begin
  if myNode<>nil then
  begin
    myNode.SetAttributeValue('IsEmbedded',myBoolToStr(AValue),'Boolean');
    if AValue=false then
      self.IsVisible:=false
    else
    begin
      self.IsVisible:=true;
      self.SourceText:=self.SourceText;
    end;
  end;
end;
procedure TXHTMLEditor.SetIsEditable(AValue:Boolean);
begin
  if myNode<>nil then
  begin
    myNode.SetAttributeValue('IsEditable',myBoolToStr(AValue),'Boolean');
    self.SourceText:=self.SourceText;  // re-generate the HTML edit page
  end;
end;
procedure TXHTMLEditor.SetShowing(AValue:Boolean);
begin
  if myNode<>nil then
  begin
    myNode.SetAttributeValue('Showing',myBoolToStr(AValue),'Boolean');
    // this property has no effect unless IsEmbedded is False.
    if IsEmbedded=false then
    begin
      if AValue=true then
        //!! if not already open....
        PopupBrowser
      else
      begin
        CloseBrowserWindow;
      end;
    end;
  end;
end;
procedure TXHTMLEditor.SetSourceText(AValue:String);
Var URLStringList:TStringList;
begin
  if myNode<>nil then
  begin
    myNode.SetAttributeValue('SourceText',AValue,'String');
    //showmessage('SetSourceText '+AValue);
    {$ifndef JScript}
    {$ifdef Chromium}
    if self.myChromium.Initialized then
    {$endif}
    {$endif}
    begin
      URLStringList:=CreateTextURL;
      self.HTMLSource:=URLStringList.Text;
    {$ifndef JScript}
    {$ifdef Chromium}
    end
    else
    begin   // cef not ready yet, so save the generated HTML in the node attribute
      URLStringList:=CreateTextURL;
      myNode.SetAttributeValue('HTMLSource',URLStringList.Text,'String');
    {$endif}
    {$endif}
    end;
  end;
end;

procedure TXHTMLEditor.SetHeaderHTML(AValue:String);
begin
  if myNode<>nil then
  begin
    if AValue <> myNode.getAttribute('HeaderHTML',false).AttribValue then
    begin
      myNode.SetAttributeValue('HeaderHTML',AValue,'String');
      self.SourceText:=self.SourceText;
    end;
  end;
end;

procedure TXHTMLEditor.SetFooterHTML(AValue:String);
begin
  if myNode<>nil then
  begin
    if AValue <> myNode.getAttribute('FooterHTML',false).AttribValue then
    begin
      myNode.SetAttributeValue('FooterHTML',AValue,'String');
      self.SourceText:=self.SourceText;
    end;
  end;
end;

Procedure TXHTMLEditor.PopUpBrowser;
Var URLStringList:TStringList;
begin
  //showmessage('PopUpBrowser. embedded='+mybooltostr(IsEmbedded));
  URLStringList:=CreateTextURL;
  if IsEmbedded=false then
  begin
    {$ifndef JScript}
    {$if defined ( windows)}
    if BrowserHandle<1 then
    {$else}
    if BrowserHandle='' then
    {$endif}
    {$endif}
    // try not to start multiple browser pages
    begin
      self.LaunchHTML('Data',URLStringList.Text,'TXHTMLEditor');
      {$ifndef JScript}
      self.BrowserLaunched:=true;
      PollingTimer.Interval:=4000;     // allow time for a browser to start up
      PollingTimer.enabled:=true;
      lastmessage:='';
      {$endif}
    end;
  end
  else
  begin
    self.HTMLSource:=URLStringList.Text;
  end;
  URLStringList.free;
end;


function TXHTMLEditor.CreateTextURL:tstringlist;
var WYSIWYGHEADER,WYSIWYGFOOTER,HelpText,OutputStringList: TStringList;
  startstring, endstring:String;
  InnerStartLength,InnerEndLength:integer;
  ActionBarClass:String;
  i:integer;
begin


WYSIWYGHEADER:= TStringList.Create;
WYSIWYGFOOTER:= TStringList.Create;
HelpText:= TStringList.Create;
OutputStringList:= TStringList.Create;

//Initalise the the header, footer and help text strings
if IsEditable = true
then startstring:='<div contenteditable="true"'
else startstring:='<div contenteditable="false"';
//startstring:=startstring+' class="wysiwyg-content" id = "my_wysiwyg_editor" style="height:100%; width:100%" >';
startstring:=startstring+' class="wysiwyg-content" id = "my_wysiwyg_editor" >';
endstring := '</div>' ;

InnerStartLength:=length(startstring);
InnerEndLength:=length(endstring );

HelpText.Add(startstring);
HelpText.Add(self.SourceText);
HelpText.Add(endstring );

{$ifdef JScript}
WYSIWYGHEADER.Add('<!DOCTYPE html>');
{$endif}
WYSIWYGHEADER.Add('<html>');
WYSIWYGHEADER.Add('<head>');
WYSIWYGHEADER.Add('<!--');
WYSIWYGHEADER.Add('The MIT License (MIT)');
WYSIWYGHEADER.Add('');
WYSIWYGHEADER.Add('Copyright (c) Jared Reich');
WYSIWYGHEADER.Add('');
WYSIWYGHEADER.Add('Permission is hereby granted, free of charge, to any person obtaining a copy');
WYSIWYGHEADER.Add('of this software and associated documentation files (the "Software"), to deal');
WYSIWYGHEADER.Add('in the Software without restriction, including without limitation the rights');
WYSIWYGHEADER.Add('to use, copy, modify, merge, publish, distribute, sublicense, and/or sell');
WYSIWYGHEADER.Add('copies of the Software, and to permit persons to whom the Software is');
WYSIWYGHEADER.Add('furnished to do so, subject to the following conditions:');
WYSIWYGHEADER.Add('');
WYSIWYGHEADER.Add('The above copyright notice and this permission notice shall be included in all');
WYSIWYGHEADER.Add('copies or substantial portions of the Software.');
WYSIWYGHEADER.Add('');
WYSIWYGHEADER.Add('THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR');
WYSIWYGHEADER.Add('IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,');
WYSIWYGHEADER.Add('FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE');
WYSIWYGHEADER.Add('AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER');
WYSIWYGHEADER.Add('LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,');
WYSIWYGHEADER.Add('OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE');
WYSIWYGHEADER.Add('SOFTWARE.');
WYSIWYGHEADER.Add('');
WYSIWYGHEADER.Add('Downloaded from ...... https://github.com/jaredreich/pell......26/01/2019');
WYSIWYGHEADER.Add('');
WYSIWYGHEADER.Add('Modified by Steve Wright 27/01/2019 to exclude images and code but include');
WYSIWYGHEADER.Add('superscripts, centering, colors, undo, save, done and quit functions ');
WYSIWYGHEADER.Add('as well as code to return the edited text to the calling program -->');
WYSIWYGHEADER.Add('');
WYSIWYGHEADER.Add('<meta name="viewport" content="user-scalable=1.0,initial-scale=1.0,minimum-scale=1.0,maximum-scale=1.0">');

WYSIWYGHEADER.Add('<title>TXHTMLEditor '+self.myNode.NodeName+'</title>');

WYSIWYGHEADER.Add('<style>');
WYSIWYGHEADER.Add('.wysiwyg-actionbar-color: #FFF !default;');
WYSIWYGHEADER.Add('.wysiwyg-border-color: rgba(10, 10, 10, 0.1) !default;');
WYSIWYGHEADER.Add('.wysiwyg-border-style: solid !default;');
WYSIWYGHEADER.Add('.wysiwyg-border-width: 1px !default;');
WYSIWYGHEADER.Add('.wysiwyg-button-height: 30px !default;');
WYSIWYGHEADER.Add('.wysiwyg-button-selected-color: #F0F0F0 !default;');
WYSIWYGHEADER.Add('.wysiwyg-button-width: 30px !default;');
//WYSIWYGHEADER.Add('.wysiwyg-content-height: 300px !default;');
//WYSIWYGHEADER.Add('.wysiwyg-content-height: 100%;');
WYSIWYGHEADER.Add('.wysiwyg-content-padding: 10px !default;');
WYSIWYGHEADER.Add('.wysiwyg {');
WYSIWYGHEADER.Add('  border: .wysiwyg-border-width .wysiwyg-border-style .wysiwyg-border-color;');
//WYSIWYGHEADER.Add('  box-sizing: border-box;');
WYSIWYGHEADER.Add('}');
WYSIWYGHEADER.Add('');
WYSIWYGHEADER.Add('.wysiwyg-content {');
WYSIWYGHEADER.Add('  box-sizing: border-box;');
//WYSIWYGHEADER.Add('  height: .wysiwyg-content-height;');
WYSIWYGHEADER.Add('  height: 100%;');
WYSIWYGHEADER.Add('  width: 100%;');
WYSIWYGHEADER.Add('  outline: 0;');
//WYSIWYGHEADER.Add('  overflow-y: auto;');
WYSIWYGHEADER.Add('  padding: .wysiwyg-content-padding;');
WYSIWYGHEADER.Add('}');
WYSIWYGHEADER.Add('');
WYSIWYGHEADER.Add('.wysiwyg-actionbar {');
WYSIWYGHEADER.Add('  background-color: powderblue;');
WYSIWYGHEADER.Add('  border-bottom: .wysiwyg-border-width .wysiwyg-border-style .wysiwyg-border-color;');
WYSIWYGHEADER.Add('}');
WYSIWYGHEADER.Add('');
WYSIWYGHEADER.Add('.wysiwyg-button {');
WYSIWYGHEADER.Add('  background-color: transparent;');
WYSIWYGHEADER.Add('  border: none;');
WYSIWYGHEADER.Add('  cursor: pointer;');
WYSIWYGHEADER.Add('  height: .wysiwyg-button-height;');
WYSIWYGHEADER.Add('  outline: 0;');
WYSIWYGHEADER.Add('  width: .wysiwyg-button-width;');
WYSIWYGHEADER.Add('  vertical-align: bottom;');
WYSIWYGHEADER.Add('}');
WYSIWYGHEADER.Add('');
WYSIWYGHEADER.Add('.wysiwyg-button-selected {');
WYSIWYGHEADER.Add('  background-color: .wysiwyg-button-selected-color;');
WYSIWYGHEADER.Add('}');
WYSIWYGHEADER.Add('.showActionBar {background-color:powderblue; border-style: solid; border-width:thin;');
WYSIWYGHEADER.Add('}');
WYSIWYGHEADER.Add('.hideActionBar {height:0px;');
WYSIWYGHEADER.Add('}');
WYSIWYGHEADER.Add('');
WYSIWYGHEADER.Add('</style>');
WYSIWYGHEADER.Add('<style>');
WYSIWYGHEADER.Add('      html {');
WYSIWYGHEADER.Add('        height: 100%;');
WYSIWYGHEADER.Add('      }');
WYSIWYGHEADER.Add('');
WYSIWYGHEADER.Add('      body {');
WYSIWYGHEADER.Add('        height: 100%;');
WYSIWYGHEADER.Add('        margin: 0;');
WYSIWYGHEADER.Add('        padding: 0;');
WYSIWYGHEADER.Add('      }');
WYSIWYGHEADER.Add('');
WYSIWYGHEADER.Add('      .content {');
WYSIWYGHEADER.Add('        box-sizing: border-box;');
WYSIWYGHEADER.Add('        margin: 0 auto;');
//WYSIWYGHEADER.Add('        max-width: 1000px;');
WYSIWYGHEADER.Add('        padding: 20px;');
WYSIWYGHEADER.Add('      }');
WYSIWYGHEADER.Add('');
WYSIWYGHEADER.Add('      #html-output {');
WYSIWYGHEADER.Add('        white-space: pre-wrap;');
WYSIWYGHEADER.Add('      }');
WYSIWYGHEADER.Add('');
WYSIWYGHEADER.Add('</style>');
WYSIWYGHEADER.Add('</head>');

WYSIWYGHEADER.Add('<body>');
WYSIWYGHEADER.Add('');
WYSIWYGHEADER.Add('<div id="FrameContent" class="content" style="display:flex;flex-direction:column;background-color:powderblue; height:100%">');
WYSIWYGHEADER.Add(myNode.GetAttribute('HeaderHTML',false).AttribValue);
if IsEditable=true then ActionBarClass := 'showActionBar'
else ActionBarClass:='hideActionBar';
WYSIWYGHEADER.Add('      <div id="MyActionBar" class="'+ActionBarClass+'" >');
WYSIWYGHEADER.Add('      </div>');
WYSIWYGHEADER.Add('      <div id="editor" class="wysiwyg" style="flex-grow: 1; width: 100%; overflow:auto;background-color:white; border-style: solid;border-width:thin;">');

WYSIWYGFOOTER.Add('      </div>');
WYSIWYGFOOTER.Add(myNode.GetAttribute('FooterHTML',false).AttribValue);
WYSIWYGFOOTER.Add('    </div>');
WYSIWYGFOOTER.Add('<script>');

WYSIWYGFOOTER.Add('function stopediting(){window.close()};');
WYSIWYGFOOTER.Add('const defaultParagraphSeparatorString = "defaultParagraphSeparator"');
WYSIWYGFOOTER.Add('const formatBlock = "formatBlock"');
WYSIWYGFOOTER.Add('const addEventListener = (parent, type, listener) => parent.addEventListener(type, listener)');
WYSIWYGFOOTER.Add('const appendChild = (parent, child) => parent.appendChild(child)');
WYSIWYGFOOTER.Add('const createElement = tag => document.createElement(tag)');
WYSIWYGFOOTER.Add('const queryCommandState = command => document.queryCommandState(command)');
WYSIWYGFOOTER.Add('const queryCommandValue = command => document.queryCommandValue(command)');
WYSIWYGFOOTER.Add('');
WYSIWYGFOOTER.Add('//export');
WYSIWYGFOOTER.Add('const exec = (command, value = null) => document.execCommand(command, false, value)');
WYSIWYGFOOTER.Add('var updateCounter = 0; ');
WYSIWYGFOOTER.Add('function checkTitle() {');
WYSIWYGFOOTER.Add('  var Checkstring = document.title.substr(document.title.length-5);');
WYSIWYGFOOTER.Add('  if (Checkstring != "Z!Z!Z" ){ ');
WYSIWYGFOOTER.Add('    alert("Error ---- There is a problem saving the edited text (text too long ?)  ---- Save your work to the clipboard, and Quit ")');
WYSIWYGFOOTER.Add('    return false; } ' );
WYSIWYGFOOTER.Add('  else return true; ' );
WYSIWYGFOOTER.Add('}');
WYSIWYGFOOTER.Add('const defaultActions = {');
WYSIWYGFOOTER.Add('');
WYSIWYGFOOTER.Add('bold: {');
WYSIWYGFOOTER.Add('    icon: "<b>B</b>",');
WYSIWYGFOOTER.Add('    title: "Bold",');
WYSIWYGFOOTER.Add('    state: () => queryCommandState("bold"),');
WYSIWYGFOOTER.Add('    result: () => exec("bold")');
WYSIWYGFOOTER.Add('  },');
WYSIWYGFOOTER.Add('');
WYSIWYGFOOTER.Add('italic: {');
WYSIWYGFOOTER.Add('    icon: "<i>I</i>",');
WYSIWYGFOOTER.Add('    title: "Italic",');
WYSIWYGFOOTER.Add('    state: () => queryCommandState("italic"),');
WYSIWYGFOOTER.Add('    result: () => exec("italic")');
WYSIWYGFOOTER.Add('  },');
WYSIWYGFOOTER.Add('');
WYSIWYGFOOTER.Add('underline: {');
WYSIWYGFOOTER.Add('    icon: "<u>U</u>",');
WYSIWYGFOOTER.Add('    title: "Underline",');
WYSIWYGFOOTER.Add('    state: () => queryCommandState("underline"),');
WYSIWYGFOOTER.Add('    result: () => exec("underline")');
WYSIWYGFOOTER.Add('  },');
WYSIWYGFOOTER.Add('');
WYSIWYGFOOTER.Add('strikethrough: {');
WYSIWYGFOOTER.Add('    icon: "<strike>S</strike>",');
WYSIWYGFOOTER.Add('    title: "Strike-through",');
WYSIWYGFOOTER.Add('    state: () => queryCommandState("strikeThrough"),');
WYSIWYGFOOTER.Add('    result: () => exec("strikeThrough")');
WYSIWYGFOOTER.Add('  },');
WYSIWYGFOOTER.Add('');
WYSIWYGFOOTER.Add(' Superscript: {');
WYSIWYGFOOTER.Add('      icon: "<b><sup>s</sup></b>",');
WYSIWYGFOOTER.Add('      title: "superscript",');
WYSIWYGFOOTER.Add('      result: () => exec("superscript")');
WYSIWYGFOOTER.Add('  },');
WYSIWYGFOOTER.Add('');
//WYSIWYGFOOTER.Add('heading1: {');
//WYSIWYGFOOTER.Add('    icon: "<b>H<sub>1</sub></b>",');
//WYSIWYGFOOTER.Add('    title: "Heading 1",');
//WYSIWYGFOOTER.Add('    result: () => exec(formatBlock, "<h1>")');
//WYSIWYGFOOTER.Add('  },');
//WYSIWYGFOOTER.Add('');
//WYSIWYGFOOTER.Add('heading2: {');
//WYSIWYGFOOTER.Add('    icon: "<b>H<sub>2</sub></b>",');
//WYSIWYGFOOTER.Add('    title: "Heading 2",');
//WYSIWYGFOOTER.Add('    result: () => exec(formatBlock, "<h2>")');
//WYSIWYGFOOTER.Add('  },');
//WYSIWYGFOOTER.Add('');
//WYSIWYGFOOTER.Add('heading3: {');
//WYSIWYGFOOTER.Add('    icon: "<b>H<sub>3</sub></b>",');
//WYSIWYGFOOTER.Add('    title: "Heading 3",');
//WYSIWYGFOOTER.Add('    result: () => exec(formatBlock, "<h3>")');
//WYSIWYGFOOTER.Add('  },');
//WYSIWYGFOOTER.Add('');
//WYSIWYGFOOTER.Add('paragraph: {');
//WYSIWYGFOOTER.Add('    icon: "&#182;",');
//WYSIWYGFOOTER.Add('    title: "Paragraph",');
//WYSIWYGFOOTER.Add('    result: () => exec(formatBlock, "<p>")');
//WYSIWYGFOOTER.Add('  },');
//WYSIWYGFOOTER.Add('');

//WYSIWYGFOOTER.Add('    result: () => document.execCommand("fontSize",false, "110%")');
WYSIWYGFOOTER.Add('small: {');
WYSIWYGFOOTER.Add('    icon: "S",');
WYSIWYGFOOTER.Add('    title: "Small font",');
WYSIWYGFOOTER.Add('    result: () => document.execCommand("fontSize",false, "1")');
WYSIWYGFOOTER.Add('  },');
WYSIWYGFOOTER.Add('');
WYSIWYGFOOTER.Add('medium: {');
WYSIWYGFOOTER.Add('    icon: "M",');
WYSIWYGFOOTER.Add('    title: "medium font",');
WYSIWYGFOOTER.Add('    result: () => document.execCommand("fontSize",false, "3")');
WYSIWYGFOOTER.Add('  },');
WYSIWYGFOOTER.Add('');
WYSIWYGFOOTER.Add('large: {');
WYSIWYGFOOTER.Add('    icon: "L",');
WYSIWYGFOOTER.Add('    title: "large font",');
WYSIWYGFOOTER.Add('    result: () => document.execCommand("fontSize",false, "5")');
WYSIWYGFOOTER.Add('  },');
WYSIWYGFOOTER.Add('');
WYSIWYGFOOTER.Add('xlarge: {');
WYSIWYGFOOTER.Add('    icon: "XL",');
WYSIWYGFOOTER.Add('    title: "xlarge font",');
WYSIWYGFOOTER.Add('    result: () => document.execCommand("fontSize",false, "7")');
WYSIWYGFOOTER.Add('  },');
WYSIWYGFOOTER.Add('');



WYSIWYGFOOTER.Add('quote: {');
WYSIWYGFOOTER.Add('    icon: "&#8220; &#8221;",');
WYSIWYGFOOTER.Add('    title: "Quote (This indents the text)",');
WYSIWYGFOOTER.Add('    result: () => exec(formatBlock, "<blockquote>")');
WYSIWYGFOOTER.Add('  },');
WYSIWYGFOOTER.Add('');
WYSIWYGFOOTER.Add('centre: {');
WYSIWYGFOOTER.Add('    icon: "<b>c</b>",');
WYSIWYGFOOTER.Add('    title: "Centre the selected text",');
WYSIWYGFOOTER.Add('    result: () => document.execCommand("justifycenter")');
WYSIWYGFOOTER.Add('  },');
WYSIWYGFOOTER.Add('');
WYSIWYGFOOTER.Add('olist: {');
WYSIWYGFOOTER.Add('    icon: "&#35;",');
WYSIWYGFOOTER.Add('    title: "Ordered List",');
WYSIWYGFOOTER.Add('    result: () => exec("insertOrderedList")');
WYSIWYGFOOTER.Add('  },');
WYSIWYGFOOTER.Add('');
WYSIWYGFOOTER.Add('ulist: {');
WYSIWYGFOOTER.Add('    icon: "&#8226;",');
WYSIWYGFOOTER.Add('    title: "Unordered List",');
WYSIWYGFOOTER.Add('    result: () => exec("insertUnorderedList")');
WYSIWYGFOOTER.Add('  },');
WYSIWYGFOOTER.Add('');
WYSIWYGFOOTER.Add('line: {');
WYSIWYGFOOTER.Add('    icon: "&#8213;",');
WYSIWYGFOOTER.Add('    title: "Horizontal Line",');
WYSIWYGFOOTER.Add('    result: () => exec("insertHorizontalRule")');
WYSIWYGFOOTER.Add('  },');
WYSIWYGFOOTER.Add('');
WYSIWYGFOOTER.Add('red: {');
WYSIWYGFOOTER.Add('    icon: "<b>r</b>",');
WYSIWYGFOOTER.Add('    title: "Red",');
WYSIWYGFOOTER.Add('    BackgroundColor: "#FF5050",');
WYSIWYGFOOTER.Add('    // I have made this more of a pink to help people');
WYSIWYGFOOTER.Add('    // who are red green colour blind to tell the difference');
WYSIWYGFOOTER.Add('    result: () => document.execCommand("foreColor",false, "#FF5050")');
WYSIWYGFOOTER.Add('  },');
WYSIWYGFOOTER.Add('');
WYSIWYGFOOTER.Add('green: {');
WYSIWYGFOOTER.Add('    icon: "<b>g</b>",');
WYSIWYGFOOTER.Add('    title: "Green",');
WYSIWYGFOOTER.Add('    BackgroundColor: "#00FF00",');
WYSIWYGFOOTER.Add('    result: () => document.execCommand("foreColor",false, "#00FF00")');
WYSIWYGFOOTER.Add('  },');
WYSIWYGFOOTER.Add('');
WYSIWYGFOOTER.Add('blue: {');
WYSIWYGFOOTER.Add('      icon: "<b>b</b>",');
WYSIWYGFOOTER.Add('      title: "Blue",');
WYSIWYGFOOTER.Add('      BackgroundColor: "#0000FF",');
WYSIWYGFOOTER.Add('      result: () =>document.execCommand("foreColor",false, "#0000FF")');
WYSIWYGFOOTER.Add('  },');
WYSIWYGFOOTER.Add('');
WYSIWYGFOOTER.Add('black: {');
WYSIWYGFOOTER.Add('      icon: "<b>b</b>",');
WYSIWYGFOOTER.Add('      title: "Black",');
WYSIWYGFOOTER.Add('      BackgroundColor: "#000000",');
WYSIWYGFOOTER.Add('      Color: "#FFFFFF",');
WYSIWYGFOOTER.Add('      result: () =>document.execCommand("foreColor",false, "#000000")');
WYSIWYGFOOTER.Add('  },');
WYSIWYGFOOTER.Add('');
WYSIWYGFOOTER.Add('link: {');
WYSIWYGFOOTER.Add('    icon: "&#128279;",');
WYSIWYGFOOTER.Add('    title: "Link",');
WYSIWYGFOOTER.Add('    result: () => {');
WYSIWYGFOOTER.Add('      const url = window.prompt("Enter the link URL")');
WYSIWYGFOOTER.Add('      if (url) exec("createLink", url)');
WYSIWYGFOOTER.Add('    }');
WYSIWYGFOOTER.Add('  },');
WYSIWYGFOOTER.Add('');
WYSIWYGFOOTER.Add('anchor: {');
WYSIWYGFOOTER.Add('    icon: "<b>a</b>",');
WYSIWYGFOOTER.Add('    title: "Anchor (link using #name)",');
WYSIWYGFOOTER.Add('    result: () => {');
WYSIWYGFOOTER.Add('      const aname = window.prompt("Enter the anchor name");');
WYSIWYGFOOTER.Add('      if (aname) {');
WYSIWYGFOOTER.Add('        exec("insertHTML","<a id="+aname+">"+aname+"</a>"); ');
WYSIWYGFOOTER.Add('        }');
WYSIWYGFOOTER.Add('      }');
WYSIWYGFOOTER.Add('  },');
WYSIWYGFOOTER.Add('');
WYSIWYGFOOTER.Add('Undo: {');
WYSIWYGFOOTER.Add('    icon: "<b>Undo</b>",');
WYSIWYGFOOTER.Add('    title: "Undo",');
WYSIWYGFOOTER.Add('    result: () => exec("undo")');
WYSIWYGFOOTER.Add('  },');
WYSIWYGFOOTER.Add('');
WYSIWYGFOOTER.Add('save: {');
WYSIWYGFOOTER.Add('    icon: "<b>Commit</b>",');
WYSIWYGFOOTER.Add('    title: "Save",');
WYSIWYGFOOTER.Add('    result: () => {  ');
WYSIWYGFOOTER.Add('                  var theText = document.getElementById("my_wysiwyg_editor").innerHTML;    ');
WYSIWYGFOOTER.Add('                  var savedtext = "TXHTMLEditor ' + self.myNode.NodeName + 'Z!Z!Z"+ theText+"Z!Z!Z";');
{$ifndef JScript}
  {$ifdef Chromium}
  if self.IsEmbedded then
  begin
    // cef.  Change the document title to trigger a cef titlechange event...
    WYSIWYGFOOTER.Add('                updateCounter = updateCounter+1;    ');
    WYSIWYGFOOTER.Add('                document.title = "'+myNode.NodeName+' "+updateCounter; ;');
  end
  else
    // Windows, with external browser page
    //!! using polling to fetch title
    WYSIWYGFOOTER.Add('                document.title =savedtext; checkTitle(); ');
  {$else}
  // Windows, with external browser page
  //!!!! using polling to fetch title
  WYSIWYGFOOTER.Add('                  document.title =savedtext; checkTitle();' );
  {$endif}
{$else}
if self.IsEmbedded then
begin
  // JS. with embedded iframe.
  WYSIWYGFOOTER.Add('                if (parent!=null) {parent.postMessage({"objid":"'+self.myNode.Nodename+'", "NameSpace":"'+self.myNode.NameSpace+'", "mtype":"titleChange", "mdata":savedtext},"*")}' );
end
else
  // JS. with external browser page
  WYSIWYGFOOTER.Add('                if (window.opener!=null) {window.opener.postMessage({"objid":"'+self.myNode.Nodename+'", "NameSpace":"'+self.myNode.NameSpace+'", "mtype":"titleChange", "mdata":savedtext},"*")} ' );
{$endif}
WYSIWYGFOOTER.Add('                  },');
WYSIWYGFOOTER.Add('      },');
WYSIWYGFOOTER.Add('');
if self.IsEmbedded = false then
begin
  // Done is only shown in an external browser frame.
  WYSIWYGFOOTER.Add('Done: {');
  WYSIWYGFOOTER.Add('    icon: "<b>Done</b>",');
  WYSIWYGFOOTER.Add('    title: "Save and exit",');
  WYSIWYGFOOTER.Add('    result: () => {  ');
  WYSIWYGFOOTER.Add('                  var savedtext = "TXHTMLEditor ' + self.myNode.NodeName + 'Z!Z!Z"+ document.getElementById("my_wysiwyg_editor").innerHTML+"Z!Z!Z";');
  WYSIWYGFOOTER.Add('                  document.title =savedtext; var ok=checkTitle(); ');
  WYSIWYGFOOTER.Add('                  if (ok) { ');
  WYSIWYGFOOTER.Add('                    if (window.opener!=null) {window.opener.postMessage({"objid":"'+self.myNode.Nodename+'", "NameSpace":"'+self.myNode.NameSpace+'", "mtype":"titleChange", "mdata":savedtext},"*");}' );
  WYSIWYGFOOTER.Add('                    setTimeout(function(){stopediting(); }, 600);} },');
  WYSIWYGFOOTER.Add('  },');
  WYSIWYGFOOTER.Add('');

  WYSIWYGFOOTER.Add('Quit: {');
  WYSIWYGFOOTER.Add('    icon: "<b>Quit</b>",');
  WYSIWYGFOOTER.Add('    title: "Exit without saving",');
  WYSIWYGFOOTER.Add('    result: () => window.close()');
  WYSIWYGFOOTER.Add('  },');
  WYSIWYGFOOTER.Add('');
end;
WYSIWYGFOOTER.Add('//code: {');
WYSIWYGFOOTER.Add('//    icon: "&lt;/&gt;",');
WYSIWYGFOOTER.Add('//    title: "Code",');
WYSIWYGFOOTER.Add('//    result: () => exec(formatBlock, "<pre>")');
WYSIWYGFOOTER.Add('//  },');
WYSIWYGFOOTER.Add('');
WYSIWYGFOOTER.Add('//image: {');
WYSIWYGFOOTER.Add('//    icon: "&#128247;",');
WYSIWYGFOOTER.Add('//    title: "Image",');
WYSIWYGFOOTER.Add('//    result: () => {');
WYSIWYGFOOTER.Add('//      const url = window.prompt("Enter the image URL")');
WYSIWYGFOOTER.Add('//      if (url) exec("insertImage", url)');
WYSIWYGFOOTER.Add('//    }');
WYSIWYGFOOTER.Add('//  }');
WYSIWYGFOOTER.Add('}');
WYSIWYGFOOTER.Add('const defaultClasses = {');
WYSIWYGFOOTER.Add('  actionbar: "wysiwyg-actionbar",');
WYSIWYGFOOTER.Add('  button: "wysiwyg-button",');
WYSIWYGFOOTER.Add('  content: "wysiwyg-content",');
WYSIWYGFOOTER.Add('  selected: "wysiwyg-button-selected"');
WYSIWYGFOOTER.Add('}');
WYSIWYGFOOTER.Add('//export');
WYSIWYGFOOTER.Add('const init = settings => {');
WYSIWYGFOOTER.Add('	const actions = settings.actions');
WYSIWYGFOOTER.Add('    ? (');
WYSIWYGFOOTER.Add('      settings.actions.map(action => {');
WYSIWYGFOOTER.Add('        if (typeof action === "string") return defaultActions[action]');
WYSIWYGFOOTER.Add('        else if (defaultActions[action.name]) return { ...defaultActions[action.name], ...action }');
WYSIWYGFOOTER.Add('        return action');
WYSIWYGFOOTER.Add('      })');
WYSIWYGFOOTER.Add('    )');
WYSIWYGFOOTER.Add('    : Object.keys(defaultActions).map(action => defaultActions[action])');
WYSIWYGFOOTER.Add('	const classes = { ...defaultClasses, ...settings.classes }');
WYSIWYGFOOTER.Add('	const defaultParagraphSeparator = settings[defaultParagraphSeparatorString] || "div"');

if IsEditable = true
then
begin
  WYSIWYGFOOTER.Add('	const actionbar = createElement("div")');
  WYSIWYGFOOTER.Add('	actionbar.style.border = "solid"');
  WYSIWYGFOOTER.Add('	actionbar.style.borderWidth="thin"');
  WYSIWYGFOOTER.Add('	actionbar.style.width = "100%"');
  WYSIWYGFOOTER.Add('    actionbar.className = classes.actionbar');
  WYSIWYGFOOTER.Add('    // detect IE8 and above, and edge');
  WYSIWYGFOOTER.Add('    if (document.documentMode || /Edge/.test(navigator.userAgent)) {');
  WYSIWYGFOOTER.Add('      appendChild(document.getElementById("MyActionBar"), actionbar)');
  WYSIWYGFOOTER.Add('    }');
  WYSIWYGFOOTER.Add('    else');
  WYSIWYGFOOTER.Add('    {');
  WYSIWYGFOOTER.Add('      var referenceNode = document.getElementById("editor");');
  WYSIWYGFOOTER.Add('      referenceNode.before(actionbar);');
  WYSIWYGFOOTER.Add('    }');
end;

WYSIWYGFOOTER.Add('	const content = settings.element.content = createElement("div")');
WYSIWYGFOOTER.Add('	content.contentEditable = false');
WYSIWYGFOOTER.Add('	content.className = classes.content');
WYSIWYGFOOTER.Add('	content.oninput = ({ target: { firstChild } }) => {');
WYSIWYGFOOTER.Add('    if (firstChild && firstChild.nodeType === 3) exec(formatBlock, `<${defaultParagraphSeparator}>`)');
WYSIWYGFOOTER.Add('    else if (content.innerHTML === "<br>") content.innerHTML = ""');
WYSIWYGFOOTER.Add('    settings.onChange(content.innerHTML)');
WYSIWYGFOOTER.Add('  }');
WYSIWYGFOOTER.Add('	content.onkeydown = event => {');
WYSIWYGFOOTER.Add('    if (event.key === "Enter" && queryCommandValue(formatBlock) === "blockquote") {');
WYSIWYGFOOTER.Add('      setTimeout(() => exec(formatBlock, `<${defaultParagraphSeparator}>`), 0)');
WYSIWYGFOOTER.Add('    }');
WYSIWYGFOOTER.Add('  }');
WYSIWYGFOOTER.Add('	appendChild(settings.element, content)');
WYSIWYGFOOTER.Add('	actions.forEach(action => {');
WYSIWYGFOOTER.Add('    const button = createElement("button")');
WYSIWYGFOOTER.Add('    button.className = classes.button');
WYSIWYGFOOTER.Add('    button.innerHTML = action.icon');
WYSIWYGFOOTER.Add('    button.title = action.title');
WYSIWYGFOOTER.Add('    button.style.backgroundColor = action.BackgroundColor');
WYSIWYGFOOTER.Add('    button.setAttribute("type", "button")');
WYSIWYGFOOTER.Add('    button.onclick = () => action.result() && content.focus()');
WYSIWYGFOOTER.Add('    if (action.state) {');
WYSIWYGFOOTER.Add('      const handler = () => button.classList[action.state() ? "add" : "remove"](classes.selected)');
WYSIWYGFOOTER.Add('      addEventListener(content, "keyup", handler)');
WYSIWYGFOOTER.Add('      addEventListener(content, "mouseup", handler)');
WYSIWYGFOOTER.Add('      addEventListener(button, "click", handler)');
WYSIWYGFOOTER.Add('    }');
WYSIWYGFOOTER.Add('    appendChild(actionbar, button)');
WYSIWYGFOOTER.Add('  })');
WYSIWYGFOOTER.Add('  if (settings.styleWithCSS) exec("styleWithCSS")');
WYSIWYGFOOTER.Add('  exec(defaultParagraphSeparatorString, defaultParagraphSeparator)');
WYSIWYGFOOTER.Add('  return settings.element');
WYSIWYGFOOTER.Add('}');
WYSIWYGFOOTER.Add('//export');
WYSIWYGFOOTER.Add('//default');
WYSIWYGFOOTER.Add('// { exec, init }');
WYSIWYGFOOTER.Add('</script>');
WYSIWYGFOOTER.Add('<script>');
WYSIWYGFOOTER.Add('      var editor = init({');
WYSIWYGFOOTER.Add('        element: document.getElementById("editor"),');
WYSIWYGFOOTER.Add('        defaultParagraphSeparator: "p",');
WYSIWYGFOOTER.Add('        onChange: function (html) {');
WYSIWYGFOOTER.Add('        }');
WYSIWYGFOOTER.Add('      })');
WYSIWYGFOOTER.Add('</script>');
WYSIWYGFOOTER.Add('</body>');
WYSIWYGFOOTER.Add('</html>');

OutputStringList.AddStrings(WYSIWYGHEADER  );
OutputStringList.Add( HelpText.Text );               //in <div wysiwyg-content...
OutputStringList.AddStrings( WYSIWYGFOOTER );

 result:= OutputStringList;

WYSIWYGHEADER.Free;
WYSIWYGFOOTER.Free;
HelpText.Free;

end;

procedure dotest;
begin
end;

begin
  // this is the set of node attributes that each GPUCanvas instance will have (added to the set inherited from TXIFrame).
  AddWrapperDefaultAttribs(myDefaultAttribs);
  AddDefaultAttribute(myDefaultAttribs,'SuspendRefresh','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'ActualHeight','Integer','','',true,false);
  AddDefaultAttribute(myDefaultAttribs,'ActualWidth','Integer','','',true,false);
  AddDefaultAttribute(myDefaultAttribs,'FrameWidth','String','350','',false);
  AddDefaultAttribute(myDefaultAttribs,'FrameHeight','String','300','',false);
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','Top','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelText','String','HTML Editor','',false);
  AddDefaultAttribute(myDefaultAttribs,'HTMLSource','String','','',false,false);
  {$ifdef Chromium}
  AddDefaultAttribute(myDefaultAttribs,'IsEmbedded','Boolean','True','Display the text in an embedded IFrame (needs CEF4)',false);
  {$else}
  AddDefaultAttribute(myDefaultAttribs,'IsEmbedded','Boolean','False','Display the text in an embedded IFrame (needs CEF4)',false);
  {$endif}
  AddDefaultAttribute(myDefaultAttribs,'IsEditable','Boolean','True','Allow the text page to be edited',false);
  AddDefaultAttribute(myDefaultAttribs,'Showing','Boolean','False','When not embedded, set this to display the text in a standalone browser page',false,false);
  AddDefaultAttribute(myDefaultAttribs,'SourceText','String','...text...','',false);
  AddDefaultAttribute(myDefaultAttribs,'HeaderHTML','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'FooterHTML','String','','',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  AddAttribOptions(MyNodeType,'LabelPos',LabelPosOptions);
  {$IFndef JScript}
  Classes.RegisterClass(TXHTMLEditor);
  AddNodeFuncLookup(MyNodeType,@CreateHTMLEditorWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateInterfaceObj,@CreateHTMLEditorWidget);
  {$endif}
  SuppressDesignerProperty(MyNodeType,'ContainerHeight');
  SuppressDesignerProperty(MyNodeType,'ContainerWidth');
  SuppressDesignerProperty(MyNodeType,'SuspendRefresh');
  SuppressDesignerProperty(MyNodeType,'BgColor');
  SuppressDesignerProperty(MyNodeType,'HTMLSource');
end.

