(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

 This component depends on the Lazarus package FrameViewer09 to
 provide the TFrameviewer widget in the desktop environment.

 Downloadable from https://github.com/BerndGabriel/HtmlViewer
 The original Delphi code on which this port is based has been released by
 Dave Baldwin into the public domain. Additional code supplied with this port
 is released under the MPL 1.1 license.
 Also see https://wiki.lazarus.freepascal.org/THtmlPort
 *)
unit XHTMLText;

{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
    Classes, SysUtils, TypInfo, StringUtils, NodeUtils, XIFrame,
    UtilsJSCompile, XForm, XCode, XButton, XVBox, XTabControl, XMemo, EventsInterface,
    {$ifndef JScript}
    LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, Propedits, RTTICtrls,
    LazsUtils, LCLIntf, LazHelpHTML,
    {$if defined ( windows)}
    UTF8Process,
    {$endif}
    framView,
    LCLType, gettext,
  {$else}
  HTMLUtils,
  {$endif}
  WrapperPanel, Events;

{$ifndef JScript}
procedure Register;
{$endif}

type
  {$ifndef JScript}
  TXHTMLText = class(TWrapperPanel)
  {$else}
  TXHTMLText = class(TXIframe)
  {$endif}
  private
    {$ifndef JScript}
    myReloadTimer:TTimer;
    myTimerCount:integer;
    fHandleButtonClick:TEventHandler;
    procedure DoReloadTimerThing(sender:TObject);
    function GetFrameWidth:string;
    function GetFrameHeight:string;
    function GetHTMLSource:string;
    procedure SetFrameWidth(AValue:string);
    procedure SetFrameHeight(AValue:string);
    procedure SetHTMLSource(AValue:string);
    procedure HandleClick(Sender:TObject);
    procedure HandleHotSpotClick(Sender:TObject; const Target:UnicodeString; const AnURL:UnicodeString; var Handled:boolean);
    procedure ShowHTML(Src: string);
    procedure WinLaunchBrowser(URL:String; var Handled:Boolean);
    {$endif}

    function GetSourceText:String;
    function GetActualWidth:integer;
    function GetActualHeight:integer;

    procedure SetSourceText(AValue:String);
    procedure SetBgColor(AValue:TColor); override;

  protected

  public
    {$ifndef JScript}
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent;IsDynamic:Boolean); override;
    procedure DoHTMLTextConstructor(TheOwner:TComponent;IsDynamic:Boolean);
    {$else}    // JScript
    constructor Create(MyForm:TForm;NodeName,NameSpace:String);  override;
    {$endif}
    function CreateTextURL(txt:String):tstringlist;
    procedure SetMyEventTypes;


  published
    property SourceText: String read GetSourceText write SetSourceText;
    property ActualHeight:integer read GetActualHeight;
    property ActualWidth:integer read GetActualWidth;

    {$ifndef JScript}
    property FrameHeight: String read GetFrameHeight write SetFrameHeight;
    property FrameWidth: String read GetFrameWidth write SetFrameWidth;
    property HTMLSource: String read GetHTMLSource write SetHTMLSource;
    // Events to be visible in Lazarus IDE
    property HandleButtonClick: TEventHandler read FHandleButtonClick write FHandleButtonClick;
    {$endif}
  end;

  {$ifdef JScript}
  procedure ResetHTMLText(nm,ns:String);
  {$endif}

implementation

const MyNodeType='TXHTMLText';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXHTMLText.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
//  MyEventTypes.Add('HTMLTextBrowserClosed');
end;

{$ifndef JScript}
procedure Register;
begin
  {$I Icons/XHTMLText.lrs}
  RegisterComponents('XComponents',[TXHTMLText]);
end;

function CreateHTMLTextWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXHTMLText',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;

procedure TXHTMLText.DoHTMLTextConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin

  self.BorderSpacing.Around:=glbBorderSpacing;

  myControl:=TFrameViewer.Create(self);
  myControl.Parent:=self;

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];

  self.myReloadTimer:=TTimer.Create(self);
  myReloadTimer.Enabled:=false;
  myReloadTimer.Interval:=200;
  myReloadTimer.OnTimer:=@self.DoReloadTimerThing;
  myTimerCount:=0;

  TFrameViewer(myControl).OnHotSpotTargetClick:=@self.HandleHotSpotClick;
  TFrameViewer(myControl).OnClick:=@self.HandleClick;
  TFrameViewer(myControl).DefBackground:=clWhite;

  self.SetMyEventTypes;
  CreateComponentDataNode2(self,MyNodeType,myDefaultAttribs, self.myEventTypes, TheOwner,IsDynamic);

  self.IsContainer:=false;

  AddLabel(myControl);

end;

constructor TXHTMLText.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoHTMLTextConstructor(TheOwner,false);
end;

constructor TXHTMLText.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoHTMLTextConstructor(TheOwner,IsDynamic);
end;

procedure TXHTMLText.HandleClick(Sender:TObject);
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;

procedure TXHTMLText.WinLaunchBrowser(URL:String; var Handled:Boolean);
var
  v: THTMLBrowserHelpViewer;
  BrowserPath, BrowserParams: string;
  p: LongInt;
  BrowserProcess: TProcessUTF8;
begin
  Handled:=false;
  // procedure to open a fresh browser instance on the desktop, and load the URL
  //!! if 2 of these in quick succession, we still get one browser instance with 2 tabs...
  {$if defined ( windows)}
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
      BrowserProcess.CommandLine:=BrowserPath+BrowserParams;
      BrowserProcess.Execute;
      Handled:=true;
    finally
      BrowserProcess.Free;
    end;
  finally
    v.Free;
  end;
  {$endif}
end;

procedure TXHTMLText.HandleHotSpotClick(Sender:TObject;  const Target:UnicodeString; const AnURL:UnicodeString; var Handled:boolean);
begin
  // expecting this is a URL link to web page...
  // pop up a separate browser (works for windows...)
  if ((FoundString(AnURL,'://') > 0) and (FoundString(AnURL,'http') = 1)) then
    WinLaunchBrowser(AnURL,Handled);
  if not Handled then
    TFrameViewer(self.myControl).HotSpotClick(Sender,AnURL,Handled);
end;

procedure TXHTMLText.DoReloadTimerThing(sender:TObject);
begin
  myReloadTimer.Enabled := false;
  myTimerCount:=myTimerCount+1;
  if (not (csDesigning in componentState))
  and (not (csLoading in componentState))
  and (myTimerCount=1) then
    begin
        self.SourceText:=self.SourceText;    // html reload
    end;
end;

function TXHTMLText.GetFrameHeight:string;
begin
  result:=MyNode.getAttribute('FrameHeight',true).AttribValue;
end;
function TXHTMLText.GetFrameWidth:string;
begin
  result:=MyNode.getAttribute('FrameWidth',true).AttribValue;
end;
function TXHTMLText.GetHTMLSource:String;
begin
  result:=MyNode.getAttribute('HTMLSource',true).AttribValue;
end;
procedure TXHTMLText.SetFrameWidth(AValue:string);
var
  tc:TControl;
begin
  myNode.SetAttributeValue('FrameWidth',AValue);
  tc:=self.myControl;
  SetHeightWidth(self.myNode,tc,'FrameWidth','FrameHeight');
end;

procedure TXHTMLText.SetFrameHeight(AValue:string);
var
  tc:TControl;
begin
  myNode.SetAttributeValue('FrameHeight',AValue);
  tc:=self.myControl;
  SetHeightWidth(self.myNode,tc,'FrameWidth','FrameHeight');
end;

procedure TXHTMLText.SetHTMLSource(AValue:string);
begin
  //if AValue<>myNode.GetAttribute('HTMLSource',true).AttribValue then
  begin
    myNode.SetAttributeValue('HTMLSource',AValue);
    self.ShowHTML(AValue);
  end;
end;

{$else} //JScript  ...............................

constructor TXHTMLText.Create(MyForm:TForm;NodeName,NameSpace:String);
begin
  inherited Create(MyForm,NodeName,NameSpace);
  self.NodeType:='TXHTMLText';
  self.IsContainer:=false;

  SetNodePropDefaults(self,myDefaultAttribs);
end;

function CreateHTMLTextWidget(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewWidget:TXHTMLText;
  h,w:integer;
begin
//showmessage('TXHTMLText CreateWidget. '+MyNode.NodeName);
  DoCreateFrameWidget(MyNode, ParentNode,ScreenObjectName,position);

  NewWidget:=TXHTMLText(myNode);

  // refresh the actual h/w attributes
  h:=NewWidget.ActualHeight;
  w:=NewWidget.ActualWidth;

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXHTMLText.Create(MyForm,NodeName,NameSpace));
end;

{$endif}

function TXHTMLText.GetSourceText:String;
begin
  result:=myNode.getAttribute('SourceText',true).AttribValue;
end;

procedure TXHTMLText.SetSourceText(AValue:String);
Var
  URLStringList:TStringList;
begin
  if myNode<>nil then
    myNode.SetAttributeValue('SourceText',AValue,'String');
  {$ifndef JScript}
  if (not (csDesigning in componentState))
  and (not (csLoading in componentState)) then
  {$endif}
  if myNode<>nil then
  begin
    URLStringList:=CreateTextURL(AValue);
    self.HTMLSource:=URLStringList.Text;
    URLStringList.Free;

    {$ifndef JScript}
    // ReLoad the HTML (later, to ensure the frame has been rendered first)
    if mytimerCount=0 then
      self.myReloadTimer.Enabled := True
    else
      myTimerCount:=0;
    {$endif}

  end;
end;

function TXHTMLText.CreateTextURL(txt:String):tstringlist;
var WYSIWYGHEADER,WYSIWYGFOOTER,TheText,OutputStringList: TStringList;
  startstring, endstring:String;
  BgCol:String;
begin
  BgCol:=MyNode.GetAttribute('BgColor',true).AttribValue;

WYSIWYGHEADER:= TStringList.Create;
WYSIWYGFOOTER:= TStringList.Create;
TheText:= TStringList.Create;
OutputStringList:= TStringList.Create;

//Initalise the the header, footer and text strings
startstring:='<div contenteditable="false" >';
endstring := '</div>' ;

TheText.Add(startstring);
TheText.Add(txt);
TheText.Add(endstring );

{$ifdef JScript}
WYSIWYGHEADER.Add('<!DOCTYPE html>');
{$endif}
WYSIWYGHEADER.Add('<html>');
WYSIWYGHEADER.Add('<head>');
WYSIWYGHEADER.Add('');
WYSIWYGHEADER.Add('<title>TXHTMLText '+self.myNode.NodeName+'</title>');
WYSIWYGHEADER.Add('</head>');

WYSIWYGHEADER.Add('<body style="margin:0px; background-color:'+BgCol+'">');
WYSIWYGHEADER.Add('');
WYSIWYGHEADER.Add('<div id="FrameContent" ');
WYSIWYGHEADER.Add(      'style="overflow:scroll; height:'+inttostr(self.actualHeight)+'px; ">');
WYSIWYGHEADER.Add('      <div id="thetext" style="height: 100%; width: 100%; ">');

WYSIWYGFOOTER.Add('      </div>');
WYSIWYGFOOTER.Add('    </div>');
WYSIWYGFOOTER.Add('</body>');
WYSIWYGFOOTER.Add('</html>');

OutputStringList.AddStrings(WYSIWYGHEADER  );
OutputStringList.AddStrings( TheText );
OutputStringList.AddStrings( WYSIWYGFOOTER );

 result:= OutputStringList;

WYSIWYGHEADER.Free;
WYSIWYGFOOTER.Free;
TheText.Free;

end;

{$ifndef JScript}
procedure TXHTMLText.ShowHTML(Src: string);
var
  mc:TWinControl;
  hnd:Boolean;
  nm:string;
begin
  if (not (csDesigning in componentState))
  and (not (csLoading in componentState))
  then
  begin
    nm:=self.myNode.NameSpace;
    if nm<>'' then nm:=nm+'_';
    nm:='tempinc/'+nm+self.myNode.NodeName;
    WriteToFile(ProjectDirectory+nm+'.html',Src);
    mc:=TWinControl(myControl);
    hnd:=mc.HandleAllocated;
    if (hnd) then
      TFrameViewer(myControl).Load(ProjectDirectory+nm+'.html');
  end;
end;
{$endif}

function TXHTMLText.GetActualHeight:integer;
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
function TXHTMLText.GetActualWidth:integer;
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

procedure TXHTMLText.SetBgColor(AValue:TColor);
var tmp:string;
begin
  inherited SetBgColor(AValue);

  {$ifndef JScript}
  TFrameViewer(myControl).DefBackground:=AValue;
  {$else}
  tmp:=MyNode.GetAttribute('BgColor',true).AttribValue;
  asm console.log('set color '+AValue+'   new color is '+tmp); end;
  {$endif}
end;

{$ifdef JScript}
procedure ResetHTMLText(nm,ns:String);
var
  thisNode:TDataNode;
begin
  thisnode:=FindDataNodeById(SystemNodeTree,nm,ns,true);
  TXHTMLText(thisNode.ScreenObject).SourceText:=TXHTMLText(thisNode.ScreenObject).SourceText;
end;
{$endif}

begin
  // this is the set of node attributes that each GPUCanvas instance will have (added to the set inherited from TXIFrame or TWrapperPanel).
  AddWrapperDefaultAttribs(myDefaultAttribs);
  AddDefaultAttribute(myDefaultAttribs,'FrameWidth','String','300','',false);
  AddDefaultAttribute(myDefaultAttribs,'FrameHeight','String','300','',false);
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'BgColor','Color','#FFFFFF','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','Top','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelText','String','HTML Text','',false);
  AddDefaultAttribute(myDefaultAttribs,'HTMLSource','String','','',false,false);
  AddDefaultAttribute(myDefaultAttribs,'Showing','Boolean','False','When not embedded, set this to display the text in a standalone browser page',false,false);
  AddDefaultAttribute(myDefaultAttribs,'SourceText','String','...text...','Provide the inner HTML including text to be rendered',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  AddAttribOptions(MyNodeType,'LabelPos',LabelPosOptions);
  {$IFndef JScript}
  Classes.RegisterClass(TXHTMLText);
  AddNodeFuncLookup(MyNodeType,@CreateHTMLTextWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateInterfaceObj,@CreateHTMLTextWidget);
  {$endif}
  SuppressDesignerProperty(MyNodeType,'ContainerHeight');
  SuppressDesignerProperty(MyNodeType,'ContainerWidth');
  SuppressDesignerProperty(MyNodeType,'HTMLSource');

end.
