(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
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
    LazsUtils, LCLIntf, Ipfilebroker, IpHtml,IpMsg,
    LCLType, gettext,
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

type
  {$ifndef JScript}
  TXHTMLText = class(TWrapperPanel)
  {$else}
  TXHTMLText = class(TXIframe)
  {$endif}
  private
    {$ifndef JScript}
    myDataProvider: TIpHtmlDataProvider;
    function GetFrameWidth:string;
    function GetFrameHeight:string;
    function GetHTMLSource:string;
    procedure SetFrameWidth(AValue:string);
    procedure SetFrameHeight(AValue:string);
    procedure SetHTMLSource(AValue:string);
    procedure HandleClick(Sender:TObject);
    procedure ShowHTML(Src: string);
    {$endif}

    function GetIsEditable:Boolean;
    function GetSourceText:String;
    function GetHeaderHTML:String;
    function GetFooterHTML:String;

    procedure SetIsEditable(AValue:Boolean);
    procedure SetSourceText(AValue:String);
    procedure SetHeaderHTML(AValue:String);
    procedure SetFooterHTML(AValue:String);

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
    function ExtractTextFromTitle(message:String):String;
    procedure SetMyEventTypes;


  published
    property IsEditable: Boolean read GetIsEditable write SetIsEditable;
    property SourceText: String read GetSourceText write SetSourceText;
    property HeaderHTML: String read GetHeaderHTML write SetHeaderHTML;
    property FooterHTML: String read GetFooterHTML write SetFooterHTML;

    {$ifndef JScript}
    property FrameHeight: String read GetFrameHeight write SetFrameHeight;
    property FrameWidth: String read GetFrameWidth write SetFrameWidth;
    property HTMLSource: String read GetHTMLSource write SetHTMLSource;
    {$endif}
  end;

implementation

const MyNodeType='TXHTMLText';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXHTMLText.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
//  MyEventTypes.Add('HTMLTextBrowserClosed');
end;

function  TXHTMLText.ExtractTextFromTitle(message:String):String;
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

  myControl:=TIpHtmlPanel.Create(self);  //ipro panel
  myControl.Parent:=self;

  myDataProvider:=TIpHtmlDataProvider.Create(self);  //ipro html provider

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];

  TIpHtmlPanel(myControl).OnClick:=@self.HandleClick;
  TIpHtmlPanel(myControl).DataProvider:=myDataProvider;

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
  if AValue<>myNode.GetAttribute('HTMLSource',true).AttribValue then
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

procedure HandleTXHTMLMessage(msg:TXHTMLMessage);
var
  ItemNode:TdataNode;
  message:string;
begin
  if (msg.objid<>'') then
  begin
    //showmessage('HandleTXHTMLMessage: '+msg.objid+' '+msg.mtype);
     //this is a notification sent out from within a HTMLText frame.
     ItemNode:=findDataNodeById(systemnodetree,msg.objid,msg.NameSpace,false);
     if ItemNode<>nil then
     begin
        if msg.mtype='titleChange' then
        begin
           //showmessage('message is titleChange');
           message:=msg.mdata;
           if message<>'' then
           begin
             message:= TXHTMLText(ItemNode).ExtractTextFromTitle(message);
             // now save the help text to the SourceText property
             if (length(trim(message))>0) then // and (StartPos>0)
             begin
                TXHTMLText(ItemNode).SourceText:= message;
             end;
           end;
        end;
     end;
  end;
end;

{$endif}

function TXHTMLText.GetIsEditable:Boolean;
begin
  result:=myStrToBool(myNode.getAttribute('IsEditable',true).AttribValue);
end;
function TXHTMLText.GetSourceText:String;
begin
  result:=myNode.getAttribute('SourceText',true).AttribValue;
end;
function TXHTMLText.GetHeaderHTML:String;
begin
  result:=myNode.getAttribute('HeaderHTML',true).AttribValue;
end;
function TXHTMLText.GetFooterHTML:String;
begin
  result:=myNode.getAttribute('FooterHTML',true).AttribValue;
end;

procedure TXHTMLText.SetIsEditable(AValue:Boolean);
begin
  if myNode<>nil then
  begin
    myNode.SetAttributeValue('IsEditable',myBoolToStr(AValue),'Boolean');
  end;
end;

procedure TXHTMLText.SetSourceText(AValue:String);
Var
  URLStringList:TStringList;
begin
  if myNode<>nil then
  begin
    myNode.SetAttributeValue('SourceText',AValue,'String');
    begin
      URLStringList:=CreateTextURL(AValue);
      self.HTMLSource:=URLStringList.Text;
    end;
    URLStringList.Free;
  end;
end;

procedure TXHTMLText.SetHeaderHTML(AValue:String);
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

procedure TXHTMLText.SetFooterHTML(AValue:String);
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

function TXHTMLText.CreateTextURL(txt:String):tstringlist;
var WYSIWYGHEADER,WYSIWYGFOOTER,TheText,OutputStringList: TStringList;
  startstring, endstring:String;
  InnerStartLength,InnerEndLength:integer;
  ActionBarClass:String;
begin

WYSIWYGHEADER:= TStringList.Create;
WYSIWYGFOOTER:= TStringList.Create;
TheText:= TStringList.Create;
OutputStringList:= TStringList.Create;

//Initalise the the header, footer and text strings
startstring:='<div contenteditable="false" class="wysiwyg-content">';
endstring := '</div>' ;

InnerStartLength:=length(startstring);
InnerEndLength:=length(endstring );

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

WYSIWYGHEADER.Add('<body>');
WYSIWYGHEADER.Add('');
WYSIWYGHEADER.Add('<div id="FrameContent" class="content" style="background-color:powderblue; ">');
WYSIWYGHEADER.Add(myNode.GetAttribute('HeaderHTML',false).AttribValue);
WYSIWYGHEADER.Add('      <div id="thetext" class="wysiwyg" style="height: 100%; width: 100%; background-color:white; border-style: solid;border-width:thin;">');

WYSIWYGFOOTER.Add('      </div>');
WYSIWYGFOOTER.Add(myNode.GetAttribute('FooterHTML',false).AttribValue);
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
  ss: TStringStream;
  NewHTML: TIpHtml;
begin
  ss := TStringStream.Create(Src);
  try
    NewHTML := TIpHtml.Create; // Beware: Will be freed automatically by IpHtmlPanel1
    TIpHtmlPanel(myControl).SetHtml(NewHTML);
    NewHTML.LoadFromStream(ss);
  finally
    ss.Free;
  end;
end;
{$endif}


begin
  // this is the set of node attributes that each GPUCanvas instance will have (added to the set inherited from TXIFrame).
  AddWrapperDefaultAttribs(myDefaultAttribs);
  AddDefaultAttribute(myDefaultAttribs,'FrameWidth','String','300','',false);
  AddDefaultAttribute(myDefaultAttribs,'FrameHeight','String','300','',false);
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','Top','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelText','String','HTML Text','',false);
  AddDefaultAttribute(myDefaultAttribs,'HTMLSource','String','','',false,false);
  AddDefaultAttribute(myDefaultAttribs,'IsEditable','Boolean','True','Allow the text page to be edited',false);
  AddDefaultAttribute(myDefaultAttribs,'Showing','Boolean','False','When not embedded, set this to display the text in a standalone browser page',false,false);
  AddDefaultAttribute(myDefaultAttribs,'SourceText','String','...text...','',false);
  AddDefaultAttribute(myDefaultAttribs,'HeaderHTML','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'FooterHTML','String','','',false);
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
  SuppressDesignerProperty(MyNodeType,'BgColor');
  SuppressDesignerProperty(MyNodeType,'HTMLSource');
  {$IFdef JScript}
  SuppressDesignerProperty(MyNodeType,'ActualHeight');
  SuppressDesignerProperty(MyNodeType,'ActualWidth');
  {$endif}

end.
