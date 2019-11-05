(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XHyperLink;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,TypInfo, NodeUtils, StringUtils,
  {$ifndef JScript}
  LResources, Forms, Controls, StdCtrls, Graphics, Dialogs,
  ExtCtrls, Propedits,RTTICtrls,LCLIntf,
  LazsUtils, Events,
  {$else}
  HTMLUtils,
  {$endif}
  WrapperPanel;

type
  TXHyperLink = class(TWrapperPanel)
  private
    { Private declarations }
    {$ifndef JScript}
    fHandleClick:TEventHandler;
    procedure Labelclick(Sender:TObject);
    {$endif}

    procedure SetMyEventTypes;

    function GetLabelCaption:string;
    function GetURL:string;

    procedure SetLabelCaption(AValue:string);
    procedure SetURL(AValue:string);

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

  published
    { Published declarations }

    // Properties defined for this class...
    property LabelCaption: String read GetLabelCaption write SetLabelCaption;
    property URL: String read GetURL write SetURL;

    {$ifndef JScript}
    // Events to be visible in Lazarus IDE
    property HandleClick: TEventHandler read FHandleClick write FHandleClick;
    {$endif}
  end;


  {$ifndef JScript}
  procedure Register;
  {$endif}

implementation

const MyNodeType='TXHyperlink';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXHyperLink.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
end;

{$ifndef JScript}
procedure Register;
begin
  {$I xhyperlink_icon.lrs}
  RegisterComponents('XComponents',[TXHyperLink]);

  // inherited from TWrapperPanel, not required here
  RegisterPropertyEditor(TypeInfo(TColor), TXHyperLink, 'BgColor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXHyperLink, 'ContainerHeight', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXHyperLink, 'ContainerWidth', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXHyperLink, 'LabelPos', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXHyperLink, 'LabelText', THiddenPropertyEditor);
  //RegisterPropertyEditor(TypeInfo(TXPropertyLink), TXHyperLink, 'Link', THiddenPropertyEditor);
end;

constructor TXHyperLink.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXHyperLink.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXHyperLink.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  self.BorderSpacing.Around:=glbBorderSpacing;

  myControl:=TLabel.Create(self);
  myControl.Parent:=self;

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];

  myControl.OnClick:=@self.LabelClick;
  myControl.font.color := clBlue;
  myControl.font.Underline:=true;

  self.SetMyEventTypes;

  CreateComponentDataNode2(self,MyNodeType,myDefaultAttribs, self.myEventTypes, TheOwner,IsDynamic);

  self.ParentColor:=true;
  // Setting IsContainer false will prevent designer dropping new child controls into this one.
  self.IsContainer:=false;

end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXHyperLink',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;


procedure TXHyperLink.LabelClick(Sender: TObject) ;
var
  LabelItem : TLabel ;
  myURL:string;
begin
  if not (csDesigning in componentState) then
      begin
         LabelItem := TLabel (sender) ;
         myURL:=LabelItem.AccessibleDescription  ;
         OpenURL(myURL);
         CallHandleEvent('Click',LabelItem.AccessibleDescription,Sender);

      end;
end;

{$else}


constructor TXHyperLink.Create(MyForm:TForm;NodeName,NameSpace:String);
begin
  inherited Create(NodeName,NameSpace);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=false;

  SetNodePropDefaults(self,myDefaultAttribs);

end;


function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  LabelCaption,URL:string;
  OnClickString:String;
begin
  LabelCaption:= MyNode.getAttribute('LabelCaption',true).AttribValue;
  URL:= MyNode.getAttribute('URL',true).AttribValue;

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''','''+NameSpace+''','''+URL+'''); " ';

  asm
    try{
    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,NameSpace,$impl.MyNodeType,position);

    var HTMLString='';
    var wrapperid =  NameSpace+ScreenObjectName;
    var MyObjectName=wrapperid+'Contents';

    HTMLString = '<a id='+MyObjectName+' href="'+URL+'" target="_blank" '+
                         OnClickString +
                         ' style="display: inline-block;"  >'+LabelCaption+'</a> ';

    var wrapper=document.getElementById(wrapperid);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);
  }
  catch(err) { alert(err.message+'  in XHyperLink.CreateWidget');}

end;

  MyNode.ScreenObject:=MyNode;

  RefreshComponentProps(myNode);

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXHyperLink.Create(MyForm,NodeName,NameSpace));
end;


{$endif}

function TXHyperLink.GetLabelCaption:String;
begin
  result:=MyNode.getAttribute('LabelCaption',true).AttribValue;
end;
function TXHyperLink.GetURL:String;
begin
  result:=MyNode.getAttribute('URL',true).AttribValue;
end;

procedure TXHyperLink.SetLabelCaption(AValue:string);
begin
  myNode.SetAttributeValue('LabelCaption',AValue);
  {$ifndef JScript}
  if myControl<>nil then
     TLabel(myControl).Caption:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
       ob.innerHTML=AValue;   }
  end;
  {$endif}
end;

  procedure TXHyperLink.SetURL(AValue:string);
  begin
    myNode.SetAttributeValue('URL',AValue);
    {$ifndef JScript}
    if myControl<>nil then
       TLabel(myControl).AccessibleDescription:=AValue;
    {$else}
    asm
      var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
      if (ob!=null) {
         ob.href=AValue;   }
    end;
    {$endif}
  end;

begin
  // this is the set of node attributes that each TXHyperLink instance will have.
  AddWrapperDefaultAttribs(myDefaultAttribs);
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','Right','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelCaption','String','BBC News','',false);
  AddDefaultAttribute(myDefaultAttribs,'URL','String','http://www.bbc.co.uk/news','',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  AddAttribOptions(MyNodeType,'LabelPos',LabelPosOptions);
  {$ifndef JScript}
  RegisterClass(TXHyperLink);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}
  SuppressDesignerProperty('TXHyperLink','BgColor');
end.
