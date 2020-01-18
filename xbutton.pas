(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XButton;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, NodeUtils, StringUtils,
  {$ifndef JScript}
  LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls,Propedits, RTTICtrls,
  LazsUtils, Events,
  {$else}
  HTMLUtils,
  {$endif}
  WrapperPanel;

type
  TXButton = class(TWrapperPanel)
  private
    { Private declarations }
    {$ifndef JScript}
    fHandleButtonClick:TEventHandler;
    procedure buttonclick(Sender:TObject);
    {$endif}
    procedure SetMyEventTypes;

    function GetCaption:string;
    function GetButtonWidth:string;
    function GetEnabled:Boolean;

    procedure SetCaption(AValue:string);
    procedure SetEnabled(AValue:Boolean);
    procedure SetButtonWidth(AValue:string);


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
    property Caption: String read GetCaption write SetCaption;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property ButtonWidth: String read GetButtonWidth write SetButtonWidth;

    {$ifndef JScript}
    // Events to be visible in Lazarus IDE
    property HandleButtonClick: TEventHandler read FHandleButtonClick write FHandleButtonClick;
    {$endif}
  end;


  {$ifndef JScript}
  procedure Register;
  {$endif}



implementation

const MyNodeType='TXButton';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXButton.SetMyEventTypes;
begin
  MyEventTypes.Add('ButtonClick');
end;

{$ifndef JScript}
procedure Register;
begin
  // Lazarus IDE component registration
  RegisterComponents('XComponents',[TXButton]);

//  RegisterPropertyEditor(TypeInfo(TXPropertyLink), TXButton, 'Link', THiddenPropertyEditor);

end;

procedure TXButton.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin

    self.BorderSpacing.Around:=glbBorderSpacing;

    myControl:=TButton.Create(self);
    myControl.Parent:=self;

    myControl.SetSubComponent(true);  // Tell the IDE the component is a sub-component
    // Make sure the embedded component can not be selected/deleted within the IDE
    myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];

    TButton(myControl).OnClick:=@self.ButtonClick;
    myControl.AutoSize:=true;

    self.SetMyEventTypes;

    CreateComponentDataNode2(self,MyNodeType,myDefaultAttribs, self.myEventTypes, TheOwner,IsDynamic);

    self.ParentColor:=true;
    // Set IsContainer false, to prevent designer dropping new child controls into this one.
    self.IsContainer:=false;

end;

constructor TXButton.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXButton.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;


procedure TXButton.ButtonClick(Sender: TObject) ;
begin
  CallHandleEvent('ButtonClick',self.myNode.NodeName,self);
end;



function CreateWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXButton',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;

{$else}
procedure AddButtonStyles;
begin
  asm
  // ----------------------------------------check if the style has already been set
   var x = document.getElementsByTagName("STYLE");
   var StyleIsSet = false;
   if (x.length>0){
     for (var i=0; i<x.length; i++){
       var y= x[i].innerHTML;
       if (y.indexOf(".replayButton") !=-1) { StyleIsSet =true}
     }
   }

   if (StyleIsSet == false){
       var StyleString = '<style>'
       +'.replayButton { '
           +' background-color: red;'
           +' }'
        +' </style>';

     //----------------------------- now append the style declarations to the head of the HTML page
     document.head.innerHTML = document.head.innerHTML+StyleString;
    }
  end;
end;

constructor TXButton.Create(MyForm:TForm;NodeName,NameSpace:String);
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
  xItemText,xEnabled:string;
  //,marginString
begin
xItemText:= MyNode.getAttribute('Caption',true).AttribValue;
xEnabled:= MyNode.getAttribute('Enabled',true).AttribValue;
//marginString := 'margin:'+glbMarginSpacing+' '
//                         +glbMarginSpacing+' '
//                         +glbMarginSpacing+' '
//                         +glbMarginSpacing+';';
AddButtonStyles;

asm
  try{

    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,NameSpace,$impl.MyNodeType,position);

    var HTMLString='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var NameSpaceString = "'"+NameSpace+"'";
    var wrapperid =  NameSpace+ScreenObjectName;
    var MyObjectName=wrapperid+'Contents';

    var EnabledString = '';
    if (xEnabled=='False') { EnabledString = ' disabled ';}

    var typestring="'ButtonClick'";
    HTMLString = '<input type="button" id='+MyObjectName+' class="widgetinner '+wrapperid+'" '+
                         'style="font-size:inherit; display: inline-block; '+
//                                marginString+'" '+
                                '" '+
    'onclick="event.stopPropagation(); pas.Events.handleEvent(null,'+typestring+','+NodeIDString+', '+NameSpaceString+', '+NodeIDString+');"'+
                        '  '+EnabledString+' value="'+xItemText+'"> ';

    var wrapper=document.getElementById(wrapperid);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);

    pas.HTMLUtils.FixHeightToLineHeight(MyObjectName);

  }
  catch(err) { alert(err.message+'  in XButton.CreateWidget');}

end;

MyNode.ScreenObject:=MyNode;
RefreshComponentProps(myNode);

result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXButton.Create(MyForm,NodeName,NameSpace));
end;

{$endif}



function TXButton.GetCaption:string;
begin
  result:=myNode.GetAttribute('Caption',true).AttribValue;
end;
function TXButton.GetEnabled:Boolean;
begin
  result:=myStrToBool(myNode.GetAttribute('Enabled',true).AttribValue);
end;
function TXButton.GetButtonWidth:String;
begin
  result:=myNode.GetAttribute('ButtonWidth',true).AttribValue;
end;

procedure TXButton.SetCaption(AValue:string);
begin
  myNode.SetAttributeValue('Caption',AValue);
  {$ifndef JScript}
  TButton(myControl).Caption:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
       //alert('set button caption '+AValue);
       ob.value=AValue;  }
  end;
  {$endif}
end;

procedure TXButton.SetEnabled(AValue:Boolean);
begin
  myNode.SetAttributeValue('Enabled',myBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  TButton(myControl).Enabled:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
    if (AValue==false) {ob.disabled = true}
    else {ob.disabled = false }
    }
  end;
  {$endif}
end;

procedure TXButton.SetButtonWidth(AValue:string);
{$ifndef JScript}
var
  tc:TControl;
{$endif}
begin
  myNode.SetAttributeValue('ButtonWidth',AValue);
  {$ifndef JScript}
  tc:=self.myControl;
  SetHeightWidth(self.myNode,tc,'ButtonWidth','');
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
  end;
  {$endif}
end;

begin
  // this is the set of node attributes that each XButton instance will have.
  AddWrapperDefaultAttribs(myDefaultAttribs);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'Caption','String','Press Me','',false);
  AddDefaultAttribute(myDefaultAttribs,'Enabled','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'ButtonWidth','String','','',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  {$ifndef JScript}
  {$I XButton.lrs}
  RegisterClass(TXButton);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);

  {$else}

  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}
  SuppressDesignerProperty('TXButton','LabelPos');
  SuppressDesignerProperty('TXButton','LabelText');
  SuppressDesignerProperty('TXButton','BgColor');
  SuppressDesignerProperty('TXButton','ContainerHeight');
  SuppressDesignerProperty('TXButton','ContainerWidth');

end.


