(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XProgressBar;

{$ifndef JScript}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, ComCtrls, Graphics, Dialogs, ExtCtrls,TypInfo, Propedits,RTTICtrls,
  WrapperPanel,NodeUtils,StringUtils, LazsUtils, Events;
{$else}
interface
uses
  Classes, SysUtils,TypInfo,
  NodeUtils,StringUtils,HTMLUtils, WrapperPanel;

{$endif}

type
  TXProgressBar = class(TWrapperPanel)
  private
    {$ifndef JScript}
    procedure Barclick(Sender:TObject);
    {$endif}

    procedure SetMyEventTypes;

    function GetItemValue:integer;
    function GetBarWidth:string;
    function GetMaxVal:integer;

    procedure SetItemValue(AValue:integer);
    procedure SetBarWidth(AValue:string);
    procedure SetMaxVal(AValue:integer);
  protected
    {$ifndef JScript}
    procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
    property ParentColor;
    {$endif}
  public
    {$ifndef JScript}
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent;IsDynamic:Boolean); override;
    {$else}
    constructor Create(MyForm:TForm;NodeName,NameSpace:String);
    {$endif}
  published
    property ItemValue: integer read GetItemValue write SetItemValue;
    property BarWidth: String read GetBarWidth write SetBarWidth;
    property MaxVal: integer read GetMaxVal write SetMaxVal;

  end;

{$ifndef JScript}
procedure Register;
{$endif}


implementation

const MyNodeType='TXProgressBar';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXProgressBar.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
end;

{$ifndef JScript}
procedure Register;
begin
  {$I xprogressbar_icon.lrs}
  RegisterComponents('XComponents',[TXProgressBar]);

  // inherited from TWrapperPanel, not required here
  RegisterPropertyEditor(TypeInfo(TColor), TXProgressBar, 'BgColor', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(TXPropertyLink), TXProgressBar, 'Link', THiddenPropertyEditor);
end;

constructor TXProgressBar.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXProgressBar.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXProgressBar.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  self.BorderSpacing.Around:=glbBorderSpacing;

  myControl:=TProgressBar.Create(self);
  myControl.Parent:=self;

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];

  myControl.OnClick:=@self.BarClick;

  self.SetMyEventTypes;

  CreateComponentDataNode2(self,MyNodeType,myDefaultAttribs, self.myEventTypes, TheOwner,IsDynamic);

  self.ParentColor:=true;
  // Setting IsContainer false will prevent designer dropping new child controls into this one.
  self.IsContainer:=false;

  TProgressBar(myControl).Min:=0;
  TProgressBar(myControl).Step:=1;
  TProgressBar(myControl).Constraints.MaxHeight:=20;

  AddLabel(myControl);

end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXProgressBar',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;

procedure TXProgressBar.BarClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;

procedure TXProgressBar.SetBarWidth(AValue:string);
    var
      tc:TControl;
    begin
      tc:=self.myControl;
  myNode.SetAttributeValue('BarWidth',AValue);
  SetHeightWidth(self.myNode,tc,'BarWidth','');
end;

{$else}

constructor TXProgressBar.Create(MyForm:TForm;NodeName,NameSpace:String);
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
  ItemValue,MaxVal,LabelText,LabelPos:string;
  ReadOnly:Boolean;
  OnClickString:String;
begin
  ItemValue:= MyNode.getAttribute('ItemValue',true).AttribValue;
  MaxVal:= MyNode.getAttribute('MaxVal',true).AttribValue;
  LabelText:= MyNode.getAttribute('LabelText',true).AttribValue;

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''','''+NameSpace+''', this.value);" ';

  asm
    try{
    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,NameSpace,$impl.MyNodeType,position);

    var HTMLString='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var wrapperid = NameSpace+ScreenObjectName;
    var MyObjectName=wrapperid+'Contents';

    var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'">'+LabelText+'</label>';

    var BarString = '<progress id='+MyObjectName+' style="display: inline-block; height:20px"  max='+MaxVal+' value='+ItemValue+
                              OnClickString +
                              '></progress>';

    HTMLString = labelstring+BarString;

    var wrapper=document.getElementById(wrapperid);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);

  }
  catch(err) { alert(err.message+'  in XProgressBar.CreateWidget');}

end;

  MyNode.ScreenObject:=MyNode;

  // now that we have a datanode and a widget, cycle attribute settings
  RefreshComponentProps(myNode);

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXProgressBar.Create(MyForm,NodeName,NameSpace));
end;


procedure TXProgressBar.SetBarWidth(AValue:string);
begin
  myNode.SetAttributeValue('BarWidth',AValue);
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
  end;
end;


{$endif}

function TXProgressBar.GetBarWidth:string;
begin
  result:=MyNode.getAttribute('BarWidth',true).AttribValue;
end;
function TXProgressBar.GetItemValue:integer;
begin
  result:=strtoint(MyNode.getAttribute('ItemValue',true).AttribValue);
end;
function TXProgressBar.GetMaxVal:integer;
begin
  result:=strtoint(MyNode.getAttribute('MaxVal',true).AttribValue);
end;

procedure TXProgressBar.SetItemValue(AValue:integer);
begin
  myNode.SetAttributeValue('ItemValue',inttostr(AValue));
  {$ifndef JScript}
  TProgressBar(myControl).Position:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
       ob.value=AValue;  }
  end;
  {$endif}
end;

procedure TXProgressBar.SetMaxVal(AValue:integer);
begin
  myNode.SetAttributeValue('MaxVal',IntToStr(AValue));
  {$ifndef JScript}
  TProgressBar(myControl).Max:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
       ob.max=AValue;  }
  end;
  {$endif}
end;

begin
  // this is the set of node attributes that each TXNumberSpinner instance will have.
  AddWrapperDefaultAttribs(myDefaultAttribs);
  AddDefaultAttribute(myDefaultAttribs,'BarWidth','String','200','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','Right','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelText','String','Progress Bar','',false);
  AddDefaultAttribute(myDefaultAttribs,'MaxVal','Integer','100','',false);
  AddDefaultAttribute(myDefaultAttribs,'ItemValue','Integer','5','',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  AddAttribOptions(MyNodeType,'LabelPos',LabelPosOptions);
  {$ifndef JScript}
  RegisterClass(TXProgressBar);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}
  SuppressDesignerProperty('TXProgressBar','BgColor');
  SuppressDesignerProperty(MyNodeType,'ContainerHeight');
  SuppressDesignerProperty(MyNodeType,'ContainerWidth');

end.
