(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XNumericSlider;

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
  TXNumericSlider = class(TWrapperPanel)
   {$ifndef JScript}
    procedure Barclick(Sender:TObject);
    procedure NumericSliderMouseUp(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X,Y:Longint);
    {$endif}

    procedure SetMyEventTypes;

    function GetItemValue:integer;
    function GetBarWidth:string;
    function GetMaxVal:integer;
    function GetMinVal:integer;

    procedure SetItemValue(AValue:integer);
    procedure SetBarWidth(AValue:string);
    procedure SetMaxVal(AValue:integer);
    procedure SetMinVal(AValue:integer);
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
    property MinVal: integer read GetMinVal write SetMinVal;

  end;

{$ifndef JScript}
procedure Register;
{$endif}

implementation

const MyNodeType='TXNumericSlider';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXNumericSlider.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');

end;

{$ifndef JScript}
procedure Register;
begin
  {$I xnumericslider_icon.lrs}
  RegisterComponents('XComponents',[TXNumericSlider]);

  // inherited from TWrapperPanel, not required here
  RegisterPropertyEditor(TypeInfo(TColor), TXNumericSlider, 'BgColor', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(TXPropertyLink), TXNumericSlider, 'Link', THiddenPropertyEditor);
end;

constructor TXNumericSlider.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXNumericSlider.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXNumericSlider.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  self.BorderSpacing.Around:=glbBorderSpacing;

  myControl:=TTrackBar.Create(self);
  myControl.Parent:=self;

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];

  myControl.OnClick:=@self.BarClick;
  //TTrackBar(myControl).onchange:=@self.NumericSliderChange;
  TTrackBar(myControl).OnMouseUp:=@self.NumericSliderMouseUp;

  self.SetMyEventTypes;

  CreateComponentDataNode2(self,MyNodeType,myDefaultAttribs, self.myEventTypes, TheOwner,IsDynamic);

  self.ParentColor:=true;
  // Setting IsContainer false will prevent designer dropping new child controls into this one.
  self.IsContainer:=false;

  TTrackBar(myControl).Min:=0;
  TTrackBar(myControl).Max:=100;
  TTrackBar(myControl).Constraints.MaxHeight:=20;

  AddLabel(myControl);

end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXNumericSlider',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;

procedure TXNumericSlider.BarClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;

procedure TXNumericSlider.NumericSliderMouseUp(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X,Y:Longint) ;
 var
    NumericSlider:  TTrackBar ;
 begin
    NumericSlider :=  TTrackBar(self.myControl) ;
    //myNode.SetAttributeValue('ItemValue',inttostr(NumericSlider.Position));
    if (self.ItemValue<>NumericSlider.Position)
    and (Button = mbLeft) then
    begin
      self.ItemValue:=NumericSlider.Position;
      CallHandleEvent('Change',inttostr(NumericSlider.Position),Sender);
    end;
 end;

procedure TXNumericSlider.SetBarWidth(AValue:string);
 var
   tc:TControl;
 begin
   tc:=self.myControl;
  myNode.SetAttributeValue('BarWidth',AValue);
  SetHeightWidth(self.myNode,tc,'BarWidth','');
end;

{$else}

constructor TXNumericSlider.Create(MyForm:TForm;NodeName,NameSpace:String);
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
  ItemValue,MaxVal,MinVal,LabelText,LabelPos:string;
  ReadOnly:String;
  OnClickString,OnChangeString:String;
begin
  ItemValue:= MyNode.getAttribute('ItemValue',true).AttribValue;
  MaxVal:= MyNode.getAttribute('MaxVal',true).AttribValue;
  MinVal:= MyNode.getAttribute('MinVal',true).AttribValue;
  LabelText:= MyNode.getAttribute('LabelText',true).AttribValue;
  ReadOnly:= MyNode.getAttribute('ReadOnly',true).AttribValue;

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''','''+NameSpace+''', this.value);" ';
  OnChangeString:='onchange="pas.NodeUtils.SetInterfaceProperty('''+ScreenObjectName+''','''+NameSpace+''',''ItemValue'',this.value);' +
                            'pas.Events.handleEvent(null,''Change'','''+ScreenObjectName+''','''+NameSpace+''', this.value.toString());" ';

  asm
    try{
    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,NameSpace,$impl.MyNodeType,position);

    var HTMLString='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var wrapperid = NameSpace+ScreenObjectName;
    var MyObjectName=wrapperid+'Contents';
    var ReadOnlyString = '';
    if (ReadOnly=='True') { ReadOnlyString = ' readonly ';}

    var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'">'+LabelText+'</label>';

    var SliderString = '<input type="range" id='+MyObjectName + ' '+
                 OnChangeString + ' '+
                 OnClickString + ' '+
                 '   '+ReadOnlyString+
                 ' value='+ItemValue+
                 ' min='+MinVal+' max='+MaxVal+' step=1 ></input>' ;

    HTMLString = labelstring+SliderString;

    var wrapper=document.getElementById(wrapperid);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);

  }
  catch(err) { alert(err.message+'  in XNumericSlider.CreateWidget');}

end;

  MyNode.ScreenObject:=MyNode;

  // now that we have a datanode and a widget, cycle attribute settings
  RefreshComponentProps(myNode);

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXNumericSlider.Create(MyForm,NodeName,NameSpace));
end;


procedure TXNumericSlider.SetBarWidth(AValue:string);
begin
  myNode.SetAttributeValue('BarWidth',AValue);
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
  end;
end;


{$endif}

function TXNumericSlider.GetBarWidth:string;
begin
  result:=MyNode.getAttribute('BarWidth',true).AttribValue;
end;
function TXNumericSlider.GetItemValue:integer;
begin
  result:=strtoint(MyNode.getAttribute('ItemValue',true).AttribValue);
end;
function TXNumericSlider.GetMaxVal:integer;
begin
  result:=strtoint(MyNode.getAttribute('MaxVal',true).AttribValue);
end;
function TXNumericSlider.GetMinVal:integer;
begin
  result:=strtoint(MyNode.getAttribute('MinVal',true).AttribValue);
end;

procedure TXNumericSlider.SetItemValue(AValue:integer);
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

procedure TXNumericSlider.SetMaxVal(AValue:integer);
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
procedure TXNumericSlider.SetMinVal(AValue:integer);
begin
  myNode.SetAttributeValue('MinVal',IntToStr(AValue));
  {$ifndef JScript}
  TProgressBar(myControl).Min:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
       ob.min=AValue;  }
  end;
  {$endif}
end;

begin
  // this is the set of node attributes that each TXNumberSpinner instance will have.
  AddWrapperDefaultAttribs(myDefaultAttribs);
  AddDefaultAttribute(myDefaultAttribs,'BarWidth','String','50','',false);
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','Right','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelText','String','Number Slider','',false);
  AddDefaultAttribute(myDefaultAttribs,'ReadOnly','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'MinVal','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'MaxVal','Integer','100','',false);
  AddDefaultAttribute(myDefaultAttribs,'ItemValue','Integer','5','',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  AddAttribOptions(MyNodeType,'LabelPos',LabelPosOptions);
  {$ifndef JScript}
  RegisterClass(TXNumericSlider);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}
  SuppressDesignerProperty('TXNumericSlider','BgColor');
  SuppressDesignerProperty(MyNodeType,'ContainerHeight');
  SuppressDesignerProperty(MyNodeType,'ContainerWidth');


end.
