(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XNumberSpinner;
{$ifndef JScript}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls,TypInfo, Propedits,RTTICtrls,
  WrapperPanel,NodeUtils,StringUtils, LazsUtils, Events, spin;
{$else}
interface
uses
  Classes, SysUtils,TypInfo,
  NodeUtils,StringUtils,HTMLUtils, WrapperPanel;

{$endif}

type
  TXNumberSpinner = class(TWrapperPanel)
  private
    { Private declarations }
    {$ifndef JScript}
    fHandleClick:TEventHandler;
    fHandleChange:TEventHandler;
    procedure Spinnerclick(Sender:TObject);
    procedure SpinnerChange(Sender: TObject);
    {$endif}

    procedure SetMyEventTypes;

    function GetMaxVal:integer;
    function GetMinVal:integer;
    function GetStepSize:integer;
    function GetItemValue:integer;
    function GetReadOnly:Boolean;
    function GetSpinnerWidth:string;

    procedure SetMaxVal(AValue:integer);
    procedure SetMinVal(AValue:integer);
    procedure SetStepSize(AValue:integer);
    procedure SetItemValue(AValue:integer);
    procedure SetReadOnly(AValue:Boolean);
    procedure SetSpinnerWidth(AValue:string);

  protected
    { Protected declarations }
//    procedure LinkLoadFromProperty(Sender: TObject);  override;
//    procedure LinkSaveToProperty(Sender: TObject);  override;
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
    constructor Create(MyForm:TForm;NodeName:String);
    {$endif}

  published
    { Published declarations }

    // Properties defined for this class...
    property MaxVal: integer read GetMaxVal write SetMaxVal;
    property MinVal: integer read GetMinVal write SetMinVal;
    property StepSize: integer read GetStepSize write SetStepSize;
    property ItemValue: integer read GetItemValue write SetItemValue;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property SpinnerWidth: String read GetSpinnerWidth write SetSpinnerWidth;

    {$ifndef JScript}
    // Events to be visible in Lazarus IDE
    property HandleClick: TEventHandler read FHandleClick write FHandleClick;
    property HandleChange: TEventHandler read FHandleChange write FHandleChange;
    {$endif}
  end;


  {$ifndef JScript}
  procedure Register;
  {$endif}


implementation

const MyNodeType='TXNumberSpinner';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXNumberSpinner.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
  MyEventTypes.Add('Change');
end;

{$ifndef JScript}
procedure Register;
begin
  {$I xnumberspinner_icon.lrs}
  RegisterComponents('XComponents',[TXNumberSpinner]);

  // suppress some of the Link properties
//  RegisterPropertyEditor(TypeInfo(TAliasStrings), TXPropertyLink, 'AliasValues', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(String), TXPropertyLink, 'TIElementName', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(TPropertyLinkOptions), TXPropertyLink, 'Options', THiddenPropertyEditor);
end;

constructor TXNumberSpinner.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXNumberSpinner.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXNumberSpinner.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  self.BorderSpacing.Around:=glbBorderSpacing;

  myControl:=TSpinEdit.Create(self);
  myControl.Parent:=self;

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];


  TSpinEdit(myControl).OnEditingDone:=@self.myeditingDone;

  TSpinEdit(myControl).OnChange:=@self.SpinnerChange;
  TSpinEdit(myControl).OnClick:=@self.SpinnerClick;

  self.SetMyEventTypes;

  CreateComponentDataNode2(self,MyNodeType,myDefaultAttribs, self.myEventTypes, TheOwner,IsDynamic);

  self.ParentColor:=true;
  // Setting IsContainer false will prevent designer dropping new child controls into this one.
  self.IsContainer:=false;

  AddLabel(myControl);


end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXNumberSpinner',ParentNode.MyForm,ParentNode,ScreenObjectName,Alignment,position);
  result:=NewNode;
end;


procedure TXNumberSpinner.SpinnerClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;

procedure TXNumberSpinner.SpinnerChange(Sender: TObject) ;
 var
    Spinner: TSpinEdit ;
 begin
    Spinner := TSpinEdit(sender) ;
    self.ItemValue:=Spinner.Value;
    CallHandleEvent('Change',IntToStr(Spinner.Value),Sender);
 end;


//procedure TXNumberSpinner.LinkLoadFromProperty(Sender: TObject);
//begin
//  if Sender=nil then ;
//  if (Link.Editor=nil) then exit;
//  inherited  LinkLoadFromProperty(Sender);
//
//  self.ItemValue:=Link.GetAsInt;
//
//end;
//
//procedure TXNumberSpinner.LinkSaveToProperty(Sender: TObject);
//begin
//  if Sender=nil then ;
//  if Link.Editor=nil then exit;
//  Link.SetAsInt(TSpinEdit(myControl).Value);
//
//end;

procedure TXNumberSpinner.SetSpinnerWidth(AValue:string);
 var
   tc:TControl;
 begin
   tc:=self.myControl;
  myNode.SetAttributeValue('SpinnerWidth',AValue);
  SetHeightWidth(self.myNode,tc,'SpinnerWidth','');
end;

{$else}

constructor TXNumberSpinner.Create(MyForm:TForm;NodeName:String);
begin
  inherited Create(NodeName);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=false;

  SetNodePropDefaults(self,myDefaultAttribs);

end;


function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer;Alignment:String):TDataNode;
var
  ItemValue,LabelText,LabelPos,MinVal,MaxVal,StepSize:string;
  ReadOnly:Boolean;
  OnChangeString, OnClickString:String;
begin
  MinVal:= MyNode.getAttribute('MinVal',true).AttribValue;
  MaxVal:= MyNode.getAttribute('MaxVal',true).AttribValue;
  StepSize:= MyNode.getAttribute('StepSize',true).AttribValue;
  ItemValue:= MyNode.getAttribute('ItemValue',true).AttribValue;
  LabelText:= MyNode.getAttribute('LabelText',true).AttribValue;
  ReadOnly:= StrToBool(MyNode.getAttribute('ReadOnly',true).AttribValue);

  //NB. click event resets focus, so that there will always be a onchange event fired when the spinners are clicked.
  OnClickString:='onclick="ob=document.getElementById('''+ScreenObjectName+''');ob.focus();pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''', this.value);event.stopPropagation();" ';
  OnChangeString:= 'onchange="pas.NodeUtils.SetInterfaceProperty('''+ScreenObjectName+''',''ItemValue'',this.value); '+
                             'pas.Events.handleEvent(null,''Change'','''+ScreenObjectName+''', this.value, ''ItemValue'');" ';


  asm
    try{
    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);

    var HTMLString='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var MyObjectName=ScreenObjectName+'Contents';

    var ReadOnlyString = '';
    if (ReadOnly==true) { ReadOnlyString = ' readonly ';}

    var inputtext= ItemValue;
    var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'">'+LabelText+'</label>';
    var SpinnerString = '<input type="number"  id='+MyObjectName+' ' +
                          OnClickString +
                          OnChangeString +
                 ' style="display: inline-block; '+
                 '" value='+ItemValue+' '+ReadOnlyString+
                 ' min='+MinVal+' max='+MaxVal+' step='+StepSize+'></input>' ;

    HTMLString = labelstring+SpinnerString;

    var wrapper=document.getElementById(ScreenObjectName);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);

  }
  catch(err) { alert(err.message+'  in XNumberSpinner.CreateWidget');}

end;

  MyNode.ScreenObject:=MyNode;

  // now that we have a datanode and a widget, cycle attribute settings
  RefreshComponentProps(myNode);

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName:String):TObject;
begin
  result:=TObject(TXNumberSpinner.Create(MyForm,NodeName));
end;

//procedure TXNumberSpinner.LinkLoadFromProperty(Sender: TObject);
//begin
//  inherited  LinkLoadFromProperty(Sender);
//end;
//
//procedure TXNumberSpinner.LinkSaveToProperty(Sender: TObject);
//begin
//  if Sender=nil then ;
//  if Link=nil then exit;
//  if Link.TIObject=nil then exit;
////  showmessage('linksavetoproperty. '+Link.TIPropertyName+' '+self.ItemValue);
//
//  SetNativeIntProp(Link.TIObject,Link.TIPropertyName,self.ItemValue);
//end;


procedure TXNumberSpinner.SetSpinnerWidth(AValue:string);
begin
  myNode.SetAttributeValue('SpinnerWidth',AValue);
  asm
  var ob = document.getElementById(this.NodeName+'Contents');
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
  end;
end;


{$endif}

function TXNumberSpinner.GetSpinnerWidth:string;
begin
  result:=MyNode.getAttribute('SpinnerWidth',true).AttribValue;
end;
function TXNumberSpinner.GetItemValue:integer;
begin
  result:=strtoint(MyNode.getAttribute('ItemValue',true).AttribValue);
end;
function TXNumberSpinner.GetMaxVal:integer;
begin
  result:=strtoint(MyNode.getAttribute('MaxVal',true).AttribValue);
end;
function TXNumberSpinner.GetMinVal:integer;
begin
  result:=strtoint(MyNode.getAttribute('MinVal',true).AttribValue);
end;
function TXNumberSpinner.GetStepSize:integer;
begin
  result:=strtoint(MyNode.getAttribute('StepSize',true).AttribValue);
end;
function TXNumberSpinner.GetReadOnly:Boolean;
begin
  result:=MyStrToBool(MyNode.getAttribute('ReadOnly',true).AttribValue);
end;

procedure TXNumberSpinner.SetItemValue(AValue:integer);
begin
  myNode.SetAttributeValue('ItemValue',inttostr(AValue));
  {$ifndef JScript}
  TSpinEdit(myControl).Value:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
       ob.value=AValue;  }
  end;
  {$endif}
end;

procedure TXNumberSpinner.SetMaxVal(AValue:integer);
begin
  myNode.SetAttributeValue('MaxVal',IntToStr(AValue));
  {$ifndef JScript}
  TSpinEdit(myControl).MaxValue:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
       ob.max=AValue;  }
  end;
  {$endif}
end;
procedure TXNumberSpinner.SetMinVal(AValue:integer);
begin
  myNode.SetAttributeValue('MinVal',IntToStr(AValue));
  {$ifndef JScript}
  TSpinEdit(myControl).MinValue:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
       ob.min=AValue;  }
  end;
  {$endif}
end;
procedure TXNumberSpinner.SetStepSize(AValue:integer);
begin
  myNode.SetAttributeValue('StepSize',IntToStr(AValue));
  {$ifndef JScript}
  TSpinEdit(myControl).Increment:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
       ob.step=AValue;  }
  end;
  {$endif}
end;
procedure TXNumberSpinner.SetReadOnly(AValue:Boolean);
begin
  myNode.SetAttributeValue('ReadOnly',myBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  TSpinEdit(myControl).ReadOnly:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
      ob.readOnly = AValue  }
  end;
  {$endif}
end;

begin
  // this is the set of node attributes that each TXNumberSpinner instance will have.
  AddDefaultAttribute(myDefaultAttribs,'Alignment','String','Left','',false);
  AddDefaultAttribute(myDefaultAttribs,'Hint','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'IsVisible','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpinnerWidth','String','50','',false);
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','Right','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelText','String','Number Spinner','',false);
  AddDefaultAttribute(myDefaultAttribs,'ReadOnly','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'MinVal','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'MaxVal','Integer','100','',false);
  AddDefaultAttribute(myDefaultAttribs,'StepSize','Integer','1','',false);
  AddDefaultAttribute(myDefaultAttribs,'ItemValue','Integer','5','',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  AddAttribOptions(MyNodeType,'LabelPos',LabelPosOptions);
  {$ifndef JScript}
  RegisterClass(TXNumberSpinner);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}
   SuppressDesignerProperty('TXNumberSpinner','BgColor');
  SuppressDesignerProperty(MyNodeType,'ContainerHeight');
  SuppressDesignerProperty(MyNodeType,'ContainerWidth');


end.
