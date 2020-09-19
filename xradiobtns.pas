(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XRadioBtns;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface
uses
  Classes, SysUtils,TypInfo, NodeUtils,StringUtils,
  {$ifndef JScript}
  LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, Propedits,RTTICtrls,
  LazsUtils, Events,
  {$else}
  HTMLUtils,
  {$endif}
  WrapperPanel;

{$ifdef JScript}
function CreateButtonsList(myNode:TDataNode;OptionList:String):string;
{$endif}


type
  TXRadioBtns = class(TWrapperPanel)
  private
    { Private declarations }
    {$ifndef JScript}
    fHandleClick:TEventHandler;
    fHandleChange:TEventHandler;
    procedure RadioGroupclick(Sender:TObject);
    procedure RadioGroupChange(Sender:TObject);
    {$endif}

    procedure SetMyEventTypes;

    function GetReadOnly:Boolean;
    function GetItemValue:String;
    function GetOptionList:String;
    function GetCaption:String;
    function GetBoxWidth:string;

    procedure SetReadOnly(AValue:Boolean);
    procedure SetItemValue(AValue:String);
    procedure SetOptionList(AValue:String);
    procedure SetCaption(AValue:String);
    procedure SetBoxWidth(AValue:string);

    procedure SetBgColor(AValue:TColor); override;


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
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property ItemValue: String read GetItemValue write SetItemValue;
    property Caption: String read GetCaption write SetCaption;
    property OptionList: String read GetOptionList write SetOptionList;
    property BoxWidth: String read GetBoxWidth write SetBoxWidth;

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

const MyNodeType='TXRadioBtns';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXRadioBtns.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
  MyEventTypes.Add('Change');
end;

{$ifndef JScript}
procedure Register;
begin
  {$I xradiobtns_icon.lrs}
  RegisterComponents('XComponents',[TXRadioBtns]);

   // inherited from TWrapperPanel, not required here
   RegisterPropertyEditor(TypeInfo(String), TXRadioBtns, 'LabelPos', THiddenPropertyEditor);
   RegisterPropertyEditor(TypeInfo(String), TXRadioBtns, 'LabelText', THiddenPropertyEditor);

end;

constructor TXRadioBtns.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXRadioBtns.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXRadioBtns.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  myControl:=TRadioGroup.Create(self);
  myControl.Parent:=self;

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];

  myControl.OnClick:=@self.RadioGroupClick;
  TRadioGroup(myControl).OnSelectionChanged:=@self.RadioGroupChange;


  self.SetMyEventTypes;

  CreateComponentDataNode2(self,MyNodeType,myDefaultAttribs, self.myEventTypes, TheOwner,IsDynamic);

  self.ParentColor:=true;
  TRadioGroup(myControl).ParentColor:=false;
  // Setting IsContainer false will prevent designer dropping new child controls into this one.
  self.IsContainer:=false;

end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXRadioBtns',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;

procedure TXRadioBtns.RadioGroupChange(Sender: TObject) ;
 var
    RadioButtonGroup : TCustomRadioGroup;
 begin
   if not (csDesigning in componentState)
   and not (csLoading in componentState) then
   begin
    RadioButtonGroup :=  TCustomRadioGroup(sender) ;
    self.ItemValue:=RadioButtonGroup.Items[RadioButtonGroup.ItemIndex];
    CallHandleEvent('Change',self.ItemValue,Sender);
   end;
 end;

procedure TXRadioBtns.RadioGroupClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState)
  and not (csLoading in componentState) then
    CallHandleEvent('Click',self.myNode.NodeName,self);
end;

procedure TXRadioBtns.SetBoxWidth(AValue:string);
 var
   tc:TControl;
 begin
   tc:=self.myControl;
  myNode.SetAttributeValue('BoxWidth',AValue);
  SetHeightWidth(self.myNode,tc,'BoxWidth','');
end;

{$else}

constructor TXRadioBtns.Create(MyForm:TForm;NodeName,NameSpace:String);
begin
  inherited Create(NodeName,NameSpace);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=false;

  SetNodePropDefaults(self,myDefaultAttribs);
end;

function CreateButtonsList(myNode:TDataNode;OptionList:String):string;
var
  OnChangeString, ItemValue,ReadOnly,myName,NameSpace,quot:string;
begin
  ReadOnly:= MyNode.getAttribute('ReadOnly',true).AttribValue;
  ItemValue:= MyNode.getAttribute('ItemValue',true).AttribValue;
  NameSpace:=myNode.nameSpace;
  myName:=myNode.NodeName;
  OnChangeString:='onchange="if (this.checked) {pas.NodeUtils.SetInterfaceProperty('''+myName+''','''+NameSpace+''',''ItemValue'',this.value);' +
                          'pas.Events.handleEvent(null,''Change'','''+myName+''','''+NameSpace+''',''';
  quot:='''';

  asm
  try{
    var ReadOnlyString = '';
    if (ReadOnly=='true') { ReadOnlyString = ' readonly ';}

    var HTMLString='';
    var wrapperid = NameSpace+myName;
    var optionlistarray=JSON.parse(OptionList);
    for (var i=0; i<optionlistarray.length; i++){
       var currentitemstring = optionlistarray[i];
       var selectedflag ='';
       if (currentitemstring==ItemValue ){selectedflag = 'checked'}
       HTMLString = HTMLString +'<input type="radio"  '+selectedflag + ReadOnlyString
                               +' id="'+wrapperid+currentitemstring+'" '
                               +' name='+wrapperid+' '
                               + OnChangeString+currentitemstring+quot+');}" '
                               +' value="'+currentitemstring+'" '
                               +'>'+currentitemstring+'<Br>';
     }
     return HTMLString;
  }
  catch(err) { alert(err.message+'  in XRadioBtns.CreateButtonsList');}
  end;

end;

function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  myCaption,ItemValue,OptionList:string;
  OnClickString:String;
begin
  myCaption:= MyNode.getAttribute('Caption',true).AttribValue;
  OptionList:= MyNode.getAttribute('OptionList',true).AttribValue;

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''','''+NameSpace+''','''');"';
  asm
    try{
    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,NameSpace,$impl.MyNodeType,position);

    var HTMLString='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var wrapperid = NameSpace+ScreenObjectName;
    var MyObjectName=wrapperid+'Contents';

    HTMLString = '<fieldset  id='+MyObjectName+' style="display: inline-block;height:100%-20px;width:100%;" '
                 + OnClickString
                 +' >  ';
    var Legend='<legend id='+MyObjectName+'Legend >'+myCaption+'</legend>';
    var Buttons=$mod.CreateButtonsList(MyNode,OptionList);
    HTMLString = HTMLString + Legend + Buttons + '</fieldset> ';

    var wrapper=document.getElementById(wrapperid);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);
  }
  catch(err) { alert(err.message+'  in XRadioBtns.CreateWidget');}

end;
  MyNode.ScreenObject:=MyNode;
  RefreshComponentProps(myNode);

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXRadioBtns.Create(MyForm,NodeName,NameSpace));
end;

procedure TXRadioBtns.SetBoxWidth(AValue:string);
begin
  //showmessage('memo width='+AValue);
  myNode.SetAttributeValue('BoxWidth',AValue);
  asm
  var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
  //  if (ob==null) {alert(this.NodeName+'Contents'+'  not found');}
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
  end;
end;

{$endif}

function TXRadioBtns.GetCaption:String;
begin
  result:=MyNode.getAttribute('Caption',true).AttribValue;
end;
function TXRadioBtns.GetItemValue:String;
begin
  result:=MyNode.getAttribute('ItemValue',true).AttribValue;
end;
function TXRadioBtns.GetOptionList:String;
begin
  result:=MyNode.getAttribute('OptionList',true).AttribValue;
end;
function TXRadioBtns.GetReadOnly:Boolean;
var
  tmp:string;
begin
  tmp:=MyNode.getAttribute('ReadOnly',true).AttribValue;
  if tmp='' then tmp:='False';
  result:=MyStrToBool(tmp);
end;
function TXRadioBtns.GetBoxWidth:string;
begin
  result:=MyNode.getAttribute('BoxWidth',true).AttribValue;
end;

procedure TXRadioBtns.SetCaption(AValue:String);
begin
  myNode.SetAttributeValue('Caption',AValue);
  {$ifndef JScript}
  TRadioGroup(myControl).Caption:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'ContentsLegend');
    if (ob!=null) {
       //alert('setcaption '+AValue);
       ob.innerHTML=AValue  }
  end;
  {$endif}
end;

procedure TXRadioBtns.SetItemValue(AValue:String);
var
  NewIndex:integer;
begin
  myNode.SetAttributeValue('ItemValue',AValue);
  {$ifndef JScript}
  NewIndex:=TRadioGroup(myControl).Items.IndexOf(AValue);
  if NewIndex>-1 then
  begin
     TRadioGroup(myControl).ItemIndex:=NewIndex;
  end;
  {$else}
  asm
  //alert('setitemvalue to '+AValue);
    var ob = document.getElementById(this.NameSpace+this.NodeName+AValue);
    if (ob!=null) {
       ob.checked=true;  }
   end;
  {$endif}
end;

procedure TXRadioBtns.SetOptionList(AValue:String);
var
  NewIndex:integer;
  myName,myCaption:string;
begin
  myNode.SetAttributeValue('OptionList',AValue);
  {$ifndef JScript}
  //TRadioGroup(myControl).items:=ListStringToStringList(AValue);
  TRadioGroup(myControl).items:=JSONStringToStringList(AValue);
  {$else}
  myName:=self.NameSpace+self.NodeName;
  myCaption:=myNode.GetAttribute('Caption',true).AttribValue;
  asm
    //alert('setoptionlist. AValue='+AValue);
    var ob = document.getElementById(myName+'Contents');
    if (ob!=null) {
      var Legend='<legend id='+myName+'ContentsLegend >"'+myCaption+'"</legend>';
      var ItemValue=ob.value;
      var Buttons=$mod.CreateButtonsList(this.myNode,AValue);
      //alert('setting innerHTML to '+Legend+Buttons);
      ob.innerHTML=Legend+Buttons;
    }
  end;
  {$endif}
end;

procedure TXRadioBtns.SetReadOnly(AValue:Boolean);
begin
  myNode.SetAttributeValue('ReadOnly',myBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  TRadioGroup(myControl).Enabled:=not AValue;
  {$else}
  asm
  //alert('setreadonly');
    var ob = document.getElementById(this.NameSpace+this.NodeName);
    if (ob!=null) {
    if (AValue==true) {ob.disabled = true}
    else {ob.disabled = false }  }
     // alert('setreadonly done');
  end;
  {$endif}
end;

procedure TXRadioBtns.SetBgColor(AValue:TColor);
begin
  inherited SetBgColor(AValue);

  {$ifndef JScript}
  TRadioGroup(myControl).Color:=AValue;
  {$endif}
end;

begin
  // this is the set of node attributes that each TXRadioBtns instance will have.
  AddWrapperDefaultAttribs(myDefaultAttribs);
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'BgColor','Color','#FFFFFF','',false);
  AddDefaultAttribute(myDefaultAttribs,'Caption','String','Radio Buttons','',false);
  AddDefaultAttribute(myDefaultAttribs,'OptionList','String','["Option 1","Option 2","Option 3"]','',false);
  AddDefaultAttribute(myDefaultAttribs,'ItemValue','String','Option 1','',false);
  AddDefaultAttribute(myDefaultAttribs,'ReadOnly','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'BoxWidth','String','300','',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  AddAttribOptions(MyNodeType,'LabelPos',LabelPosOptions);
  {$ifndef JScript}
  RegisterClass(TXRadioBtns);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}
  SuppressDesignerProperty(MyNodeType,'LabelPos');
  SuppressDesignerProperty(MyNodeType,'LabelText');
  SuppressDesignerProperty(MyNodeType,'ContainerHeight');
  SuppressDesignerProperty(MyNodeType,'ContainerWidth');
end.
end.
