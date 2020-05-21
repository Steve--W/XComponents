(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XComboBox;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, TypInfo, NodeUtils, StringUtils,
  {$ifndef JScript}
  LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, Propedits,RTTICtrls,
  LazsUtils, Events,
  {$else}
  HTMLUtils,
  {$endif}
  WrapperPanel;

type
  TXComboBox = class(TWrapperPanel)
  private
    { Private declarations }
    fPriorIndex:integer;
    {$ifndef JScript}
    fHandleClick:TEventHandler;
    fHandleChange:TEventHandler;
    procedure ComboBoxclick(Sender:TObject);
    procedure ComboBoxChange(Sender: TObject);
    {$endif}

    procedure SetMyEventTypes;

    function GetItemIndex:integer;
    function GetItemValue:string;
    function GetReadOnly:Boolean;
    function GetBoxWidth:string;
    function GetOptionList:string;

    procedure SetItemIndex(AValue:integer);
    procedure SetItemValue(AValue:string);
    procedure SetReadOnly(AValue:Boolean);
    procedure SetBoxWidth(AValue:string);
    procedure SetOptionList(AValue:string);

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
    constructor Create(MyForm:TForm;NodeName,NameSpace:String);
    {$endif}
    function IndexOfOption(AValue:String):integer;

  published
    { Published declarations }

    // Properties defined for this class...
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property ItemValue: String read GetItemValue write SetItemValue;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property BoxWidth: String read GetBoxWidth write SetBoxWidth;
    property OptionList: String read GetOptionList write SetOptionList;
    property PriorIndex:integer read fPriorIndex write fPriorIndex;

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

const MyNodeType='TXComboBox';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXComboBox.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
  MyEventTypes.Add('Change');
end;

{$ifndef JScript}
procedure Register;
begin
  RegisterComponents('XComponents',[TXComboBox]);

  RegisterPropertyEditor(TypeInfo(String), TXComboBox, 'ItemValue', THiddenPropertyEditor);
    // suppress some of the Link properties
//    RegisterPropertyEditor(TypeInfo(TAliasStrings), TXPropertyLink, 'AliasValues', THiddenPropertyEditor);
//    RegisterPropertyEditor(TypeInfo(String), TXPropertyLink, 'TIElementName', THiddenPropertyEditor);
//    RegisterPropertyEditor(TypeInfo(TPropertyLinkOptions), TXPropertyLink, 'Options', THiddenPropertyEditor);
end;

constructor TXComboBox.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXComboBox.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXComboBox.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  self.BorderSpacing.Around:=glbBorderSpacing;

  myControl:=TComboBox.Create(self);
  myControl.Parent:=self;

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];


  TComboBox(myControl).OnEditingDone:=@self.myeditingDone;

  TComboBox(myControl).OnChange:=@self.ComboBoxChange;
  TComboBox(myControl).OnClick:=@self.ComboBoxClick;

  self.SetMyEventTypes;

  CreateComponentDataNode2(self,MyNodeType,myDefaultAttribs, self.myEventTypes, TheOwner,IsDynamic);

  fPriorIndex:=0;
  self.ParentColor:=true;
  // Setting IsContainer false will prevent designer dropping new child controls into this one.
  self.IsContainer:=false;

  AddLabel(myControl);

end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXComboBox',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;


procedure TXComboBox.ComboBoxClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;

procedure TXComboBox.ComboBoxChange(Sender: TObject) ;
 var
    ComboBox: TComboBox ;
 begin
    ComboBox := TComboBox(sender) ;
    self.ItemIndex:=ComboBox.ItemIndex;
    CallHandleEvent('Change',self.ItemValue,Sender);
    fPriorIndex:=ComboBox.ItemIndex;
 end;



procedure TXComboBox.SetBoxWidth(AValue:string);
 var
   tc:TControl;
 begin
   tc:=self.myControl;
  myNode.SetAttributeValue('BoxWidth',AValue);
  SetHeightWidth(self.myNode,tc,'BoxWidth','');
end;


{$else}
constructor TXComboBox.Create(MyForm:TForm;NodeName,NameSpace:String);
begin
  inherited Create(NodeName,NameSpace);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=false;
  fPriorIndex:=0;

  SetNodePropDefaults(self,myDefaultAttribs);

end;

function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  ItemIndex,LabelText,LabelPos,OptionList:string;
  ReadOnly:Boolean;
  OnChangeString, OnClickString:String;
begin
//showmessage('createwidget combobox '+ScreenObjectName);
  OptionList:= MyNode.getAttribute('OptionList',true).AttribValue;
  ItemIndex:= MyNode.getAttribute('ItemIndex',true).AttribValue;
  LabelText:= MyNode.getAttribute('LabelText',true).AttribValue;
  ReadOnly:= StrToBool(MyNode.getAttribute('ReadOnly',true).AttribValue);

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''','''+NameSpace+''', this.value);" ';
  OnChangeString:= 'onchange="pas.NodeUtils.SetInterfaceProperty('''+ScreenObjectName+''','''+NameSpace+''',''ItemIndex'',pas.SysUtils.IntToStr(this.selectedIndex)); '+
                           'pas.Events.handleEvent(null,''Change'','''+ScreenObjectName+''','''+NameSpace+''', this.options[selectedIndex].value, ''ItemValue'');'+
                           'pas.NodeUtils.SetInterfaceProperty('''+ScreenObjectName+''','''+NameSpace+''',''PriorIndex'',pas.SysUtils.IntToStr(this.selectedIndex)); '+
                           '" ';

  asm
    try{
    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,NameSpace,$impl.MyNodeType,position);

    var HTMLString='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var wrapperid =  NameSpace+ScreenObjectName;
    var MyObjectName=wrapperid+'Contents';

    var ReadOnlyString = '';
    if (ReadOnly==true) { ReadOnlyString = ' readonly ';}

    var TypeString = 'text';

    var inputtext= ItemIndex;
    var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'">'+LabelText+'</label>';
    var ComboString = '<select id="'+MyObjectName+'" ' + ReadOnlyString +
                  OnChangeString +
                  OnClickString +
                  ' class="widgetinner '+wrapperid+'" ' +
                  ' style="display: inline-block;"   value='+ItemIndex+'> ';

     var optionlistarray=JSON.parse( OptionList);
     for (var i=0; i<optionlistarray.length; i++){
       var selectedflag ='';
       if (i==ItemIndex ){selectedflag = 'selected'}
       ComboString = ComboString +'<option value="'+optionlistarray[i]+'" '+selectedflag+'>'+optionlistarray[i]+'</option> ';
     }
     ComboString = ComboString +'</select> ';

    HTMLString = labelstring+ComboString;

    var wrapper=document.getElementById(wrapperid);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);

    // attempt to fix the height for a combo box to one line-height... (still not displayed same as editbox...tbd!!!!)
//    var dummyEBoxString = '<input type="'+TypeString+'"  id='+MyObjectName+'dummy ' +
//                     ' class="widgetinner '+wrapperid+'" ' +
//                     ' style="display: inline-block;" >';
//    wrapper.insertAdjacentHTML('beforeend', dummyEBoxString);

    pas.HTMLUtils.FixHeightToLineHeight(MyObjectName);

//    var ob=document.getElementById(MyObjectName);
//    var dum=document.getElementById(MyObjectName+'dummy');
//    var obStyle = window.getComputedStyle(dum);
//    ob.style.height = obStyle.getPropertyValue('line-height');
//    //alert('combobox height='+ob.style.height);
//    wrapper.removeChild(dum);
  }
  catch(err) { alert(err.message+'  in XComboBox.CreateWidget');}

end;

  MyNode.ScreenObject:=MyNode;

  // now that we have a datanode and a widget, cycle attribute settings
  RefreshComponentProps(myNode);

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXComboBox.Create(MyForm,NodeName,NameSpace));
end;


procedure TXComboBox.SetBoxWidth(AValue:string);
begin
  //showmessage('box width='+AValue);
  myNode.SetAttributeValue('BoxWidth',AValue);
  asm
  var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
  //  if (ob==null) {alert(this.NodeName+'Contents'+'  not found');}
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
  end;
end;


{$endif}

function TXComboBox.IndexOfOption(AValue:String):integer;
var
  i:integer;
  options:TStringList;
begin
  options:=JSONStringToStringList(self.OptionList);
  i:=options.IndexOf(AValue);
  result:=i;
end;

function TXComboBox.GetBoxWidth:string;
begin
  result:=MyNode.getAttribute('BoxWidth',true).AttribValue;
end;
function TXComboBox.GetOptionList:string;
begin
  result:=MyNode.getAttribute('OptionList',true).AttribValue;
end;
function TXComboBox.GetItemIndex:integer;
begin
  result:=StrToInt(MyNode.getAttribute('ItemIndex',true).AttribValue);
end;
function TXComboBox.GetItemValue:string;
var
  i:integer;
  val:String;
begin
  i:=strToInt(MyNode.getAttribute('ItemIndex',true).AttribValue);
  if i>-1 then
  begin
    {$ifndef JScript}
    val:=TComboBox(myControl).Items[i];
    {$else}
    asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
      val=ob.options[i].value;
    }
    end;
    {$endif}
    result:=val;
  end;
end;
function TXComboBox.GetReadOnly:Boolean;
begin
  result:=MyStrToBool(MyNode.getAttribute('ReadOnly',true).AttribValue);
end;

procedure TXComboBox.SetItemIndex(AValue:integer);
begin
  myNode.SetAttributeValue('ItemIndex',intToStr(AValue),'Integer');
  {$ifndef JScript}
  TComboBox(myControl).ItemIndex:=AValue;
  myNode.SetAttributeValue('ItemValue',TComboBox(myControl).Text,'String');
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
       ob.selectedIndex=AValue;
       this.myNode.SetAttributeValue('ItemValue',ob.value,'String',false);
       }
  end;
  {$endif}
end;

procedure TXComboBox.SetItemValue(AValue:string);
var
  i:integer;
  options:TStringList;
begin
  options:=JSONStringToStringList(self.OptionList);

  i:=options.IndexOf(AValue);
  self.ItemIndex:=i;
end;


procedure TXComboBox.SetReadOnly(AValue:Boolean);
begin
  myNode.SetAttributeValue('ReadOnly',myBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  TLabeledEdit(myControl).ReadOnly:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
      ob.readOnly = AValue  }
  end;
  {$endif}
end;

procedure TXComboBox.SetOptionList(AValue:String);
begin
  myNode.SetAttributeValue('OptionList',AValue,'String');
  {$ifndef JScript}
  //TComboBox(myControl).items:=ListStringToStringList(AValue);
  TComboBox(myControl).items:=JSONStringToStringList(AValue);
  if self.ItemIndex>-1 then
    self.ItemIndex:=self.ItemIndex;   // to reset the displayed itemvalue
  {$else}
  asm
    function removeOptions(selectbox)
    {
        var i;
        for(i = selectbox.options.length - 1 ; i >= 0 ; i--)
        {
            selectbox.remove(i);
        }
    }

    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
      removeOptions(ob);
      var selectedIndex = this.ItemIndex;

      try {
      var optionlistarray=JSON.parse( AValue);
      for (var i=0; i<optionlistarray.length; i++){
        var option = document.createElement("option");
        option.text = optionlistarray[i];
        if (i==this.ItemIndex ){option.selected=true;}
        ob.add(option);
      }
      }
      catch(err) {  alert("Error in TXComboBox.SetOptionList: "+ err.message); };
    }
  end;
  {$endif}
end;


begin
  // this is the set of node attributes that each TXComboBox instance will have.
  AddWrapperDefaultAttribs(myDefaultAttribs);
  AddDefaultAttribute(myDefaultAttribs,'BoxWidth','String','200','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','Right','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelText','String','Combo Box','',false);
  AddDefaultAttribute(myDefaultAttribs,'ReadOnly','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'ItemIndex','Integer','-1','',false);
  AddDefaultAttribute(myDefaultAttribs,'OptionList','String','["Option 1","Option 2","Option 3"]','',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  AddAttribOptions(MyNodeType,'LabelPos',LabelPosOptions);
  {$ifndef JScript}
  {$I XComboBox.lrs}
  RegisterClass(TXComboBox);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}
  SuppressDesignerProperty('TXComboBox','ItemValue');
  SuppressDesignerProperty('TXComboBox','BgColor');
  SuppressDesignerProperty(MyNodeType,'ContainerHeight');
  SuppressDesignerProperty(MyNodeType,'ContainerWidth');

end.
