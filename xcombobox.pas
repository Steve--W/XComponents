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
    constructor Create(MyForm:TForm;NodeName:String);
    {$endif}

  published
    { Published declarations }

    // Properties defined for this class...
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property ItemValue: String read GetItemValue write SetItemValue;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property BoxWidth: String read GetBoxWidth write SetBoxWidth;
    property OptionList: String read GetOptionList write SetOptionList;

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

  self.ParentColor:=true;
  // Setting IsContainer false will prevent designer dropping new child controls into this one.
  self.IsContainer:=false;

  AddLabel(myControl);

end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXComboBox',ParentNode.MyForm,ParentNode,ScreenObjectName,Alignment,position);
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
 end;


//procedure TXComboBox.LinkLoadFromProperty(Sender: TObject);
//begin
//  if Sender=nil then ;
//  if (Link.Editor=nil) then exit;
//  inherited  LinkLoadFromProperty(Sender);
//
//  self.ItemIndex:=Link.GetAsInt;
//
//end;
//
//procedure TXComboBox.LinkSaveToProperty(Sender: TObject);
//begin
//  if Sender=nil then ;
//  if Link.Editor=nil then exit;
//  Link.SetAsText(TEdit(myControl).Text);
//
//end;

procedure TXComboBox.SetBoxWidth(AValue:string);
 var
   tc:TControl;
 begin
   tc:=self.myControl;
  myNode.SetAttributeValue('BoxWidth',AValue);
  SetHeightWidth(self.myNode,tc,'BoxWidth','');
end;


{$else}
constructor TXComboBox.Create(MyForm:TForm;NodeName:String);
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
  ItemIndex,LabelText,LabelPos,OptionList:string;
  ReadOnly:Boolean;
  OnChangeString, OnClickString:String;
begin
//showmessage('createwidget combobox '+ScreenObjectName);
  OptionList:= MyNode.getAttribute('OptionList',true).AttribValue;
  ItemIndex:= MyNode.getAttribute('ItemIndex',true).AttribValue;
  LabelText:= MyNode.getAttribute('LabelText',true).AttribValue;
  ReadOnly:= StrToBool(MyNode.getAttribute('ReadOnly',true).AttribValue);

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''', this.value);" ';
  OnChangeString:= 'onchange="pas.NodeUtils.SetInterfaceProperty('''+ScreenObjectName+''',''ItemIndex'',pas.SysUtils.IntToStr(this.selectedIndex)); '+
                           'pas.Events.handleEvent(null,''Change'','''+ScreenObjectName+''', this.options[selectedIndex].value, ''ItemValue'');" ';

  asm
    try{
    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);

    var HTMLString='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var MyObjectName=ScreenObjectName+'Contents';

    var ReadOnlyString = '';
    if (ReadOnly==true) { ReadOnlyString = ' readonly ';}

    var TypeString = 'text';

    var inputtext= ItemIndex;
    var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'">'+LabelText+'</label>';
    var ComboString = '<select id="'+MyObjectName+'" ' + ReadOnlyString +
                  OnChangeString +
                  OnClickString +
                  ' style="display: inline-block;"   value='+ItemIndex+'> ';

     var optionlistarray=JSON.parse( OptionList);
     for (var i=0; i<optionlistarray.length; i++){
       var selectedflag ='';
       if (i==ItemIndex ){selectedflag = 'selected'}
       ComboString = ComboString +'<option value="'+optionlistarray[i]+'" '+selectedflag+'>'+optionlistarray[i]+'</option> ';
     }
     ComboString = ComboString +'</select> ';

    HTMLString = labelstring+ComboString;

    var wrapper=document.getElementById(ScreenObjectName);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);

    // attempt to fix the height for a combo box to one line-height... (still not displayed same as editbox...tbd!!!!)
    var dummyEBoxString = '<input type="'+TypeString+'"  id='+MyObjectName+'dummy ' +
                     ' style="display: inline-block;" >';
    wrapper.insertAdjacentHTML('beforeend', dummyEBoxString);
    var ob=document.getElementById(MyObjectName);
    var dum=document.getElementById(MyObjectName+'dummy');
    var obStyle = window.getComputedStyle(dum);
    ob.style.height = obStyle.getPropertyValue('line-height');
    //alert('combobox height='+ob.style.height);
    wrapper.removeChild(dum);
  }
  catch(err) { alert(err.message+'  in XComboBox.CreateWidget');}

end;

  MyNode.ScreenObject:=MyNode;

  // now that we have a datanode and a widget, cycle attribute settings
  RefreshComponentProps(myNode);

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName:String):TObject;
begin
  result:=TObject(TXComboBox.Create(MyForm,NodeName));
end;

//procedure TXComboBox.LinkLoadFromProperty(Sender: TObject);
//begin
//  inherited  LinkLoadFromProperty(Sender);
//end;
//
//procedure TXComboBox.LinkSaveToProperty(Sender: TObject);
//begin
//  if Sender=nil then ;
//  if Link=nil then exit;
//  if Link.TIObject=nil then exit;
////  showmessage('linksavetoproperty. '+Link.TIPropertyName+' '+self.ItemIndex);
//
//  SetStringProp(Link.TIObject,Link.TIPropertyName,intToStr(self.ItemIndex));
//end;


procedure TXComboBox.SetBoxWidth(AValue:string);
begin
  //showmessage('box width='+AValue);
  myNode.SetAttributeValue('BoxWidth',AValue);
  asm
  var ob = document.getElementById(this.NodeName+'Contents');
  //  if (ob==null) {alert(this.NodeName+'Contents'+'  not found');}
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
  end;
end;


{$endif}

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
    var ob = document.getElementById(this.NodeName+'Contents');
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
    var ob = document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
       ob.selectedIndex=AValue;
       this.myNode.SetAttributeValue('ItemValue',ob.value,'String',false);
       }
  end;
  //LinkSaveToProperty(self);
  {$endif}
end;

procedure TXComboBox.SetItemValue(AValue:string);
var
  i:integer;
  options:TStringList;
  tmp:string;
begin
  tmp:=self.OptionList;
  //options:=ListStringToStringList(self.OptionList);
  options:=JSONStringToStringList(self.OptionList);
  //json test...
  tmp:=StringListToJsonString(options);

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
    var ob = document.getElementById(this.NodeName+'Contents');
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

    var ob = document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
      removeOptions(ob);
      var selectedIndex = this.ItemIndex;

      var optionlistarray=JSON.parse( AValue);
      for (var i=0; i<optionlistarray.length; i++){
        var option = document.createElement("option");
        option.text = optionlistarray[i];
        if (i==this.ItemIndex ){option.selected=true;}
        ob.add(option);
      }
    }
  end;
  {$endif}
end;


begin
  // this is the set of node attributes that each TXComboBox instance will have.
  AddDefaultAttribute(myDefaultAttribs,'Alignment','String','Left','',false);
  AddDefaultAttribute(myDefaultAttribs,'Hint','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'IsVisible','Boolean','True','',false);
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
