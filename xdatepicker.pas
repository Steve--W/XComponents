(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XDatePicker;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, TypInfo, NodeUtils, StringUtils,
  {$ifndef JScript}
  LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, DateTimePicker, ExtCtrls, Propedits,RTTICtrls,
  LazsUtils, Events,
  {$else}
  HTMLUtils,
  {$endif}
  WrapperPanel;

type
  TXDatePicker = class(TWrapperPanel)
  private
    { Private declarations }
    {$ifndef JScript}
    fHandleClick:TEventHandler;
    fHandleChange:TEventHandler;
    procedure Pickerclick(Sender:TObject);
    procedure PickerChange(Sender: TObject);
    {$endif}

    procedure SetMyEventTypes;

    function GetItemValue:string;
    function GetReadOnly:Boolean;
    function GetBoxWidth:string;

    procedure SetItemValue(AValue:string);
    procedure SetReadOnly(AValue:Boolean);
    procedure SetBoxWidth(AValue:string);

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

  published
    { Published declarations }

    // Properties defined for this class...
    property ItemValue: String read GetItemValue write SetItemValue;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
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

const MyNodeType='TXDatePicker';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXDatePicker.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
  MyEventTypes.Add('Change');
end;

{$ifndef JScript}
procedure Register;
begin
  {$I xdatepicker_icon.lrs}
  RegisterComponents('XComponents',[TXDatePicker]);

  // suppress some of the Link properties
//  RegisterPropertyEditor(TypeInfo(TAliasStrings), TXPropertyLink, 'AliasValues', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(String), TXPropertyLink, 'TIElementName', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(TPropertyLinkOptions), TXPropertyLink, 'Options', THiddenPropertyEditor);
end;

constructor TXDatePicker.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXDatePicker.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXDatePicker.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  self.BorderSpacing.Around:=glbBorderSpacing;

  myControl:=TDateTimePicker.Create(self);
  myControl.Parent:=self;

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];


  TDateTimePicker(myControl).OnEditingDone:=@self.myeditingDone;

  TDateTimePicker(myControl).OnChange:=@self.PickerChange;
  TDateTimePicker(myControl).OnClick:=@self.PickerClick;

  self.SetMyEventTypes;

  CreateComponentDataNode2(self,MyNodeType,myDefaultAttribs, self.myEventTypes, TheOwner,IsDynamic);

  self.ParentColor:=true;
  // Setting IsContainer false will prevent designer dropping new child controls into this one.
  self.IsContainer:=false;

  AddLabel(myControl,self.LabelText);

  ItemValue:=DateToStr(TDateTimePicker(myControl).Date);

end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXDatePicker',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;


procedure TXDatePicker.PickerClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;

procedure TXDatePicker.PickerChange(Sender: TObject) ;
 var
    dtp: TDateTimePicker ;
 begin
    dtp := TDateTimePicker(sender) ;
    self.ItemValue:=DateToStr(dtp.Date);
    CallHandleEvent('Change',self.ItemValue,Sender);
 end;


//procedure TXDatePicker.LinkLoadFromProperty(Sender: TObject);
//begin
//  if Sender=nil then ;
//  if (Link.Editor=nil) then exit;
//  inherited  LinkLoadFromProperty(Sender);
//
//  self.ItemValue:=Link.GetAsText;
//
//end;
//
//procedure TXDatePicker.LinkSaveToProperty(Sender: TObject);
//begin
//  if Sender=nil then ;
//  if Link.Editor=nil then exit;
//  Link.SetAsText(TEdit(myControl).Text);
//
//end;

procedure TXDatePicker.SetBoxWidth(AValue:string);
 var
   tc:TControl;
 begin
   tc:=self.myControl;
  myNode.SetAttributeValue('BoxWidth',AValue);
  SetHeightWidth(self.myNode,tc,'BoxWidth','');
end;


{$else}
constructor TXDatePicker.Create(MyForm:TForm;NodeName,NameSpace:String);
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
  ItemValue,LabelText,LabelPos:string;
  ReadOnly:Boolean;
  OnChangeString, OnClickString:String;
begin
  ItemValue:= MyNode.getAttribute('ItemValue',true).AttribValue;
  LabelText:= MyNode.getAttribute('LabelText',true).AttribValue;
  ReadOnly:= StrToBool(MyNode.getAttribute('ReadOnly',true).AttribValue);

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''','''+NameSpace+''', this.value.toString());" ';
  OnChangeString:= 'onchange="pas.NodeUtils.SetInterfaceProperty('''+ScreenObjectName+''','''+NameSpace+''',''ItemValue'',this.value.toString()); '+
                             'pas.Events.handleEvent(null,''Change'','''+ScreenObjectName+''','''+NameSpace+''', this.value.toString(), ''ItemValue'');" ';

  asm
    try{
    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,NameSpace,$impl.MyNodeType,position);

    var HTMLString='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var wrapperid =  NameSpace+ScreenObjectName;
    var MyObjectName=wrapperid+'Contents';

    var ReadOnlyString = '';
    if (ReadOnly==true) { ReadOnlyString = ' readonly ';}

    var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'">'+LabelText+'</label>';
    var PickerString = '<input type="date" id='+MyObjectName+' '+
                       ' class="widgetinner '+wrapperid+'" ' +
                        OnClickString +
                        OnChangeString +
                        ' style="display: inline-block;"   '+ReadOnlyString+'> ';

    HTMLString = labelstring+PickerString;

    var wrapper=document.getElementById(wrapperid);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);

    pas.HTMLUtils.FixHeightToLineHeight(MyObjectName);
  }
  catch(err) { alert(err.message+'  in XDatePicker.CreateWidget');}

end;

  MyNode.ScreenObject:=MyNode;

  // now that we have a datanode and a widget, cycle attribute settings
  RefreshComponentProps(myNode);

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXDatePicker.Create(MyForm,NodeName,NameSpace));
end;

//procedure TXDatePicker.LinkLoadFromProperty(Sender: TObject);
//begin
//  inherited  LinkLoadFromProperty(Sender);
//end;
//
//procedure TXDatePicker.LinkSaveToProperty(Sender: TObject);
//begin
//  if Sender=nil then ;
//  if Link=nil then exit;
//  if Link.TIObject=nil then exit;
////  showmessage('linksavetoproperty. '+Link.TIPropertyName+' '+self.ItemValue);
//
//  SetStringProp(Link.TIObject,Link.TIPropertyName,self.ItemValue);
//end;

procedure TXDatePicker.SetBoxWidth(AValue:string);
begin
  myNode.SetAttributeValue('BoxWidth',AValue);
  asm
  var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
  end;
end;


{$endif}

function TXDatePicker.GetBoxWidth:string;
begin
  result:=MyNode.getAttribute('BoxWidth',true).AttribValue;
end;
function TXDatePicker.GetItemValue:string;
begin
  result:=MyNode.getAttribute('ItemValue',true).AttribValue;
end;
function TXDatePicker.GetReadOnly:Boolean;
begin
  result:=MyStrToBool(MyNode.getAttribute('ReadOnly',true).AttribValue);
end;

procedure TXDatePicker.SetItemValue(AValue:string);
begin
  {$ifndef JScript}
  myNode.SetAttributeValue('ItemValue',AValue);
  TDateTimePicker(myControl).Date:=StrToDate(AValue);
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
        //alert('set '+AValue);
        // format is DD/MM/YYYY from the property editor, OR YYYY-MM-DD from the picker widget
        var bits = AValue.split('/');
        if (bits.length > 1) {
        var mmddyyyy = bits[1]+'/'+bits[0]+'/'+bits[2];
        }
        else
        {
          bits = AValue.split('-');
          mmddyyyy = bits[1]+'/'+bits[2]+'/'+bits[0];
        }
        //alert('formatted '+mmddyyyy);
        var date = new Date(mmddyyyy);   // use format MM/DD/YYYY

        var month = date.getMonth() +1;
        var monthstr = month.toString();
        if (month<10) {monthstr='0'+monthstr;}

        var day = date.getDate();
        var daystr = day.toString();
        if (day<10) {daystr='0'+daystr;}

        var year = date.getFullYear();
        //alert(daystr+'/'+monthstr+'/'+year);

        ob.value=year+'-'+monthstr+'-'+daystr;      // wants format YYYY-MM-DD
        this.SetAttributeValue('ItemValue',daystr+'/'+monthstr+'/'+year,'String',false); // set property format DD/MM/YYYY
        }
  end;
  //LinkSaveToProperty(self);
  {$endif}
end;

procedure TXDatePicker.SetReadOnly(AValue:Boolean);
begin
  myNode.SetAttributeValue('ReadOnly',myBoolToStr(AValue),'Boolean');

  {$ifndef JScript}
  TDateTimePicker(myControl).ReadOnly:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
      ob.readOnly = AValue  }
  end;
  {$endif}
end;


begin
  // this is the set of node attributes that each XHBox instance will have.
  AddWrapperDefaultAttribs(myDefaultAttribs);
  AddDefaultAttribute(myDefaultAttribs,'BoxWidth','String','200','',false);
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','Right','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelText','String','Date Picker','',false);
  AddDefaultAttribute(myDefaultAttribs,'ReadOnly','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'ItemValue','String','01/01/2000','',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);
  //ItemValue:=DateToStr(TDateTimePicker(myControl).Date);

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  AddAttribOptions(MyNodeType,'LabelPos',LabelPosOptions);
  {$ifndef JScript}
  RegisterClass(TXDatePicker);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}
  SuppressDesignerProperty('TXDatePicker','BgColor');
  SuppressDesignerProperty(MyNodeType,'ContainerHeight');
  SuppressDesignerProperty(MyNodeType,'ContainerWidth');


end.
