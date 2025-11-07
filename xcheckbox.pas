(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XCheckBox;
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


type
  TXCheckBox = class(TWrapperPanel)
  private
    { Private declarations }
    {$ifndef JScript}
    fHandleClick:TEventHandler;
    procedure CheckBoxclick(Sender:TObject);
    procedure CheckBoxChange(Sender:TObject);
    {$endif}

    procedure SetMyEventTypes;

    function GetChecked:Boolean;
    function GetReadOnly:Boolean;

    procedure SetChecked(AValue:Boolean);
    procedure SetReadOnly(AValue:Boolean);

  protected
    { Protected declarations }
 //   procedure LinkLoadFromProperty(Sender: TObject);  override;
 //   procedure LinkSaveToProperty(Sender: TObject);  override;
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
    property Checked: Boolean read GetChecked write SetChecked;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;

    {$ifndef JScript}
    // Events to be visible in Lazarus IDE
    property HandleClick: TEventHandler read FHandleClick write FHandleClick;
    {$endif}
  end;

  {$ifndef JScript}
  procedure Register;
  {$endif}


implementation

const MyNodeType='TXCheckBox';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXCheckBox.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
  //MyEventTypes.Add('Change');
end;

{$ifndef JScript}
procedure Register;
begin
  {$I xcheckbox_icon.lrs}
  RegisterComponents('XComponents',[TXCheckBox]);

  // inherited from TWrapperPanel, not required here
  RegisterPropertyEditor(TypeInfo(String), TXCheckBox, 'ContainerHeight', THiddenPropertyEditor);

  // suppress some of the link properties
//  RegisterPropertyEditor(TypeInfo(TAliasStrings), TXPropertyLink, 'AliasValues', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(String), TXPropertyLink, 'TIElementName', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(TPropertyLinkOptions), TXPropertyLink, 'Options', THiddenPropertyEditor);
end;

constructor TXCheckBox.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXCheckBox.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXCheckBox.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  self.BorderSpacing.Around:=glbBorderSpacing;

  myControl:=TCheckBox.Create(self);
  myControl.Parent:=self;

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];

  myControl.Constraints.MaxHeight:=20;
  myControl.Constraints.MaxWidth:=20;

  TCheckBox(myControl).OnEditingDone:=@self.myeditingDone;
  myControl.OnClick:=@self.CheckBoxClick;
  TCheckBox(myControl).OnChange:=@self.CheckBoxChange;

  self.SetMyEventTypes;

  CreateComponentDataNode2(self,MyNodeType,myDefaultAttribs, self.myEventTypes, TheOwner,IsDynamic);

  self.ParentColor:=true;
  // Setting IsContainer false will prevent designer dropping new child controls into this one.
  self.IsContainer:=false;

  AddLabel(myControl,self.LabelText);

end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXCheckBox',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;

procedure TXCheckBox.CheckBoxChange(Sender: TObject) ;
var
  itemchecked:string;
begin
  itemchecked:=MyBooltostr(TCheckBox(Sender).checked);
 // CallHandleEvent('Change',itemchecked,Sender);

end;

procedure TXCheckBox.CheckBoxClick(Sender: TObject) ;
var
  itemchecked:string;
begin
  if not (csDesigning in componentState) then
  begin
    itemchecked:=MyBooltostr(TCheckBox(Sender).checked);
    self.Checked:=TCheckBox(Sender).checked;
    CallHandleEvent('Click',itemchecked,self);
  end;
end;

{$else}

constructor TXCheckBox.Create(MyForm:TForm;NodeName,NameSpace:String);
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
  Checked,ReadOnly,LabelText,LabelPos:string;
  OnChangeString, OnClickString, OnPasteString:String;
begin
  Checked:= MyNode.getAttribute('Checked',true).AttribValue;
  LabelText:= MyNode.getAttribute('LabelText',true).AttribValue;
  ReadOnly:= MyNode.getAttribute('ReadOnly',true).AttribValue;

  OnClickString:='onclick="if (this.checked!=undefined) {' +
                          'pas.NodeUtils.SetInterfaceProperty('''+ScreenObjectName+''','''+NameSpace+''',''Checked'',this.checked.toString());' +
                          'event.stopPropagation(); ' +
                          'pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''','''+NameSpace+''', this.checked.toString());}"';

  asm
    try{
    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,NameSpace,$impl.MyNodeType,position);
    wrapper.style.display = 'flex';
    var goright =  'flex-e'+'nd';

    var HTMLString='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var wrapperid =  NameSpace+ScreenObjectName;
    var MyObjectName=wrapperid+'Contents';

    var ReadOnlyString = '';
    if (ReadOnly=='True') { ReadOnlyString = ' readonly ';}

    var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'">'+LabelText+'</label>';

    var Checkstring = '';
    if (Checked == 'true'){Checkstring = 'checked'};

    var CheckBoxString = '<input  type="checkbox" id='+MyObjectName+ ' '+
                       ' class="widgetinner '+wrapperid+'" '+
                       OnClickString +
                       Checkstring +
                       ' style="display:inline-block;" '+ReadOnlyString+' >' ;

    HTMLString = labelstring+CheckBoxString;

    var wrapper=document.getElementById(wrapperid);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);

    pas.HTMLUtils.FixHeightToLineHeight(ScreenObjectName);
  }
  catch(err) { alert(err.message+'  in XCheckBox.CreateXCheckBox');}

end;

  MyNode.ScreenObject:=MyNode;
  RefreshComponentProps(myNode);

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXCheckBox.Create(MyForm,NodeName,NameSpace));
end;

//procedure TXCheckBox.LinkLoadFromProperty(Sender: TObject);
//begin
//  inherited  LinkLoadFromProperty(Sender);
//end;
//
//procedure TXCheckBox.LinkSaveToProperty(Sender: TObject);
//begin
//  if Sender=nil then ;
//  if Link=nil then exit;
//  if Link.TIObject=nil then exit;
////  showmessage('linksavetoproperty. '+Link.TIPropertyName+' '+myBoolToStr(self.Checked));
//
//  SetBoolProp(Link.TIObject,Link.TIPropertyName,self.Checked);
//end;

{$endif}


function TXCheckBox.GetChecked:Boolean;
begin
  //showmessage('getchecked');
  result:=MyStrToBool(MyNode.getAttribute('Checked',true).AttribValue);
end;
function TXCheckBox.GetReadOnly:Boolean;
begin
  result:=MyStrToBool(MyNode.getAttribute('ReadOnly',true).AttribValue);
end;

procedure TXCheckBox.SetChecked(AValue:Boolean);
begin
  myNode.SetAttributeValue('Checked',MyBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  TCheckBox(myControl).Checked:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
       ob.checked=AValue;  }
  end;
//  LinkSaveToProperty(self);
  {$endif}
end;

procedure TXCheckBox.SetReadOnly(AValue:Boolean);
begin
  myNode.SetAttributeValue('ReadOnly',myBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  TCheckBox(myControl).Enabled:=not AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
    if (AValue==true) {ob.disabled = true}
    else {ob.disabled = false }  }
  end;
  {$endif}
end;

begin
  // this is the set of node attributes that each XCheckBox instance will have.
  AddWrapperDefaultAttribs(myDefaultAttribs);
  AddDefaultAttribute(myDefaultAttribs,'ContainerWidth','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','Right','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelText','String','Checkbox','',false);
  AddDefaultAttribute(myDefaultAttribs,'ReadOnly','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'Checked','Boolean','False','',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  AddAttribOptions(MyNodeType,'LabelPos',LabelPosOptions);
  {$ifndef JScript}
  RegisterClass(TXCheckBox);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}
  SuppressDesignerProperty(MyNodeType,'BgColor');
  SuppressDesignerProperty(MyNodeType,'ContainerHeight');
  SuppressDesignerProperty(MyNodeType,'ContainerWidth');
end.

