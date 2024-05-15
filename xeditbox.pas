(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XEditBox;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, TypInfo, NodeUtils,StringUtils,
  {$ifndef JScript}
  LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, Propedits,RTTICtrls,
  LazsUtils, Events,
  {$else}
  HTMLUtils,
  {$endif}
  WrapperPanel;


type
  TXEditBox = class(TWrapperPanel)
  private
    { Private declarations }
    {$ifndef JScript}
    fHandleClick:TEventHandler;
    fHandleChange:TEventHandler;
    procedure EditBoxclick(Sender:TObject);
    procedure EditBoxChange(Sender: TObject);
    {$endif}

    procedure SetMyEventTypes;


    function GetItemValue:string;
    function GetReadOnly:Boolean;
    function GetBoxWidth:string;
    function GetPasswordBox:Boolean;
    function GetHasFocus:Boolean;
    function GetIncludeDataInSave:Boolean;

    procedure SetItemValue(AValue:string);
    procedure SetReadOnly(AValue:Boolean);
    procedure SetBoxWidth(AValue:string);
    procedure SetPasswordBox(AValue:Boolean);
    procedure SetHasFocus(AValue:Boolean);
    procedure SetIncludeDataInSave(AValue:Boolean);

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
    property PasswordBox: Boolean read GetPasswordBox write SetPasswordBox;
    property HasFocus: Boolean read GetHasFocus write SetHasFocus;
    property IncludeDataInSave:Boolean read GetIncludeDataInSave write SetIncludeDataInSave;

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

const MyNodeType='TXEditBox';
var
  myDefaultAttribs:TDefaultAttributesArray;


procedure TXEditBox.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
  MyEventTypes.Add('Change');
  {$ifdef JScript}
  MyEventTypes.Add('EditBoxPaste');
  {$endif}
end;

{$ifndef JScript}
procedure Register;
begin
  // Lazarus IDE component registration
  RegisterComponents('XComponents',[TXEditBox]);

  // suppress some of the Link properties
//  RegisterPropertyEditor(TypeInfo(TAliasStrings), TXPropertyLink, 'AliasValues', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(String), TXPropertyLink, 'TIElementName', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(TPropertyLinkOptions), TXPropertyLink, 'Options', THiddenPropertyEditor);
end;

constructor TXEditBox.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXEditBox.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXEditBox.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  self.BorderSpacing.Around:=glbBorderSpacing;

  myControl:=TEdit.Create(self);
  myControl.Parent:=self;

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];


  TEdit(myControl).OnEditingDone:=@self.myeditingDone;

  TEdit(myControl).OnExit:=@self.EditBoxChange;
  TEdit(myControl).OnClick:=@self.EditBoxClick;

  self.SetMyEventTypes;

  CreateComponentDataNode2(self,MyNodeType,myDefaultAttribs, self.myEventTypes, TheOwner,IsDynamic);

  self.ParentColor:=true;
  // Setting IsContainer false will prevent designer dropping new child controls into this one.
  self.IsContainer:=false;

  AddLabel(myControl);


end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXEditBox',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;

procedure TXEditBox.EditBoxClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;

procedure TXEditBox.EditBoxChange(Sender: TObject) ;
 var
    EditBox: TEdit ;
 begin
    EditBox := TEdit(sender) ;
    if EditBox.text <> self.ItemValue then
    begin
      self.ItemValue:=EditBox.text;
      CallHandleEvent('Change',EditBox.text,Sender);
    end;
 end;

procedure TXEditBox.SetBoxWidth(AValue:string);
 var
   tc:TControl;
 begin
   tc:=self.myControl;
  myNode.SetAttributeValue('BoxWidth',AValue);
  SetHeightWidth(self.myNode,tc,'BoxWidth','');
end;

{$else}

constructor TXEditBox.Create(MyForm:TForm;NodeName,NameSpace:String);
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
  ReadOnly,PasswordBox:Boolean;
  OnChangeString, OnClickString, OnPasteString:String;
begin
  ItemValue:= MyNode.getAttribute('ItemValue',true).AttribValue;
  LabelText:= MyNode.getAttribute('LabelText',true).AttribValue;
  ReadOnly:= StrToBool(MyNode.getAttribute('ReadOnly',true).AttribValue);
  PasswordBox:= StrToBool(MyNode.getAttribute('PasswordBox',true).AttribValue);

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''','''+NameSpace+''', this.value);" ';
  OnChangeString:= 'onchange="pas.NodeUtils.SetInterfaceProperty('''+ScreenObjectName+''','''+NameSpace+''',''ItemValue'',this.value); '+
                             'pas.Events.handleEvent(null,''Change'','''+ScreenObjectName+''','''+NameSpace+''', this.value, ''ItemValue'');" ';
  OnPasteString:= 'onpaste="pas.Events.handleEvent(null,''EditBoxPaste'','''+ScreenObjectName+''','''+NameSpace+''', this.value);" ';

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
    if (PasswordBox) { TypeString = 'password';}

    var inputtext= ItemValue;
    var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'">'+LabelText+'</label>';
    var EBoxString = '<input type="'+TypeString+'"  id='+MyObjectName+' ' +
                     ' class="widgetinput '+wrapperid+'" ' +
                          OnPasteString +
                          OnClickString +
                          OnChangeString +
                 ' style="display: inline-block; padding:0px;'+
                 '" value="'+inputtext+'"'+ReadOnlyString+'>' ;

    HTMLString = labelstring+EBoxString;

    var wrapper=document.getElementById(wrapperid);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);

    // fix the height for an edit box to one line-height...
    pas.HTMLUtils.FixHeightToLineHeight(MyObjectName);
  }
  catch(err) { alert(err.message+'  in XEditBox.CreateXEditBox');}

end;

  MyNode.ScreenObject:=MyNode;

  // now that we have a datanode and a widget, cycle attribute settings
  RefreshComponentProps(myNode);

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXEditBox.Create(MyForm,NodeName,NameSpace));
end;

//procedure TXEditBox.LinkLoadFromProperty(Sender: TObject);
//begin
//  inherited  LinkLoadFromProperty(Sender);
//end;
//
//procedure TXEditBox.LinkSaveToProperty(Sender: TObject);
//begin
//  if Sender=nil then ;
//  if Link=nil then exit;
//  if Link.TIObject=nil then exit;
////  showmessage('linksavetoproperty. '+Link.TIPropertyName+' '+self.ItemValue);
//
//  SetStringProp(Link.TIObject,Link.TIPropertyName,self.ItemValue);
//end;


procedure TXEditBox.SetBoxWidth(AValue:string);
var
  perc:Boolean;
begin
  //showmessage('memo width='+AValue);
  myNode.SetAttributeValue('BoxWidth',AValue);
  if FoundString(AValue,'%')>0 then
    perc:=true
  else
    perc:=false;
  asm
  // px width must apply to the edit box (id=name+'Contents'),
  // but a percentage width must apply to the container (id=name)
  var con = document.getElementById(this.NameSpace+this.NodeName);
  var box = document.getElementById(this.NameSpace+this.NodeName+'Contents');
  if (perc==true) {
    pas.HTMLUtils.SetHeightWidthHTML(this,con,'W',AValue);
    pas.HTMLUtils.SetHeightWidthHTML(this,box,'W','100%');
  }
  else {
    pas.HTMLUtils.SetHeightWidthHTML(this,con,'W','');
    pas.HTMLUtils.SetHeightWidthHTML(this,box,'W',AValue);
  }
  end;
end;


{$endif}


function TXEditBox.GetBoxWidth:string;
begin
  result:=MyNode.getAttribute('BoxWidth',true).AttribValue;
end;
function TXEditBox.GetItemValue:string;
begin
  result:=MyNode.getAttribute('ItemValue',true).AttribValue;
end;
function TXEditBox.GetReadOnly:Boolean;
begin
  result:=MyStrToBool(MyNode.getAttribute('ReadOnly',true).AttribValue);
end;
function TXEditBox.GetHasFocus:Boolean;
var
  fc:Boolean;
begin
  fc:=false;
  {$ifndef JScript}
  fc:=TEdit(myControl).Focused;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
        fc = ob.hasFocus();
      }
  end;
  {$endif}
  myNode.SetAttributeValue('HasFocus',myBoolToStr(fc),'Boolean');
end;
function TXEditBox.GetPasswordBox:Boolean;
var
  tmp:string;
begin
  if myNode<>nil then
  begin
    tmp:=myNode.GetAttribute('PasswordBox',true).AttribValue;
    if tmp='' then tmp:='True';
    result:=myStrToBool(tmp);
  end
  else
    result:=True;
end;
function TXEditBox.GetIncludeDataInSave:Boolean;
begin
  result:=MyStrToBool(MyNode.getAttribute('IncludeDataInSave',true).AttribValue);
end;

procedure TXEditBox.SetItemValue(AValue:string);
begin
  myNode.SetAttributeValue('ItemValue',AValue);
  {$ifndef JScript}
  if length(AValue) < 37440 then             //!!!! arbitrary test....things fall over if text is too long
  begin
    TEdit(myControl).Text:=AValue;
    TEdit(myControl).Enabled:=true;
  end
  else
  begin
    TEdit(myControl).Text:='...';
    TEdit(myControl).Enabled:=false;
  end;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
       ob.value=AValue;  }
  end;
  {$endif}
end;

procedure TXEditBox.SetReadOnly(AValue:Boolean);
begin
  myNode.SetAttributeValue('ReadOnly',myBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  TEdit(myControl).ReadOnly:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
      ob.readOnly = AValue  }
  end;
  {$endif}
end;

procedure TXEditBox.SetHasFocus(AValue:Boolean);
begin
  myNode.SetAttributeValue('HasFocus',myBoolToStr(AValue),'Boolean');
  if (not StartingUp) then
  begin
  {$ifndef JScript}
  if (not (csDesigning in componentState))
  and (not (csLoading in componentState))
  and (self.IsVisible) then
  begin
    try
    if AValue=true then
      //TEdit(myControl).SetFocus
      self.myNode.MyForm.ActiveControl:=TEdit(myControl)
    else
      //self.SetFocus;
      self.myNode.MyForm.ActiveControl:=self;
    except
      on ex:exception do
        EXIT;
    end;
  end;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
      if (AValue==true) {
        ob.focus();  }
      else {
        ob = document.getElementById(this.NameSpace+this.NodeName);
        ob.focus();  }
      }
  end;
  {$endif}
  end;
end;

procedure TXEditBox.SetPasswordBox(AValue:Boolean);
begin
  myNode.SetAttributeValue('PasswordBox',myBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  if AValue=true then
    TEdit(myControl).PasswordChar:='*'
  else
    TEdit(myControl).PasswordChar:=#0;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
      if (AValue) {
         ob.type = 'password'  }
      else {
           ob.type = 'text'}
      }
  end;
  {$endif}
end;
procedure TXEditBox.SetIncludeDataInSave(AValue:Boolean);
begin
  myNode.SetAttributeValue('IncludeDataInSave',MyBoolToStr(AValue),'Boolean');
end;

begin
  // this is the set of node attributes that each TXEditBox instance will have.
  AddWrapperDefaultAttribs(myDefaultAttribs);
  AddDefaultAttribute(myDefaultAttribs,'BoxWidth','String','200','',false);
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','Right','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelText','String','Edit Box','',false);
  AddDefaultAttribute(myDefaultAttribs,'ReadOnly','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'HasFocus','Boolean','False','',false,false);
  AddDefaultAttribute(myDefaultAttribs,'PasswordBox','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'ItemValue','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'IncludeDataInSave','Boolean','True','If false, the editbox ItemValue will be excluded from saved system data',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  AddExclusionAttribToTable(MyNodeType,'IncludeDataInSave','ItemValue');

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  AddAttribOptions(MyNodeType,'LabelPos',LabelPosOptions);
  {$ifndef JScript}
  {$I XEditBox.lrs}
  RegisterClass(TXEditBox);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}

  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}
  SuppressDesignerProperty('TXEditBox','BgColor');
  SuppressDesignerProperty(MyNodeType,'ContainerHeight');
  SuppressDesignerProperty(MyNodeType,'ContainerWidth');
end.
