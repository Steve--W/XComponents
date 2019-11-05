(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XColorPicker;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,TypInfo, NodeUtils,StringUtils,
  {$ifndef JScript}
  LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ColorBox, ExtCtrls, Propedits,RTTICtrls,
  LazsUtils, Events,
  {$else}
  HTMLUtils,
  {$endif}
  WrapperPanel;

type
  TXColorPicker = class(TWrapperPanel)
  private
     { Private declarations }
     {$ifndef JScript}
     fHandleClick:TEventHandler;
     fHandleChange:TEventHandler;
     fMyColorDialog:TColorDialog;
     fTheBox:TColorBox;
     fTheButton:TButton;
     procedure Pickerclick(Sender:TObject);
     procedure PickerChange(Sender: TObject);
     procedure DialogButtonClick(Sender: TObject) ;
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
//     procedure LinkLoadFromProperty(Sender: TObject);  override;
//     procedure LinkSaveToProperty(Sender: TObject);  override;
     {$ifndef JScript}
     procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
     property ParentColor;
     {$endif}
   public
     { Public declarations }
     {$ifndef JScript}
     constructor Create(TheOwner: TComponent); override;
     constructor Create(TheOwner: TComponent;IsDynamic:Boolean); override;
     property MyColorDialog: TcolorDialog read fMyColorDialog write fMyColorDialog;
     property TheBox: TcolorBox read fTheBox write fTheBox;
     property TheButton: TButton read fTheButton write fTheButton;
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

const MyNodeType='TXColorPicker';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXColorPicker.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
  MyEventTypes.Add('Change');
end;

{$ifndef JScript}
procedure Register;
begin
  {$I xcolorpicker_icon.lrs}
  RegisterComponents('XComponents',[TXColorPicker]);

  // suppress some of the Link properties
//  RegisterPropertyEditor(TypeInfo(TAliasStrings), TXPropertyLink, 'AliasValues', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(String), TXPropertyLink, 'TIElementName', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(TPropertyLinkOptions), TXPropertyLink, 'Options', THiddenPropertyEditor);
end;

constructor TXColorPicker.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXColorPicker.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXColorPicker.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  self.BorderSpacing.Around:=glbBorderSpacing;

  myControl:=TPanel.Create(self);
  myControl.Parent:=self;

  theButton:=TButton.Create(self);
  theButton.Parent:=TWinControl(myControl);
  theButton.Align:=alLeft;

  theBox:=TColorBox.Create(self);
  theBox.Parent:=TWinControl(myControl);
  theBox.Align:=alClient;

  myColorDialog:=TColorDialog.Create(self);

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];

  TPanel(myControl).AutoSize:=true;
  TPanel(myControl).BevelInner:=bvNone;
  TPanel(myControl).BevelOuter:=bvNone;
  TPanel(myControl).BorderStyle := bsNone;
  TPanel(myControl).BorderWidth:=0;
  theButton.Width:=25;
  theButton.Hint:='More colours...';
  theButton.ShowHint:=true;

  TColorBox(theBox).OnEditingDone:=@self.myeditingDone;

  TColorBox(theBox).OnChange:=@self.PickerChange;
  TColorBox(theBox).OnClick:=@self.PickerClick;

  theButton.OnClick:=@self.DialogButtonClick;

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
  NewNode:=CreateDynamicLazWidget('TXColorPicker',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;


procedure TXColorPicker.PickerClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;

procedure TXColorPicker.PickerChange(Sender: TObject) ;
 var
    cb: TColorBox ;
 begin
    cb := TColorBox(sender) ;
    self.ItemValue:=ColorToHexRGB(cb.Selected);
    CallHandleEvent('Change',self.ItemValue,Sender);
 end;
procedure TXColorPicker.DialogButtonClick(Sender: TObject) ;
var
   oldColor:TColor;
   pickedColor:TColor;
begin
  if not (csDesigning in componentState) then
  begin
    oldColor:=HexRGBToColor(self.ItemValue);
    myColorDialog.Color:=oldColor;
    if myColorDialog.Execute then
    begin
      pickedColor:=(myColorDialog.Color);
      self.ItemValue:=ColorToHexRGB(pickedColor);
    end;
  end;
end;



//procedure TXColorPicker.LinkLoadFromProperty(Sender: TObject);
//begin
//  if Sender=nil then ;
//  if (Link.Editor=nil) then exit;
//  inherited  LinkLoadFromProperty(Sender);
//
//  self.ItemValue:=Link.GetAsText;
//
//end;
//
//procedure TXColorPicker.LinkSaveToProperty(Sender: TObject);
//begin
//  if Sender=nil then ;
//  if Link.Editor=nil then exit;
//  Link.SetAsText(TColorBox(TheBox).Text);
//
//end;

procedure TXColorPicker.SetBoxWidth(AValue:string);
 var
   tc:TControl;
 begin
   tc:=self.TheBox;
  myNode.SetAttributeValue('BoxWidth',AValue);
  SetHeightWidth(self.myNode,tc,'BoxWidth','');
end;


{$else}
constructor TXColorPicker.Create(MyForm:TForm;NodeName,NameSpace:String);
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
    var PickerString = '<input type="color"  id='+MyObjectName+' '+
                        OnClickString +
                        OnChangeString +
                       ' style="display: inline-block;" value='+ItemValue+' '+ReadOnlyString+'> ';

    HTMLString = labelstring+PickerString;

    var wrapper=document.getElementById(wrapperid);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);

  }
  catch(err) { alert(err.message+'  in XColorPicker.CreateWidget');}

end;

  MyNode.ScreenObject:=MyNode;

  // now that we have a datanode and a widget, cycle attribute settings
  RefreshComponentProps(myNode);

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXColorPicker.Create(MyForm,NodeName,NameSpace));
end;

//procedure TXColorPicker.LinkLoadFromProperty(Sender: TObject);
//begin
//  inherited  LinkLoadFromProperty(Sender);
//end;
//
//procedure TXColorPicker.LinkSaveToProperty(Sender: TObject);
//begin
//  if Sender=nil then ;
//  if Link=nil then exit;
//  if Link.TIObject=nil then exit;
////  showmessage('linksavetoproperty. '+Link.TIPropertyName+' '+self.ItemValue);
//
//  SetStringProp(Link.TIObject,Link.TIPropertyName,self.ItemValue);
//end;


procedure TXColorPicker.SetBoxWidth(AValue:string);
begin
  myNode.SetAttributeValue('BoxWidth',AValue);
  asm
  var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
  end;
end;


{$endif}

function TXColorPicker.GetBoxWidth:string;
begin
  result:=MyNode.getAttribute('BoxWidth',true).AttribValue;
end;
function TXColorPicker.GetItemValue:string;
begin
  result:=MyNode.getAttribute('ItemValue',true).AttribValue;
end;
function TXColorPicker.GetReadOnly:Boolean;
begin
  result:=MyStrToBool(MyNode.getAttribute('ReadOnly',true).AttribValue);
end;

procedure TXColorPicker.SetItemValue(AValue:string);
begin
  myNode.SetAttributeValue('ItemValue',AValue);
  {$ifndef JScript}
  TColorBox(TheBox).Selected:=HexRGBToColor(AValue);
  //TheButton.Color:=TheBox.Selected;         //doesn't work
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
        ob.value=AValue;
        }
  end;
  //LinkSaveToProperty(self);
  {$endif}
end;

procedure TXColorPicker.SetReadOnly(AValue:Boolean);
begin
  myNode.SetAttributeValue('ReadOnly',myBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  TColorBox(TheBox).ReadOnly:=AValue;
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
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','Right','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelText','String','Colour Picker','',false);
  AddDefaultAttribute(myDefaultAttribs,'ReadOnly','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'ItemValue','String','','',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  AddAttribOptions(MyNodeType,'LabelPos',LabelPosOptions);
  {$ifndef JScript}
  RegisterClass(TXColorPicker);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}
  SuppressDesignerProperty('TXColorPicker','BgColor');
  SuppressDesignerProperty(MyNodeType,'ContainerHeight');
  SuppressDesignerProperty(MyNodeType,'ContainerWidth');


end.
