(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit WrapperPanel;

{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface
uses
  Classes, SysUtils, Types, TypInfo, StringUtils, NodeUtils,
  {$ifndef JScript}
  LCLClasses, Forms, Controls, StdCtrls, ExtCtrls, Graphics, Dialogs,
  ProjectIntf, LazIDEIntf, PropEdits, RTTICtrls, LCLProc,
  LazsUtils,
  {$else}
  HTMLUtils,
  {$endif}
  Events;


{$ifndef JScript}
type TWrapperPanel=class;  //forward

type
  TAlignmentProperty = class (TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    //procedure Edit; override;
  end;
type
  TLabelPosProperty = class (TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    //procedure Edit; override;
  end;
type
  TScrollBarsProperty = class (TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;
type
  TLanguageProperty = class (TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;



{$endif}

{$ifndef JScript}
type TWrapperPanel=Class(TCustomPanel)
{$else}
type TWrapperPanel=Class(TInterfaceObject)
{$endif}
 private
   {$ifndef JScript}
   FSelectionBorderColor: TColor;
   FmyNode:TDataNode;
   FmyControl:TControl;
   {$endif}
   FIsContainer:Boolean;
   FAlignChildrenVertical:Boolean;
//   FLink: TXPropertyLink;

   function GetName:string;
   function GetIsVisible:Boolean;
   function GetContainerWidth:string;
   function GetContainerHeight:string;
   function GetHint:string;
   function GetBgColor:TColor;
   function GetLabelText:string;
   function GetLabelPos:String;
   function GetAlignment:String;
   function GetSpacingAround:integer;
   function GetBorder:Boolean;
   function GetHTMLClasses:String;

   procedure SetIsVisible(AValue:Boolean);
   procedure SetMyName(AValue:string);
   procedure SetHint(AValue:string);
   procedure SetLabelText(AValue:string);
   procedure SetAlignment(AValue:string);
   procedure SetSpacingAround(AValue:integer);
   procedure SetBorder(AValue:Boolean);
   procedure SetHTMLClasses(AValue:string);

   {$ifndef JScript}
//   procedure SetLink(const AValue: TXPropertyLink);
   function DoAlignChildControls(TheAlign: TAlign; AControl: TControl;
                     AControlList: TFPList; var ARect: TRect): Boolean; override;

   procedure SetName(const NewName: TComponentName); override;
   procedure SetParent(NewParent: TWinControl);  override;
   procedure SetSelectionBorderColor(AValue: TColor);

   procedure WrapperClick(Sender: TObject) ;
   {$endif}
 public
   FIsSelected:Boolean;
   {$ifndef JScript}
    myEventTypes:TStringList;
    myLbl:TLabel;
    myExtension:TXDesignerExtension;
    constructor Create(TheOwner:TComponent);  override;
    constructor Create(TheOwner:TComponent;IsDynamic:Boolean); virtual;
    destructor Destroy; override;
    procedure AddLabel(TargetControl:TControl);
    procedure EditingDone; override;
    procedure MyEditingDone(Sender:TObject);
    {$else}
    constructor Create(NodeName,NameSpace:String);
    procedure SortOutAlignment;
    {$endif}

    procedure SetLabelPos(AValue:String);    virtual;
    procedure SetIsSelected(AValue: Boolean);   virtual;
    procedure SetBgColor(AValue:TColor);       virtual;
    procedure SetContainerHeight(AValue:string); virtual;
    procedure SetContainerWidth(AValue:string); virtual;
    procedure SortOutMyAlignmentAndLabelPos;

protected
  {$ifndef JScript}
    procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
    procedure Paint;override;
    procedure Loaded; override;
    {$endif}

published
  {$ifndef JScript}
  property Align;
  property AutoSize;
  property OnMouseDown;
  property OnMouseMove;
  property OnMouseUp;

  property myControl:TControl read FmyControl write FmyControl;
  property myNode:TDataNode read FmyNode write FmyNode;
  property SelectionBorderColor: TColor read FSelectionBorderColor write SetSelectionBorderColor default clGreen;
  {$endif}

  property IsContainer:Boolean read FIsContainer write FIsContainer;
  property AlignChildrenVertical:Boolean read FAlignChildrenVertical write FAlignChildrenVertical;
  property IsSelected: Boolean read FIsSelected write SetIsSelected default false;
  property Alignment:String read GetAlignment write SetAlignment;
  property LabelText: String read GetLabelText write SetLabelText;
  property LabelPos: String read GetLabelPos write SetLabelPos;
  property IsVisible:Boolean read GetIsVisible write SetIsVisible;
  property Hint: String read GetHint write SetHint;
  property Name: String read GetName write SetMyName;
  property ContainerWidth: String read GetContainerWidth write SetContainerWidth;
  property ContainerHeight: String read GetContainerHeight write SetContainerHeight;
  property BgColor: TColor read GetBgColor write SetBgColor;
  property SpacingAround:integer read GetSpacingAround write SetSpacingAround;
  property Border: Boolean read GetBorder write SetBorder;
  property HTMLClasses: String read GetHTMLClasses write SetHTMLClasses;

end;

{$ifndef JScript}
procedure SuppressWrapperDesignerProperties;
{$endif}
procedure SuppressDesignerProperty(Classname:String; pName:String);
function AddDynamicWidget(TypeName:String;ParentForm:TForm;ParentNode:TDataNode;NodeName,NameSpace,Alignment:String;position:integer):TdataNode;
//procedure SetCommonWrapperProperties(myWrapper:TWrapperPanel);
//procedure SetCommonWrapperPropDefaults(myWrapper:TWrapperPanel);
procedure AddWrapperDefaultAttribs(var myDefaultAttribs:TDefaultAttributesArray);


type TSuppressedDesignerProperty = record
  ClassName:String;
  PName:String;
  end;

type TSuppressedDesignerProperties = Array of TSuppressedDesignerProperty;
var SuppressedDesignerProperties:TSuppressedDesignerProperties;

function FindSuppressedProperty(Classname,pName:string):integer;

implementation
uses XScrollBox, XTabControl, XForm;


{$ifndef JScript}
constructor TWrapperPanel.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,false);
end;

constructor TWrapperPanel.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TWrapperPanel.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  ControlStyle := ControlStyle - [csSetCaption];
  Caption:='';

  AutoSize:=true;

  inherited align:=alNone;
  ParentColor:=true;

  BevelInner:=bvNone;
  BevelOuter:=bvNone;
  BorderStyle := bsNone;
  BorderWidth:=0;

  SpacingAround:=0;

  IsSelected:=false;
  IsContainer:=true;
  AlignChildrenVertical:=true;

  SelectionBorderColor:=glbSelectionBorderColor;

  MyEventTypes:=TStringList.Create;

  self.OnClick:=@WrapperClick;
  self.Tag:=-1;

  // Lazarus Designer extension...
  if csDesigning in componentState then
   begin
      myExtension:=TXDesignerExtension.Create;
      myExtension.myWrapper:=self;
   end;

  // Property linking...
//  FLink:=TXPropertyLink.Create(Self);
//  FLink.Filter:=[{tkUnknown,}tkInteger,tkChar,tkEnumeration,
//                 tkFloat,{tkSet,tkMethod,}tkSString,tkLString,tkAString,
//                 tkWString,tkVariant,{tkArray,tkRecord,tkInterface,}
//                 {tkClass,tkObject,}tkWChar,tkBool,tkInt64,
//                 tkQWord{,tkDynArray,tkInterfaceRaw}];
//  FLink.Options:=[ploAutoSave];
//  FLink.OnLoadFromProperty:=@LinkLoadFromProperty;
//  FLink.OnSaveToProperty:=@LinkSaveToProperty;
end;

destructor TWrapperPanel.Destroy;
begin
//  FreeThenNil(FLink);
  if csDesigning in componentState then
  begin
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
    myExtension.Destroy;
  end;
  inherited Destroy;
end;

procedure TWrapperPanel.Loaded;
var
s:String;
myTag:TComponentTag;
begin
  // this happens after the load of this component from lfm is complete.
  inherited Loaded;
  Caption := EmptyStr;

  self.ContainerHeight:=self.ContainerHeight;

//  FLink.LoadFromProperty;
end;

procedure TWrapperPanel.WrapperClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;

procedure TWrapperPanel.Paint;
begin
  inherited Paint;
  PaintSelectedRectangle(TCustomControl(self).Canvas,self.GetClientRect,self.SelectionBorderColor,self.IsSelected);
end;

procedure TWrapperPanel.EditingDone;
begin
  inherited EditingDone;
  //FLink.EditingDone;
end;

procedure TWrapperPanel.MyEditingDone(Sender:TObject);
begin
  EditingDone;
end;

procedure TWrapperPanel.SetParent(NewParent: TWinControl);
begin
  inherited;
  CheckParentIsXContainer(self);
  ResetAlignment(TControl(self));
end;

//procedure TWrapperPanel.SetLink(const AValue: TXPropertyLink);
//begin
//  if FLink=AValue then exit;
//  FLink.Assign(AValue);
//end;

//procedure TWrapperPanel.LinkLoadFromProperty(Sender: TObject);
//begin
//  if Sender=nil then ;
//  if (Link.Editor=nil) then exit;
//
//  //writeln('LinkLoadFromProperty A ',Name,
//  //  ' FLink.GetAsText=',FLink.GetAsText,' Text=',Text,
//  //  ' PropName=',FLink.TIPropertyName);
//  //showmessage('loadfromproperty');
//
//  if MyNode<>nil then
//    myNode.SetAttributeValue('Link',LinkToStr(Link));
//
//end;
//
//procedure TWrapperPanel.LinkSaveToProperty(Sender: TObject);
//begin
//end;

function TLabelPosProperty.GetAttributes: TPropertyAttributes;
begin
  // editor, sorted list
  //Result := [paDialog, paValueList, paSortList];
  Result := [paValueList, paSortList, paPickList];
end;
procedure TLabelPosProperty.GetValues(Proc: TGetStrProc);
begin
  Proc (LabelPosOptions[0]);
  Proc (LabelPosOptions[1]);
  Proc (LabelPosOptions[2]);
  Proc (LabelPosOptions[3]);
end;
function TLanguageProperty.GetAttributes: TPropertyAttributes;
begin
  // editor, sorted list
  //Result := [paDialog, paValueList, paSortList];
  Result := [paValueList, paSortList, paPickList];
end;
procedure TLanguageProperty.GetValues(Proc: TGetStrProc);
begin
  Proc (LanguageOptions[0]);
  Proc (LanguageOptions[1]);
end;
function TAlignmentProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paPickList];
end;
procedure TAlignmentProperty.GetValues(Proc: TGetStrProc);
begin
  Proc (AlignmentOptions[0]);
  Proc (AlignmentOptions[1]);
  Proc (AlignmentOptions[2]);
  Proc (AlignmentOptions[3]);
  Proc (AlignmentOptions[4]);
end;
function TScrollBarsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paPickList];
end;
procedure TScrollBarsProperty.GetValues(Proc: TGetStrProc);
begin
  Proc (ScrollBarsOptions[0]);
  Proc (ScrollBarsOptions[1]);
  Proc (ScrollBarsOptions[2]);
end;

procedure SuppressDesignerProperty(ClassName:String; pName:String);
var
  pInfo:PPropInfo;
  ptInfo:PTypeInfo;
  i:integer;
  AClass:TPersistentClass;
begin
  try
    AClass:=getclass(ClassName);
    if (AClass<>nil) then
    begin
      try
        pInfo:= FindPropInfo(AClass, pname);
        if pInfo<>nil then
        begin
          ptInfo:=pInfo^.PropType;
          RegisterPropertyEditor(ptInfo, AClass, pName, THiddenPropertyEditor);
        end;
      except
         on e:EPropertyError do
         begin
         end;
      end;
    end;

  finally

    // add the property to a suppression list, to be available (eg) for dynamic object inspectors
    if FindSuppressedProperty(ClassName,pName) < 0 then
    begin
      Setlength(SuppressedDesignerProperties,length(SuppressedDesignerProperties)+1);
      SuppressedDesignerProperties[length(SuppressedDesignerProperties)-1].ClassName:=ClassName;
      SuppressedDesignerProperties[length(SuppressedDesignerProperties)-1].pName:=pName;
    end;
  end;
end;

procedure SuppressWrapperDesignerProperties;
begin
  // Hide some inherited properties in the Lazarus IDE
  RegisterPropertyEditor(TypeInfo(TMouseEvent), TWrapperPanel, 'OnMouseDown', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMouseEvent), TWrapperPanel, 'OnMouseUp', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMouseMoveEvent), TWrapperPanel, 'OnMouseMove', THiddenPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TAlign), TWrapperPanel, 'Align', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TWrapperPanel, 'Autosize', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TWrapperPanel, 'Height', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TWrapperPanel, 'Width', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TWrapperPanel, 'Top', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TWrapperPanel, 'Left', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlChildSizing), TWrapperPanel, 'ChildSizing', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TSizeConstraints), TWrapperPanel, 'Constraints', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCursor), TWrapperPanel, 'Cursor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TWrapperPanel, 'Tag', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(THelpContext), TWrapperPanel, 'HelpContext', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(THelpType), TWrapperPanel, 'HelpType', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TWrapperPanel, 'HelpKeyword', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCaption), TWrapperPanel, 'Caption', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TWrapperPanel, 'ParentColor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TWrapperPanel, 'AlignChildrenVertical', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TWrapperPanel, 'IsContainer', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControl), TWrapperPanel, 'myControl', THiddenPropertyEditor);

  RegisterPropertyEditor(TypeInfo(Boolean), TWrapperPanel, 'IsSelected', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TColor), TWrapperPanel, 'SelectionBorderColor', THiddenPropertyEditor);

  //special property editors...
  RegisterPropertyEditor (TypeInfo(string), TWrapperPanel, 'Alignment', TAlignmentProperty);
  RegisterPropertyEditor (TypeInfo(string), TWrapperPanel, 'LabelPos', TLabelPosProperty);
end;


{$else}

constructor TWrapperPanel.Create(NodeName,NameSpace:String);
begin
  inherited Create('UI',NodeName,'','',false);       //!!!!namespace???

  AlignChildrenVertical:=true;

  self.NodeName:=NodeName;
  self.Namespace:=NameSpace;
  self.IsContainer:=true;
  self.IsVisible:=true;
  myNode:=TDataNode(self);
  //showmessage('adding '+NodeName+' to SystemNodeTree');
  AddChildToParentNode(SystemNodeTree,myNode,-1);  // to be re-parented later


end;

procedure SuppressDesignerProperty(Classname:String; pName:String);
begin
  // add the property to a suppression list, to be available (eg) for dynamic object inspectors
  if FindSuppressedProperty(ClassName,pName) < 0 then
  begin
    Setlength(SuppressedDesignerProperties,length(SuppressedDesignerProperties)+1);
    SuppressedDesignerProperties[length(SuppressedDesignerProperties)-1].ClassName:=ClassName;
    SuppressedDesignerProperties[length(SuppressedDesignerProperties)-1].pName:=pName;
  end;
end;


{$endif}



function AddDynamicWidget(TypeName:String;ParentForm:TForm;ParentNode:TDataNode;NodeName,NameSpace,Alignment:String;position:integer):TdataNode;
var
  NewNode:TDataNode;
  fn:TAddComponentFunc;
  starting:Boolean;
  {$ifdef JScript}
  NewWidget:TInterfaceObject;
  {$endif}
begin
  fn:=LookupComponentFunc(TypeName);
  if fn<>nil then
  begin
    Starting:=StartingUp;
    StartingUp:=true;
    {$ifndef JScript}
    //Create Widget (also creates datanode)
    NewNode:=fn(parentNode,NodeName,NameSpace,position,Alignment);
    {$else}
 //   NewWidget:=TInterfaceObject(CreateInterfaceObject(nil,TypeName,NodeName,NameSpace));
    NewWidget:=TInterfaceObject(CreateInterfaceObject(ParentForm,TypeName,NodeName,NameSpace));
    if IsPublishedProp(NewWidget,'Alignment') then
      SetStringProp(NewWidget,'Alignment',Alignment);
    NewNode:=TdataNode(NewWidget);
    NewWidget.myNode:=NewNode;
    if ParentNode<>nil then
      AddChildToParentNode(ParentNode,NewNode,position);
    fn(NewNode,ParentNode,NodeName,NameSpace,position,Alignment);
    {$endif}
    StartingUp:=Starting;
    NewNode.IsDynamic:=true;

    result:=NewNode;
  end
  else
  begin
    showmessage('No function defined to instantiate component of type '+TypeName);
    result:=nil;
  end;
end;

procedure AddWrapperDefaultAttribs(var myDefaultAttribs:TDefaultAttributesArray);
begin
  AddDefaultAttribute(myDefaultAttribs,'Alignment','String','Left','',false);
  AddDefaultAttribute(myDefaultAttribs,'Hint','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'IsVisible','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'HTMLClasses','String','','',false);
end;


function FindSuppressedProperty(Classname,pName:string):integer;
var
  i,l:integer;
begin
  result:=-1;
  l:=length(SuppressedDesignerProperties);
  i:=0;
  while i < length(SuppressedDesignerProperties) do
  begin
    if (SuppressedDesignerProperties[i].ClassName = ClassName)
    and (SuppressedDesignerProperties[i].PName = pName) then
    begin
      result:=i;
      i:= length(SuppressedDesignerProperties);
    end;
    i:=i+1;
  end;
end;


function TWrapperPanel.GetName:string;
var
  myname:string;
begin
  {$ifndef JScript}
  result:=inherited Name;
  {$else}
  result:=self.NodeName;
  {$endif}
end;

function TWrapperPanel.GetIsVisible:Boolean;
var
  tmp:String;
begin
  if myNode<>nil then
  begin
    tmp:=myNode.GetAttribute('IsVisible',true).AttribValue;
    if tmp='' then tmp:='True';
    result:=myStrToBool(tmp);
  end
  else
    result:=True;
end;
function TWrapperPanel.GetHint:string;
begin
  result:=myNode.GetAttribute('Hint',true).AttribValue;
end;
function TWrapperPanel.GetContainerWidth:string;
begin
  result:=myNode.GetAttribute('ContainerWidth',true).AttribValue;
end;
function TWrapperPanel.GetContainerHeight:string;
begin
  result:=myNode.GetAttribute('ContainerHeight',true).AttribValue;
end;
function TWrapperPanel.GetLabelPos:string;
begin
  if myNode<>nil then
     result:=myNode.GetAttribute('LabelPos',true).AttribValue
  else
     result:='Top';
end;
function TWrapperPanel.GetBorder:Boolean;
begin
  result:=myStrToBool(myNode.GetAttribute('Border',true).AttribValue);
end;
function TWrapperPanel.GetHTMLClasses:string;
begin
  result:=myNode.GetAttribute('HTMLClasses',true).AttribValue;
end;

{$ifndef JScript}
// Name is the first property loaded from .lfm.
// Hijacking this so that we can reset blank default values for all string properties
// (because there is a problem - string properties are NOT saved to Lazarus lfm when the value is blank, so
// if the user wants the property to be blank, then we shouldn't set any non-blank defaults when
// re-loading the project from lfm).
procedure TWrapperPanel.SetName(const NewName: TComponentName);
var
  ApplyName:TComponentName;
begin
  ApplyName:=GetNameToApply(self.myNode,self.myNode.MyForm.Name,self.Name,NewName,componentState);

  inherited SetName(ApplyName);

  if (csLoading in componentState) then
  begin
    //!!!! have to check this for all component types,... add missing properties.....
    ContainerWidth:='';
    ContainerHeight:='';
    Hint:='';
    LabelText:='';
    LabelPos:='';
  end;

end;
{$endif}

procedure TWrapperPanel.SetMyName(AValue:string);
begin

  {$ifndef JScript}
  inherited Name:=AValue;
  {$else}
  self.Name:=AValue;

  asm
     var ob = document.getElementById(this.NameSpace+this.NodeName);
     inner = pas.HTMLUtils.ScreenObjectInnerComponent(this);
     if (inner.id == this.NameSpace+this.NodeName+'Contents') {
       inner.id = this.NameSpace+AValue+'Contents';
       }
        //!!!! issue here with naming of html components / references within event handlers / inner components / etc
     ob.id = this.NameSpace+AValue;
  end;
  {$endif}

  {$ifndef JScript}
  if  (csLoading in componentState) then
    if myNode<>nil then
      myNode.NodeName:=AValue;
  {$endif}
end;

{$ifndef JScript}
procedure TWrapperPanel.SetSelectionBorderColor(AValue: TColor);
// Colour of the 'IsSelected' dotted border
begin
  if AValue<>FSelectionBorderColor then
    begin
      FSelectionBorderColor:=AValue;
      Repaint;
    end;
end;
{$else}
{$endif}

procedure TWrapperPanel.SetIsSelected(AValue: Boolean);
begin
  if AValue<>FIsSelected then
    begin
      FIsSelected:=AValue;
      {$ifndef JScript}
      ShowHideSelectedBorder(self.myNode,FIsSelected);
      Repaint;
      {$else}
      ShowHideSelectedBorder(TDataNode(self),FIsSelected);
      {$endif}
    end;
end;


procedure TWrapperPanel.SetContainerWidth(AValue:string);
begin
  myNode.SetAttributeValue('ContainerWidth',AValue);
  {$ifndef JScript}
  SetHeightWidth(self.myNode,TControl(self),'ContainerWidth','ContainerHeight');
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName);
    if (ob==null) {alert('cannot find object '+this.NameSpace+this.NodeName+' for width set');}
    else {pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue); }
  end;

  {$endif}
end;

procedure TWrapperPanel.SetContainerHeight(AValue:string);
begin
  myNode.SetAttributeValue('ContainerHeight',AValue);
  {$ifndef JScript}
  SetHeightWidth(self.myNode,TControl(self),'ContainerWidth','ContainerHeight');
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName);
    if (ob==null) {alert('cannot find object '+this.NameSpace+this.NodeName+' for height set');}
    else {pas.HTMLUtils.SetHeightWidthHTML(this,ob,'H',AValue);}
  end;

  {$endif}
end;


procedure TWrapperPanel.SetIsVisible(AValue:Boolean);
begin
  if myNode<>nil then
  begin
    myNode.SetAttributeValue('IsVisible',myBoolToStr(AValue),'Boolean');
    {$ifndef JScript}
    self.Visible:=AValue;

    {$else}
    asm
      var ob = document.getElementById(this.NameSpace+this.NodeName);
      if (ob!=null)  {
        if (AValue==true) {
          if (this.NodeType != 'TXMenuItem') {
            ob.style.display = 'flex';                //!!!! this needs to be reset to whatever it was before!!!!
            if (this.LabelPos!='') {
              self.SortOutMyAlignmentAndLabelPos;
              }
            else
              {self.SortOutAlignment;}
          }
          else
          { //!! for a menu item
            // delete the 'display' attribute
            ob.removeAttribute("style");
          }
        }
        else  {
          ob.style.display = 'none';
        }
      }
    end;
    {$endif}
  end;
end;


procedure TWrapperPanel.SetHint(AValue:string);
begin
  myNode.SetAttributeValue('Hint',AValue);
  {$ifndef JScript}
  if AValue<>'' then
    self.ShowHint:=true
  else
    self.ShowHint:=false;
  inherited Hint:=AValue;
  {$else}

  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName);
    if (ob!=null)  {
    ob.title=AValue; }
  end;

  {$endif}
end;

procedure TWrapperPanel.SetHTMLClasses(AValue:string);
begin
  myNode.SetAttributeValue('HTMLClasses',AValue);

  {$ifndef JScript}
  {$else}
  if myNode.IsDynamic then
  begin
    asm

      var ob = document.getElementById(this.NameSpace+this.NodeName);
      if (ob==null) {alert('cannot find object '+this.NameSpace+this.NodeName+' for HTMLClasses set');}
      else {
        pas.HTMLUtils.ApplyClasses(ob,AValue,this);

       // var elems=ob.getElementsByTagName("*");
        var elems=ob.getElementsByClassName(ob.id);
        for (var i=0; i<elems.length; i++)
          //if ((elems[i].id!=undefined)&&(elems[i].id.indexOf(ob.id)==0))
          {
          if (elems[i].id!=undefined) {
            pas.HTMLUtils.ApplyClasses(elems[i],AValue,this);
          }
        }
      }
    end;
  end;
  {$endif}
end;


function TWrapperPanel.GetAlignment:String;
begin
  result:=MyNode.getAttribute('Alignment',true).AttribValue;
end;

procedure TWrapperPanel.SetAlignment(AValue:string);
begin
  if myNode<>nil then
  begin
    myNode.SetAttributeValue('Alignment',AValue);

    {$ifndef JScript}
    if self.Parent<>nil then
    begin
      //showmessage('set alignment '+self.Name);
      self.SortOutMyAlignmentAndLabelPos;
    end;
    {$else}
    //showmessage('setAlignment '+myNode.NodeName+' '+AValue+' '+self.LabelPos);
    if self.LabelPos<>'' then
      self.SortOutMyAlignmentAndLabelPos
    else
      self.SortOutAlignment;
    {$endif}

  end;
end;

function TWrapperPanel.GetLabelText:String;
begin
  result:=MyNode.getAttribute('LabelText',true).AttribValue;
end;

procedure TWrapperPanel.SetLabelText(AValue:string);
begin
  myNode.SetAttributeValue('LabelText',AValue);
  {$ifndef JScript}
  if self.myLbl<>nil then
     self.myLbl.Caption:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'ContentsLbl');
    if (ob!=null) {
      ob.innerHTML=AValue;   }
  end;

  {$endif}
end;

{$ifndef JScript}


procedure TWrapperPanel.AddLabel(TargetControl:TControl);
var
  Lbl:TLabel;
begin

  // add a label
   MyLbl := TLabel.Create(self);
   MyLbl.Caption:='.';
   LabelRightSettings(TargetControl,myLbl);
   MyLbl.parent:=self;
end;

procedure TWrapperPanel.SetLabelPos(AValue:string);
var
  ch:TControl;
  i,j:integer;
  Saveccl:TControlChildrenLayout;
begin
  myNode.SetAttributeValue('LabelPos',AValue);
  if (myLbl<>nil)
  and (AValue<>'') then
  begin
    ch:=nil;
    // find the component to which the label is currently anchored
    for i:=0 to self.ControlCount-1 do
    begin
      for j:=0 to self.Controls[i].AnchoredControlCount-1 do
      begin
        if (self.Controls[i].AnchoredControls[j] = myLbl)
        and (self.Controls[i].AnchoredControls[j]<>myLbl.Owner)
        then
          ch:=TControl(self.Controls[i]);
      end;
    end;

    if (ch<>nil)
    and (ch is TControl) then
    begin
      self.SortOutMyAlignmentAndLabelPos;
    end;
  end;
end;

procedure TWrapperPanel.SortOutMyAlignmentAndLabelPos;
var
    MyAlignment,MyLabelPos,NewAlignment:String;
begin
  if (self.Parent<>nil) and (self.Name<>'') then
  begin
    MyAlignment:=self.Alignment;
    MyLabelPos:=self.LabelPos;
    NewAlignment:=SortOutAlignmentAndLabelPos(self,myLbl,myControl,MyAlignment,MyLabelPos);

    if NewAlignment<>MyAlignment then
    begin
     // showmessage('old='+self.Alignment+' new='+NewAlignment);
      self.Alignment:=NewAlignment;
    end;
  end;
end;

  {$else}
procedure TWrapperPanel.SortOutMyAlignmentAndLabelPos;
var
    ParentAlignChildrenVertical, ContainerType:Boolean;
    MyAlignment,MyLabelPos,NewAlignment,typ,nm:String;
    ParentNode:TDataNode;
    pos:integer;
begin
  //if self.IsDynamic then showmessage('SortOutMyAlignmentAndLabelPos '+self.NodeName);
  ContainerType := self.IsContainer;
  MyAlignment:=self.Alignment;
  MyLabelPos:=self.LabelPos;
  ParentNode:=FindParentOfNodeByName(SystemNodeTree,self.NodeName,self.NameSpace,false,pos);
  //ParentNode:=FindParentOfNode(SystemNodeTree,self);        //!!!! doesn't work....why not???

  ParentAlignChildrenVertical:=true;
  if (ParentNode<>nil)
  and (ParentNode is TWrapperPanel) then
  begin
      ParentAlignChildrenVertical := TWrapperPanel(ParentNode).AlignChildrenVertical;

      NewAlignment:=AlignmentResetInvalidCombinations(MyAlignment,self.NodeName,self.NodeType,
                                                      ParentAlignChildrenVertical,self.IsContainer,
                                                      (length(ParentNode.ChildNodes)>1));

      if NewAlignment<>MyAlignment then
      begin
        MyAlignment:=NewAlignment;
        self.SetAttributeValue('Alignment',NewAlignment);
      end;

      nm:=self.NodeName;
      typ:=self.NodeType;
      asm
        try {
           var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
           var lbl = document.getElementById(this.NameSpace+this.NodeName+'ContentsLbl');
           var wrapper = document.getElementById(this.NameSpace+this.NodeName);
           var lp = MyLabelPos;

           if ((ob!=null) && (wrapper!=null)) {
           // clear everything first...
           //var savedDisplay = wrapper.style.display;
           //wrapper.style.display='';
           wrapper.classList.remove('hbox');
           wrapper.classList.remove('hboxNoStretch');
           wrapper.classList.remove('vbox');
           wrapper.classList.remove('vboxNoStretch');
           wrapper.classList.remove('vboxNoFlex');
           wrapper.classList.remove('AlignmentCentre');
           wrapper.classList.remove('AlignmentRight');
           wrapper.classList.remove('AlignmentLeft');
           wrapper.classList.remove('AlignmentLeftContainer');
           wrapper.classList.remove('AlignmentTop');
           wrapper.classList.remove('AlignmentBottom');

          if (lbl!=null) {
             lbl.style.padding='0px';

             if (lp=='Left') {
               lbl.parentNode.insertBefore(lbl, ob);  //put lbl before ob
               wrapper.classList.add('hboxNoStretch');
               lbl.style.alignSelf='center';
               lbl.style.padding='0px 3px 0px 0px';               // t,r,b,l
             }
             else if (lp=='Right') {
               ob.parentNode.insertBefore(ob, lbl);  //put lbl after ob
               wrapper.classList.add('hboxNoStretch');
               lbl.style.alignSelf='center';
               lbl.style.padding='0px 0px 0px 3px';               // t,r,b,l
             }
             else if (lp=='Top') {
               ob.parentNode.insertBefore(lbl, ob);
               //wrapper.classList.add('vboxNoStretch');
               wrapper.classList.add('vboxNoFlex');
               lbl.style.alignSelf='center';
               lbl.style.padding='0px 0px 0px 3px';               // t,r,b,l
             }
             else if (lp=='Bottom') {
               ob.parentNode.insertBefore(ob, lbl);
               //wrapper.classList.add('vboxNoStretch');
               wrapper.classList.add('vboxNoFlex');
               lbl.style.alignSelf='center';
               lbl.style.padding='3px 0px 0px 0px';               // t,r,b,l
             }
           }


           if (MyAlignment=='Right') {
             if (ParentAlignChildrenVertical) {
             ob.style.float='right';
             wrapper.classList.add('AlignmentRight');
             if (lbl!=null) {
               lbl.style.float='right';
               if ((lp=='Top')||(lp=='Bottom')) {
                   lbl.style.alignSelf='flex-e'+'nd';
               }
             }
           }
           }
           else if (MyAlignment=='Left') {
           if (ParentAlignChildrenVertical) {
               ob.style.float='left';
               if (ContainerType==true) {
                 wrapper.classList.add('AlignmentLeftContainer'); }
               else {
                 wrapper.classList.add('AlignmentLeft');  }
               if (lbl!=null) {
                 lbl.style.float='left';
                 if ((lp=='Top')||(lp=='Bottom')) {
                     lbl.style.alignSelf='flex-start';
                 }
               }
             }
             }
           else if (MyAlignment=='Centre') {
             ob.style.float='left';
              wrapper.classList.add('AlignmentCentre');
              if (lbl!=null) {
                 lbl.style.float='left';
              }
           }

           else if (MyAlignment=='Top') {
           if (ParentAlignChildrenVertical==false) {
             ob.style.float='left';
             wrapper.classList.add('AlignmentTop');
             if (lbl!=null) {
               lbl.style.float='left';
               if ((lp=='Left')||(lp=='Right')) {
                   lbl.style.alignSelf='flex-start';
                 }
             }
            }
            }
           else if (MyAlignment=='Bottom') {
           if (ParentAlignChildrenVertical==false) {
             ob.style.float='left';
             wrapper.classList.add('AlignmentBottom');
             if (lbl!=null) {
              lbl.style.float='left';
              if ((lp=='Left')||(lp=='Right')) {
                   lbl.style.alignSelf='flex-e'+'nd';
                   }
             }
         }
        }


       }
     } catch(err) { alert(err.message+'  in WrapperPanel.SortOutMyAlignmentAndLabelPos'); }
    end;
  end;
end;

procedure TWrapperPanel.SortOutAlignment;
var
  ParentAlignChildrenVertical, ContainerType:Boolean;
  MyAlignment,MyLabelPos,NewAlignment:String;
  ParentNode:TDataNode;
  nm,typ:String;
  pos:integer;
begin
  ContainerType := self.IsContainer;
  nm:=self.NodeName;
  typ:=self.NodeType;
  MyAlignment:=self.Alignment;
  MyLabelPos:=self.LabelPos;
  ParentNode:=FindParentOfNodeByName(SystemNodeTree,self.NodeName,self.NameSpace,false,pos);


  if ParentNode<>nil then
  begin
    ParentAlignChildrenVertical := TWrapperPanel(ParentNode).AlignChildrenVertical;

    // reset invalid combinations
    NewAlignment:=AlignmentResetInvalidCombinations(MyAlignment,self.NodeName,self.NodeType,
                                                    ParentAlignChildrenVertical,self.IsContainer,
                                                    (length(ParentNode.ChildNodes)>1));

    if NewAlignment<>MyAlignment then
    begin
      MyAlignment:=NewAlignment;
      self.SetAttributeValue('Alignment',NewAlignment);
    end;


    asm
    try {
           var wrapper = document.getElementById(this.NameSpace+this.NodeName);
           var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');

           if ((ob!=null)  && (wrapper!=null)) {
             wrapper.classList.remove('hboxNoStretch');
             wrapper.classList.remove('vboxNoStretch');
             wrapper.classList.remove('vboxNoFlex');
             wrapper.classList.remove('AlignmentCentre');
             wrapper.classList.remove('AlignmentRight');
             wrapper.classList.remove('AlignmentLeft');
             wrapper.classList.remove('AlignmentLeftContainer');
             wrapper.classList.remove('AlignmentTop');
             wrapper.classList.remove('AlignmentBottom');


             if (MyAlignment=='Right') {
               if (ParentAlignChildrenVertical) {
               ob.style.float='right';
               wrapper.classList.add('AlignmentRight');
             }
             }
             else if (MyAlignment=='Left') {
                if (ParentAlignChildrenVertical) {
                 ob.style.float='left';
                  if (ContainerType==true) {
                   wrapper.classList.add('AlignmentLeftContainer'); }
                 else {
                   wrapper.classList.add('AlignmentLeft');  }
               }
               }
             else if (MyAlignment=='Centre') {
               ob.style.float='left';
                wrapper.classList.add('AlignmentCentre');
             }

             else if (MyAlignment=='Top') {
             if (ParentAlignChildrenVertical==false) {
               ob.style.float='left';
               wrapper.classList.add('AlignmentTop');
             }
             }
             else if (MyAlignment=='Bottom') {
             if (ParentAlignChildrenVertical==false) {
               ob.style.float='left';
               wrapper.classList.add('AlignmentBottom');
             }
           }

         }
       } catch(err) { alert(err.message+'  in WrapperPanel.SortOutAlignment'); }
    end;
  end;
end;

procedure TWrapperPanel.SetLabelPos(AValue:string);
var
  lp:string;
begin
  myNode.SetAttributeValue('LabelPos',AValue);
  SortOutMyAlignmentAndLabelPos;
end;

{$endif}

function TWrapperPanel.GetSpacingAround:integer;
var
  str:String;
begin
  str:=mynode.GetAttribute('SpacingAround',true).AttribValue;
  if str='' then str:='0';
  result:=StrToInt(str);
end;
procedure TWrapperPanel.SetSpacingAround(AValue:integer);
var
  str:String;
begin
  str:=intToStr(AValue);
  if myNode<>nil then
    myNode.SetAttributeValue('SpacingAround',str,'Integer');

  {$ifndef JScript}
  self.BorderSpacing.Around:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName);
    if (ob!=null) {
      ob.style.margin=str+'px';
    }
  end;
  {$endif}
end;
procedure TWrapperPanel.SetBorder(AValue:Boolean);
begin
  myNode.SetAttributeValue('Border',myBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  if AValue=true then
  begin
    self.BevelInner:=bvLowered;
    self.BevelOuter:=bvRaised;
    self.BevelWidth:=1;
  end
  else
  begin
    self.BevelInner:=bvNone;
    self.BevelOuter:=bvNone;
    self.BorderStyle := bsNone;
    self.BorderWidth:=0;
  end;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName);
    if (ob!=null) {
    if (AValue==true ) {
      ob.classList.remove("no-border");
      ob.classList.add("normal-border");
    }
    else {
       ob.classList.remove("normal-border");
       ob.classList.add("no-border");
    } }
  end;
  {$endif}
end;


{$ifndef JScript}
function TWrapperPanel.GetBgColor:TColor;
begin
  result:=HexRGBToColor(myNode.GetAttribute('BgColor',true).AttribValue);
end;
procedure TWrapperPanel.SetBgColor(AValue:TColor);
var str:string;
begin
  if myNode<>nil then
  begin
    str:= ColorToHexRGB(AValue);
    myNode.SetAttributeValue('BgColor',str,'Color');
  end;

  if ParentColor=false then
    Color:=AValue;
end;

{$else}
function TWrapperPanel.GetBgColor:string;
begin
  result:=myNode.GetAttribute('BgColor',true).AttribValue;
end;
procedure TWrapperPanel.SetBgColor(AValue:string);
begin
  SetAttributeValue('BgColor',AValue,'Color');

  asm
  try {
    var ob = document.getElementById(this.NameSpace+this.NodeName);
    if (ob!=null) {
    ob.style.backgroundColor = AValue;  }
    } catch(err) { alert(err.message+'  in WrapperPanel.SetBgColor'); }
  end;

end;
{$endif}



{$ifndef JScript}


function TWrapperPanel.DoAlignChildControls(TheAlign: TAlign; AControl: TControl;
                     AControlList: TFPList; var ARect: TRect): Boolean;
var
  i,j:integer;
  NewList:TFPList;
  tmp:TControl;
begin
  if self.IsContainer then
    AControlList.Sort(@SortAlignList);
  inherited DoAlignChildControls(TheAlign,AControl,AControlList,ARect);
end;


{$endif}
begin
  {$ifndef JScript}
  RegisterClass(TPersistent);
  RegisterClass(TComponent);
  RegisterClass(TLCLComponent);
  RegisterClass(TControl);
  RegisterClass(TWinControl);
  RegisterClass(TCustomControl);
  RegisterClass(TCustomPanel);
  RegisterClass(TWrapperPanel);

  // Hide some properties in the Lazarus IDE
  SuppressWrapperDesignerProperties;
  {$endif}

end.

