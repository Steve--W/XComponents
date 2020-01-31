(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XGroupBox;

{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,TypInfo, NodeUtils,StringUtils,
  {$ifndef JScript}
  LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, Types, Messages,
  Propedits,RTTICtrls,Menus,
  LazsUtils, Events,
  {$else}
  HTMLUtils,
  {$endif}
  WrapperPanel;

{$ifndef JScript}
type
  // Container component - cannot descend from WrapperPanel, must be  a stand-alone container to work with Lazarus IDE
  TXGroupBox = class(TGroupBox)
  private
    FSelectionBorderColor: TColor;
    FIsSelected:Boolean;
    FIsContainer:Boolean;
    FAlignChildrenVertical:Boolean;
    fHandleClick:TEventHandler;
    FmyNode:TDataNode;
    myExtension:TXDesignerExtension;

    procedure SetMyEventTypes;

    procedure GroupBoxclick(Sender:TObject);
    function GetName:string;
    function GetContainerWidth:string;
    function GetContainerHeight:string;
    function GetHint:string;
    function GetBgColor:TColor;
    function GetAlignment:String;
    function GetCaption:String;
    function GetIsVisible:Boolean;
    function GetHTMLClasses:String;

    procedure SetMyName(AValue:string);
    procedure SetIsSelected(AValue: Boolean);
    procedure SetSelectionBorderColor(AValue: TColor);
    procedure SetContainerHeight(AValue:string);
    procedure SetContainerWidth(AValue:string);
    procedure SetHint(AValue:string);
    procedure SetBgColor(AValue:TColor);
    procedure SetAlignment(AValue:string);
    procedure SetName(const NewName: TComponentName); override;
    procedure SetParent(NewParent: TWinControl); override;
    procedure SetCaption(AValue:string);
    procedure SetIsVisible(AValue:Boolean);
    procedure SetHTMLClasses(AValue:string);

    function DoAlignChildControls(TheAlign: TAlign; AControl: TControl;
                      AControlList: TFPList; var ARect: TRect): Boolean; override;
  public
     myEventTypes:TStringList;
     constructor Create(TheOwner: TComponent); override;
     constructor Create(TheOwner: TComponent;IsDynamic:Boolean); virtual;
     procedure SortOutAlignment;
 protected
   procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
   procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
 published
   property ClientHeight;
   property ParentColor;

   property AutoSize;
   property Align;
   property Anchors;

   property myNode:TDataNode read FmyNode write FmyNode;
   property IsContainer:Boolean read FIsContainer write FIsContainer;
   property AlignChildrenVertical:Boolean read FAlignChildrenVertical write FAlignChildrenVertical;
   property IsSelected: Boolean read FIsSelected write SetIsSelected default false;
   property SelectionBorderColor: TColor read FSelectionBorderColor write SetSelectionBorderColor default clGreen;
   property Alignment:String read GetAlignment write SetAlignment;
   property IsVisible:Boolean read GetIsVisible write SetIsVisible;
   property HTMLClasses: String read GetHTMLClasses write SetHTMLClasses;

   property Hint: String read GetHint write SetHint;
   property Name: String read GetName write SetMyName;
   property ContainerWidth: String read GetContainerWidth write SetContainerWidth;
   property ContainerHeight: String read GetContainerHeight write SetContainerHeight;
   property BgColor: TColor read GetBgColor write SetBgColor;

   property Caption: String read GetCaption write SetCaption;

   // Events to be visible in Lazarus IDE
   property HandleClick: TEventHandler read FHandleClick write FHandleClick;
 end;

procedure Register;

{$else}
type
  TXGroupBox = class(TWrapperPanel)
  private
    procedure SetMyEventTypes;

    function GetCaption:String;
    procedure SetCaption(AValue:string);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(MyForm:TForm;NodeName,NameSpace:String);

  published
    { Published declarations }
    property Caption: String read GetCaption write SetCaption;

  end;
{$endif}


implementation

const MyNodeType='TXGroupBox';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXGroupBox.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
end;

{$ifndef JScript}
procedure Register;
begin
  {$I xgroupbox_icon.lrs}
  RegisterComponents('XComponents',[TXGroupBox]);

  //special property editors
  RegisterPropertyEditor (TypeInfo(string), TXGroupBox, 'Alignment', TAlignmentProperty);

  // Hide some inherited properties
  RegisterPropertyEditor(TypeInfo(TAlign), TXGroupBox, 'Align', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TAnchors), TXGroupBox, 'Anchors', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXGroupBox, 'AutoScroll', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXGroupBox, 'AutoSize', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TBiDiMode), TXGroupBox, 'BiDiMode', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXGroupBox, 'ParentBiDiMode', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlBorderSpacing), TXGroupBox, 'BorderSpacing', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlBorderStyle), TXGroupBox, 'BorderStyle', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TColor), TXGroupBox, 'Color', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXGroupBox, 'DockSite', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXGroupBox, 'DoubleBuffered', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCursor), TXGroupBox, 'DragCursor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDragKind), TXGroupBox, 'DragKind', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDragMode), TXGroupBox, 'DragMode', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXGroupBox, 'Enabled', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TFont), TXGroupBox, 'Font', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXGroupBox, 'ParentFont', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXGroupBox, 'ShowHint', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXGroupBox, 'ParentDoubleBuffered', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXGroupBox, 'ParentShowHint', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TPopupmenu), TXGroupBox, 'PopupMenu', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTabOrder), TXGroupBox, 'TabOrder', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXGroupBox, 'TabStop', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXGroupBox, 'Visible', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXGroupBox, 'Height', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXGroupBox, 'Width', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXGroupBox, 'Top', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXGroupBox, 'Left', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCursor), TXGroupBox, 'Cursor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXGroupBox, 'Tag', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(THelpContext), TXGroupBox, 'HelpContext', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(THelpType), TXGroupBox, 'HelpType', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXGroupBox, 'HelpKeyword', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlChildSizing), TXGroupBox, 'ChildSizing', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TSizeConstraints), TXGroupBox, 'Constraints', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXGroupBox, 'ParentColor', THiddenPropertyEditor);

  RegisterPropertyEditor(TypeInfo(Boolean), TXGroupBox, 'AlignChildrenVertical', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXGroupBox, 'IsContainer', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXGroupBox, 'IsSelected', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TColor), TXGroupBox, 'SelectionBorderColor', THiddenPropertyEditor);

  //.....suppress unwanted designer events.......
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXGroupBox, 'OnClick', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TConstrainedResizeEvent), TXGroupBox, 'OnConstrainedResize', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXGroupBox, 'OnDblClick', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TContextpopupEvent), TXGroupBox, 'OnContextpopup', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDockDropEvent), TXGroupBox, 'OnDockDrop', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDockOverEvent), TXGroupBox, 'OnDockOver', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDragDropEvent), TXGroupBox, 'OnDragDrop', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDragOverEvent), TXGroupBox, 'OnDragOver', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TEndDragEvent), TXGroupBox, 'OnEndDock', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TEndDragEvent), TXGroupBox, 'OnEndDrag', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXGroupBox, 'OnEnter', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXGroupBox, 'OnExit', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TGetSiteInfoEvent), TXGroupBox, 'OnGetSiteInfo', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMouseEvent), TXGroupBox, 'OnMouseDown', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXGroupBox, 'OnMouseEnter', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXGroupBox, 'OnMouseLeave', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMouseMoveEvent), TXGroupBox, 'OnMouseMove', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMouseEvent), TXGroupBox, 'OnMouseUp', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMouseWheelEvent), TXGroupBox, 'OnMouseWheel', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMouseWheelUpDownEvent), TXGroupBox, 'OnMouseWheelDown', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMouseWheelUpDownEvent), TXGroupBox, 'OnMouseWheelUp', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXGroupBox, 'OnPaint', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXGroupBox, 'OnResize', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStartDockEvent), TXGroupBox, 'OnStartDock', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStartDragEvent), TXGroupBox, 'OnStartDrag', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TUnDockEvent), TXGroupBox, 'OnUnDock', THiddenPropertyEditor);
end;

procedure TXGroupBox.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin

  AutoSize:=true;
  //ParentColor:=false;
  BorderStyle := bsNone;
  BorderWidth:=1;

  self.ParentColor:=true;

  self.OnClick:=@GroupBoxClick;

  MyEventTypes:=TStringList.Create;
  self.SetMyEventTypes;

  CreateComponentDataNode2(self,MyNodeType,myDefaultAttribs, self.myEventTypes, TheOwner,IsDynamic);

  // Lazarus Designer extension...
  if csDesigning in componentState then
   begin
      myExtension:=TXDesignerExtension.Create;
      myExtension.myWrapper:=self;
   end;

  IsSelected:=false;
  IsContainer:=true;
  SelectionBorderColor:=glbSelectionBorderColor;

  AlignChildrenVertical:=true;

end;


constructor TXGroupBox.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,false);
end;

constructor TXGroupBox.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,IsDynamic);
end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXGroupBox',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;

procedure TXGroupBox.SetParent(NewParent: TWinControl);
begin
  inherited;
  CheckParentIsXContainer(self);
  ResetAlignment(TControl(self));
end;

procedure TXGroupBox.GroupBoxClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;

procedure TXGroupBox.WMPaint(var Message: TWMPaint);
var
  MCanvas:TControlCanvas;
  DrawBounds: TRect;
begin
  inherited;
  MCanvas := TControlCanvas.Create;
  DrawBounds := self.GetClientRect;
  try
    MCanvas.Control := Self;
    PaintSelectedRectangle(MCanvas,DrawBounds,glbSelectionBorderColor,self.IsSelected);
  finally
    MCanvas.Free;
  end;
end;

procedure TXGroupBox.SortOutAlignment;
var
    MyAlignment,MyLabelPos,NewAlignment:String;
begin
  if (self.Parent<>nil) and (self.Name<>'') then
  begin
    //showmessage('sortout '+self.Name);
    MyAlignment:=self.Alignment;
    MyLabelPos:='';

    NewAlignment:=SortOutAlignmentAndLabelPos(self,nil,nil,MyAlignment,MyLabelPos);

    if NewAlignment<>MyAlignment then
    begin
     // showmessage('old='+self.Alignment+' new='+NewAlignment);
      self.Alignment:=NewAlignment;
     // showmessage('change done');
    end;
  end;
end;

{$else}
constructor TXGroupBox.Create(MyForm:TForm;NodeName,NameSpace:String);
begin
  inherited Create(NodeName,NameSpace);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  AlignChildrenVertical:=true;

  self.SetMyEventTypes;
  self.IsContainer:=true;

  SetNodePropDefaults(self,myDefaultAttribs);

end;


function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  Caption:string;
  OnClickString:String;
begin
  Caption:= uppercase(MyNode.getAttribute('Caption',true).AttribValue);

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''','''+NameSpace+''', this.value);" ';
  //showmessage('groupbox createwidget');

  asm
    try{

    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,NameSpace,$impl.MyNodeType,position);

    var HTMLString='';
    var wrapperid =  NameSpace+ScreenObjectName;
    var MyObjectName=wrapperid+'Contents';

   // HTMLString = '<div  id='+MyObjectName+ ' style=" height:100%; width:100%; position:relative; z-index:0;" ' +
    HTMLString = '<div  id='+MyObjectName+ ' style=" height:100%; width:100%; z-index:0;"  class="vboxNoStretch widgetinner '+NameSpace+ScreenObjectName+'" ' +
                 OnClickString +
                  '><legend>'+Caption+'</legend></div>';


    var wrapper=document.getElementById(wrapperid);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);

  }
  catch(err) { alert(err.message+'  in XScrollBox.CreateWidget');}

  end;

  MyNode.ScreenObject:=MyNode;

  RefreshComponentProps(myNode);

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  //showmessage('CreateinterfaceObj '+NodeName);
  result:=TObject(TXGroupBox.Create(MyForm,NodeName,NameSpace));
end;

{$endif}

{$ifndef JScript}
function TXGroupBox.GetName:string;
var
  myname:string;
begin
  result:=inherited Name;
end;
function TXGroupBox.GetHint:string;
begin
  result:=myNode.GetAttribute('Hint',true).AttribValue;
end;
function TXGroupBox.GetContainerWidth:string;
begin
  result:=myNode.GetAttribute('ContainerWidth',true).AttribValue;
end;
function TXGroupBox.GetContainerHeight:string;
begin
  result:=myNode.GetAttribute('ContainerHeight',true).AttribValue;
end;

// Name is the first property loaded from .lfm.
// Hijacking this so that we can reset blank default values for all string properties
// (because there is a problem - string properties are NOT saved to lfm when the value is blank, so
// if the user wants the property to be blank, then we shouldn't set any non-blank defaults when
// re-loading the project from lfm).
procedure TXGroupBox.SetName(const NewName: TComponentName);
var
  ApplyName:TComponentName;
begin
  ApplyName:=GetNameToApply(self.myNode,self.myNode.MyForm.Name,self.Name,NewName,componentState);

  inherited SetName(ApplyName);

  if (csLoading in componentState) then
  begin
//    showmessage('TWrapperPanel setname. loading.');

    ContainerWidth:='';
    ContainerHeight:='';
    Hint:='';
  end;

end;

function TXGroupBox.GetAlignment:String;
begin
  result:=MyNode.getAttribute('Alignment',true).AttribValue;
end;
procedure TXGroupBox.SetAlignment(AValue:string);
begin
  if myNode<>nil then
  begin
    myNode.SetAttributeValue('Alignment',AValue);

    if self.Parent<>nil then
    begin
      self.SortOutAlignment;
    end;
  end;
end;

{$endif}

function TXGroupBox.GetCaption:string;
begin
  result:=MyNode.getAttribute('Caption',true).AttribValue;
end;

{$ifndef JScript}
procedure TXGroupBox.SetMyName(AValue:string);
begin
  inherited Name:=AValue;

  if  (csLoading in componentState) then
    if myNode<>nil then
      myNode.NodeName:=AValue;
end;

procedure TXGroupBox.SetSelectionBorderColor(AValue: TColor);
// Colour of the 'IsSelected' dotted border
begin
  if AValue<>FSelectionBorderColor then
    begin
      FSelectionBorderColor:=AValue;
      Repaint;
    end;
end;

procedure TXGroupBox.SetIsSelected(AValue: Boolean);
begin
  if AValue<>FIsSelected then
    begin
      FIsSelected:=AValue;
      ShowHideSelectedBorder(self.myNode,FIsSelected);
      Repaint;
    end;
end;
function TXGroupBox.GetIsVisible:Boolean;
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
procedure TXGroupBox.SetIsVisible(AValue:Boolean);
begin
  if myNode<>nil then
    myNode.SetAttributeValue('IsVisible',myBoolToStr(AValue),'Boolean');
  self.Visible:=AValue;
end;


procedure TXGroupBox.SetHint(AValue:string);
begin
  myNode.SetAttributeValue('Hint',AValue);
  if AValue<>'' then
    self.ShowHint:=true
  else
    self.ShowHint:=false;
  inherited Hint:=AValue;
end;

procedure TXGroupBox.SetContainerWidth(AValue:string);
begin
  myNode.SetAttributeValue('ContainerWidth',AValue);
  SetHeightWidth(self.myNode,TControl(self),'ContainerWidth','ContainerHeight');
end;

procedure TXGroupBox.SetContainerHeight(AValue:string);
begin
  myNode.SetAttributeValue('ContainerHeight',AValue);
  SetHeightWidth(self.myNode,TControl(self),'ContainerWidth','ContainerHeight');
end;

function TXGroupBox.GetHTMLClasses:string;
begin
  result:=myNode.GetAttribute('HTMLClasses',true).AttribValue;
end;
procedure TXGroupBox.SetHTMLClasses(AValue:string);
begin
  myNode.SetAttributeValue('HTMLClasses',AValue);
end;
{$endif}

procedure TXGroupBox.SetCaption(AValue:string);
var
  AVal:string;
begin
  myNode.SetAttributeValue('Caption',AValue);
  AVal:=uppercase(AValue);
  {$ifndef JScript}
  inherited Caption:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
      }
  end;
  {$endif}
end;



{$ifndef JScript}
function TXGroupBox.GetBgColor:TColor;
begin
  result:=HexRGBToColor(myNode.GetAttribute('BgColor',true).AttribValue);
end;
procedure TXGroupBox.SetBgColor(AValue:TColor);
var str:string;
begin
  if myNode<>nil then
  begin
    str:= ColorToHexRGB(AValue);
    myNode.SetAttributeValue('BgColor',str,'Color');
  end;

  Color:=AValue;
end;
function TXGroupBox.DoAlignChildControls(TheAlign: TAlign; AControl: TControl;
                     AControlList: TFPList; var ARect: TRect): Boolean;
var
  i,j:integer;
  NewList:TFPList;
  tmp:TControl;
begin
  AControlList.Sort(@SortAlignList);
  inherited DoAlignChildControls(TheAlign,AControl,AControlList,ARect);
end;

{$endif}

begin
  // this is the set of node attributes that each XHBox instance will have.
  AddWrapperDefaultAttribs(myDefaultAttribs);
  AddDefaultAttribute(myDefaultAttribs,'ContainerWidth','String','300px','',false);
  AddDefaultAttribute(myDefaultAttribs,'ContainerHeight','String','300px','',false);
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'BgColor','Color','#555555','',false);
  AddDefaultAttribute(myDefaultAttribs,'Caption','String','Group Caption','',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  {$ifndef JScript}
  RegisterClass(TXGroupBox);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  SuppressDesignerProperty(MyNodeType,'LabelPos');
  SuppressDesignerProperty(MyNodeType,'LabelText');
  {$endif}
end.
