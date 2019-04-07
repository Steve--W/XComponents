(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XScrollBox;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,TypInfo, NodeUtils,StringUtils,
  {$ifndef JScript}
  LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, Types,
  Propedits,RTTICtrls,Menus,
  LazsUtils, Events, XTabControl,
  {$else}
  HTMLUtils,
  {$endif}
  WrapperPanel;

{$ifndef JScript}

type
  // Container component - cannot descend from WrapperPanel, must be  a stand-alone container to work with Lazarus IDE
  TXScrollBox = class(TScrollBox)
  private
    FSelectionBorderColor: TColor;
    FIsSelected:Boolean;
    FIsContainer:Boolean;
    FAlignChildrenVertical:Boolean;
    fHandleClick:TEventHandler;
    FmyNode:TDataNode;
    myExtension:TXDesignerExtension;

    procedure SetMyEventTypes;

    procedure ScrollBoxclick(Sender:TObject);
    function GetName:string;
    function GetContainerWidth:string;
    function GetContainerHeight:string;
    function GetHint:string;
    function GetBgColor:TColor;
    function GetScrollType:string;
    function GetAlignment:String;
    function GetIsVisible:Boolean;
    function GetSpacingAround:integer;

    procedure SetMyName(AValue:string);
    procedure SetIsSelected(AValue: Boolean);
    procedure SetSelectionBorderColor(AValue: TColor);
    procedure SetContainerHeight(AValue:string);
    procedure SetContainerWidth(AValue:string);
    procedure SetHint(AValue:string);
    procedure SetBgColor(AValue:TColor);
    procedure SetScrollType(AValue:string);
    procedure SetAlignment(AValue:string);
    procedure SetIsVisible(AValue:Boolean);
    procedure SetSpacingAround(AValue:integer);
    procedure SetName(const NewName: TComponentName); override;
    procedure SetParent(NewParent: TWinControl); override;

    function DoAlignChildControls(TheAlign: TAlign; AControl: TControl;
                      AControlList: TFPList; var ARect: TRect): Boolean; override;
  public
     myEventTypes:TStringList;
     constructor Create(TheOwner: TComponent); override;
     constructor Create(TheOwner: TComponent;IsDynamic:Boolean); virtual;
     procedure SortOutAlignment;
 protected
   procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
   procedure Paint;override;
   procedure Loaded; override;
 published
   property Caption;
   property ClientHeight;
   property ParentColor;

    property AutoSize;
   property Align;
   property Anchors;
   property Constraints;
   property Childsizing;

   property myNode:TDataNode read FmyNode write FmyNode;
   property IsContainer:Boolean read FIsContainer write FIsContainer;
   property AlignChildrenVertical:Boolean read FAlignChildrenVertical write FAlignChildrenVertical;
   property IsSelected: Boolean read FIsSelected write SetIsSelected default false;
   property SelectionBorderColor: TColor read FSelectionBorderColor write SetSelectionBorderColor default clGreen;
   property Alignment:String read GetAlignment write SetAlignment;
   property IsVisible:Boolean read GetIsVisible write SetIsVisible;
   property SpacingAround:integer read GetSpacingAround write SetSpacingAround;

   property Hint: String read GetHint write SetHint;
   property Name: String read GetName write SetMyName;
   property ContainerWidth: String read GetContainerWidth write SetContainerWidth;
   property ContainerHeight: String read GetContainerHeight write SetContainerHeight;
   property BgColor: TColor read GetBgColor write SetBgColor;

   property ScrollType: String read GetScrollType write SetScrollType;

   // Events to be visible in Lazarus IDE
   property HandleClick: TEventHandler read FHandleClick write FHandleClick;
 end;


  procedure Register;


{$else}
type
  TXScrollBox = class(TWrapperPanel)
  private
    procedure SetMyEventTypes;

    function GetScrollType:string;

    procedure SetScrollType(AValue:string);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(MyForm:TForm;NodeName:String);

  published
    { Published declarations }
    property ScrollType: String read GetScrollType write SetScrollType;

  end;
{$endif}


implementation

const MyNodeType='TXScrollBox';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXScrollBox.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
end;

{$ifndef JScript}
procedure Register;
begin
  // Lazarus IDE component registration
  RegisterComponents('XComponents',[TXScrollBox]);

  //special property editors
  RegisterPropertyEditor (TypeInfo(string), TXScrollBox, 'ScrollType', TScrollBarsProperty);
  RegisterPropertyEditor (TypeInfo(string), TXScrollBox, 'Alignment', TAlignmentProperty);

  // Hide some inherited properties
  RegisterPropertyEditor(TypeInfo(TAlign), TXScrollBox, 'Align', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TAnchors), TXScrollBox, 'Anchors', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'AutoScroll', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'AutoSize', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TBiDiMode), TXScrollBox, 'BiDiMode', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'ParentBiDiMode', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlBorderSpacing), TXScrollBox, 'BorderSpacing', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlBorderStyle), TXScrollBox, 'BorderStyle', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TColor), TXScrollBox, 'Color', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'DockSite', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCursor), TXScrollBox, 'DragCursor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDragKind), TXScrollBox, 'DragKind', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDragMode), TXScrollBox, 'DragMode', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'Enabled', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TFont), TXScrollBox, 'Font', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'ParentFont', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlScrollBar), TXScrollBox, 'HorzScrollBar', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlScrollBar), TXScrollBox, 'VertScrollBar', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'ShowHint', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'ParentShowHint', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TPopupmenu), TXScrollBox, 'PopupMenu', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTabOrder), TXScrollBox, 'TabOrder', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'TabStop', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'Visible', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXScrollBox, 'Height', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXScrollBox, 'Width', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXScrollBox, 'Top', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXScrollBox, 'Left', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCursor), TXScrollBox, 'Cursor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXScrollBox, 'Tag', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(THelpContext), TXScrollBox, 'HelpContext', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(THelpType), TXScrollBox, 'HelpType', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXScrollBox, 'HelpKeyword', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCaption), TXScrollBox, 'Caption', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlChildSizing), TXScrollBox, 'ChildSizing', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TSizeConstraints), TXScrollBox, 'Constraints', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'ParentColor', THiddenPropertyEditor);

  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'AlignChildrenVertical', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'IsContainer', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'IsSelected', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TColor), TXScrollBox, 'SelectionBorderColor', THiddenPropertyEditor);

  //.....suppress unwanted designer events.......
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXScrollBox, 'OnClick', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TConstrainedResizeEvent), TXScrollBox, 'OnConstrainedResize', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXScrollBox, 'OnDblClick', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TContextpopupEvent), TXScrollBox, 'OnContextpopup', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDockDropEvent), TXScrollBox, 'OnDockDrop', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDockOverEvent), TXScrollBox, 'OnDockOver', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDragDropEvent), TXScrollBox, 'OnDragDrop', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDragOverEvent), TXScrollBox, 'OnDragOver', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TEndDragEvent), TXScrollBox, 'OnEndDock', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TEndDragEvent), TXScrollBox, 'OnEndDrag', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXScrollBox, 'OnEnter', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXScrollBox, 'OnExit', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TGetSiteInfoEvent), TXScrollBox, 'OnGetSiteInfo', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMouseEvent), TXScrollBox, 'OnMouseDown', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXScrollBox, 'OnMouseEnter', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXScrollBox, 'OnMouseLeave', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMouseMoveEvent), TXScrollBox, 'OnMouseMove', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMouseEvent), TXScrollBox, 'OnMouseUp', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMouseWheelEvent), TXScrollBox, 'OnMouseWheel', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMouseWheelUpDownEvent), TXScrollBox, 'OnMouseWheelDown', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMouseWheelUpDownEvent), TXScrollBox, 'OnMouseWheelUp', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXScrollBox, 'OnPaint', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXScrollBox, 'OnResize', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStartDockEvent), TXScrollBox, 'OnStartDock', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStartDragEvent), TXScrollBox, 'OnStartDrag', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TUnDockEvent), TXScrollBox, 'OnUnDock', THiddenPropertyEditor);
end;

procedure TXScrollBox.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  ControlStyle := ControlStyle - [csSetCaption];

  AutoSize:=true;
  ParentColor:=false;
  BorderStyle := bsNone;
  BorderWidth:=0;
  Caption:='';
  AutoScroll:=true;


  self.OnClick:=@ScrollBoxClick;

  MyEventTypes:=TStringList.Create;
  self.SetMyEventTypes;
  //self.myNode:=CreateComponentDataNode(self.Name,MyNodeType, self.myEventTypes, self,TheOwner,IsDynamic);
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

constructor TXScrollBox.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,false);
end;

constructor TXScrollBox.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,IsDynamic);
end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXScrollBox',ParentNode.MyForm,ParentNode,ScreenObjectName,Alignment,position);
  result:=NewNode;
end;

procedure TXScrollBox.SetParent(NewParent: TWinControl);
begin
  inherited;
  CheckParentIsXContainer(self);
  ResetAlignment(TControl(self));
end;

procedure TXScrollBox.ScrollBoxClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;

procedure TXScrollBox.Paint;
begin
  inherited Paint;
  PaintSelectedRectangle(TCustomControl(self).Canvas,self.GetClientRect,self.SelectionBorderColor,self.IsSelected);
end;

procedure TXScrollBox.Loaded;
var
i:integer;
begin
  inherited Loaded;

  // special case (MyRootDiv for template project has align=alClient)
  if align=alClient then
  begin
     ContainerWidth:='';
     ContainerHeight:='';
  end;

end;

procedure TXScrollBox.SortOutAlignment;
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
constructor TXScrollBox.Create(MyForm:TForm;NodeName:String);
begin
  inherited Create(NodeName);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  AlignChildrenVertical:=true;

  self.SetMyEventTypes;
  self.IsContainer:=true;

  SetNodePropDefaults(self,myDefaultAttribs);
end;


function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer;Alignment:String):TDataNode;
var
  ScrollType:string;
  OnChangeString, OnClickString, OnPasteString:String;
begin
  ScrollType:= uppercase(MyNode.getAttribute('ScrollType',true).AttribValue);

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''', this.value);" ';
 // showmessage('scrollbox createwidget '+ScreenObjectName);

  asm
    try{

      var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);

      var HTMLString='';
      var MyObjectName=ScreenObjectName+'Contents';
      var oflow = ''
      if ((ScrollType=='BOTH')||(ScrollType=='RIGHT')) {oflow = 'overflow-y:scroll; '}
      if ((ScrollType=='BOTH')||(ScrollType=='BOTTOM')) {oflow = oflow+'overflow-x:scroll; '}

      HTMLString = '<div id='+MyObjectName+ '  class="vboxNoStretch" style="'+oflow+' height:100%; width:100%;" ' +
                   OnClickString +
                   '></div> ';


      var wrapper=document.getElementById(ScreenObjectName);
      wrapper.insertAdjacentHTML('beforeend', HTMLString);

    }
    catch(err) { alert(err.message+'  in XScrollBox.CreateWidget');}

  end;

  MyNode.ScreenObject:=MyNode;
  RefreshComponentProps(myNode);

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName:String):TObject;
begin
  //showmessage('CreateinterfaceObj '+NodeName);
  result:=TObject(TXScrollBox.Create(MyForm,NodeName));
  //showmessage('CreateinterfaceObj '+NodeName+' done');
end;

{$endif}

{$ifndef JScript}
function TXScrollBox.GetName:string;
var
  myname:string;
begin
  result:=inherited Name;
end;
function TXScrollBox.GetHint:string;
begin
  result:=myNode.GetAttribute('Hint',true).AttribValue;
end;
function TXScrollBox.GetContainerWidth:string;
begin
  result:=myNode.GetAttribute('ContainerWidth',true).AttribValue;
end;
function TXScrollBox.GetContainerHeight:string;
begin
  result:=myNode.GetAttribute('ContainerHeight',true).AttribValue;
end;
function TXScrollBox.GetIsVisible:Boolean;
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
function TXScrollBox.GetSpacingAround:integer;
var
  str:String;
begin
  str:=mynode.GetAttribute('SpacingAround',true).AttribValue;
  if str='' then str:='0';
  result:=StrToInt(str);
end;
procedure TXScrollBox.SetSpacingAround(AValue:integer);
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
    var ob = document.getElementById(this.NodeName);
    if (ob!=null) {
      ob.style.margin=str+'px';
    }
  end;
  {$endif}
end;


// Name is the first property loaded from .lfm.
// Hijacking this so that we can reset blank default values for all string properties
// (because there is a problem - string properties are NOT saved to lfm when the value is blank, so
// if the user wants the property to be blank, then we shouldn't set any non-blank defaults when
// re-loading the project from lfm).
procedure TXScrollBox.SetName(const NewName: TComponentName);
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

function TXScrollBox.GetAlignment:String;
begin
  result:=MyNode.getAttribute('Alignment',true).AttribValue;
end;
procedure TXScrollBox.SetAlignment(AValue:string);
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

function TXScrollBox.GetScrollType:string;
begin
  result:=MyNode.getAttribute('ScrollType',true).AttribValue;
end;


{$ifndef JScript}
procedure TXScrollBox.SetMyName(AValue:string);
begin
  inherited Name:=AValue;

  if myNode<>nil then
     myNode.NodeName:=AValue;
  // also rename any associated event code ???
end;

procedure TXScrollBox.SetSelectionBorderColor(AValue: TColor);
// Colour of the 'IsSelected' dotted border
begin
  if AValue<>FSelectionBorderColor then
    begin
      FSelectionBorderColor:=AValue;
      Repaint;
    end;
end;

procedure TXScrollBox.SetIsSelected(AValue: Boolean);
begin
  if AValue<>FIsSelected then
    begin
      FIsSelected:=AValue;
      ShowHideSelectedBorder(self.myNode,FIsSelected);
      Repaint;
    end;
end;


procedure TXScrollBox.SetHint(AValue:string);
begin
  myNode.SetAttributeValue('Hint',AValue);
  if AValue<>'' then
    self.ShowHint:=true
  else
    self.ShowHint:=false;
  inherited Hint:=AValue;
end;

procedure TXScrollBox.SetContainerWidth(AValue:string);
var
tmp:string;
begin
  myNode.SetAttributeValue('ContainerWidth',AValue);
  SetHeightWidth(self.myNode,TControl(self),'ContainerWidth','ContainerHeight');

end;

procedure TXScrollBox.SetContainerHeight(AValue:string);
begin
  myNode.SetAttributeValue('ContainerHeight',AValue);
  SetHeightWidth(self.myNode,TControl(self),'ContainerWidth','ContainerHeight');
end;

{$endif}


procedure TXScrollBox.SetScrollType(AValue:string);
var
  AVal:string;
begin
  myNode.SetAttributeValue('ScrollType',AValue);
  AVal:=uppercase(AValue);
  {$ifndef JScript}
  if (AVal='BOTH')
  or (AVal='RIGHT') then
     self.VertScrollBar.Visible:=true
  else
    self.VertScrollBar.Visible:=false;
  if (AVal='BOTH')
  or (AVal='BOTTOM') then
    self.HorzScrollBar.Visible:=true
  else
    self.HorzScrollBar.Visible:=false;
  {$else}
  asm
    var ob = document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
      ob.style.overlow='none';
      if ((AVal=='BOTH')||(AVal=='RIGHT')) {ob.style.overflowY='scroll';}
      if ((AVal=='BOTH')||(AVal=='BOTTOM')) {ob.style.overflowX='scroll';}
      }
  end;
  {$endif}
end;



{$ifndef JScript}
function TXScrollBox.GetBgColor:TColor;
begin
  result:=HexRGBToColor(myNode.GetAttribute('BgColor',true).AttribValue);
end;
procedure TXScrollBox.SetBgColor(AValue:TColor);
var str:string;
begin
  if myNode<>nil then
  begin
    str:= ColorToHexRGB(AValue);
    myNode.SetAttributeValue('BgColor',str,'Color');
  end;

  Color:=AValue;
end;
procedure TXScrollBox.SetIsVisible(AValue:Boolean);
begin
  if myNode<>nil then
    myNode.SetAttributeValue('IsVisible',myBoolToStr(AValue),'Boolean');
  self.Visible:=AValue;
end;

function TXScrollBox.DoAlignChildControls(TheAlign: TAlign; AControl: TControl;
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
  // this is the set of node attributes that each TXScrollBox instance will have.
  AddDefaultAttribute(myDefaultAttribs,'Alignment','String','Left','',false);
  AddDefaultAttribute(myDefaultAttribs,'Hint','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'IsVisible','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'ContainerWidth','String','300px','',false);
  AddDefaultAttribute(myDefaultAttribs,'ContainerHeight','String','300px','',false);
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'BgColor','Color','#FFFFFF','',false);
  AddDefaultAttribute(myDefaultAttribs,'ScrollType','String','Both','',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  AddAttribOptions(MyNodeType,'ScrollType',ScrollBarsOptions);
  {$ifndef JScript}
  {$I XScrollBox.lrs}
  RegisterClass(TXScrollBox);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  SuppressDesignerProperty(MyNodeType,'LabelPos');
  SuppressDesignerProperty(MyNodeType,'LabelText');
  {$endif}
end.
