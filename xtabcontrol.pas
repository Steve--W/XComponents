(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XTabControl;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, TypInfo, NodeUtils,StringUtils,
  {$ifndef JScript}
  LResources, Forms, Controls, StdCtrls, ComCtrls, Graphics, Dialogs, ExtCtrls, Types, ImgList,Messages,
  Propedits,RTTICtrls, ComponentEditors, Menus, ObjInspStrConsts,
  LazsUtils, Events, LazLogger,
  {$else}
  HTMLUtils,
  {$endif}
  WrapperPanel;

{$ifdef JScript}
procedure ChangeTabPage(nodeId,parentNodeId,NameSpace:string;TabNode:TDataNode=nil);
{$endif}

{$ifndef JScript}
type
  // Container component - cannot descend from WrapperPanel, must be  a stand-alone tabsheet to work with Lazarus IDE
 TXTabSheet = class(TTabSheet)
  private
  FIsSelected:Boolean;
  FIsContainer:Boolean;
  FAlignChildrenVertical:Boolean;
  fHandleClick:TEventHandler;
  FSelectionBorderColor: TColor;
  FmyNode:TDataNode;

  procedure SetMyEventTypes;
  procedure WMPaint(var Message: TWMPaint); message WM_PAINT;

  procedure TabSheetclick(Sender:TObject);
  //procedure TabSheetChangeBounds(Sender:TObject);

  function GetName:string;
  function GetHint:string;
  function GetBgColor:TColor;
  function GetCaption:string;
  function GetIsVisible:Boolean;
  function GetHTMLClasses:String;

  procedure SetMyName(AValue:string);
  procedure SetIsSelected(AValue: Boolean);
  procedure SetSelectionBorderColor(AValue: TColor);
  procedure SetHint(AValue:string);
  procedure SetBgColor(AValue:TColor);
  procedure SetCaption(AValue:string);
  procedure SetIsVisible(AValue:Boolean);
  procedure SetHTMLClasses(AValue:string);

  procedure SetName(const NewName: TComponentName); override;
  function DoAlignChildControls(TheAlign: TAlign; AControl: TControl;
                    AControlList: TFPList; var ARect: TRect): Boolean; override;
  procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
 // procedure Paint;override;                 // paint override not available
  public
    myEventTypes:TStringList;
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent;IsDynamic:Boolean); virtual;

  published

    property myNode:TDataNode read FmyNode write FmyNode;
    property AlignChildrenVertical:Boolean read FAlignChildrenVertical write FAlignChildrenVertical;
    property IsContainer:Boolean read FIsContainer write FIsContainer;
    property IsSelected: Boolean read FIsSelected write SetIsSelected default false;
    property SelectionBorderColor: TColor read FSelectionBorderColor write SetSelectionBorderColor default clGreen;
    property IsVisible:Boolean read GetIsVisible write SetIsVisible;
    property HTMLClasses: String read GetHTMLClasses write SetHTMLClasses;

    property Hint: String read GetHint write SetHint;
    property Name: String read GetName write SetMyName;
    property Caption: String read GetCaption write SetCaption;
    property BgColor: TColor read GetBgColor write SetBgColor;

    // Events to be visible in Lazarus IDE
    property HandleClick: TEventHandler read FHandleClick write FHandleClick;
end;

 // Container component - cannot descend from WrapperPanel, must be  a stand-alone container to work with Lazarus IDE
  TXTabControl = class(TPageControl)
  private
    FSelectionBorderColor: TColor;
    FIsSelected:Boolean;
    FIsContainer:Boolean;
    fHandleClick:TEventHandler;
    fHandleChange:TEventHandler;
    FmyNode:TDataNode;
    myExtension:TXDesignerExtension;

    procedure SetMyEventTypes;

    //  procedure Paint;override;               // paint override not available
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure TabControlclick(Sender:TObject);
    procedure TabPageChanged(Sender:TObject);

    function GetName:string;
    function GetContainerWidth:string;
    function GetContainerHeight:string;
    function GetHint:string;
    function GetBgColor:TColor;
    function GetAlignment:String;
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
    procedure SetIsVisible(AValue:Boolean);
    procedure SetHTMLClasses(AValue:string);

    function AddTabSheet:TXTabSheet;
    function GetActiveTabSheet: TXTabSheet;
    function GetTabSheet(Index: Integer): TXTabSheet;
    procedure SetActiveTabSheet(const AValue: TXTabSheet);
    function GetmyTabIndex: longint;
    procedure SetmyTabIndex(const AValue: longint);
    function GetPageClass: TCustomPageClass;            override;
    procedure SelectNextPage(GoForward: Boolean);
    procedure SelectNextPage(GoForward: Boolean;CheckTabVisible: Boolean);
    function FindNextPage(CurPage: TXTabSheet; GoForward, CheckTabVisible: Boolean): TXTabSheet ;
    procedure SetName(const NewName: TComponentName); override;
    procedure SetParent(NewParent: TWinControl); override;
    procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
  public
     myEventTypes:TStringList;
     constructor Create(TheOwner: TComponent); override;
     constructor Create(TheOwner: TComponent;IsDynamic:Boolean); virtual;

     procedure SortOutAlignment;
     function GetHeightOfTabs:integer;
     function IndexOfPage(PageId:String):integer;
     property Pages[Index: Integer]: TXTabSheet read GetTabSheet;

 published
   property AutoSize;
   property Align;

   property ActivePage: TXTabSheet read GetActiveTabSheet write SetActiveTabSheet;
   property TabIndex: longint read GetmyTabIndex write SetmyTabIndex;

   property myNode:TDataNode read FmyNode write FmyNode;
   property IsContainer:Boolean read FIsContainer write FIsContainer;
   property IsSelected: Boolean read FIsSelected write SetIsSelected default false;
   property IsVisible:Boolean read GetIsVisible write SetIsVisible;
   property SelectionBorderColor: TColor read FSelectionBorderColor write SetSelectionBorderColor default clGreen;

   property Hint: String read GetHint write SetHint;
   property Name: String read GetName write SetMyName;
   property ContainerWidth: String read GetContainerWidth write SetContainerWidth;
   property ContainerHeight: String read GetContainerHeight write SetContainerHeight;
   property BgColor: TColor read GetBgColor write SetBgColor;
   property Alignment:String read GetAlignment write SetAlignment;
   property HTMLClasses: String read GetHTMLClasses write SetHTMLClasses;

   // Events to be visible in Lazarus IDE
   property HandleClick: TEventHandler read FHandleClick write FHandleClick;
   property HandleChange: TEventHandler read FHandleChange write FHandleChange;
 end;


 TXTabControlComponentEditor = class(TDefaultComponentEditor)
  protected
    procedure AddNewPageToDesigner(Index: integer); virtual;
    procedure DoAddPage; virtual;
    procedure DoInsertPage; virtual;
    procedure DoDeletePage; virtual;
    procedure DoMoveActivePageLeft; virtual;
    procedure DoMoveActivePageRight; virtual;
    procedure DoMovePage(CurIndex, NewIndex: Integer); virtual;
    procedure AddMenuItemsForPages(ParentMenuItem: TMenuItem); virtual;
    procedure ShowPageMenuItemClick(Sender: TObject);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AnItem: TMenuItem); override;
    function TabControl: TXTabControl; virtual;
  end;

procedure Register;

{$else}
type
  TXTabControl = class(TWrapperPanel)
    procedure SetMyEventTypes;
    function GetmyTabIndex: longint;
    procedure SetmyTabIndex(const AValue: longint);
 public
     function IndexOfPage(PageId:String):integer;
     constructor Create(MyForm:TForm;NodeName,NameSpace:String);
 published
   property TabIndex: longint read GetmyTabIndex write SetmyTabIndex;

  end;

type
  TXTabSheet = class(TWrapperPanel)
  private
    function GetCaption:string;
    procedure SetCaption(AValue:string);

    procedure SetMyEventTypes;

  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(MyForm:TForm;NodeName,NameSpace:String);

  published
    { Published declarations }
    property Caption: String read GetCaption write SetCaption;

  end;

procedure RebuildButtons(TCNode:TdataNode);

{$endif}


implementation
uses XScrollBox;

const MyNodeType='TXTabControl';
var
  ControlDefaultAttribs:TDefaultAttributesArray;
  PageDefaultAttribs:TDefaultAttributesArray;

procedure TXTabControl.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
  MyEventTypes.Add('Change');
end;
procedure TXTabSheet.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
end;

{$ifndef JScript}

procedure Register;
begin
  {$I xtabcontrol_icon.lrs}
  // Lazarus IDE component registration
  RegisterComponents('XComponents',[TXTabControl]);
  RegisterNoIcon([TXTabSheet]);

  //special property editors
  RegisterPropertyEditor (TypeInfo(string), TXTabControl, 'Alignment', TAlignmentProperty);

  // Hide some inherited properties
  RegisterPropertyEditor(TypeInfo(TXTabSheet), TXTabControl, 'ActivePage', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TAlign), TXTabControl, 'Align', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TAnchors), TXTabControl, 'Anchors', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'AutoScroll', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'AutoSize', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TBiDiMode), TXTabControl, 'BiDiMode', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'ParentBiDiMode', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlBorderSpacing), TXTabControl, 'BorderSpacing', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlBorderStyle), TXTabControl, 'BorderStyle', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TColor), TXTabControl, 'Color', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'DockSite', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCursor), TXTabControl, 'DragCursor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDragKind), TXTabControl, 'DragKind', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDragMode), TXTabControl, 'DragMode', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'Enabled', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TFont), TXTabControl, 'Font', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'ParentFont', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlScrollBar), TXTabControl, 'HorzScrollBar', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlScrollBar), TXTabControl, 'VertScrollBar', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'ShowHint', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'ParentShowHint', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TPopupmenu), TXTabControl, 'PopupMenu', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTabOrder), TXTabControl, 'TabOrder', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'TabStop', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'Visible', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXTabControl, 'Height', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXTabControl, 'Width', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXTabControl, 'Top', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXTabControl, 'Left', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCursor), TXTabControl, 'Cursor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXTabControl, 'Tag', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(THelpContext), TXTabControl, 'HelpContext', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(THelpType), TXTabControl, 'HelpType', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXTabControl, 'HelpKeyword', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlChildSizing), TXTabControl, 'ChildSizing', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TSizeConstraints), TXTabControl, 'Constraints', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'ParentColor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'HotTrack', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCustomImageList), TXTabControl, 'Images', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(longInt), TXTabControl, 'ImagesWidth', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'MultiLine', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'MultiSelect', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'OwnerDraw', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'RaggedRight', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'ScrollOpposite', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTabStyle), TXTabControl, 'Style', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(SmallInt), TXTabControl, 'TabHeight', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTabPosition), TXTabControl, 'TabPosition', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(SmallInt), TXTabControl, 'TabWidth', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCTabControlOptions), TXTabControl, 'Options', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'ShowTabs', THiddenPropertyEditor);

  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'IsContainer', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'IsSelected', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TColor), TXTabControl, 'SelectionBorderColor', THiddenPropertyEditor);



//.....TXTabControl events.......
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabControl, 'OnChange', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TTabChangingEvent), TXTabControl, 'OnChanging', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabControl, 'OnCloseTabClicked', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TContextpopupEvent), TXTabControl, 'OnContextpopup', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TDockDropEvent), TXTabControl, 'OnDockDrop', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TDockOverEvent), TXTabControl, 'OnDockOver', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TDragDropEvent), TXTabControl, 'OnDragDrop', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TDragOverEvent), TXTabControl, 'OnDragOver', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TEndDragEvent), TXTabControl, 'OnEndDock', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TEndDragEvent), TXTabControl, 'OnEndDrag', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabControl, 'OnEnter', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabControl, 'OnExit', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TGetDockCaptionEvent), TXTabControl, 'OnGetDockCaption', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TGetDockCaptionEvent), TXTabControl, 'OnGetDockCaption', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TTabGetImageEvent), TXTabControl, 'OnGetImageIndex', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TGetSiteInfoEvent), TXTabControl, 'OnGetSiteInfo', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TMouseEvent), TXTabControl, 'OnMouseDown', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabControl, 'OnMouseEnter', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabControl, 'OnMouseLeave', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TMouseMoveEvent), TXTabControl, 'OnMouseMove', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TMouseEvent), TXTabControl, 'OnMouseUp', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TMouseWheelEvent), TXTabControl, 'OnMouseWheel', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TMouseWheelUpDownEvent), TXTabControl, 'OnMouseWheelDown', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TMouseWheelUpDownEvent), TXTabControl, 'OnMouseWheelUp', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabControl, 'OnResize', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TStartDockEvent), TXTabControl, 'OnStartDock', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TStartDragEvent), TXTabControl, 'OnStartDrag', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TUnDockEvent), TXTabControl, 'OnUnDock', THiddenPropertyEditor);

//.....TXTabSheet properties.......
RegisterPropertyEditor(TypeInfo(TBiDiMode), TXTabSheet, 'BiDiMode', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Boolean), TXTabSheet, 'ParentBiDiMode', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TBorderWidth), TXTabSheet, 'BorderWidth', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TControlChildSizing), TXTabSheet, 'ChildSizing', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TCursor), TXTabSheet, 'Cursor', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Boolean), TXTabSheet, 'Enabled', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TFont), TXTabSheet, 'Font', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Integer), TXTabSheet, 'Height', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(THelpContext), TXTabSheet, 'HelpContext', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(THelpType), TXTabSheet, 'HelpType', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(String), TXTabSheet, 'HelpKeyword', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TImageIndex), TXTabSheet, 'ImageIndex', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Integer), TXTabSheet, 'Left', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Longint), TXTabSheet, 'PageIndex', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Boolean), TXTabSheet, 'ParentFont', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Boolean), TXTabSheet, 'ParentShowHint', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Boolean), TXTabSheet, 'ShowHint', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TPopupmenu), TXTabSheet, 'PopupMenu', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Integer), TXTabSheet, 'Top', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Integer), TXTabSheet, 'Tag', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Boolean), TXTabSheet, 'TabVisible', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Integer), TXTabSheet, 'Width', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Boolean), TXTabSheet, 'AlignChildrenVertical', THiddenPropertyEditor);

RegisterPropertyEditor(TypeInfo(Boolean), TXTabSheet, 'IsContainer', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Boolean), TXTabSheet, 'IsSelected', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TColor), TXTabSheet, 'SelectionBorderColor', THiddenPropertyEditor);

//.....TXTabSheet events.......
RegisterPropertyEditor(TypeInfo(TContextpopupEvent), TXTabSheet, 'OnContextpopup', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TDragDropEvent), TXTabSheet, 'OnDragDrop', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TDragOverEvent), TXTabSheet, 'OnDragOver', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TEndDragEvent), TXTabSheet, 'OnEndDrag', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabSheet, 'OnEnter', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabSheet, 'OnExit', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabSheet, 'OnHide', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TMouseEvent), TXTabSheet, 'OnMouseDown', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabSheet, 'OnMouseEnter', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabSheet, 'OnMouseLeave', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TMouseMoveEvent), TXTabSheet, 'OnMouseMove', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TMouseEvent), TXTabSheet, 'OnMouseUp', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TMouseWheelEvent), TXTabSheet, 'OnMouseWheel', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TMouseWheelUpDownEvent), TXTabSheet, 'OnMouseWheelDown', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TMouseWheelUpDownEvent), TXTabSheet, 'OnMouseWheelUp', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabSheet, 'OnResize', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabSheet, 'OnShow', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TStartDragEvent), TXTabSheet, 'OnStartDrag', THiddenPropertyEditor);

end;

constructor TXTabControl.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,false);
end;

constructor TXTabControl.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXTabControl.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  ControlStyle := ControlStyle - [csSetCaption];

  AutoSize:=false;
  ParentColor:=false;

  BorderStyle := bsNone;
  BorderWidth:=0;


  IsSelected:=false;
  IsContainer:=true;
  SelectionBorderColor:=glbSelectionBorderColor;

  Constraints.MinHeight:=20;
  Constraints.MinWidth:=20;

  MyEventTypes:=TStringList.Create;

  self.Tag:=-1;
  self.OnClick:=@self.TabControlClick;
  self.OnChange:=@self.TabPageChanged;
  self.SetMyEventTypes;

  CreateComponentDataNode2(self,MyNodeType,ControlDefaultAttribs, self.myEventTypes, TheOwner,IsDynamic);

  // Lazarus Designer extension...
  if csDesigning in componentState then
   begin
      myExtension:=TXDesignerExtension.Create;
      myExtension.myWrapper:=self;
   end;

end;

constructor TXTabSheet.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,false);
end;

constructor TXTabSheet.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXTabSheet.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin

  ControlStyle := ControlStyle - [csSetCaption];

  AutoSize:=false;
  Align:=alClient;
  ParentColor:=false;
  SelectionBorderColor:=glbSelectionBorderColor;

  BorderStyle := bsNone;
  BorderWidth:=0;

  IsSelected:=false;
  IsContainer:=true;
  AlignChildrenVertical:=true;

  MyEventTypes:=TStringList.Create;
  self.OnClick:=@self.TabSheetClick;

  self.myNode:=TDataNode.Create('UI','','','TXTabSheet',false);     //self.name....always blank here?
  self.myNode.ScreenObject:=self;
  if (TheOwner is TXTabControl)
  and (TXTabControl(TheOwner).myNode<>nil) then
     self.myNode.MyForm:=TXTabControl(TheOwner).myNode.MyForm
  else if (TheOwner is TForm)  then          // should never happen from IDE (csAcceptsControls in TWinControl(NewParent).ControlStyle)
    self.myNode.MyForm:=TForm(TheOwner);

  self.SetMyEventTypes;
  self.myNode.myEventTypes:=self.myEventTypes;
  SetLength(self.myNode.myEventHandlers,self.myNode.myEventTypes.Count);

  AddDefaultAttribs(self,self.myNode,PageDefaultAttribs);

end;


function CreateTCWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXTabControl',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;
function CreateTSWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewWidget:TXTabSheet;
  NewNode:TDataNode;
  tc:TXTabControl;
begin
  tc:=TXTabControl(ParentNode.ScreenObject);
  NewWidget:=tc.AddTabSheet;
  if position>-1 then
    NewWidget.PageIndex:=position;
  tc.PageIndex:=NewWidget.PageIndex;
  NewWidget.Name:=Namespace+ScreenObjectName;
  NewNode:=NewWidget.myNode;
  NewNode.NodeName:=ScreenObjectName;
  NewNode.NameSpace:=NameSpace;
  AddChildToParentNode(ParentNode,NewNode,position);
  result:=NewNode;
end;

procedure TXTabControl.SetParent(NewParent: TWinControl);
begin
  inherited;
  CheckParentIsXContainer(self);
  ResetAlignment(TControl(self));
end;

procedure TXTabControl.TabControlClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;
procedure TXTabSheet.TabSheetClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;

procedure TXTabControl.TabPageChanged(Sender: TObject) ;
var
   MyTabControl : TPageControl;
   TabID:string;
begin
  Tabindex := inherited TabIndex;    // keep TabIndex in step

  if not (csDesigning in componentState) then
  begin
     if NOT (Sender is  TPageControl) then
     begin
       ShowMessage('In TabPageChanged expected TPageControl found '+Sender.ClassName) ;
       exit;
     end;
     MyTabControl:=TPageControl(Sender);
     TabID:=MyTabControl.ActivePage.Name;

     if (self.Visible) then
       // nudge the parent to resize contents
       ResizeMe(self.myNode);


     CallHandleEvent('Change',TabID,Sender);

     // do a Component Click event as well        //!!!! why?
     CallHandleEvent('Click',TabID,Sender);       //!!!!namespace???
  end;

end;

function TXTabControl.GetHeightOfTabs:integer;
var
 p1:TXTabSheet;
  ch,ph:integer;
begin
  p1:=self.Pages[0];
  ch:=self.Height;
  result:= ch - p1.height - p1.borderwidth;
end;

function TXTabControl.GetHTMLClasses:string;
begin
  result:=myNode.GetAttribute('HTMLClasses',true).AttribValue;
end;
procedure TXTabControl.SetHTMLClasses(AValue:string);
begin
  myNode.SetAttributeValue('HTMLClasses',AValue);
end;

function TXTabControl.GetIsVisible:Boolean;
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
procedure TXTabControl.SetIsVisible(AValue:Boolean);
begin
  if myNode<>nil then
    myNode.SetAttributeValue('IsVisible',myBoolToStr(AValue),'Boolean');
  self.Visible:=AValue;
end;

function TXTabControl.GetActiveTabSheet: TXTabSheet;
begin
 // debugln('XTabControl.GetActiveTabSheet');
  Result:=TXTabSheet(inherited ActivePageComponent);
end;
function TXTabControl.GetTabSheet(Index: Integer): TXTabSheet;
begin
  //debugln('XTabControl.GetTabSheet');
  Result:=TXTabSheet(inherited Pages[Index]);
end;
procedure TXTabControl.SetActiveTabSheet(const AValue: TXTabSheet);
begin
  //debugln(['XTabControl.SetActiveTabSheet ',DbgSName(Self),' ',DbgSName(AValue)]);
  ActivePageComponent := AValue;
end;
function TXTabControl.GetPageClass: TCustomPageClass;
begin
 // debugln('XTabcontrol.GetPageClass');
  Result := TXTabSheet;
end;
function TXTabControl.FindNextPage(CurPage: TXTabSheet; GoForward,
  CheckTabVisible: Boolean): TXTabSheet ;
var
  I, StartIndex: Integer;
begin
 // debugln('TXTabControl.FindNextPage');
  Result := nil;
  if PageCount = 0 then
    exit;
  StartIndex := IndexOf(CurPage);
  if StartIndex < 0 then
    if GoForward then
      StartIndex := PageCount - 1
    else
      StartIndex := 0;
  i := StartIndex;
  repeat
    if GoForward then
    begin
      Inc(i);
      if i = PageCount then
        i := 0;
    end else
    begin
      if i = 0 then
        i := PageCount;
      Dec(I);
    end;
    if not CheckTabVisible or Pages[i].TabVisible then
    begin
      Result := Pages[i];
      exit;
    end;
  until i = StartIndex;
end;
procedure TXTabControl.SelectNextPage(GoForward: Boolean);
begin
  //debugln('TXTabControl.SelectNextPage1');
  SelectNextPage(GoForward,true);
end;
procedure TXTabControl.SelectNextPage(GoForward: Boolean; CheckTabVisible: Boolean);
var
  NextPage: TXTabSheet;
begin
   //debugln('TXTabControl.SelectNextPage2');
 NextPage:=FindNextPage(ActivePage,GoForward,CheckTabVisible);
  if NextPage<>nil then ActivePage:=NextPage;
end;

function TXTabControl.GetName:string;
var
  myname:string;
begin
  result:=inherited Name;
end;
function TXTabSheet.GetName:string;
var
  myname:string;
begin
  result:=inherited Name;
end;
function TXTabControl.GetHint:string;
begin
  result:=myNode.GetAttribute('Hint',true).AttribValue;
end;
function TXTabSheet.GetHint:string;
begin
  result:=myNode.GetAttribute('Hint',true).AttribValue;
end;
function TXTabControl.GetContainerWidth:string;
begin
  result:=myNode.GetAttribute('ContainerWidth',true).AttribValue;
end;
function TXTabControl.GetContainerHeight:string;
begin
  result:=myNode.GetAttribute('ContainerHeight',true).AttribValue;
end;
function TXTabControl.GetAlignment:String;
begin
  result:=MyNode.getAttribute('Alignment',true).AttribValue;
end;

procedure TXTabControl.SetMyName(AValue:string);
begin
  inherited Name:=AValue;

  if  (csLoading in componentState) then
    if myNode<>nil then
      myNode.NodeName:=AValue;
end;
procedure TXTabSheet.SetMyName(AValue:string);
begin
  inherited Name:=AValue;

  if  (csLoading in componentState) then
    if myNode<>nil then
       myNode.NodeName:=AValue;
  // what else might need renaming ???
end;

procedure TXTabControl.SetIsSelected(AValue: Boolean);
begin
  if AValue<>FIsSelected then
    begin
      FIsSelected:=AValue;
      ShowHideSelectedBorder(self.myNode,FIsSelected);
      Repaint;
    end;
end;

procedure TXTabSheet.SetIsSelected(AValue: Boolean);
begin
  if AValue<>FIsSelected then
    begin
      FIsSelected:=AValue;

      if (AValue=true) and (self.Parent<>nil) then
        if TXTabControl(self.Parent).ActivePage<>self then
           TXTabControl(self.Parent).ActivePage:=self;
      ShowHideSelectedBorder(self.myNode,FIsSelected);
      Repaint;
    end;
end;
function TXTabSheet.GetIsVisible:Boolean;
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
procedure TXTabSheet.SetIsVisible(AValue:Boolean);
begin
  if myNode<>nil then
    myNode.SetAttributeValue('IsVisible',myBoolToStr(AValue),'Boolean');
  self.TabVisible:=AValue;
end;


procedure TXTabControl.SetContainerWidth(AValue:string);
begin
  myNode.SetAttributeValue('ContainerWidth',AValue);
  SetHeightWidth(self.myNode,TControl(self),'ContainerWidth','ContainerHeight');
end;

procedure TXTabControl.SetContainerHeight(AValue:string);
begin
  myNode.SetAttributeValue('ContainerHeight',AValue);
  SetHeightWidth(self.myNode,TControl(self),'ContainerWidth','ContainerHeight');
end;

procedure TXTabControl.SetHint(AValue:string);
begin
  myNode.SetAttributeValue('Hint',AValue);
  if AValue<>'' then
    self.ShowHint:=true
  else
    self.ShowHint:=false;
  inherited Hint:=AValue;
end;
procedure TXTabSheet.SetHint(AValue:string);
begin
  myNode.SetAttributeValue('Hint',AValue);
  if AValue<>'' then
    self.ShowHint:=true
  else
    self.ShowHint:=false;
  inherited Hint:=AValue;
end;

procedure TXTabControl.SetAlignment(AValue:string);
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

// Name is the first property loaded from .lfm
// Hijacking this so that we can decide which set of default values to apply for other properties
// (because there is a problem produced for string properties which are NOT saved to lfm
// when the value is blank).
procedure TXTabControl.SetName(const NewName: TComponentName);
var
  ApplyName:TComponentName;
begin
  ApplyName:=GetNameToApply(self.myNode,self.myNode.MyForm.Name,self.Name,NewName,componentState);

  inherited SetName(ApplyName);

  if (csLoading in componentState) then
  begin
//    showmessage('TXTabControl setname. loading.');
    ContainerWidth:='';
    ContainerHeight:='';
    Hint:='';
  end;

end;
procedure TXTabSheet.SetName(const NewName: TComponentName);
var
  ApplyName:TComponentName;
begin
  ApplyName:=GetNameToApply(self.myNode,self.myNode.MyForm.Name,self.Name,NewName,componentState);

  inherited SetName(ApplyName);

  if (csLoading in componentState) then
  begin
//    showmessage('TXTabSheet setname. loading.');
    //Caption:='';
    Hint:='';
  end;

end;


function TXTabSheet.DoAlignChildControls(TheAlign: TAlign; AControl: TControl;
                     AControlList: TFPList; var ARect: TRect): Boolean;
var
  i,j:integer;
  NewList:TFPList;
  tmp:TControl;
begin
  AControlList.Sort(@SortAlignList);
  inherited DoAlignChildControls(TheAlign,AControl,AControlList,ARect);
end;

procedure TXTabControl.SetSelectionBorderColor(AValue: TColor);
// Colour of the 'IsSelected' dotted border
begin
  if AValue<>FSelectionBorderColor then
    begin
      FSelectionBorderColor:=AValue;
      Repaint;
    end;
end;
procedure TXTabSheet.SetSelectionBorderColor(AValue: TColor);
// Colour of the 'IsSelected' dotted border
begin
  if AValue<>FSelectionBorderColor then
    begin
      FSelectionBorderColor:=AValue;
      Repaint;
    end;
end;

function TXTabControl.GetBgColor:TColor;
begin
  result:=HexRGBToColor(myNode.GetAttribute('BgColor',true).AttribValue);
end;
function TXTabSheet.GetBgColor:TColor;
begin
  result:=HexRGBToColor(myNode.GetAttribute('BgColor',true).AttribValue);
end;
procedure TXTabControl.SetBgColor(AValue:TColor);
var str:string;
begin
  if myNode<>nil then
  begin
    str:= ColorToHexRGB(AValue);
    myNode.SetAttributeValue('BgColor',str,'Color');
  end;

  Color:=AValue;
end;
procedure TXTabSheet.SetBgColor(AValue:TColor);
var str:string;
begin
  if myNode<>nil then
  begin
    str:= ColorToHexRGB(AValue);
    myNode.SetAttributeValue('BgColor',str,'Color');
  end;

  Color:=AValue;
end;

procedure TXTabControl.SortOutAlignment;
var
    MyAlignment,MyLabelPos,NewAlignment:String;
begin
  if (self.Parent<>nil) and (self.Name<>'') then
  begin
     MyAlignment:=self.Alignment;
    MyLabelPos:='';

    NewAlignment:=SortOutAlignmentAndLabelPos(self,nil,nil,MyAlignment,MyLabelPos);

    if NewAlignment<>MyAlignment then
    begin
      self.Alignment:=NewAlignment;
     end;
  end;
end;

procedure TXTabControl.WMPaint(var Message: TWMPaint);
var
  MCanvas:TControlCanvas;
  DrawBounds: TRect;
begin
  inherited;
  MCanvas := TControlCanvas.Create;
  DrawBounds := self.GetClientRect;
  InflateRect(DrawBounds, 1, 1);
  try
    MCanvas.Control := Self;
    PaintSelectedRectangle(MCanvas,DrawBounds,glbSelectionBorderColor,self.IsSelected);
  finally
    MCanvas.Free;
  end;
end;

procedure TXTabSheet.WMPaint(var Message: TWMPaint);
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

function TXTabSheet.GetHTMLClasses:string;
begin
  result:=myNode.GetAttribute('HTMLClasses',true).AttribValue;
end;
procedure TXTabSheet.SetHTMLClasses(AValue:string);
begin
  myNode.SetAttributeValue('HTMLClasses',AValue);
end;


{$else}
constructor TXTabControl.Create(MyForm:TForm;NodeName,NameSpace:String);
begin
  inherited Create(NodeName,NameSpace);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  AlignChildrenVertical:=true;

  self.SetMyEventTypes;
  self.IsContainer:=true;

  SetNodePropDefaults(self,ControlDefaultAttribs);

end;
constructor TXTabSheet.Create(MyForm:TForm;NodeName,NameSpace:String);
begin
  inherited Create(NodeName,NameSpace);
  self.NodeType:='TXTabSheet';
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=true;

  SetNodePropDefaults(self,PageDefaultAttribs);
end;

function CreateTabControl(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  OnChangeString, OnClickString, OnPasteString, BgColor:String;
begin

  BgColor:=MyNode.GetAttribute('BgColor',true).AttribValue;
  if BgColor='' then BgColor:='#FFFFFF';

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''','''+NameSpace+''', this.value);" ';
  //showmessage('tabcontrol createwidget '+NameSpace+'.'+ScreenObjectName);

  asm
    try{
    // ----------------------------------------check if the style has already been set
    var x = document.getElementsByTagName("STYLE");
    var StyleIsSet = false;
    if (x.length>0){
      for (var i=0; i<x.length; i++){
        var y= x[i].innerHTML;
        if (y.indexOf("div.TabPage") !=-1) { StyleIsSet =true}
      }
    }

    // ----------------------------------------add style if not already been set
    if (StyleIsSet == false)
    {
      // ----------------------------Define the styling to be used for  "TabPage"
         var Styletext='<style type="text/css">';
//         Styletext=Styletext+'div.TabPage { background-color:'+BgColor+'; height:98%; width:100%}';
         Styletext=Styletext+'div.TabPage { height:98%; width:100%}';
         Styletext=Styletext+
         '.TabButton { '
             +' font-size: inherit;'
             +' color: inherit;'
             +' font-style: inherit;'
             +' font-family: inherit;'
             +' background: #d1d0ce;'     //'not selected' color
             +' }'
          +'.TabButton:focus { '
             +' border-bottom: none; '
             +' outline: none;'
             +' }'
         Styletext=Styletext+'</style>';

      //----------------------------- now append the style declarations to the head of the HTML page
         document.head.innerHTML = document.head.innerHTML+Styletext;
    }


    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,NameSpace,$impl.MyNodeType,position);

    //localcontainer is an inner div.  Its id is  ScreenObjectName+'Contents'
    // It is a child of the outer container div (wrapper)
    //
     var localcontainer = document.createElement("div");
     var wrapperid = NameSpace+ScreenObjectName;
     localcontainer.id = wrapperid+'Contents';
     localcontainer.className = wrapperid;
     localcontainer.style.display="inline-block;";
     localcontainer.style.height="100%";
     localcontainer.style.width="100%";
     document.getElementById(wrapperid).appendChild(localcontainer);

  // -----------------------------Define the HTML to be used to create the Tab control
  // NB --- "TabButton" and "TabPage" are the classnames used for styling the tab controls
  // -------"TabButtonDiv" is the classname used for styling the div containing the tab buttons

    var TabButtonsDef = '<div id="'+wrapperid+'ContentsButtons'+'" class="TabButtonDiv '+NameSpace+ScreenObjectName+'"'+
                        '>'+
                        '</div>';

  //------------------------------------ now append the declarations to the Parent
     localcontainer.innerHTML = localcontainer.innerHTML + TabButtonsDef;

    var wrapper=document.getElementById(wrapperid);
  }
  catch(err) { alert(err.message+'  in XTabControl.CreateTabControl');}

  end;
  MyNode.ScreenObject:=MyNode;

  // now that we have a datanode and a widget, cycle attribute settings
  RefreshComponentProps(myNode);

  result:=myNode;
end;

procedure openTab(TabName,TabControlName,NameSpace:string;PageNode:TDataNode);
var
  myNode, ControlNode:TdataNode;
  siblingpos:Integer;
  tabId,tcId,bgColor:String;
begin
  MyNode:=PageNode;
  bgColor:=MyNode.GetAttribute('BgColor',true).AttribValue;
  tabId:=NameSpace+TabName;
  tcId:=NameSpace+TabControlName;
  asm
  try{
      var butsdiv=document.getElementById(tcId+'ContentsButtons');
      var tabsdiv=document.getElementById(tcId+'Contents');

      var i;
      //alert('OpenTab  TabControl='+NameSpace+TabControlName+' tabId='+tabId);

      var x = tabsdiv.getElementsByClassName('TabPage');
      if (x==null) {alert('cannot find element by class name TabPage');}
      else {
      for (i = 0; i < x.length; i++) {
         //alert('hiding '+x[i].id);
         x[i].style.display = "none";
      } }

      var y = butsdiv.getElementsByClassName('TabButton');

      if (y==null) {alert('cannot find element by class name TabButton');}
      else {
      for (i = 0; i <y.length; i++) {
         y[i].style.background ='#d1d0ce';// dark background when not selected
         y[i].style.border = '1px solid black';
      } }

      var selectedTab = document.getElementById(tabId);
      selectedTab.style.display = "block";
      var selectedTab = document.getElementById(tabId+'Contents');
      selectedTab.style.display = "flex";

      var selectedTabButton = document.getElementById(tabId+'Button');
      if (selectedTabButton==null) {alert('cannot find element by name '+TabName+'Button');}
      else {
        selectedTabButton.style.background = bgColor; // Same background color as the tab page when selected
        selectedTabButton.style.borderBottom = 'none';
      }

      } catch(err) {alert('Error in XTabControl.OpenTab '+ err.message);}
  end;

  if MyNode=nil then
    myNode:=findDataNodeById(SystemNodetree,TabName,NameSpace,true);
  if myNode<>nil then
  begin
    ControlNode:=FindParentOfNodeByName(SystemNodeTree,TabName,NameSpace,true,siblingpos);
    if (siblingpos>-1)
    and (TXTabControl(ControlNode).TabIndex<>siblingpos) then
      TXTabControl(ControlNode).TabIndex:=siblingpos;
    //showmessage('Tabindex='+intToStr(TXTabControl(ControlNode).TabIndex));
  end;
  ResetAllRenderedCombos(MyNode);
end;

procedure ChangeTabPage(nodeId,parentNodeId,NameSpace:string;TabNode:TDataNode=nil);
var
  PageNode:TDataNode;
begin
 //   showmessage('calling openTab('+NodeId+','+parentNodeId+')'+','+NameSpace);
  PageNode:=TabNode;
  if PageNode=nil then
    PageNode:=FindDataNodeById(SystemNodeTree,nodeId,NameSpace,true);
  openTab(NodeId,parentNodeId,NameSpace,PageNode);
end;

procedure RebuildButtons(TCNode:TdataNode);
var
  OnClickString,buttonstring:String;
  thisTab:TDataNode;
  cap:string;
  i:integer;
begin
  for i:=0 to length(TCNode.ChildNodes)-1 do
  begin
    thisTab:=TCNode.ChildNodes[i];
    cap:=thisTab.GetAttribute('Caption',false).AttribValue;
    OnClickString:='onclick="event.stopPropagation();'+
                         'pas.XTabControl.ChangeTabPage('''+thisTab.NodeName+''','''+TCNode.NodeName+''','''+thisTab.NameSpace+''',null); '+
                         'pas.Events.handleEvent(null,''Change'','''+TCNode.NodeName+''','''+thisTab.NameSpace+''','''+thisTab.NodeName+''');' +
                         'pas.Events.handleEvent(null,''Click'','''+thisTab.NodeName+''','''+thisTab.NameSpace+''', ''''); '+
                         '" ';
    buttonstring := buttonstring +
                  '<button id="'+thisTab.NameSpace+thisTab.NodeName+'Button" class="TabButton '+TCNode.NodeName+'" ' +
                           OnClickString +
                           ' style="background:rgb(241, 240, 238);border:none; padding:5px" ' +
                        '>'+cap+'</button>';
  end;
  asm
  try{
    var ButtonsDiv = document.getElementById(TCNode.NodeName+'ContentsButtons');
    if (ButtonsDiv==null) {alert('Cannot find ButtonsDiv '+TCNode.NodeName+'Buttons');}
    else
      ButtonsDiv.innerHTML = buttonstring;
    }catch(err) { alert(err.message+' in XTabControl.RebuildButtons '+TCNode.NodeName);}
  end;
end;

function CreateTabSheet(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String): TDataNode;
var
  ParentName,PageCaption,NodeID,ControlName:string;
begin
  //showmessage('tabsheet createwidget');
  ControlName:=ParentNode.NodeName;
  ParentName:=NameSpace+MyNode.GetAttribute('ParentName',false).AttribValue+'Contents';
  PageCaption:=MyNode.GetAttribute('Caption',false).AttribValue;
  NodeID:=MyNode.NodeName;

  asm
    try{
    //alert('pagecaption='+PageCaption+' parent='+ParentName);

    var wrapperid = NameSpace+ScreenObjectName;
    var ButtonsDiv = document.getElementById(ParentName+'Buttons');
    //alert('built ButtonsDiv '+ ParentName+'Buttons');

    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,NameSpace,'TXTabSheet',position);
    wrapper.style.height = '100%';
    wrapper.style.width = '100%';
    wrapper.className='TabPage';

    var TabContentDef ="<div id='" +wrapperid+"Contents'  class='vboxNoStretch "+NameSpace+ScreenObjectName+"' style='height:98%; width:100%' ></div>";

    wrapper.innerHTML = wrapper.innerHTML + TabContentDef;

    var wrapper=document.getElementById(wrapperid);
  }
  catch(err) { alert(err.message+'  in XTabControl.CreateTabSheet');}
  end;

  MyNode.ScreenObject:=MyNode;
  RefreshComponentProps(myNode);

  RebuildButtons(ParentNode);

  TXTabControl(ParentNode).HTMLClasses := TXTabControl(ParentNode).HTMLClasses;

  result:=myNode;
end;

function CreateTabControlInterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXTabControl.Create(MyForm,NodeName,NameSpace));
end;
function CreateTabPageInterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXTabSheet.Create(MyForm,NodeName,NameSpace));
end;
{$endif}

function TXTabControl.GetmyTabIndex: longint;
begin
  {$ifndef JScript}
  Result:=inherited TabIndex;
  {$else}
  result:=StrToInt(myNode.GetAttribute('TabIndex',true).AttribValue);
  {$endif}
end;
procedure TXTabControl.SetmyTabIndex(const AValue: longint);
var
  myTabSheetNode:String;
  idx:longint;
begin
  if myNode<>nil then
  begin
    idx := AValue;
    if length(myNode.ChildNodes)>0 then
    begin
      if (idx>=length(myNode.ChildNodes))
      or (idx<0) then
        idx:=0;
    end;
    myNode.SetAttributeValue('TabIndex',inttostr(idx));
    {$ifndef JScript}
    inherited TabIndex := idx;
    {$else}
    if (idx>-1)
    and (length(myNode.ChildNodes)>idx) then
    begin
      //showmessage('calling openTab('+self.NodeName+','+intToStr(idx)+')');
      // find the name of the required tab sheet...
      myTabSheetNode:=myNode.ChildNodes[idx].NodeName;
      openTab(myTabSheetNode,self.NodeName,self.NameSpace,myNode.ChildNodes[idx]);
    end;
    {$endif}

  end;
end;

function TXTabSheet.GetCaption:string;
begin
  result:=myNode.GetAttribute('Caption',true).AttribValue;
end;
procedure TXTabSheet.SetCaption(AValue:string);
begin
  myNode.SetAttributeValue('Caption',AValue);
  {$ifndef JScript}
  inherited Caption:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Button');
    if (ob!=null) {
      ob.innerHTML=AValue;
    }
  end;
  {$endif}
end;

function TXTabControl.IndexOfPage(PageId:String):integer;
var
  myTabSheetNode:String;
  i,idx:integer;
begin
  idx:=-1;
  for i:=0 to length(myNode.ChildNodes)-1 do
  begin
    if (myNode.ChildNodes[i].NodeName=PageId) then
      idx:=i;
  end;
  result:=idx;
end;


{$ifndef JScript}


function TXTabControl.AddTabSheet: TXTabSheet;
begin
  //debugln('addtabsheet');
  Result := TXTabSheet.Create(Self.Owner,self.myNode.IsDynamic);
  Result.PageControl := Self;
  Result.Parent:=Self;
end;


{ TXTabControlComponentEditor }

const
  nbvAddPage       = 0;
  nbvInsertPage    = 1;
  nbvDeletePage    = 2;
  nbvMovePageLeft  = 3;
  nbvMovePageRight = 4;
  nbvShowPage      = 5;

procedure TXTabControlComponentEditor.ShowPageMenuItemClick(Sender: TObject);
var
  AMenuItem: TMenuItem;
  NewPageIndex: integer;
begin
  AMenuItem:=TMenuItem(Sender);
  if (AMenuItem=nil) or (not (AMenuItem is TMenuItem)) then exit;
  NewPageIndex:=AMenuItem.MenuIndex;
  if (NewPageIndex<0) or (NewPageIndex>=TabControl.PageCount) then exit;
  TabControl.PageIndex:=NewPageIndex;
  GetDesigner.SelectOnlyThisComponent(TabControl.CustomPage(TabControl.PageIndex));
end;

procedure TXTabControlComponentEditor.AddNewPageToDesigner(Index: integer);
var
  Hook: TPropertyEditorHook;
  NewPage: TCustomPage;
  NewName: string;
  i:integer;
begin
  //debugln('AddNewPageToDesigner. index='+inttostr(index));
  Hook:=nil;
  if not GetHook(Hook) then exit;
  NewPage:=TabControl.CustomPage(Index);
  NewName:=GetDesigner.CreateUniqueComponentName(NewPage.ClassName);
  NewPage.Caption:=NewName;
  NewPage.Name:=NewName;
  TabControl.PageIndex:=Index;

  Hook.PersistentAdded(TXTabSheet(NewPage),true);
  Modified;
  //debugln('AddNewPageToDesigner done');
end;



procedure TXTabControlComponentEditor.DoAddPage;       //!!!! Namespace???
var
  Hook: TPropertyEditorHook;
  newPage:TXTabSheet;
  lPageName:string;
begin
  //debugln('doaddpage');
  Hook:=nil;
  if not GetHook(Hook) then exit;

  lPageName := Designer.CreateUniqueComponentName(TXTabSheet.ClassName);
  NewPage := TabControl.AddTabSheet;
  NewPage.Name:=lPageName;
  NewPage.Caption:=lPageName;
  TabControl.PageIndex:=TabControl.PageCount-1;

  Hook.PersistentAdded(NewPage, True);
  Modified;
  if Designer <> nil then Designer.Modified;
  TabControl.Invalidate;


end;

procedure TXTabControlComponentEditor.DoInsertPage;
var
  NewIndex: integer;
  Hook: TPropertyEditorHook;
  newPage:TXTabSheet;
  lPageName:string;
begin
  //debugln('doinsertpage');
  Hook:=nil;
  if not GetHook(Hook) then exit;

  NewIndex:=TabControl.PageIndex;
  if NewIndex<0 then NewIndex:=0;

  lPageName := Designer.CreateUniqueComponentName(TXTabSheet.ClassName);
  NewPage := TabControl.AddTabSheet;
  NewPage.Name:=lPageName;
  NewPage.Caption:=lPageName;
  TabControl.PageIndex:=NewIndex;

  Hook.PersistentAdded(NewPage, True);
  Modified;
  if Designer <> nil then Designer.Modified;
  TabControl.Invalidate;

end;

procedure TXTabControlComponentEditor.DoDeletePage;
var
  Hook: TPropertyEditorHook;
  OldIndex: integer;
  PageComponent: TPersistent;
begin
  OldIndex:=TabControl.PageIndex;
  if (OldIndex>=0) and (OldIndex<TabControl.PageCount) then begin
    if not GetHook(Hook) then exit;
    PageComponent := TPersistent(TabControl.Pages[OldIndex]);
    Hook.DeletePersistent(PageComponent);
  end;
end;

procedure TXTabControlComponentEditor.DoMoveActivePageLeft;
var
  Index: integer;
begin
  Index:=TabControl.PageIndex;
  if (Index<0) then exit;
  DoMovePage(Index,Index-1);
end;

procedure TXTabControlComponentEditor.DoMoveActivePageRight;
var
  Index: integer;
begin
  Index:=TabControl.PageIndex;
  if (Index>=0)
  and (Index>=TabControl.PageCount-1) then exit;
  DoMovePage(Index,Index+1);
end;

procedure TXTabControlComponentEditor.DoMovePage(
  CurIndex, NewIndex: Integer);
begin
  TabControl.Pages[CurIndex].PageIndex:=NewIndex;
  //.Pages.Move(CurIndex,NewIndex);
  Modified;
end;

procedure TXTabControlComponentEditor.AddMenuItemsForPages(
  ParentMenuItem: TMenuItem);
var
  i: integer;
  NewMenuItem: TMenuItem;
begin
  ParentMenuItem.Enabled:=TabControl.PageCount>0;
  for i:=0 to TabControl.PageCount-1 do begin
    NewMenuItem:=TMenuItem.Create(ParentMenuItem);
    NewMenuItem.Name:='ShowPage'+IntToStr(i);
    NewMenuItem.Caption:=TabControl.CustomPage(i).Name+' "'+TabControl.Pages[i].Caption+'"';
    NewMenuItem.OnClick:=@ShowPageMenuItemClick;
    ParentMenuItem.Add(NewMenuItem);
  end;
end;

procedure TXTabControlComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    nbvAddPage:       DoAddPage;
    nbvInsertPage:    DoInsertPage;
    nbvDeletePage:    DoDeletePage; // beware: this can free the editor itself
    nbvMovePageLeft:  DoMoveActivePageLeft;
    nbvMovePageRight: DoMoveActivePageRight;
  end;
end;

function TXTabControlComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    nbvAddPage:       Result:=nbcesAddPage;
    nbvInsertPage:    Result:=nbcesInsertPage;
    nbvDeletePage:    Result:=nbcesDeletePage;
    nbvMovePageLeft:  Result:=nbcesMovePageLeft;
    nbvMovePageRight: Result:=nbcesMovePageRight;
    nbvShowPage:      Result:=nbcesShowPage;
  else
    Result:='';
  end;
end;

function TXTabControlComponentEditor.GetVerbCount: Integer;
begin
  Result:=6;
end;

procedure TXTabControlComponentEditor.PrepareItem(Index: Integer;
  const AnItem: TMenuItem);
begin
  inherited PrepareItem(Index, AnItem);
  case Index of
    nbvAddPage:       ;
    nbvInsertPage:    AnItem.Enabled:=TabControl.PageIndex>=0;
    nbvDeletePage:    AnItem.Enabled:=TabControl.PageIndex>=0;
    nbvMovePageLeft:  AnItem.Enabled:=TabControl.PageIndex>0;
    nbvMovePageRight: AnItem.Enabled:=TabControl.PageIndex<TabControl.PageCount-1;
    nbvShowPage:      AddMenuItemsForPages(AnItem);
  end;
end;

function TXTabControlComponentEditor.TabControl: TXTabControl;
begin
  Result:=TXTabControl(GetComponent);
end;
{$endif}

begin
  // this is the set of node attributes that each TXTabControl instance will have.
  AddWrapperDefaultAttribs(ControlDefaultAttribs);
  AddDefaultAttribute(ControlDefaultAttribs,'ContainerWidth','String','300px','',false);
  AddDefaultAttribute(ControlDefaultAttribs,'ContainerHeight','String','300px','',false);
  AddDefaultAttribute(ControlDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(ControlDefaultAttribs,'BgColor','Color','#FFFFFF','',false);
  AddDefaultAttribute(ControlDefaultAttribs,'TabIndex','Integer','-1','',false);
  AddDefaultsToTable('TXTabControl',ControlDefaultAttribs);

  // this is the set of node attributes that each TXTabSheet instance will have.
  AddDefaultAttribute(PageDefaultAttribs,'Hint','String','','',false);
  AddDefaultAttribute(PageDefaultAttribs,'IsVisible','Boolean','True','',false);
  AddDefaultAttribute(PageDefaultAttribs,'HTMLClasses','String','','',false);
  AddDefaultAttribute(PageDefaultAttribs,'BgColor','Color','#FFFFFF','',false);
  AddDefaultAttribute(PageDefaultAttribs,'Caption','String','NewPage','',false);
  AddDefaultsToTable('TXTabSheet',PageDefaultAttribs);


  AddAttribOptions('TXTabControl','Alignment',AlignmentOptions);
  {$ifndef JScript}
  RegisterClass(TXTabControl);
  RegisterClass(TXTabSheet);
  RegisterComponentEditor(TXTabControl, TXTabControlComponentEditor);
  AddNodeFuncLookup('TXTabControl',@CreateTCWidget);
  AddNodeFuncLookup('TXTabSheet',@CreateTSWidget);
  {$else}
  AddNodeFuncLookup('TXTabControl',@CreateTabControlInterfaceObj,@CreateTabControl);
  AddNodeFuncLookup('TXTabSheet',@CreateTabPageInterfaceObj,@CreateTabSheet);
  SuppressDesignerProperty('TXTabControl','LabelPos');
  SuppressDesignerProperty('TXTabControl','LabelText');
  SuppressDesignerProperty('TXTabSheet','LabelPos');
  SuppressDesignerProperty('TXTabSheet','LabelText');
  {$endif}
end.
