(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XMenu;

{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}
interface

uses
  Classes, SysUtils, NodeUtils, StringUtils,
  {$ifndef JScript}
  LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls,Propedits, ImgList, RTTICtrls,
  LazsUtils, Events, ComponentEditors, Menus,
  {$else}
  HTMLUtils,
  {$endif}
  WrapperPanel;

{$ifdef JScript}
procedure addMenuBarStyles;
procedure menuBoxInit(menu:TObject);
procedure resetButton(button:TObject);
procedure closeSubMenu(menuName:String);
procedure CloseAnyDropdown(menuName:String);
procedure menuButtonClick(buttonName:String);
//procedure menuBarClick(menuName:String);
procedure buttonMouseover(buttonName:String);
procedure menuItemMouseover(itemName:String);

{$endif}

{$ifndef JScript}

const
  mesAddItem = 'Add Item';

type
  // Container component - cannot descend from WrapperPanel, must be  a stand-alone component to work with Lazarus IDE
 TXMenuItem = class(TMenuItem)
  private
  FIsSelected:Boolean;
  FIsContainer:Boolean;
  fHandleClick:TEventHandler;
  FSelectionBorderColor: TColor;
  FmyNode:TDataNode;
  procedure MenuItemclick(Sender:TObject);

  procedure SetMyEventTypes;

  function GetName:string;
  function GetHint:string;
  function GetCaption:string;
  function GetIsVisible:Boolean;
  function GetIsEnabled:Boolean;

  procedure SetParentComponent(Value:TComponent);  override;

  procedure SetMyName(AValue:string);
  procedure SetIsSelected(AValue: Boolean);
  procedure SetSelectionBorderColor(AValue: TColor);
  procedure SetHint(AValue:string);
  procedure SetCaption(AValue:string);
  procedure SetIsVisible(AValue:Boolean);
  procedure SetIsEnabled(AValue:Boolean);

  function AddMenuItem(NewName:String):TXMenuItem;
  procedure SetName(const NewName: TComponentName); override;
  procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
  public
    myEventTypes:TStringList;
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent;IsDynamic:Boolean); virtual;

  published

    property myNode:TDataNode read FmyNode write FmyNode;
    property IsContainer:Boolean read FIsContainer write FIsContainer;
    property IsSelected: Boolean read FIsSelected write SetIsSelected default false;
    property SelectionBorderColor: TColor read FSelectionBorderColor write SetSelectionBorderColor default clGreen;
    property IsVisible:Boolean read GetIsVisible write SetIsVisible;
    property IsEnabled:Boolean read GetIsEnabled write SetIsEnabled;

    property Hint: String read GetHint write SetHint;
    property Name: String read GetName write SetMyName;
    property Caption: String read GetCaption write SetCaption;

    // Events to be visible in Lazarus IDE
    property HandleClick: TEventHandler read FHandleClick write FHandleClick;
end;

 // Container component - cannot descend from WrapperPanel, must be  a stand-alone container to work with Lazarus IDE
  TXMainMenu = class(TMainMenu)
  private
    FSelectionBorderColor: TColor;
    FIsSelected:Boolean;
    FIsContainer:Boolean;
    FmyNode:TDataNode;
    myExtension:TXDesignerExtension;

    procedure SetMyEventTypes;

    function GetName:string;
    function GetIsVisible:Boolean;

    procedure SetMyName(AValue:string);
    procedure SetIsSelected(AValue: Boolean);
    procedure SetSelectionBorderColor(AValue: TColor);
    procedure SetIsVisible(AValue:Boolean);

    function AddMenuItem(NewName:String):TXMenuItem;
    procedure SetName(const NewName: TComponentName); override;
    procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
  public
     myEventTypes:TStringList;
     constructor Create(TheOwner: TComponent); override;
     constructor Create(TheOwner: TComponent;IsDynamic:Boolean); virtual;

 published

   property myNode:TDataNode read FmyNode write FmyNode;
   property IsContainer:Boolean read FIsContainer write FIsContainer;
   property IsSelected: Boolean read FIsSelected write SetIsSelected default false;
   property IsVisible:Boolean read GetIsVisible write SetIsVisible;
   property SelectionBorderColor: TColor read FSelectionBorderColor write SetSelectionBorderColor default clGreen;

   property Name: String read GetName write SetMyName;

 end;




  TXMenuComponentEditor = class(TDefaultComponentEditor)
  protected
    fMenuComponent:TMenu;     // may be a mainmenu or a menuitem
    procedure DoAddItem; virtual;

    constructor Create(AComponent: TComponent;
      ADesigner: TComponentEditorDesigner);  override;

  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AnItem: TMenuItem); override;
     function GetMenuComponent: TMenu;
    property MenuComponent: TMenu read FMenuComponent;
  end;

procedure register;

{$else}
type
  TXMainMenu = class(TWrapperPanel)
  private
    fActiveButton:TObject;
    procedure SetMyEventTypes;
  public
     constructor Create(MyForm:TForm;NodeName,NameSpace:String);

   property ActiveButton:TObject read fActiveButton write fActiveButton;
 published
 end;

type
  TXMenuItem = class(TWrapperPanel)
  private
    function GetCaption:string;
    function GetIsEnabled:Boolean;

     procedure SetCaption(AValue:string);
     procedure SetIsEnabled(AValue:Boolean);

    procedure SetMyEventTypes;

  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(MyForm:TForm;NodeName,NameSpace:String);

  published
    { Published declarations }
    property Caption: String read GetCaption write SetCaption;
    property IsEnabled:Boolean read GetIsEnabled write SetIsEnabled;

  end;

{$endif}

implementation

const MyNodeType='TXMainMenu';
var
  MenuDefaultAttribs:TDefaultAttributesArray;
  ItemDefaultAttribs:TDefaultAttributesArray;

procedure TXMainMenu.SetMyEventTypes;
begin
end;
procedure TXMenuItem.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
end;

{$ifndef JScript}

procedure Register;
begin
  {$I XMainMenu.lrs}
  // Lazarus IDE component registration
  RegisterComponents('XComponents',[TXMainMenu]);
  //RegisterComponents('XComponents',[TXMenuItem]);
  RegisterNoIcon([TXMenuItem]);

  // Hide some inherited properties
  RegisterPropertyEditor(TypeInfo(TBiDiMode), TXMainMenu, 'BiDiMode', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCustomImageList), TXMainMenu, 'Images', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXMainMenu, 'ParentBiDiMode', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXMainMenu, 'OwnerDraw', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXMainMenu, 'Tag', THiddenPropertyEditor);

  //.....TXMainMenu events.......
  RegisterPropertyEditor(TypeInfo(TMenuChangeEvent), TXMainMenu, 'OnChange', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMenuDrawItemEvent), TXMainMenu, 'OnDrawItem', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMenuMeasureItemEvent), TXMainMenu, 'OnMeasureItem', THiddenPropertyEditor);

  //.....TXMenuItem properties.......
  RegisterPropertyEditor(TypeInfo(TBasicAction), TXMenuItem, 'Action', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXMenuItem, 'AutoCheck', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TBitmap), TXMenuItem, 'Bitmap', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXMenuItem, 'Checked', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXMenuItem, 'Default', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXMenuItem, 'Enabled', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TGlyphShowMode), TXMenuItem, 'GlyphShowMode', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Byte), TXMenuItem, 'GroupIndex', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(THelpContext), TXMenuItem, 'HelpContext', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TXMenuItem, 'ImageIndex', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXMenuItem, 'RadioItem', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXMenuItem, 'RightJustify', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TShortCut), TXMenuItem, 'ShortCut', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TShortCut), TXMenuItem, 'ShortCutKey2', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXMenuItem, 'ShowAlwaysCheckable', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCustomImageList), TXMenuItem, 'SubMenuImages', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXMenuItem, 'SubMenuImagesWidth', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXMenuItem, 'Tag', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXMenuItem, 'Visible', THiddenPropertyEditor);

  RegisterPropertyEditor(TypeInfo(Boolean), TXMenuItem, 'IsContainer', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXMenuItem, 'IsSelected', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TColor), TXMenuItem, 'SelectionBorderColor', THiddenPropertyEditor);

  //.....TXMenuItem events.......
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXMenuItem, 'OnClick', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMenuDrawItemEvent), TXMenuItem, 'OnDrawItem', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMenuMeasureItemEvent), TXMainMenu, 'OnMeasureItem', THiddenPropertyEditor);

end;

constructor TXMainMenu.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,false);
  self.SetParentComponent(TheOwner);
end;

constructor TXMainMenu.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,IsDynamic);
  self.SetParentComponent(TheOwner);
end;

procedure TXMainMenu.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin

  IsSelected:=false;
  IsContainer:=true;
  SelectionBorderColor:=glbSelectionBorderColor;

  MyEventTypes:=TStringList.Create;

  self.SetMyEventTypes;

  //self.myNode:=CreateComponentDataNode(self.Name,MyNodeType, self.myEventTypes, self,TheOwner,IsDynamic);
  CreateComponentDataNode2(self,MyNodeType,MenuDefaultAttribs, self.myEventTypes, TheOwner,IsDynamic);

   //Lazarus Designer extension...
  if csDesigning in componentState then
   begin
      myExtension:=TXDesignerExtension.Create;
      myExtension.myWrapper:=TControl(self);
   end;

end;

constructor TXMenuItem.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,false);
end;

constructor TXMenuItem.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner.Owner);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXMenuItem.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin

  SelectionBorderColor:=glbSelectionBorderColor;

  IsSelected:=false;
  IsContainer:=true;

  MyEventTypes:=TStringList.Create;
  self.OnClick:=@self.MenuItemClick;

  self.myNode:=TDataNode.Create('UI',self.Name,'','TXMenuItem',false);
  self.myNode.ScreenObject:=self;

  // Set the parent menu
  if (TheOwner is TXMainMenu)
  and (TXMainMenu(TheOwner).myNode<>nil) then
  begin
     self.myNode.MyForm:=TXMainMenu(TheOwner).myNode.MyForm;
     self.SetParentComponent(TXMainMenu(TheOwner));
  end
  else if (TheOwner is TXMenuItem)
  and (TXMenuItem(TheOwner).myNode<>nil) then
  begin
     self.myNode.MyForm:=TXMenuItem(TheOwner).myNode.MyForm;
     self.SetParentComponent(TXMenuItem(TheOwner));
  end
  else if (TheOwner is TMainMenu)   then
  begin
     self.myNode.MyForm:=TForm(TMainMenu(TheOwner).Owner);
     self.SetParentComponent(TMainMenu(TheOwner));
  end
  else if (TheOwner is TForm)  then
  begin
    self.myNode.MyForm:=TForm(TheOwner);
    self.SetParentComponent(TForm(TheOwner).Menu);
  end;

  self.SetMyEventTypes;
  self.myNode.myEventTypes:=self.myEventTypes;
  SetLength(self.myNode.myEventHandlers,self.myNode.myEventTypes.Count);

  Self.myNode.IsDynamic:=false;

  AddDefaultAttribs(self,self.myNode,ItemdefaultAttribs);

end;

procedure TXMenuItem.SetParentComponent(Value:TComponent);
begin
  inherited SetParentComponent(Value);
end;

function CreateMMWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
  ParentForm:TForm;
begin
  NewNode:=CreateDynamicLazWidget('TXMainMenu',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  //!!!! the 'parent' for a menu node should be the form node for the relevant form
  AddChildToParentNode(ParentNode,NewNode,position);
  ParentForm:=TForm(ParentNode.ScreenObject);
  ParentForm.Menu:=TMainMenu(NewNode.ScreenObject);
  result:=NewNode;
end;
function CreateMIWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewWidget:TXMenuItem;
  NewNode:TDataNode;
  mm:TMenu;
begin
  mm:=TMenu(ParentNode.ScreenObject);
  if (mm is TXMainMenu) then
  begin
    NewWidget:=TXMainMenu(mm).AddMenuItem(ScreenObjectName);
    //showmessage('menu item added under TXMainMenu.  parentnode is '+ParentNode.NodeType);
  end
  else
  begin
    NewWidget:=TXMenuItem(mm).AddMenuItem(ScreenObjectName);
    //showmessage('menu item added under TXMenuItem.  parentnode is '+ParentNode.NodeType);
  end;

  //NewWidget.Name:=ScreenObjectName;
  NewNode:=NewWidget.myNode;
  AddChildToParentNode(ParentNode,NewNode,position);
  result:=NewNode;
end;

procedure TXMenuItem.MenuItemClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState)
  then
     if self.Count=0 then
       CallHandleEvent('Click',self.myNode.NodeName,self)
     else
       CallHandleEvent('OpenSubMenu',self.myNode.NodeName,self);
end;

function TXMainMenu.GetIsVisible:Boolean;
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
procedure TXMainMenu.SetIsVisible(AValue:Boolean);
begin
  if myNode<>nil then
    myNode.SetAttributeValue('IsVisible',myBoolToStr(AValue),'Boolean');
  //self.Visible:=AValue;
end;

function TXMainMenu.GetName:string;
var
  myname:string;
begin
  result:=inherited Name;
end;
function TXMenuItem.GetName:string;
var
  myname:string;
begin
  result:=inherited Name;
end;
function TXMenuItem.GetHint:string;
begin
  result:=myNode.GetAttribute('Hint',true).AttribValue;
end;
procedure TXMainMenu.SetMyName(AValue:string);
begin
  inherited Name:=AValue;

  if myNode<>nil then
     myNode.NodeName:=AValue;
  // also rename any associated event code ???  !!!!  name change not implemented...tbd
end;
procedure TXMenuItem.SetMyName(AValue:string);
begin
  inherited Name:=AValue;

  if myNode<>nil then
     myNode.NodeName:=AValue;
  // what else might need renaming ???
end;

procedure TXMainMenu.SetIsSelected(AValue: Boolean);
begin
  if AValue<>FIsSelected then
    begin
      FIsSelected:=AValue;
      //ShowHideSelectedBorder(self.myNode,FIsSelected);
      //Repaint;
    end;
end;
procedure TXMenuItem.SetIsSelected(AValue: Boolean);
begin
  if AValue<>FIsSelected then
    begin
      FIsSelected:=AValue;

      //ShowHideSelectedBorder(self.myNode,FIsSelected);
      //Repaint;
    end;
end;
function TXMenuItem.GetIsVisible:Boolean;
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
procedure TXMenuItem.SetIsVisible(AValue:Boolean);
begin
  if myNode<>nil then
    myNode.SetAttributeValue('IsVisible',myBoolToStr(AValue),'Boolean');
  self.Visible:=AValue;
end;

procedure TXMenuItem.SetHint(AValue:string);
begin
  myNode.SetAttributeValue('Hint',AValue);
  inherited Hint:=AValue;
end;

// Name is the first property loaded from .lfm
// Hijacking this so that we can decide which set of default values to apply for other properties
// (because there is a problem produced for string properties which are NOT saved to lfm
// when the value is blank).
procedure TXMainMenu.SetName(const NewName: TComponentName);
var
  ApplyName:TComponentName;
begin
  ApplyName:=GetNameToApply(self.myNode,self.myNode.MyForm.Name,self.Name,NewName,componentState);

  inherited SetName(ApplyName);

  if (csLoading in componentState) then
  begin
//    showmessage('TXMainMenu setname. loading.');
    //Hint:='';
  end;

end;
procedure TXMenuItem.SetName(const NewName: TComponentName);
var
  ApplyName:TComponentName;
begin
  ApplyName:=GetNameToApply(self.myNode,self.myNode.MyForm.Name,self.Name,NewName,componentState);

  inherited SetName(ApplyName);

  if (csLoading in componentState) then
  begin
//    showmessage('TXMenuItem setname. loading.');
    Hint:='';
  end;

end;

procedure TXMainMenu.SetSelectionBorderColor(AValue: TColor);
// Colour of the 'IsSelected' dotted border
begin
  if AValue<>FSelectionBorderColor then
    begin
      FSelectionBorderColor:=AValue;
      //Repaint;
    end;
end;
procedure TXMenuItem.SetSelectionBorderColor(AValue: TColor);
// Colour of the 'IsSelected' dotted border
begin
  if AValue<>FSelectionBorderColor then
    begin
      FSelectionBorderColor:=AValue;
      //Repaint;
    end;
end;

function TXMainMenu.AddMenuItem(NewName:String): TXMenuItem;
var
  NewItem:TXMenuItem;
begin
//  showmessage('AddMenuItem. MyNode type='+myNode.NodeType+' name='+myNode.NodeName);
  NewItem := TXMenuItem.Create(Self,self.myNode.IsDynamic);
  NewItem.Name:=NewName;
  result:=NewItem;
end;

function TXMenuItem.AddMenuItem(NewName:String): TXMenuItem;
var
  NewItem:TXMenuItem;
begin
//  showmessage('AddMenuItem. MyNode type='+myNode.NodeType+' name='+myNode.NodeName);
  NewItem := TXMenuItem.Create(Self,self.myNode.IsDynamic);
  NewItem.Name:=NewName;
  result:=NewItem;
end;

{$else}


procedure addMenuBarStyles;
begin
asm
// ----------------------------------------check if the styles have already been set
var x = document.getElementsByTagName("STYLE");
var StyleIsSet = false;
if (x.length>0){
  for (var i=0; i<x.length; i++){
    var y= x[i].innerHTML;
    if (y.indexOf("menuBar") !=-1) { StyleIsSet =true}
  }
}

if (StyleIsSet==false) {
//First, the bar. A DIV tag is used with a style class defined to set the appearance.

var StyleText='<style>';

StyleText=StyleText + 'div.menuBar {';
StyleText=StyleText + 'font-family: "MS Sans Serif", Arial, sans-serif;';
StyleText=StyleText + 'font-size: 8pt;';
StyleText=StyleText + 'font-style: normal;';
StyleText=StyleText + 'font-weight: normal;';
StyleText=StyleText + 'color: #000000;';
StyleText=StyleText + '}';

StyleText=StyleText + 'div.menuBar {';
StyleText=StyleText + '  background-color: #d0d0d0;';
StyleText=StyleText + '  border: 2px solid;';
StyleText=StyleText + '  border-color: #f0f0f0 #909090 #909090 #f0f0f0;';
StyleText=StyleText + '  padding: 4px 2px 4px 2px;';
StyleText=StyleText + '  text-align: left;';
StyleText=StyleText + '}';


//For the buttons, normal hypertext links (A tags) are used. This way, both a style class and a hover: pseudo-class can be defined to
//set the default and mouseover appearance. The style sheet is updated as follows.

StyleText=StyleText + 'div.menuBar,';
StyleText=StyleText + 'div.menuBar a.menuButton {';
StyleText=StyleText + '  font-family: "MS Sans Serif", Arial, sans-serif;';
StyleText=StyleText + '  font-size: 8pt;';
StyleText=StyleText + '  font-style: normal;';
StyleText=StyleText + '  font-weight: normal;';
StyleText=StyleText + '  color: #000000;';
StyleText=StyleText + '}';

StyleText=StyleText + 'div.menuBar {';
StyleText=StyleText + '  background-color: #d0d0d0;';
StyleText=StyleText + '  border: 2px solid;';
StyleText=StyleText + '  border-color: #f0f0f0 #909090 #909090 #f0f0f0;';
StyleText=StyleText + '  padding: 4px 2px 4px 2px;';
StyleText=StyleText + '  text-align: left;';
StyleText=StyleText + '}';

StyleText=StyleText + 'div.menuBar a.menuButton {';
StyleText=StyleText + '  background-color: transparent; ';
StyleText=StyleText + '  border: 1px solid #d0d0d0;';
StyleText=StyleText + '  color: #000000;';
StyleText=StyleText + '  cursor: default;';
StyleText=StyleText + '  left: 0px;';
StyleText=StyleText + '  margin: 1px;';
StyleText=StyleText + '  padding: 2px 6px 2px 6px;';
StyleText=StyleText + '  position: relative;';
StyleText=StyleText + '  text-decoration: none;';
StyleText=StyleText + '  top: 0px;';
StyleText=StyleText + '  z-index: 100;';
StyleText=StyleText + '}';

StyleText=StyleText + 'div.menuBar a.menuButton:hover {';
StyleText=StyleText + '  background-color: transparent;';
StyleText=StyleText + '  border-color: #f0f0f0 #909090 #909090 #f0f0f0;';
StyleText=StyleText + '  color: #000000;';
StyleText=StyleText + '}';
//First, note that the button's border color on the normal class matches that of the bar background, so it will
//remain invisible until moused over. Also, the cursor:default setting to used prevent the normal link cursor from
//displaying when the buttons are moused over. This makes it look more like a menu bar found on a typical application window.
//The reason for using relative positioning will be seen later.
//


//When a button is clicked on, we want to change its appearance, making it look depressed. This is done by defining another style class.

StyleText=StyleText + 'div.menuBar a.menuButtonActive,';
StyleText=StyleText + 'div.menuBar a.menuButtonActive:hover {';
StyleText=StyleText + '  background-color: #a0a0a0;';
StyleText=StyleText + '  border-color: #909090 #f0f0f0 #f0f0f0 #909090;';
StyleText=StyleText + '  color: #ffffff;';
StyleText=StyleText + '  left: 1px;';
StyleText=StyleText + '  top: 1px;';
StyleText=StyleText + '}';


//A similar approach is used for the drop down menus. Each menu will consist of a DIV tag which acts as a
//container for several item links and possibly some separator bars. As before, style classes as set up for each component of the menu.


StyleText=StyleText + 'div.menuBar,';
StyleText=StyleText + 'div.menuBar a.menuButton,';
StyleText=StyleText + 'div.menu,';
StyleText=StyleText + 'div.menu a.menuItem {';
StyleText=StyleText + '  font-family: "MS Sans Serif", Arial, sans-serif;';
StyleText=StyleText + '  font-size: 8pt;';
StyleText=StyleText + '  font-style: normal;';
StyleText=StyleText + '  font-weight: normal;';
StyleText=StyleText + '  color: #000000;';
StyleText=StyleText + '}';


StyleText=StyleText + 'div.menu {';
StyleText=StyleText + '  background-color: #d0d0d0;';
StyleText=StyleText + '  border: 2px solid;';
StyleText=StyleText + '  border-color: #f0f0f0 #909090 #909090 #f0f0f0;';
StyleText=StyleText + '  left: 0px;';
StyleText=StyleText + '  padding: 0px 1px 1px 0px;';
StyleText=StyleText + '  position: absolute;';
StyleText=StyleText + '  top: 0px;';
StyleText=StyleText + '  visibility: hidden;';
StyleText=StyleText + '  z-index: 101;';
StyleText=StyleText + '}';

StyleText=StyleText + 'div.menu a.menuItem {';
StyleText=StyleText + '  color: #000000;';
StyleText=StyleText + '  cursor: default;';
StyleText=StyleText + '  display: block;';
StyleText=StyleText + '  padding: 3px 1em;';
StyleText=StyleText + '  text-decoration: none;';
StyleText=StyleText + '  white-space: nowrap;';
StyleText=StyleText + '}';

StyleText=StyleText + 'div.menu a.menuItem:hover, div.menu a.menuItemHighlight {';
StyleText=StyleText + '  background-color: #000080;';
StyleText=StyleText + '  color: #ffffff;';
StyleText=StyleText + '}';

StyleText=StyleText + 'div.menu div.menuItemSep {';
StyleText=StyleText + '  border-top: 1px solid #909090;';
StyleText=StyleText + '  border-bottom: 1px solid #f0f0f0;';
StyleText=StyleText + '  margin: 4px 2px;';
StyleText=StyleText + '}';

StyleText=StyleText + 'div.menu a.menuItem span.menuItemText {}';

StyleText=StyleText + 'div.menu a.menuItem span.menuItemArrow {';
StyleText=StyleText + '  margin-right: -.75em;';
StyleText=StyleText + '}';


StyleText=StyleText + '.menuItemDisabled {';
StyleText=StyleText + '  color: #aaaaaa !important;';
StyleText=StyleText + '}';

StyleText=StyleText + '</style>';

//alert(StyleText);
document.head.innerHTML = document.head.innerHTML+StyleText;

}
//else {alert('style found already');}

end;
end;

constructor TXMainMenu.Create(MyForm:TForm;NodeName,NameSpace:String);
begin
  inherited Create(NodeName,NameSpace);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=true;

  SetNodePropDefaults(self,MenuDefaultAttribs);

end;
constructor TXMenuItem.Create(MyForm:TForm;NodeName,NameSpace:String);
begin
  inherited Create(NodeName,NameSpace);
  self.NodeType:='TXMenuItem';
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=true;

  SetNodePropDefaults(self,ItemDefaultAttribs);
 // Caption:='New Item';
end;


function CreateMainMenu(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  BgColor:String;
begin
  //showmessage('CreateMainMenu');

  asm
    try{
    pas.XMenu.addMenuBarStyles();

    //alert('add main menu to parent '+ParentNode.NodeName);
    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,NameSpace,$impl.MyNodeType,0);
    wrapper.position="";


    //localcontainer is an inner div.  Its id is  ScreenObjectName+'Contents'
    // It is a child of the outer container div (wrapper)
    //
     var localcontainer = document.createElement("div");
     localcontainer.id = ScreenObjectName+'Contents';
     localcontainer.classList.add('menuBar');

     //localcontainer.onmouseover = function(){pas.XMenu.menuMouseover(ScreenObjectName);pas.HTMLUtils.StopBubbling(event);}
     wrapper.onclick = function(){pas.XMenu.CloseAnyDropdown(ScreenObjectName);pas.HTMLUtils.StopBubbling(event);}

     wrapper.appendChild(localcontainer);


    }
    catch(err) { alert(err.message+'  in XMenu.CreateMainMenu');}

  end;

  MyNode.ScreenObject:=MyNode;

  // now that we have a datanode and a widget, cycle attribute settings
  RefreshComponentProps(myNode);
  //SetCommonWrapperProperties(TWrapperPanel(myNode));

result:=myNode;
end;


function IdentifyMainMenu(menuName:String):TXMainMenu;
var
  StartNode:TDataNode;
  found:Boolean;
  ParentName:String;
begin
//showmessage('IdentifyMainMenu '+menuName);
  StartNode:=findDataNodeById(SystemNodeTree,menuName,'',true);
  found:=false;
  while ((found=false) and (StartNode<>nil)) do
  begin
    if StartNode.NodeType = 'TXMainMenu' then
      found:=true
    else
    begin
      ParentName:=StartNode.GetAttribute('ParentName',false).AttribValue;
      //showmessage('ParentName='+Parentname);
      StartNode:=findDataNodeById(SystemNodeTree,ParentName,'',true);
    end;
  end;
  result:=TXMainMenu(StartNode.ScreenObject);
end;

function IdentifyParentMenu(itemName:String):TXMenuItem;
var
  StartNode:TDataNode;
  ParentName:String;
begin
  StartNode:=findDataNodeById(SystemNodeTree,itemName,'',true);
  if (StartNode<>nil) then
  begin
      ParentName:=StartNode.GetAttribute('ParentName',false).AttribValue;
      StartNode:=findDataNodeById(SystemNodeTree,ParentName,'',true);
      result:=TXMenuItem(StartNode.ScreenObject);
  end
  else
    result:=nil;
end;

procedure menuBoxInit(menu:TObject);
begin

asm
{

  var itemList, spanList;
  var textEl, arrowEl;
  var itemWidth;
  var w, dw;
  var i, j;


  // For IE, replace arrow characters.

  if (pas.HTMLUtils.browser.isIE) {
    menu.style.lineHeight = "2.5ex";
    spanList = menu.getElementsByTagName("SPAN");
    for (i = 0; i < spanList.length; i++)
      if (pas.HTMLUtils.hasClassName(spanList[i], "menuItemArrow")) {
        spanList[i].style.fontFamily = "Webdings";
        spanList[i].firstChild.nodeValue = "4";
      }
  }
  // Find the width of a menu item.

  itemList = menu.getElementsByTagName("A");
  if (itemList.length > 0)
    itemWidth = itemList[0].offsetWidth;
  else
    return;

      // For items with arrows, add padding to item text to make the
// arrows flush right.

for (i = 0; i < itemList.length; i++) {
  spanList = itemList[i].getElementsByTagName("SPAN");
  textEl  = null;
  arrowEl = null;
  for (j = 0; j < spanList.length; j++) {
    if (pas.HTMLUtils.hasClassName(spanList[j], "menuItemText"))
      textEl = spanList[j];
    if (pas.HTMLUtils.hasClassName(spanList[j], "menuItemArrow"))
      arrowEl = spanList[j];
  }
  if (textEl != null && arrowEl != null) {
    textEl.style.paddingRight = (itemWidth
      - (textEl.offsetWidth + arrowEl.offsetWidth)) + "px";
    // For Opera, remove the negative right margin to fix a display bug.
    if (pas.HTMLUtils.browser.isOP)
      arrowEl.style.marginRight = "0px";
  }
}

  // Fix IE hover problem by setting an explicit width on first item of
// the menu.

if (pas.HTMLUtils.browser.isIE) {
  w = itemList[0].offsetWidth;
  itemList[0].style.width = w + "px";
  dw = itemList[0].offsetWidth - w;
  w -= dw;
  itemList[0].style.width = w + "px";
}

}
end;

end;

procedure resetButton(button:TObject);
begin

asm

  if (button!=null) {
    //alert('resetButton '+button.id);

    // Restore the button's style class.

    pas.HTMLUtils.removeClassName(button, "menuButtonActive");

    // Hide the button's menu, first closing any sub menus.

    if (button.menu != null) {
      //alert('closing button menu '+ button.menu.id);
      pas.XMenu.closeSubMenu(button.menu.id);
      //alert('set hidden '+ button.menu.id);
      button.menu.style.visibility = "hidden";
    }
  }
end;

end;

procedure menuButtonClick(buttonName:String);
var
  mainmenu:TXMainMenu;
  ButtonNode,parentNode:TDataNode;
  submenuname:String;
begin
  mainmenu:=IdentifyMainMenu(buttonName);
  CloseAnyDropdown(mainmenu.NodeName);

  ButtonNode:=FindDataNodeById(mainmenu,buttonName,'',true);
  ParentNode:=FindParentOfNode(mainmenu,ButtonNode);

  //showmessage('parent for '+buttonName+' is '+ParentNode.NodeName);
  submenuname:=buttonName+'Box';

asm
  {
   function depressButton(button) {

  var x, y;

  // Update the button's style class to make it look like it's
  // depressed.

  //alert('setting menuButtonActive on '+button.id);
  button.className += " menuButtonActive";

  // Position the associated drop down menu under the button and
  // show it.
  if (button.menu!=null) {
    x = pas.HTMLUtils.getPageOffsetLeft(button);
    y = pas.HTMLUtils.getPageOffsetTop(button) + button.offsetHeight;

   // alert('depressButton  x='+x+' y='+y+' menu is '+button.menu.id);

   //alert('showing button menu '+button.menu.id);
    button.menu.style.left = x + "px";
    button.menu.style.top  = y + "px";
    button.menu.style.visibility = "visible";
  }
}




// Get the target button element.
 // alert('menuButtonClick '+buttonName+' submenuname='+submenuname);

  var button = document.getElementById(buttonName);

  // Blur focus from the link to remove that annoying outline.

  button.blur();

  // Associate the named sub-menu to this button if not already done.
  // Additionally, initialize menu display.

  if (button.menu == null) {
    button.menu = document.getElementById(submenuname);
    if (button.menu!=null)
    {
    if ((!(button.menu.hasAttribute("isInitialized")))
     ||(button.menu.isInitialized == null))  {
      pas.XMenu.menuBoxInit(button.menu);
      button.menu.isInitialized = true;
      }
     }
  }

  // Activate this button, unless it was the currently active one.

  if (button != mainmenu.ActiveButton) {
    depressButton(button);
    mainmenu.ActiveButton = button;
  }
  else {
    mainmenu.ActiveButton = null;
    }
}
end;

end;


procedure buttonMouseover(buttonName:String);
var
  mainmenu:TXMainMenu;
begin
  // MouseOver handler for top level menu items
  mainmenu:=IdentifyMainMenu(buttonName);

  asm
   {

     var button = document.getElementById(buttonName);

    // If any other button menu is active, make this one active instead.

    if (mainmenu.ActiveButton != null && mainmenu.ActiveButton != button)
    {
      pas.XMenu.menuButtonClick(buttonName);
      }
  }
  end;

end;



function CreateMenuItem(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  OnChangeString, OnClickString, OnMouseOverString1, OnMouseOverString2,MouseOutString:String;
  BgColor, Caption, ParentName:String;
  IsMainItem:Boolean;
begin
  if ParentNode.NodeType = 'TXMainMenu' then
     IsMainItem:=true
  else
     IsMainItem:=false;
  ParentName:=ParentNode.NodeName;

  Caption:=MyNode.GetAttribute('Caption',true).AttribValue;

  BgColor:='#FFFFFF';

  //showmessage('menuitem createwidget '+ScreenObjectName+' parent='+ParentName);

  OnClickString:='onclick="event.stopPropagation();'+
                            'if (!(event.target.classList.contains(''menuItemDisabled''))) {' +
                            'pas.XMenu.menuButtonClick('''+ScreenObjectName+'''); '+
                            'pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''','''',''''); '+
                            '}'+
                            '" ';

  OnMouseOverString1:='onmouseover="pas.XMenu.buttonMouseover('''+ScreenObjectName+''');pas.HTMLUtils.StopBubbling(event);" ';
  OnMouseOverString2:='onmouseover="pas.XMenu.menuItemMouseover('''+ScreenObjectName+''');pas.HTMLUtils.StopBubbling(event);" ';

//  MouseOutString:=' onmouseout="pas.XMenu.closeSubMenu(event.target.parentNode.id);"';

  asm
    try{


  // -----------------------------Define the HTML to be used to create Menu control

  // if this is a main menu item, use button tag
  if (IsMainItem) {

      var MyParent = pas.HTMLUtils.ScreenObjectInnerComponent(ParentNode);

      var MenuItemHTML =
                     '<a href="#" class="menuButton" id="'+ScreenObjectName+'"' +
                     OnClickString +
                     OnMouseOverString1 +
                     '>'+Caption+
                     '</a>'+
                     '<div id="'+ScreenObjectName+'Box'+'" class="menu" visibility = "hidden" '+
 //                    MouseOutString +
                     '></div>';

       pas.HTMLUtils.AddObjectToParentObject(ParentNode,MyParent.id,ScreenObjectName,position,MenuItemHTML);
       }
  else {
      MyParent =  document.getElementById(ParentName);
      //alert('Adding item '+ScreenObjectName+' parent: '+ParentName);

      if ((IsMainItem==false) && (pas.HTMLUtils.hasClassName(MyParent,'menuItem'))) {
        // adjust the parent item's html definition so that the expansion arrow is visible
        // eg...
        //<a class="menuItem" href="...">
        //  <span class="menuItemText">Menu 3 Item 4</span>
        //  <span class="menuItemArrow">&#9654;</span></a>

        // ParentName should give us the <a> item relating to the menu item that is the parent of this sub item.
        // so, insert the arrow for that parent, if not already there
        var ob = document.getElementById(ParentName+'Span');
        if (ob==null) {
          var newSpan = document.createElement("SPAN");
          newSpan.id = ParentName+'Span';
          newSpan.className = 'menuItemArrow';
          newSpan.innerHTML = '&#9654;';
          MyParent.appendChild(newSpan);
        }
      }

      MenuItemHTML='<a href="#" class="menuItem" id="'+ScreenObjectName+'" '+
                    OnClickString +
                    OnMouseOverString2+
                    ' >' +
                    '<span class="menuItemText">'+Caption+'</span>' +
                    '</a>' +
                    '<div id="'+ScreenObjectName+'Box'+'" class="menu" '+
  //                  MouseOutString +
                    '></div>';

      pas.HTMLUtils.AddObjectToParentObject(ParentNode,MyParent.id,ScreenObjectName,position,MenuItemHTML);

      var wrapper=document.getElementById(ScreenObjectName);

      //alert('attaching '+wrapper.id+' to parent '+ParentName+'Box');
      var menubox = document.getElementById(ParentName+"Box");
      menubox.appendChild(wrapper);
      }
  }
  catch(err) { alert(err.message+'  in XMenu.CreateMenuItem');}

end;

  MyNode.ScreenObject:=MyNode;

    // now that we have a datanode and a widget, cycle attribute settings
    //SetCommonWrapperProperties(TWrapperPanel(myNode));
    RefreshComponentProps(myNode);
    myNode.SetAttributeValue('ParentName',ParentName);

result:=myNode;
end;

procedure closeSubMenu(menuName:String);    // name of a 'Box' div
begin

asm
try{
  var menu=document.getElementById(menuName);
  if ((menu == null) || (menu==undefined) || (menu.activeItem == null)) {
    return;
    }
  //alert('closeSubMenu '+menuName);

  // Recursively close any sub menus.

  if (menu.activeItem.subMenu != null) {
    //alert('menu.activeItem.subMenu is '+menu.activeItem.subMenu.id);
    pas.XMenu.closeSubMenu(menu.activeItem.subMenu.id);
 //   alert('hiding submenu '+menu.activeItem.subMenu.id);
    menu.activeItem.subMenu.style.visibility = "hidden";
    menu.activeItem.subMenu = null;
  }
 // alert(menuName+' removeClassName menuItemHighlight');
  pas.HTMLUtils.removeClassName(menu.activeItem, "menuItemHighlight");
  //alert('menu '+menu.id+' clear activeitem ');
  menu.activeItem = null;

  //alert(menuName+' closeSubMenu done');
  } catch(err) { alert(err.message+'  in XMenu.closeSubMenu'); }
end;

end;

procedure CloseAnyDropdown(menuName:String);
var
  mainmenunode:TDataNode;
  mainmenu:TXMainMenu;
begin
  mainmenunode:=findDataNodeById(SystemNodeTree,menuName,'',true);
  mainmenu:=TXMainMenu(mainmenunode.ScreenObject);

asm
{
  // Close any active sub menu.

  if (mainmenu.ActiveButton != null) {
      pas.XMenu.resetButton(mainmenu.ActiveButton);
      }
}
end;

end;


procedure menuItemMouseover(ItemName:String);
var
  parentmenu:TXMenuItem;
  localName,parentName:String;
begin
  //showmessage('mimo 0. ItemName='+ItemName);
  parentmenu:= IdentifyParentMenu(ItemName);

  //if parentmenu<>nil then showmessage('mouseover. ItemName='+ItemName+' parent='+parentmenu.Nodename);
  parentName:=parentMenu.NodeName+'Box';
  localName:=ItemName;

asm
{
  var   x, y;
  // Find the target item element and its parent menu element.
  var menu = document.getElementById(parentName);    // the box we are in
  var item = document.getElementById(localName);     // the item under the mouse

  // Close any active sub menu and mark this one as active.
  //alert('menu '+menu.id+' set activeitem '+item.id);
  if (menu.activeItem != null) {
    //alert('menuItemMouseover close submenu '+menu.id);
    pas.XMenu.closeSubMenu(menu.id);
    }
  menu.activeItem = item;

  // Highlight the item element.
  item.className += " menuItemHighlight";

  // Initialize the item's submenu, if not already done.
  if (item.subMenu == null) {
    item.subMenu = document.getElementById(localName+'Box');          //????
    //item.subMenu = document.getElementById(localName);          //????
    if (item.subMenu!=null)
    {
    if ((!(item.subMenu.hasAttribute("isInitialized")))
     ||(item.subMenu.isInitialized == null))  {
      //alert('initialising submenu '+localName);
      pas.XMenu.menuBoxInit(item.subMenu);
      item.subMenu.isInitialized = true;
     }
     }
  }

  if (item.subMenu!=null)
  {
  // Get position for submenu based on the menu item.
  x = pas.HTMLUtils.getPageOffsetLeft(item);
  //+ item.offsetWidth;                           //!!!! still not right.... tbd
  y = pas.HTMLUtils.getPageOffsetTop(item);

  //alert('initial x='+x+' y='+y);

  // Adjust position to fit in view.
  var maxX, maxY;

  if (pas.HTMLUtils.browser.isIE) {
    maxX =
      (document.documentElement.scrollLeft   != 0 ?
         document.documentElement.scrollLeft
       : document.body.scrollLeft)
    + (document.documentElement.clientWidth  != 0 ?
       document.documentElement.clientWidth
       : document.body.clientWidth);
    maxY =
      (document.documentElement.scrollTop    != 0 ?
       document.documentElement.scrollTop
       : document.body.scrollTop)
    + (document.documentElement.clientHeight != 0 ?
       document.documentElement.clientHeight
       : document.body.clientHeight);
  }
  if (pas.HTMLUtils.browser.isOP) {
    maxX = document.documentElement.scrollLeft + window.innerWidth;
    maxY = document.documentElement.scrollTop  + window.innerHeight;
  }
  if (pas.HTMLUtils.browser.isNS) {
    maxX = window.scrollX + window.innerWidth;
    maxY = window.scrollY + window.innerHeight;
  }
  maxX -= item.subMenu.offsetWidth;
  maxY -= item.subMenu.offsetHeight;

  if (x > maxX)
    x = Math.max(0, x - item.offsetWidth - item.subMenu.offsetWidth
      + (menu.offsetWidth - item.offsetWidth));
  y = Math.max(0, Math.min(y, maxY));

  //alert('final x='+x+' y='+y);

  // Position and show it.
  //alert('showing submenu '+item.subMenu.id);
  item.subMenu.style.left = x + "px";
  item.subMenu.style.top  = y + "px";
  item.subMenu.style.visibility = "visible";
  }
}
end;

end;



function CreateMainMenuInterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXMainMenu.Create(MyForm,NodeName,NameSpace));
end;
function CreateMenuItemInterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXMenuItem.Create(MyForm,NodeName,NameSpace));
end;
{$endif}

function TXMenuItem.GetCaption:string;
begin
  result:=myNode.GetAttribute('Caption',true).AttribValue;
end;
procedure TXMenuItem.SetCaption(AValue:string);
begin
  myNode.SetAttributeValue('Caption',AValue);
  {$ifndef JScript}
  inherited Caption:=AValue;
  {$else}
  asm
    var ob=document.getElementById(this.myNode.NodeName);
    if (ob!=null) {
      ob.innerHTML=AValue;
    }
  end;
  {$endif}
end;

function TXMenuItem.GetIsEnabled:Boolean;
var
  tmp:String;
begin
  if myNode<>nil then
  begin
    tmp:=myNode.GetAttribute('IsEnabled',true).AttribValue;
    if tmp='' then tmp:='True';
    result:=myStrToBool(tmp);
  end
  else
    result:=True;
end;
procedure TXMenuItem.SetIsEnabled(AValue:Boolean);
begin
  if myNode<>nil then
    myNode.SetAttributeValue('IsEnabled',myBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  self.Enabled:=AValue;
  {$else}
  asm
    var ob=document.getElementById(this.myNode.NodeName);
    if (ob!=null) {
      if (AValue==true) {
      ob.classList.remove("menuItemDisabled"); }
      else {
      ob.classList.add("menuItemDisabled"); }
    }
  end;
  {$endif}
end;

{$ifndef JScript}
{ TXMenuComponentEditor }

const
  nbvAddItem       = 0;

constructor TXMenuComponentEditor.Create(AComponent: TComponent;
    ADesigner: TComponentEditorDesigner);
begin
  inherited create(AComponent,ADesigner);

  self.fMenuComponent:=TMenu(AComponent);
end;

procedure TXMenuComponentEditor.DoAddItem;
var
  Hook: TPropertyEditorHook;
  newItem:TXMenuItem;
  MenuNode,ItemNode:TDataNode;
  lItemName:string;
begin
  Hook:=nil;
  if not GetHook(Hook) then exit;

  if (MenuComponent is TXMainMenu) then
    MenuNode:=TXMainMenu(MenuComponent).myNode
  else
    MenuNode:=TXMenuItem(MenuComponent).myNode;

  lItemName := Designer.CreateUniqueComponentName(TXMenuItem.ClassName);
  //showmessage('DoAddItem. menunode='+MenuNode.NodeType+' '+MenuNode.NodeName+' name='+lItemName);
  ItemNode := CreateMIWidget(MenuNode,lItemName,'',-1,'');
  //showmessage('CreateMIWidget done.  ItemNode.Nodename='+ItemNode.Nodename);
  NewItem := TXMenuItem(ItemNode.ScreenObject);

  Hook.PersistentAdded(NewItem, True);
  Modified;
  if Designer <> nil then Designer.Modified;
  //showmessage('DoAddItem done');
end;

procedure TXMenuComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    nbvAddItem:       DoAddItem;
  end;
end;

function TXMenuComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    nbvAddItem:       Result:=mesAddItem;
  else
    Result:='';
  end;
end;

function TXMenuComponentEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

procedure TXMenuComponentEditor.PrepareItem(Index: Integer;
  const AnItem: TMenuItem);
begin
  inherited PrepareItem(Index, AnItem);
  case Index of
    nbvAddItem:       ;
    //nbvInsertPage:    AnItem.Enabled:=TabControl.PageIndex>=0;
    //nbvDeletePage:    AnItem.Enabled:=TabControl.PageIndex>=0;
    //nbvMovePageLeft:  AnItem.Enabled:=TabControl.PageIndex>0;
    //nbvMovePageRight: AnItem.Enabled:=TabControl.PageIndex<TabControl.PageCount-1;
    //nbvShowPage:      AddMenuItemsForPages(AnItem);
  end;
end;
function TXMenuComponentEditor.GetMenuComponent: TMenu;
begin
  Result := FMenuComponent;
end;
{$endif}


begin
  // this is the set of node attributes that each Menu instance will have.
  AddDefaultAttribute(MenuDefaultAttribs,'IsVisible','Boolean','True','',false);
  AddDefaultsToTable('TXMainMenu',MenuDefaultAttribs);

  // this is the set of node attributes that each MenuItem instance will have.
  AddDefaultAttribute(ItemDefaultAttribs,'Caption','String','New Item','',false);
  AddDefaultAttribute(ItemDefaultAttribs,'Hint','String','','',false);
  AddDefaultAttribute(ItemDefaultAttribs,'IsVisible','Boolean','True','',false);
  AddDefaultAttribute(ItemDefaultAttribs,'IsEnabled','Boolean','True','',false);
  AddDefaultsToTable('TXMenuItem',ItemDefaultAttribs);

  {$ifndef JScript}
  RegisterClass(TXMainMenu);
  RegisterClass(TXMenuItem);
  RegisterComponentEditor(TXMainMenu, TXMenuComponentEditor);
  RegisterComponentEditor(TXMenuItem, TXMenuComponentEditor);
  AddNodeFuncLookup('TXMainMenu',@CreateMMWidget);
  AddNodeFuncLookup('TXMenuItem',@CreateMIWidget);
  {$else}
  AddNodeFuncLookup('TXMainMenu',@CreateMainMenuInterfaceObj,@CreateMainMenu);
  AddNodeFuncLookup('TXMenuItem',@CreateMenuItemInterfaceObj,@CreateMenuItem);
  SuppressDesignerProperty('TXMainMenu','LabelPos');
  SuppressDesignerProperty('TXMainMenu','LabelText');
  SuppressDesignerProperty('TXMenuItem','LabelPos');
  SuppressDesignerProperty('TXMenuItem','LabelText');
  {$endif}
end.

