(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit LazsUtils;

{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, Controls,StdCtrls, SysUtils, LCLIntf, ProjectIntf, LazIDEIntf, LResources, Dialogs, Types, Forms, Graphics,Clipbrd,
  PropEdits, RTTICtrls, TypInfo, Menus,
  {$ifdef Chromium}
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFConstants, uCEFTypes, uCEFChromiumEvents,
  {$endif}
  StringUtils, NodeUtils, Pas2jsCompiler;
(*
type TXPropertyLink = class(TPropertyLink)
private
  FTIObjectName:String;
protected
published
  property TIObjectName:String read FTIObjectName write FTIObjectName;
//  property OnBeforeWrite;
//  property OnAfterWrite;
//  property Options;
  property TIObject;
  property TIPropertyName;
//  property TIElementName;
end;
*)
type
  TMyProjectEvents = class
  private
    //function OnProjectBuilding(Sender: TObject): TModalResult;
  public
    constructor Create;
    destructor Destroy; override;
    function OnProjectOpened(Sender: TObject;
                                   AProject: TLazProject): TModalResult;
  end;

var myProjectEvents:TMyProjectEvents;



  type
  TXDesignerExtension = class
  private
    function AddClicked(ADesigner: TIDesigner;
             MouseDownComponent: TComponent; Button: TMouseButton;
             Shift: TShiftState; X, Y: Integer;
             var AComponentClass: TComponentClass;
             var NewParent: TComponent): boolean;
    procedure PersistentDeleting(APersistent: TPersistent);
    procedure PersistentAdded(APersistent: TPersistent; Select: boolean);
    procedure RefreshPropertyValues;
  public
    myWrapper:TControl;
    constructor Create;
    destructor Destroy; override;
  end;

//function FindObjectByID(WithinForm:TForm;ScreenObjectID:String):TControl;
function HasProperty(aObject:TObject; aProperty:string):boolean;
function GetBooleanProperty(aObject:TObject; aProperty:string):Boolean;
Procedure SetBooleanProperty(aObject:TObject; aProperty:string; AValue : Boolean);
procedure IdentifyChildNodes(ParentControl:TControl);
procedure SetHeightWidth(myNode:TDataNode;var MySelf:TControl;WidthProp,HeightProp:string);
procedure FetchHeightWidth(NewNode:TDataNode; var h,w:string;WidthProp,HeightProp:string);
procedure CheckPercentageSizing(myComponent:TControl);
function DoXFormCreated(myForm:TForm):TDataNode;
procedure DoFormResize(MyForm: TForm; RootObject:TWinControl);
//function LinkToStr(ALink:TXPropertyLink):string;
procedure ShowHideSelectedBorder(myNode:TDataNode;showborder:Boolean);
procedure PaintSelectedRectangle(myCanvas:TCanvas;PanelArea:TRect;SelectionBorderColor:TColor;IsSelected:boolean);
function FindSizedParent(myComponent:TControl;hw:String):TControl;
procedure ClearAllAlignment(myLabel,myControl:TControl);
function mygetClipboardData(stringname:string):string;
procedure ResetAlignment(AControl:TControl);
function SortAlignList(Item1, Item2: Pointer): Integer;
procedure InsertUnderParent(var MyComponent:TControl;MyParent:TWinControl;position:integer);
//procedure ClearAllScreenObjects;
function DeleteScreenObject(MyNode:TDataNode):string;  overload;
function DeleteScreenObject(ThisObject:TObject):string; overload;
procedure DoAlignmentAndLabelPosCascade(mySelf:TWinControl);
function SortOutAlignmentAndLabelPos(myself,myLbl,myControl:TControl; MyAlignment,MyLabelPos:string):String;
procedure LabelRightSettings(TargetControl:Tcontrol;myLbl:TLabel);
procedure CheckParentIsXContainer(AControl:TControl);
function GetNameToApply(myNode:TDataNode;FormName,Oldname,NewName:String;componentState:TComponentState):String;
Procedure WriteToFile(TextFileName,FileContents:String);
procedure WriteToLocalStore(KeyName,TheData:String);
function ReadFromLocalStore(KeyName:String):String;
function ReadFile(TextFileName:String):String;
procedure ClearLocalStore(KeyName:String);
function ResourceToString(resName:string):String;
procedure ResourceToFile(resName,fileName:string);
//{$ifdef Chromium}
//procedure SetupCEFResources;
//{$endif}

const
  glbBorderWidth:integer = 3;
  glbLabelSpacing:integer = 3;
  glbBorderSpacing:integer = 3;
  glbSelectionBorderColor:TColor = clGreen;



implementation
uses WrapperPanel, Events, XIFrame, XTabControl, XScrollBox, XForm;

{$ifdef Chromium}
var dummyChromium:TChromium;
{$endif}

   //...... IDE Extension to pick up designer events .....
constructor TXDesignerExtension.Create;
begin
  // register handler
  GlobalDesignHook.AddHandlerAddClicked(@AddClicked);                  // sort out which components can contain others
  GlobalDesignHook.AddHandlerPersistentDeleting(@PersistentDeleting);
  GlobalDesignHook.AddHandlerPersistentAdded(@PersistentAdded);
  GlobalDesignHook.AddHandlerRefreshPropertyValues(@RefreshPropertyValues);
end;

destructor TXDesignerExtension.Destroy;
begin
  // remove handler:
  GlobalDesignHook.RemoveHandlerAddClicked(@AddClicked);
  GlobalDesignHook.RemoveHandlerPersistentDeleting(@PersistentDeleting);
  GlobalDesignHook.RemoveHandlerPersistentAdded(@PersistentAdded);
  GlobalDesignHook.RemoveHandlerRefreshPropertyValues(@RefreshPropertyValues);
end;

function TXDesignerExtension.AddClicked(ADesigner: TIDesigner;
             MouseDownComponent: TComponent; Button: TMouseButton;
             Shift: TShiftState; X, Y: Integer;
             var AComponentClass: TComponentClass;
             var NewParent: TComponent): boolean;
var
  ParentIsContainer:Boolean;
  tmp:TObject;
begin
  // MouseDownComponent may be a wrapperpanel, or a control inside the wrapper which is a container type
  if (MouseDownComponent is TControl)
  and (MouseDownComponent = self.myWrapper) then
  begin
    // Whenever the user drops a control on a non-container component,
    // the new control is added as a sibling, not as a child.
    //!!!!Lazarus This does not work for drag-drop in the Lazarus object inspector !!!!
    ParentIsContainer:=true;
    if IsPublishedProp(MouseDownComponent,'IsContainer') then
      ParentIsContainer:=GetBooleanProperty(MouseDownComponent,'IsContainer');

    if ParentIsContainer = false then
       NewParent:=TControl(MouseDownComponent).Parent
    else
    begin
      //showmessage('mousedowncomponent is '+MouseDownComponent.ClassName);
      if HasProperty(MouseDownComponent,'myControl') then
      begin
        // is a wrapperpanel
        tmp:=GetObjectProp(MouseDownComponent,'myControl');
        //if tmp<>nil then showmessage('myControl is '+tmp.ClassName);
        NewParent:=TComponent(tmp);                //!!!!! this doesn't work here - still uses original parent
        //showmessage('set parent to '+NewParent.ClassName);
      end
      else if (MouseDownComponent.GetParentComponent is TWrapperPanel) then
      begin
        // is fine. Nothing to do.
      end;
    end;


    //if NewParent<>nil then showmessage('ac. new parent is '+NewParent.ClassName);
    if (IsPublishedProp(AComponentClass,'myNode') = false) then
    begin
      showmessage('WARNING:  This component is not cross-compilable to javascript - please select only from the available XComponents');
    end;
  end;
  Result:=true;
end;

//function ScanChildrenForControl(CurrentItem:TControl;ScreenObjectID:String):TControl;
//var FoundItem,TempItem:TControl;
//    TempControl :TControl;
//    i:integer;
//begin
//   FoundItem:= nil;
//   if CurrentItem is TWinControl then
//   begin
//     for i:=0 to TWinControl(CurrentItem).ControlCount - 1 do
//     begin
//        if FoundItem = nil then  // object has not been found so keep looking
//        begin
//          TempControl:= TWinControl(CurrentItem).Controls[i];
//          begin
//            if TempControl is TControl then   // it might be the control we are looking for..... or it may have children so search them for the object
//            begin
//              TempItem := TControl(TempControl);
//              if Trim(Uppercase(TempItem.name)) = Trim(Uppercase(ScreenObjectID))
//              then  FoundItem:= TempItem
//              else  TempItem:= ScanChildrenForControl(TempItem,ScreenObjectID);
//              if  TempItem<>nil
//              then begin if TempItem.name = ScreenObjectID then  FoundItem:= TempItem; end;
//            end;
//          end ;
//        end;
//     end;
//   end;
//   result:= FoundItem;
//end;

//function FindObjectByID(WithinForm:TForm;ScreenObjectID:String):TControl;
//var FoundItem, TempItem :TControl;
//    i:integer;
//begin
//   FoundItem:= nil;
//   TempItem:=ScanChildrenForControl(WithinForm,ScreenObjectID);
//   if TempItem <> nil
//   then
//   begin
//       FoundItem:= TempItem ;
//       //showmessage('Success .........>'+ScreenObjectID+'< has been found');
//   end
//   else
//     showmessage('Error in Utilities.FindObjectByID >'+ScreenObjectID+'< not found');
//
//   FindObjectByID:=FoundItem;
//end;

procedure InsertUnderParent(var MyComponent:TControl;MyParent:TWinControl;position:integer);
var
  bw,c:integer;
begin
  if MyComponent.Parent<>nil then
    MyComponent.Parent.RemoveControl(MyComponent);

  if position=-1 then
  begin
    //insert the new control as the LAST child
    c:=MyParent.ControlCount;
    MyParent.InsertControl(MyComponent,c)
  end
  else
  begin
    if MyParent.ControlCount>=position then
      MyParent.InsertControl(MyComponent,position)
    else
      MyParent.InsertControl(MyComponent,0);
  end;

  if MyParent is TWrapperPanel then
  begin
    if TWrapperPanel(MyParent).BorderStyle<>bsNone then        //!!!! is this still ok?
    begin
      bw:=MyParent.BorderWidth;
      MyComponent.BorderSpacing.Left:=bw;
      MyComponent.BorderSpacing.Top:=bw;
    end;
  end;

end;

function DeleteScreenObject(MyNode:TDataNode):string;  overload;
var
    ThisObject:TObject;
begin
   if (MyNode.ScreenObject<>nil)
   and (MyNode.ScreenObject<>MyNode) then
     if Assigned(MyNode.ScreenObject) then
       FreeAndNil(MyNode.ScreenObject);  // this will free all child objects at same time
   NilScreenObject(MyNode);              // this clears all the pointers to the (now freed) children
end;

function DeleteScreenObject(ThisObject:TObject):string; overload;
begin
   if ThisObject<>nil then
   begin
     FreeAndNil(ThisObject);
   end;
end;

procedure CheckParentIsXContainer(AControl:TControl);
// In Lazarus IDE, user can drag/drop a component in the design tree such that it becomes a child
// of a wrapperpanel which is not a container type.  Trap this here and re-parent if necessary.
var
   ParentIsContainer:Boolean;
begin
  if AControl.Parent<>nil then
  begin
    ParentIsContainer:=true;
    if not (AControl.Parent is TForm) then
    begin
      if IsPublishedProp(AControl.Parent,'IsContainer') then
        ParentIsContainer:=GetBooleanProperty(AControl.Parent,'IsContainer');

      if ParentIsContainer = false then
      begin
         //showmessage('re-parent '+AControl.ClassName);
         AControl.Parent:=(AControl.Parent).Parent;
      end;
    end;
  end;
end;

procedure ResetAlignment(AControl:TControl);
var
   ParentAlignChildrenVertical:Boolean;
   myAlignmt,myLabelPos:String;
   myControl:TControl;
   myLbl:TLabel;
begin
  if (AControl.Parent<>nil)
  and (AControl.Parent is TControl) then
  begin
    myAlignmt:='';
    if IsPublishedProp(AControl,'Alignment') then
    begin
      myAlignmt := GetStrProp(AControl,'Alignment');
      myLabelPos:='';
      if IsPublishedProp(AControl,'myLabelPos') then
        myLabelPos := GetStrProp(AControl,'myLabelPos');
      myControl:=nil;
      if IsPublishedProp(AControl,'myControl') then
        myControl := TControl(GetObjectProp(AControl,'myControl'));
      myLbl:=nil;
      if IsPublishedProp(AControl,'myLbl') then
        myLbl := TLabel(GetObjectProp(AControl,'myLbl'));

      setalignproperty(AControl,AControl.Parent);
      myAlignmt:=SortOutAlignmentAndLabelPos(AControl,myLbl,myControl,myAlignmt,myLabelPos);
      SetStrProp(AControl,'Alignment',myAlignmt);
    end;
    //showmessage('reset '+AControl.Name)
  end;
end;

procedure TXDesignerExtension.PersistentDeleting(APersistent: TPersistent);
var i:integer;
   datanode,ParentNode:TDataNode;
begin
  if (APersistent is TControl)
  and (APersistent = self.myWrapper) then
  begin
    if (self.myWrapper is TControl) then
    begin
      if IsPublishedProp(self.myWrapper,'myNode') then
      begin
        datanode:= TDataNode(GetObjectProp(self.myWrapper,'myNode'));
        if datanode<>nil then
        begin
          //ParentNode:=FindParentOfnode(SystemNodeTree,datanode.NodeName);
          ParentNode:=FindParentOfnode(SystemNodeTree,datanode);
          if ParentNode<>nil then
            ParentNode.RemoveChildNode(datanode);
          datanode.DeleteMe;
        end;
        SetObjectProp(self.myWrapper,'myNode',nil);
      end;
    end;
  end;
end;

procedure TXDesignerExtension.PersistentAdded(APersistent: TPersistent; Select: boolean);
var
   myParent:TWinControl;
   tmp:TObject;
begin
  //showmessage('persistentadded. APersistent is '+APersistent.ClassName+' myParent is '+myParent......
  if (APersistent is TWinControl)
  and (APersistent = self.myWrapper) then     // !!!!this is called for lots of components, when one is added  (???)
  begin
    if (self.myWrapper is TWinControl) then
    begin
      myParent:=self.myWrapper.Parent;

    end;
  end;
end;

procedure TXDesignerExtension.RefreshPropertyValues;
begin
//  //showmessage('refreshpropertyvalues '+self.myWrapper.ClassName);
//  //!!!! this is the nearest I could find to an event that would fire when a component is
//  // dragged to a new parent in the designer.
//  if (self.myWrapper is TControl) then
//    CheckParentIsXContainer(self.myWrapper);
end;

function HasProperty(aObject:TObject; aProperty:string):boolean;
var
  PropInfo:PPropInfo;
begin
  PropInfo := GetPropInfo(aObject.ClassInfo, aProperty);
  result := PropInfo<>nil;
end;

function GetBooleanProperty(aObject:TObject; aProperty:string):Boolean;
var
  PropValue:Boolean;
  PropInfo: PPropInfo;
begin
  PropValue:=true;
  if HasProperty(aObject,aProperty)  then
  begin
    PropInfo := GetPropInfo(aObject.ClassInfo, aProperty);
    if Assigned(PropInfo) then
      PropValue := GetPropValue(aObject, aProperty);
  end;
  result:=PropValue;
end;


Procedure SetBooleanProperty(aObject:TObject; aProperty:string; AValue : Boolean);
var
  PropInfo: PPropInfo;
begin
  if aObject<>nil then
  begin
    if HasProperty(aObject,aProperty)  then
    begin
      PropInfo := GetPropInfo(aObject.ClassInfo, aProperty);
      if Assigned(PropInfo) then
        SetPropValue(aObject, aProperty,AValue);
    end;
  end;
end;






//function LinkToStr(ALink:TXPropertyLink):string;
//begin
//  result:=ALink.TIObject.GetNamePath;
//  result:=result + AttribLinkDelimiter + ALink.TIPropertyName;
//end;

procedure IdentifyMenuNodes(ParentItem:TMenu);
// On startup (Lazarus FormCreate) this sets up all the parent>child relationships
// for the data nodes relating to components created in Lazarus IDE.
var
  parentnode,childnode:TDataNode;
  i,childcount:integer;
  childitem:TMenuItem;
begin
    if IsPublishedProp(ParentItem,'myNode') then
    begin
      parentnode:= TDataNode(GetObjectProp(ParentItem,'myNode'));
    end;
    if parentnode<>nil then
    begin
      SetLength(parentnode.ChildNodes,0);
      //if (ParentControl is TWinControl) then
      begin
        if (ParentItem is TMainMenu) then
          childcount:=TMainMenu(ParentItem).Items.Count
        else
          childcount:=TMenuItem(ParentItem).Count;
        for i:=0 to ChildCount-1 do
        begin
          if (ParentItem is TMainMenu) then
            childitem:=TMainMenu(ParentItem).Items[i]
          else
            childitem:=TMenuItem(ParentItem).Items[i];
          if IsPublishedProp(childitem,'myNode') then
          begin
            childnode:= TDataNode(GetObjectProp(childitem,'myNode'));

            AddChildToParentNode(parentnode,childnode,-1);
            //showmessage(parentnode.NodeName+' has child '+childnode.NodeName);
            IdentifyMenuNodes(TMenu(childitem));  //!!!!! not working for sub-levels
          end;
        end;
      end;
    end;
end;


procedure IdentifyChildNodes(ParentControl:TControl);
// On startup (Lazarus FormCreate) this sets up all the parent>child relationships
// for the data nodes relating to components created in Lazarus IDE.
var
  parentnode,childnode:TDataNode;
  i:integer;
  childcontrol:TControl;
  FormMainMenu:TMainMenu;
  tmp:string;
begin
    FormMainMenu:=nil;
    if ParentControl is TForm then
    begin
    //  parentnode:=FindDataNodeById(UIRootNode,ParentControl.Name,true);
      FormMainMenu:=TForm(ParentControl).Menu;
    end;
    if IsPublishedProp(ParentControl,'myNode') then
    begin
      parentnode:= TDataNode(GetObjectProp(ParentControl,'myNode'));
    end;

    if parentnode<>nil then
    begin
      SetLength(parentnode.ChildNodes,0);

      if FormMainMenu<>nil then         // !!!! what if there are other main menus defined???
      begin
        if IsPublishedProp(TObject(FormMainMenu),'myNode') then
        begin
          childnode:= TDataNode(GetObjectProp(TObject(FormMainMenu),'myNode'));
          AddChildToParentNode(parentnode,childnode,-1);
          IdentifyMenuNodes(FormMainMenu);
        end;
      end;

      if (ParentControl is TWinControl) then
      begin
        for i:=0 to TWinControl(ParentControl).ControlCount-1 do
        begin
          if IsPublishedProp(TWinControl(ParentControl).Controls[i],'myNode') then
          begin
            childcontrol:=TWinControl(ParentControl).Controls[i];
            childnode:= TDataNode(GetObjectProp(childcontrol,'myNode'));
            if childnode.NodeClass='NV' then
              tmp:='xxx';

            AddChildToParentNode(parentnode,childnode,-1);
            //showmessage(parentnode.NodeName+' has child '+childnode.NodeName);
            IdentifyChildNodes(childcontrol);
          end;
        end;
      end;

   end;
end;

function FindSizedParent(myComponent:TControl;hw:String):TControl;
var
  pr:TControl;
  parentFound:Boolean;
  chw:String;
begin
  pr:=MyComponent.Parent;

  // try to prevent endless resizing loop problems
  parentFound:=false;
  while (not parentFound)
  and (pr<>nil)
  and (pr.Parent<>nil)
  and (not (pr is TXForm)) do
  begin
    parentFound:=true;
    // candidate parent - check other stuff...
    if (((pr.AutoSize=true) and (pr.Align=alNone)) or (pr.Align=alClient)) then
      parentFound:=false
    else if ((HasProperty(pr,'myControl')) and (GetObjectProp(pr,'myControl')=myComponent)) then
      parentFound:=false
    else
    begin
      //if (hw='h')
      // bypass an unsized VBox for a height change
      // bypass an unsized HBox for a width change
      //and ((pr.ClassName='TXVBox') or (pr.ClassName='TXTabControl')) then
      //begin
      //  chw:=GetStrProp(pr,'ContainerHeight');
      //  if chw='' then parentFound:=false;
      //end
      //else if (hw='w')
      //and ((pr.ClassName='TXHBox') or (pr.ClassName='TXTabControl')) then
      //begin
      //  chw:=GetStrProp(pr,'ContainerWidth');
      //  if chw='' then parentFound:=false;
      //end;

      // try bypassing ANY unsized container
      if (hw='h') then
      begin
        if HasProperty(pr,'ContainerHeight') then
        begin
          chw:= GetStrProp(pr,'ContainerHeight');
          if chw='' then parentFound:=false;
        end;
      end
      else if (hw='w') then
      begin
        if HasProperty(pr,'ContainerWidth') then
        begin
          chw:= GetStrProp(pr,'ContainerWidth');
          if chw='' then parentFound:=false;
        end;
      end;
    end;
    if parentFound=false then
      pr:=pr.Parent;
  end;
(*
  while (pr<>nil)
  and (pr.Parent<>nil)
  and (not (pr is TXForm))
  and (((pr.AutoSize=true) and (pr.Align=alNone))
    or (pr.Align=alClient)
    // bypass the wrapperpanel container of this component
    or ((HasProperty(pr,'myControl')) and (GetObjectProp(pr,'myControl')=myComponent)
    // bypass an unsized VBox for a height change
    // bypass an unsized HBox for a width change
    or ((pr.ClassName='XVBox') and ( ......
    )
    )
    do
      pr:=pr.Parent;
      *)
  result:=pr;
end;


procedure CheckPercentageSizing(myComponent:TControl);
 var
     myTag:TComponentTag;
     tmpo:TObject;
     prw,prh:TControl;
     str,hh,ww:string;
     h,w,min,tmp,tmp2,tmp3:integer;
     parentHeight:integer;
begin
if (myComponent<>nil)
and (myComponent.Align<>alClient)
then
begin
  str:=myComponent.ClassName;
   myComponent.DisableAutoSizing;
   myTag:=nil;
   if myComponent.Tag>0 then
   begin
     // width and height requirements for this component are stored in its Tag
     myTag:=TComponentTag(myComponent.Tag);
     if (myTag<>nil) and (Assigned(myTag)) then
     begin
       if myComponent.ClassName = 'TXMemo' then
         hh:='';
        hh:=myTag.HH;
        ww:=myTag.WW;
        prw:=FindSizedParent(myComponent,'w');
        prh:=FindSizedParent(myComponent,'h');
        if (hh<>'') and (prh<>nil) then
        begin

             if FoundString(hh,'%')>0 then
             begin
             // Percentage
                  if myComponent.ClassName = 'TXMemo' then
                    h:=0;
                  Hh := myStringReplace(hh, '%', '',999,999);
//                tmp:=prh.ClientHeight;
//                tmp2:=prh.Height;
                if hh='' then h:=-1 else h:=strtoint(hh);
                if prh.ClientHeight>0 then
                begin
                  if h>0 then
                  begin
                    if (MyComponent is TWinControl) then
                    begin
                      parentHeight:=prh.Height;
                      if prh.ClassName='TXTabControl' then
                        parentHeight:=prh.ClientHeight;
                      min:=trunc(parentHeight*h/100) - (2*TWinControl(MyComponent).BorderWidth)
                                 - MyComponent.BorderSpacing.Top - MyComponent.BorderSpacing.Bottom - MyComponent.BorderSpacing.Around - 4;  // 4 is a fudge to prevent endless resizing loop problem
                      if min>0 then
                        MyComponent.Constraints.MinHeight:=min;
                    end
                    else
                    begin
                      min:=trunc(prh.Height*h/100)
                                 - MyComponent.BorderSpacing.Top - MyComponent.BorderSpacing.Bottom - MyComponent.BorderSpacing.Around - 4;  // 4 is a fudge to prevent endless resizing loop problem
                      if min>0 then
                        MyComponent.Constraints.MinHeight:=min;
                    end;
                    MyComponent.Constraints.MaxHeight:=MyComponent.Constraints.MinHeight;
                    MyComponent.Height:=MyComponent.Constraints.MinHeight;
                  end;
                end;

             end;
          end;

          if (ww<>'') and (prw<>nil) then
          begin
             if FoundString(ww,'%')>0 then
             begin
             // Percentage
                ww := myStringReplace(ww, '%', '',999,999);
                tmp:=prw.ClientWidth;
                tmp2:=prw.Width;
                tmp3:= prw.Constraints.MaxWidth;
                if ww='' then w:=-1 else w:=strtoint(ww);
                if prw.ClientWidth>0 then
                begin
                  if w>0 then
                  begin
                    if (myComponent is TWinControl) then
                    begin
                      min:=trunc(prw.Width*w/100) - (2*TWinControl(MyComponent).BorderWidth)
                                 - MyComponent.BorderSpacing.Left - MyComponent.BorderSpacing.Right - MyComponent.BorderSpacing.Around - 2;  // 2 is a fudge
                      if min>0 then
                        MyComponent.Constraints.MinWidth:=min;
                    end
                    else
                    begin
                      min:=trunc(prw.Width*w/100)
                                 - MyComponent.BorderSpacing.Left - MyComponent.BorderSpacing.Right - MyComponent.BorderSpacing.Around - 2;  // 2 is a fudge
                      if min>0 then
                        MyComponent.Constraints.MinWidth:=min;
                    end;
                    MyComponent.Constraints.MaxWidth:=MyComponent.Constraints.MinWidth;
                    MyComponent.Width:=MyComponent.Constraints.MinWidth
                  end;

                end;
             end;
          end;
      end;
   end;
   myComponent.EnableAutoSizing;

end;
end;

procedure CascadeResize(myComponent:TControl);
var
  i:integer;
begin
  if myComponent.Visible=true then
  begin
    CheckPercentageSizing(myComponent);
    if myComponent is TWinControl then
    begin
      for i:=0 to TWinControl(myComponent).ControlCount-1 do
      begin
        CascadeResize(TWinControl(myComponent).Controls[i]);
      end;

    end;
  end;
end;

function DoXFormCreated(myForm:TForm):TDataNode;
// Called from the FormCreate event for a form in an XComponents project (Form is of type TXForm).
begin
  TXForm(myForm).IsContainer:=true;
  result:=AddFormToNodetree(myForm);
  TXForm(myForm).myNode.IsDynamic:=false;
  // Go through the controls created at design time, and set up the data node parent/child links
  IdentifyChildNodes(myForm);
  // adjust the child order under UIRootNode so that any NV (non-visual) components are listed AFTER the form node.
  ResequenceNVNodes;


  // this bit sorts out alignment properties for everything after starting up project
  if myForm.ControlCount>0 then
    if IsPublishedProp(myForm.Controls[0],'myNode') then
      DoAlignmentAndLabelPosCascade(TWinControl(myForm.Controls[0]));

  {$ifdef Chromium}
  // cef issue - if this is not done at startup, creating new TChromium instances on popup forms causes errors
  if dummyChromium=nil then
    dummyChromium:=TChromium.Create(nil);
  {$endif}
end;

procedure DoFormResize(MyForm: TForm; RootObject:TWinControl);
var tmp:integer;
  xpad,ypad:integer;
begin
  tmp:=MyForm.ClientHeight;
  tmp:=MyForm.ClientWidth;

  xpad:=0;   //10;
  ypad:=0;   //20;

  if RootObject <> nil then
  begin
    if (RootObject.Constraints.MaxHeight <> MyForm.ClientHeight - ypad)
    or (RootObject.Constraints.MaxWidth <> MyForm.ClientWidth - xpad) then
    begin
      RootObject.Constraints.MaxHeight:=MyForm.ClientHeight - ypad;
      RootObject.Constraints.MinHeight:=MyForm.ClientHeight - ypad;
      RootObject.Constraints.MaxWidth:=MyForm.ClientWidth - xpad;
      RootObject.Constraints.MinWidth:=MyForm.ClientWidth - xpad;

      // cascade resize to other components ...
      CascadeResize(RootObject);

    end;
  end
  else
  begin
    showmessage('oops. take a look at FormResize');
  end;
end;

procedure FetchHeightWidth(NewNode:TDataNode; var h,w:string;WidthProp,HeightProp:string);
begin
  // get height and width
  if HeightProp<>'' then
    h:=NewNode.getAttribute(HeightProp,true).AttribValue
  else
    h:='';
  if WidthProp<>'' then
    w:=NewNode.getAttribute(WidthProp,true).AttribValue
  else
    w:='';
end;

procedure SetHeightWidth(myNode:TDataNode;var MySelf:TControl;WidthProp,HeightProp:string);
var
    myTag:TComponentTag;
    hh,ww:String;
    h,w:integer;
    ok:boolean;
begin
// This sets the component height and width if pixel size is specified.
// Percentage sizes are set dynamically in CheckPercentageSizing (called during on-resize events)

  FetchHeightWidth(myNode,hh,ww,WidthProp,HeightProp);
  mySelf.Tag:=-1;    // clear any prior percentage settings first

  if hh<>'' then
  begin
   ok:=TryStrToInt(hh,h);
   if (ok) or (FoundString(hh,'px')>0) then
   begin
     // Pixels
     Hh := myStringReplace(hh, 'px', '',999,999);
     if hh='' then h:=-1 else h:=strtoint(hh);
     if h>0 then
     begin
       MySelf.Constraints.MaxHeight:=h;
       MySelf.Constraints.MinHeight:=h;
       MySelf.Height:=h;
     end;
   end;
  end
  else
  begin
    MySelf.Constraints.MaxHeight:=0;
    MySelf.Constraints.MinHeight:=0;
  end;


  if ww<>'' then
  begin
     ok:=TryStrToInt(ww,w);
     if (ok) or (FoundString(ww,'px')>0) then
    begin
      Ww := myStringReplace(ww, 'px', '',999,999);
      if ww='' then w:=-1 else w:=strtoint(ww);
      if w>0 then
      begin
        MySelf.Constraints.MaxWidth:=w;
        MySelf.Constraints.MinWidth:=w;
        MySelf.Width:=w;
      end;
    end;
  end
  else
  begin
    MySelf.Constraints.MaxWidth:=0;
    MySelf.Constraints.MinWidth:=0;
  end;

  if (FoundString(hh,'%')>0) or (FoundString(ww,'%')>0) then
  begin
    // save the size attributes with the screen object for use in on-resize event handlers
    myTag:=TComponentTag.Create;
    myTag.HH:=hh;
    myTag.WW:=ww;
    MySelf.Tag:=WinSizeDependentInt(myTag);
    EventCode.HandleResizeComponent(MySelf);
    MySelf.OnResize:=@EventCode.HandleResizeComponent;
  end
  else
  begin
    mySelf.Tag:=-1;
    MySelf.OnResize:=nil;
  end;
end;

procedure ShowHideSelectedBorder(myNode:TDataNode;showborder:Boolean);
var
  i:integer;
  p:TWinControl;
  myComponent:TCustomControl;
  ItemIsContainer:Boolean;
begin
  myComponent:=TCustomControl(myNode.ScreenObject);
  // shift the borderwidth in or out.  This fires a paint cycle - see paint funcs for further rendering.
  // NB. cef issue. Do not change borderstyle on any Chromium component!
  if (showborder) then
  begin
    myComponent.BorderWidth:=glbBorderWidth;
  end
  else
  begin
    myComponent.BorderWidth:=0;
  end;

  // special case - TabSheet (have to refresh border settings on the parent tabcontrol as well)
// 29/5/2020 deleted this as it makes cef components disappear!
//  if myComponent.ClassName = 'TXTabSheet' then
//  begin
//    p:=myComponent.parent;
//    TCustomControl(myComponent.parent).BorderStyle:=bsSingle;
//    TCustomControl(myComponent.parent).BorderWidth:=glbBorderWidth;
//    TCustomControl(myComponent.parent).BorderStyle:=bsNone;
//    TCustomControl(myComponent.parent).BorderWidth:=0;
//  end;
end;


procedure PaintSelectedRectangle(myCanvas:TCanvas;PanelArea:TRect;SelectionBorderColor:TColor;IsSelected:boolean);
var
  R: TRect;
begin
  if (IsSelected) then
  begin
    // Draw a dotted rectangle
    R := PanelArea;
    InflateRect(R, -1, -1);
    myCanvas.Pen.Color := SelectionBorderColor;
    myCanvas.Pen.Width := glbBorderWidth;
    myCanvas.Pen.Style := psDash;
    myCanvas.Brush.Style := bsClear;
    myCanvas.Rectangle(R);
  end;
end;

procedure ClearAllAlignment(myLabel,myControl:TControl);
var
  p:TWinControl;
begin

  if myLabel<>nil then
  begin
    myLabel.Anchors:=[];
    myLabel.AnchorSideTop.Control := nil;
    myLabel.AnchorSideBottom.Control := nil;
    myLabel.AnchorSideLeft.Control := nil;
    myLabel.AnchorSideRight.Control := nil;
    myLabel.AnchorVerticalCenterTo(nil);
    myLabel.AnchorHorizontalCenterTo(nil);
    MyLabel.BorderSpacing.Around:=0;
  end;

  if myControl<>nil then
  begin
    p:=myControl.Parent;
  //  myControl.Parent:=nil;
    myControl.AnchorSideTop.Control := nil;
    myControl.AnchorSideBottom.Control := nil;
    myControl.AnchorSideRight.Control := nil;
    myControl.AnchorSideLeft.Control := nil;
    myControl.AnchorHorizontalCenterTo(nil);
    myControl.AnchorVerticalCenterTo(nil);
    myControl.Anchors := [];     //[akLeft, akTop];
 //   myControl.Parent:=p;
  end;

end;

function SortAlignList(Item1, Item2: Pointer): Integer;
var
  tmp1,tmp2:TControl;
  i,j:integer;
begin
  tmp1:=TControl(Item1);
  tmp2:=TControl(Item2);
  i:= tmp1.Parent.GetControlIndex(tmp1);
  j:= tmp1.Parent.GetControlIndex(tmp2);
  if i > j  then
    result:= 1
  else if i=j then
    result:=0
  else
    result:=-1;
end;

function mygetClipboardData(stringname:string):string;
begin
  ShowMessage('Loading new '+ stringname+' data from the clipboard');
  result :=  Clipboard.AsText;
end;

constructor TMyProjectEvents.Create;
begin
  inherited Create;
end;
destructor TMyProjectEvents.Destroy;
begin
  inherited Destroy;
end;
function TMyProjectEvents.OnProjectOpened(Sender: TObject;
                               AProject: TLazProject): TModalResult;
var
  j:integer;
  fm:TForm;
begin
  // fires after opening a new project in the IDE.
  // If this is a XComponents project (form exists; form contains a TXScrollBox) then
  // run through the alignment properties.
  //showmessage('sender is '+Sender.ClassName);   // sender is TMainIDE
  if Screen<>nil then
  begin
    //showmessage('form count is '+inttostr(Screen.FormCount));
    for j := Screen.FormCount - 1 downto 0 do
    begin
      fm := Screen.Forms[j];
      if fm.ControlCount>0 then
        if IsPublishedProp(fm.Controls[0],'myNode') then
          DoAlignmentAndLabelPosCascade(TWinControl(fm.Controls[0]));

    end;
  end;
end;
//function TMyProjectEvents.OnProjectBuilding(Sender: TObject): TModalResult;
//begin
//end;

procedure LabelLeftSettings(TargetControl:TControl;myLbl:TLabel);
begin
  if MyLbl<>nil then
  begin

    MyLbl.Anchors := [akRight, akTop];
    MyLbl.AnchorSideRight.Control := TargetControl;
    MyLbl.AnchorSideRight.Side := asrLeft;
    MyLbl.AnchorVerticalCenterTo(TargetControl);
    MyLbl.Alignment := taRightJustify;
    MyLbl.BorderSpacing.Right:=glbLabelSpacing;
end;
end;
procedure LabelRightSettings(TargetControl:TControl;myLbl:TLabel);
begin
  if MyLbl<>nil then
  begin

    MyLbl.Anchors := [akLeft, akTop];
    MyLbl.AnchorSideLeft.Control := TargetControl;
    MyLbl.AnchorSideLeft.Side := asrRight;
    MyLbl.AnchorVerticalCenterTo(TargetControl);
    MyLbl.Alignment := taLeftJustify;
    MyLbl.BorderSpacing.Left:=glbLabelSpacing;
  end;
end;
procedure LabelTopSettings(TargetControl:TControl;myLbl:TLabel);
begin
  if MyLbl<>nil then
  begin

    MyLbl.Anchors := [akBottom];
    MyLbl.AnchorSideBottom.Control := TargetControl;
    MyLbl.AnchorSideBottom.Side := asrTop;
    MyLbl.AnchorHorizontalCenterTo(TargetControl);
    MyLbl.Alignment := taLeftJustify;
    MyLbl.BorderSpacing.Bottom:=glbLabelSpacing;
  end;
end;
procedure LabelBottomSettings(TargetControl:TControl;myLbl:TLabel);
begin
  if MyLbl<>nil then
  begin

    MyLbl.Anchors := [akTop];
    MyLbl.AnchorSideTop.Control := TargetControl;
    MyLbl.AnchorSideTop.Side := asrBottom;
    MyLbl.AnchorHorizontalCenterTo(TargetControl);
    MyLbl.Alignment := taLeftJustify;
    MyLbl.BorderSpacing.Top:=glbLabelSpacing;
  end;
end;

(* Lazarus: childsizing alignment only works on controls with:-
Anchors=[akLeft,akTop]
AnchorSide[akLeft].Control=nil
AnchorSide[akTop].Control=nil
Align=alNone
*)
function SortOutAlignmentAndLabelPos(myself,myLbl,myControl:TControl; MyAlignment,MyLabelPos:string):String;
var
    ParentAlignChildrenVertical:Boolean;
    TheControl,TheAnchorTarget,myParent:TControl;
    myNode:TDataNode;
begin
  myParent:=myself.Parent;
  if (myself.Parent<>nil) and (myself.Name<>'') then
  begin
    myNode:=TDataNode(GetObjectProp(myself,'MyNode'));

    ParentAlignChildrenVertical := GetBooleanProperty(myself.Parent, 'AlignChildrenVertical');


    if myControl<>nil then
    begin
      TheControl:=TControl(myControl);
      TheAnchorTarget:=myself;
    end
    else
    begin
      TheControl:=myself;
      TheAnchorTarget:=myParent;
    end;

    MyAlignment:=AlignmentResetInvalidCombinations(MyAlignment,myNode.NodeName,myself.ClassName,ParentAlignChildrenVertical,(myself=TheControl),(myself.parent.ControlCount>1));

    TheControl.DisableAutoSizing;
    ClearAllAlignment(myLbl,TheControl);

    if (MyLabelPos = 'Right') then
      LabelRightSettings(TheControl,TLabel(myLbl))
    else if (MyLabelPos = 'Left') then
      LabelLeftSettings(TheControl,TLabel(myLbl))
    else if (MyLabelPos = 'Top') then
      LabelTopSettings(TheControl,TLabel(myLbl))
    else if (MyLabelPos = 'Bottom') then
      LabelBottomSettings(TheControl,TLabel(myLbl));


    if MyAlignment='Right' then
    begin
      if ParentAlignChildrenVertical=true then   // in a vertical list
      begin
        if (MyLabelPos = 'Left') or (myLbl=nil) then
        begin
          if TheControl=mySelf then TheControl.Align:=alNone;       // alTop sets anchors left,right and top, so clear it for a container
          TheControl.Anchors := [akRight, akTop];
          TheControl.AnchorSideRight.Control := TheAnchorTarget;
          TheControl.AnchorSideRight.Side := asrRight;

        end
        else if MyLabelPos = 'Right' then
        begin
          MyLbl.Anchors := [akTop, akRight];
          myLbl.AnchorSideRight.Control := TheAnchorTarget;
          myLbl.AnchorSideRight.Side := asrRight;
          MyLbl.AnchorVerticalCenterTo(TheControl);
          TheControl.Anchors := [akRight, akTop];
          TheControl.anchorsideRight.Control:=MyLbl;
          TheControl.anchorsideRight.Side:=asrLeft;


        end
        else if MyLabelPos = 'Top' then
        begin
          MyLbl.Anchors := [akTop, akRight];
          myLbl.AnchorSideTop.Control := TheAnchorTarget;
          myLbl.AnchorSideTop.Side := asrTop;
          MyLbl.AnchorHorizontalCenterTo(TheControl);
          TheControl.Anchors := [akRight, akTop];
          TheControl.anchorsideTop.Control:=MyLbl;
          TheControl.anchorsideTop.Side:=asrBottom;
          TheControl.AnchorSideRight.Control := TheAnchorTarget;
          TheControl.AnchorSideRight.Side := asrRight;
        end
        else if MyLabelPos = 'Bottom' then
        begin
          TheControl.Anchors := [akRight,akTop];
          TheControl.AnchorSideRight.Control := TheAnchorTarget;
          TheControl.AnchorSideRight.Side := asrRight;
        end;
      end;
    end
    else if MyAlignment = 'Left' then
    begin
      if ParentAlignChildrenVertical=true then   // in a vertical list
      begin
        if (myself=thecontrol) then SetAlignProperty(TheControl,myself.Parent);   //resets align to alTop
        if (MyLabelPos = 'Right') or (myLbl=nil) then
        begin
          TheControl.Anchors := [akLeft, akTop];
          TheControl.AnchorSideLeft.Control := TheAnchorTarget;
          TheControl.AnchorSideLeft.Side := asrLeft;
        end
        else if (MyLabelPos = 'Left') then
        begin
          TheControl.Anchors := [akLeft, akTop];
          TheControl.anchorsideLeft.Control:=MyLbl;
          TheControl.anchorsideLeft.Side:=asrRight;
          MyLbl.Anchors := [akTop, akLeft];
          myLbl.AnchorSideLeft.Control := TheAnchorTarget;
          myLbl.AnchorSideLeft.Side := asrLeft;
          MyLbl.AnchorVerticalCenterTo(TheControl);
        end
        else if (MyLabelPos = 'Top') then
        begin
          MyLbl.Anchors := [akTop, akLeft];
          myLbl.AnchorSideTop.Control := TheAnchorTarget;
          myLbl.AnchorSideTop.Side := asrTop;
          MyLbl.AnchorHorizontalCenterTo(TheControl);
          TheControl.Anchors := [akLeft, akTop];
          TheControl.anchorsideTop.Control:=MyLbl;
          TheControl.anchorsideTop.Side:=asrBottom;
          TheControl.AnchorSideLeft.Control := TheAnchorTarget;
          TheControl.AnchorSideLeft.Side := asrLeft;
        end
        else if (MyLabelPos = 'Bottom') then
        begin
          TheControl.Anchors := [akLeft, akTop];
          TheControl.AnchorSideLeft.Control := TheAnchorTarget;
          TheControl.AnchorSideLeft.Side := asrLeft;
        end;
      end;
    end
    else if MyAlignment = 'Centre' then
    begin
      TheControl.Anchors := [akLeft, akTop];
      TheControl.Align:=alNone;                       // Centering doesn't work if Align is set
      if ParentAlignChildrenVertical=true then
      begin
        if MyLabelPos='Top' then
        begin
          MyLbl.Anchors := [akTop, akLeft];
          myLbl.AnchorSideTop.Control := TheAnchorTarget;
          myLbl.AnchorSideTop.Side := asrTop;
          MyLbl.AnchorHorizontalCenterTo(TheControl);
          TheControl.Anchors := [akLeft, akTop];
          TheControl.anchorsideTop.Control:=MyLbl;
          TheControl.anchorsideTop.Side:=asrBottom;
        end;
         TheControl.AnchorHorizontalCenterTo(TheControl.Parent);
      end
      else
        begin
          if MyLabelPos='Left' then
          begin
            TheControl.Anchors := [akLeft, akTop];
            TheControl.anchorsideLeft.Control:=MyLbl;
            TheControl.anchorsideLeft.Side:=asrRight;
            MyLbl.Anchors := [akTop, akLeft];
            myLbl.AnchorSideLeft.Control := TheAnchorTarget;
            myLbl.AnchorSideLeft.Side := asrLeft;
            MyLbl.AnchorVerticalCenterTo(TheControl);
          end;
         TheControl.AnchorVerticalCenterTo(TheControl.Parent);
        end;
    end
    else if MyAlignment = 'Top' then   // in a horizontal list
    begin
      if ParentAlignChildrenVertical=false then
      begin
        if (myself=thecontrol) then SetAlignProperty(TheControl,myself.Parent);  //resets align to alLeft
        if (MyLabelPos = 'Right') or (myLbl=nil) then
        begin
          TheControl.Anchors := [akLeft,akTop];
          TheControl.AnchorSideTop.Control := TheAnchorTarget;
          TheControl.AnchorSideTop.Side := asrTop;
        end
        else if (MyLabelPos = 'Left') then
        begin
          TheControl.Anchors := [akLeft, akTop];
          TheControl.AnchorSideTop.Control := TheAnchorTarget;
          TheControl.AnchorSideTop.Side := asrTop;
          TheControl.anchorsideLeft.Control:=MyLbl;
          TheControl.anchorsideLeft.Side:=asrRight;
          MyLbl.Anchors := [akTop, akLeft];
          myLbl.AnchorSideLeft.Control := TheAnchorTarget;
          myLbl.AnchorSideLeft.Side := asrLeft;
          MyLbl.AnchorVerticalCenterTo(TheControl);

        end
        else if (MyLabelPos = 'Top') then
        begin
          MyLbl.Anchors := [akTop, akLeft];
          myLbl.AnchorSideTop.Control := TheAnchorTarget;
          myLbl.AnchorSideTop.Side := asrTop;
          MyLbl.AnchorHorizontalCenterTo(TheControl);
          TheControl.Anchors := [akLeft, akTop];
          TheControl.anchorsideTop.Control:=MyLbl;
          TheControl.anchorsideTop.Side:=asrBottom;
        end
        else if (MyLabelPos = 'Bottom') then
        begin
          TheControl.Anchors := [akLeft,akTop];
          TheControl.AnchorSideTop.Control := TheAnchorTarget;
          TheControl.AnchorSideTop.Side := asrTop;
        end;
      end;
    end
    else if MyAlignment = 'Bottom' then  // in a horizontal list
    begin
      if ParentAlignChildrenVertical=false then
      begin
        if (MyLabelPos = 'Right') or (myLbl=nil) then
        begin
          TheControl.Anchors := [akLeft,akBottom];
          TheControl.AnchorSideBottom.Control := TheAnchorTarget;
          TheControl.AnchorSideBottom.Side := asrBottom;
        end
        else if (MyLabelPos = 'Left') then
        begin
          TheControl.Anchors := [akLeft, akBottom];
          TheControl.AnchorSideBottom.Control := TheAnchorTarget;
          TheControl.AnchorSideBottom.Side := asrBottom;
          TheControl.anchorsideLeft.Control:=MyLbl;
          TheControl.anchorsideLeft.Side:=asrRight;
          MyLbl.Anchors := [akBottom, akLeft];
          myLbl.AnchorSideLeft.Control := TheAnchorTarget;
          myLbl.AnchorSideLeft.Side := asrLeft;
          MyLbl.AnchorVerticalCenterTo(TheControl);
        end
        else if (MyLabelPos = 'Top') then
        begin
          TheControl.Anchors := [akLeft,akBottom];
          TheControl.AnchorSideBottom.Control := TheAnchorTarget;
          TheControl.AnchorSideBottom.Side := asrBottom;
        end
        else if (MyLabelPos = 'Bottom') then
        begin
          MyLbl.Anchors := [akBottom, akLeft];
          myLbl.AnchorSideBottom.Control := TheAnchorTarget;
          myLbl.AnchorSideBottom.Side := asrBottom;
          MyLbl.AnchorHorizontalCenterTo(TheControl);
          TheControl.Anchors := [akLeft, akBottom];
          TheControl.anchorsideBottom.Control:=MyLbl;
          TheControl.anchorsideBottom.Side:=asrTop;
        end;
      end;
    end;

    TheControl.EnableAutoSizing;
  end;
  result:= myAlignment;
end;


procedure DoAlignmentAndLabelPosCascade(mySelf:TWinControl);
var
    i:integer;
begin
  //showmessage('DoAlignmentAndLabelPosCascade. mySelf is '+mySelf.Classname);
  // Bit of a Lazarus fudge here...
  // Re-evaluate Alignment and LabelPos properties for all 'X-container' children
  // Called at the end of the form Create (ie after all components have been loaded)
  if (myself is TWrapperPanel) then
    TWrapperPanel(myself).SortOutMyAlignmentAndLabelPos
  else if (myself is TXScrollBox) then
    TXScrollBox(myself).SortOutAlignment
  else if (myself is TXTabControl) then
    TXTabControl(myself).SortOutAlignment;

  For i:=0 to myself.ControlCount-1 do
  begin
    if IsPublishedProp(myself.Controls[0],'myNode') then
      DoAlignmentAndLabelPosCascade(TWinControl(myself.Controls[i]))
  end;
end;

function GetNameToApply(myNode:TDataNode;FormName,Oldname,NewName:String;componentState:TComponentState):String;
var
    ApplyName:String;
begin
  if (csDesigning in componentState) then
  begin
    if (OldName<>NewName)
    and (OldName<>'') then
    begin
      //showmessage('change name '+OldName+' to '+NewName);
      ApplyName:=SetUniqueName(myNode,FormName,NewName);
    end
    else
      ApplyName:=NewName;
  end
  else
    ApplyName:=NewName;
  result:=ApplyName;
end;

Procedure WriteToFile(TextFileName,FileContents:String);
var F: TextFile;
begin
  AssignFile(F, TextFileName);
  try
    {$I-}
    ReWrite(F);
    Write(F, FileContents);
    {$I+}
  finally
    CloseFile(F);
  end;
end;

function ReadFile(TextFileName:String):String;
var
    F:TextFile;
    fulltext,text:String;
begin
  fulltext:='';
  AssignFile(F, TextFileName);
  if FileExists(TextFileName) then
  begin
    Reset(F);

    // Load the file contents
    while not Eof(F) do
    begin
      ReadLn(F, text);
      fulltext:=fulltext+lineending+text;
    end;

    CloseFile(F);
  end;

  result:=fulltext;
end;

procedure WriteToLocalStore(KeyName,TheData:String);
begin
  if KeyName<>'' then
    WriteToFile(KeyName,TheData);
end;

function ReadFromLocalStore(KeyName:String):String;
var
    TheData:String;
begin
   if KeyName<>'' then
     TheData:=ReadFile(KeyName);
   result:=TheData;
end;

procedure ClearLocalStore(KeyName:String);
begin
  if KeyName<>'' then
    DeleteFile(KeyName);
end;

function ResourceToString(resName:string):String;
var
  Stream: TLazarusResourceStream;
  Lines:TStringList;
begin
  Stream := nil;
  try
    Lines:=TStringList.Create;
    //find the lazarus resource
    Stream := TLazarusResourceStream.Create(resName, nil);

    //save to a stringlist
    Lines.LoadFromStream(Stream);
    result:=Lines.Text;
   finally
     Stream.Free;
     Lines.Free;
   end;
end;

procedure ResourceToFile(resName,fileName:string);
var
  str:String;
begin
  str:=ResourceToString(resName);
  WriteToFile(fileName,str);
end;


initialization

  myProjectEvents:=TMyProjectEvents.Create;

end.
(*  ...... for reference, in case of need......
{$IFDEF UNIX              }    // --UNIX--
    {$IFDEF BEOS          }    // BEOS
    {$ENDIF}
    {$IFDEF SUNOS         }    // SUNOS
    {$ENDIF}
    {$IFDEF LINUX           }
      {$IFDEF ANDROID       }  // ANDROID
      {$ELSE}               }  // LINUX
      {$ENDIF}
    {$ENDIF}// end Linux
  {$IFDEF BSD             }    // --BSD--
    {$IFDEF FREEBSD       }    // FREEBSD
    {$ENDIF}
    {$IFDEF NETBSD        }    // NETBSD
    {$ENDIF}
    {$IFDEF DARWIN        }    // Macintosh see also MAC below
    {$ENDIF}
  {$ENDIF}// end Bsd
{$ENDIF}// end Unix

{$IFDEF WINDOWS       }        // --WINDOWS--
  {$IFDEF WIN32       }        // WIN32
  {$ENDIF}
  {$IFDEF WIN64       }        // WIN64
  {$ENDIF}
  {$IFDEF WINCE       }        // WINCE
  {$ENDIF}

{$ENDIF}// end Windows

{$IFDEF OS2         }          // --OS2--
  {$IFDEF EMX         }        // EMX
  {$ELSE}                      // OS2
  {$ENDIF}
{$ENDIF}// end os2
                               // --OTHER--
{$IFDEF GO32V2    }            // GO32V2
{$ENDIF}
{$IFDEF AMIGA     }            // Classic Amiga
{$ENDIF}
{$IFDEF ATARI     }            // Atari TOS
{$ENDIF}
{$IFDEF MAC       }            // Classic Macintosh
{$ENDIF}
{$IFDEF PALMOS    }            // PalmOS
{$ENDIF}
*)

