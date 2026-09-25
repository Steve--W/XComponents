(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XCheckListBox;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface
uses
  Classes, SysUtils,TypInfo, NodeUtils,StringUtils,Math,
  {$ifndef JScript}
  LResources, Forms, Controls, StdCtrls, CheckLst, Graphics, Dialogs, ExtCtrls, Propedits,RTTICtrls,
  LazsUtils, Events,
  {$else}
  HTMLUtils,
  {$endif}
  WrapperPanel;


type
  TXCheckListBox = class(TWrapperPanel)
  private
    { Private declarations }
    {$ifndef JScript}
    fHandleClick:TEventHandler;
    procedure CheckListBoxclick(Sender:TObject);
    //procedure CheckListBoxChange(Sender:TObject);
    procedure myOnExit(Sender: TObject) ;
    {$endif}

    procedure SetMyEventTypes;

    function GetItemsChecked:String;
    function GetEnabled:Boolean;
    function GetItems:String;
    {$ifndef JScript}
    function GetContainerWidth:string;
    function GetContainerHeight:string;
    function GetBgColor:TColor;
    {$endif}

    procedure SetItemsChecked(AValue:String);
    procedure SetEnabled(AValue:Boolean);
    procedure SetItems(AValue:String);
    {$ifndef JScript}
    procedure SetContainerHeight(AValue:string);
    procedure SetContainerWidth(AValue:string);
    procedure SetBgColor(AValue:TColor);
    {$endif}

    {$ifndef JScript}
    procedure ResizeCheckListBoxToContent;
    {$endif}
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
    property ItemsChecked: String read GetItemsChecked write SetItemsChecked;
    property Items: String read GetItems write SetItems;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property ContainerWidth: String read GetContainerWidth write SetContainerWidth;
    property ContainerHeight: String read GetContainerHeight write SetContainerHeight;


    {$ifndef JScript}
    property BgColor: TColor read GetBgColor write SetBgColor;

    // Events to be visible in Lazarus IDE
    property HandleClick: TEventHandler read FHandleClick write FHandleClick;
    {$endif}
  end;

  {$ifdef JScript}
  function CreateCheckBoxes(myNode:TDataNode;ItemList,wrapperid:String):string;
  {$endif}

  {$ifndef JScript}
  procedure Register;
  {$endif}


implementation

const MyNodeType='TXCheckListBox';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXCheckListBox.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
  //MyEventTypes.Add('Change');
end;

{$ifndef JScript}
procedure Register;
begin
  {$I xcheckbox_icon.lrs}
  RegisterComponents('XComponents',[TXCheckListBox]);

  // inherited from TWrapperPanel, not required here
  RegisterPropertyEditor(TypeInfo(String), TXCheckListBox, 'ContainerHeight', THiddenPropertyEditor);

  // suppress some of the link properties
//  RegisterPropertyEditor(TypeInfo(TAliasStrings), TXPropertyLink, 'AliasValues', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(String), TXPropertyLink, 'TIElementName', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(TPropertyLinkOptions), TXPropertyLink, 'Options', THiddenPropertyEditor);
end;

constructor TXCheckListBox.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXCheckListBox.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXCheckListBox.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  self.BorderSpacing.Around:=glbBorderSpacing;

  myControl:=TCheckListBox.Create(self);
  myControl.Parent:=self;

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];

  //TCheckListBox(myControl).OnEditingDone:=@self.myeditingDone;  /// use onExit??
  TCheckListBox(myControl).OnExit:=@self.myOnExit;
  myControl.OnClick:=@self.CheckListBoxClick;
  TCheckListBox(myControl).AllowGrayed:=false;
  TCheckListBox(myControl).Columns:=1;
  //TCheckListBox(myControl).Align:=alClient;
  //TCheckListBox(myControl).AutoSize:=true;

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
  NewNode:=CreateDynamicLazWidget('TXCheckListBox',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;

procedure TXCheckListBox.myOnExit(Sender: TObject) ;
var
  ss:string;
  i:integer;
begin
  if not (csDesigning in componentState) then
  begin
    // rebuild the ItemsChecked list
    ss := '[';
    for i:=0 to TCheckListBox(myControl).Count-1 do
    begin
      if i>0 then
        ss:=ss+',';
      if TCheckListBox(myControl).Checked[i] = true then
        ss := ss + '"True"'
      else
        ss := ss + '"False"';
    end;
    ss:=ss+']';
    self.ItemsChecked:=ss;
  end;
end;

//procedure TXCheckListBox.CheckListBoxChange(Sender: TObject) ;
//var
//  itemchecked:array of boolean;
//begin
//  //itemchecked:=MyBooltostr(TCheckListBox(Sender).checked);
// // CallHandleEvent('Change',itemchecked,Sender);
//
//end;

procedure TXCheckListBox.CheckListBoxClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
  begin
    //itemschecked:=TCheckListBox(Sender).checked;
    //self.Checked:=TCheckListBox(Sender).checked;
    CallHandleEvent('Click',self.myNode.NodeName,self);
  end;
end;


{$else}

constructor TXCheckListBox.Create(MyForm:TForm;NodeName,NameSpace:String);
begin
  inherited Create(NodeName,NameSpace);
  //asm console.log('TXCheckListBox.Create '+NodeName); end;
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=false;

  SetNodePropDefaults(self,myDefaultAttribs);

end;

function CreateCheckBoxes(myNode:TDataNode;ItemList,wrapperid:String):string;
var
  OnChangeString, ItemValue,Enabled,Checked,myName,NameSpace:string;
begin
  Enabled:= MyNode.getAttribute('Enabled',true).AttribValue;
  Checked:= MyNode.getAttribute('Checked',true).AttribValue;
  NameSpace:=myNode.nameSpace;
  myName:=myNode.NodeName;
  //OnChangeString:='onchange="if (this.checked) {pas.NodeUtils.SetInterfaceProperty('''+myName+''','''+NameSpace+''',''ItemValue'',this.value);' +
  //                        'pas.Events.handleEvent(null,''Change'','''+myName+''','''+NameSpace+''',''';

  asm
  try{
    var ReadOnlyString = '';
    if (Enabled=='false') { ReadOnlyString = ' readonly ';}

    var HTMLString='<div id="'+wrapperid+'Box" class=VBoxNoStretch >';
    var wrapperid = NameSpace+myName;
    var itemlistarray=JSON.parse(ItemList);
    for (var i=0; i<itemlistarray.length; i++){
       var currentitemstring = itemlistarray[i];
       HTMLString = HTMLString +'<input type="checkbox"  ' + ReadOnlyString
                             //  +' id="'+wrapperid+currentitemstring+'" '
                               +' name='+wrapperid+' '
                            //   + OnChangeString+currentitemstring+quot+');}" '
                               +' value="'+currentitemstring+'" '
                               +'>'+currentitemstring+'<Br>';
     }
     HTMLString = HTMLString + '</div>'
     return HTMLString;
  }
  catch(err) { alert(err.message+'  in XCheckListBox.CreateCheckBoxes');}
  end;

end;
function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  Checked,Enabled,LabelText,LabelPos,Items:string;
  OnChangeString, OnClickString, OnPasteString:String;
begin
  Checked:= MyNode.getAttribute('Checked',true).AttribValue;
  Items:= MyNode.getAttribute('Items',true).AttribValue;
  LabelText:= MyNode.getAttribute('LabelText',true).AttribValue;
  Enabled:= MyNode.getAttribute('Enabled',true).AttribValue;

  //OnClickString:='onclick="if (1==1) {' +
   //                       'pas.NodeUtils.SetInterfaceProperty('''+ScreenObjectName+''','''+NameSpace+''',''Checked'',this.checked.toString());' +
   //                       'event.stopPropagation(); ' +
   //                       'pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''','''+NameSpace+''', this.checked.toString());}"';

  asm
    try{
    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,NameSpace,$impl.MyNodeType,position);
    wrapper.style.display = 'flex';
    wrapper.style.overflow = 'scroll';
    var goright =  'flex-e'+'nd';

    var HTMLString='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var wrapperid =  NameSpace+ScreenObjectName;
    var MyObjectName=wrapperid+'Contents';

    HTMLString = '<div  id='+MyObjectName+' style="display: inline-block;height:100%-20px;width:100%;" '
                 //+ OnClickString
                 +' >  ';

                 var ReadOnlyString = '';
    if (Enabled=='False') { ReadOnlyString = ' readonly ';}

    var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'">'+LabelText+'</label>';

    var Checkstring = '';
    if (Checked == 'true'){Checkstring = 'checked'};

    var CheckBoxListString =  $mod.CreateCheckBoxes(MyNode,Items,wrapperid);

    HTMLString = HTMLString + labelstring+'<div>'+CheckBoxListString+'</div>';
    HTMLString = HTMLString + '</div>'

    var wrapper=document.getElementById(wrapperid);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);

    pas.HTMLUtils.FixHeightToLineHeight(ScreenObjectName);
  }
  catch(err) { alert(err.message+'  in XCheckListBox.CreateWidget');}

end;

  MyNode.ScreenObject:=MyNode;
  RefreshComponentProps(myNode);

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXCheckListBox.Create(MyForm,NodeName,NameSpace));
end;


{$endif}


function TXCheckListBox.GetItemsChecked:String;
begin
  result:=MyNode.getAttribute('ItemsChecked',true).AttribValue;
end;
function TXCheckListBox.GetItems:String;
begin
  result:=MyNode.getAttribute('Items',true).AttribValue;
end;
function TXCheckListBox.GetEnabled:Boolean;
begin
  result:=MyStrToBool(MyNode.getAttribute('Enabled',true).AttribValue);
end;

procedure TXCheckListBox.SetItemsChecked(AValue:String);
var
  ss:TStringList;
  i:integer;
begin
  myNode.SetAttributeValue('ItemsChecked',AValue);
  // convert string (like [true,false]) to a boolean array
  if AValue = '[]' then
  begin
    {$ifndef JScript}
    for i := 0 to TCheckListBox(myControl).Count-1 do
        TCheckListBox(myControl).Checked[I] := False;
    {$else}
    {$endif}
  end
  else
  begin
    ss := JSONStringToStringList(AValue);
    {$ifndef JScript}
    if TCheckListBox(myControl).Count < ss.Count then
    begin
      for i:= TCheckListBox(myControl).Count to ss.Count-1 do
        TCheckListBox(myControl).Items.Add('item'+intToStr(i));
    end;
    for i:=0 to ss.Count-1 do
      TCheckListBox(myControl).Checked[i]:= MyStrToBool(ss[i]);
    {$else}
    asm
      var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
      if (ob!=null) {
        ob.checked=AValue;  }
    end;
    {$endif}

  end;
end;

{$ifndef JScript}
procedure TXCheckListBox.ResizeCheckListBoxToContent;
var
  I, MaxWidth, TotalHeight, ParentWidth, ParentHeight: Integer;
  mybox:TCheckListBox;
begin
  if not (csDesigning in componentState) then
  begin
    mybox := TCheckListBox(myControl);
    //make sure instantiation is completed first
    if (mybox = nil) or (mybox.Parent = nil) then
        Exit;
    if mybox.Parent.Parent = nil then exit;

    begin
      MaxWidth := 0;
      mybox.Canvas.Font := mybox.Font;

      // Calculate Width based on the longest item
      for I := 0 to mybox.Items.Count - 1 do
      begin
        MaxWidth := Max(MaxWidth, mybox.Canvas.TextWidth(mybox.Items[I]));
      end;

      // Calculate Height based on item count multiplied by row height
      // AListBox.ItemHeight defaults to 0 if system-managed, use Canvas text height + padding
      if mybox.ItemHeight > 0 then
        TotalHeight := mybox.Items.Count * mybox.ItemHeight
      else
        TotalHeight := mybox.Items.Count * (mybox.Canvas.TextHeight('Wg') + 4);

      // Add padding for checkboxes and borders
      MaxWidth := MaxWidth + 40;
      TotalHeight := TotalHeight + 6;

      ParentHeight := TCustomPanel(self).Height;
      ParentWidth := TCustomPanel(self).Width;

      // Handle Width and Horizontal Scrollbar
      mybox.Width := ParentWidth;
      if MaxWidth > ParentWidth then
      begin
        // Forcing ScrollWidth ensures the horizontal scrollbar triggers
        mybox.ScrollWidth := ParentWidth;
      end
      else
      begin
        //mybox.Width := MaxWidth;
        mybox.ScrollWidth := 0; // Clears the horizontal scrollbar if not needed
      end;

      // Handle Height
      // Vertical scrollbars appear automatically in Lazarus if Height < content height
      if TotalHeight > ParentHeight then
        mybox.Height := ParentHeight
      else
        mybox.Height := TotalHeight;
    end;
  end;
end;

function TXCheckListBox.GetBgColor:TColor;
begin
  result:=HexRGBToColor(myNode.GetAttribute('BgColor',true).AttribValue);
end;
procedure TXCheckListBox.SetBgColor(AValue:TColor);
var str:string;
begin
  if myNode<>nil then
  begin
    str:= ColorToHexRGB(AValue);
    myNode.SetAttributeValue('BgColor',str,'Color');
  end;

  TCheckListBox(myControl).Color:=AValue;
end;
function TXCheckListBox.GetContainerWidth:string;
begin
  result:=myNode.GetAttribute('ContainerWidth',true).AttribValue;
end;
function TXCheckListBox.GetContainerHeight:string;
begin
  result:=myNode.GetAttribute('ContainerHeight',true).AttribValue;
end;
procedure TXCheckListBox.SetContainerWidth(AValue:string);
begin
  myNode.SetAttributeValue('ContainerWidth',AValue);
  SetHeightWidth(self.myNode,TControl(self),'ContainerWidth','ContainerHeight');
end;

procedure TXCheckListBox.SetContainerHeight(AValue:string);
begin
  myNode.SetAttributeValue('ContainerHeight',AValue);
  SetHeightWidth(self.myNode,TControl(self),'ContainerWidth','ContainerHeight');
end;
{$endif}


procedure TXCheckListBox.SetItems(AValue:String);
var
  ss:TStrings;
begin
  myNode.SetAttributeValue('Items',AValue);
  ss := JSONStringToStringList(AValue);
  {$ifndef JScript}
  TCheckListBox(myControl).Items.Assign(ss);
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
        //console.log('looking for '+ this.NameSpace+this.NodeName+'Box')
        var wrapper = document.getElementById(this.NameSpace+this.NodeName+'Contents');
        //console.log('found box'+box.id);
        wrapper.innerHTML = '';
        wrapper.innerHTML = $mod.CreateCheckBoxes(this.myNode,AValue,wrapper.id);
        }
  end;
  {$endif}
  myNode.SetAttributeValue('ItemsChecked','[]');
  {$ifndef JScript}
  //TCheckListBox(myControl).AdjustSize;  // nope
  ResizeCheckListBoxToContent;
  {$endif}
end;


procedure TXCheckListBox.SetEnabled(AValue:Boolean);
begin
  myNode.SetAttributeValue('Enabled',myBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  TCheckListBox(myControl).Enabled:=not AValue;
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
  // this is the set of node attributes that each XCheckListBox instance will have.
  AddWrapperDefaultAttribs(myDefaultAttribs);
  AddDefaultAttribute(myDefaultAttribs,'BgColor','Color','#FFFFFF','',false);
  AddDefaultAttribute(myDefaultAttribs,'ContainerWidth','String','200px','',false);
  AddDefaultAttribute(myDefaultAttribs,'ContainerHeight','String','200px','',false);
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','Top','',true);
  AddDefaultAttribute(myDefaultAttribs,'LabelText','String','CheckList','',false);
  AddDefaultAttribute(myDefaultAttribs,'Enabled','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'Items','String','["a","b","c"]','',false);
  AddDefaultAttribute(myDefaultAttribs,'ItemsChecked','String','["True","False","True"]','',true);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  //AddAttribOptions(MyNodeType,'LabelPos',LabelPosOptions);
  {$ifndef JScript}
  RegisterClass(TXCheckListBox);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}
  //SuppressDesignerProperty(MyNodeType,'LabelPos');
  //SuppressDesignerProperty(MyNodeType,'ContainerHeight');
  //SuppressDesignerProperty(MyNodeType,'ContainerWidth');
end.

