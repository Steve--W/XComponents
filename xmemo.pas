(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XMemo;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, TypInfo, NodeUtils, StringUtils,
  {$ifndef JScript}
  LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, Propedits,RTTICtrls,
  LazsUtils, Events,
  {$else}
  HTMLUtils,
  {$endif}
   WrapperPanel;


type
  TXMemo = class(TWrapperPanel)
  private
    { Private declarations }
    {$ifndef JScript}
    fHandleClick:TEventHandler;
    fHandleChange:TEventHandler;

    procedure MemoClick(Sender:TObject);
    procedure MemoChange(Sender: TObject);
    {$endif}

    procedure SetMyEventTypes;


    function GetItemValue:string;
    function GetReadOnly:Boolean;
    function GetIncludeTextInSave:Boolean;
    function GetMemoWidth:string;
    function GetMemoHeight:string;

    procedure SetItemValue(AValue:string);
    procedure SetReadOnly(AValue:Boolean);
    procedure SetIncludeTextInSave(AValue:Boolean);
    procedure SetMemoWidth(AValue:string);
    procedure SetMemoHeight(AValue:string);

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
    property IncludeTextInSave: Boolean read GetIncludeTextInSave write SetIncludeTextInSave;
    property MemoHeight: String read GetMemoHeight write SetMemoHeight;
    property MemoWidth: String read GetMemoWidth write SetMemoWidth;

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

const MyNodeType='TXMemo';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXMemo.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
  MyEventTypes.Add('Change');
  {$ifdef JScript}
  MyEventTypes.Add('MemoPaste');
  {$endif}
end;

{$ifndef JScript}

procedure Register;
begin
  {$I xmemo_icon.lrs}
  RegisterComponents('XComponents',[TXMemo]);

  // inherited from TWrapperPanel, not required here
  RegisterPropertyEditor(TypeInfo(TColor), TXMemo, 'BgColor', THiddenPropertyEditor);

  // suppress some of the Link properties
//  RegisterPropertyEditor(TypeInfo(TAliasStrings), TXPropertyLink, 'AliasValues', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(String), TXPropertyLink, 'TIElementName', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(TPropertyLinkOptions), TXPropertyLink, 'Options', THiddenPropertyEditor);
end;

constructor TXMemo.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXMemo.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXMemo.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  self.BorderSpacing.Around:=glbBorderSpacing;

  myControl:=TMemo.Create(self);
  myControl.Parent:=self;

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];
  TMemo(myControl).OnEditingDone:=@self.myeditingDone;

  TMemo(myControl).OnExit:=@self.MemoChange;
  myControl.OnClick:=@self.MemoClick;

  TMemo(myControl).WordWrap:=true;
  TMemo(myControl).ScrollBars:=ssVertical;

  ////TMemo(myControl).Font.Name:='Courier';

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
  NewNode:=CreateDynamicLazWidget('TXMemo',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;

procedure TXMemo.MemoClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;

procedure TXMemo.MemoChange(Sender: TObject) ;
 var
    Memo: TMemo ;
 begin
    Memo := TMemo(sender) ;
    self.ItemValue:=Memo.text;
    CallHandleEvent('Change',Memo.text,Sender);
 end;

//procedure TXMemo.LinkLoadFromProperty(Sender: TObject);
//begin
//  if Sender=nil then ;
//  if (Link.Editor=nil) then exit;
//  inherited  LinkLoadFromProperty(Sender);
//
//  self.ItemValue:=Link.GetAsText;
//
//end;
//
//procedure TXMemo.LinkSaveToProperty(Sender: TObject);
//begin
//  if Sender=nil then ;
//  if Link.Editor=nil then exit;
//  Link.SetAsText(TMemo(myControl).Text);
//
//end;

procedure TXMemo.SetMemoWidth(AValue:string);
 var
   tc:TControl;
 begin
  tc:=self.myControl;
  myNode.SetAttributeValue('MemoWidth',AValue);
  SetHeightWidth(self.myNode,tc,'MemoWidth','MemoHeight');
  tc.Update;
  tc.Invalidate;
end;

procedure TXMemo.SetMemoHeight(AValue:string);
 var
   tc:TControl;
 begin
  tc:=self.myControl;
  myNode.SetAttributeValue('MemoHeight',AValue);
  SetHeightWidth(self.myNode,tc,'MemoWidth','MemoHeight');
  tc.Update;
  tc.Invalidate;
end;

{$else}

constructor TXMemo.Create(MyForm:TForm;NodeName,NameSpace:String);
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
  LabelText:string;
  ReadOnly:Boolean;
  OnChangeString, OnClickString, OnPasteString:String;
begin
  LabelText:= MyNode.getAttribute('LabelText',true).AttribValue;
  ReadOnly:= StrToBool(MyNode.getAttribute('ReadOnly',true).AttribValue);

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''','''+NameSpace+''', this.value);" ';
  OnChangeString:= 'onchange="pas.NodeUtils.SetInterfaceProperty('''+ScreenObjectName+''','''+NameSpace+''',''ItemValue'',this.value); '+
                             'pas.Events.handleEvent(null,''Change'','''+ScreenObjectName+''','''+NameSpace+''', this.value, ''ItemValue'');" ';
  OnPasteString:= 'onpaste="var pdata=(event.clipboardData || window.clipboardData).getData(''text'');' +
                             'pas.Events.handleEvent(null,''MemoPaste'','''+ScreenObjectName+''','''+NameSpace+''', pdata);" ';

  asm
    try{

    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,NameSpace,$impl.MyNodeType,position);

    var HTMLString='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var wrapperid = NameSpace+ScreenObjectName;
    var MyObjectName=wrapperid+'Contents';

    var ReadOnlyString = '';
    if (ReadOnly==true) { ReadOnlyString = ' readonly ';}

    var Pastetypestring="'MemoPaste'";
    var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'">'+LabelText+'</label>';


    var MemoString ='<textarea  id='+MyObjectName+' '+
                        OnPasteString +
                        OnClickString +
                        OnChangeString +
                        ' class="widgetinner '+wrapperid+'" ' +
                        ' style="display:inline-block; padding:1px; height:100%; width:100%;"  >'+
                       '</textarea> ';

    HTMLString = MemoString+labelstring;

    var wrapper=document.getElementById(wrapperid);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);

  }
  catch(err) { alert(err.message+'  in XMemo.CreateXMemo');}

end;

  MyNode.ScreenObject:=MyNode;
  RefreshComponentProps(myNode);

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXMemo.Create(MyForm,NodeName,NameSpace));
end;

//procedure TXMemo.LinkLoadFromProperty(Sender: TObject);
//begin
//  inherited  LinkLoadFromProperty(Sender);
//end;
//
//procedure TXMemo.LinkSaveToProperty(Sender: TObject);
//begin
//  if Sender=nil then ;
//  if Link=nil then exit;
//  if Link.TIObject=nil then exit;
////  showmessage('linksavetoproperty. '+Link.TIPropertyName+' '+self.ItemValue);
//
//  SetStringProp(Link.TIObject,Link.TIPropertyName,self.ItemValue);
//end;


procedure TXMemo.SetMemoWidth(AValue:string);
begin
  //showmessage('memo width='+AValue);
  myNode.SetAttributeValue('MemoWidth',AValue);
  asm
  var ob = document.getElementById(this.NameSpace+this.NodeName);
  //  if (ob==null) {alert(this.NodeName+'  not found');}
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
  end;
end;

procedure TXMemo.SetMemoHeight(AValue:string);
begin
  myNode.SetAttributeValue('MemoHeight',AValue);
  asm
  var ob = document.getElementById(this.NameSpace+this.NodeName);
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'H',AValue);
  end;
end;

{$endif}


function TXMemo.GetItemValue:string;
begin
  result:=MyNode.getAttribute('ItemValue',true).AttribValue;
end;
function TXMemo.GetReadOnly:Boolean;
begin
  result:=MyStrToBool(MyNode.getAttribute('ReadOnly',true).AttribValue);
end;
function TXMemo.GetIncludeTextInSave:Boolean;
begin
  result:=MyStrToBool(MyNode.getAttribute('IncludeTextInSave',true).AttribValue);
end;
function TXMemo.GetMemoHeight:string;
begin
  result:=MyNode.getAttribute('MemoHeight',true).AttribValue;
end;
function TXMemo.GetMemoWidth:string;
begin
  result:=MyNode.getAttribute('MemoWidth',true).AttribValue;
end;

procedure TXMemo.SetItemValue(AValue:string);
var
  l:integer;
begin
  myNode.SetAttributeValue('ItemValue',AValue);
  {$ifndef JScript}
  l:=length(AValue);
  if l > 1000000 then
    TMemo(myControl).Text:='<<.....text too long to edit here....>>'
  else
    TMemo(myControl).Text:=AValue;   //!!!! hanging up when text is very long...
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
       ob.value=AValue;  }
  end;
  //LinkSaveToProperty(self);
  {$endif}
end;

procedure TXMemo.SetReadOnly(AValue:Boolean);
begin
  myNode.SetAttributeValue('ReadOnly',myBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  TMemo(myControl).ReadOnly:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
      ob.readOnly = AValue  }
  end;
  {$endif}
end;

procedure TXMemo.SetIncludeTextInSave(AValue:Boolean);
begin
  myNode.SetAttributeValue('IncludeTextInSave',myBoolToStr(AValue),'Boolean');
end;

begin
  // this is the set of node attributes that each TXMemo instance will have.
  AddWrapperDefaultAttribs(myDefaultAttribs);
  AddDefaultAttribute(myDefaultAttribs,'ContainerWidth','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'ContainerHeight','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'MemoWidth','String','400','',false);
  AddDefaultAttribute(myDefaultAttribs,'MemoHeight','String','200','',false);
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','Top','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelText','String','Memo Box','',false);
  AddDefaultAttribute(myDefaultAttribs,'ReadOnly','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'ItemValue','String','....text....','',false);
  AddDefaultAttribute(myDefaultAttribs,'IncludeTextInSave','Boolean','True','If false, the memo contents will be excluded from saved system data',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  AddExclusionAttribToTable(MyNodeType,'IncludeTextInSave','ItemValue');

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  AddAttribOptions(MyNodeType,'LabelPos',LabelPosOptions);
  {$ifndef JScript}
  RegisterClass(TXMemo);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}
  SuppressDesignerProperty('TXMemo','BgColor');
  SuppressDesignerProperty(MyNodeType,'ContainerHeight');
  SuppressDesignerProperty(MyNodeType,'ContainerWidth');
end.
