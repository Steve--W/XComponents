(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XStore;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface
uses
  Classes, SysUtils, Types,NodeUtils,StringUtils,
  {$ifndef JScript}
  Forms, Controls, StdCtrls, LResources, Graphics, Dialogs, ExtCtrls, PropEdits, RTTICtrls,
  LazsUtils,
  {$Else}
  HTMLUtils,
  {$endif}
  WrapperPanel;

type
  {$ifndef JScript}
  TXStore = class(TComponent)
  {$Else}
  TXStore = class(TWrapperPanel)
  {$endif}
  private
    {$ifndef JScript}
    FmyNode:TDataNode;
    FmyControl:TControl;
    {$endif}

  protected
    {$ifndef JScript}
    function GetName:string;
    procedure SetMyName(AValue:string);
    {$endif}

    function getDataValue:String;
    function getKeyName:String;

    procedure setDataValue(AValue:String);
    procedure setKeyName(AValue:String);

  public
    {$ifndef JScript}
    procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
    constructor Create(TheOwner: TComponent);  override;
    constructor Create(TheOwner: TComponent;IsDynamic:Boolean);
    {$else}
    constructor Create(MyForm:TForm;NodeName,NameSpace:String);
    {$endif}
    destructor Destroy; override;

  published
    {$ifndef JScript}
    property myControl:TControl read FmyControl write FmyControl;
    property Name: String read GetName write SetMyName;
    property myNode:TDataNode read FmyNode write FmyNode;
    {$endif}
    property KeyName:String read getKeyName write setKeyName;
    property DataValue:String read getDataValue write setDataValue;

  end;

  {$ifndef JScript}
  procedure Register;
  {$endif}

implementation

var
  myDefaultAttribs:TDefaultAttributesArray;

{$ifndef JScript}
procedure Register;
begin
  {$I XStore.lrs}
  RegisterComponents('XComponents',[TXStore]);

  RegisterPropertyEditor(TypeInfo(TControl), TXStore, 'myControl', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXStore, 'Tag', THiddenPropertyEditor);
end;

procedure TXStore.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
var
  NewNode:TDataNode;
begin

    NewNode:=TDataNode.Create('NV',self.Name,'','TXStore',false);
    NewNode.ScreenObject:=self;
    NewNode.MyEventTypes:=TStringList.Create;
    SetLength(NewNode.myEventHandlers,0);
    NewNode.MyForm:=TForm(TheOwner);
    NewNode.IsDynamic:=IsDynamic;
    self.myNode:=NewNode;
    // temporarily set as child of root node, so that name uniqueness checks can be done during design
    AddChildToParentNode(SystemNodetree,NewNode,-1);

    AddDefaultAttribs(self,NewNode,mydefaultAttribs);

end;

constructor TXStore.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,false);
end;

constructor TXStore.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,IsDynamic);
end;

function TXStore.GetName:string;
var
  myname:string;
begin
  result:=inherited Name;
end;

procedure TXStore.SetMyName(AValue:string);
begin
  inherited Name:=AValue;

  if  (csLoading in componentState) then
    if myNode<>nil then
      myNode.NodeName:=AValue;
end;

function CreateStore(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXStore',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;

{$else}

constructor TXStore.Create(MyForm:TForm;NodeName,NameSpace:String);
begin
  inherited Create(NodeName,NameSpace);
  self.NodeClass:='NV';
  self.NodeType:='TXStore';
  self.MyForm:=MyForm;

  self.myNode.MyEventTypes:=TStringList.Create;
  self.IsContainer:=false;
  SetNodePropDefaults(self,myDefaultAttribs);

end;

function CreateStore(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
begin
//showmessage('Create Store widget');

MyNode.ScreenObject:=MyNode;

result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXStore.Create(MyForm,NodeName,NameSpace));
end;

{$endif}

destructor TXStore.Destroy;
begin
//  if csDesigning in componentState then
//  begin
//    GlobalDesignHook.RemoveAllHandlersForObject(Self);
//    myExtension.Destroy;
//  end;
  ClearLocalStore(KeyName);
  inherited Destroy;
end;

procedure TXStore.setKeyName(AValue:String);
begin
  myNode.SetAttributeValue('KeyName',AValue);
end;

procedure TXStore.setDataValue(AValue:String);
begin
  myNode.SetAttributeValue('DataValue',AValue);

  {$ifndef JScript}
  if not (csDesigning in componentState) then
  {$endif}
    WriteToLocalStore(KeyName,AValue);
end;

function TXStore.getKeyName:String;
begin
  result:=myNode.GetAttribute('KeyName',true).AttribValue;
end;

function TXStore.getDataValue:String;
begin
  {$ifndef JScript}
  if not (csDesigning in componentState) then
    result:=ReadFromLocalStore(KeyName)
  else
    result:=myNode.GetAttribute('DataValue',true).AttribValue;
  {$else}
  result:=ReadFromLocalStore(KeyName)
  {$endif}
end;



begin
  // this is the set of node attributes that each XStore instance will have.
  AddDefaultAttribute(myDefaultAttribs,'KeyName','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'DataValue','String','','',false);

  {$ifndef JScript}
  RegisterClass(TXStore);
  AddNodeFuncLookup('TXStore',@CreateStore);
  {$else}
  AddNodeFuncLookup('TXStore',@CreateinterfaceObj,@CreateStore);
  SuppressDesignerProperty('TXStore','Alignment');
  SuppressDesignerProperty('TXStore','IsVisible');
  SuppressDesignerProperty('TXStore','LabelPos');
  SuppressDesignerProperty('TXStore','LabelText');
  SuppressDesignerProperty('TXStore','Hint');
  SuppressDesignerProperty('TXStore','Border');
  {$endif}
end.
