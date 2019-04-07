(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XTrapEvents;
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
  TXTrapEvents = class(TComponent)
  {$Else}
  TXTrapEvents = class(TWrapperPanel)
  {$endif}
  private
    {$ifndef JScript}
    FmyNode:TDataNode;
    FmyControl:TControl;
    {$endif}

  protected
    {$ifndef JScript}
    FHandleAny:TEventHandler;
    function GetName:string;
    procedure SetMyName(AValue:string);
    {$endif}


  public
    {$ifndef JScript}
    myEventTypes:TStringList;
    procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
    constructor Create(TheOwner: TComponent);  override;
    constructor Create(TheOwner: TComponent;IsDynamic:Boolean);
    {$else}
    constructor Create(MyForm:TForm;NodeName:String);
    {$endif}
    destructor Destroy; override;
    procedure SetMyEventTypes;

  published
    {$ifndef JScript}
    property myControl:TControl read FmyControl write FmyControl;
    property Name: String read GetName write SetMyName;
    property myNode:TDataNode read FmyNode write FmyNode;
    {$endif}

    {$ifndef JScript}
    // Events to be visible in Lazarus IDE
    property HandleAny: TEventHandler read FHandleAny write FHandleAny;
    {$endif}
  end;

  {$ifndef JScript}
  procedure Register;
  {$endif}

implementation

const MyNodeType='TXTrapEvents';

procedure TXTrapEvents.SetMyEventTypes;
begin
  MyEventTypes.Add('Any');
end;

{$ifndef JScript}
procedure Register;
begin
  {$I Icons/XTrapEvents.lrs}
  RegisterComponents('XComponents',[TXTrapEvents]);

  RegisterPropertyEditor(TypeInfo(TControl), TXTrapEvents, 'myControl', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXTrapEvents, 'Tag', THiddenPropertyEditor);
end;

procedure TXTrapEvents.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
var
  NewNode:TDataNode;
begin
    //self.myNode:=CreateComponentDataNode(self.Name,MyNodeType, self.myEventTypes, self,TheOwner,IsDynamic);
    NewNode:=TDataNode.Create('NV',self.Name,'TXTrapEvents',false);
    NewNode.ScreenObject:=self;
    NewNode.MyEventTypes:=TStringList.Create;
    NewNode.MyForm:=TForm(TheOwner);
    NewNode.IsDynamic:=IsDynamic;

    self.MyEventTypes:=TStringList.Create;
    self.SetMyEventTypes;

    SetLength(NewNode.myEventHandlers,self.myEventTypes.Count);
    NewNode.myEventTypes:=self.myEventTypes;


    AddChildToParentNode(SystemNodetree,NewNode,-1);

    self.myNode:=NewNode;
end;

constructor TXTrapEvents.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,false);
end;

constructor TXTrapEvents.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,IsDynamic);
end;

function TXTrapEvents.GetName:string;
var
  myname:string;
begin
  result:=inherited Name;
end;

procedure TXTrapEvents.SetMyName(AValue:string);
begin
  inherited Name:=AValue;

  if myNode<>nil then
     myNode.NodeName:=AValue;
end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXTrapEvents',ParentNode.MyForm,ParentNode,ScreenObjectName,Alignment,position);
  result:=NewNode;
end;

{$else}

constructor TXTrapEvents.Create(MyForm:TForm;NodeName:String);
begin
  inherited Create(NodeName);
  self.NodeClass:='NV';
  self.NodeType:='TXTrapEvents';
  self.MyForm:=MyForm;

  self.myNode.MyEventTypes:=TStringList.Create;
  self.SetMyEventTypes;
  self.IsContainer:=false;

end;

function CreateTrapper(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer;Alignment:String):TDataNode;
begin
//showmessage('Create Store widget');

MyNode.ScreenObject:=MyNode;

result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName:String):TObject;
begin
  result:=TObject(TXTrapEvents.Create(MyForm,NodeName));
end;

{$endif}

destructor TXTrapEvents.Destroy;
begin
//  ClearLocalStore(KeyName);
  inherited Destroy;
end;


begin
  {$ifndef JScript}
  RegisterClass(TXTrapEvents);
  AddNodeFuncLookup('TXTrapEvents',@CreateWidget);
  {$else}
  AddNodeFuncLookup('TXTrapEvents',@CreateinterfaceObj,@CreateTrapper);
  SuppressDesignerProperty('TXTrapEvents','Alignment');
  SuppressDesignerProperty('TXTrapEvents','IsVisible');
  SuppressDesignerProperty('TXTrapEvents','LabelPos');
  SuppressDesignerProperty('TXTrapEvents','LabelText');
  SuppressDesignerProperty('TXTrapEvents','Hint');
  {$endif}
end.

end.
