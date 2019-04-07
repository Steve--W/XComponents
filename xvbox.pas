(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XVBox;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface
uses
  Classes, SysUtils, Types, NodeUtils, StringUtils,
  {$ifndef JScript}
  LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, PropEdits, RTTICtrls,
  LazsUtils, LazIDEIntf,
  {$else}
  HTMLUtils,
  {$endif}
  WrapperPanel;

type
  TXVBox = class(TWrapperPanel)         // for Lazarus, descends from TCustomPanel; for JScript, descends from TDataNode
  private
    { Private declarations }
    procedure SetMyEventTypes;

    function GetInheritColor:Boolean;
    procedure SetInheritColor(AValue:Boolean);

  protected
    { Protected declarations }
    {$ifndef JScript}
    procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
    {$endif}
  public
    { Public declarations }
    {$ifndef JScript}
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent;IsDynamic:Boolean); override;
    {$else}
    constructor Create(MyForm:TForm;NodeName:String);
    {$endif}
  published
    { Published declarations }

    // Properties defined for this class...
    property InheritColor:Boolean read GetInheritColor write SetInheritColor;
  end;

  {$ifndef JScript}
procedure Register;
{$endif}

implementation

const MyNodeType='TXVBox';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXVBox.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
end;

{$ifndef JScript}
procedure Register;
begin
  // Lazarus IDE component registration
  RegisterComponents('XComponents',[TXVBox]);

  // inherited from TWrapperPanel, not required here
  RegisterPropertyEditor(TypeInfo(String), TXVBox, 'LabelPos', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXVBox, 'LabelText', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(TXPropertyLink), TXVBox, 'Link', THiddenPropertyEditor);

  LazarusIDE.AddHandlerOnProjectOpened(@myProjectEvents.OnProjectOpened);
  //LazarusIDE.AddHandlerOnProjectBuilding(@myProjectEvents.OnProjectBuilding);
end;

constructor TXVBox.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXVBox.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXVBox.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  Caption:='';
  Constraints.MinHeight:=20;
  Constraints.MinWidth:=20;

  self.BorderSpacing.Around:=0;
  self.ParentColor:=false;

  self.SetMyEventTypes;
  CreateComponentDataNode2(self,MyNodeType,myDefaultAttribs, self.myEventTypes, TheOwner,IsDynamic);

  AlignChildrenVertical:=true;

end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXVBox',ParentNode.MyForm,ParentNode,ScreenObjectName,Alignment,position);
  result:=NewNode;
end;

{$else}
constructor TXVBox.Create(MyForm:TForm;NodeName:String);
begin
  inherited Create(NodeName);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;

  SetNodePropDefaults(self,myDefaultAttribs);
end;

function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer;Alignment:String):TDataNode;
var
  ShowBorder:boolean;
  myObj:TXVBox;
  OnClickString:String;
begin

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''', '''');" ';

  asm
  try{
      var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);

      var HTMLString='';
      var MyObjectName=ScreenObjectName+'Contents';

      HTMLString = '<div  id="'+MyObjectName+'" class="vboxNoStretch" '  +
                     ' style="height:100%;width:100%; "' +
                     OnClickString +
                     '></div>  ';

      var wrapper=document.getElementById(ScreenObjectName);
      wrapper.insertAdjacentHTML('beforeend', HTMLString);

  }catch(err) { alert(err.message+'  in XVBox.CreateVHBox');}
  end;
  MyNode.ScreenObject:=MyNode;

  RefreshComponentProps(myNode);

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;Nodename:String):TObject;
begin
result:=TObject(TXVBox.Create(MyForm,NodeName));
end;

{$endif}

function TXVBox.GetInheritColor:Boolean;
begin
  result:=myStrToBool(myNode.GetAttribute('InheritColor',true).AttribValue);
end;

procedure TXVBox.SetInheritColor(AValue:Boolean);
var
  clr:String;
  parentNode:TDataNode;
begin
  if myNode<>nil then
  begin
    myNode.SetAttributeValue('InheritColor',myBoolToStr(AValue),'Boolean');
    parentNode:=FindParentOfNode(SystemNodeTree,myNode);
    if parentNode<>nil then
    begin
      if AValue=true then
      begin
        clr:= parentNode.GetAttribute('BgColor',true).AttribValue;
        myNode.SetAttributeValue('BgColor',clr,'Color');
        {$ifndef JScript}
        self.ParentColor:=true;
        {$else}
        asm
          var ob = document.getElementById(this.NodeName);
          if (ob!=null) {
            if (AValue==true ) {
               ob.style.backgroundColor='inherit';
          } }
        end;
        {$endif}
      end
      else
      begin
        clr:= myNode.GetAttribute('BgColor',true).AttribValue;
        {$ifndef JScript}
        self.ParentColor:=false;
        self.Color:=HexRGBToColor(clr);
        {$else}
        asm
          var ob = document.getElementById(this.NodeName);
          if (ob!=null) {
            if (AValue==true ) {
               ob.style.backgroundColor=clr;
          } }
        end;
        {$endif}
      end;
    end;
  end;
end;

begin
  // this is the set of node attributes that each XVBox instance will have.
  AddDefaultAttribute(myDefaultAttribs,'Alignment','String','Left','',false);
  AddDefaultAttribute(myDefaultAttribs,'Hint','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'IsVisible','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'ContainerWidth','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'ContainerHeight','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'BgColor','Color','#FFFFFF','',false);
  AddDefaultAttribute(myDefaultAttribs,'InheritColor','Boolean','False','',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  {$ifndef JScript}
  {$I XVBox.lrs}
  RegisterClass(TXVBox);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  SuppressDesignerProperty(MyNodeType,'LabelPos');
  SuppressDesignerProperty(MyNodeType,'LabelText');
  {$endif}
end.


