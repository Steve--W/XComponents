(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XHBox;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, Types, NodeUtils, StringUtils,
  {$ifndef JScript}
  LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, PropEdits, RTTICtrls,
  LazsUtils,
  {$else}
  HTMLUtils,
  {$endif}
  WrapperPanel;

type
  TXHBox = class(TWrapperPanel)       // Laz: descends from TCustomPanel>TWrapperPanel   JS: descends from TObject>TWrapperPanel
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

const MyNodeType='TXHBox';

var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXHBox.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
end;

{$ifndef JScript}
procedure Register;
begin
  // Lazarus IDE component registration
  RegisterComponents('XComponents',[TXHBox]);

  // inherited from TWrapperPanel, not required here
  RegisterPropertyEditor(TypeInfo(String), TXHBox, 'LabelPos', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXHBox, 'LabelText', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(TXPropertyLink), TXHBox, 'Link', THiddenPropertyEditor);
end;

constructor TXHBox.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXHBox.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXHBox.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  Caption:='';
  Constraints.MinHeight:=20;
  Constraints.MinWidth:=20;

  self.BorderSpacing.Around:=0;
  self.ParentColor:=false;

  self.SetMyEventTypes;
  CreateComponentDataNode2(self,MyNodeType,myDefaultAttribs, self.myEventTypes, TheOwner,IsDynamic);

  AlignChildrenVertical:=false;

end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXHBox',ParentNode.MyForm,ParentNode,ScreenObjectName,Alignment,position);
  result:=NewNode;
end;

{$else}
constructor TXHBox.Create(MyForm:TForm;NodeName:String);
begin
  inherited Create(NodeName);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;

  AlignChildrenVertical:=false;

  SetNodePropDefaults(self,myDefaultAttribs);

end;

function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer;Alignment:String):TDataNode;
var
  ShowBorder:boolean;
  Bdr,OnClickString:string;
begin
  //showmessage('create hbox widget');
  Bdr:= MyNode.getAttribute('Border',true).AttribValue;
  if Bdr<>'' then
    ShowBorder:=MyStrToBool(Bdr)
  else
    Showborder:=false;

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''', '''');" ';

  asm
    try{

    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);

    var HTMLString='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var MyObjectName=ScreenObjectName+'Contents';

    HTMLString = '<div  id="'+MyObjectName+'" class="hbox" '+
                       'style="height:100%;width:100%; "'+
                       OnClickString +
                        '></div>  ';

    var wrapper=document.getElementById(ScreenObjectName);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);

    }catch(err) { alert(err.message+'  in XHBox.CreateHBox');}
  end;

  MyNode.ScreenObject:=MyNode;

  RefreshComponentProps(myNode);

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName:String):TObject;
begin
  result:=TObject(TXHBox.Create(MyForm,NodeName));
end;

{$endif}


function TXHBox.GetInheritColor:Boolean;
begin
  result:=myStrToBool(myNode.GetAttribute('InheritColor',true).AttribValue);
end;

procedure TXHBox.SetInheritColor(AValue:Boolean);
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
          //self.Color:=HexRGBToColor(clr);
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
  // this is the set of node attributes that each XHBox instance will have.
  AddDefaultAttribute(myDefaultAttribs,'Alignment','String','Left','',false);
  AddDefaultAttribute(myDefaultAttribs,'Hint','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'IsVisible','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'ContainerWidth','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'ContainerHeight','String','300','',false);
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'BgColor','Color','#FFFFFF','',false);
  AddDefaultAttribute(myDefaultAttribs,'InheritColor','Boolean','False','',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  {$ifndef JScript}
  {$I XHBox.lrs}
  RegisterClass(TXHBox);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  SuppressDesignerProperty(MyNodeType,'LabelPos');
  SuppressDesignerProperty(MyNodeType,'LabelText');
  {$endif}
end.


