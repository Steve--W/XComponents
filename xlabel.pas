(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XLabel;
{$ifndef JScript}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls,TypInfo,Messages, Propedits,RTTICtrls,
  WrapperPanel,NodeUtils,StringUtils, LazsUtils, Events;
{$else}
interface
uses
  Classes, SysUtils,TypInfo,
  NodeUtils,StringUtils,HTMLUtils, WrapperPanel;

{$endif}

type
  TXLabel = class(TWrapperPanel)
  private
    { Private declarations }
    {$ifndef JScript}
    fHandleClick:TEventHandler;
    procedure Labelclick(Sender:TObject);
    {$endif}

    procedure SetMyEventTypes;

    function GetLabelCaption:string;

    procedure SetLabelCaption(AValue:string);

  protected
    { Protected declarations }
    {$ifndef JScript}
    procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
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
    property LabelCaption: String read GetLabelCaption write SetLabelCaption;

    {$ifndef JScript}
    // Events to be visible in Lazarus IDE
    property HandleClick: TEventHandler read FHandleClick write FHandleClick;
    {$endif}
  end;


  {$ifndef JScript}
  procedure Register;
  {$endif}

implementation

const MyNodeType='TXLabel';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXLabel.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
end;

{$ifndef JScript}
procedure register;
begin
  {$I xlabel_icon.lrs}
  RegisterComponents('XComponents',[TXLabel]);

  // inherited from TWrapperPanel, not required here
  RegisterPropertyEditor(TypeInfo(String), TXLabel, 'LabelPos', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXLabel, 'LabelText', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(TXPropertyLink), TXLabel, 'Link', THiddenPropertyEditor);
end;

constructor TXLabel.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXLabel.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXLabel.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  self.BorderSpacing.Around:=glbBorderSpacing;

  myControl:=TLabel.Create(self);
  myControl.Parent:=self;

  myControl.AutoSize:=true;

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
//  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];

  myControl.OnClick:=@self.LabelClick;

  self.SetMyEventTypes;

  CreateComponentDataNode2(self,MyNodeType,myDefaultAttribs, self.myEventTypes, TheOwner,IsDynamic);

  self.ParentColor:=true;
  // Setting IsContainer false will prevent designer dropping new child controls into this one.
  self.IsContainer:=false;

end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXLabel',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;

procedure TXLabel.LabelClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;

procedure TXLabel.WMPaint(var Message: TWMPaint);
var
  cap:String;
  lbl:TLabel;
begin
  lbl:=TLabel(self.myControl);
  cap:=TLabel(self.myControl).Caption;
  try
    inherited;
  except
    on e:exception do
    begin end;
  end;
end;

{$else}

constructor TXLabel.Create(MyForm:TForm;NodeName,NameSpace:String);
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
  OnClickString:String;
  marginString:string;
begin
  LabelText:= MyNode.getAttribute('LabelCaption',true).AttribValue;
  marginString := 'margin:'+glbMarginSpacing+' '
                           +glbMarginSpacing+' '
                           +glbMarginSpacing+' '
                           +glbMarginSpacing+';';
  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''','''+NameSpace+''', this.value);" ';

  asm
    try{
    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,NameSpace,$impl.MyNodeType,position);

    var wrapperid = NameSpace+ScreenObjectName;
    var MyObjectName=wrapperid+'Contents';

    var HTMLString = ' <label id='+MyObjectName+' '+
                   OnClickString +
                 ' style="display: inline-block;'+marginString+'; backgroundColor=inherit"  >'
                 +LabelText+'</label> ';

    var wrapper=document.getElementById(wrapperid);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);

    }
    catch(err) { alert(err.message+'  in XLabel.CreateWidget');}

  end;

  MyNode.ScreenObject:=MyNode;

  RefreshComponentProps(myNode);

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXLabel.Create(MyForm,NodeName,NameSpace));
end;


{$endif}

function TXLabel.GetLabelCaption:String;
begin
  result:=MyNode.getAttribute('LabelCaption',true).AttribValue;
end;
procedure TXLabel.SetLabelCaption(AValue:string);
begin
  myNode.SetAttributeValue('LabelCaption',AValue);

  {$ifndef JScript}
  if myControl<>nil then
  begin
      TLabel(myControl).Caption:=AValue;
  end;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
    //alert('set labelCaption to '+AValue);
       ob.innerHTML=AValue;   }
  end;
  {$endif}

end;

begin
  // this is the set of node attributes that each XHBox instance will have.
  AddWrapperDefaultAttribs(myDefaultAttribs);
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelCaption','String','...Label...','',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  {$ifndef JScript}
  RegisterClass(TXLabel);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}
  SuppressDesignerProperty(MyNodeType,'LabelPos');
  SuppressDesignerProperty(MyNodeType,'LabelText');
  SuppressDesignerProperty(MyNodeType,'BgColor');

end.
