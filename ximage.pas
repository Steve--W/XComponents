(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XImage;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface
uses
  Classes, SysUtils, StringUtils, NodeUtils, TypInfo,
  {$ifndef JScript}
  LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, Propedits,RTTICtrls,
  LazsUtils, Events,
  {$else}
  HTMLUtils,
  {$endif}
  WrapperPanel;

type
  TXImage = class(TWrapperPanel)
  private
    { Private declarations }
    {$ifndef JScript}
    fHandleClick:TEventHandler;
    procedure ImageClick(Sender:TObject);
    {$endif}

    procedure SetMyEventTypes;

    function GetSource:string;
    function GetImageWidth:string;
    function GetImageHeight:string;
    function GetClickXPos:integer;
    function GetClickYPos:integer;

    procedure SetSource(AValue:string);
    procedure SetImageWidth(AValue:string);
    procedure SetImageHeight(AValue:string);

  protected
    { Protected declarations }
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
    property Source: String read GetSource write SetSource;
    property ImageHeight: String read GetImageHeight write SetImageHeight;
    property ImageWidth: String read GetImageWidth write SetImageWidth;
    property ClickXPos: integer read GetClickXPos;
    property ClickYPos: integer read GetClickYPos;

    {$ifndef JScript}
    // Events to be visible in Lazarus IDE
    property HandleClick: TEventHandler read FHandleClick write FHandleClick;
    {$endif}
  end;


  {$ifndef JScript}
  procedure Register;
  {$else}
  procedure SaveClickPos(NodeName:String; X,Y:integer);
  {$endif}

implementation

const MyNodeType='TXImage';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXImage.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
end;

{$ifndef JScript}
procedure Register;
begin
  {$I ximage_icon.lrs}
  RegisterComponents('XComponents',[TXImage]);

  // inherited from TWrapperPanel, not required here
  RegisterPropertyEditor(TypeInfo(TColor), TXImage, 'BgColor', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(TXPropertyLink), TXImage, 'Link', THiddenPropertyEditor);
end;

constructor TXImage.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXImage.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXImage.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  self.BorderSpacing.Around:=glbBorderSpacing;

  myControl:=TImage.Create(self);
  myControl.Parent:=self;

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
//  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];

  myControl.OnClick:=@self.ImageClick;

 // showmessage('Image height = '+inttostr(TempImage.Height)+'   width =  '+inttostr(TempImage.Width));
  TImage(myControl).Stretch:=true;

  self.SetMyEventTypes;

  CreateComponentDataNode2(self,MyNodeType,myDefaultAttribs, self.myEventTypes, TheOwner,IsDynamic);

  self.ParentColor:=true;
  // Setting IsContainer false will prevent designer dropping new child controls into this one.
  self.IsContainer:=false;

  AddLabel(myControl);

 end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXImage',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;

procedure TXImage.ImageClick(Sender: TObject) ;
    var
      Pi: TPoint;         // "image" coordinates (=pixels)
begin
  if not (csDesigning in componentState) then
  begin
    Pi := TImage(sender).ScreenToClient(Mouse.CursorPos);
    //showmessage('mousepos '+IntToStr(pi.X)+' '+IntToStr(pi.Y));
    myNode.SetAttributeValue('ClickXPos',intToStr(Pi.X));
    myNode.SetAttributeValue('ClickYPos',intToStr(Pi.Y));
    CallHandleEvent('Click',self.myNode.NodeName,self);
  end;
end;


procedure TXImage.SetImageWidth(AValue:string);
    var
      tc:TControl;
    begin
      tc:=self.myControl;
  myNode.SetAttributeValue('ImageWidth',AValue);
  SetHeightWidth(self.myNode,tc,'ImageWidth','ImageHeight');
end;

procedure TXImage.SetImageHeight(AValue:string);
    var
      tc:TControl;
    begin
      tc:=self.myControl;
  myNode.SetAttributeValue('ImageHeight',AValue);
  SetHeightWidth(self.myNode,tc,'ImageWidth','ImageHeight');
end;

{$else}

constructor TXImage.Create(MyForm:TForm;NodeName,NameSpace:String);
begin
  inherited Create(NodeName,NameSpace);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=false;

  SetNodePropDefaults(self,myDefaultAttribs);

end;

procedure SaveClickPos(NodeName:String; X,Y:integer);
var
myNode:TDataNode;
begin
  myNode:=FindDataNodeById(SystemNodeTree,NodeName,'',true);      //namespace??
  if myNode<>nil then
  begin
    myNode.SetAttributeValue('ClickXPos',intToStr(X));
    myNode.SetAttributeValue('ClickYPos',intToStr(Y));
  end;
end;

function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  Source,LabelText:string;
  OnClickString:String;
  marginString:string;
begin
  LabelText:= MyNode.getAttribute('LabelText',true).AttribValue;
  Source:= MyNode.getAttribute('Source',true).AttribValue;
  marginString := 'margin:'+glbMarginSpacing+' '
                           +glbMarginSpacing+' '
                           +glbMarginSpacing+' '
                           +glbMarginSpacing+';';
  OnClickString:='onclick="event.stopPropagation();pas.XImage.SaveClickPos('''+ScreenObjectName+''',event.offsetX,event.offsetY);pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''','''+NameSpace+''', '''');" ';

  asm
    try{
    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,NameSpace,$impl.MyNodeType,position);

    var wrapperid = NameSpace+ScreenObjectName;
    var MyObjectName=wrapperid+'Contents';

    var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'">'+LabelText+'</label>';

    var ImageString = ' <img  id='+MyObjectName+ ' style="display: inline-block; height:90%; width:90%" src='+Source+' '+
                         OnClickString +
                         ' >';

    var HTMLString = labelstring+ImageString;

    var wrapper=document.getElementById(wrapperid);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);
    }
    catch(err) { alert(err.message+'  in XImage.CreateWidget');}

  end;

  MyNode.ScreenObject:=MyNode;

  RefreshComponentProps(myNode);

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXImage.Create(MyForm,NodeName,NameSpace));
end;

procedure TXImage.SetImageWidth(AValue:string);
begin
  myNode.SetAttributeValue('ImageWidth',AValue);
  asm
  //var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
  var ob = document.getElementById(this.NameSpace+this.NodeName);
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
  end;
end;

procedure TXImage.SetImageHeight(AValue:string);
begin
  myNode.SetAttributeValue('ImageHeight',AValue);
  asm
  //var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
  var ob = document.getElementById(this.NameSpace+this.NodeName);
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'H',AValue);
  end;
end;

{$endif}

function TXImage.GetSource:String;
begin
  result:=MyNode.getAttribute('Source',true).AttribValue;
end;
function TXImage.GetImageHeight:string;
begin
  result:=MyNode.getAttribute('ImageHeight',true).AttribValue;
end;
function TXImage.GetImageWidth:string;
begin
  result:=MyNode.getAttribute('ImageWidth',true).AttribValue;
end;
function TXImage.GetClickXPos:integer;
begin
  result:=-1;
  if myNode<>nil then
    result:=StrToInt(myNode.GetAttribute('ClickXPos',false).AttribValue);
end;
function TXImage.GetClickYPos:integer;
begin
  result:=-1;
  if myNode<>nil then
    result:=StrToInt(myNode.GetAttribute('ClickYPos',false).AttribValue);
end;


procedure TXImage.SetSource(AValue:string);
{$ifndef JScript}
var
  strm:TStringStream;
{$endif}
begin
  myNode.SetAttributeValue('Source',AValue);
  {$ifndef JScript}
  if myControl<>nil then
  begin
     if AValue<>'' then
     begin
       try
         TImage(myControl).Picture.LoadFromFile(self.Source);
       except
         on e:exception do
           begin
             TImage(myControl).Picture.LoadFromLazarusResource('dfltImage');
             //showmessage('XImage.SetSource Unable to load specified image file '+AValue);
           end;

       end;

     end
     else
       TImage(myControl).Picture.LoadFromLazarusResource('dfltImage');

  end;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
       if (AValue=='') {
         ob.src='dfltImage.gif';
       }
       else {
       try{
         ob.src=AValue
         }
             catch(err) {
               alert(err.message+'  in XImage.SetSource '+AValue);
               ob.src='dfltImage.gif';
               }
         }
       }

  end;
  {$endif}

end;

begin
  // this is the set of node attributes that each XHBox instance will have.
  AddWrapperDefaultAttribs(myDefaultAttribs);
  AddDefaultAttribute(myDefaultAttribs,'ImageWidth','String','250','',false);
  AddDefaultAttribute(myDefaultAttribs,'ImageHeight','String','200','',false);
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','Right','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelText','String','Image','',false);
  AddDefaultAttribute(myDefaultAttribs,'Source','String','','',false);  //default loads resource dfltImage (in resource file XImage.lrs)
  AddDefaultAttribute(myDefaultAttribs,'ClickXPos','Integer','','',true,false);
  AddDefaultAttribute(myDefaultAttribs,'ClickYPos','Integer','','',true,false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  AddAttribOptions(MyNodeType,'LabelPos',LabelPosOptions);
  {$ifndef JScript}
  {$I XImage.lrs}           // contains default image
  RegisterClass(TXImage);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}
  SuppressDesignerProperty('TXImage','ContainerHeight');
  SuppressDesignerProperty('TXImage','ContainerWidth');

end.
