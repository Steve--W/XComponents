(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XForm;
interface

uses
  Classes, SysUtils, Types, NodeUtils,StringUtils,
  {$ifndef JScript}
  Forms, Menus, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, PropEdits, RTTICtrls,
  WrapperPanel, LazsUtils,XScrollBox;
  {$Else}
  HTMLUtils;
  {$endif}

procedure ShowXForm(XFormID:String; modal:Boolean);
procedure CloseXForm(XFormID:String);

{$ifndef JScript}


type TXForm = class(TForm)
  {$else}
type TXForm = class(TInterfaceObject)            // from TDataNode
  {$endif}
private
  fIsSelected:Boolean;
  fIsContainer:Boolean;
  {$ifndef JScript}
  fMyNode:TDataNode;
  {$endif}

  function getHeight:integer;
  function getWidth:integer;
  function getTop:integer;
  function getLeft:integer;
  function getCaption:string;
  function getShowing:string;
  procedure SetHeight(AValue:integer);
  procedure SetWidth(AValue:integer);
  procedure SetTop(AValue:integer);
  procedure SetLeft(AValue:integer);
  procedure SetCaption(AValue:string);
  procedure SetShowing(AValue:string);
public
  {$ifndef JScript}
  procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);  virtual;
  {$else}
  constructor Create(NodeName:String);
  {$endif}
published
  {$ifndef JScript}
  property myNode:TDataNode read FmyNode write FmyNode;
  {$endif}
  property IsContainer:Boolean read FIsContainer write FIsContainer;
  property IsSelected:Boolean read fIsSelected write fIsSelected;
  property Height:integer read getHeight write SetHeight;
  property Width:integer read getWidth write SetWidth;
  property Top:Integer read getTop write SetTop;
  property Left:Integer read getLeft write SetLeft;
  property Caption:String read getCaption write SetCaption;
  property Showing:String read getShowing write SetShowing;
end;

  {$ifndef JScript}
  {$else}
procedure OpenModal(WindowId:string);
procedure CloseModal(WindowId:string);
procedure addTheModalBackground(ParentName,WindowId:string);
procedure addaModalContentItem(MyName:string);
procedure InitialiseXFormStyles();
{$endif}

var OpenXForms:TStringList;

implementation

var
  myDefaultAttribs:TDefaultAttributesArray;


{$ifndef JScript}

function FindFormByName(const AName: string): TForm;
var
  i: Integer;
begin
  for i := 0 to Screen.FormCount - 1 do
  begin
    Result := Screen.Forms[i];
    if (Result.Name = AName) then
      Exit;
  end;
  Result := nil;
end;


function CreateForm(ParentNode:TDataNode;ScreenObjectName:string;position:integer;Alignment:String):TDataNode;
// for dynamic creation of a XForm (Windows) at runtime.
var
  NewForm:TXForm;
  NewNode,PanelNode:TDataNode;
  TmpPanel:TXScrollBox;
  dummyEventsList:TStringList;
begin
  dummyEventsList:=TStringList.Create;

  NewForm :=  TXForm.CreateNew(Application);
  NewForm.name:=ScreenObjectName;
  NewForm.OnClose:=@NewForm.FormClose;

  CreateComponentDataNode2(NewForm,'TXForm',myDefaultAttribs, dummyEventsList, NewForm,true);
  NewNode:=NewForm.myNode;
  NewNode.NodeName:=ScreenObjectName;
  AddChildToParentNode(ParentNode,NewNode,position);

  NewForm.IsContainer:=true;

  result:= NewNode;
end;


procedure ShowXForm(XFormID:String; modal:Boolean);
var OldParent:TComponent;
  FormToShow:TXForm;
begin
  FormToShow:=TXForm(Application.FindComponent(XFormID));
  if FormToShow<>nil then
  begin
    if (FormToShow<>MainForm) then
    begin
      FormToShow.BorderStyle:=bsSingle;
      if modal then
        FormToShow.Showing:='Modal'
      else
        FormToShow.Showing:='Normal';
    end;
  end
  else
    ShowMessage('XForm '+XFormID+' not found');
end;

procedure TXForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if myNode<>nil then
    myNode.SetAttributeValue('Showing','No');
end;

{$Else}

//
//-------------------------- declaration of the Styles for the Popup -----------------------------
procedure InitialiseXFormStyles();
begin

  asm
  try{
    // ----------------------------------------check if the style has already been set
    var x = document.getElementsByTagName("STYLE");
    var StyleIsSet = false;
    if (x.length>0){
      for (var i=0; i<x.length; i++){
        var y= x[i].innerHTML;
        if (y.indexOf("modal-background") !=-1) { StyleIsSet =true}
      }
    }
    if (StyleIsSet == false){
       var ModalBackgroundStyleString = ''
       +'<style>'
        +'/* The Modal (background) */'
        +'.modal-background {'
            +'display: none; /* Hidden by default */'
            +'position: fixed; /* Stay in place */'
            +'z-index: 1; /* Sit on top */'
            +'padding-top: 10px; /* Location of the box */'
            +'left: 0;'
            +'top: 0;'
            +'width: 100%; /* Full width */'
            +'height: 100%; /* Full height */'
            //+'overflow: auto; /* Enable scroll if needed */'
            +'background-color: rgb(0,0,0); /* Fallback color */'
            +'background-color: rgba(0,0,0,0.3); /* Black w/ opacity */'
        +'} '
        +'.modal-content {'
            +'background-color: #FFFFFF;'
            +'position: absolute;'
            +'border: 1px solid #888800;'
        +'}'
        +'</style>';
      //----------------------------- now append the style declarations to the head of the HTML page
      document.head.innerHTML = document.head.innerHTML+ModalBackgroundStyleString;
    }
  }catch(err)  {alert('Error in XForm.InitialiseXFormStyles '+ err.message);}
  end;

end;


//===========================================================================================
//-------------------------- declaration of a Modal Window -----------------------------
procedure addTheModalBackground(ParentName,WindowId:string);
var
  OnClickString:String;
begin
  if WindowId = MainForm.Name then
    EXIT;  //wrapper for main form already exists

  OnClickString:='pas.XForm.CloseModal(event.target.id); event.stopPropagation();';
  asm
  try{
  //alert('addTheModalBackground '+WindowId);
    $mod.InitialiseXFormStyles()
    var HTMLString = ''
    +'<div id='+WindowId+' class="modal-background" '
    +'onclick="'+OnClickString+'">'
    +'</div>';

    //----- now append the declarations to the Parent -------------------------------------------
    var ParentItem=document.getElementById(ParentName);
    ParentItem.insertAdjacentHTML('beforeend', HTMLString);

    //alert('addTheModalBackground done');
  }catch(err) {alert('Error in XForm.addTheModalBackground '+ err.message);}
  end;

end;

//-------------------------- declaration of the Content Items-----------------------------
procedure addaModalContentItem(MyName:string);
var
  ContentName:String;
begin
  ContentName:=MyName+'Contents';
  asm
  try{
      var HTMLString = ''
      +'  <!-- Form '+MyName+' content -->'
      +'  <div id="'+ContentName+'" class="modal-content" > '
      +'    <div id="'+MyName+'Caption" ></div> '
      +'  </div>';

      var ParentItem = document.getElementById(MyName);
      ParentItem.innerHTML = ParentItem.innerHTML + HTMLString;

  }catch(err){alert('Error in XForm.addaModalContentItem '+ err.message);}
  end;

end;


procedure OpenModal(WindowId:string);
begin
  asm
  try{
     var modalwindowid= WindowId;
      var modal = document.getElementById(modalwindowid);
      modal.style.display = 'block';
  }catch(err){alert('Error in XForm.OpenModal '+ err.message);}
  end;
  if WindowId<>MainForm.Name then
    if OpenXForms.IndexOf(WindowId)<0 then
      OpenXForms.Add(WindowId);
end;

procedure CloseModal(WindowId:string);
var
  formNode:TDataNode;
begin
  formNode:=FindDataNodeById(SystemNodeTree,WindowId,false);
  if formNode<>nil then
    formNode.SetAttributeValue('Showing','No');
  asm
    var modal = document.getElementById(WindowId);
    if (modal!=null) {
      modal.style.display = "none";   }
  end;

  if OpenXForms.IndexOf(WindowId)>-1 then
    OpenXForms.delete(OpenXForms.IndexOf(WindowId));
end;

procedure ShowXForm(XFormID:String; modal:Boolean);
var
  XFormNode:TDataNode;
  XFormObj:TXForm;
begin
  XFormNode:=FindDataNodeById(SystemNodeTree,XFormID,true);
  //showmessage('ShowXForm. XFormNode is '+XFormNode.NodeName);
  if XFormNode<>nil then
  begin
    XFormObj:=TXForm(XFormNode.ScreenObject);
    XFormObj.Showing:='Modal';

  end;
end;

constructor TXForm.Create(NodeName:String);
begin
  inherited Create('UI',NodeName,'TXForm',true);
  myNode:=TDataNode(self);

  IsContainer:=true;
  SetNodePropDefaults(self,myDefaultAttribs);
end;

function CreateinterfaceObj(MyForm:TForm;Nodename:String):TObject;
var newobj:TObject;
begin
  //showmessage('createinterfaceobj for XForm '+NodeName);
  newObj:=TObject(TXForm.Create(Nodename));
  if MyForm<>nil then
    TInterfaceObject(newObj).myForm:=MyForm
  else
  begin
    // dynamically created XForm needs a dummy 'form' object to hold the name
    MyForm:=TForm.Create;
    MyForm.Name:=Nodename;
    TInterfaceObject(newObj).myForm:=MyForm;
  end;
  TInterfaceObject(newObj).myNode:=TDataNode(newObj);

  result:=newObj;
end;

function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer;Alignment:String):TDataNode;
var
   ParentName:string;
begin
 // showmessage('createWidget XForm.  Node type is '+MyNode.ClassName);
  if ParentNode<>nil then
    ParentName:=ParentNode.NodeName
  else
    Parentname:=TXForm(MainForm).myNode.NodeName;

  asm
  try{
    $mod.addTheModalBackground(ParentName,ScreenObjectName);
    $mod.addaModalContentItem(ScreenObjectName);

    }catch(err) { alert(err.message+' in XForm.CreateWidget');}
  end;

  MyNode.ScreenObject:=MyNode;
  TXForm(myNode).myNode:=myNode;

  if ScreenObjectName=MainForm.Name then
  begin
    TXForm(myNode).Caption := '';
    TXForm(myNode).Top := 0;
    TXForm(myNode).Left := 0;
    TXForm(myNode).Height := 0;   // goes to 100%
    TXForm(myNode).Width := 0;    // goes to 100%
  end
  else
  begin
    RefreshComponentProps(myNode);
    TXForm(myNode).IsContainer := true;
  end;
  result:=myNode;
end;
{$endif}

function TXForm.GetHeight:integer;
begin
  if myNode<>nil then
    result:=StrToInt(myNode.GetAttribute('Height',true).AttribValue)
  else
    {$ifndef JScript}
    result := inherited Height;
    {$else}
    result:=0;
    {$endif}
end;

function TXForm.GetWidth:integer;
begin
  if myNode<>nil then
    result:=StrToInt(myNode.GetAttribute('Width',true).AttribValue)
  else
    {$ifndef JScript}
    result := inherited Width;
    {$else}
    result:=0;
    {$endif}
end;

function TXForm.GetCaption:string;
begin
  if myNode<>nil then
    result:=myNode.GetAttribute('Caption',true).AttribValue
  else
    {$ifndef JScript}
    result := inherited Caption;
    {$else}
    result:='';
    {$endif}
end;


function TXForm.GetTop:integer;
var
  AttrVal:string;
begin
  //showmessage('TXForm GetTop');
  if myNode<>nil then
  begin
    AttrVal:=myNode.GetAttribute('Top',true).AttribValue;
    if AttrVal<>'' then
      result:=StrToInt(AttrVal)
    else
      result:=0;
  end
  else
    {$ifndef JScript}
    result := inherited Top;
    {$else}
    result:=0;
    {$endif}
end;

function TXForm.GetLeft:integer;
var
  AttrVal:string;
begin
  //showmessage('TXForm GetLeft');
  if myNode<>nil then
  begin
    AttrVal:=myNode.GetAttribute('Left',true).AttribValue;
    if AttrVal<>'' then
      result:=StrToInt(AttrVal)
    else
      result:=0;
  end
  else
    {$ifndef JScript}
    result := inherited Left;
    {$else}
    result:=0;
    {$endif}
end;

procedure  CloseXForm(XFormID:String);
var
  FormToClose:TForm;
  formNode:TdataNode;
begin
  formNode:=FindDataNodeByid(SystemNodeTree,XFormID,false);
  if formNode<>nil then
  begin
    TXForm(formNode.ScreenObject).Showing:='No';
  end
  else
  begin
    {$ifndef JScript}
    FormToClose:=TForm(Application.FindComponent(XFormID));
     if FormToClose<>nil then
         FormToClose.Close;
    {$else}
    CloseModal(XFormID);
    {$endif}
  end;
end;

function TXForm.GetShowing:string;
begin
  if myNode<>nil then
    result:=myNode.GetAttribute('Showing',true).AttribValue
  else
    result := 'No';
end;

procedure TXForm.SetShowing(AValue:string);
begin
  if myNode<>nil then
  begin
    //showmessage('TXForm '+myNode.NodeName+' SetShowing '+AValue);
    myNode.SetAttributeValue('Showing',AValue);

    {$ifndef JScript}
    if AValue='No' then
    begin
      // close the form
      self.Close;
      if OpenXForms.IndexOf(self.Name)>-1 then
        OpenXForms.delete(OpenXForms.IndexOf(self.Name));
    end
    else
    begin
      //showmessage('TXForm '+self.Name+' SetShowing '+AValue);
      if self.Visible = false then
        if AValue='Modal' then
          self.showmodal
        else if AValue='Normal' then
          self.Show;
      if self.Name<>MainForm.Name then
        if OpenXForms.IndexOf(self.Name)<0 then
          OpenXForms.Add(self.Name);
    end;
    {$else}
    if self.NodeName<>'' then
      begin
      //showmessage('TXForm SetShowing '+AValue+' for node '+self.NodeName);
      if AValue='No' then
      begin
        CloseModal(self.NodeName);
      end
      else
      begin
        self.Top:=self.Top;
        self.Left:=Self.Left;
        //showmessage('TXForm '+self.NodeName+' SetShowing '+AValue);
        asm
        try{
           var modalwindowid= this.NodeName;
           //alert('open windowid='+modalwindowid);
           var modal = document.getElementById(modalwindowid);
           // alert('found '+modal);
           modal.style.display = 'block';
        }catch(err){alert('Error in XForm.SetShowing '+ err.message);}
        end;
        if self.NodeName<>MainForm.Name then
          if OpenXForms.IndexOf(self.NodeName)<0 then
            OpenXForms.Add(self.NodeName);
      end;
    end;
    {$endif}
  end;
end;

procedure TXForm.SetWidth(AValue:integer);
begin
  //showmessage('TXForm SetFormWidth');
  if myNode<>nil then
    myNode.SetAttributeValue('Width',intToStr(AValue));
  {$ifndef JScript}
  inherited width:=AValue;
  {$else}
  asm
    var ob=document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
      var str=AValue;
      if (AValue==0) str='100%';
      pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',str);
    }
  end;
  {$endif}
end;

procedure TXForm.SetTop(AValue:integer);
var
  t:string;
begin
  //showmessage('TXForm SetTop');
  if myNode<>nil then
    myNode.SetAttributeValue('Top',inttostr(AValue));
  {$ifndef JScript}
  inherited Top:=AValue;
  {$else}
  t:=inttostr(AValue)+'px';
  asm
    var ob=document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
      ob.style.top=t;

      // if this means the modal window is not fully visible, adjust the top
        var style = window.getComputedStyle(ob);
        var hh = style.height;
        var h = parseInt(hh, 10);

        var btm = AValue + h;
        var viewHeight = Math.max(document.documentElement.clientHeight, window.innerHeight);
        //alert(this.NodeName+' viewHeight='+viewHeight+' bottom='+btm);
        if (btm > viewHeight) {ob.style.top="20px";}


    }
  end;
  {$endif}
end;

procedure TXForm.SetLeft(AValue:integer);
    var
      l:string;
begin
  //showmessage('TXForm SetLeft');
  if myNode<>nil then
    myNode.SetAttributeValue('Left',inttostr(AValue));
  {$ifndef JScript}
  inherited Left:=AValue;
  {$else}
  l:=inttostr(AValue)+'px';
  asm
    var ob=document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
      ob.style.left=l;

      // if this means the modal window is not fully visible, adjust the left
      var style = window.getComputedStyle(ob);
      var ww = style.width;
      var w = parseInt(ww, 10);

      var rgt = AValue + w;
      var viewWidth = Math.max(document.documentElement.clientWidth, window.innerWidth);
      if (rgt > viewWidth) {ob.style.left="20px";}

      }
  end;
  {$endif}
end;


procedure TXForm.SetHeight(AValue:integer);
begin
  //showmessage('TXForm SetHeight '+inttostr(AValue));
  if myNode<>nil then
    myNode.SetAttributeValue('Height',intToStr(AValue));
  {$ifndef JScript}
  inherited Height := AValue;
  {$else}
  asm
    var ob=document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
      var str=AValue;
      if (AValue==0) str='100%';
      pas.HTMLUtils.SetHeightWidthHTML(this,ob,'H',str);
    }
  end;
  {$endif}
end;

procedure TXForm.SetCaption(AValue:string);
begin
  //showmessage('TXForm SetCaption');
  if myNode<>nil then
    myNode.SetAttributeValue('Caption',AValue);
  {$ifndef JScript}
  inherited Caption:=AValue;
  {$else}
  asm
    var ob=document.getElementById(this.NodeName+'Caption');
    if (ob!=null) {
      ob.innerHTML=AValue;
    }
  end;
  {$endif}
end;

begin
  // this is the set of node attributes that each TXForm instance will have.
  AddDefaultAttribute(myDefaultAttribs,'Showing','String','No','',false,false);
  AddDefaultAttribute(myDefaultAttribs,'Width','Integer','400','',false);
  AddDefaultAttribute(myDefaultAttribs,'Height','Integer','200','',false);
  AddDefaultAttribute(myDefaultAttribs,'Top','Integer','50','',false);
  AddDefaultAttribute(myDefaultAttribs,'Left','Integer','50','',false);
  AddDefaultAttribute(myDefaultAttribs,'Caption','String','My Title','',false);

  OpenXForms:=TStringList.Create;
  {$ifndef JScript}
  RegisterClass(TXForm);
  AddNodeFuncLookup('TXForm',@CreateForm);
  {$else}
  AddNodeFuncLookup('TXForm',@CreateinterfaceObj,@CreateWidget);
  {$endif}
end.

