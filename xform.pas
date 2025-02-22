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
  Classes, SysUtils, Types, NodeUtils,StringUtils, Events,
  {$ifndef JScript}
  Forms, Menus, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, PropEdits, RTTICtrls,
  WrapperPanel, LazsUtils,XScrollBox;
  {$Else}
  HTMLUtils;
  {$endif}

procedure ShowXForm(XFormID:String; modal:Boolean;Namespace:String='');
procedure XFormClose(XFormID:String;Namespace:String='');

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
  fHandleClosure:TEventHandler;
  procedure OnActivateForm(Sender: TObject);
  {$endif}


  function getHeight:integer;
  function getWidth:integer;
  function getTop:integer;
  function getLeft:integer;
  function getCaption:string;
  function getShowing:string;
  function getBgColor:TColor;
  procedure SetHeight(AValue:integer);
  procedure SetWidth(AValue:integer);
  procedure SetTop(AValue:integer);
  procedure SetLeft(AValue:integer);
  procedure SetCaption(AValue:string);
  procedure SetShowing(AValue:string);
  procedure SetBgColor(AValue:TColor);
public
  {$ifndef JScript}
  myEventTypes:TStringList;
  constructor Create(TheOwner: TComponent); override;
  constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
  procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);  virtual;
  {$else}
  constructor Create(NodeName,NameSpace:String);
  {$endif}
  procedure SetFormEventTypes;
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
  property BgColor:TColor read getBgColor write SetBgColor;

  {$ifndef JScript}
  // Events to be visible in Lazarus IDE    **** not working (TXForm as Lazarus component???) ....tbd ****
  property HandleClosure: TEventHandler read FHandleClosure write FHandleClosure;
  {$endif}
end;

  {$ifndef JScript}
  {$else}
procedure OpenModal(NodeName:string;NameSpace:String='');
procedure CloseModal(NodeName:string;NameSpace:String='');
procedure addTheModalBackground(ParentName,WindowId,NodeName,NameSpace:string);
procedure addaModalContentItem(MyName:string;MyNode:TDataNode);
procedure InitialiseXFormStyles();
{$endif}
function InOpenXForms(NodeName,NameSpace:String):integer;
procedure AddOpenXForm(NodeName,NameSpace:String);
procedure DeleteOpenXForm (NodeName,NameSpace:String);

type TOpenXForm = record
  NodeName:String;
  NameSpace:String;
end;
var OpenXForms : array of TOpenXForm;

implementation

var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXForm.SetFormEventTypes;
begin
  {$ifndef JScript}
  MyEventTypes:=TStringList.Create;
  MyEventTypes.Add('Closure');
  {$endif}
  myNode.MyEventTypes:=TStringList.Create;
  myNode.myEventTypes.Add('Closure');
  myNode.InitialiseEventHandlers;
  //SetLength(myNode.myEventHandlers,myNode.myEventTypes.Count);
  //myNode.myEventHandlers[myNode.myEventTypes.Count-1]:=TEventHandlerRec.Create;
end;

function InOpenXForms(NodeName,NameSpace:String):integer;
var
  i:integer;
begin
  result:=-1;
  for i:=0 to length(OpenXForms)-1 do
    if (OpenXForms[i].NodeName = Nodename)
    and (OpenXForms[i].Namespace = NameSpace) then
      result:=i;
end;
procedure AddOpenXForm(NodeName,NameSpace:String);
var
  newrec:TOpenXForm;
begin
  SetLength(OpenXForms,length(OpenXForms)+1);
  newrec.nodeName:=NodeName;
  newrec.NameSpace:=NameSpace;
  OpenXForms[length(OpenXForms)-1]:=newrec;
end;
procedure DeleteOpenXForm (NodeName,NameSpace:String);
var
  i,j:integer;
begin
  for i:=length(OpenXForms)-1 downto 0 do
    if (OpenXForms[i].NodeName = Nodename)
    and (OpenXForms[i].Namespace = NameSpace) then
    begin
      for j:=i+1 to length(OpenXForms)-1 do
        OpenXForms[j-1]:=OpenXForms[j];
      SetLength(OpenXForms,length(OpenXForms)-1);
    end;
end;

{$ifndef JScript}

//function FindFormByName(const AName: string): TForm;      //!!!!namespace??
//var
//  i: Integer;
//begin
//  for i := 0 to Screen.FormCount - 1 do
//  begin
//    Result := Screen.Forms[i];
//    if (Result.Name = AName) then
//      Exit;
//  end;
//  Result := nil;
//end;

function CreateXForm(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
// for dynamic creation of a XForm (Desktop) at runtime.
var
  NewForm:TXForm;
  NewNode,PanelNode:TDataNode;
  TmpPanel:TXScrollBox;
begin

  NewForm :=  TXForm.CreateNew(Application);
  NewForm.name:=NameSpace+ScreenObjectName;    //!! Namespace here....for uniqueness

  CreateComponentDataNode2(NewForm,'TXForm',myDefaultAttribs, nil, NewForm,true);
  NewNode:=NewForm.myNode;
  NewNode.NodeName:=ScreenObjectName;
  NewNode.NameSpace:=NameSpace;
  NewForm.SetFormEventTypes;
  AddChildToParentNode(ParentNode,NewNode,position);

  NewForm.IsContainer:=true;

  result:= NewNode;
end;


procedure ShowXForm(XFormID:String; modal:Boolean;Namespace:String='');
var OldParent:TComponent;
  FormToShow:TXForm;
begin
  FormToShow:=TXForm(Application.FindComponent(Namespace+XFormID));
  if FormToShow<>nil then
  begin
    if (FormToShow<>MainForm) then
    begin
      if modal then
        FormToShow.Showing:='Modal'
      else
        FormToShow.Showing:='Normal';
    end;
  end
  else
    ShowMessage('XForm '+XFormID+' not found');
end;

procedure TXForm.OnActivateForm(Sender: TObject);
begin
  Screen.cursor:=crDefault;
end;

constructor TXForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;
constructor TXForm.CreateNew(AOwner: TComponent; Num: Integer = 0);
begin
  inherited CreateNew(AOwner,Num);
  self.OnClose:=@self.formclose;
  self.OnActivate:=@self.OnActivateForm;
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
procedure addTheModalBackground(ParentName,WindowId,NodeName,NameSpace:string);
var
  OnClickString:String;
begin
  if WindowId = MainForm.Name then
    EXIT;  //wrapper for main form already exists

  OnClickString:='if (event.target == this) {pas.XForm.CloseModal(event.target.id,'''+NameSpace+''');} event.stopPropagation();';
  asm
  try{
  //alert('addTheModalBackground '+WindowId+' '+NodeName+' '+NameSpace);
    $mod.InitialiseXFormStyles()
    var HTMLString = ''
    +'<div id='+WindowId+' class="modal-background '+NameSpace+WindowId+'" '
    +'onmousedown="'+OnClickString+'">'
    +'</div>';

    //----- now append the declarations to the Parent -------------------------------------------
    //alert('looking for parent '+NameSpace+ParentName);
    var ParentItem=document.getElementById(NameSpace+ParentName);
    if (ParentItem==null) {
      ParentItem=document.getElementById(ParentName);
      }
    ParentItem.insertAdjacentHTML('beforeend', HTMLString);

  }catch(err) {alert('Error in XForm.addTheModalBackground '+ err.message);}
  end;

end;

//-------------------------- declaration of the Content Items-----------------------------
procedure addaModalContentItem(MyName:string;MyNode:TDataNode);
var
  ContentName:String;
  bgColor:String;
begin
  ContentName:=MyName+'Contents';
  bgColor:=MyNode.GetAttribute('BgColor',true).AttribValue;
  asm
  try{
  //alert('adding contentitem to '+MyName);
      var HTMLString = ''
      +'  <!-- Form '+MyName+' content -->'
      +'  <div id="'+ContentName+'" class="modal-content '+MyName+'" style="background-color:'+bgColor+'" > '
      +'    <div id="'+MyName+'Caption" ></div> '
      +'  </div>';

      var ParentItem = document.getElementById(MyName);
      ParentItem.innerHTML = ParentItem.innerHTML + HTMLString;

  }catch(err){alert('Error in XForm.addaModalContentItem '+ err.message);}
  end;

end;


procedure OpenModal(NodeName:string;NameSpace:String='');
begin
  asm
  try{
      var modal = document.getElementById(NameSpace+NodeName);
      if (modal!=null) {
        // place this form at the end of the forms list so that it appears 'on top' even if
        // there are other forms already open
        let pp=modal.parentNode;
        pp.appendChild(modal);
        modal.style.display = 'block';
        }
  }catch(err){alert('Error in XForm.OpenModal '+ err.message);}
  end;
  if (NodeName<>MainForm.Name) or (NameSpace<>'') then
    if InOpenXForms(NodeName,NameSpace)<0 then
      AddOpenXForm(NodeName,NameSpace);
end;

procedure CloseModal(NodeName:string;NameSpace:String='');
var
  formNode:TDataNode;
begin
//  showmessage('CloseModal '+NameSpace+NodeName);
  formNode:=FindDataNodeById(SystemNodeTree,NodeName,NameSpace,false);
  if formNode<>nil then
    formNode.SetAttributeValue('Showing','No');
  asm
    var modal = document.getElementById(NameSpace+NodeName);
    if (modal!=null) {
      modal.style.display = "none";   }
  end;

  if InOpenXForms(NodeName,NameSpace)>-1 then
    DeleteOpenXForm(NodeName,NameSpace);

  if formNode<>nil then
    HandleEvent(nil,'Closure',NodeName,NameSpace,'');
end;

procedure ShowXForm(XFormID:String; modal:Boolean;Namespace:String='');
var
  XFormNode:TDataNode;
  XFormObj:TXForm;
begin
  XFormNode:=FindDataNodeById(SystemNodeTree,XFormID,Namespace,true);
  //showmessage('ShowXForm. XFormNode is '+XFormNode.NodeName);
  if XFormNode<>nil then
  begin
    XFormObj:=TXForm(XFormNode.ScreenObject);
    XFormObj.Showing:='Modal';

  end;
end;

constructor TXForm.Create(NodeName,NameSpace:String);
begin
  inherited Create('UI',NodeName,NameSpace,'TXForm',true);
  myNode:=TDataNode(self);
  self.SetFormEventTypes;

  IsContainer:=true;
  SetNodePropDefaults(self,myDefaultAttribs);
end;

function CreateinterfaceObj(MyForm:TForm;Nodename,NameSpace:String):TObject;
var newobj:TObject;
begin
  //showmessage('createinterfaceobj for XForm '+NameSpace+'.'+NodeName);
  newObj:=TObject(TXForm.Create(Nodename,NameSpace));
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

 // showmessage('createinterfaceobj for XForm done');
  result:=newObj;
end;

function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
   ParentName:string;
begin
  //showmessage('createWidget XForm.  '+ScreenObjectName);
  if ParentNode<>nil then
    ParentName:=ParentNode.NodeName
  else
    Parentname:=UIRootNode.NodeName;     //TXForm(MainForm).myNode.NodeName;
  //showmessage('creating form widget '+ScreenObjectname+' parentName='+ParentName);
  asm
  try{
    var wrapperid =  NameSpace+ScreenObjectName;
    $mod.addTheModalBackground(ParentName,wrapperid,ScreenObjectName,NameSpace);
    $mod.addaModalContentItem(wrapperid,MyNode);

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
  //showmessage('createWidget XForm.  done');
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

procedure  XFormClose(XFormID:String;NameSpace:String='');
var
  FormToClose:TForm;
  formNode:TdataNode;
begin
  formNode:=FindDataNodeByid(SystemNodeTree,XFormID,NameSpace,false);
  if formNode<>nil then
  begin
    TXForm(formNode.ScreenObject).Showing:='No';
  end
  else
  begin
    {$ifndef JScript}
    FormToClose:=TForm(Application.FindComponent(XFormID));  //!!!! does this ever happen?
     if FormToClose<>nil then
         FormToClose.Close;
    {$else}
    showmessage('problem closing '+XFormID);
    CloseModal(XFormID,NameSpace);
    {$endif}
  end;
end;

function TXForm.GetShowing:string;
begin
  if myNode<>nil then
    result:=myNode.GetAttribute('Showing',true).AttribValue
  else
    result := 'No';
  if (result='') and (self<>MainForm) then result:='No';
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
      DeleteOpenXForm(self.myNode.NodeName,self.myNode.NameSpace);
      HandleEvent(nil,'Closure',myNode.NodeName,myNode.NameSpace,'');
    end
    else
    begin
      //showmessage('TXForm '+self.Name+' SetShowing '+AValue);
       if (self.myNode.NodeName<>MainForm.Name) or (self.myNode.NameSpace<>'') then
         if InOpenXForms(self.myNode.NodeName,self.myNode.NameSpace)<0 then
           AddOpenXForm(self.myNode.NodeName,self.myNode.NameSpace);
      if self.Visible = false then
        if AValue='Modal' then
          self.showmodal
        else if AValue='Normal' then
          self.Show;

    end;
    {$else}
    if self.NodeName<>'' then
      begin
      //showmessage('TXForm SetShowing '+AValue+' for node '+self.NodeName);
      if AValue='No' then
      begin
        CloseModal(self.NodeName,self.NameSpace);
      end
      else
      begin
        self.Top:=self.Top;
        self.Left:=Self.Left;
        //showmessage('TXForm '+self.NodeName+' SetShowing '+AValue);
        asm
        try{
           var modalwindowid= this.NodeName;
           var NameSpace=this.NameSpace;
           //alert('open windowid='+NameSpace+modalwindowid);
           var modal = document.getElementById(NameSpace+modalwindowid);
           // alert('found '+modal);
           modal.style.display = 'block';
        }catch(err){alert('Error in XForm.SetShowing '+ err.message);}
        end;
        if (self.NodeName<>MainForm.Name) or (self.NameSpace<>'') then
          if InOpenXForms(self.NodeName,self.NameSpace)<0 then
            AddOpenXForm(self.NodeName,self.NameSpace);
      end;
    end;
    {$endif}
  end;
end;


procedure TXForm.SetHeight(AValue:integer);
begin
  //showmessage('TXForm SetHeight '+inttostr(AValue));
  if myNode<>nil then
    myNode.SetAttributeValue('Height',intToStr(AValue));
  {$ifndef JScript}
  inherited Height := AValue;
  {$else}
  //showmessage('TXForm '+myNode.NodeName+' SetHeight '+inttostr(AValue));
  asm
    var ob=document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
      var str=AValue;
      if (AValue==0) str='99%';
      pas.HTMLUtils.SetHeightWidthHTML(this,ob,'H',str);
    }
  end;
  {$endif}
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
    var ob=document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
      var str=AValue;
      if (AValue==0) str='99%';
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
    var ob=document.getElementById(this.NameSpace+this.NodeName+'Contents');
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
    var ob=document.getElementById(this.NameSpace+this.NodeName+'Contents');
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


procedure TXForm.SetCaption(AValue:string);
begin
  //showmessage('TXForm SetCaption');
  if myNode<>nil then
    myNode.SetAttributeValue('Caption',AValue);
  {$ifndef JScript}
  inherited Caption:=AValue;
  {$else}
  asm
    var ob=document.getElementById(this.NameSpace+this.NodeName+'Caption');
    if (ob!=null) {
      ob.innerHTML=AValue;
    }
  end;
  {$endif}
end;

function TXForm.GetBgColor:TColor;
begin
  {$ifndef JScript}
  if myNode<>nil then
    result:=HexRGBToColor(myNode.GetAttribute('BgColor',true).AttribValue);
  {$else}
  result:=GetAttribute('BgColor',true).AttribValue;
  {$endif}
end;
procedure TXForm.SetBgColor(AValue:TColor);
var str:string;
begin
  {$ifndef JScript}
  if myNode<>nil then
  begin
    str:= ColorToHexRGB(AValue);
    myNode.SetAttributeValue('BgColor',str,'Color');
  end;
  Color:=AValue;

  {$else}
  SetAttributeValue('BgColor',AValue,'Color');
  asm
  try {
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
      ob.style.backgroundColor = AValue;  }
    } catch(err) { alert(err.message+'  in XForm.SetBgColor'); }
  end;
  {$endif}
end;


begin
  // this is the set of node attributes that each TXForm instance will have.
  AddDefaultAttribute(myDefaultAttribs,'Showing','String','No','',false,false);
  AddDefaultAttribute(myDefaultAttribs,'Width','Integer','400','',false);
  AddDefaultAttribute(myDefaultAttribs,'Height','Integer','300','',false);
  AddDefaultAttribute(myDefaultAttribs,'Top','Integer','50','',false);
  AddDefaultAttribute(myDefaultAttribs,'Left','Integer','50','',false);
  AddDefaultAttribute(myDefaultAttribs,'Caption','String','My Title','',false);
  AddDefaultAttribute(myDefaultAttribs,'BgColor','Color','#FFFFFF','',false);

  setLength(OpenXForms,0);
  {$ifndef JScript}
  RegisterClass(TXForm);
  AddNodeFuncLookup('TXForm',@CreateXForm);
  {$else}
  AddNodeFuncLookup('TXForm',@CreateinterfaceObj,@CreateWidget);
  {$endif}
end.

