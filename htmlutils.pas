(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit HTMLUtils;
interface
{$ifndef JScript}
implementation
end.
{$else}
uses Classes, SysUtils, StringUtils, NodeUtils
  ;


procedure removeClassName(el:TObject; ClassName:String);
procedure StopBubbling(event:TObject);
function addHandVBoxStyles():string;
//procedure ClearAllScreenObjects;
function getAncestorWithTagAndClass(node:TObject; tagName, className:String):TObject;
function getPageOffsetLeft(el:TObject):Integer;
function getPageOffsetTop(el:TObject):Integer;
function hasClassName(el:TObject; name:String):Boolean;
function PrepareHeightWidthHTML(var HW,StrVal,StyleVal:string):Boolean;
procedure SetHeightWidthHTML(MyNode:TDataNode; ob:TObject; HW,AttrValue:string);
function DeleteScreenObject(MyNode:TDataNode):string;
function CreateWrapperHtml(NewNode,ParentNode:TDataNode;ClassName,ScreenObjectName,ScreenObjectType:string):string;
function CreateWrapperDiv(MyNode,ParentNode:TDataNode;NodeClass,ScreenObjectName,ScreenObjectType:string;position:integer ):TObject;
procedure AddObjectToParentObject(ParentNode:TDataNode;ParentId,myId:String;position:integer;HTMLString:string);
function ScreenObjectInnerComponent(SystemNode:TDataNode):TObject;
procedure UnHighlight(ObjID:string; HadBorder:boolean);
procedure Highlight(ObjID:string);
procedure ShowHideSelectedBorder(myNode:TDataNode;showborder:Boolean);
function GetDataNodeFromTreeNode(nodeID:string):TDataNode;
function getParentByTagName(topname:String;node:TObject;tagname:String):TObject;
function ContainsChildWithTag(node:TObject;tagname:String):Boolean;
procedure WriteToLocalStore(KeyName,TheData:String);
function ReadFromLocalStore(KeyName:String):String;
procedure ClearLocalStore(KeyName:String);
function GetCurrentHeight(ObjectName:String):integer;
function GetCurrentWidth(ObjectName:String):integer;
procedure ShowGreyOverlay(ParentName,WindowId:String);
procedure DeleteGreyOverlay(DivId:String);


const
  glbBorderWidth:integer = 3;
  glbLabelSpacing:integer = 3;
  glbMarginSpacing:string = '3px';

var browser:TObject;


implementation
uses XForm ,XIframe, XSVGContainer ;

procedure IdentifyBrowser;
begin

asm
function Browser()
{

  var ua, s, i;

  this.isIE    = false;  // Internet Explorer
  this.isOP    = false;  // Opera
  this.isNS    = false;  // Netscape
  this.version = null;

  ua = navigator.userAgent;

  s = "Opera";
  if ((i = ua.indexOf(s)) >= 0) {
    this.isOP = true;
    this.version = parseFloat(ua.substr(i + s.length));
    return;
  }

  s = "Netscape6/";
  if ((i = ua.indexOf(s)) >= 0) {
    this.isNS = true;
    this.version = parseFloat(ua.substr(i + s.length));
    return;
  }

  // Treat any other "Gecko" browser as Netscape 6.1.

  s = "Gecko";
  if ((i = ua.indexOf(s)) >= 0) {
    this.isNS = true;
    this.version = 6.1;
    return;
  }

  s = "MSIE";
  if ((i = ua.indexOf(s))) {
    this.isIE = true;
    this.version = parseFloat(ua.substr(i + s.length));
    return;
  }
}
//alert('IdentifyBrowser');
pas.HTMLUtils.browser = new Browser();
//alert('IdentifyBrowser done.  isIE='+pas.HTMLUtils.browser.isIE);
end;

end;

procedure removeClassName(el:TObject; ClassName:String);
begin
asm
{

  var i, curList, newList;

  if (el.className == null)
    return;

  // Remove the given class name from the element's className property.

  newList = new Array();
  curList = el.className.split(" ");
  for (i = 0; i < curList.length; i++)
    if (curList[i] != ClassName)
      newList.push(curList[i]);
  el.className = newList.join(" ");
}
end;
end;


function addHandVBoxStyles():string;
var dummy:integer;
begin
dummy:=0;

asm
   // ----------------------------------------check if the style has already been set
    var x = document.getElementsByTagName("STYLE");
    var StyleIsSet = false;
    if (x.length>0){
      for (var i=0; i<x.length; i++){
        var y= x[i].innerHTML;
        if (y.indexOf(".hbox") !=-1) { StyleIsSet =true}
      }
    }
   
    if (StyleIsSet == false){
        var HandVBoxStyleString = '<style>'
   //     +'div.outline: none !important; '
        +'.hbox { '
            +' display: -webkit-flex;'
            +' display: -ms-flexbox;'
            +' display: -flex;'
            +' -webkit-flex-direction: row;'
            +' -ms-flex-direction: row;'
            +' flex-direction: row;'
            +' -webkit-align-content: stretch;'
            +' -ms-flex-line-pack: stretch;'
            +' align-items: stretch;'
            +' }'
       +'.hboxNoStretch { '
            +' display: -webkit-flex;'
           +' display: -ms-flexbox;'
           +' display: -flex;'
           +' -webkit-flex-direction: row;'
           +' -ms-flex-direction: row;'
           +' flex-direction: row;'
           +' -webkit-align-content: flex-start;'
           +' -ms-flex-line-pack: start;'
           +' align-items: flex-start;'
           +' }'

      +'.vbox { '
             +' display: -webkit-flex;'
             +' display: -ms-flexbox;'
             +' display: flex;'
              +' -webkit-flex-direction: column;'
              +' -ms-flex-direction: column;'
              +' flex-direction: column;'
              +' -webkit-align-content: stretch;'
              +' -ms-flex-line-pack: stretch;'
              +' align-items: stretch;'
              +' }'
            +'.vboxNoStretch { '
              +' display: -webkit-flex;'
              +' display: -ms-flexbox;'
              +' display: flex;'
               +' -webkit-flex-direction: column;'
               +' -ms-flex-direction: column;'
               +' flex-direction: column;'
               +' -webkit-align-content: flex-start;'
               +' -ms-flex-line-pack: start;'
               +' align-items: flex-start;'
               +' }'
           +'.vboxNoFlex { '
               +' display: inline-block;'
               +' -webkit-flex-direction: column;'
               +' -ms-flex-direction: column;'
               +' flex-direction: column;'
               +' -webkit-align-content: flex-start;'
               +' -ms-flex-line-pack: start;'
               +' align-items: flex-start;'
               +' }'


        +'.AlignmentCentre {display: flex;'
           +'align-items: center;'
           +'align-self: center;'
           +'flex-shrink: 0; '
           +'justify-content: start;}'             //center?
        +'.AlignmentRight {display:flex;'
         +'align-items: flex-e'+'nd;'
          +'align-self: flex-e'+'nd;'
          +'flex-shrink: 0; '
          +'justify-content: flex-e'+'nd;'
          +'float:right;'
          +'}'
        +'.AlignmentLeft {display:flex;'
          +'align-items: flex-start;'
          +'align-self: flex-start;'
          +'flex-shrink: 0; '
          +'justify-content: flex-start;}'
        +'.AlignmentLeftContainer {display:flex;'
          +'align-items: flex-start;'
          +'align-self: stretch;'
          +'flex-shrink: 0; '
            +'justify-content: flex-start;}'
        +'.AlignmentTop {display:flex;'
        +'align-items: flex-start;'
        +'align-self: flex-start;'
          +'justify-content: flex-start;}'
        +'.AlignmentBottom {display:flex;'
          +'align-items: flex-e'+'nd;'
          +'align-self: flex-e'+'nd;'
          +'justify-content: flex-e'+'nd;}'

        +'  input {'
                +' line-height: 20px;'
             +'}'


           +' </style>';

      //----------------------------- now append the style declarations to the head of the HTML page
      document.head.innerHTML = document.head.innerHTML+HandVBoxStyleString;
     }
     end;

end;


function hasClassName(el:TObject; name:String):Boolean;
begin
asm
{

  var i, list;

  // Return true if the given element currently has the given class
  // name.

  list = el.className.split(" ");
  for (i = 0; i < list.length; i++)
    if (list[i] == name)
      return true;

  return false;
}
end;
end;

procedure StopBubbling(event:TObject);
begin
asm
  if (pas.HTMLUtils.browser.isIE)
    window.event.cancelBubble = true;
  else
    event.stopPropagation();
end;
end;


function getAncestorWithTagAndClass(node:TObject; tagName, className:String):TObject;
begin
asm
{

  // Starting with the given node, find the nearest containing element
  // with the specified tag name and style class.

  while (node != null) {
    if (node.tagName != null && node.tagName == tagName &&
        pas.HTMLUtils.hasClassName(node, className))
      return node;
    node = node.parentNode;
  }

  return node;
}
end;
end;

function getPageOffsetLeft(el:TObject):Integer;
var
  x:integer;
begin
asm
{
function getOffset(el) {
  const rect = el.getBoundingClientRect();
  return {
    left: rect.left + window.scrollX,
    top: rect.top + window.scrollY
  };
}
  // Return the x coordinate of an element relative to the page.

//  x = el.offsetLeft;
//  if (el.offsetParent != null) {
//    x += pas.HTMLUtils.getPageOffsetLeft(el.offsetParent);
//    }
x=getOffset(el).left;
}
end;
  result:=x;
end;

function getPageOffsetTop(el:TObject):Integer;
var
  y:integer;
begin
asm
{
  // Return the y coordinate of an element relative to the page.

  y = el.offsetTop;
  if (el.offsetParent != null)
    y += pas.HTMLUtils.getPageOffsetTop(el.offsetParent);
}
end;
result:=y;
end;


function CheckMatch(Instring,teststring:string;startpos:integer):boolean;
var i:integer;
  match:boolean;
  temp1,temp2:string;
begin
   match:=true;
   for i:= 1 to Length(testString) do
   begin
      if (i+startpos-1)<= Length(Instring) then
      begin
        temp1:=Instring[i+startpos-1];
        temp2:=teststring[i] ;
        if temp1<>temp2
        then match:=false;
      end
      else match:=false;
   end;
   result := match;
end;

function PrepareHeightWidthHTML(var HW,StrVal,StyleVal:string):Boolean;
var
  hw1:string;
  pct:Boolean;
begin
  // prepare height and width for html
  pct:=false;
  if (FoundString(StrVal,'%')>0) then
    pct:=true;
  if (FoundString(StrVal,'px')>0) or (pct=true) then
    hw1:=StrVal
  else if StrVal<>'' then
    hw1:=StrVal+'px'
  else
     hw1:='';
  StrVal:=hw1;

  if HW='H' then
  begin
    StyleVal:= 'height:'+hw1+';' ;
    if (FoundString(hw1,'px')>0) then
      StyleVal := StyleVal + 'max-height:'+hw1+';';
  end;
  if HW='W' then
  begin
    StyleVal:= 'width:'+hw1+';' ;
    if (FoundString(hw1,'px')>0) then
      StyleVal := StyleVal + 'max-width:'+hw1+';';
  end;
  result:=pct;
end;

procedure SetHeightWidthHTML(MyNode:TDataNode; ob:TObject; HW,AttrValue:string);
var
  hwStr:string;
  StyleHW,StyleHW0:string;
  pct:Boolean;
begin
  pct:=PrepareHeightWidthHTML(HW,AttrValue,StyleHW0);
  hwStr:=AttrValue;
 // StyleHW:=StyleHW0;
  asm
    if (ob!=null) {
      if (HW=='H') {
        ob.style.height=hwStr;
        if (pct==false) {
          ob.style.minHeight=hwStr;
          ob.style.maxHeight=hwStr;
        }
        else
        {
        ob.style.minHeight='';
        ob.style.maxHeight='';
        }
      }
      else {
        ob.style.width=hwStr;
        if (pct==false) {
          ob.style.minWidth=hwStr;
          ob.style.maxWidth=hwStr;
        }
        else
        {
          ob.style.minWidth='';
          ob.style.maxWidth='';
       }
      }
    }
  end;

end;

function DeleteScreenObject(MyNode:TDataNode):string;
var
    ObjName:string;
begin
   ObjName:=MyNode.NodeName;

  asm
    try{
    var ThisObject = document.getElementById(ObjName);
    if (ThisObject!=null) {
       ThisObject.parentNode.removeChild(ThisObject);
      }
    }catch(err) { alert(err.message+' in HTMLUtils.DeleteScreenObject');}
  end;

  NilScreenObject(MyNode);

end;
(*
procedure ClearAllScreenObjects;
var
    SystemRootNameVar:string;
begin
  SystemRootNameVar:=SystemRootName;
  showmessage('ClearAllScreenObjects');
  asm
try{
       var self=document.getElementById(SystemRootNameVar);
       while (self.firstChild)
         { self.removeChild(self.firstChild);}
    }catch(err) { alert(err.message+'  in HTMLUtils.ClearAllScreenObjects');}

end;
//  showmessage('ClearAllScreenObjects done');
end;
*)

procedure AddObjectToParentObject(ParentNode:TDataNode;ParentId,myId:String;position:integer;HTMLString:string);
var
  pos:integer;
  mysib:TDataNode;
begin
     pos:=position;
     if pos > 0 then
     begin
       // NB.  System node for the new element has already been inserted at the relevant position
       if length( ParentNode.ChildNodes)>pos+1 then  // eg. 3 children.  position=1. sib is 2.
       begin
         //ShowMessage('looking for sib at '+ inttostr(position+1)+' parentid='+ParentId);
         mysib:= ParentNode.ChildNodes[pos+1] ;
         //ShowMessage('sib is '+ mysib.NodeName);
         if mysib.ScreenObject=nil then
         begin
            pos:=-1;
            //showmessage('ScreenObject nil');
         end;
       end
       else
       begin
         if pos > length( ParentNode.ChildNodes)-1 then
           ShowMessage('cannot insert under '+ParentNode.NodeName+ ' at position '+IntToStr(pos)+'. reverted to end');
         pos:=-1;
       end;
     end;

     asm
       try {
         var myParent=document.getElementById(ParentId);
           // Insert the new container under the given parent, at the correct sibling position
           if (pos==-1)  {
           myParent.insertAdjacentHTML('beforeend', HTMLString);
           }
           else if ( pos==0) {
           myParent.insertAdjacentHTML('afterbegin', HTMLString);
           }
           else {
             var mySibling=document.getElementById(mysib.NodeName);
             if (mySibling!=null) {
               mySibling.insertAdjacentHTML('beforebegin', HTMLString);
             }
             else {
               // insert msg here.... (1)
               var str=sibname;
               alert(str);
               myParent.insertAdjacentHTML('beforeend', HTMLString);
             }
             }
        } catch(err) { alert(err.message+'  in HTMLUtils.AddObjectToParentObject');}
      end;

           (*  (1) *************** this causes compile error : String exceeds end of line  £££££££why????
                str='sibling ' + str + ' not found. inserting at end';
                *)

end;

function ScreenObjectInnerComponent(SystemNode:TDataNode):TObject;
var
  innername:string;
begin
   begin
     innername:=SystemNode.NodeName+'Contents';
     asm
       Result=document.getElementById(innername);
     end;
     if Result=nil then
     asm
       Result=document.getElementById(SystemNode.NodeName);
     end;
     if Result=nil then
       ShowMessage('object '+ SystemNode.NodeName + ' not found in HTMLUtils.ScreenObjectInnerComponent') ;
   end;
end;


function CreateWrapperHtml(NewNode,ParentNode:TDataNode;ClassName,ScreenObjectName,ScreenObjectType:string):string;
var
  Border:String;
begin
  Border:=NewNode.GetAttribute('Border',true).AttribValue;

asm
try{

    // note tabindex=0 allows a div to be focused.  Only the focused element will listen to keyboard events.

    var ClassString =' class="';
    if (Border=='True') {ClassString = ClassString+' normal-border '+ClassName;}
    else  {ClassString = ClassString+' no-border '+ClassName;}

    ClassString= ClassString + '" ';

    var ComponentHTML='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var componentClick="'Click'";

    var WrapperStyle = ' background-color:inherit; white-space:nowrap; ';

    var FullHTMLString='<div '+ClassString+' style="'+WrapperStyle+'" tabindex="0" id='+ScreenObjectName+
                ' onclick="event.stopPropagation(); pas.Events.handleEvent(null,'+componentClick+','+NodeIDString+', this.value);" '+
                   ' </div> ';

  }catch(err) { alert(err.message+'  in HTMLUtils.CreateWrapperHtml');}

  return FullHTMLString;
end;

end;

function CreateWrapperDiv(MyNode,ParentNode:TDataNode;NodeClass,ScreenObjectName,ScreenObjectType:string;position:integer ):TObject;
var
  bdr:string;
  ShowBorder:boolean;
begin
  Bdr:= MyNode.getAttribute('Border',true).AttribValue;
  if Bdr<>'' then
    ShowBorder:=MyStrToBool(Bdr)
  else
    Showborder:=false;

  asm
    try {
       var MyParent = pas.HTMLUtils.ScreenObjectInnerComponent(ParentNode);
 //      alert('adding '+MyNode.NodeName+' to '+MyParent.id);
       var HTMLImplementation = pas.HTMLUtils.CreateWrapperHtml(MyNode,ParentNode,'',ScreenObjectName,ScreenObjectType);
       pas.HTMLUtils.AddObjectToParentObject(ParentNode,MyParent.id,ScreenObjectName,position,HTMLImplementation);
       var wrapper=document.getElementById(ScreenObjectName);
       if ((wrapper.style.overflow!='scroll') && (ScreenObjectType!='TXMainMenu')  && (ScreenObjectType!='TXMenuItem'))
       {
          wrapper.style.overflow = 'hidden';
       }
       if (ShowBorder==true) {
          wrapper.classList.add("normal-border");
       }

       return wrapper;

   } catch(err) { alert(err.message+'  in HTMLUtils.CreateWrapperDiv');}
  end;

end;

procedure UnHighlight(ObjID:string; HadBorder:boolean);
begin

    asm
      try{
     // alert('unhighlight '+ObjID);
      var ob=document.getElementById(ObjID)
      if (ob!=null) {
        ob.classList.remove("highlight-border");

        if (HadBorder=='True') {
           ob.classList.add("normal-border");
           }
      }
      }catch(err) { alert(ObjID+': '+err.message+'  in HTMLUtils.UnHighlight'); }
    end;

end;

procedure Highlight(ObjID:string);
begin

    asm
    try{
    //alert('Highlight '+ObjID);
    var ob=document.getElementById(ObjID);
    if (ob!=null) {
      ob.classList.remove("normal-border");
      ob.classList.remove("no-border");
      ob.classList.add("highlight-border");
    }
    }catch(err) { alert(err.message+'  in HTMLUtils.Highlight'); }
    end;

end;

procedure ShowHideSelectedBorder(myNode:TDataNode;showborder:Boolean);
var
  HadBorder:Boolean;
begin
  //showmessage('ShowHideSelectedBorder '+myNode.NodeType+'  '+myBoolToStr(showborder));
  if myNode.GetAttribute('Border',true).AttribValue <> '' then
     HadBorder:=myStrToBool(myNode.GetAttribute('Border',true).AttribValue)
  else
     HadBorder:=false;

  asm
    var ob = document.getElementById(this.NodeName);
    if (ob!=null) {
    if (showborder==true) {
       pas.HTMLUtils.Highlight(ob.id);
    }
    else {
       pas.HTMLUtils.UnHighlight(ob.id, HadBorder);
    }      }
  end;

  //......special case - SVG Container has to re-generate ..........
  if myNode.NodeType='TXSVGContainer' then
  begin
    TXSVGContainer(myNode).XMLString:=TXSVGContainer(myNode).XMLString;
  end;

end;

function GetDataNodeFromTreeNode(nodeID:string):TDataNode;
var
  bits:TStringList;
begin
  bits:=stringsplit(nodeID,'Contents');
  result:=FindDataNodeById(SystemNodeTree,bits[0],true);
end;

function getParentByTagName(topname:String;node:TObject;tagname:String):TObject;
var
  foundOb:TObject;
begin
  asm
    {
      foundOb=null;
      var parent;
      if (node == null || tagname == '') {foundOb=null}
      else
      {
        parent  = node.parentNode;
        tagname = tagname.toUpperCase();
        //alert('looking for tagname '+tagname);

        while ((parent.id != topname+'Contents')&&(foundOb==null)) {
          //alert('parent.id='+parent.id + 'tagname='+parent.tagName);
	  if (parent.tagName == tagname) {
		  foundOb = parent;
	  }
          else {
	    parent = parent.parentNode;
          }
        }

        foundOb = parent;
      }
  }
  end;
  result:=foundOb;
end;

function ContainsChildWithTag(node:TObject;tagname:String):Boolean;
var
  found:Boolean;
begin
  found:=false;
  asm
  if (node == null || tagname == '') {found=false}
  else
  {
    tagname = tagname.toUpperCase();

    for (var i=0; i<node.children.length; i++) {
      if (node.children[i].tagName==tagname) {found=true;}
    }
  }
  end;
  result:=found;
end;

procedure WriteToLocalStore(KeyName,TheData:String);
begin
  if KeyName<>'' then
  begin
    asm
    try{
    localStorage.setItem(KeyName,TheData);
    }catch(err) { alert(err.message+'  in HTMLUtils.WriteToLocalStore'); }
    end;
  end;
end;

function ReadFromLocalStore(KeyName:String):String;
var
  TheData:String;
begin
  TheData:='';
  if KeyName<>'' then
  begin
    asm
    try{
    TheData=localStorage.getItem(KeyName);
    if (TheData==null) {TheData='';}
    }catch(err) { alert(err.message+'  in HTMLUtils.ReadFromLocalStore'); }
    end;
  end;
  result:=TheData;
end;

procedure ClearLocalStore(KeyName:String);
begin
  if KeyName<>'' then
  begin
    asm
    try{
    localStorage.removeItem(KeyName);
    }catch(err) { alert(err.message+'  in HTMLUtils.ClearLocalStore'); }
    end;
  end;
end;

function GetCurrentHeight(ObjectName:String):integer;
var
  h:integer;
begin
  h:=0;        //have to initialise else pas2js deletes it
  asm
    var ob = document.getElementById(ObjectName);
    if (ob!=null) {
      var style = window.getComputedStyle(ob);
      var hh = style.height;
      h = parseInt(hh, 10);
  //     alert('calculated height='+h);
      }
  end;
  result:=h;
end;

function GetCurrentWidth(ObjectName:String):integer;
var
  w:integer;
begin
  w:=0;           //have to initialise else pas2js deletes it
  asm
    var ob = document.getElementById(ObjectName);
    if (ob!=null) {
      var style = window.getComputedStyle(ob);
      var ww = style.width;
      w = parseInt(ww, 10);
  //      alert('calculated Width='+w);
      }
  end;
  result:=w;
end;

procedure ShowGreyOverlay(ParentName,WindowId:String);
begin
asm
try{
//alert('ShowGreyOverlay '+WindowId);
  pas.XForm.InitialiseXFormStyles();
  var HTMLString = ''
  +'<div id='+WindowId+' class="modal-background" style="display:block; cursor:progress;" '
  +'>Please wait...</div>';

  var ParentItem=document.getElementById(ParentName);
  ParentItem.insertAdjacentHTML('beforeend', HTMLString);

}catch(err) {alert('Error in HTMLUtils.ShowGreyOverlay '+ err.message);}
end;
end;

procedure DeleteGreyOverlay(DivId:String);
begin
asm
  var ob = document.getElementById(DivId);
  if (ob!=null) {
    ob.parentNode.removeChild(ob);
    }
end;
end;

type TKeyValue = record
   key:String;
   val:String;
   end;
type TKeyValueArray=array of TkeyValue;

function StoredSystemsArray:TKeyValueArray;
var
  arr:TKeyValueArray;
begin
  setlength(arr,0);
  asm
    var archive = [],
        keys = Object.keys(localStorage),
        i = 0, key;

    for (; key = keys[i]; i++) {
        archive.push( key + '=' + localStorage.getItem(key));
    }

    arr = archive;
  end;
end;

begin
IdentifyBrowser;
end.
{$endif}



