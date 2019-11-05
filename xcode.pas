(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *)
unit XCode;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,TypInfo, NodeUtils, StringUtils,
  {$ifndef JScript}
  LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, Propedits,RTTICtrls,
  LazsUtils, Events, SynEdit, SynEditTypes, SynHighlighterPas,
  {$else}
  HTMLUtils,
  {$endif}
  WrapperPanel;

{$ifdef JScript}
type TTextContext = record
  IsInMultiLineCommentString:Boolean;
  IsInAltMultiLineCommentString:Boolean;
  IsInASMMultiLineCommentString:Boolean;
  MultiLineCommentChangedStatus:Boolean;
end;
{$endif}

type
  TXCode = class(TWrapperPanel)
  private
    { Private declarations }
    {$ifndef JScript}
    fHandleClick:TEventHandler;
    fHandleChange:TEventHandler;
    fHandleClickMessage:TEventHandler;
    fEditor:TSynEdit;
    fMessages:TMemo;

    procedure MemoClick(Sender:TObject);
    procedure MemoChange(Sender: TObject);
    procedure ClickMessage(Sender: TObject);
   {$endif}

    procedure SetMyEventTypes;

    function GetItemValue:string;
    function GetReadOnly:Boolean;
    function GetMessagesHeight:string;
    function GetMessageLines:string;

    procedure SetItemValue(AValue:string);
    procedure SetReadOnly(AValue:Boolean);
    procedure SetMessagesHeight(AValue:string);
    procedure SetMessageLines(AValue:string);

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
    // variables used in the text formatting functions (see DoKeyUp)
    context:TTextContext;
    blackoutline:TstringArray;
    boldoutline:TstringArray;
    redoutline:TstringArray;
    greenoutline:TstringArray;
    blueoutline:TstringArray;
    IsInitalised:Boolean;
    LengthOfRangeSelected:integer;
    SavedLineContextArray:TstringArray;
    topSave:integer;
    {$endif}

    {$ifndef JScript}
    function GetMessage(Var MessageFound:Boolean;SelectedLine,delim:string):string;

    property TheEditor:TSynEdit read fEditor write fEditor;
    property TheMessages:TMemo  read fMessages write fMessages;
    {$else}
    procedure GoToCharPos(CharPos:integer);
    function LineNumToYPixel(elem:TObject; LineNum:integer):integer;
    function CharPosToYPixel(elem,LinesArray:TObject; CharPos:integer; var xpos,lNum:integer):integer;
    procedure ResetScrollPos(obj:TObject;topSave:integer);

    {$endif}
    procedure GetFileNameLineNumAndCharPos(Var MessageFound:Boolean;SelectedLine,delim:string;
                       var FileName,LineNum,CharNum:string);
    procedure GoToLineCharPos(LineNum,CharPos:integer);
    procedure AddMessage(txt:String);

  published
    { Published declarations }

    // Properties defined for this class...
    property ItemValue: String read GetItemValue write SetItemValue;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property MessagesHeight: String read GetMessagesHeight write SetMessagesHeight;
    property MessageLines:String read GetMessageLines write SetMessageLines;

    {$ifndef JScript}
    // Events to be visible in Lazarus IDE
    property HandleClick: TEventHandler read FHandleClick write FHandleClick;
    property HandleChange: TEventHandler read FHandleChange write FHandleChange;
    property HandleClickMessage: TEventHandler read FHandleClickMessage write FHandleClickMessage;
    {$endif}
  end;


  {$ifndef JScript}
  procedure Register;
  {$endif}


  {$ifdef JScript}
  procedure AddCodeEditorStyles;
  procedure DoKeyUp(myId:String;NodeId,NameSpace:String;event:TObject);
  procedure DoKeyDown(myId:String;NodeId,NameSpace:String;event:TObject);
  procedure SyncScroll(myId:String);
  function suppressScrolling(e:TObject):integer;
  function GetTextLineHeight(element:TObject):integer;
  {$endif}

const  glbLineHeight='1.5';

implementation

const MyNodeType='TXCode';

var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXCode.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
  MyEventTypes.Add('Change');
  MyEventTypes.Add('ClickMessage');
end;

{$ifndef JScript}
var SynPasSynMain: TSynPasSyn;


procedure Register;
begin
  {$I xcode_icon.lrs}
  RegisterComponents('XComponents',[TXCode]);

  // inherited from TWrapperPanel, not required here
  RegisterPropertyEditor(TypeInfo(TColor), TXCode, 'BgColor', THiddenPropertyEditor);

end;

constructor TXCode.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXCode.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXCode.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
var
  syned:TSynEdit;
  messgs:TMemo;
begin
  if SynPasSynMain=nil then
  begin
    SynPasSynMain:= TSynPasSyn.Create(nil);
    SynPasSynMain.CommentAttri.foreground:=clgreen;
    SynPasSynMain.CommentAttri.Style:=[fsItalic];
    SynPasSynMain.StringAttri.foreground:=clblue;
  end;

  self.BorderSpacing.Around:=glbBorderSpacing;

  myControl:=TPanel.Create(self);
  myControl.Parent:=self;


  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];

  syned:=TSynEdit.Create(self);
  syned.Parent:=TPanel(myControl);
  syned.SetSubComponent(true);
  syned.ControlStyle := syned.ControlStyle - [csNoDesignSelectable];

  syned.OnExit:=@self.MemoChange;
  syned.OnClick:=@self.MemoClick;

  syned.Highlighter:= SynPasSynMain;

  syned.ScrollBars:=ssAutoBoth;

  TheEditor:=syned;

  messgs:=TMemo.Create(self);
  messgs.Parent:=TPanel(myControl);
  messgs.SetSubComponent(true);
  messgs.ControlStyle := messgs.ControlStyle - [csNoDesignSelectable];
  messgs.ScrollBars:=ssAutoBoth;
  messgs.OnClick:=@self.ClickMessage;
  TheMessages:=messgs;

  myControl.Align:=alClient;      //panel
  TheEditor.Align:=alClient;      //synedit
  TheMessages.Align:=alBottom;    //memo
  myControl.Tag:=-1;

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
  NewNode:=CreateDynamicLazWidget('TXCode',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;

function TXCode.GetMessage(Var MessageFound:Boolean;SelectedLine,delim:string):string;
var     List1: TStringList;
        resultstring:string;
begin
  resultstring:='';
  MessageFound:=false;
  List1 := TStringList.Create;
  try
    List1.LineBreak := delim;
    List1.Text := SelectedLine;
    if List1.count >1 then
    begin
      resultstring:= delim+ List1[1];
      MessageFound:=true;
    end;
  finally
    List1.Free;
  end;
  result:=resultstring;
end;

procedure TXCode.MemoClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;

procedure TXCode.ClickMessage(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('ClickMessage',self.myNode.NodeName,self);
end;

procedure TXCode.MemoChange(Sender: TObject) ;
 var
    Code: TSynEdit ;
    txt:String;
    i:integer;
 begin
    Code := TSynEdit(sender) ;
    for i:=0 to Code.Lines.count - 1 do
    begin
      txt:=txt+Code.Lines[i]+LineEnding;
    end;

    self.ItemValue:=txt;
    CallHandleEvent('Change',Code.text,Sender);
 end;

procedure TXCode.SetMessagesHeight(AValue:string);
 var
   tc:TControl;
 begin
  tc:=self.TheMessages;
  myNode.SetAttributeValue('MessagesHeight',AValue);
  SetHeightWidth(self.myNode,tc,'EditorWidth','MessagesHeight');
  tc.Update;
end;

{$else}

constructor TXCode.Create(MyForm:TForm;NodeName,NameSpace:String);
begin
  inherited Create(NodeName,NameSpace);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=false;
  self.topSave:=0;

  SetNodePropDefaults(self,myDefaultAttribs);

end;

function suppressScrolling(e:TObject):integer;
var
  top:integer;
begin
asm
  top = 0;
        var key = (e.keyCode ? e.keyCode : e.which);
        //console.log('key='+key);
        if(key == 13) {
            //e.preventDefault();

            top = e.target.scrollTop;
                    }
        //console.log('top='+top);

end;
result:=top;
end;

procedure TXCode.ResetScrollPos(obj:TObject;topSave:integer);
begin
asm
  var xpos=0;
  var ln=0;
  var LinesArray = obj.value.split("\n");
  var cp=this.CharPosToYPixel(obj,LinesArray,obj.selectionEnd,
         {get: function () {return xpos;}, set: function (v) {xpos = v;}},
         {get: function () {return ln;}, set: function (v) {ln = v;}});
  console.log('scrollTop='+obj.scrollTop+' selectionEnd='+obj.selectionEnd + ' cp='+cp);
  if ((cp-obj.scrollTop)<5) {
    obj.scrollTop=topSave;
    }
end;
end;

function GetTextLineHeight(element:TObject):integer;
var
  lHeight:integer;
begin
  asm
    var r=element.rows;
    var obStyle = window.getComputedStyle(element, null);
    var fs = obStyle.getPropertyValue('font-size');
    var lh = obStyle.getPropertyValue('line-height');
    var fontSize = parseFloat(fs);
    if (isNaN(lh))  {lh=1.5;}
    lHeight = Math.floor(fontSize * lh);
    //alert('fontSize='+fontSize+' fs='+fs+' lh='+lh+' lineHeight='+lHeight);

  end;
  result:=lHeight;
end;

function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  LabelText:string;
  ReadOnly:Boolean;
  OnChangeString, OnClickString, OnPasteString, OnKeyUpString, OnScrollString, MsgClickString, OnKeyDownString:String;
  lh:integer;
begin
 //showmessage('TXCode CreateWidget. Parent='+ParentNode.NodeName);
  LabelText:= MyNode.getAttribute('LabelText',true).AttribValue;
  ReadOnly:= StrToBool(MyNode.getAttribute('ReadOnly',true).AttribValue);

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''','''+NameSpace+''', '''');" ';
  OnChangeString:= 'onchange="pas.NodeUtils.SetInterfaceProperty('''+ScreenObjectName+''','''+NameSpace+''',''ItemValue'',event.target.value); '+
                             'pas.Events.handleEvent(null,''Change'','''+ScreenObjectName+''','''+NameSpace+''', event.target.value, ''ItemValue'');" ';
  OnScrollString:='onscroll="var p = event.target.parentNode.parentNode;pas.XCode.SyncScroll(p.id);"';
  OnKeyUpString:='onkeyup="var p = event.target.parentNode.parentNode;'
                        + 'pas.XCode.DoKeyUp(p.id,'''+ScreenObjectName+''','''+NameSpace+''',event); '
                        // Chrome bug?? Reset the scrollTop if return key inserts a line, causing the textarea to auto-scroll to cursor pos (v. annoying)
                        + 'if (p.parentNode.topSave>0) { '
                        + '  var sysnode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,p.parentNode.id,'''+NameSpace+''',true); '
                        + '  sysnode.ResetScrollPos(event.target,p.parentNode.topSave); } '
                         + '"';
  OnKeyDownString := 'onkeydown=" ' +
                     'var p = event.target.parentNode.parentNode; ' +
                     'p.parentNode.topSave=pas.XCode.suppressScrolling(event); ' +
                     'pas.XCode.DoKeyDown(p.id,'''+ScreenObjectName+''','''+NameSpace+''',event);' +
                     '"';

  MsgClickString:='onmouseup="event.stopPropagation(); '+
  // 'alert(''selectionstart=''+event.target.selectionStart); '+
                  'var lnum=this.value.substr(0, event.target.selectionStart).split(''\n'').length; '+
                  'var slnum=lnum.toString(); '+
                  'pas.Events.handleEvent(null,''ClickMessage'','''+ScreenObjectName+''','''+NameSpace+''', slnum);" ';

  AddCodeEditorStyles;

  asm
    try{
    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,NameSpace,$impl.MyNodeType,position);
    wrapper.classList.add("textAreaBorder");

    var HTMLString='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var wrapperid =  NameSpace+ScreenObjectName;
    var MyObjectName=wrapperid+'Contents';

    var ReadOnlyString = ' ';
    if (ReadOnly==true) { ReadOnlyString = ' readonly ';}

    var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'">'+LabelText+'</label>';

    var editorString =
    '<div id='+MyObjectName+' style=" background-color:#FFFFFF; height:100%; width:100%; z-index: 1;" class="vbox" '+
    //        OnClickString +
        '>'+
        '<div id='+MyObjectName+'Panel style="display:inline-block; height:100%; width:100%;  position:relative; z-index: 1;" >' +
        '<textarea id='+MyObjectName+'LineNumbers readonly '+
            'class="textarea noscrollbar LineNumberTextArea"; '+
            'onwheel = "return false;"; '+
            'spellcheck="false"; contenteditable="false" >' +
            '</textarea> <!––  Line Numbers   -->' +
        '<textarea id='+MyObjectName+'Real  '+
           OnChangeString +
           OnKeyUpString +
           OnKeyDownString +
           OnScrollString +
           ReadOnlyString +
           ' class="textarea WhiteTextArea " ; spellcheck="false" ; contenteditable="true" >' +
        '</textarea> ' +

        // Place transparent editboxes on top of each other so that the colour can be changed by selecting which layer to place the chars
        //    ....only the bottom one is editable directly
        '<textarea id="'+MyObjectName+'Bold" class="textarea BoldTextArea BlackTextArea" ;spellcheck="false" ; contenteditable="false" ></textarea>  <!––  keywords       -->' +
        '<textarea id="'+MyObjectName+'Red" class="textarea RedTextArea" ; spellcheck="false" ;contenteditable="false" ></textarea>                  <!––  Errors         -->' +
        '<textarea id="'+MyObjectName+'Green" class="textarea GreenTextArea" ; spellcheck="false" ;contenteditable="false" ></textarea>              <!––  Comments       -->' +
        '<textarea id="'+MyObjectName+'Blue" class="textarea BlueTextArea" ; spellcheck="false" ;contenteditable="false" ></textarea>                <!––  Strings        -->' +
        '<textarea id="'+MyObjectName+'Black" class="textarea BlackTextArea" ; spellcheck="false" ;contenteditable="false" ></textarea>             <!––  Normal Code    -->' +
      '</div>' +
      '<textarea id='+MyObjectName+'Messages readonly class="messagesarea" '+
        MsgClickString +
      '></textarea>'+
      '</div>' ;
     HTMLString = labelstring+editorString;

    var wrapper=document.getElementById(wrapperid);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);

  }
  catch(err) { alert(err.message+'  in XCode.CreateXCode');}

end;

  MyNode.ScreenObject:=MyNode;

  RefreshComponentProps(myNode);

  TXCode(myNode).context.IsInMultiLineCommentString:=false;
  TXCode(myNode).context.IsInAltMultiLineCommentString:=false;
  TXCode(myNode).context.IsInASMMultiLineCommentString:=false;
  TXCode(myNode).context.MultiLineCommentChangedStatus:=false;
  SetLength(TXCode(myNode).blackoutline,0);
  SetLength(TXCode(myNode).boldoutline,0);
  SetLength(TXCode(myNode).redoutline,0);
  SetLength(TXCode(myNode).greenoutline,0);
  SetLength(TXCode(myNode).blueoutline,0);
  TXCode(myNode).IsInitalised := false;
  TXCode(myNode).LengthOfRangeSelected  := 0;
  SetLength(TXCode(myNode).SavedLineContextArray,0);

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXCode.Create(MyForm,NodeName,NameSpace));
end;

procedure TXCode.SetMessagesHeight(AValue:string);
begin
  myNode.SetAttributeValue('MessagesHeight',AValue);
  asm
  var ob = document.getElementById(this.NameSpace+this.NodeName+'ContentsMessages');
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'H',AValue);
  end;
end;

procedure AddCodeEditorStyles;
var
  StyleHTML:String;
  StandardOverlayStyle:String;
begin
  StandardOverlayStyle :=
  '        margin: 0;' + LineEnding +
  '        padding: 0;' + LineEnding +
  '        background: transparent;' + LineEnding +
  '        position: absolute;' + LineEnding +
  '        left: 45px;' + LineEnding +
  '        width:calc(100% - 50px);' + LineEnding +
  '        white-space: pre;' + LineEnding;

   StyleHTML:=
  '<style>' + LineEnding +
  '    .noscrollbar::-webkit-scrollbar {' + LineEnding +
  '        display: none;' + LineEnding +
  '    }' + LineEnding +
  '    .textarea {' + LineEnding +
  '        font-family: monospace; font-size: 14px;' + LineEnding +
  '        border: 0;' + LineEnding +
  '        line-height: ' + glbLineHeight+';'+LineEnding +
  '        height: 100%;' + LineEnding +
  '    }' + LineEnding +
  '    .textAreaBorder {' + LineEnding +
  '        border: groove 1px #ccc;' + LineEnding +
  '        padding: 0px 0px 0px 0px' + LineEnding +
  '    }' + LineEnding +
  '    .messagesarea {' + LineEnding +
  '        font-family: monospace; font-size: 12px;' + LineEnding +
  '        border: 0;' + LineEnding +
  '        height: 20%;' + LineEnding +
  '    }' + LineEnding +
  '    .LineNumberTextArea {' + LineEnding +
  '        margin: 0;' + LineEnding +
  '        background: #DDDDDD;' + LineEnding +
  '        position: absolute;' + LineEnding +
  '        z-index: 998;' + LineEnding +
  '        width: 43px;' + LineEnding +
  '        left: 0px;' + LineEnding +
  '        white-space: pre;' + LineEnding +
  '    }' + LineEnding +
  '    .WhiteTextArea {' + LineEnding +
  '        color: transparent;' + LineEnding +
  '        caret-color: black;' + LineEnding +
  '        z-index: 999;' + LineEnding +
        StandardOverlayStyle +
  '    }' + LineEnding +
  '    .BlackTextArea {' + LineEnding +
  '        z-index: 998;' + LineEnding +
       StandardOverlayStyle +
  '    }' + LineEnding +
  '    .BoldTextArea {' + LineEnding +
  '        z-index: 997;' + LineEnding +
  '        font-weight: bold;' + LineEnding +
       StandardOverlayStyle +
  '    }' + LineEnding +
  '    .BlueTextArea {' + LineEnding +
  '        color: blue;' + LineEnding +
  '        z-index: 996;' + LineEnding +
       StandardOverlayStyle +
  '    }' + LineEnding +
  '    .RedTextArea {' + LineEnding +
  '        color: red;' + LineEnding +
  '        z-index: 995;' + LineEnding +
  '        text-decoration: underline;' + LineEnding +
       StandardOverlayStyle +
  '    }' + LineEnding +
  '    .GreenTextArea {' + LineEnding +
  '        color: green;' + LineEnding +
  '        font-style: italic;' + LineEnding +
  '        z-index: 994;' + LineEnding +
       StandardOverlayStyle +
  '    }' + LineEnding +
  '</style>';

  asm
    try{
        // ----------------------------------------check if the style has already been set
        var x = document.getElementsByTagName("STYLE");
        var StyleIsSet = false;
        // check all the existing style blocks...
        if (x.length>0){
          for (var i=0; i<x.length; i++){
            var y= x[i].innerHTML;
            if (y.indexOf(".GreenTextArea") !=-1) { StyleIsSet =true}
          }
        }
        if (StyleIsSet == false){
           //----------------------------- now append the style declarations to the head of the HTML page
           document.head.innerHTML = document.head.innerHTML+StyleHTML;
        }
   }catch(err) {alert('Error in XCode.addCodeEditorStyles '+ err.message);}
  end;
end;

procedure DoKeyDown(myId:String;NodeId,NameSpace:String;event:TObject);
var
  myNode:TXCode;
begin
  myNode:=TXCode(FindDataNodeById(SystemNodeTree,NodeId,NameSpace,false));
  if myNode<>nil then
  begin
  asm
  //alert('DoKeyDown');
//  function CursorGotoXY(x,y)
//  {  var charcount = 0;
//     const mydoc = document.getElementById(myId+"Real");
//     var stringArray = mydoc.value.split("\n");
//     for (var j = 0; j < stringArray.length; j++)
//     {
//       if (j< y){ charcount = charcount + stringArray[j].length +1;};
//     }
//     charcount = charcount + x;
//     mydoc.focus();
//     mydoc.setSelectionRange(charcount,(charcount + 1));
//  }

  function SuppressTabKey(event)
  // code to suppress the tab key default behavior on KeyDown
  {
     var myElement = document.getElementById(myId+"Real")
         myNode.LengthOfRangeSelected = Math.abs(myElement.selectionStart - myElement.selectionEnd);
     var x = event.which || event.keyCode; // firefox does not support event.which but it does suppoert event.keyCode;
     //detect 'tab' key
     if(x == 9)
     {
       //prevent focusing on next element
       event.preventDefault();
     }
  }
  SuppressTabKey(event);
  end;
  end;
end;

procedure DoKeyUp(myId:String;NodeId,NameSpace:String;event:TObject);
var
  myNode:TXCode;
// This does the text syntax colouring...
// assume all reserved words start and end with a space
begin
  myNode:=TXCode(FindDataNodeById(SystemNodeTree,NodeId,NameSpace,false));

  if myNode<>nil then
  begin
  asm
  //alert('DoKeyUp.  Id='+myId);
  var ReservedWords = [];
  // assume all reserved words start and end with a space
  ReservedWords[1] = ["absolute", "abstract", "alias", "and", "array", "as", "asm", "assembler"];
  ReservedWords[2] = ["begin", "break"];
  ReservedWords[3] = ["case", "cdecl", "class", "const", "constructor", "continue", "cppdecl"];
  ReservedWords[4] = ["default", "destructor", "dispose", "div", "do", "downto"];
  ReservedWords[5] = ["else", "end", "except", "exit", "export", "exports", "external"];
  ReservedWords[6] = ["false", "file", "for", "forward", "function"];
  ReservedWords[7] = ["generic", "goto"];
  ReservedWords[8] = [""];
  ReservedWords[9] = ["if", "implementation", "in", "index", "inherited", "initialization", "inline", "interface", "is"];
  ReservedWords[10] = [""];
  ReservedWords[11] = [""];
  ReservedWords[12] = ["label", "library", "local"];
  ReservedWords[13] = ["mod"];
  ReservedWords[14] = ["name", "new", "nil", "nostackframe", "not"];
  ReservedWords[15] = ["object", "of", "oldfpccall", "on", "operator", "or", "out", "override"];
  ReservedWords[16] = ["packed", "private", "procedure", "program", "property", "protected", "published"];
  ReservedWords[17] = [""];
  ReservedWords[18] = ["raise", "read", "record", "register", "repeat"];
  ReservedWords[19] = ["safecall", "self", "set", "shl", "shr", "softfloat", "specialize", "stdcall", "string"];
  ReservedWords[20] = ["then", "threadvar", "to", "true", "try", "type"];
  ReservedWords[21] = ["unit", "until", "uses"];
  ReservedWords[22] = ["var", "virtual"];
  ReservedWords[23] = ["while", "with", "write"];
  ReservedWords[24] = ["xor"];
  ReservedWords[25] = [""];
  ReservedWords[26] = [""];

  var StringDelimiter = "'";
  var StartOfSingleLineCommentdelimiter = "/";
  var StartOfMultiLineComment = "{";
  var EndOfMultiLineComment = "}";
  var EndOfLine = "\n";

  function isDelimiter(inChar)
  {
      var isadelimiter = false;
      if ((inChar == " ") || (inChar == ";") || (inChar == "=") || (inChar == "(") || (inChar == "-") || (inChar == "/") || (inChar == "*") ||
          (inChar == ")") || (inChar == "}") || (inChar == "]") || (inChar == "+") || (inChar == "{") || (inChar == "[") ||
          (inChar == "&") || (inChar == "|") || (inChar == "!") ) {
          isadelimiter = true;        }
      return isadelimiter;
  }

  function matchReservedWord(lineString, firstcharpos, charindex) {
      var ReservedWordlength = 0;
      var foundstring = "";
      try {
          // for all reserved words starting with the first letter
          for (var k = 0; k < ReservedWords[charindex].length; k++) {
              var testWord = ReservedWords[charindex][k];
              var followingChar = firstcharpos + testWord.length;
              if (isDelimiter(lineString[followingChar])){
                  var match = true;
                  for (var m = 0; m < testWord.length; m++) {
                      if (lineString[firstcharpos + m] != testWord[m]) { match = false; };
                  }
                  if (match == true) { ReservedWordlength = testWord.length; foundstring = testWord};
              }
          }
      }
      catch (err) { alert("Error -- " + err.message); };
      return ReservedWordlength;
  };

  //-------------------------------------------------------------------------------------------------------------------------
  //                  core line parser called by both ParseThisLine(....) and ParseWholeDocumaent();
  //-------------------------------------------------------------------------------------------------------------------------

  function ParseLine(j,line,localcontext,SyntaxText)
  {
              var IsInSingleLineCommentString = false;
              var IsInStringLiteral = false;
              var ReservedWordCharCount = 0;
              var SavedIsInMultiLineCommentString=localcontext.IsInMultiLineCommentString;

              for (var i =0; i < line.length; i++)
              {
                  //---------------------------------------------------------
                  //          Find start and end of comments and asm blocks
                  //---------------------------------------------------------

                  if (IsInStringLiteral==false)
                  {
      					// find multi line comment markers
      					if (IsInSingleLineCommentString == false)
      					{
      						// Note there are two types of multi line marker and they need to be matched up
      					    if ((localcontext.IsInAltMultiLineCommentString == false)&&(localcontext.IsInASMMultiLineCommentString == false))
      					    {
      							if(line[i] == EndOfMultiLineComment)
      							{ localcontext.IsInMultiLineCommentString = false; }
      							if (line[i] == StartOfMultiLineComment)
      							{ localcontext.IsInMultiLineCommentString = true;}
      						};
      					    if (localcontext.IsInAltMultiLineCommentString == true)
      					    {
      							if((line[i] == "*")&&(line[i+1] == ")"))
      							{ localcontext.IsInMultiLineCommentString = false;
      							  localcontext.IsInAltMultiLineCommentString = false;
      							}
      						};

      					    if (localcontext.IsInASMMultiLineCommentString == true)
      					    {
      							if((line[i] == "e")&&(line[i+1] == "n")&&(line[i+2] == "d")&&(line[i+3] == ";") )
      							{ localcontext.IsInMultiLineCommentString = false;
      							  localcontext.IsInASMMultiLineCommentString = false;
      							}
      						};

      						if (localcontext.IsInMultiLineCommentString == false)
      						{
      							if((line[i] == "(")&&(line[i+1] == "*"))
      							{ localcontext.IsInMultiLineCommentString = true;
      							  localcontext.IsInAltMultiLineCommentString = true;
      							}

      							if((line[i-4] == " ")&&(line[i-3] == "a")&&(line[i-2] == "s")&&(line[i-1] == "m")&&(line[i] == " ") )
      							{ localcontext.IsInMultiLineCommentString = true;
      							  localcontext.IsInASMMultiLineCommentString = true;
      							}
      						}
      					}

      					if (localcontext.IsInMultiLineCommentString == false)
      					{
      						  // find single line comment marker pair "//"
      						  if((line[i] == StartOfSingleLineCommentdelimiter)&&(line[i + 1] == StartOfSingleLineCommentdelimiter))
      							{ IsInSingleLineCommentString = true };
      					}
                  }

                  //---------------------------------------------------------
                  // mark the chars with the appropriate colour highlighting
                  //---------------------------------------------------------

                  if ((localcontext.IsInMultiLineCommentString == true)||(IsInSingleLineCommentString == true))
                      { SyntaxText.newgreenline = SyntaxText.newgreenline + line[i] }
                      else { SyntaxText.newgreenline = SyntaxText.newgreenline + " " };

                  if ((localcontext.IsInMultiLineCommentString == false) && (IsInSingleLineCommentString == false)&& (IsInStringLiteral == false)&&(ReservedWordCharCount<1))
                      { SyntaxText.newblackline = SyntaxText.newblackline + line[i] }
                      else { SyntaxText.newblackline = SyntaxText.newblackline + " " };

                  if ((localcontext.IsInMultiLineCommentString == false) && (IsInSingleLineCommentString == false) && (IsInStringLiteral == false)&&(ReservedWordCharCount>0))
                      { SyntaxText.newboldline = SyntaxText.newboldline + line[i]}
                      else { SyntaxText.newboldline = SyntaxText.newboldline + " " };

                  if (IsInStringLiteral == true)
                      { SyntaxText.newblueline = SyntaxText.newblueline + line[i] }
                      else {  SyntaxText.newblueline = SyntaxText.newblueline + " " } ;

                  //---------------------------------------------------------
                  //          Find strings and reserved words
                  //---------------------------------------------------------

                  if ((localcontext.IsInMultiLineCommentString == false) && (IsInSingleLineCommentString == false))
                  {
                     // find start and end of string
                     if(line[i] == StringDelimiter)
                     {
                       if (IsInStringLiteral == false)
                       {
                         IsInStringLiteral = true;
                       }
                       else { IsInStringLiteral = false };
                     }

                     // find reserved words
                     if (ReservedWordCharCount > 0) { ReservedWordCharCount = ReservedWordCharCount - 1; };
                     var lowercaseline = line.toLowerCase() + " ";
                     if ((IsInStringLiteral == false)&& isDelimiter(line[i])
                         && (lowercaseline.charCodeAt(i + 1) > 96) && (lowercaseline.charCodeAt(i + 1) < 123)) // space follwed by a lower case letter
                     {
                         var charindex = lowercaseline.charCodeAt(i + 1) - 96;
                         ReservedWordCharCount = matchReservedWord(lowercaseline, i + 1, charindex);
                     }
                  }
              }
              var MultilineChanges = true;
              if (SavedIsInMultiLineCommentString == localcontext.IsInMultiLineCommentString){MultilineChanges = false};
              localcontext.MultiLineCommentChangedStatus=MultilineChanges;
       return  MultilineChanges; // true if any of the changes on this line have consequences for the next line(s)
  }

  function ParseThisLine(startAtLine,mycontext,
       blackoutline,boldoutline,redoutline,greenoutline, blueoutline)  // the string arrays for each colour
  // function to parse the document if the number of lines change has not changed so we can limit our changes to a subset of the lines in the document
  {
      try {
           var mylocalContext= JSON.parse(JSON.stringify(mycontext));
          var blackstring = "";
          var boldstring = "";
          var redstring = "";
          var greenstring = "";
          var bluestring = "";
          var linenumoutline = [""];

          var stringArray = document.getElementById(myId+"Real").value.split("\n");
          for (var j = startAtLine; j < stringArray.length; j++)
          {
              var MultiLineCommentChangedStatus = mylocalContext.MultiLineCommentChangedStatus;
              var line =" "+ stringArray[j]; // add a leading space so all reserved words always have a space in front of them
              var SyntaxText ={ newblackline : "", newboldline : "",newredline : "", newgreenline : "", newblueline : ""};

              // parse the line
              MultiLineCommentChangedStatus=ParseLine(j,line,mylocalContext,SyntaxText );
              mylocalContext.MultiLineCommentChangedStatus = MultiLineCommentChangedStatus;
              // save the new context and savee its old value to see if we need to do the next lines or not
              if (myNode.SavedLineContextArray[j+1] != undefined)
              {var oldcontext = JSON.parse(JSON.stringify(myNode.SavedLineContextArray[j+1]));}
              else {var oldcontext = JSON.parse(JSON.stringify(myNode.SavedLineContextArray[j]));}; // this is just to keep the compiler happy
              myNode.SavedLineContextArray[j+1]= JSON.parse(JSON.stringify(mylocalContext));

              // take away the leading char padding each line (which is there to make sure reserved words have a space preceeding them)
              blackoutline[j] = SyntaxText.newblackline.slice(1);
              boldoutline[j] = SyntaxText.newboldline.slice(1);
              redoutline[j] = SyntaxText.newredline.slice(1);
              greenoutline[j] = SyntaxText.newgreenline.slice(1);
              blueoutline[j] = SyntaxText.newblueline.slice(1);
         //     myNode.SavedLineContextArray[j]
             if (mylocalContext.IsInMultiLineCommentString == oldcontext.IsInMultiLineCommentString)
                {  break; }
          }
          // avoid repainting between updating the different layers
          document.getElementById(myId+"Real").style.visibility="hidden";

          document.getElementById(myId+"Black").value =blackoutline.join("\n");
          document.getElementById(myId+"Bold").value = boldoutline.join("\n");
          document.getElementById(myId+"Red").value = redoutline.join("\n");
          document.getElementById(myId+"Green").value = greenoutline.join("\n");
          document.getElementById(myId+"Blue").value = blueoutline.join("\n");

          // restore having avoided repainting between updating the different layers
          document.getElementById(myId+"Real").style.visibility="visible";

      }
      catch(err) {  alert("Error in ParseThisLine syntax highlighting -- "+ err.message); };
  }


  // code to apply syntax highlighting .... myTextArea (the bottom one) holds the full text...the overlays only have the non standard chars
  // Remember to synchronize the scroll position for all the edit boxes

  function ParseWholeDocument()
  // function to parse the document if the number of lines change (or this is the first pass)
  {
      try {
          var blackstring = "";
          var boldstring = "";
          var redstring = "";
          var greenstring = "";
          var bluestring = "";

          myNode.blackoutline = [];
          myNode.boldoutline =  [];
          myNode.redoutline = [];
          myNode.greenoutline =  [];
          myNode.blueoutline =  [];

          var linenumoutline = [""];

          myNode.context.IsInMultiLineCommentString=false;
          myNode.context.IsInAltMultiLineCommentString=false;
          myNode.context.IsInASMMultiLineCommentString=false;

          var stringArray = document.getElementById(myId+"Real").value.split("\n");
          for (var j = 0; j < stringArray.length; j++)
          {
              var line =" "+ stringArray[j]+" "; // add a leading and trailing space so all reserved words always have a space in front of them (and after them if they are at the end of a line)
              var SyntaxText ={ newblackline : "", newboldline : "",newredline : "", newgreenline : "", newblueline : ""};
              myNode.SavedLineContextArray[j] = JSON.parse(JSON.stringify(myNode.context));

              ParseLine(j,line,myNode.context,SyntaxText );

              // take away the leading char padding each line (which is there to make sure reserved words have a space preceeding them)
              myNode.blackoutline[j] = SyntaxText.newblackline.slice(1);
              myNode.boldoutline[j] = SyntaxText.newboldline.slice(1);
              myNode.redoutline[j] = SyntaxText.newredline.slice(1);
              myNode.greenoutline[j] = SyntaxText.newgreenline.slice(1);
              myNode.blueoutline[j] = SyntaxText.newblueline.slice(1);
          }
          // avoid repainting between updating the different layers
          document.getElementById(myId+"Real").style.visibility="hidden";

          document.getElementById(myId+"Black").value =myNode.blackoutline.join("\n");
          document.getElementById(myId+"Bold").value = myNode.boldoutline.join("\n");
          document.getElementById(myId+"Red").value = myNode.redoutline.join("\n");
          document.getElementById(myId+"Green").value = myNode.greenoutline.join("\n");
          document.getElementById(myId+"Blue").value = myNode.blueoutline.join("\n");

          // restore having avoided repainting between updating the different layers
          document.getElementById(myId+"Real").style.visibility="visible";

          // now add line numbers
          for (var j = 0; j < stringArray.length; j++)
          {
              linenumoutline[j] = (j+1).toString();
              var strLen = 4 - linenumoutline[j].length;
              for (var k = 0; k < strLen; k++) { linenumoutline[j] = " "+ linenumoutline[j]; };
          }
          document.getElementById(myId+"LineNumbers").value = linenumoutline.join("\n");
      }
      catch(err) {  alert("Error in ParseWholeDocument syntax highlighting --- "+ err.message); };
  }

  function CurrentLine()
  {
     var CursorPosition = document.getElementById(myId+"Real").selectionStart;
     var stringarray = document.getElementById(myId+"Real").value.substring(0,CursorPosition).split("\n");
     return stringarray.length -1;
  }

  function chooseParser(event)
  {
  //alert('chooseParser');
     if (event!=null)
       {var x = event.which || event.keyCode; }// firefox does not support event.which but it does suppoert event.keyCode;
     else
       {x = 0; myNode.IsInitalised = false;}

     //detect backspace,CursorUp,CursorDown,delete,return and cntrl V keys or non zero range - then redo the whole document as it could have changed the number of lines
     if ((myNode.IsInitalised == false) ||(x==8)||(x==38)||(x==40) ||(x==46) ||(x==13) ||(x==86)||(myNode.LengthOfRangeSelected !=0))
     {
     //alert('parse whole document');
         myNode.IsInitalised = true;
         ParseWholeDocument();
     }
     else
     {
     //alert('parse one line');
         var Selectedline = CurrentLine();
         //alert('Selectedline='+Selectedline);
         ParseThisLine(Selectedline,
                         myNode.SavedLineContextArray[Selectedline],
                         myNode.blackoutline,  // the string arrays for each colour
         	         myNode.boldoutline,
         	         myNode.redoutline,
         	         myNode.greenoutline,
                         myNode.blueoutline);
     };
     myNode.LengthOfRangeSelected = 0;
     //alert('SyncScroll');
     pas.XCode.SyncScroll(myId);
  }
  chooseParser(event);

  end;
  end;
end;

procedure SyncScroll(myId:String);
begin
  asm
       {
          var myscrollTop = document.getElementById(myId+"Real").scrollTop;
          var myscrollLeft =  document.getElementById(myId+"Real").scrollLeft;

          document.getElementById(myId+"Black").scrollTop = myscrollTop;
          document.getElementById(myId+"Bold").scrollTop = myscrollTop;
          document.getElementById(myId+"Red").scrollTop = myscrollTop;
          document.getElementById(myId+"Green").scrollTop = myscrollTop;
          document.getElementById(myId+"Blue").scrollTop = myscrollTop;
          document.getElementById(myId+"LineNumbers").scrollTop = myscrollTop;

          document.getElementById(myId+"Black").scrollLeft = myscrollLeft;
          document.getElementById(myId+"Bold").scrollLeft = myscrollLeft;
          document.getElementById(myId+"Red").scrollLeft = myscrollLeft;
          document.getElementById(myId+"Green").scrollLeft = myscrollLeft;
          document.getElementById(myId+"Blue").scrollLeft = myscrollLeft;
          document.getElementById(myId+"LineNumbers").scrollLeft = myscrollLeft;
          }
  end;
end;

{$endif}

procedure TXCode.GoToLineCharPos(LineNum,CharPos:integer);
begin
  {$ifndef JScript}
  TheEditor.CaretY:=LineNum;
  TheEditor.CaretX:=CharPos;
  TheEditor.SetFocus;
  {$else}
   asm
// CharPos here is offset index from start of line (not whole textarea)
//     alert('LineNum='+LineNum+' CharPos='+CharPos+' looking for '+this.NodeName+'ContentsReal');
       var elem = document.getElementById(this.NameSpace+this.NodeName+'ContentsReal');

       if(elem != null) {
         elem.focus();
           var LinesArray = elem.value.split("\n");
           var charcount=0;
           if (LineNum<LinesArray.length) {
             for (var i=0; i<LineNum-1; i++ ) {
               charcount=charcount+LinesArray[i].length+1;
             }
           }
           charcount=charcount+CharPos-1;

           // Highlight the character at this position
           if(elem.createTextRange) {
               var range = elem.createTextRange();
               range.move('character', charcount);
               range.select();
           }
           else {
               if(elem.selectionStart) {
                 elem.setSelectionRange(charcount, (charcount+1));
               }
            }

            //.....scroll to the selection position .............
            // we need to scroll to this row but scrolls are in pixels,
            // so we need to know a row's height, in pixels
            if (LineNum>1) {LineNum=LineNum-2;} else {LineNum=0;}
            var st=this.LineNumToYPixel(elem,LineNum);
            // Y scroll !!
            elem.scrollTop = st;

            //may also need to scroll horizontally....
            var w=CharPos;

            // X scroll !!
            if (w>elem.clientWidth) {
              elem.scrollLeft = Math.max(0,Math.trunc(w-20));  }
            else
              {elem.scrollLeft = 0;  }


            elem.blur();
            elem.focus();
}

   end;
   {$endif}
end;

{$ifdef JScript}
function TXCode.LineNumToYPixel(elem:TObject; LineNum:integer):integer;
var
  ypx:integer;
begin
  asm
    try {
    // we need to know a row's height, in pixels
    var lh=pas.XCode.GetTextLineHeight(elem);
    ypx=(LineNum*lh);
    } catch(err) { alert(err.message+'  in XCode.LineNumToYPixel'); }
  end;
  result:=ypx;
end;

function TXCode.CharPosToYPixel(elem,LinesArray:TObject; CharPos:integer; var xpos,lNum:integer):integer;
var
  ypx:integer;
  LineNum:integer;
begin
  LineNum:=0;
  // Charpos is a character offset from start of whole textarea.
  // xpos is returned as the character offset on the found line.
  // ypx is returned as the Y-pixel offset of the found line from start of textarea.
  ypx:=0;
  asm
    try {
    //var LinesArray = elem.value.split("\n");
    //alert('total lines='+LinesArray.length);
    var charcount=0;
    LineNum=0;
    var i=0;

    do
    {
        charcount=charcount+LinesArray[i].length+1;
        i++;
    }
    while ((charcount<CharPos)&&(i<LinesArray.length));

    if (i>0) {LineNum=i-1;}
    //alert('charcount '+charcount+' found at line '+i+' LineNum='+LineNum);
    xpos.set(CharPos - (charcount - LinesArray[LineNum].length));
    //alert('CharPosToYPixel:  xpos '+xpos.get()+' LineNum='+LineNum);
    lNum.set(LineNum);

    ypx=this.LineNumToYPixel(elem,LineNum);
    } catch(err) { alert(err.message+'  in XCode.CharPosToYPixel'); }
  end;
  result:=ypx;
end;

procedure TXCode.GoToCharPos(CharPos:integer);
var
  LineNum:integer;
begin
  LineNum:=0;
   // Charpos here is the character offset from start of text
   asm
     try {
       //alert(' CharPos='+CharPos+' looking for '+this.NodeName+'ContentsReal');
       var elem = document.getElementById(this.NameSpace+this.NodeName+'ContentsReal');

       if(elem != null) {
         elem.focus();

         // Highlight the character at this position
         if(elem.createTextRange) {
               var range = elem.createTextRange();
               range.move('character', CharPos);
               range.select();
           }
           else {
               if(elem.selectionStart) {
                 elem.setSelectionRange(CharPos, (CharPos+1));
               }
            }

            //.....scroll to the selection position (minus 1 line) .............
            var lh=pas.XCode.GetTextLineHeight(elem);
            LineNum=0;
            var LinesArray = elem.value.split("\n");
            //alert('total lines='+LinesArray.length);
            var xpos=null;
            var localnum=null;
            var st=this.CharPosToYPixel(elem,LinesArray,CharPos,
                     {get: function () {return xpos;}, set: function (v) {xpos = v;}},
                     {get: function () {return localnum;}, set: function (v) {localnum = v;}});
            //alert('xpos='+xpos+' LineNum='+localnum+' st='+st);
            if (st>=2*lh) {st=st-(2*lh);} else {st=0;}


            // scroll (scrollTop is in y-pixels) !!
            elem.scrollTop = st;

            //may also need to scroll horizontally....
            var xpos=CharPos - xpos;
            //var xpos=CharPos - (charcount - LinesArray[LineNum].length);
            //alert('xpos='+xpos);

            var txt=LinesArray[LineNum].substring(0,xpos+1);
            //var canvas = document.createElement('canvas');
            //var ctx = canvas.getContext("2d");
            //ctx.fontFamily = "monospace";   //elem.fontFamily;
            //ctx.fontSize = "14px";   //elem.fontSize;
            //var w = ctx.measureText(txt).width;
            var w=txt.length*7;                        //!! the fontsize is fixed, so ok to hard-code 7px here for char-width.
            //alert('w='+txt.length+'*7 = '+w);

            // scroll !!
            if (w>elem.clientWidth) {
              elem.scrollLeft = Math.max(0,Math.trunc(w-20));  }
            else
              {elem.scrollLeft = 0;  }

            elem.blur();
            elem.focus();
       }
     } catch(err) { alert(err.message+'  in XCode.GoToCharPos'); }
   end;
end;
{$endif}

procedure TXCode.GetFileNameLineNumAndCharPos(Var MessageFound:Boolean;SelectedLine,delim:string;var FileName,LineNum,CharNum:string);
var     List1,List2: TStringList;
        tempstr:string;
        i:Integer;
begin
  charnum:='0';
  MessageFound:=false;
  List1 := TStringList.Create;
  List2:=TStringList.Create;
  try
    List1.LineBreak := delim;
    List1.Text := SelectedLine;
    if List1.count >1 then
    begin
      FileName:= List1[0];     //+'.Pas';
      tempstr:=List1[1];
      List2.LineBreak:='tempinc\';
      List2.Text:=FileName;
      if List2.Count>1 then
        FileName:=List2[1];
      i:=1;
      while (( tempstr[i]<>',') and ( tempstr[i]<>')') and (i<length(tempstr))) do
      begin
        LineNum:=LineNum+ tempstr[i];
        i:=i+1;
      end;

      if ( tempstr[i]<>')') then
      begin
        i:=i+1;
        while  (tempstr[i]<>')') and (i<length(tempstr)) do
        begin
          CharNum:=CharNum+ tempstr[i];
          i:=i+1;
        end;
      end;

      if (LineNum<>'') and (CharNum<>'') then
        MessageFound:=true;
    end;
  finally
    List1.Free;
    List2.Free;
  end;
end;


function TXCode.GetItemValue:string;
begin
  result:=MyNode.getAttribute('ItemValue',true).AttribValue;
end;
function TXCode.GetReadOnly:Boolean;
begin
  result:=MyStrToBool(MyNode.getAttribute('ReadOnly',true).AttribValue);
end;
function TXCode.GetMessagesHeight:string;
begin
  result:=MyNode.getAttribute('MessagesHeight',true).AttribValue;
end;
function TXCode.GetMessageLines:string;
begin
  result:=MyNode.getAttribute('MessageLines',true).AttribValue;
end;

procedure TXCode.SetItemValue(AValue:string);
var
  items:TStringList;
  tmp:string;
  i:integer;
begin
  if myNode<>nil then
  begin
    myNode.SetAttributeValue('ItemValue',AValue);
    {$ifndef JScript}
    items := TstringList.Create;
    items.StrictDelimiter:=true;
    items.LineBreak:=LineEnding;
    items.text:= AValue;

    TSynEdit(TheEditor).Clear;
    //showmessage('code num lines '+inttostr(items.count));
    for i:=0 to items.count - 1 do
    begin
      tmp:=items[i];
      TSynEdit(TheEditor).Lines.Add(tmp);
    end;

    {$else}
    asm
      var ob = document.getElementById(this.NameSpace+this.NodeName+'ContentsReal');
      if (ob!=null) {
         //alert('set item value for '+ this.NodeName+'ContentsReal to '+AValue);
         ob.value=AValue;
         pas.XCode.DoKeyUp(this.NodeName+'Contents',this.NodeName,this.NameSpace,null); }
    end;
//    LinkSaveToProperty(self);
    {$endif}

  end;
end;

procedure TXCode.AddMessage(txt:String);
var
  mLines:TStringList;
  allLines:String;
begin
 {$ifndef JScript}
 TheMessages.Lines.Add(txt);
 self.MessageLines:=TheMessages.Lines.Text;
 {$else}
 asm
   var ob = document.getElementById(this.NameSpace+this.NodeName+'ContentsMessages');
   if (ob!=null) {
      ob.value=ob.value+'\n'+txt;
      allLines=ob.value;
      }
 end;
 self.MessageLines:=allLines;
 {$endif}
end;

procedure TXCode.SetMessageLines(AValue:string);
 var
   lines:TStringList;
   i:integer;
   tmp:string;
 begin
  myNode.SetAttributeValue('MessageLines',AValue);
  lines:=TStringList.Create;
  lines.StrictDelimiter:=true;
  lines.LineBreak:=LineEnding;
  lines.Text:=AValue;
  {$ifndef JScript}
  TheMessages.Clear;
  for i:=0 to lines.count - 1 do
  begin
    tmp:=lines[i];
    TheMessages.Lines.Add(tmp);
  end;
  theMessages.Update;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'ContentsMessages');
    if (ob!=null) {
       ob.value=AValue;  }
  end;
  {$endif}
end;



procedure TXCode.SetReadOnly(AValue:Boolean);
begin
  myNode.SetAttributeValue('ReadOnly',myBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  TSynEdit(TheEditor).ReadOnly:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'ContentsReal');
    if (ob!=null) {
      ob.readOnly = AValue  }
  end;
  {$endif}
end;

begin
  // this is the set of node attributes that each XCode instance will have.
  AddWrapperDefaultAttribs(myDefaultAttribs);
  AddDefaultAttribute(myDefaultAttribs,'ContainerWidth','String','400','',false);
  AddDefaultAttribute(myDefaultAttribs,'ContainerHeight','String','300','',false);
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelText','String','Code Editor','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','Top','',false);
  AddDefaultAttribute(myDefaultAttribs,'ItemValue','String','...text...','',false);
  AddDefaultAttribute(myDefaultAttribs,'MessagesHeight','String','1','',false);
  AddDefaultAttribute(myDefaultAttribs,'ReadOnly','Boolean','False','',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);


  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  AddAttribOptions(MyNodeType,'LabelPos',LabelPosOptions);
  {$ifndef JScript}
  RegisterClass(TXCode);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}
  SuppressDesignerProperty('TXCode','BgColor');

end.


