(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XSVGContainer;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
Classes, SysUtils, TypInfo, StringUtils, NodeUtils,
WrapperPanel, Events, XIFrame, XForm,
{$ifndef JScript}
Controls, LResources, Graphics, Propedits, Dialogs,
LazsUtils
 {$ifdef Chromium}
 ,uCEFChromium, uCEFInterfaces, uCEFTypes
 {$endif}
{$else}
HTMLUtils
{$endif}
;

{$ifndef JScript}
procedure Register;
{$else}
procedure HandleMessage(msg:String);
{$endif}
function SVGItemFromTitle(IFrameTitle:String):TDataNode;

type
  TXSVGContainer = class(TXIFrame)
  private
    { Private declarations }
    function GetXMLString:string;
    procedure SetXMLString(AValue:string);
   {$ifndef JScript}
    procedure DoSVGConstructor;
    procedure Loaded; override;
   {$endif}
  protected
    { Protected declarations }
  public
    procedure SetSuspendRefresh(AValue: Boolean); override;
    procedure ConstructXMLString(myName,NameSpace:String);
    function  SpliceSVGChildStrings(parentstring,childstring:string):string;
    function XMLWarnings(SVGString:String):String;
    function WarnAboutTTypeBezierCurves(instring:string):string;
    function FullXMLString:String;
    { Public declarations }
    {$ifndef JScript}
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent;IsDynamic:Boolean); override;
    procedure ReLoadURL;   override;
      {$ifdef Chromium}
      procedure TitleChange(Sender: TObject;const cefBrowser:ICefBrowser;const NewTitle:UString) ; virtual;
      {$endif}
    //property myNode;
    {$else}
    constructor Create(MyForm:TForm;NodeName,NameSpace:String);  override;
    {$endif}
  published
    { Published declarations }

    // Properties defined for this class...
    property XMLString: String read GetXMLString write SetXMLString;

  end;



//..... Interface Objects for SVG 'internal' graphic segments (line, circle etc)..............
//      These are not screen objects, but are created here to support the
//      DataNode / Interface Object structures for property setting etc.
//      They are not available in the Lazarus IDE, but must be created dynamically.
type
   TXSVGWidget = class(TDataNode)
     private
       FIsSelected:Boolean;
       function getXMLString: String;
       function getXPos: String;
       function getYPos: String;
       function getHeight: String;
       function getWidth: String;
       function getStrokeWidth: String;
       function getStrokeColor: TColor;
       function getFillColor: TColor;
       function getFillTransparent: Boolean;
       function getRotate: String;
       function GetHint:String;
       procedure setXMLString(AValue:string);
       procedure setXPos(AValue:string);
       procedure setYPos(AValue:string);
       procedure setWidth(AValue:string);
       procedure setHeight(AValue:string);
       procedure setStrokeWidth(AValue:string);
       procedure setStrokeColor(AValue:TColor);
       procedure setFillColor(AValue:TColor);
       procedure setFillTransparent(AValue:Boolean);
       procedure setRotate(AValue:string);
       procedure SetHint(AValue:String);
       {$ifndef JScript}
       function CommonSetup(myNameSpace:String;ParentNode:TdataNode;position:integer):TdataNode;
       {$else}
       function SetupWidget(ParentNode:TDataNode;position:integer):TDataNode;
       procedure FinishSVGInObCreate;
       {$endif}
       procedure SetMyEventTypes;

     public
       myNode:TDataNode;          // self-reference for Laz/JS compatibility
       procedure ConstructXMLString; virtual;
       procedure SetDefaultAttribs;  virtual;

     published
       property IsSelected: Boolean read FIsSelected write FIsSelected;
       property XMLString: String read GetXMLString write SetXMLString;
       property XPos: string read GetXPos write SetXPos;
       property YPos: string read GetYPos write SetYPos;
       property Width: String read GetWidth write SetWidth;
       property Height: String read GetHeight write SetHeight;
       property StrokeWidth: String read GetStrokeWidth write SetStrokeWidth;
       property StrokeColor: TColor read GetStrokeColor write SetStrokeColor;
       property FillColor: TColor read GetFillColor write SetFillColor;
       property FillTransparent: Boolean read GetFillTransparent write SetFillTransparent;
       property Rotate: String read GetRotate write SetRotate;
       property Hint: String read GetHint write SetHint;
  end;

 type
   TXSVGText = class(TXSVGWidget)
     private
       function getTextString: String;
       function getFontFamily: String;
       function getFontWeight: String;
       function getFontStyle: String;
       procedure setTextString(AValue:string);
       procedure setFontFamily(AValue:string);
       procedure setFontWeight(AValue:string);
       procedure setFontStyle(AValue:string);
     public

       procedure ConstructXMLString;  override;
       procedure SetDefaultAttribs;  override;

     published
       property TextString: String read GetTextString write SetTextString;
       property FontFamily: String read GetFontFamily write SetFontFamily;
       property FontWeight: String read GetFontWeight write SetFontWeight;
       property FontStyle: String read GetFontStyle write SetFontStyle;
  end;

type
   TXSVGRect = class(TXSVGWidget)
     private
     public

       procedure ConstructXMLString;  override;
       procedure SetDefaultAttribs;    override;

     published
end;

type
   TXSVGRoundedRect = class(TXSVGWidget)
     private
       function getrx: String;
       function getry: String;
       procedure setrx(AValue:string);
       procedure setry(AValue:string);
     public

       procedure ConstructXMLString;  override;
       procedure SetDefaultAttribs;   override;

     published
       property rx: String read Getrx write Setrx;
       property ry: String read Getry write Setry;
end;

type
   TXSVGCircle = class(TXSVGWidget)
     private
       function getRadius: String;
       procedure setRadius(AValue:string);
     public

       procedure ConstructXMLString;  override;
       procedure SetDefaultAttribs;   override;

     published
       property Radius: String read GetRadius write SetRadius;
end;

type
TXSVGEllipse = class(TXSVGWidget)
 private
   function getrx: String;
   function getry: String;
   procedure setrx(AValue:string);
   procedure setry(AValue:string);
 public

   procedure ConstructXMLString;  override;
   procedure SetDefaultAttribs;   override;

 published
   property rx: String read Getrx write Setrx;
   property ry: String read Getry write Setry;
end;

type
TXSVGLine = class(TXSVGWidget)
  private
    function getX1: String;
    function getX2: String;
    function getY1: String;
    function getY2: String;
    procedure setX1(AValue:string);
    procedure setX2(AValue:string);
    procedure setY1(AValue:string);
    procedure setY2(AValue:string);
  public

    procedure ConstructXMLString;  override;
    procedure SetDefaultAttribs;    override;

  published
    property X1: String read GetX1 write SetX1;
    property Y1: String read GetY1 write SetY1;
    property X2: String read GetX2 write SetX2;
    property Y2: String read GetY2 write SetY2;
end;

type
TXSVGPolyLine = class(TXSVGWidget)
  private
    function getXCoords: String;
    function getYCoords: String;
    procedure setXCoords(AValue:string);
    procedure setYCoords(AValue:string);
  public

    procedure ConstructXMLString;  override;
    procedure SetDefaultAttribs;    override;

  published
    property XCoords: String read GetXCoords write SetXCoords;
    property YCoords: String read GetYCoords write SetYCoords;
 end;

type
TXSVGPolyGon = class(TXSVGPolyLine)
  private
  public

    procedure ConstructXMLString;  override;
    procedure SetDefaultAttribs;    override;

  published
 end;


implementation

var
  SVGDefaultAttribs:TDefaultAttributesArray;

procedure TXSVGWidget.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
end;

procedure TXSVGContainer.ConstructXMLString(myName,NameSpace:String);
// Construct the xml string, and set the attribute value in the data node.
var
  JavascriptString,stylestring,SvgString:string;
begin
    JavascriptString:='<script> '
    +'  document.title = "'+myNode.NodeName+' '+myNode.NodeType+'"; '        //!!!!namespace
    +'  document.documentElement.addEventListener("click", function( event ) { event.stopPropagation();'
 //   +'    alert("click event on "+event.target.id); '
    +'    var x=event.clientX; '
    +'    var y=event.clientY; '
    +'    var ts= "";'
    +'    var t=event.target;'
    +'    var targetstring = "" ;'
    +'    if ((t!=null)&&(t.attributes.length>0)) '
    +'    { ts = t.id; '
    +'      targetstring = "'+myNode.NodeName+' '+myNode.NodeType+' Target = >'+ attributeListdelimiter + '"+ts+"' + attributeListdelimiter +'";  '
//    +'      targetstring = "Target = >'+ attributeListdelimiter + '"+ts+"' + attributeListdelimiter +'";  '
 //   +'      alert("changing document title to: "+targetstring +  x + "'+attributeListdelimiter+'"+ y + "'+attributeListdelimiter+'"); '
    +'      document.title = targetstring +  x + "'+attributeListdelimiter+'"+ y + "'+attributeListdelimiter+'";'
    {$ifdef JScript}
    +'      parent.postMessage(document.title,"*");  '
    {$endif}
    +     '}'
    +'   }, false); '
     +'</script> '  ;

    stylestring:=' <style>    '
    +' a:focus {              '
     +'  stroke: black;         '
     +'  stroke-width: 5;     '
     +'}                     '
     +'</style>              ';

    //!!!! issues with sizing.  The internal iframe document body has a 8px border - unable to remove this ????

    // find the current size of the iframe container
//    {$ifndef JScript}
//    {$else}
//    asm
//    //alert('looking for '+this.NodeName);
//    var ob = document.getElementById(this.NameSpace+this.NodeName);
//    if (ob!=null) {
//      var style = window.getComputedStyle(ob);
//      var hh = style.height;
//      FrameHeight = parseInt(hh, 10) - 16;
//      var ww = style.height;
//      FrameWidth = parseInt(hh, 10) - 8;
//      alert('FrameWidth='+FrameWidth+' height='+FrameHeight);
//    }
//    end;
//    {$endif}

    SvgString:= '<svg id="'+NameSpace+myName+'" top="0" left="0" '
              //+' width="'+IntToStr(FrameWidth)+'px" height="'+IntToStr(FrameHeight)+'px" overflow:"hidden" position:"fixed">'
              +' width="100%" height="92%" overflow:"hidden" position:"fixed">'
              +'</svg> ';


    self.myNode.SetAttributeValue('XMLString',JavascriptString+stylestring+SvgString);

end;

function SVGItemFromTitle(IFrameTitle:String):TDataNode;
var
  bits,items:TStringList;
  ItemID, XStr, YStr:string;
begin
  bits:=TStringList.Create;
  bits.StrictDelimiter:=true;
  bits.LineBreak:='>';
  bits.Text:=IFrameTitle;
  if bits.Count>1 then
  begin
    items:=TStringList.Create;
    items.StrictDelimiter:=true;
    items.LineBreak:=attributeListdelimiter;
    items.Text:=bits[1];
    if items.Count>3 then
    begin
      ItemId:=items[1];
      XStr:=items[2];
      YStr:=items[3];

      if ItemId<>'' then
        result:=FindDataNodeById(SystemNodeTree,ItemId,'',false)     //!!!! deal with NameSpace
      else
        result:=nil;
    end
    else
      result:=nil;
  end
  else
    result:=nil;
end;

{$ifndef JScript}
procedure Register;
begin
{$I XSVGFrame.lrs}
{$ifdef Chromium}
  RegisterComponents('XComponents',[TXSVGContainer]);
{$endif}
  RegisterPropertyEditor(TypeInfo(string), TXSVGContainer, 'HTMLSource', THiddenPropertyEditor);

end;

procedure TXSVGContainer.Loaded;
var
i:integer;
begin
  inherited Loaded;
  if not (csDesigning in componentState) then
  begin
    self.ConstructXMLString(self.Name,'');         //!!!!namespace??
    self.XMLString:=self.XMLString;    // load the string and refresh display
  end;
end;


procedure TXSVGContainer.ReLoadURL;
begin
  if (self.SuspendRefresh)
  {$ifndef JScript}
  or (GlobalSuppressFrameDisplay)
  {$endif}
  then
    EXIT;

  {$ifdef Chromium}
  if (self.myControl<>nil)
  and (not (csDesigning in componentState))
  and (myChromium.Initialized=true) then
  begin
    myChromium.Browser.MainFrame.LoadURL('data:text/html, '+UTF8Decode(self.FullXMLString));
  end;
  {$endif}
end;

function CreateSVGContainerWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXSVGContainer',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  TXSVGContainer(NewNode.ScreenObject).constructXMLString(NewNode.NodeName,NameSpace);
  result:=NewNode;
end;

procedure TXSVGContainer.DoSVGConstructor;
begin
  self.IsContainer:=true;
  {$ifdef Chromium}
  myChromium.OnTitleChange:=@self.TitleChange;
  {$endif}

  MyEventTypes.Add('MouseDown');
  self.myNode.NodeType:='TXSVGContainer';
  AddDefaultAttribs(self,self.myNode,SVGdefaultAttribs);
  self.constructXMLString(self.myNode.NodeName,self.myNode.NameSpace);
end;

constructor TXSVGContainer.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoSVGConstructor;
end;

constructor TXSVGContainer.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoSVGConstructor;
end;

{$ifdef Chromium}
procedure TXSVGContainer.TitleChange(Sender: TObject;const cefBrowser:ICefBrowser;const NewTitle:UString) ;
var
  params:TStringList;
  c:integer;
  SVGWidget:TXSVGWidget;
  ItemNode:TDataNode;
  TempMsg : ICefProcessMessage;
begin
  if (not (csDesigning in componentState))
  and (not StartingUp)
  and (self.myNode<>nil) then
  begin


    if (FoundString(NewTitle,'Target = >')>0) then
    begin
      params:=stringsplit(NewTitle,attributeListdelimiter);
      c:=params.count;
      if params.Count=4 then
      begin
        FrameTitle:=NewTitle;
        // want the last 3 elements (targetname, X, Y) !!!!
        ItemNode:=SVGItemFromTitle(NewTitle);

        if (ItemNode<>nil)
        and (ItemNode.NodeClass='SVG')
        and (ItemNode.ScreenObject<>self) then
        begin
          SVGWidget:=TXSVGWidget(ItemNode.ScreenObject);
          CallHandleEventLater('Click',NewTitle,SVGWidget);      //!!!! causes fallover whether on a timer or not !!!!
        end
        else
        begin
          CallHandleEventLater('Click',NewTitle,self);
        end;
      end;
    end;
  end;
end;
{$endif}

{$else} //JScript
constructor TXSVGContainer.Create(MyForm:TForm;NodeName,NameSpace:String);
begin
  inherited Create(MyForm,NodeName,NameSpace);
  self.NodeType:='TXSVGContainer';
  self.IsContainer:=true;
  SetNodePropDefaults(self,SVGDefaultAttribs);
end;

function CreateSVGContainerWidget(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
begin
  //showmessage('CreateSVGContainerWidget');

  DoCreateFrameWidget(MyNode, ParentNode,ScreenObjectName,position);
  TXSVGContainer(MyNode).ConstructXMLString(ScreenObjectName,NameSpace);
  result:=myNode;
end;

function CreateinterfaceObjSVG(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXSVGContainer.Create(MyForm,NodeName,NameSpace));
end;


function TXSVGWidget.SetupWidget(ParentNode:TDataNode;position:integer):TDataNode;
var
  NewNode:TDataNode;
begin
//  showmessage('TXSVGWidget.SetupWidget 1');
  NewNode:=self.myNode;
  AddChildToParentNode(ParentNode,NewNode,position);

  SetLength(NewNode.myEventHandlers,NewNode.myEventTypes.Count);

  RefreshComponentProps(myNode);

//  showmessage('TXSVGWidget.SetupWidget 2');
  result:=NewNode;
end;

procedure HandleMessage(msg:String);
// Expecting a Title change message from an embedded Chromium (CEF) IFrame
var
  ItemNode:TdataNode;
begin
  //showmessage('HandleMessage XIframe');
  if (FoundString(msg,'Target = >')>0) then
  begin
     //this is a title change notification sent out from inside an IFrame.
     ItemNode:=SVGItemFromTitle(msg);
     if ItemNode<>nil then
     begin
       handleEvent('Click',ItemNode.NodeName, ItemNode.NameSpace,msg);
     end;
  end;

end;
{$endif}

procedure TXSVGContainer.SetSuspendRefresh(AValue: Boolean);
begin
  if (myNode<>nil)
  {$ifndef JScript}
  and ((not (csLoading in componentState)) or (AValue=true))
  {$endif}
  then
  begin
    myNode.SetAttributeValue('SuspendRefresh',myBoolToStr(AValue),'Boolean');
    if AValue=false then
      {$ifndef JScript}
      // if the frame is on a visible form...
      if TXForm(self.myNode.MyForm).Showing <> 'No' then
      {$endif}
      begin
        // for an svg container, rebuild the HTMLSource string from child components
        myNode.SetAttributeValue('HTMLSource',TXSVGContainer(self).FullXMLString);
        self.RedisplayFrame;
      end;
  end;
end;

function TXSVGContainer.GetXMLString:String;
begin
  result:=myNode.getAttribute('XMLString',true).AttribValue;     // this excludes child data
end;

function TXSVGContainer.FullXMLString:String;
var
  SVGString,childdata:string;
  i:integer;
  childNode:TDataNode;
begin
  childdata:='';
  SVGString:=myNode.getAttribute('XMLString',true).AttribValue;
  // There may be some children of this node, in which case they are all SVG elements, and we need
  // to include those elements in the result here.
  for i:=0 to length(myNode.ChildNodes)-1 do
  begin
    childNode:=myNode.ChildNodes[i];
    childdata:=childdata + childNode.GetAttribute('XMLString',true).AttribValue;
  end;

  result:=SpliceSVGChildStrings(SVGString,childdata);
end;

function TXSVGContainer.XMLWarnings(SVGString:String):String;
var
  FilteredSVGString:string;
begin
  WarnAboutTTypeBezierCurves(SVGString); // T type bezier curves are not supported in this SVG component;
//  FilteredSVGString:=DelChars(SVGString,'%');
//  if( length(FilteredSVGString)<> length(SVGString))
//  then
//  begin
//    ShowMessage('Warning % dimension statements are not supported by this SVG component');
//  end;
//  SVGString := FilteredSVGString;
  FilteredSVGString:= myStringReplace(SVGString,'transparent','none',-1,-1);
  if( length(FilteredSVGString)<> length(SVGString))
  then
  begin
    ShowMessage('Warning the fill type of "transparent" is not supported by this SVG component - it has been replaced with "none"');
  end;
  SVGString := FilteredSVGString;
end;

procedure TXSVGContainer.SetXMLString(AValue:string);
var
  SVGString,FilteredSVGString:string;
begin
  {$ifndef JScript}
  if not (csLoading in componentState) then
  {$endif}
  begin
    SVGString:=AValue;
    FilteredSVGString:=self.XMLWarnings(SVGString);

    myNode.SetAttributeValue('XMLString',SVGString);

    {$ifndef JScript}
    SVGString:='';
    {$else}
    SVGString:=self.FullXMLString; // adds in the child data
    {$endif}
    //{$ifdef JScript} showmessage('setXMLString '+SVGString);{$endif}
    myNode.SetAttributeValue('HTMLSource',SVGString);
    self.RedisplayFrame;

  end;

end;

  function  TXSVGContainer.SpliceSVGChildStrings(parentstring,childstring:string):string;
  var parentlength, endTagPos,i,j:integer;
      found:boolean;
      startstring, endstring,returnstr:string;
  begin
    parentlength:=length(parentstring);
    found:=false;
    endTagPos:=0;
    startstring:='';
    endstring:='';
    returnstr:='';
    for i:= parentlength downto 1 do
    begin
      if found = false
      then
        //this finds the start of the  closing SVG tag if this is a valid SVG string
        if parentstring[i]='<'
        then
        begin
          for j:= i to parentlength do endstring:=endstring+ parentstring[j];
          for j:= 1 to i-1 do startstring:=startstring+ parentstring[j];
          if '</svg>' = lowercase(trimwhitespace(endstring))
          then
          begin
            endTagPos:=i;
            found:=true;
            returnstr:=startstring+ ' '+ childstring+' '+endstring;
          end
          else ShowMessage('SVG string is not valid:  ' + parentstring);
        end;
    end;
    //ShowMessage('after splice: '+returnstr);
    result:= returnstr;
  end;

  function TXSVGContainer.WarnAboutTTypeBezierCurves(instring:string):string;
  var startOfPathStatement,startOfPathData,i:integer;
    tempstr:string;
    endOfData:Boolean;
  begin
    startOfPathStatement:=FoundString(inString,'<path');
    if startOfPathStatement > 0 then
    begin
      endOfData:=false;
      startOfPathData:=FoundString(inString,'d=');
      if startOfPathData>0 then
      begin
        for i:= startOfPathData+2 to length(instring) do
        begin
          if endOfData=false then
          begin
            tempstr:= instring[i];
            if ((tempstr='t')or  (tempstr='T')) and (instring[i-1]=' ')
                then ShowMessage('Bezier curves of type "T" are not supported by this SVG component - please use an alternative form');
            if (tempstr='"') or (tempstr='>')  then endOfData:=true;
          end;
        end;
      end;
    end;
  end;

procedure TXSVGWidget.ConstructXMLString;
begin
  //showmessage('TXSVGWidget.ConstructXMLString.  does nothing');
end;

procedure TXSVGWidget.SetDefaultAttribs;
begin
  //showmessage('TXSVGWidget.SetDefaultAttribs.  does nothing');
end;


{$ifndef JScript}
function TXSVGWidget.CommonSetup(myNameSpace:String;ParentNode:TdataNode;position:integer):TdataNode;
var
  NewNode:TDataNode;
begin
  self.SetMyEventTypes;
  self.myNode:=TDataNode(self);
  NewNode:=self.myNode;
  NewNode.ScreenObject:=self;
  NewNode.NameSpace:=myNameSpace;
  AddChildToParentNode(ParentNode,NewNode,position);

  result:=NewNode;
end;
{$else}
  procedure TXSVGWidget.FinishSVGInObCreate;
  begin
    self.myNode:=TDataNode(self);
    self.SetMyEventTypes;
    self.SetDefaultAttribs;
  end;
{$endif}
//---------------------- SVGText ----------------------------------------------

procedure TXSVGText.ConstructXMLString;
begin
  //showmessage('setting text svg string');
  self.XMLString:=' <text id="'+self.NameSpace+self.NodeName+'" x="'+XPos+'" y="'+YPos
              +'" transform="rotate('+rotate+','+XPos+','+YPos+')"'
              +' style="font-family: '+fontFamily+'; font-weight:'+fontWeight+';font-size:'+height+'; font-style: '+fontStyle+'" >'
              +TextString
              +'<title>'+Hint+'</title>'
              +' </text>';
end;


procedure TXSVGText.SetDefaultAttribs;
begin
  self.myNode.SetAttributeValue('XPos','50');
  self.myNode.SetAttributeValue('YPos','50');
  self.myNode.SetAttributeValue('TextString','SVG Text Example');
  self.myNode.SetAttributeValue('Height','20');
  self.myNode.SetAttributeValue('FontFamily',' impact, georgia, times, serif;');
  self.myNode.SetAttributeValue('FontWeight',' normal');
  self.myNode.SetAttributeValue('FontStyle',' normal');
  self.myNode.SetAttributeValue('Rotate','0');
  self.ConstructXMLString;
end;

{$ifndef JScript}
function addSVGText(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
   NewWidget:TXSVGText;
   NewNode:TDataNode;
begin

   NewWidget:=TXSVGText.Create('SVG',ScreenObjectName,NameSpace,'TXSVGText',true);
   NewNode:=NewWidget.CommonSetup(NameSpace,ParentNode,position);
   NewWidget.SetDefaultAttribs;

   result:=NewNode;
end;
{$else}
function CreateInObSVGText(MyForm:TForm;NodeName,NameSpace:String):TObject;
var
  NewObj:TXSVGText;
begin
  NewObj:=TXSVGText.Create('SVG',NodeName,NameSpace,'TXSVGText',true);
  NewObj.FinishSVGInObCreate;
  result:=NewObj;
end;

function CreateWidgetSVGText(myNode,ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
   var
     NewWidget:TXSVGText;
  begin
     NewWidget:=TXSVGText(myNode);

     NewWidget.XPos:=NewWidget.XPos;
     NewWidget.YPos:=NewWidget.YPos;
     NewWidget.TextString:=NewWidget.TextString;
     NewWidget.Height:=NewWidget.Height;
     NewWidget.FontFamily:=NewWidget.FontFamily;
     NewWidget.FontWeight:=NewWidget.FontWeight;
     NewWidget.FontStyle:=NewWidget.FontStyle;
     NewWidget.Rotate:=NewWidget.Rotate;

     result:=NewWidget.SetupWidget(ParentNode,position);
  end;
{$endif}

function ColorToStr(Clr:TColor):String;
var
  s:string;
begin
    {$ifndef JScript}
    s:=ColorToHexRGB(Clr);
    {$ifdef Chromium}
    result:=s.Replace('#','%23');
    {$else}
    result:=s;
    {$endif}
    {$else}
    result:=Clr;
    {$endif}
end;

function FillColorToStr(Clr:TColor;Transparent:Boolean):String;
begin
  if Transparent then
    result:='none'
  else
    result:=ColorToStr(Clr);
end;

//---------------------- SVGRect ----------------------------------------------

procedure TXSVGRect.ConstructXMLString;
begin
    self.XMLString:='<rect id="'+self.NameSpace+self.NodeName+'" x="'+XPos+'" y="'+YPos
              +'" transform="rotate('+rotate+','+XPos+','+YPos+')" width="'+Width+'" height="'+Height
              +'" stroke="'+ColorToStr(StrokeColor)+'" fill="'+FillColorToStr(FillColor,FillTransparent)+'" stroke-width="'+StrokeWidth+'" >'
    +'<title>'+Hint+'</title>'
    +' </rect>';
end;

procedure TXSVGRect.SetDefaultAttribs;
begin
  self.myNode.SetAttributeValue('XPos','50');
  self.myNode.SetAttributeValue('YPos','50');
  self.myNode.SetAttributeValue('Width','50');
  self.myNode.SetAttributeValue('Height','50');
  self.myNode.SetAttributeValue('StrokeWidth','3');
  self.myNode.SetAttributeValue('StrokeColor','#BBBBBB','Color');
  self.myNode.SetAttributeValue('FillColor','#FF0000','Color');
  self.myNode.SetAttributeValue('FillTransparent','False','Boolean');
  self.myNode.SetAttributeValue('Rotate','0');
  self.ConstructXMLString;
end;

{$ifndef JScript}
function addSVGRect(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
   var
     NewWidget:TXSVGRect;
     NewNode:TDataNode;
  begin
     NewWidget:=TXSVGRect.Create('SVG',ScreenObjectName,NameSpace,'TXSVGRect',true);
     NewNode:=NewWidget.CommonSetup(NameSpace,ParentNode,position);
     NewWidget.SetDefaultAttribs;
     result:=NewNode;
  end;
{$else}
function CreateInObSVGRect(MyForm:TForm;NodeName,NameSpace:String):TObject;
var
  NewObj:TXSVGRect;
begin
  //showmessage('CreateInObSVGRect '+NodeName);
  NewObj:=TXSVGRect.Create('SVG',NodeName,NameSpace,'TXSVGRect',true);
  NewObj.FinishSVGInObCreate;
  result:=NewObj;
end;

function CreateWidgetSVGRect(myNode,ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
   var
     NewWidget:TXSVGRect;
  begin
  //showmessage('CreateWidgetSVGRect');
     NewWidget:=TXSVGRect(myNode);

     NewWidget.XPos:=NewWidget.XPos;
     NewWidget.YPos:=NewWidget.YPos;
     NewWidget.Width:=NewWidget.Width;
     NewWidget.Height:=NewWidget.Height;
     NewWidget.StrokeWidth:=NewWidget.StrokeWidth;
     NewWidget.StrokeColor:=NewWidget.StrokeColor;
     NewWidget.FillColor:=NewWidget.FillColor;
     NewWidget.FillTransparent:=NewWidget.FillTransparent;
     NewWidget.Rotate:=NewWidget.Rotate;

     result:=NewWidget.SetupWidget(ParentNode,position);
  end;
{$endif}

//---------------------- SVGRoundedRect ----------------------------------------------

procedure TXSVGRoundedRect.ConstructXMLString;
begin
  //{$ifdef JScript}
  //showmessage('TXSVGRoundedRect.ConstructXMLString. ');
  //{$endif}
    self.XMLString:='<rect id="'+self.NameSpace+self.NodeName+'" x="'+XPos+'" y="'+YPos+'" rx="'+rx+'" ry="'+ry
              +'" title="'+Hint
              +'" transform="rotate('+Rotate+','+XPos+','+YPos+')" width="'+Width+'" height="'+Height
              +'" stroke="'+ColorToStr(StrokeColor)+'" fill="'+FillColorToStr(FillColor,FillTransparent)+'" stroke-width="'+StrokeWidth+'" >'
    +'<title>'+Hint+'</title>'
    +' </rect>';
end;

procedure TXSVGRoundedRect.SetDefaultAttribs;
begin
  self.myNode.SetAttributeValue('XPos','50');
  self.myNode.SetAttributeValue('YPos','50');
  self.myNode.SetAttributeValue('Rx','10');
  self.myNode.SetAttributeValue('Ry','10');
  self.myNode.SetAttributeValue('Width','50');
  self.myNode.SetAttributeValue('Height','50');
  self.myNode.SetAttributeValue('StrokeWidth','3');
  self.myNode.SetAttributeValue('StrokeColor','#BBBBBB','Color');
  self.myNode.SetAttributeValue('FillColor','#FF0000','Color');
  self.myNode.SetAttributeValue('FillTransparent','False','Boolean');
  self.myNode.SetAttributeValue('Rotate','0');
  self.ConstructXMLString;
end;

{$ifndef JScript}
function addSVGRoundedRect(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
   var
     NewWidget:TXSVGRoundedRect;
     NewNode:TDataNode;
  begin
     NewWidget:=TXSVGRoundedRect.Create('SVG',ScreenObjectName,NameSpace,'TXSVGRoundedRect',true);
     NewNode:=NewWidget.CommonSetup(NameSpace,ParentNode,position);
     NewWidget.SetDefaultAttribs;
     result:=NewNode;
  end;
{$else}
function CreateInObSVGRoundedRect(MyForm:TForm;NodeName,NameSpace:String):TObject;
var
  NewObj:TXSVGRoundedRect;
begin
  //showmessage('CreateInObSVGRoundedRect '+NodeName);
  NewObj:=TXSVGRoundedRect.Create('SVG',NodeName,NameSpace,'TXSVGRoundedRect',true);
  NewObj.FinishSVGInObCreate;
  result:=NewObj;
end;

function CreateWidgetSVGRoundedRect(myNode,ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
 var
   NewWidget:TXSVGRoundedRect;
begin
  //showmessage('CreateWidgetSVGRoundedRect ');
   NewWidget:=TXSVGRoundedRect(myNode);

   NewWidget.XPos:=NewWidget.XPos;
   NewWidget.YPos:=NewWidget.YPos;
   NewWidget.Rx:=NewWidget.Rx;
   NewWidget.Ry:=NewWidget.Ry;
   NewWidget.Width:=NewWidget.Width;
   NewWidget.Height:=NewWidget.Height;
   NewWidget.StrokeWidth:=NewWidget.StrokeWidth;
   NewWidget.StrokeColor:=NewWidget.StrokeColor;
   NewWidget.FillColor:=NewWidget.FillColor;
   NewWidget.FillTransparent:=NewWidget.FillTransparent;
   NewWidget.Rotate:=NewWidget.Rotate;

   result:=NewWidget.SetupWidget(ParentNode,position);
end;


{$endif}

//---------------------- SVGCircle ----------------------------------------------

procedure TXSVGCircle.ConstructXMLString;
begin
//showmessage('ConstructXMLString '+NodeName);
  self.XMLString:='<circle id="'+self.NameSpace+self.NodeName+'" cx="'+XPos+'" cy="'+YPos
           +'" transform="rotate('+rotate+','+XPos+','+YPos+')" r="'+Radius
           +'" stroke="'+ColorToStr(StrokeColor)+'" fill="'+FillColorToStr(FillColor,FillTransparent)
           +'" stroke-width="'+StrokeWidth+' "> '
  +'<title>'+Hint+'</title>'
  +'</circle>';
end;

procedure TXSVGCircle.SetDefaultAttribs;
begin
//showmessage('SetDefaultAttribs '+NodeName);
  self.myNode.SetAttributeValue('YPos','75');
  self.myNode.SetAttributeValue('XPos','25');
  self.myNode.SetAttributeValue('Radius','20');
  self.myNode.SetAttributeValue('StrokeWidth','5');
  self.myNode.SetAttributeValue('StrokeColor','#00FF00','Color');
  self.myNode.SetAttributeValue('FillColor','#FFFF00','Color');
  self.myNode.SetAttributeValue('FillTransparent','False','Boolean');
  self.myNode.SetAttributeValue('Rotate','0');
  self.ConstructXMLString;
end;

{$ifndef JScript}
function addSVGCircle(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
   var
     NewWidget:TXSVGCircle;
     NewNode:TDataNode;
  begin
     NewWidget:=TXSVGCircle.Create('SVG',ScreenObjectName,NameSpace,'TXSVGCircle',true);
     NewNode:=NewWidget.CommonSetup(NameSpace,ParentNode,position);
     NewWidget.SetDefaultAttribs;
     result:=NewNode;
 end;
{$else}
function CreateInObSVGCircle(MyForm:TForm;NodeName,NameSpace:String):TObject;
var
  NewObj:TXSVGCircle;
begin
 // showmessage('CreateInObSVGCircle '+NodeName);
  NewObj:=TXSVGCircle.Create('SVG',NodeName,NameSpace,'TXSVGCircle',true);
  NewObj.FinishSVGInObCreate;
  result:=NewObj;
end;

function CreateWidgetSVGCircle(myNode,ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
 var
   NewWidget:TXSVGCircle;
begin
//showmessage('CreateWidgetSVGCircle '+myNode.NodeName);
   NewWidget:=TXSVGCircle(myNode);

   NewWidget.XPos:=NewWidget.XPos;
   NewWidget.YPos:=NewWidget.YPos;
   NewWidget.Radius:=NewWidget.Radius;
   NewWidget.StrokeWidth:=NewWidget.StrokeWidth;
   NewWidget.StrokeColor:=NewWidget.StrokeColor;
   NewWidget.FillColor:=NewWidget.FillColor;
   NewWidget.FillTransparent:=NewWidget.FillTransparent;
   NewWidget.Rotate:=NewWidget.Rotate;

   result:=NewWidget.SetupWidget(ParentNode,position);
end;


{$endif}

//---------------------- SVGEllipse ----------------------------------------------

procedure TXSVGEllipse.ConstructXMLString;
begin
//showmessage('TXSVGEllipse.ConstructXMLString '+NodeName);
  self.XMLString:='<ellipse id="'+self.NameSpace+self.NodeName+'" cx="'+XPos+'"  cy="'+YPos+'"  rx="'+Rx+'" ry="'+Ry+'"'
           +' transform="rotate('+Rotate+','+XPos+','+YPos+')"'
           +' stroke="'+ColorToStr(StrokeColor)+'" fill="'+FillColorToStr(FillColor,FillTransparent)+'" stroke-width="'+StrokeWidth+'" > '
  +'<title>'+Hint+'</title>'
  +' </ellipse>';
end;

procedure TXSVGEllipse.SetDefaultAttribs;
begin
//showmessage('SetDefaultAttribs '+NodeName);
  self.myNode.SetAttributeValue('YPos','75');
  self.myNode.SetAttributeValue('XPos','100');
  self.myNode.SetAttributeValue('Rx','20');
  self.myNode.SetAttributeValue('Ry','50');
  self.myNode.SetAttributeValue('StrokeWidth','5');
  self.myNode.SetAttributeValue('StrokeColor','#66FF66','Color');
  self.myNode.SetAttributeValue('FillColor','#AABBCC','Color');
  self.myNode.SetAttributeValue('FillTransparent','False','Boolean');
  self.myNode.SetAttributeValue('Rotate','0');
  self.ConstructXMLString;
end;

{$ifndef JScript}
function addSVGEllipse(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
   var
     NewWidget:TXSVGEllipse;
     NewNode:TDataNode;
  begin
     NewWidget:=TXSVGEllipse.Create('SVG',ScreenObjectName,NameSpace,'TXSVGEllipse',true);
     NewNode:=NewWidget.CommonSetup(NameSpace,ParentNode,position);
     NewWidget.SetDefaultAttribs;
     result:=NewNode;
  end;
{$else}
function CreateInObSVGEllipse(MyForm:TForm;NodeName,NameSpace:String):TObject;
var
  NewObj:TXSVGEllipse;
begin
 // showmessage('CreateInObSVGEllipse '+NodeName);
  NewObj:=TXSVGEllipse.Create('SVG',NodeName,NameSpace,'TXSVGEllipse',true);
  NewObj.FinishSVGInObCreate;
  result:=NewObj;
end;

function CreateWidgetSVGEllipse(myNode,ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
 var
   NewWidget:TXSVGEllipse;
begin
//showmessage('CreateWidgetSVGEllipse '+myNode.NodeName);
   NewWidget:=TXSVGEllipse(myNode);

   NewWidget.XPos:=NewWidget.XPos;
   NewWidget.YPos:=NewWidget.YPos;
   NewWidget.Rx:=NewWidget.Rx;
   NewWidget.Ry:=NewWidget.Ry;
   NewWidget.StrokeWidth:=NewWidget.StrokeWidth;
   NewWidget.StrokeColor:=NewWidget.StrokeColor;
   NewWidget.FillColor:=NewWidget.FillColor;
   NewWidget.FillTransparent:=NewWidget.FillTransparent;
   NewWidget.Rotate:=NewWidget.Rotate;

   result:=NewWidget.SetupWidget(ParentNode,position);
end;
{$endif}

//---------------------- SVGLine ----------------------------------------------

procedure TXSVGLine.ConstructXMLString;
begin
//showmessage('Line. ConstructXMLString '+NodeName);
  self.XMLString:='<line id="'+self.NameSpace+self.NodeName+'" x1="'+ X1+'" x2="'+X2+'" y1="'+Y1+'" y2="'+Y2+'"'
          +' transform="rotate('+rotate+','+X1+','+Y1+')"'
          +' stroke="'+ColorToStr(strokeColor)+'" stroke-width="'+strokeWidth+'" > '
  +'<title>'+Hint+'</title>'
  +' </line>';
end;

procedure TXSVGLine.SetDefaultAttribs;
begin
  //showmessage('Line. SetDefaultAttribs '+NodeName);
  self.myNode.SetAttributeValue('X1','19');
  self.myNode.SetAttributeValue('Y1','110');
  self.myNode.SetAttributeValue('X2','50');
  self.myNode.SetAttributeValue('Y2','150');
  self.myNode.SetAttributeValue('StrokeWidth','5');
  self.myNode.SetAttributeValue('StrokeColor','#66FF66','Color');
  self.myNode.SetAttributeValue('Rotate','0');
  self.ConstructXMLString;
end;

{$ifndef JScript}
function addSVGLine(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
   var
     NewWidget:TXSVGLine;
     NewNode:TDataNode;
  begin
     NewWidget:=TXSVGLine.Create('SVG',ScreenObjectName,NameSpace,'TXSVGLine',true);
     NewNode:=NewWidget.CommonSetup(NameSpace,ParentNode,position);
     NewWidget.SetDefaultAttribs;
     result:=NewNode;
  end;
{$else}
function CreateInObSVGLine(MyForm:TForm;NodeName,NameSpace:String):TObject;
var
  NewObj:TXSVGLine;
begin
  //showmessage('CreateInObSVGLine '+NodeName);
  NewObj:=TXSVGLine.Create('SVG',NodeName,NameSpace,'TXSVGLine',true);
  NewObj.FinishSVGInObCreate;
  result:=NewObj;
end;

function CreateWidgetSVGLine(myNode,ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
 var
   NewWidget:TXSVGLine;
begin
//showmessage('CreateWidgetSVGLine '+myNode.NodeName);
   NewWidget:=TXSVGLine(myNode);

   NewWidget.X1:=NewWidget.X1;
   NewWidget.Y1:=NewWidget.Y1;
   NewWidget.X2:=NewWidget.X2;
   NewWidget.Y2:=NewWidget.Y2;
   NewWidget.StrokeWidth:=NewWidget.StrokeWidth;
   NewWidget.StrokeColor:=NewWidget.StrokeColor;
   NewWidget.Rotate:=NewWidget.Rotate;

   result:=NewWidget.SetupWidget(ParentNode,position);
end;
{$endif}

//---------------------- SVGPolyLine ----------------------------------------------

// stringlist from comma-delimited string
function GetArrayFromString(instring:String):TIntArray;
var stringlist :TStringList;
   numpoints,i:integer;
   resultArray: TIntArray;
begin
 stringlist:=stringsplit(instring,',' );
 numpoints:= stringlist.Count;
 setlength(resultArray,numpoints);
 for i:= 0 to numpoints-1 do
 begin
   resultArray[i]:=strtoint(TrimWhiteSpace(stringlist[i]));
 end;
 result:=  resultArray;
end;

procedure TXSVGPolyLine.ConstructXMLString;
var
  str:string;
  XArray,YArray:TIntArray;
  i,numpoints:integer;
begin
  //showmessage('PolyLine. ConstructXMLString '+NodeName);
  XArray:= GetArrayFromString(XCoords);
  YArray:= GetArrayFromString(YCoords);
  str:=  '<polyline id="'+self.NameSpace+self.NodeName+'" points="';
  numpoints:= length(XArray);
  for i:=0 to numpoints-1 do
  begin
    str:=str +inttostr(XArray[i])+' '+inttostr(YArray[i])+' ';
  end;
  self.XMLString:=str+ '" transform="rotate('+Rotate+','+inttostr(XArray[0])+','+inttostr(YArray[0])+')"'
                      +' stroke="'+ColorToStr(StrokeColor)+'" fill="'+FillColorToStr(FillColor,FillTransparent)+'" stroke-width="'+StrokeWidth+'" > '
  +'<title>'+Hint+'</title>'
  +' </polyline>';
end;

procedure TXSVGPolyLine.SetDefaultAttribs;
begin
  //showmessage('PolyLine. SetDefaultAttribs '+NodeName);
  self.myNode.SetAttributeValue('XCoords','60 , 65 , 70 , 75  , 80 , 85 , 90,  95 , 100 ');
  self.myNode.SetAttributeValue('YCoords','110, 120, 115 ,130 ,125 ,140, 135 ,150 , 145');
  self.myNode.SetAttributeValue('StrokeWidth','2');
  self.myNode.SetAttributeValue('FillColor','#FFFFFF','Color');
  self.myNode.SetAttributeValue('FillTransparent','False','Boolean');
  self.myNode.SetAttributeValue('StrokeColor','#000000','Color');
  self.myNode.SetAttributeValue('Rotate','0');
  ConstructXMLString;
end;

{$ifndef JScript}
function addSVGPolyLine(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
   var
     NewWidget:TXSVGPolyLine;
     NewNode:TDataNode;
  begin
     NewWidget:=TXSVGPolyLine.Create('SVG',ScreenObjectName,NameSpace,'TXSVGPolyLine',true);
     NewNode:=NewWidget.CommonSetup(NameSpace,ParentNode,position);
     NewWidget.SetDefaultAttribs;
     result:=NewNode;
  end;
{$else}
function CreateInObSVGPolyLine(MyForm:TForm;NodeName,NameSpace:String):TObject;
var
  NewObj:TXSVGPolyLine;
begin
  //showmessage('CreateInObSVGPolyLine '+NodeName);
  NewObj:=TXSVGPolyLine.Create('SVG',NodeName,NameSpace,'TXSVGPolyLine',true);
  NewObj.FinishSVGInObCreate;
  result:=NewObj;
end;

function CreateWidgetSVGPolyLine(myNode,ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
 var
   NewWidget:TXSVGPolyLine;
begin
//showmessage('CreateWidgetSVGPolyLine '+myNode.NodeName);
   NewWidget:=TXSVGPolyLine(myNode);

   NewWidget.XCoords:=NewWidget.XCoords;
   NewWidget.YCoords:=NewWidget.YCoords;
   NewWidget.StrokeWidth:=NewWidget.StrokeWidth;
   NewWidget.StrokeColor:=NewWidget.StrokeColor;
   NewWidget.FillColor:=NewWidget.FillColor;
   NewWidget.FillTransparent:=NewWidget.FillTransparent;
   NewWidget.Rotate:=NewWidget.Rotate;

   result:=NewWidget.SetupWidget(ParentNode,position);
end;
{$endif}

//---------------------- SVGPolyGon ----------------------------------------------

procedure TXSVGPolyGon.ConstructXMLString;
var
  str:string;
  XArray,YArray:TIntArray;
  i,numpoints:integer;
begin
  //showmessage('PolyGon. ConstructXMLString '+NodeName);
  XArray:= GetArrayFromString(XCoords);
  YArray:= GetArrayFromString(YCoords);
  str:=  '<polygon id="'+self.NameSpace+self.NodeName+'" points="';
  numpoints:= length(XArray);
  for i:=0 to numpoints-1 do
  begin
    str:=str +inttostr(XArray[i])+' '+inttostr(YArray[i])+' ';
  end;
  self.XMLString:=str+ '" transform="rotate('+rotate+','+inttostr(XArray[0])+','+inttostr(YArray[0])+')"'
                      +' stroke="'+ColorToStr(StrokeColor)+'" fill="'+FillColorToStr(FillColor,FillTransparent)+'"'
                      +' stroke-width="'+StrokeWidth+'" >'
          +'<title>'+Hint+'</title>'
          +'</polygon>';
end;

procedure TXSVGPolyGon.SetDefaultAttribs;
begin
  //showmessage('PolyGon. SetDefaultAttribs '+NodeName);
  self.myNode.SetAttributeValue('XCoords','50 ,  55 ,   70 , 60 ,  65 ,  50 ,  35  , 40 ,  30 ,  45 ');
  self.myNode.SetAttributeValue('YCoords','160 , 180 , 180 , 190 , 205 , 195 , 205 , 190 , 180,  180');
  self.myNode.SetAttributeValue('StrokeWidth','2');
  self.myNode.SetAttributeValue('StrokeColor','#FF0000','Color');
  self.myNode.SetAttributeValue('FillColor','#AAAAAA','Color');
  self.myNode.SetAttributeValue('FillTransparent','False','Boolean');
  self.myNode.SetAttributeValue('Rotate','0');
  ConstructXMLString;
end;

{$ifndef JScript}
function addSVGPolyGon(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
   var
     NewWidget:TXSVGPolyGon;
     NewNode:TDataNode;
  begin
     NewWidget:=TXSVGPolyGon.Create('SVG',ScreenObjectName,NameSpace,'TXSVGPolyGon',true);
     NewNode:=NewWidget.CommonSetup(NameSpace,ParentNode,position);
     NewWidget.SetDefaultAttribs;
     result:=NewNode;
  end;
{$else}

function CreateInObSVGPolyGon(MyForm:TForm;NodeName,NameSpace:String):TObject;
var
  NewObj:TXSVGPolyGon;
begin
  //showmessage('CreateInObSVGPolyGon '+NodeName);
  NewObj:=TXSVGPolyGon.Create('SVG',NodeName,NameSpace,'TXSVGPolyGon',true);
  NewObj.FinishSVGInObCreate;
  result:=NewObj;
end;

function CreateWidgetSVGPolyGon(myNode,ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
 var
   NewWidget:TXSVGPolyGon;
begin
//showmessage('CreateWidgetSVGPolyGon '+myNode.NodeName);
   NewWidget:=TXSVGPolyGon(myNode);

   NewWidget.XCoords:=NewWidget.XCoords;
   NewWidget.YCoords:=NewWidget.YCoords;
   NewWidget.StrokeWidth:=NewWidget.StrokeWidth;
   NewWidget.StrokeColor:=NewWidget.StrokeColor;
   NewWidget.FillColor:=NewWidget.FillColor;
   NewWidget.FillTransparent:=NewWidget.FillTransparent;
   NewWidget.Rotate:=NewWidget.Rotate;

   result:=NewWidget.SetupWidget(ParentNode,position);
end;
{$endif}



  function TXSVGWidget.getXMLString: String;
  begin
     result:=MyNode.getAttribute('XMLString',true).AttribValue;
  end;
  function TXSVGWidget.getXPos: String;
  begin
     result:=MyNode.getAttribute('XPos',true).AttribValue;
  end;
  function TXSVGWidget.getYPos: String;
  begin
     result:=MyNode.getAttribute('YPos',true).AttribValue;
  end;
  function TXSVGWidget.getWidth: String;
  begin
     result:=MyNode.getAttribute('Width',true).AttribValue;
  end;
  function TXSVGWidget.getHeight: String;
  begin
     result:=MyNode.getAttribute('Height',true).AttribValue;
  end;
  function TXSVGWidget.getStrokeWidth: String;
  begin
     result:=MyNode.getAttribute('StrokeWidth',true).AttribValue;
  end;
  function TXSVGWidget.getStrokeColor: TColor;
  begin
    {$ifndef JSCript}
    result:=HexRGBToColor(myNode.GetAttribute('StrokeColor',true).AttribValue);
    {$else}
    result:=MyNode.getAttribute('StrokeColor',true).AttribValue;
    {$endif}
  end;
  function TXSVGWidget.getFillColor: TColor;
  begin
     {$ifndef JSCript}
     result:=HexRGBToColor(myNode.GetAttribute('FillColor',true).AttribValue);
     {$else}
     result:=MyNode.getAttribute('FillColor',true).AttribValue;
     {$endif}
  end;
  function TXSVGWidget.getFillTransparent: Boolean;
  begin
     result:=MyStrToBool(MyNode.getAttribute('FillTransparent',true).AttribValue);
  end;
  function TXSVGWidget.getRotate: String;
  begin
     result:=MyNode.getAttribute('Rotate',true).AttribValue;
  end;
  function TXSVGWidget.getHint: String;
  begin
     result:=MyNode.getAttribute('Hint',true).AttribValue;
  end;


  procedure TXSVGWidget.setXMLString(AValue:string);
  var
    myParent:TXSVGContainer;
    pn:TDataNode;
  begin
    myNode.SetAttributeValue('XMLString',AValue);

    pn:=FindParentOfNode(SystemNodeTree,MyNode);
    if pn<>nil then
    begin
      {$ifndef JScript}
      myParent:=TXSVGContainer(pn.ScreenObject);
      {$else}
      // if (not StartingUp) then showmessage('widget '+myNode.NodeType+' setXMLString '+AValue);
      myParent:=TXSVGContainer(pn);
      {$endif}
      myParent.XMLString:=myParent.XMLString;  // will re-draw the parent SVG frame

    end;

  end;


  procedure TXSVGWidget.setXPos(AValue:string);
  begin
    myNode.SetAttributeValue('XPos',AValue);
    self.ConstructXMLString;
  end;
  procedure TXSVGWidget.setYPos(AValue:string);
  begin
    myNode.SetAttributeValue('YPos',AValue);
    self.ConstructXMLString;
  end;
  procedure TXSVGWidget.setWidth(AValue:string);
  begin
    myNode.SetAttributeValue('Width',AValue);
    self.ConstructXMLString;
  end;
  procedure TXSVGWidget.setHeight(AValue:string);
  begin
    myNode.SetAttributeValue('Height',AValue);
    self.ConstructXMLString;
  end;
  procedure TXSVGWidget.setStrokeWidth(AValue:string);
  begin
    myNode.SetAttributeValue('StrokeWidth',AValue);
    self.ConstructXMLString;
  end;
  procedure TXSVGWidget.setStrokeColor(AValue:TColor);
  var str:string;
  begin
    if myNode<>nil then
    begin
      {$ifndef JScript}
      str:= ColorToHexRGB(AValue);
      myNode.SetAttributeValue('StrokeColor',str,'Color');
      {$else}
      myNode.SetAttributeValue('StrokeColor',AValue,'Color');
      {$endif}
      self.ConstructXMLString;
    end;
  end;
  procedure TXSVGWidget.setFillColor(AValue:TColor);
  var str:string;
  begin
    if myNode<>nil then
    begin
      {$ifndef JScript}
      str:= ColorToHexRGB(AValue);
      myNode.SetAttributeValue('FillColor',str,'Color');
      {$else}
      //if myNode.NodeType='TXSVGRoundedRect' then showmessage('SetFillColor '+AValue);
      myNode.SetAttributeValue('FillColor',AValue,'Color');
      {$endif}
      self.ConstructXMLString;
    end;
  end;
  procedure TXSVGWidget.setFillTransparent(AValue:Boolean);
  begin
    if myNode<>nil then
    begin
      myNode.SetAttributeValue('FillTransparent',myBoolToStr(AValue),'Boolean');
      self.ConstructXMLString;
    end;
  end;
  procedure TXSVGWidget.setRotate(AValue:string);
  begin
    myNode.SetAttributeValue('Rotate',AValue);
    self.ConstructXMLString;
  end;
  procedure TXSVGWidget.setHint(AValue:string);
  begin
    myNode.SetAttributeValue('Hint',AValue);
    self.ConstructXMLString;
  end;


function TXSVGText.getTextString: String;
begin
   result:=MyNode.getAttribute('TextString',true).AttribValue;
end;
function TXSVGText.getFontFamily: String;
begin
   result:=MyNode.getAttribute('FontFamily',true).AttribValue;
end;
function TXSVGText.getFontWeight: String;
begin
   result:=MyNode.getAttribute('FontWeight',true).AttribValue;
end;
function TXSVGText.getFontStyle: String;
begin
   result:=MyNode.getAttribute('FontStyle',true).AttribValue;
end;

procedure TXSVGText.setTextString(AValue:string);
begin
  myNode.SetAttributeValue('TextString',AValue);
  self.ConstructXMLString;
end;
procedure TXSVGText.setFontFamily(AValue:string);
begin
  myNode.SetAttributeValue('FontFamily',AValue);
  self.ConstructXMLString;
end;
procedure TXSVGText.setFontWeight(AValue:string);
begin
  myNode.SetAttributeValue('FontWeight',AValue);
  self.ConstructXMLString;
end;
procedure TXSVGText.setFontStyle(AValue:string);
begin
  myNode.SetAttributeValue('FontStyle',AValue);
  self.ConstructXMLString;
end;


function TXSVGRoundedRect.getrx: String;
begin
   result:=MyNode.getAttribute('Rx',true).AttribValue;
end;
function TXSVGRoundedRect.getry: String;
begin
   result:=MyNode.getAttribute('Ry',true).AttribValue;
end;
procedure TXSVGRoundedRect.setrx(AValue:string);
begin
  myNode.SetAttributeValue('Rx',AValue);
  self.ConstructXMLString;
end;
procedure TXSVGRoundedRect.setry(AValue:string);
begin
  myNode.SetAttributeValue('Ry',AValue);
  self.ConstructXMLString;
end;

function TXSVGCircle.getRadius: String;
begin
   result:=MyNode.getAttribute('Radius',true).AttribValue;
end;
procedure TXSVGCircle.setRadius(AValue:string);
begin
  myNode.SetAttributeValue('Radius',AValue);
  self.ConstructXMLString;
end;

function TXSVGEllipse.getrx: String;
begin
   result:=MyNode.getAttribute('Rx',true).AttribValue;
end;
function TXSVGEllipse.getry: String;
begin
   result:=MyNode.getAttribute('Ry',true).AttribValue;
end;
procedure TXSVGEllipse.setrx(AValue:string);
begin
  myNode.SetAttributeValue('Rx',AValue);
  self.ConstructXMLString;
end;
procedure TXSVGEllipse.setry(AValue:string);
begin
  myNode.SetAttributeValue('Ry',AValue);
  self.ConstructXMLString;
end;

function TXSVGLine.getX1: String;
begin
   result:=MyNode.getAttribute('X1',true).AttribValue;
end;
function TXSVGLine.getY1: String;
begin
   result:=MyNode.getAttribute('Y1',true).AttribValue;
end;
function TXSVGLine.getX2: String;
begin
   result:=MyNode.getAttribute('X2',true).AttribValue;
end;
function TXSVGLine.getY2: String;
begin
   result:=MyNode.getAttribute('Y2',true).AttribValue;
end;
procedure TXSVGLine.setX1(AValue:string);
begin
  myNode.SetAttributeValue('X1',AValue);
  self.ConstructXMLString;
end;
procedure TXSVGLine.setY1(AValue:string);
begin
  myNode.SetAttributeValue('Y1',AValue);
  self.ConstructXMLString;
end;
procedure TXSVGLine.setX2(AValue:string);
begin
  myNode.SetAttributeValue('X2',AValue);
  self.ConstructXMLString;
end;
procedure TXSVGLine.setY2(AValue:string);
begin
  myNode.SetAttributeValue('Y2',AValue);
  self.ConstructXMLString;
end;

function TXSVGPolyLine.getXCoords: String;
begin
   result:=MyNode.getAttribute('XCoords',true).AttribValue;
end;
function TXSVGPolyLine.getYCoords: String;
begin
   result:=MyNode.getAttribute('YCoords',true).AttribValue;
end;
procedure TXSVGPolyLine.setXCoords(AValue:string);
begin
  myNode.SetAttributeValue('XCoords',AValue);
  self.ConstructXMLString;
end;
procedure TXSVGPolyLine.setYCoords(AValue:string);
begin
  myNode.SetAttributeValue('YCoords',AValue);
  self.ConstructXMLString;
end;

begin
  AddWrapperDefaultAttribs(SVGDefaultAttribs);
  AddDefaultAttribute(SVGDefaultAttribs,'SuspendRefresh','Boolean','False','',false);
  AddDefaultAttribute(SVGDefaultAttribs,'ActualHeight','Integer','','',true,false);
  AddDefaultAttribute(SVGDefaultAttribs,'ActualWidth','Integer','','',true,false);
  AddDefaultAttribute(SVGDefaultAttribs,'FrameWidth','String','300','',false);
  AddDefaultAttribute(SVGDefaultAttribs,'FrameHeight','String','300','',false);
  AddDefaultAttribute(SVGDefaultAttribs,'Border','Boolean','True','',false);
  AddDefaultAttribute(SVGDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(SVGDefaultAttribs,'LabelPos','String','Top','',false);
  AddDefaultAttribute(SVGDefaultAttribs,'LabelText','String','SVG Frame','',false);
  AddDefaultAttribute(SVGDefaultAttribs,'BgColor','Color','#FFFFFF','',false);
  AddDefaultAttribute(SVGDefaultAttribs,'HTMLSource','String','','',false,false);
  AddDefaultsToTable('TXSVGContainer',SVGDefaultAttribs);

  AddAttribOptions('TXSVGContainer','Alignment',AlignmentOptions);
  AddAttribOptions('TXSVGContainer','LabelPos',LabelPosOptions);

  {$IFndef JScript}
  Classes.RegisterClass(TXSVGContainer);
  Classes.RegisterClass(TXSVGWidget);
  Classes.RegisterClass(TXSVGText);
  Classes.RegisterClass(TXSVGRect);
  Classes.RegisterClass(TXSVGRoundedRect);
  Classes.RegisterClass(TXSVGCircle);
  Classes.RegisterClass(TXSVGEllipse);
  Classes.RegisterClass(TXSVGLine);
  Classes.RegisterClass(TXSVGPolyLine);
  Classes.RegisterClass(TXSVGPolyGon);
  AddNodeFuncLookup('TXSVGContainer',@CreateSVGContainerWidget);
  AddNodeFuncLookup('TXSVGText',@addSVGText);
  AddNodeFuncLookup('TXSVGRect',@addSVGRect);
  AddNodeFuncLookup('TXSVGRoundedRect',@addSVGRoundedRect);
  AddNodeFuncLookup('TXSVGCircle',@addSVGCircle);
  AddNodeFuncLookup('TXSVGEllipse',@addSVGEllipse);
  AddNodeFuncLookup('TXSVGLine',@addSVGLine);
  AddNodeFuncLookup('TXSVGPolyLine',@addSVGPolyLine);
  AddNodeFuncLookup('TXSVGPolyGon',@addSVGPolyGon);

  {$else}
  AddNodeFuncLookup('TXSVGContainer',@CreateInterfaceObjSVG,@CreateSVGContainerWidget);
  AddNodeFuncLookup('TXSVGText',@CreateInObSVGText,@CreateWidgetSVGText);
  AddNodeFuncLookup('TXSVGRect',@CreateInObSVGRect,@CreateWidgetSVGRect);
  AddNodeFuncLookup('TXSVGRoundedRect',@CreateInObSVGRoundedRect,@CreateWidgetSVGRoundedRect);
  AddNodeFuncLookup('TXSVGCircle',@CreateInObSVGCircle,@CreateWidgetSVGCircle);
  AddNodeFuncLookup('TXSVGEllipse',@CreateInObSVGEllipse,@CreateWidgetSVGEllipse);
  AddNodeFuncLookup('TXSVGLine',@CreateInObSVGLine,@CreateWidgetSVGLine);
  AddNodeFuncLookup('TXSVGPolyLine',@CreateInObSVGPolyLine,@CreateWidgetSVGPolyLine);
  AddNodeFuncLookup('TXSVGPolyGon',@CreateInObSVGPolyGon,@CreateWidgetSVGPolyGon);

  {$endif}

  SuppressDesignerProperty('TXSVGContainer','ContainerHeight');
  SuppressDesignerProperty('TXSVGContainer','ContainerWidth');
  SuppressDesignerProperty('TXSVGContainer','BgColor');
//  SuppressDesignerProperty('TXSVGContainer','HTMLSource');

  SuppressDesignerProperty('TXSVGText','Width');

  SuppressDesignerProperty('TXSVGCircle','Width');
  SuppressDesignerProperty('TXSVGCircle','Height');

  SuppressDesignerProperty('TXSVGEllipse','Width');
  SuppressDesignerProperty('TXSVGEllipse','Height');

  SuppressDesignerProperty('TXSVGLine','XPos');
  SuppressDesignerProperty('TXSVGLine','YPos');
  SuppressDesignerProperty('TXSVGLine','Width');
  SuppressDesignerProperty('TXSVGLine','Height');
  SuppressDesignerProperty('TXSVGLine','FillColor');

  SuppressDesignerProperty('TXSVGPolyLine','XPos');
  SuppressDesignerProperty('TXSVGPolyLine','YPos');
  SuppressDesignerProperty('TXSVGPolyLine','Width');
  SuppressDesignerProperty('TXSVGPolyLine','Height');
  SuppressDesignerProperty('TXSVGPolyLine','FillColor');

end.

