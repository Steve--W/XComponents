(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XBitMap;

(*  EXAMPLE.......

/* XPM */
static char * XFACE[] = {
/* <Values> */
/* <width/columns> <height/rows> <colors> <chars per pixel>*/
"48 4 2 1",
/* <Colors> */
"a c #ffffff",
"b c #000000",
/* <Pixels> */
"abaabaababaaabaabababaabaabaababaabaaababaabaaab",
"abaabaababaaabaabababaabaabaababaabaaababaabaaab",
"abaabaababaaabaabababaabaabaababaabaaababaabaaab",
"abaabaababaaabaabababaabaabaababaabaaababaabaaab"
};
*)

{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, TypInfo, NodeUtils, StringUtils,
  {$ifndef JScript}
  LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, Propedits,RTTICtrls,
  LazsUtils, Events, LCLType, LCLIntf,
  {$else}
  HTMLUtils,
  {$endif}
  WrapperPanel;

type
TColorLookup = class(TObject)
  public
  PixelType:String;
  r:integer;
  g:integer;
  b:integer;
  a:integer;
end;

TXBitMapColors=Array of TColorLookup;

TXBitMap = class(TWrapperPanel)
private
  { Private declarations }
  {$ifndef JScript}
  fHandleClick:TEventHandler;
  fTheBitMap:TPixMap;
  {$endif}
  fMapPixelArray:TStringArray;
  fColorsArray:TStringList;
  fColorsLookup:TXBitMapColors;
  fBitMapWidth:integer;
  fBitMapHeight:integer;
  fCharsPerColor:integer;
  fNumColors:integer;

  {$ifndef JScript}
  procedure ImageClick(Sender:TObject);
  {$endif}

  procedure SetMyEventTypes;

  function GetMapData:string;
  function GetMapColors:string;
  function GetImageWidth:string;
  function GetImageHeight:string;
  function GetMapPixelArray:TstringArray;
  function GetActualHeight:integer;
  function GetActualWidth:integer;

  procedure SetMapData(AValue:string);
  procedure SetMapColors(AValue:string);
  procedure SetImageWidth(AValue:string);
  procedure SetImageHeight(AValue:string);
  procedure SetMapPixelArray(AValue:TstringArray);

  procedure RebuildXPMDataString;
  function LookupColor(ColorChar:String):TColorLookup;

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
  function GetMapPixelArraySection(x,y,w,h:integer):TstringArray;
  procedure SetMapPixelArraySection(AValue:TstringArray;x,y:integer);
  property TheBitMap:TPixMap read fTheBitMap write fTheBitMap;
  {$else}
  constructor Create(MyForm:TForm;NodeName,NameSpace:String);
  function GetMapPixelArraySection(x,y,w,h:integer):TstringArray;  overload;
  procedure SetMapPixelArraySection(AValue:TstringArray;x,y:integer); overload;
  Procedure PaintRect( sx, sy, sWidth, sHeight, ImageWidth, ImageHeight:integer);
  {$endif}
  property ColorsArray:TStringList read fColorsArray write fColorsArray;
  property ColorsLookup:TXBitMapColors read fColorsLookup write fColorsLookup;
  property BitMapWidth:integer read fBitMapWidth write fBitMapWidth;
  property BitMapHeight:integer read fBitMapHeight write fBitMapHeight;
  property NumColors:integer read fNumColors write fNumColors;
  property CharsPerColor:integer read fCharsPerColor write fCharsPerColor;

published
  { Published declarations }

  // Properties defined for this class...
  property MapData: String read GetMapData write SetMapData;
  property MapColors: String read GetMapColors write SetMapColors;
  property ImageHeight: String read GetImageHeight write SetImageHeight;
  property ImageWidth: String read GetImageWidth write SetImageWidth;
  property MapPixelArray:TStringArray read GetMapPixelArray write SetMapPixelArray;
  property ActualHeight:integer read GetActualHeight;
  property ActualWidth:integer read GetActualWidth;

  {$ifndef JScript}
  // Events to be visible in Lazarus IDE
  property HandleClick: TEventHandler read FHandleClick write FHandleClick;
  {$endif}
end;


{$ifndef JScript}
procedure Register;
{$endif}

implementation

const MyNodeType='TXBitMap';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXBitMap.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
end;


{$ifndef JScript}
procedure Register;
begin
  {$I Icons/XBitmap.lrs}
  RegisterComponents('XComponents',[TXBitMap]);

  // inherited from TWrapperPanel, not required here
  RegisterPropertyEditor(TypeInfo(TColor), TXBitMap, 'BgColor', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(TXPropertyLink), TXBitMap, 'Link', THiddenPropertyEditor);
end;

constructor TXBitMap.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXBitMap.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXBitMap.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
var
myxpmArray:TStringList;
//mystream:TMemoryStream;
begin
  self.BorderSpacing.Around:=glbBorderSpacing;

  myControl:=TImage.Create(self);
  myControl.Parent:=self;

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
//  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];

  myControl.OnClick:=@self.ImageClick;


  self.TheBitMap:=TPixMap.Create;
  fColorsArray:=TStringList.Create;
  fColorsArray.StrictDelimiter:=true;
  fColorsArray.LineBreak:='",';
  SetLength(fMapPixelArray,0);
  SetLength(fColorsLookup,0);

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
  NewNode:=CreateDynamicLazWidget('TXBitMap',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;

procedure TXBitMap.ImageClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;

procedure TXBitMap.SetImageWidth(AValue:string);
    var
      tc:TControl;
    begin
      tc:=self.myControl;
  myNode.SetAttributeValue('ImageWidth',AValue);
  SetHeightWidth(self.myNode,tc,'ImageWidth','ImageHeight');
end;

procedure TXBitMap.SetImageHeight(AValue:string);
    var
      tc:TControl;
    begin
      tc:=self.myControl;
  myNode.SetAttributeValue('ImageHeight',AValue);
  SetHeightWidth(self.myNode,tc,'ImageWidth','ImageHeight');
end;

{$else}
constructor TXBitMap.Create(MyForm:TForm;NodeName,NameSpace:String);
begin
  inherited Create(NodeName,NameSpace);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=false;
  fColorsArray:=TStringList.Create;
  fColorsArray.StrictDelimiter:=true;
  fColorsArray.LineBreak:='",';
  SetLength(fMapPixelArray,0);
  SetLength(fColorsLookup,0);

  SetNodePropDefaults(self,myDefaultAttribs);

end;


function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  LabelText:string;
  OnClickString:String;
  marginString:string;
begin
  LabelText:= MyNode.getAttribute('LabelText',true).AttribValue;

  marginString := 'margin:'+glbMarginSpacing+' '
                           +glbMarginSpacing+' '
                           +glbMarginSpacing+' '
                           +glbMarginSpacing+';';

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''', '''+NameSpace+''','''');" ';

  asm
    try{
    var wrapperid =  NameSpace+ScreenObjectName;
    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,NameSpace,$impl.MyNodeType,position);
    var MyObjectName=wrapperid+'Contents';
    var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'">'+LabelText+'</label>';

    var ImageString = ' <canvas  id='+MyObjectName+ ' style="display: inline-block; width:100%; height:100%;" ' +
                         OnClickString +
                         ' >';

    var HTMLString = labelstring+ImageString;

    var wrapper=document.getElementById(wrapperid);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);
    }
    catch(err) { alert(err.message+'  in XBitMap.CreateWidget');}

  end;

  MyNode.ScreenObject:=MyNode;

  RefreshComponentProps(myNode);

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXBitMap.Create(MyForm,NodeName,NameSpace));
end;

Procedure TXBitMap.PaintRect( sx, sy, sWidth, sHeight, ImageWidth, ImageHeight:integer);
var
  NumColours:integer;
begin
  NumColours:=length(self.fColorsLookup);
   asm
   try {
   //alert('PaintRect. '+this.NodeName+' Imagewidth='+ImageWidth+' Imageheight='+ImageHeight+' sWidth='+sWidth+' sHeight='+sHeight);
	var RawImagecanvas  = document.getElementById(this.NameSpace+this.NodeName+'Contents');
        if (RawImagecanvas!=null) {
	  var RawImagecontext = RawImagecanvas.getContext("2d");
	  var RawImageData = RawImagecontext.createImageData(ImageWidth, ImageHeight);
 //         RawImagecontext.setTransform(1, 0, 0, 1, 0, 0);       // clear any previous scaling
          //alert('ImageData length='+RawImageData.data.length);

          var pixelchars=this.fColorsLookup[0].PixelType.length;      // number or characters in the color strings
          console.log('pixelchars='+pixelchars);

          function SetRawPixel(x,y,r,g,b,a)
	  {
            //console.log('SetRawPixel '+x+','+y+','+r+','+g+','+b+','+a);
            var pixelIndex =  4 * (x + y * ImageWidth);
            //console.log('SetRawPixel '+x+','+y+' index='+pixelIndex);
	    RawImageData.data[pixelIndex    ] = r;  //0..255 red   color
	    RawImageData.data[pixelIndex + 1] = g;  //0..255 green color
	    RawImageData.data[pixelIndex + 2] = b;  //0..255 blue  color
	    RawImageData.data[pixelIndex + 3] = a;  //0..255 transparency
	  }

	  function PaintRawImage(sx, sy, sWidth, sHeight) //Copy a rectangle from the Raw Pixel Data
	  {
	    // Canvas X and Y are sx, sy, because we are dealing with a custom component with coincedent origins
	    // context.putImageData(RawImageData, canvasX, canvasY, sx, sy, sWidth, sHeight);
	    // we assume that sx,and sy etc deal with the row indexing and times 4 aspects internally
	    RawImagecontext.putImageData(RawImageData, sx, sy, sx, sy, sWidth, sHeight);

            //alert('scale '+(ImageWidth/(sWidth/pixelchars))+','+(ImageHeight/sHeight));
            // scale the rendered image (scale the context, then re-draw the image
            RawImagecontext.webkitImageSmoothingEnabled = false;
            RawImagecontext.mozImageSmoothingEnabled = false;
            RawImagecontext.imageSmoothingEnabled = false;
            RawImagecontext.setTransform(1, 0, 0, 1, 0, 0);       // clear any previous scaling
            RawImagecontext.scale((ImageWidth/(sWidth/pixelchars)),(ImageHeight/sHeight));
            RawImagecontext.drawImage(RawImagecanvas, 0, 0);
	  }

	  function LookupColour(ob,instring)
	  {
            //console.log('lookup '+instring);
  	    var r = 0;
  	    var g = 0;
  	    var b = 0;
  	    var a = 0; // if a lookupchar is not found the pixel is transparent
	    for (var i = 0; i < NumColours; i++)
   	    {
		  if (instring == ob.fColorsLookup[i].PixelType)
		    {
		        r = ob.fColorsLookup[i].r;
		        g = ob.fColorsLookup[i].g;
		        b = ob.fColorsLookup[i].b;
		        a = 255;
                        //console.log('found color '+instring+' = '+r+' '+g+' '+b+' '+a);
                        return [r,g,b,a];
		    }
	    }
             return [r,g,b,a];
	  }
          //console.log('NumColours='+NumColours);
          //console.log('sy='+sy+' sHeight='+sHeight);
	  for (var j = 0; j < sHeight; j++)
	  {
                  var p=0;
		  for (var i = 0; i < sWidth; i+=pixelchars)
		  {
                    var pix='';
                    for (var k=0; k<pixelchars; k++) {pix=pix+this.fMapPixelArray[sy+j][sx+i+k];}
                    var clr=LookupColour(this,pix);
                    //console.log('SetRawPixel('+sx+p+','+sy+j+','+clr[0]+','+clr[1]+','+clr[2]+','+clr[3]+')');
                    if (clr[3]==0) {console.log('pixel ('+pix+') not found '+sx+p+','+sy+j);  }

                    SetRawPixel(sx+p,sy+j,clr[0],clr[1],clr[2],clr[3]);
                    p=p+1;
		  }
	    }
            PaintRawImage(sx,sy,sWidth, sHeight);
          }
      } catch(err) { alert(err.message+'  in XBitMap.PaintRect');}
   end;
end;


procedure TXBitMap.SetImageWidth(AValue:string);
begin
  myNode.SetAttributeValue('ImageWidth',AValue);
  asm
  var ob = document.getElementById(this.NameSpace+this.NodeName);
  if (ob!=null) {
    pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
    //for correct scaling, have to set width the same as CSS width
    var obc = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    obc.setAttribute('width',ob.style.width);
  }
  end;
end;

procedure TXBitMap.SetImageHeight(AValue:string);
begin
  myNode.SetAttributeValue('ImageHeight',AValue);
  asm
  var ob = document.getElementById(this.NameSpace+this.NodeName);
  if (ob!=null) {
    pas.HTMLUtils.SetHeightWidthHTML(this,ob,'H',AValue);
    //for correct scaling, have to set height the same as CSS height
    var obc = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    obc.setAttribute('height',ob.style.height);
  }
  end;
end;
{$endif}

function TXBitMap.GetMapData:String;
begin
  // because there may have been indexed setting of the pixel array, which will not have updated
  // the node attribute value, first rebuild the full data set here from widget property values.
  RebuildXPMDataString;

  result:=MyNode.getAttribute('MapData',true).AttribValue;
end;
function TXBitMap.GetMapColors:String;
begin
  result:=MyNode.getAttribute('MapColors',true).AttribValue;
end;
function TXBitMap.GetImageHeight:string;
begin
  result:=MyNode.getAttribute('ImageHeight',true).AttribValue;
end;
function TXBitMap.GetImageWidth:string;
begin
  result:=MyNode.getAttribute('ImageWidth',true).AttribValue;
end;

function TXBitMap.GetMapPixelArray:TstringArray;
begin
  result:=fMapPixelArray;
end;

function TXBitMap.GetMapPixelArraySection(x,y,w,h:integer):TstringArray;
var
  i,j,r:integer;
  arr:TStringArray;
  rowstr:String;
begin
  r:=-1;
  if (length(fMapPixelArray)>0)
  and (length(fMapPixelArray[0])>0) then
    if (y+h) < length(fMapPixelArray) then
      for i:=y to y+h-1 do
      begin
        setlength(arr,i+1);
        r:=r+1;
        rowstr:='';
        if (x+1+w) < length(fMapPixelArray[i]) then
          for j:=(x+1) to (x+1)+w-1 do
          begin
            rowstr:=rowstr+fMapPixelArray[i][j];
          end;
        arr[r]:=rowstr;
      end;
  result:=arr;
end;
procedure TXBitMap.SetMapPixelArraySection(AValue:TstringArray;x,y:integer);
// both x and y are zero-based indices
  var
    i,j:integer;
begin
  if (length(fMapPixelArray)>0)
  and (length(AValue)>0)
  and ((y+length(AValue))<length(fMapPixelArray))
  and ((x+length(AValue[0]))<length(fMapPixelArray[0])) then
  begin
    for i:=0 to length(AValue)-1 do
      for j:=1 to length(AValue[0]) do
        fMapPixelArray[(y+i)][(x+j)]:=AValue[i][j];
  end;

end;

function TXBitMap.GetActualHeight:integer;
// NB. this is a read-only attribute (no setter)
var
  h:integer;
begin
  {$ifndef JScript}
  h:=TImage(self.myControl).Height;
  {$else}
  h:=GetCurrentHeight(self.NodeName);
  {$endif}
  myNode.SetAttributeValue('ActualHeight',inttostr(h),'Integer',true);     // just so the attribute exists
  result:=h;
end;
function TXBitMap.GetActualWidth:integer;
// NB. this is a read-only attribute (no setter)
var
  w:integer;
begin
  {$ifndef JScript}
  w:=TImage(self.myControl).Width;
  {$else}
  w:=GetCurrentWidth(self.NodeName);
  {$endif}
  myNode.SetAttributeValue('ActualWidth',inttostr(w),'Integer',true);     // just so the attribute exists
  result:=w;
end;

function TXBitMap.LookupColor(ColorChar:String):TColorLookup;
var
  i:integer;
begin
  result:=nil;
  i:=0;
  while i<length(fColorsLookup) do
  begin
    if fColorsLookup[i].PixelType=ColorChar then
    begin
      result:=fColorsLookup[i];
      i:=length(fColorsLookup);
    end;
    i:=i+1;
  end;
end;

function StripComments(instr:String):String;
var
  workstr,tmp:String;
  x1,x2:integer;
begin
  workstr:=instr;
  if length(instr) > 3 then
    while pos('/*',workstr)>0 do
    begin
      x1:=pos('/*',workstr);
      x2:=pos('*/',workstr);
      if x2<=x1 then
        x2:=x1+2;
      {$ifndef JScript}
      workstr:=workstr.Remove(x1-1,(x2-x1)+2);
      {$else}
      //showmessage('StripComments x1='+inttostr(x1)+' x2='+inttostr(x2)+' workstr=>>'+workstr+'<<');
      tmp:=LeftStr(workstr, x1-1);
      workstr:=tmp + RightStr(workstr, length(workstr)-x2-1);
      //showmessage('workstr=>>'+workstr+'<<');
      {$endif}
    end;
  result:=workstr;
end;

procedure TXBitMap.SetMapPixelArray(AValue:TstringArray);
var
  bits:TStringList;
  i:integer;
begin
  fMapPixelArray:=AValue;
  bits:=TStringList.Create;
  bits.StrictDelimiter:=true;
  bits.LineBreak:=',';
  for i:=0 to length(AValue)-1 do
    bits.Add(AValue[i]);

  myNode.SetAttributeValue('MapPixelArray',bits.Text,'StringArray',true);

  bits.Free;
end;

procedure TXBitMap.SetMapData(AValue:string);
// Set the whole XPM file.  Use it to reset the MapColors array.
var
  myxpmArray, bits, bits0:TStringList;
  i,c,d,p,x,cpos:integer;
  oldData,newData,tmp,tmp1:String;
  colorItem:TColorLookup;
  ok:boolean;
  {$ifndef JScript}
  mystream:TMemoryStream;
  AColor:TColor;
  DestRect:TRect;
  {$else}
  rtmp,gtmp,btmp,atmp,h,w,sh,sw:integer;
  {$endif}
  lengthOfColorLine:integer;
  lengthOfPixelsLine:integer;
begin
 // {$ifdef JScript}showmessage('SetMapData 1');{$endif}
 // showmessage('SetMapData '+AValue);
  ok:=true;
  myxpmArray:=TStringList.Create;
  myxpmarray.StrictDelimiter:=true;
  myxpmarray.LineBreak:='",';                // nb. data may contain commas
  newData:=AValue;
  newData:=myStringReplace(newData,LineEnding,'',-1,-1);
  newData:=myStringReplace(newData,chr(10),'',-1,-1);

  myxpmArray.Text:=newData;

  //showmessage('myxpmArray initial count='+inttostr(myxpmArray.Count));

  {$ifndef JScript}
  if myControl<>nil then
  begin
     oldData:=MyNode.getAttribute('MapData',true).AttribValue;
     if (AValue<>'')
     and (AValue<>oldData) then
     begin

       mystream:=TMemoryStream.Create;
       myxpmArray.SaveToStream(mystream);
       mystream.Position:= 0;

       if TheBitMap.IsStreamFormatSupported(mystream) then
       begin
         try
           TheBitMap.Clear;
           TheBitMap.LoadFromStream(mystream);
           TImage(myControl).Picture.Bitmap.Clear;
           TImage(myControl).Picture.Bitmap.SetSize(TImage(self.myControl).Width,TImage(self.myControl).Height);

           TImage(myControl).Canvas.Clear;
           DestRect:=TImage(myControl).ClientRect;
           TImage(myControl).Canvas.StretchDraw(DestRect,TheBitMap);      // make the image fit the container size

           myNode.SetAttributeValue('MapData',AValue);

         except
           on e:exception do
           begin
             showmessage('SetMapData - unable to load XPM data');
             ok:=false;
           end;
         end;
       end
       else
       begin
         showmessage('SetMapData - invalid XPM formatting');
         ok:=false;
       end;

       mystream.Free;

     end;
  end;
  {$else}
  //see below, after setting pixelarray
  {$endif}

  if ok then
  begin
    //{$ifdef JScript}showmessage('SetMapData 2');{$endif}
    bits0:=tStringList.Create;
    bits0.StrictDelimiter:=true;
    bits0.LineBreak:='"';
    bits:=tStringList.Create;
    bits.StrictDelimiter:=true;
    bits.LineBreak:=' ';
    // find the initial definition line (first line that starts with ")
    i:=0;
    while i < myxpmarray.Count do
    begin
      tmp:=myxpmarray[i];
      tmp:=StripComments(myxpmarray[i]);
      x:=pos('{',tmp);
      if x>0 then
      {$ifndef JScript}
        tmp:=tmp.Remove(0,x);
      {$else}
        tmp:=RightStr(tmp, length(tmp)-x);
      {$endif}
      if tmp[1]='"' then
      begin
        bits0.Text:=tmp;    //split on "
        tmp:=bits0[1];
        bits.Text:=tmp;    // split on spaces
        // should have 4 bits (width, height, colors, pixchars)...
        if bits.Count=4 then
        begin
          BitMapWidth:=StrToInt(bits[0]);
          BitMapHeight:=StrToInt(bits[1]);
          NumColors:=StrToInt(bits[2]);
          CharsPerColor:=StrToInt(bits[3]);
          LengthOfColorLine:=1 + CharsPerColor + 3 + 9;
          lengthOfPixelsLine:=1 + (BitMapWidth * CharsPerColor);
         end
        else
        begin
          showmessage('SetMapData - invalid XPM formatting');
          ok:=false;
        end;
        d:=i;
        i:=myxpmarray.Count;
      end;
      i:=i+1;
    end;

    //{$ifdef JScript}showmessage('SetMapData 3');{$endif}
    fColorsArray.Clear;
    SetLength(fColorsLookup,0);
    // find the colors defined in the data
    // Put the color definitions into a StringList property
    bits.LineBreak:=#10;
    i:=0;
    c:=0;
    while i < myxpmarray.Count do
    begin
      tmp:=myxpmarray[i];
      bits0.Text:=myxpmarray[i];     // split on double quotes
      if bits0.Count>1 then
      begin
        tmp:=bits0[1];

        // find the first 'c' after a space (cpos)
        // pixeltype will be the first n characters, where n is cpos-2.
        // (pixeltype may include space characters)
        cpos:=0;
        p:=2;
        while p<length(tmp)-2 do
        begin
          if (tmp[p]+tmp[p+1]+tmp[p+2]) = ' c ' then
          begin
            cpos:=p;
            tmp[p]:=#10;
            tmp[p+2]:=#10;
            bits.Text:=tmp;
            p:=length(tmp);
          end;
          p:=p+1;
        end;

        if (bits.count=3) and (bits[1]='c') then
        begin
          //fColorsArray.add('"'+tmp+'"');
          tmp:='"'+bits[0]+' '+bits[1]+' '+bits[2];
          fColorsArray.add('"'+bits[0]+' '+bits[1]+' '+bits[2]);
          c:=i;
          colorItem:=TColorLookup.Create;
          colorItem.PixelType:=bits[0];
          if bits[2]='none' then
          begin
            {$ifndef JScript}
            AColor:=clWhite;
            {$endif}
            colorItem.r := 255;
            colorItem.g := 255;
            colorItem.b := 255;
            colorItem.a:=0;
          end
          else
          begin
            {$ifndef JScript}
            AColor:=HexRGBtoColor(bits[2]);
            colorItem.r := GetRValue(AColor);
            colorItem.g := GetGValue(AColor);
            colorItem.b := GetBValue(AColor);
            colorItem.a:=255;
            {$else}
            HexRGBToColor(bits[2],rtmp,gtmp,btmp,atmp);
            colorItem.r := rtmp;
            colorItem.g := gtmp;
            colorItem.b := btmp;
            colorItem.a:=255;
            //tmp1:=bits[2];
            //asm
            //console.log(tmp+'  color '+tmp1+' to rgb '+rtmp+','+gtmp+','+btmp);
            //end;
            {$endif}
          end;
          SetLength(fColorsLookup,length(fColorsLookup)+1);
          //showmessage('fColorsLookup['+inttostr(length(fColorsLookup)-1)+'] = '+inttostr(colorItem.r)+' '+inttostr(colorItem.g)+' '+inttostr(colorItem.b));
          fColorsLookup[length(fColorsLookup)-1]:=colorItem;
          bits.Clear;
        end
        else
        begin
          if fColorsArray.Count>0 then
          begin
            //showmessage('colour collection stopped at line '+inttostr(i)+' '+myxpmarray[i]);
            i:=myxpmarray.Count;  // colours done - stop
          end;
        end;
      end
      else
      begin
        //showmessage('skip line '+inttostr(i)+' '+myxpmArray[i]);
        //{$ifdef JScript}tmp:=myxpmArray[i]; asm console.log('skip line '+i+' >>'+tmp+'<<'); end;{$endif}
        if length(myxpmarray[i])<LengthOfColorLine then
          // add this to the next one...
          myxpmarray[i+1]:=myxpmarray[i]+'",'+myxpmarray[i+1];
        tmp:=myxpmarray[i];
        tmp:=myxpmarray[i+1];
      end;
      i:=i+1;
    end;
    if fColorsArray.Count<>NumColors then
      showmessage('Number or Colours mismatch (NumColors='+inttostr(NumColors)+' count='+inttostr(fColorsArray.Count));

    myNode.SetAttributeValue('MapColors',fColorsArray.Text);

    //{$ifdef JScript}showmessage('SetMapData 4.  myxpmarray.Count='+inttostr(myxpmarray.Count));{$endif}

    // Put the Pixels map into a StringList property
    SetLength(fMapPixelArray,0);

    i:=myxpmArray.count;
    {$ifdef JScript} asm console.log('myxpmarray.Count='+i); end; {$endif}  //529

    i:=c+1;
    while i < myxpmarray.Count do
    begin
      tmp:=myxpmarray[i];
      if StripComments(myxpmarray[i])[1]='"' then
      begin
        bits0.Text:=myxpmarray[i];
        tmp:=bits0[1];
        //{$ifdef JScript} asm console.log(tmp); end; {$endif}  //529
        //showmessage(tmp);
        SetLength(fMapPixelArray,length(fMapPixelArray)+1);
        fMapPixelArray[length(fMapPixelArray)-1]:=tmp;
      end
      else
      begin
        {$ifdef JScript} asm console.log('skip '+i); end; {$endif}  //529
        if length(myxpmarray[i])<lengthOfPixelsLine then
        begin
          // add this to the next one
          myxpmarray[i+1]:='",'+myxpmarray[i]+myxpmarray[i+1];
        end
        else
          if length(fMapPixelArray)>0 then
            i:=myxpmarray.Count;  // pixels done - stop
      end;
      i:=i+1;
    end;
    if  length(fMapPixelArray)<>BitMapHeight then
      showmessage('BitMapHeight mismatch (BitMapHeight='+inttostr(BitMapHeight)+' count='+inttostr(length(fMapPixelArray)));
    MapPixelArray:=fMapPixelArray;  //sets the node attribute value

    {$ifndef JScript}
    {$else}
    //showmessage('SetMapData 5');
    h:=self.ActualHeight;
    sh:=length(fMapPixelArray);
    //showmessage('h='+inttostr(h)+' sh='+inttostr(sh));
    if h>0 then
    begin
      w:=self.ActualWidth;
      sw:=length(fMapPixelArray[0]);
      //showmessage('w='+inttostr(w));
      self.PaintRect( 0, 0, sw, sh, w, h);
    end;
    myNode.SetAttributeValue('MapData',AValue);
    //showmessage('SetMapData done');
    {$endif}

    bits0.Free;
    bits.Free;
  end;

  myxpmArray.Free;

end;

procedure TXBitMap.RebuildXPMDataString;
var
   newmapdata:String;
   i:integer;
begin
  newmapdata:= '/* XPM */' +
    'static char * XMap[] = {' +
    '/* <Values>*/' +
    '/* <width/columns> <height/rows> <colors> <chars per pixel>*/' +
    '"'+inttostr(fBitMapWidth)+' '+inttostr(fBitMapHeight)+' '+inttostr(fNumColors)+' '+inttostr(fCharsPerColor)+'",'+
    '/* <Colors>*/';
  for i:=0 to fColorsArray.Count-1 do
    newmapdata:=newmapdata + fColorsArray[i] + ',';
  newmapdata:=newmapdata + '/* <Pixels>*/';
  for i:=0 to length(fMapPixelArray)- 1 do
    if i< length(fMapPixelArray) then
      newmapdata:=newmapdata + '"' + fMapPixelArray[i] + '",'
    else
      newmapdata:=newmapdata + '"' + fMapPixelArray[i] + '"';
  newmapdata:=newmapdata + '};';

  myNode.SetAttributeValue('MapData',newmapdata);
end;

procedure TXBitMap.SetMapColors(AValue:string);
var
  bits:TStringlist;
  i:integer;
  tmp,NewMapData,oldval:String;
begin
  oldval:=myNode.GetAttribute('MapColors',true).AttribValue;
  if AValue<>oldval then
  begin
    myNode.SetAttributeValue('MapColors',AValue);
    // apply a new set of colours to the existing bitmap ????
    // !!!! would need to validate against the pixel array for any missing colors !!!!

    bits:=TStringList.Create;
    bits.StrictDelimiter:=true;
    bits.LineBreak:=',';
    bits.Text:=AValue;

    fColorsArray.Clear;
    for i:=0 to bits.Count-1 do
    begin
      tmp:=StripComments(bits[i]);
      if tmp<>'' then
        fColorsArray.Add(tmp);
    end;
    bits.Free;
    fNumColors:=fColorsArray.Count;

    if AValue<>'' then
    begin
      RebuildXPMDataString;  //sets the node attribute value from arrays
      NewMapData:=myNode.GetAttribute('MapData',false).AttribValue;
      myNode.SetAttributeValue('MapData','...');   // fudge so that the following SetMapData will see a change.
      MapData:=NewMapData; //redisplays bitmap
    end;
  end;
end;

begin
  // this is the set of node attributes that each XBitmap instance will have.
  AddWrapperDefaultAttribs(myDefaultAttribs);
  AddDefaultAttribute(myDefaultAttribs,'ActualHeight','Integer','','',true,false);
  AddDefaultAttribute(myDefaultAttribs,'ActualWidth','Integer','','',true,false);
  AddDefaultAttribute(myDefaultAttribs,'ImageWidth','String','250','',false);
  AddDefaultAttribute(myDefaultAttribs,'ImageHeight','String','200','',false);
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','Right','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelText','String','Bitmap Image','',false);
  AddDefaultAttribute(myDefaultAttribs,'MapColors','String','','',false,false);
  AddDefaultAttribute(myDefaultAttribs,'MapData','String','/* XPM */' +
      'static char * XMap[] = {' +
      '/* <Values>' +
      '/* <width/columns> <height/rows> <colors> <chars per pixel>*/' +
      '"48 20 2 1",' +
      '/* <Colors>*/' +
      '"a c #ffffff",' +
      '"b c #000000",' +
      '/* <Pixels>*/' +
      '"abaabaababaaabaabababaabaabaababaabaaababaabaaab",' +
      '"bbaabaababaaabaabababaabaabaababaabaaababaabaaab",' +
      '"abaabaababaaabaabababaabaabaababaabaaababaabaaab",' +
      '"abaabaababaaabaabababaabaabaababaabaaababaabaaab",' +
      '"bbaabaababaaabaabababaabaabaababaabaaababaabaaab",' +
      '"abaabaababaaabaabababaabaabaababaabaaababaabaaab",' +
      '"abaabaababaaabaabababaabaabaababaabaaababaabaaab",' +
      '"bbaabaababaaabaabababaabaabaababaabaaababaabaaab",' +
      '"abaabaababaaabaabababaabaabaababaabaaababaabaaab",' +
      '"bbaabaababaaabaabababaabaabaababaabaaababaabaaab",' +
      '"abaabaababaaabaabababaabaabaababaabaaababaabaaab",' +
      '"bbaabaababaaabaabababaabaabaababaabaaababaabaaab",' +
      '"abaabaababaaabaabababaabaabaababaabaaababaabaaab",' +
      '"abaabaababaaabaabababaabaabaababaabaaababaabaaab",' +
      '"bbaabaababaaabaabababaabaabaababaabaaababaabaaab",' +
      '"abaabaababaaabaabababaabaabaababaabaaababaabaaab",' +
      '"abaabaababaaabaabababaabaabaababaabaaababaabaaab",' +
      '"bbaabaababaaabaabababaabaabaababaabaaababaabaaab",' +
      '"abaabaababaaabaabababaabaabaababaabaaababaabaaab",' +
      '"bbaabaababaaabaabababaabaabaababaabaaababaabaaab"' +
      '/* <Extensions>*/' +
      '};','',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  AddAttribOptions(MyNodeType,'LabelPos',LabelPosOptions);
  {$ifndef JScript}
  RegisterClass(TXBitMap);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}
  SuppressDesignerProperty(MyNodeType,'ContainerHeight');
  SuppressDesignerProperty(MyNodeType,'ContainerWidth');

end.
