(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit StringUtils;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, Math, TypInfo, EventsInterface
  {$ifndef JScript}
  , fpjson  , jsonparser
  , Forms, Dialogs, Controls, Graphics;
  {$else}
  ;
  {$endif}

{$ifdef JScript}
type AnsiString = String;
{$endif}

type TStringArray = Array of String;
type T2DStringArray = Array of TStringArray;
type T3DStringArray = Array of T2DStringArray;

function FoundString(inString,searchString:string):integer;  // find first occurrence of "searchString"
function FoundStringCI(inString,searchString:string):integer;  // case-insensitive
function myStringReplace(Instring,OldString,NewString:String;ReplaceNum,MaxStringLength:integer):String;
function MyBoolToStr(inBool:boolean):string;
function MyStrToBool(inStr:string):Boolean;
function TrimWhiteSpace(Instring:string):String;
function stringsplit(str:string; separator:string; skiplast:boolean=true):TStringList;
function IsInStringList(myList:TStringList;elem:string):boolean;
function CommaListToStringArray(AValue:String):TStringArray;
function StringToSubStringList(InString,delimiter:String; skiplast:boolean=true):TStringList;
function DelChars(Instring,FilterChar:String):String;
function stripLeadingStringIfPresent(instring,LeadingString:String):String;
Function confirm(Textmessage:string):boolean;
Function prompt(TextMessage,promptString:string):string;
function JSONStringToStringList(const JSONString:String):TStringList;
function StringListToJSONString(StringList:TStringList):String;
function NumArrayToJSONString(NumArray:TNumArray):String;
function Num3DArrayToJsonString(arr:T3DNumArray):String;
function Num2dArrayToString(NumArray:T2dNumArray):String;
function JsonStringTo3DNumArray(const str:String):T3DNumArray;
function QuoteIt(str:String):String;
procedure ShowAllChars(str:String);
function IsStrFloatNum(str: string): Boolean;


{$ifndef JScript}
function ColorToHexRGB(Color: TColor): string;
function HexRGBToColor(RGBString: String): TColor;
{$else}
procedure HexRGBToColor(RGBString: String; var r,g,b,a:integer);
procedure ShowMessage(text:String);
{$endif}

var MainUnitName:String;

implementation
uses Events;

procedure ShowAllChars(str:String);
var
  msg:string;
  i:integer;
begin
  for i:=1 to length(str) do
  begin
    if ord(str[i])>31 then
      msg:=msg+str[i]
    else
      msg:=msg+'#'+inttostr(ord(str[i]));
  end;
  showmessage('chars='+msg);
end;

{$ifndef JScript}
{$else}

procedure Showmessage(text:String);
begin
  asm
    alert(text);
  end;
end;

{$endif}

function IsStrFloatNum(str: string): Boolean;
var
 dummyNumber:double;
 posError:integer;
begin
 result:=true;
 val(str, dummyNumber, posError);
 if poserror>0 then
   result:=false;
end;

function QuoteIt(str:String):String;
begin
  if trim(str)='' then
    result:='""'
  else
  begin
    if (str[1]='"')
    or (str[1]='''') then
      result:=str
    else
      result:='"'+str+'"';
  end;
end;

function IsInStringList(myList:TStringList;elem:string):boolean;
var
  i:integer;
  found:boolean;
begin
  found:=false;
  i:=mylist.IndexOf(elem);
  if i>-1 then found:=true;

  result:=found;
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

function CharReplace(Instring,OldChar,NewChar:String):string;
var i:integer;
    finalstring:string;
begin
  finalstring:='';
  for i:=1 to Length(Instring) do
  begin
    if Instring[i]= OldChar
    then finalstring:=finalstring+NewChar
    else finalstring:=finalstring+Instring[i] ;
  end;
  result :=  finalstring;
End;

function  myStringReplace(Instring,OldString,NewString:String;ReplaceNum,MaxStringLength:integer):String;
// replaces "ReplaceNum" occurrences of oldstring in the first "MaxStringLength" of Instring --- NB it is case sensitive
var i,matchLength:integer;
  match:Boolean;
  replaceCount:integer;
  finalstring,tempstr:String;
begin
  finalstring:='';
  replaceCount:=0;
  matchLength:=0;
  if ReplaceNum<0 then ReplaceNum:=999999;
  if MaxStringLength<0 then MaxStringLength:=9999999;
  for i:=1 to Length(Instring) do
  begin
    tempstr:=Instring[i];
    if (( tempstr<>OldString[1])and (i>matchLength)) or (i>MaxStringLength)
    then
    begin
      finalstring:=finalstring+tempstr;
    end
    else   //potential match
    if (i>matchLength) then
    begin
      match := CheckMatch(Instring,oldstring,i);
      if (match = false) or (replaceCount>=ReplaceNum)
      then finalstring:=finalstring+tempstr
      else
      begin
          replaceCount:=replaceCount+1;
          finalstring:=finalstring+NewString;
          matchlength:= i+ Length(OldString)-1;
      end;
    end;
  end;
  result :=  finalstring;
end;

function FoundString(inString,searchString:string):integer;  // find first occurrance of "searchString"
  var
    match,found:Boolean;
    i,tempresult:integer;
    tempstr:String;
  begin
    tempresult:=0;
    found:=false;
    for i:=1 to Length(Instring) do
    begin
      if found=false then
      begin
        tempstr:=Instring[i];
        if ( tempstr=searchString[1]) then
        begin  //potential match
          match := CheckMatch(Instring,searchString,i);
          if (match = true) then
          begin
            tempresult:=i;
            found:=true;
          end;
        end;
      end;
    end;
    result :=  tempresult;
end;

function FoundStringCI(inString,searchString:string):integer;  // case-insensitive
  var
    tempresult:integer;
  begin
    tempresult:=FoundString(upperCase(inString),upperCase(searchString));
    result :=  tempresult;
end;


function DelChars(Instring,FilterChar:String):String;
var i:integer;
  newstring,tempstr:String;
begin
  newstring:='';
  for i:=1 to Length(Instring) do
  begin
    tempstr:=Instring[i];
    if  tempstr<>FilterChar
    then
    begin
      newstring:=newstring+tempstr;
    end;
  end;
  result :=  newstring;
end;

function StringToSubStringList(InString,delimiter:String;skiplast:Boolean=true):TStringList;
var items : TStringList;
{$ifdef JScript}
  i:integer;
  DelimiterAtEnd:boolean;
  {$endif}
begin
  items := TstringList.Create;
  items.StrictDelimiter:=true;
  items.SkipLastLineBreak:=skiplast;
  items.LineBreak:=delimiter;
  items.text:= InString;

  {$ifdef JScript}
  DelimiterAtEnd:=true;
  if SkipLast=false then
  if length(InString)>=length(delimiter) then
  begin
    for i:=length(InString)-length(Delimiter)+1 to length(InString) do
      if InString[i]<>Delimiter[i-(length(InString)-length(Delimiter))] then
        DelimiterAtEnd:=false;
    if DelimiterAtEnd then
    begin
      items.Add('');
      //showmessage('item added for '+InString);
    end;
  end;
  {$endif}
  StringToSubStringList:=items;
  //items.free;
end;

function TrimWhiteSpace(Instring:string):String;
begin
  result:=DelChars(Instring,' ');
end;

function stripLeadingStringIfPresent(instring,LeadingString:String):String;
var i:integer;
  OutString:string;
  done:boolean;
begin
  OutString:='';
  done:=false;
  for i :=1 to Length(instring) do
  begin
    if ( i <=  Length(LeadingString)) and (done=false) then
    begin
      if instring[i]<>LeadingString[i]
      then
      begin
        done:=true;
        OutString:=OutString+instring[i]   ;
      end;
    end
    else OutString:=OutString+instring[i]   ;
  end;
  result:=OutString;
end;


function MyBoolToStr(inBool:boolean):string;
begin
  if inBool=true then
    result:='True'
  else if inBool=false then
    result:='False'
  else
  begin
    showmessage('invalid boolean');
      {$ifdef JScript}
      asm
        alert('inBool='+inBool);
      end;
      {$endif}

  end;
end;

function MyStrToBool(inStr:string):Boolean;
begin
  if uppercase(TrimWhiteSpace(instr))='TRUE' then
    result:=True
  else if uppercase(TrimWhiteSpace(instr))='FALSE' then
    result:=False
  else if inStr='' then
    result:=false
  else
    showmessage('invalid boolean string '+inStr);
end;

function myFloatToStr(invar:real):string;
begin
  result:=FloatToStr(invar);
end;

function mystrtoFloat(instring:string):real;
begin
  result:=strtoFloat(instring);
end;

function stringsplit(str:string; separator:string; skiplast:boolean=true):TStringList;
var
   localStringList:TStringList;
begin
  localStringList:=StringToSubStringList(str,separator,skiplast);
  result:=localStringList;
end;


function CommaListToStringArray(AValue:String):TStringArray;
var
    bits:TStringList;
    i:integer;
    arr:TStringArray;
begin
  bits:=TStringList.Create;
  bits.StrictDelimiter:=true;
  bits.LineBreak:=',';
  bits.Text:=AValue;
  setlength(arr,bits.Count);
  for i:=0 to bits.count-1 do
    arr[i]:=bits[i];
  result:=arr;
  bits.Free;
end;

function Num2dArrayToString(NumArray:T2dNumArray):String;
var
    TempString:String;
    i:integer;
begin
  TempString:='[';
  for i:=0 to length(NumArray)-1 do
  begin
    if i>0 then TempString:=TempString+',';
    TempString:=TempString+NumArrayToJsonString(NumArray[i]);
  end;
  TempString:=TempString+']';
  result:=TempString;
end;

function Num3DArrayToJsonString(arr:T3DNumArray):String;
var
    z:integer;
    str:String;
begin
  str:='';
  for z:=0 to length(arr)-1 do
  begin
    if z>0 then
      str:=str+',';
    str:=str+Num2DArrayToString(arr[z]);
  end;
  result:='['+str+']';
end;



{$ifndef JScript}
function JSONStringToStringList(const JSONString:String):TStringList;
var
  jData : TJSONData;
  i:integer;
  items : TStringList;
begin
  items := TstringList.Create;
  items.StrictDelimiter:=true;
  items.LineBreak:=',';
  // create from string
  try
    jData := GetJSON(JSONString);
  except
    on E: Exception do
    begin
      showmessage('JSON error: '+e.Message);
      jData := nil;
    end;
  end;
  if jData<>nil then
  begin
    for i:=0 to jData.Count-1 do
      items.Add(jData.Items[i].AsString);
    jData.free;
  end;
  result:=items;
end;


function JsonStringTo3DNumArray(const str:String):T3DNumArray;
var
   Data,zData : TJSONData;
   zCount:integer;
   arr:T3DNumArray;
   ArrayStr:String;
    zItem,yItem,xItem : TJSONData;
    z,y,x:integer;
    object_type:string;
begin
  setlength(arr,0);
  try
    Data := GetJSON(str);
  except
    on E: Exception do
    begin
      showmessage('JSON error: '+e.Message);
      Data := nil;
    end;
  end;
//  ArrayStr:= Data.AsJSON;         // "[[[...],[...]],[[...]]]"
//  if ArrayStr[1]='"' then
//  begin
//    Delete(ArrayStr,1,1);
//    Delete(ArrayStr,length(ArrayStr),1);
//  end;
  if Data<>nil then
  begin
    ArrayStr:=str;

    zData := GetJSON(ArrayStr);
    zcount:=zData.Count;
    setlength(arr,zCount);
    for z :=0 to zcount-1 do
    begin
      zItem := zData.Items[z];
      setlength(arr[z],zItem.Count);
      for y:=0 to zItem.Count-1 do
      begin
        yItem := zItem.Items[y];
        setlength(arr[z,y],yItem.Count);
        object_type := GetEnumName(TypeInfo(TJSONtype), Ord(yItem.JSONType));
        if object_type='jtArray' then
        begin
          for x:=0 to yItem.Count-1 do
          begin
            xItem:= yItem.Items[x];
            object_type := GetEnumName(TypeInfo(TJSONtype), Ord(xItem.JSONType));
            if object_type='jtNumber' then
            begin
              arr[z,y,x]:=xItem.AsFloat;
            end
            else
              arr[z,y,x]:=0.0;
          end;
        end;
      end;
    end;
  end;
  Data.Free;
  zData.Free;

  result:=arr;
end;


function StringListToJsonString(StringList:TStringList):String;
var
  jData : TJSONData;
  jArr: TJSONArray;
  s:string;
  i:integer;
begin
  jArr:=TJSONArray.Create;
  for i:=0 to StringList.Count-1 do
    jArr.Add(StringList[i]);
  s:=jArr.AsJSON;
  // create from string
  jData := GetJSON(s);     //('{"Fld1" : "Hello", "Fld2" : 42, "Colors" : ["Red", "Green", "Blue"]}');
  // output as a flat string
  s := jData.AsJSON;
  // output as nicely formatted JSON
  //s := jData.FormatJSON;
  jData.Free;
  result:=s;
end;

function NumArrayToJsonString(NumArray:TNumArray):String;
var
    TempString:String;
    i:integer;
begin
  //example '[0,1.2,3.65,8.02]'
  TempString:='[';
  if NumArray<>nil then
    for i:=0 to length(NumArray)-1 do
    begin
      if i>0 then TempString:=TempString+',';
      TempString:=TempString+floatToStr(NumArray[i]);
    end;
  TempString:=TempString+']';
  result:=TempString;
end;


//function ImgArrayToJsonString(ImgArray:TImgArray):String;
//var
//    TempString:String;
//    i:integer;
//begin
//  TempString:='[';
//  for i:=0 to length(ImgArray)-1 do
//  begin
//    if i>0 then TempString:=TempString+',';
//    TempString:=TempString+ImgArray[i];
//  end;
//  TempString:=TempString+']';
//  result:=TempString;
//end;

function ColorToHexRGB(Color: TColor): string;
var
  N: Longint;
begin
  if Color=clNone then
    begin Result:= ''; exit; end;
  N:= ColorToRGB(Color);
//  {$ifndef JScript}
  Result:= '#'+
//  {$else}
//  Result:= '%23'+
//  {$endif}
    IntToHex(Red(N), 2)+
    IntToHex(Green(N), 2)+
    IntToHex(Blue(N), 2);
end;

function HexRGBToColor(RGBString: String): TColor;
type
  T4Byte = array [0 .. 2] of Byte;
var
  bits:TStringList;
  str:string;
  ba: T4Byte;
  shadow: LongWord absolute ba;
begin
  result:=clNone;
  if RGBString='' then
    result:=clNone
  else
  begin
    // in case of escaped # char, switch to #
    RGBString:=mystringreplace(RGBString,'%23','#',1,-1);
    bits := StringSplit(RGBString,'#');
    if bits.Count>1 then
    begin
      str:=bits[1];
      shadow := StrToInt('$' + str);
      // ba now contains the bytes of YourHEXString
      result:=RGBToColor(ba[2],ba[1],ba[0]);
    end;
  end;
end;
{$else}
function JSONStringToStringList(const JSONString:String):TStringList;
var items : TStringList;
    TempString:String;
begin
  // example optionlist '["Banana","Cherry","Lemon","Carrot","Eggplant","Potato"]'
  TempString:=JSONString;
  TempString := StringReplace(TempString, '[', '',[rfReplaceAll]);
  TempString := StringReplace(TempString, ']', '',[rfReplaceAll]);
  TempString := StringReplace(TempString, '"', '',[rfReplaceAll]);
  items := TstringList.Create;
  items.StrictDelimiter:=true;
  items.LineBreak:=',';
  items.text:= TempString;
  result:=items;
end;


function StringListToJsonString(StringList:TStringList):String;
var
    TempString:String;
    i:integer;
begin
  //example optionlist '["Banana","Cherry","Lemon","Carrot","Eggplant","Potato"]'
  TempString:='[';
  for i:=0 to StringList.Count-1 do
  begin
    if i>0 then TempString:=TempString+',';
    TempString:=TempString+QuoteIt(StringList[i]);
  end;
  TempString:=TempString+']';
  result:=TempString;
end;

function NumArrayToJsonString(NumArray:TNumArray):String;
var
    TempString:String;
    i:integer;
begin
  TempString:='[';
  for i:=0 to length(NumArray)-1 do
  begin
    if i>0 then TempString:=TempString+',';
    TempString:=TempString+floatToStr(NumArray[i]);
  end;
  TempString:=TempString+']';
  result:=TempString;
end;

//function ImgArrayToJsonString(ImgArray:TImgArray):String;
//var
//    TempString:String;
//    i:integer;
//begin
//  TempString:='[';
//  for i:=0 to length(ImgArray)-1 do
//  begin
//    if i>0 then TempString:=TempString+',';
//    TempString:=TempString+ImgArray[i];
//  end;
//  TempString:=TempString+']';
//  result:=TempString;
//end;

procedure HexRGBToColor(RGBString: String; var r,g,b,a:integer);
type
  T4Byte = array [0 .. 2] of Byte;
var
  bits:TStringList;
  str,rs,gs,bs:string;
begin
  r:=0;
  g:=0;
  b:=0;
  a:=255;
  if RGBString<>'' then
  begin
    // in case of escaped # char, switch to #
    RGBString:=mystringreplace(RGBString,'%23','#',1,-1);
    bits := StringSplit(RGBString,'#');
    if bits.Count>1 then
    begin
      str:=bits[1];
      if length(str)=6 then
      begin
        rs:=str[1]+str[2];
        gs:=str[3]+str[4];
        bs:=str[5]+str[6];
        r:=strtoint('$'+rs);
        g:=strtoint('$'+gs);
        b:=strtoint('$'+bs);
      end;
      a:=0;
    end;
  end;
end;

function JsonStringTo3DNumArray(const str:String):T3DNumArray;
var
arr:T3DNumArray;
begin
  asm
    arr = JSON.parse(str);
  end;
  result:=arr;
end;
{$endif}

Function confirm(Textmessage:string):boolean;
{$ifndef JScript}
begin
   if MessageDlg('Confirm', Textmessage, mtConfirmation,
   [mbYes, mbNo],0) = mrYes
  then result:=true
  else result:=false;
   //Following event handler being called due to requirement for
   //event logging.
   // Not available for capture by user-written code.
   HandleEvent(nil,'UserConfirm','UIRootNode','',myBoolToStr(result));
{$else}
var
  conf:Boolean;
begin
  asm
    conf=confirm(Textmessage);

    //Following event handler being called due to requirement for
    //event logging.
    // Not available for capture by user-written code.
    if (conf!=null) {
      pas.Events.handleEvent(null,'UserConfirm','UIRootNode','',conf.toString());
    }
  end;
  result:=conf;
{$endif}
end;

{$ifndef JScript}
Function prompt(TextMessage,promptString:string):string;
var
  ok:Boolean;
begin
   ok:=InputQuery('Input', TextMessage, promptString);
   if ok then
     result:=promptstring
   else
     result:='';
   //Following event handler being called due to requirement for
   //event logging.
   // Not available for capture by user-written code.
   HandleEvent(nil,'UserInput','UIRootNode','',result);
end;
{$else}
Function prompt(TextMessage,promptString:string):string;
var
  str:String;
begin
  asm
    var res=prompt(TextMessage,promptString);
    if (res==null) {str=''} else {str=res}

      //Following event handler being called due to requirement for
      //event logging.
      // Not available for capture by user-written code.
      if (res!=null) {
        pas.Events.handleEvent(null,'UserInput','UIRootNode','',res);
      }
  end;
  result:=str;
end;
{$endif}

begin
end.

