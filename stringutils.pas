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
  Classes, SysUtils, Math, EventsInterface
  {$ifndef JScript}
  , fpjson  , jsonparser
  , Forms, Dialogs, Controls, Graphics;
  {$else}
  ;
  {$endif}

{$ifdef JScript}
type AnsiString = String;
{$endif}

function FoundString(inString,searchString:string):integer;  // find first occurrence of "searchString"
function myStringReplace(Instring,OldString,NewString:String;ReplaceNum,MaxStringLength:integer):String;
function MyBoolToStr(inBool:boolean):string;
function MyStrToBool(inStr:string):Boolean;
function TrimWhiteSpace(Instring:string):String;
function stringsplit(str:string; separator:string):TStringList;
function IsInStringList(myList:TStringList;elem:string):boolean;
function CommaListToStringArray(AValue:String):TStringArray;
function StringToSubStringList(InString,delimiter:String):TStringList;
function DelChars(Instring,FilterChar:String):String;
function stripLeadingStringIfPresent(instring,LeadingString:String):String;
Function confirm(Textmessage:string):boolean;
Function prompt(TextMessage,promptString:string):string;
function JSONStringToStringList(JSONString:String):TStringList;
function StringListToJSONString(StringList:TStringList):String;
function NumArrayToJSONString(NumArray:TNumArray):String;
function ImgArrayToJSONString(ImgArray:TImgArray):String;

{$ifndef JScript}
function ColorToHexRGB(Color: TColor): string;
function HexRGBToColor(RGBString: String): TColor;
{$else}
procedure HexRGBToColor(RGBString: String; var r,g,b,a:integer);
procedure ShowMessage(text:String);
{$endif}

var MainUnitName:String;

implementation

{$ifndef JScript}
{$else}

procedure Showmessage(text:String);
begin
  asm
    alert(text);
  end;
end;

{$endif}

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

function StringToSubStringList(InString,delimiter:String):TStringList;
var items : TStringList;
begin
  items := TstringList.Create;
  items.StrictDelimiter:=true;
  //items.SkipLastLineBreak:=false;
  items.LineBreak:=delimiter;
  items.text:= InString;
  StringToSubStringList:=items;
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

function stringsplit(str:string; separator:string):TStringList;
var
   localStringList:TStringList;
begin
  localStringList:=StringToSubStringList(str,separator);
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

{$ifndef JScript}
function JSONStringToStringList(JSONString:String):TStringList;
var
  jData : TJSONData;
  i:integer;
  items : TStringList;
begin
  items := TstringList.Create;
  // create from string
  jData := GetJSON(JSONString);
  for i:=0 to jData.Count-1 do
    items.Add(jData.Items[i].AsString);
  items.StrictDelimiter:=true;
  items.LineBreak:=',';
  result:=items;
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
  result:=s;
end;

function NumArrayToJsonString(NumArray:TNumArray):String;
var
    TempString:String;
    i:integer;
begin
  //example optionlist '["Banana","Cherry","Lemon","Carrot","Eggplant","Potato"]'
  TempString:='[';
  for i:=0 to length(NumArray)-1 do
  begin
    if i>0 then TempString:=TempString+',';
    TempString:=TempString+floatToStr(NumArray[i]);
  end;
  TempString:=TempString+']';
  result:=TempString;
end;

function ImgArrayToJsonString(ImgArray:TImgArray):String;
var
    TempString:String;
    i:integer;
begin
  //example optionlist '["Banana","Cherry","Lemon","Carrot","Eggplant","Potato"]'
  TempString:='[';
  for i:=0 to length(ImgArray)-1 do
  begin
    if i>0 then TempString:=TempString+',';
    TempString:=TempString+ImgArray[i];
  end;
  TempString:=TempString+']';
  result:=TempString;
end;

function ColorToHexRGB(Color: TColor): string;
var
  N: Longint;
begin
  if Color=clNone then
    begin Result:= ''; exit; end;
  N:= ColorToRGB(Color);
  Result:= '#'+
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
function JSONStringToStringList(JSONString:String):TStringList;
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
    TempString:=TempString+'"'+StringList[i]+'"';
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

function ImgArrayToJsonString(ImgArray:TImgArray):String;
var
    TempString:String;
    i:integer;
begin
  TempString:='[';
  for i:=0 to length(ImgArray)-1 do
  begin
    if i>0 then TempString:=TempString+',';
    TempString:=TempString+ImgArray[i];
  end;
  TempString:=TempString+']';
  result:=TempString;
end;

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
{$endif}

{$ifndef JScript}
Function confirm(Textmessage:string):boolean;
begin
   if MessageDlg('Confirm', Textmessage, mtConfirmation,
   [mbYes, mbNo],0) = mrYes
  then result:=true
  else result:=false;
end;
{$else}
Function confirm(Textmessage:string):boolean;
var
  conf:Boolean;
begin
  asm
    conf=confirm(Textmessage);
  end;
  result:=conf;
end;
{$endif}

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
end;
{$else}
Function prompt(TextMessage,promptString:string):string;
var
  str:String;
begin
  asm
    var res=prompt(TextMessage,promptString);
    if (res==null) {str=''} else {str=res}
  end;
  result:=str;
end;
{$endif}

begin
end.
