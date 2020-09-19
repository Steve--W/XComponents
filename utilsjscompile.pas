(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit UtilsJSCompile;

{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface
uses
  Classes, SysUtils, StringUtils, XCode
  {$ifndef JScript}
  ,Forms,StdCtrls,Dialogs,LCLIntf, Pas2JSCompiler, Pas2JSLogger, Pas2JSFileUtils, LResources, URIParser;
  {$else}
   ;
  {$endif}


  function BuildHTMLHead(ProgramName,docTitle,DeployMode,JSString:String):String;
  function BuildHTMLBody:String;
{$ifndef JScript}
procedure AddRequiredFile(ResourceName,ResourcePath:String);
procedure InitialiseCompilerResources(ProgramName,ProgPath:string);
function CreateHTMLWrapper(ProgramName,DeployMode:String; embedJS:Boolean; JSString:String):String;
procedure TranspileMyProgram(ProgramName,ProgPath,ProjectPath:string; UseCodeEditor:TXCode; NoOptimise:Boolean; ExtraDirectives:TStringList);
procedure CompileJSandExecute(ProjectPath:String;ExtraDirectives,ExtraHTML:TStringList);  overload;
procedure CompileJSandExecute(ProjectPath:String);  overload;
procedure WriteResourceFiles(ProgramName,ProgPath:String);

type
  TRequiredResource = record
    ResourceName:String;
    ResourcePath:String;
  end;
  TRequiredResources = Array of TRequiredResource;

var ListOfUnits: TStringList;
var
  MainFormHeight,MainFormWidth:integer;
  RequiredFiles:TRequiredResources;
  ProjectCompilerLogMemo:TMemo;

type TMyCompilerObj = Class(TObject)
  Compiler:TPas2jsCompiler;
  CurrentCodeEditor:TObject;       //TXCode
  private
    procedure DoLogForEvents(Sender: TObject; const Msg: String);
    procedure DoLogForProject(Sender: TObject; const Msg: String);
end;

{$endif}
var
  RequiredFolders:TStringList;
  AdditionalScript:String;


implementation
uses NodeUtils
{$ifndef JScript}
,LazsUtils
{$endif}
;

function BuildHTMLHead(ProgramName,docTitle,DeployMode,JSString:String):String;
var
  BatchString,IFrameMessageHandler:String;
begin
  IFrameMessageHandler:='  window.addEventListener("message", function(ev) { '+Lineending
                  + 'if (ev.data.objid==undefined) { '  +LineEnding
                  + '    pas.XSVGContainer.HandleMessage(ev.data); } '+lineEnding   // svg titlechange
                  + 'else { '+lineEnding
                  + '  if (ev.data.mtype=="titleChange") { '+lineEnding
                  + '    pas.XHTMLEditor.HandleTXHTMLMessage(ev.data);  '+lineEnding   // htmleditor text change
                  + '   } '
                  + '} });'  +LineEnding;

  BatchString:=BatchString
    +'  <head >'  +LineEnding
    +'   <meta charset="utf-8"> '  +LineEnding
    +'   <title>'+docTitle+'</title>'  +LineEnding
    +'<noscript>Your browser does not support JavaScript!</noscript> '  +LineEnding
    +'    <!_ load the javascript libraries _>'  +LineEnding;

    BatchString:=BatchString+'<div id="ProjectCodeContainer">'+JSString+'</div>';

    BatchString:=BatchString + '<script type="application/javascript" >'+LineEnding;
    Batchstring:=BatchString + '/*Deployment1*/var myDeployedMode = '''+DeployMode+''';/*Deployment2*/'+LineEnding;
    BatchString:=BatchString+'</script>' +LineEnding

    +'    <style> html {height: 100%;} ' +LineEnding
    +'            body { height:100%; min-height: 100%;} ' +LineEnding
    +'    </style>' +LineEnding
    +'    <style>  '  +LineEnding
    +'       @keyframes fadeIn { from { opacity: 0; }}  '  +LineEnding
    +'       .highlight-border { border:dashed 3px green !important; outline:none !important;}'  +LineEnding
    +'       .normal-border { border:solid 1px gray; outline:none;}'  +LineEnding
    +'       .no-border { border:none 1px gray; outline:none;}'  +LineEnding
    +'    </style>  '  +LineEnding
    +AdditionalScript  +LineEnding
    +'<script > '  +LineEnding
    +'function  StartupCode(){ ' +LineEnding
    +'  try{'  +LineEnding
    +'    rtl.run("'+ProgramName+'"); ' +LineEnding
    +'    pas.'+ProgramName+'.InitialisePage();'  +LineEnding
    +IFrameMessageHandler
    +'  }catch(err) {alert("Error in StartupCode ---"+err.message);}; '  +LineEnding
    +'};  '   +LineEnding
    +'</script>  '   +LineEnding
    +'</head> '  +LineEnding;

    result:=BatchString;
end;

function BuildHTMLBody:String;
begin
  result:=
  '<body style="margin:0px; font:normal 12px Verdana, Arial, sans-serif;"'
  +'        onload = " StartupCode();" ' +LineEnding
  +'> '  +LineEnding
  +'  <div  id = "'+NodeUtils.SystemRootName+'" class="vbox" style="height:100%; width:100%;top:0px;left:0px; position:relative; z-index:0;"> '  +LineEnding
  +'    <div  id = "'+MainForm.Name+'" class="vbox" style="height:100%; width:100%;top:0px;left:0px"> '  +LineEnding
  +'    </div> '  +LineEnding
  +'  </div> '  +LineEnding
  +'</body>'   +LineEnding
  +'</html> ' +LineEnding  ;
end;

{$ifndef JScript}
procedure TMyCompilerObj.DoLogForEvents(Sender: TObject; const Msg: String);
begin
  if CurrentCodeEditor<>nil then
    TXCode(CurrentCodeEditor).MessageLines:=TXCode(CurrentCodeEditor).MessageLines+LineEnding+Msg;
end;
procedure TMyCompilerObj.DoLogForProject(Sender: TObject; const Msg: String);
begin
  if  ProjectCompilerLogMemo<>nil then
    ProjectCompilerLogMemo.Lines.Add(Msg);
end;

procedure AddRequiredFile(ResourceName,ResourcePath:String);
begin
   SetLength(RequiredFiles,length(RequiredFiles)+1);
   RequiredFiles[length(RequiredFiles)-1].ResourceName:=ResourceName;
   RequiredFiles[length(RequiredFiles)-1].ResourcePath:=ResourcePath;
end;


procedure WriteResourceFiles(ProgramName,ProgPath:String);
var
  i:integer;
begin
  // Write out each required unit to a physical disk file, ready for use in pas2js compiler

  if not DirectoryExists(ProgPath+'resources')  then ForceDirectories(ProgPath+'resources');
  if not DirectoryExists(ProgPath+'resources/rtl')  then ForceDirectories(ProgPath+'resources/rtl');
  if not DirectoryExists(ProgPath+'resources/project')  then ForceDirectories(ProgPath+'resources/project');
  if not DirectoryExists(ProgPath+'resources/xcomponents')  then ForceDirectories(ProgPath+'resources/xcomponents');
  if not DirectoryExists(ProgPath+'resources/pas2jstranspiler')  then ForceDirectories(ProgPath+'resources/pas2jstranspiler');

  for i:=0 to RequiredFolders.Count-1 do
  begin
    if not DirectoryExists(ProgPath+RequiredFolders[i])  then ForceDirectories(ProgPath+RequiredFolders[i]);
  end;

  for i:=0 to length(RequiredFiles)-1 do
  begin
     ResourceToFile(RequiredFiles[i].ResourceName,ProgPath+RequiredFiles[i].ResourcePath);
  end;

  //files needed for the project to be compilable by pas2js...
  ResourceToFile('rtl',ProgPath+'resources/rtl/rtl.js');

  ResourceToFile('classes',ProgPath+'resources/rtl/classes.pas');
  ResourceToFile('contnrs',ProgPath+'resources/rtl/contnrs.pas');
  ResourceToFile('dateutils',ProgPath+'resources/rtl/dateutils.pas');
  ResourceToFile('js',ProgPath+'resources/rtl/js.pas');
  ResourceToFile('math',ProgPath+'resources/rtl/math.pas');
  ResourceToFile('rtlconsts',ProgPath+'resources/rtl/rtlconsts.pas');
  ResourceToFile('strutils',ProgPath+'resources/rtl/strutils.pas');
  ResourceToFile('system',ProgPath+'resources/rtl/system.pas');
  ResourceToFile('sysutils',ProgPath+'resources/rtl/sysutils.pas');
  ResourceToFile('types',ProgPath+'resources/rtl/types.pas');
  ResourceToFile('typinfo',ProgPath+'resources/rtl/typinfo.pas');

//  ResourceToFile('rtti',ProgPath+'resources/rtl/rtti.pas');
//  ResourceToFile('browserconsole',ProgPath+'resources/rtl/browserconsole.pas');
//  ResourceToFile('class2pas',ProgPath+'resources/rtl/class2pas.pas');
//  ResourceToFile('hotreloadclient',ProgPath+'resources/rtl/hotreloadclient.pas');
//  ResourceToFile('libjquery',ProgPath+'resources/rtl/libjquery.pas');
//  ResourceToFile('nodejs',ProgPath+'resources/rtl/nodejs.pas');
//  ResourceToFile('objpas',ProgPath+'resources/rtl/objpas.pas');
//  ResourceToFile('timer',ProgPath+'resources/rtl/timer.pas');
//  ResourceToFile('web',ProgPath+'resources/rtl/web.pas');
//  ResourceToFile('webaudio',ProgPath+'resources/rtl/webaudio.pas');
//  ResourceToFile('webbluetooth',ProgPath+'resources/rtl/webbluetooth.pas');
//  ResourceToFile('webgl',ProgPath+'resources/rtl/webgl.pas');
//  ResourceToFile('webrouter',ProgPath+'resources/rtl/webrouter.pas');


  //ResourceToFile('jsonreader',ProgPath+'resources/rtl/jsonreader.pp');     // these don't compile is pas2js (PChar issues)
  //ResourceToFile('jsonscanner',ProgPath+'resources/rtl/jsonscanner.pp');
  //ResourceToFile('jsonparser',ProgPath+'resources/rtl/jsonparser.pp');
  //ResourceToFile('fpjson',ProgPath+'resources/rtl/fpjson.pp');

  ResourceToFile('wrapperpanel',ProgPath+'resources/xcomponents/wrapperpanel.pas');
  ResourceToFile('stringutils',ProgPath+'resources/xcomponents/stringutils.pas');
  ResourceToFile('nodeutils',ProgPath+'resources/xcomponents/nodeutils.pas');
  ResourceToFile('htmlutils',ProgPath+'resources/xcomponents/htmlutils.pas');
  ResourceToFile('xvbox',ProgPath+'resources/xcomponents/xvbox.pas');
  ResourceToFile('xhbox',ProgPath+'resources/xcomponents/xhbox.pas');
  ResourceToFile('xbutton',ProgPath+'resources/xcomponents/xbutton.pas');
  ResourceToFile('events',ProgPath+'resources/xcomponents/events.pas');
  ResourceToFile('xeditbox',ProgPath+'resources/xcomponents/xeditbox.pas');
  ResourceToFile('xcheckbox',ProgPath+'resources/xcomponents/xcheckbox.pas');
  ResourceToFile('xscrollbox',ProgPath+'resources/xcomponents/xscrollbox.pas');
  ResourceToFile('xtabcontrol',ProgPath+'resources/xcomponents/xtabcontrol.pas');
  ResourceToFile('xlabel',ProgPath+'resources/xcomponents/xlabel.pas');
  ResourceToFile('xhyperlink',ProgPath+'resources/xcomponents/xhyperlink.pas');
  ResourceToFile('xmemo',ProgPath+'resources/xcomponents/xmemo.pas');
  ResourceToFile('xtree',ProgPath+'resources/xcomponents/xtree.pas');
  ResourceToFile('xradiobtns',ProgPath+'resources/xcomponents/xradiobtns.pas');
  ResourceToFile('xtable',ProgPath+'resources/xcomponents/xtable.pas');
  ResourceToFile('xform',ProgPath+'resources/xcomponents/xform.pas');
  ResourceToFile('xprogressbar',ProgPath+'resources/xcomponents/xprogressbar.pas');
  ResourceToFile('xnumericslider',ProgPath+'resources/xcomponents/xnumericslider.pas');
  ResourceToFile('xnumberspinner',ProgPath+'resources/xcomponents/xnumberspinner.pas');
  ResourceToFile('xcombobox',ProgPath+'resources/xcomponents/xcombobox.pas');
  ResourceToFile('xcolorpicker',ProgPath+'resources/xcomponents/xcolorpicker.pas');
  ResourceToFile('xdatepicker',ProgPath+'resources/xcomponents/xdatepicker.pas');
  ResourceToFile('ximage',ProgPath+'resources/xcomponents/ximage.pas');
  ResourceToFile('xgroupbox',ProgPath+'resources/xcomponents/xgroupbox.pas');
  ResourceToFile('xmenu',ProgPath+'resources/xcomponents/xmenu.pas');
  ResourceToFile('xcode',ProgPath+'resources/xcomponents/xcode.pas');
  ResourceToFile('xstore',ProgPath+'resources/xcomponents/xstore.pas');
  ResourceToFile('xiframe',ProgPath+'resources/xcomponents/xiframe.pas');
  ResourceToFile('xsvgcontainer',ProgPath+'resources/xcomponents/xsvgcontainer.pas');
  ResourceToFile('xbitmap',ProgPath+'resources/xcomponents/xbitmap.pas');
  ResourceToFile('xtrapevents',ProgPath+'resources/xcomponents/xtrapevents.pas');
  ResourceToFile('xhtmltext',ProgPath+'resources/xcomponents/xhtmltext.pas');
  ResourceToFile('xhtmleditor',ProgPath+'resources/xcomponents/xhtmleditor.pas');
  ResourceToFile('utilsjscompile',ProgPath+'resources/xcomponents/utilsjscompile.pas');
  ResourceToFile('lazsutils',ProgPath+'resources/xcomponents/lazsutils.pas');
  ResourceToFile('eventsinterface',ProgPath+'resources/xcomponents/eventsinterface.pas');
  ResourceToFile('pastedialogunit',ProgPath+'resources/xcomponents/pastedialogunit.pas');
  ResourceToFile('compilerlogunit',ProgPath+'resources/xcomponents/compilerlogunit.pas');

(*  // files needed for web-pas2jscompiler to be compilable by pas2js, and built into the project JS file...
  ResourceToFile('fppas2js',ProgPath+'resources/pas2jstranspiler/fppas2js.pp');
  ResourceToFile('fppjssrcmap',ProgPath+'resources/pas2jstranspiler/fppjssrcmap.pp');
  ResourceToFile('pas2jscompiler',ProgPath+'resources/pas2jstranspiler/pas2jscompiler.pp');
  ResourceToFile('fpjson',ProgPath+'resources/pas2jstranspiler/fpjson.pp');                       // used by filecache
  ResourceToFile('pas2jsfilecache',ProgPath+'resources/pas2jstranspiler/pas2jsfilecache.pp');
  ResourceToFile('pas2jsfiler',ProgPath+'resources/pas2jstranspiler/pas2jsfiler.pp');
  ResourceToFile('pas2jsfileutils',ProgPath+'resources/pas2jstranspiler/pas2jsfileutils.pp');
  ResourceToFile('pas2jsfileutilsnodejs',ProgPath+'resources/pas2jstranspiler/pas2jsfileutilsnodejs.inc');
  ResourceToFile('pas2jslogger',ProgPath+'resources/pas2jstranspiler/pas2jslogger.pp');
  ResourceToFile('pas2jspparser',ProgPath+'resources/pas2jstranspiler/pas2jspparser.pp');
  ResourceToFile('pas2js_defines',ProgPath+'resources/pas2jstranspiler/pas2js_defines.inc');
  ResourceToFile('nodejsfs',ProgPath+'resources/pas2jstranspiler/nodejsfs.pas');
  ResourceToFile('contnrs',ProgPath+'resources/pas2jstranspiler/contnrs.pas');
  ResourceToFile('nodejs',ProgPath+'resources/pas2jstranspiler/nodejs.pas');
  ResourceToFile('jstree',ProgPath+'resources/pas2jstranspiler/jstree.pp');
  ResourceToFile('jswriter',ProgPath+'resources/pas2jstranspiler/jswriter.pp');
  ResourceToFile('jsbase',ProgPath+'resources/pas2jstranspiler/jsbase.pp');
  ResourceToFile('jssrcmap',ProgPath+'resources/pas2jstranspiler/jssrcmap.pas');
  ResourceToFile('jstoken',ProgPath+'resources/pas2jstranspiler/jstoken.pp');
  ResourceToFile('pscanner',ProgPath+'resources/pas2jstranspiler/pscanner.pp');
  ResourceToFile('pparser',ProgPath+'resources/pas2jstranspiler/pparser.pp');
  ResourceToFile('pastree',ProgPath+'resources/pas2jstranspiler/pastree.pp');
  ResourceToFile('pasresolver',ProgPath+'resources/pas2jstranspiler/pasresolver.pp');
  ResourceToFile('pasuseanalyzer',ProgPath+'resources/pas2jstranspiler/pasuseanalyzer.pp');
  ResourceToFile('pasresolveeval',ProgPath+'resources/pas2jstranspiler/pasresolveeval.pas');
  ResourceToFile('web',ProgPath+'resources/pas2jstranspiler/web.pas');
  ResourceToFile('pas2jsutils',ProgPath+'resources/pas2jstranspiler/pas2jsutils.pp');
  ResourceToFile('pas2jsfs',ProgPath+'resources/pas2jstranspiler/pas2jsfs.pp');
  ResourceToFile('webfilecache',ProgPath+'resources/pas2jstranspiler/webfilecache.pp');
  ResourceToFile('pas2jswebcompiler',ProgPath+'resources/pas2jstranspiler/pas2jswebcompiler.pp');
*)
  ResourceToFile('dfltImage','dfltImage.gif');
end;

procedure InitialiseCompilerResources(ProgramName,ProgPath:string);
begin
  // make the RTL and other source files available
  WriteResourceFiles(ProgramName,ProgPath);
end;

procedure TranspileMyProgram(ProgramName,ProgPath,ProjectPath:string; UseCodeEditor:TXCode; NoOptimise:Boolean; ExtraDirectives:TStringList);
// Compile a .pas file to generate a .js file.
// This does the cross-compile by running the internal pas2js compiler
var

  myParams:TStringList;
  MyCompilerObj :TMyCompilerObj;
  i: Integer;
  TheStream : TFileStream;
begin

  // set up runtime parameters for pas2js
  myParams:=TStringList.Create;
  //ProjectDirectory+'pas2js -dJScript -Jc -Jirtl.js '+ProgramName+'.pas'
  myParams.Add('-dJScript');
  {$ifdef Chromium}
  myParams.Add('-dChromium');
  {$endif}
  if ExtraDirectives<>nil then
    for i:=0 to ExtraDirectives.Count-1 do
      myParams.Add(ExtraDirectives[i]);
  // required for compilation of embedded pas2jscompiler
  myParams.Add('-Tnodejs');
  myParams.Add('-Pecmascript5');
  myParams.Add('-MobjFPC');
  myParams.Add('-Sc');
  myParams.Add('-Jeutf-8');
  if NoOptimise then myParams.Add('-O-');           //  Disable optimizations

  for i:=0 to RequiredFolders.Count-1 do
  begin
    myParams.Add('-Fi'+ProgPath+RequiredFolders[i]);
    myParams.Add('-Fu'+ProgPath+RequiredFolders[i]);
  end;
  //-Jminclude
  myParams.Add('-Tbrowser');

  // other parameters...
  myParams.Add('-Jc');
  myParams.Add('-Jirtl.js');
  myParams.Add('-l');
  myParams.Add('-vwnhe');
  myParams.Add('-Fu'+ProgPath+'resources/project');
  myParams.Add('-Fu'+ProgPath+'resources/xcomponents');
  myParams.Add('-Fu'+ProgPath+'resources/rtl');
  myParams.Add('-Fu'+ProgPath+'resources/pas2jstranspiler');
  myParams.Add('-Fu'+ProgPath+'tempinc');
  myParams.Add('-Fi'+ProgPath+'tempinc');
  myParams.Add('-FU'+ProgPath);
//  myParams.Add('-iW');   // show full version
  myParams.Add(ProgPath+ProjectPath+ProgramName+'.pas');

  DeleteFile(ProgPath+ProgramName+'.js');

   try

     MyCompilerObj := TMyCompilerObj.Create;
     MyCompilerObj.Compiler:=TPas2jsCompiler.Create;
     MyCompilerObj.CurrentCodeEditor:=UseCodeEditor;
     ExitCode:=0;

     {$ifndef JScript}
     if UseCodeEditor<>nil then
       MyCompilerObj.Compiler.Log.OnLog:=@MyCompilerObj.DoLogForevents
     else
     begin
       if  ProjectCompilerLogMemo<>nil then
         ProjectCompilerLogMemo.Lines.Clear;
       MyCompilerObj.Compiler.Log.OnLog:=@MyCompilerObj.DoLogForProject;
     end;
     {$else}
     MyCompilerObj.Compiler.Log.OnLog:=@MyCompilerObj.DoLog;
     {$endif}


     try
       //..........................................
       MyCompilerObj.Compiler.Run(ParamStr(0),ProgPath,myParams);
       //..........................................
     except
       on E: ECompilerTerminate do
       begin
         writeln(E.Message);
       end;
       on E: Exception do
       begin
         {AllowWriteln}
         writeln(E.Message);
         {AllowWriteln-}
       end;
     end;
   finally

     MyCompilerObj.Compiler.Free;    // this releases all the .inc files etc so we can freely load them into code editors.
     MyCompilerObj.Free;

  end;
   myParams.free;
end;

function CreateHTMLWrapper(ProgramName,DeployMode:String; embedJS:Boolean; JSString:String):String;
var
  BatchString,IFrameMessageHandler,docTitle:String;
begin

//  AsyncDelayFunc:=
//  'function asyncDelay(fn,msec,...args) {  '  +LineEnding
//  +'  async function dodelay(fn1,ms,...args) {  '  +LineEnding
//  +'    let promise = new Promise((resolve, reject) => { '  +LineEnding
//  +'      setTimeout(() => resolve("done!"), ms) '  +LineEnding
//  +'    }); '  +LineEnding
//  +'    fn(...args); '  +LineEnding
//  +'    let result = await promise; // wait till the promise resolves (*) '  +LineEnding
//  +'  '  +LineEnding
////  +'    alert(result); // "done!" '  +LineEnding
//  +'} '  +LineEnding
////  +'alert(fn); '  +LineEnding
////  +'alert(...args); '  +LineEnding
//  +'dodelay(fn,msec,...args); '  +LineEnding
//  +'}';


  docTitle:=UIRootNode.GetAttribute('SystemName',true).AttribValue;
  if docTitle='' then
    docTitle:=ProgramName;

  BatchString:= '<!DOCTYPE HTML>'  +LineEnding
    +'<html  lang="en">'  +LineEnding;

  BatchString:=BatchString + BuildHTMLHead(ProgramName,docTitle,DeployMode,JSString);
  BatchString:=BatchString + BuildHTMLBody;


  result:=batchstring;
end;


procedure CompileJSandExecute(ProjectPath:String; ExtraDirectives,ExtraHTML:TStringList);
var
  ProgramName,DefaultFontString,HTMLString:String;
  i:integer;
  TheLines:TStringList;
begin
  TheLines:=TStringList.Create;

  MainFormHeight:=MainForm.height;
  MainFormWidth:=MainForm.width;

  ProgramName:=MainUnitName;


    // Save the system definition data for the compiler
    SaveSystemToIncFile;

    // now cross compile from a saved copy of this source with the conditional define
    // switch (JScript) set to compile the JS version instead of the Lazarus version
    TranspileMyProgram(ProgramName,ProjectDirectory,ProjectPath,nil,false, ExtraDirectives);   //'resources/project/'


    // if the JS file exists, run the html file on browser...
    if FileExists(ProjectDirectory+ProgramName+'.js') then      // transpiled project code, plus pas2js rtl
    begin
      TheLines.Clear;
      TheLines.LoadFromFile(ProjectDirectory+ProgramName+'.js');
      TheLines.insert(0,'    <script type="application/javascript" >');
      TheLines.add('   </script>  ');

      if ExtraHTML<>nil then                             // eg. pyodide load script
        for i:=0 to ExtraHTML.Count-1 do
          TheLines.insert(i,ExtraHTML[i]);


      HTMLString:=CreateHTMLWrapper(ProgramName,'FromLaz',true,TheLines.Text);
      WriteToFile(ProgramName+'.html',HTMLString);

      OpenDocument(ProjectDirectory+ProgramName+'.html');
    end;

    FreeAndNil(TheLines);
end;
procedure CompileJSandExecute(ProjectPath:String);
begin
  CompileJSandExecute(ProjectPath,nil,nil);
end;

{$endif}




begin
    {$ifndef JScript}
    ProjectDirectory:= ExtractFilePath(Application.ExeName);
    {$endif}
    RequiredFolders:=TStringList.Create;
end.

