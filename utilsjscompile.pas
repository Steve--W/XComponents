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
  Classes, SysUtils, StringUtils, XCode,
  {$ifndef JScript}
  Forms,StdCtrls,Dialogs,LCLIntf, Pas2JSCompiler, Pas2JSLogger, Pas2JSFileUtils, LResources, URIParser;
  //Process,
  {$else}
  webfilecache, pas2jswebcompiler;
  {$endif}

procedure WriteIncFile(Compiler:TObject;IncName,EventType,IncPath:String;
                       var MainCode:TStringList;IncCode:TStringList);
function LoadIncludeFile(Compiler:TObject;FileName,IncPath:String):TStringList;
{$ifndef JScript}
procedure AddRequiredFile(ResourceName,ResourcePath:String);
procedure InitialiseCompilerResources(ProgramName,ProgPath:string);
function CreateHTMLWrapper(ProgramName,DeployMode:String; embedJS:Boolean; JSString:String):String;
procedure CompileMyProgram(ProgramName,ProgPath,ProjectPath:string; UseCodeEditor:TXCode; NoOptimise:Boolean);
procedure CompileJSandExecute(ProjectPath:String);
procedure ResourceToFile(resName,fileName:string);
procedure WriteResourceFiles(ProgramName,ProgPath:String);
function ResourceToString(resName:string):String;
procedure WriteRTLIncFile(filepath,filename,suffix:String);  overload;
procedure WriteRTLIncFile(filepath,filename:String);  overload;
procedure WriteRTLIncFiles;

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
{$else}
type
    TWebCompilerObj = Class(TObject)
    Private
      FCompiler : TPas2JSWebCompiler;
      FCodeEditor:TXCode;
      procedure OnUnitLoaded(Sender: TObject; aFileName: String; aError: string);
    Protected
    Public
      procedure DoLog(Sender: TObject; const Msg: String);
      Constructor Create;
      property myCodeEditor:TXCode read FCodeEditor write FCodeEditor;
      property Compiler:TPas2JSWebCompiler read FCompiler write FCompiler;
    end;

    var MyWebCompiler:TWebCompilerObj;

      JSOutput:String;

      eventsinterfacepas:String;
      interfacetypespas:String;

      classespas:String;
      contnrspas:String;
      dateutilspas:String;
      jspas:String;
      mathpas:String;
      rtlconstspas:String;
      rttipas:String;
      strutilspas:String;
      systempas:String;
      sysutilspas:String;
      typespas:String;
      typinfopas:String;

//systempas,sysutilspas,rtlconstspas,jspas,typespas,classespas:String;
//      browserconsolepas:String;
//      class2paspas:String;
//      hotreloadclientpas:String;
//      libjquerypas:String;
//      nodejspas:String;
//      objpaspas:String;
//      timerpas:String;
//      webpas:String;
//      webaudiopas:String;
//      webbluetoothpas:String;
//      webglpas:String;
//      webrouterpas:String;


procedure LoadRTLFilesForPas2JS(lWebFS : TPas2JSWebFS);    //TWebCompilerObj);

{$endif}
var
  RequiredFolders:TStringList;
  gpujs:String;                 // contents of resource file gpu.js


implementation
{$ifndef JScript}
uses LazsUtils, NodeUtils;
{$endif}


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

function ResourceToString(resName:string):String;
var
  Stream: TLazarusResourceStream;
  Lines:TStringList;
begin
  Stream := nil;
  try
    Lines:=TStringList.Create;
    //find the lazarus resource
    Stream := TLazarusResourceStream.Create(resName, nil);

    //save to a stringlist
    Lines.LoadFromStream(Stream);
    result:=Lines.Text;
   finally
     Stream.Free;
     Lines.Free;
   end;
end;
procedure ResourceToFile(resName,fileName:string);
var
  str:String;
begin
  str:=ResourceToString(resName);
  WriteToFile(fileName,str);
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
  ResourceToFile('rtti',ProgPath+'resources/rtl/rtti.pas');
  ResourceToFile('strutils',ProgPath+'resources/rtl/strutils.pas');
  ResourceToFile('system',ProgPath+'resources/rtl/system.pas');
  ResourceToFile('sysutils',ProgPath+'resources/rtl/sysutils.pas');
  ResourceToFile('types',ProgPath+'resources/rtl/types.pas');
  ResourceToFile('typinfo',ProgPath+'resources/rtl/typinfo.pas');

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
  ResourceToFile('xthreads',ProgPath+'resources/xcomponents/xthreads.pas');
  ResourceToFile('xgpucanvas',ProgPath+'resources/xcomponents/xgpucanvas.pas');
  ResourceToFile('xhtmltext',ProgPath+'resources/xcomponents/xhtmltext.pas');
  ResourceToFile('xhtmleditor',ProgPath+'resources/xcomponents/xhtmleditor.pas');
  ResourceToFile('utilsjscompile',ProgPath+'resources/xcomponents/utilsjscompile.pas');
  ResourceToFile('lazsutils',ProgPath+'resources/xcomponents/lazsutils.pas');
  ResourceToFile('eventsinterface',ProgPath+'resources/xcomponents/eventsinterface.pas');
  ResourceToFile('pastedialogunit',ProgPath+'resources/xcomponents/pastedialogunit.pas');
  ResourceToFile('compilerlogunit',ProgPath+'resources/xcomponents/compilerlogunit.pas');

  ResourceToFile('gpu',ProgPath+'resources/xcomponents/gpu.js');

  // files needed for web-pas2jscompiler itself to be compilable by pas2js, and built into the project JS file...
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

end;

procedure InitialiseCompilerResources(ProgramName,ProgPath:string);
begin
  // make the RTL and other source files available
  WriteResourceFiles(ProgramName,ProgPath);
  ResourceToFile('dfltImage','dfltImage.gif');
end;

procedure CompileMyProgram(ProgramName,ProgPath,ProjectPath:string; UseCodeEditor:TXCode; NoOptimise:Boolean);
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
end;

function CreateHTMLWrapper(ProgramName,DeployMode:String; embedJS:Boolean; JSString:String):String;
var
  BatchString,IFrameMessageHandler,docTitle, GPUMessageHandler:String;
begin
  IFrameMessageHandler:='  window.addEventListener("message", function(ev) { '+Lineending
                  + 'if (ev.data.objid==undefined) { '  +LineEnding
                  + '    pas.XSVGContainer.HandleMessage(ev.data); } '+lineEnding   // svg titlechange
                  + 'else { '+lineEnding
                  + '  if (ev.data.mtype=="titleChange") { '+lineEnding
                  + '    pas.XHTMLEditor.HandleTXHTMLMessage(ev.data);  '+lineEnding   // htmleditor text change
                  + '   } '
//                  + '  else { '
//                  + '    pas.XGPUCanvas.HandleGPUMessage(ev.data); '+lineEnding
//                  + '  } '
                  + '} });'  +LineEnding;

  docTitle:=UIRootNode.GetAttribute('SystemName',true).AttribValue;
  if docTitle='' then
    docTitle:=ProgramName;

  BatchString:= '<!DOCTYPE HTML>'  +LineEnding
    +'<html  lang="en">'  +LineEnding
    +'  <head >'  +LineEnding
    +'   <meta charset="utf-8"> '  +LineEnding
    +'   <title>'+docTitle+'</title>'  +LineEnding
    +'    <!_ load the javascript libraries _>'  +LineEnding

    +'    <script type="application/javascript" ';
    if embedJS then
      BatchString:=BatchString+'>'+JSString
    else
      BatchString:=BatchString+' src="'+ProgramName+'.js">';

    Batchstring:=BatchString + '/*Deployment1*/var myDeployedMode = '''+DeployMode+''';/*Deployment2*/'+LineEnding;

    BatchString:=BatchString+'</script>' +LineEnding
    +'    <Style> html {height: 100%;} ' +LineEnding
    +'            body { height:100%; min-height: 100%;} ' +LineEnding
    +'    </Style>' +LineEnding
    +'    <Style>  '  +LineEnding
    +'       @keyframes fadeIn { from { opacity: 0; }}  '  +LineEnding
    +'       .highlight-border { border:dashed 3px green; outline:none;}'  +LineEnding
    +'       .normal-border { border:solid 1px gray; outline:none;}'  +LineEnding
    +'       .no-border { border:none 1px gray; outline:none;}'  +LineEnding
    +'    </Style>  '  +LineEnding
    +'    <script > '  +LineEnding
    +'  var testnum = 666; ' + LineEnding
    +'          function  StartupCode(){ ' +LineEnding
    +'          try{'  +LineEnding
    +'             rtl.run("'+ProgramName+'"); ' +LineEnding
    +'             pas.'+ProgramName+'.InitialisePage();'  +LineEnding
    +IFrameMessageHandler
    +'             }catch(err) {alert("Error in StartupCode ---"+err.message);}; '  +LineEnding
    +'          };  '   +LineEnding
    +'    </script>  '   +LineEnding
    +'  </head> '  +LineEnding
    +'  <body style="margin:0px; font:normal 12px Verdana, Arial, sans-serif;"'
    +'        onload = " StartupCode();" ' +LineEnding
    +'> '  +LineEnding
    +'     <div  id = "'+SystemRootName+'" class="vbox" style="height:100%; width:100%;top:0px;left:0px; position:relative; z-index:0;"> '  +LineEnding
    +'       <div  id = "'+MainForm.Name+'" class="vbox" style="height:100%; width:100%;top:0px;left:0px"> '  +LineEnding
    +'       </div> '  +LineEnding
    +'     </div> '  +LineEnding
    +'  </body>'   +LineEnding
    +'</html> ' +LineEnding  ;

  result:=batchstring;
end;

procedure CompileJSandExecute(ProjectPath:String);
//  procedure CompileJSandExecute(fpcPath,ProjectPath:String);
var
  ProgramName,DefaultFontString,HTMLString:String;
  i:integer;
  TheLines:TStringList;
begin
  TheLines:=TStringList.Create;

  // save the config data
//  TheLines.Add(fpcPath);
//  TheLines.SaveToFile('config.dta');

  MainFormHeight:=MainForm.height;
  MainFormWidth:=MainForm.width;

  ProgramName:=MainUnitName;


    // Save the system definition data for the compiler
    SaveSystem(false);

    // now cross compile from a saved copy of this source with the conditional define
    // switch (JScript) set to compile the JS version instead of the Lazarus version
    // After compilation, run the HTML file
    CompileMyProgram(ProgramName,ProjectDirectory,ProjectPath,nil,false);   //'resources/project/'


    // if the JS file exists, run on browser...
    if FileExists(ProjectDirectory+ProgramName+'.js') then
    begin
      TheLines.Clear;
      TheLines.LoadFromFile(ProjectDirectory+ProgramName+'.js');
      HTMLString:=CreateHTMLWrapper(ProgramName,'FromLaz',true,TheLines.Text);
      WriteToFile(ProgramName+'.html',HTMLString);

      OpenDocument(ProjectDirectory+ProgramName+'.html');
    end;

    FreeAndNil(TheLines);
end;

procedure WriteRTLIncFile(filepath,filename,suffix:String);
var
  TheStream:TFileStream;
  Incname,tempText,oneLine:string;
  TxtLines:TStringList;
  i,j:integer;
  URI: TURI;
begin
  // Load up the required file
  TxtLines:=TStringList.Create;
  TxtLines.LoadFromFile(filepath+filename+'.'+suffix);

  // Set up a file stream for the target .inc file
  IncName:='tempinc/'+filename+suffix+'.inc';
  try
    TheStream:=TFileStream.Create(IncName,fmCreate or fmOpenRead or fmOpenWrite or fmShareDenyNone);
  except
    showmessage('arg!');
  end;


  if suffix='js' then
  begin
    TxtLines.Text:=''''+SubstituteSpecials(TxtLines.Text)+'''';
  end
  else
  begin
      // Substitute single-quotes in the text to be copied
      TxtLines.Text:=MyStringReplace(TxtLines.Text,'''','&myapos;',-1,-1);
      for i:=0 to TxtLines.Count-1 do
      begin
        TxtLines[i]:='+ '''+TxtLines[i]+'\n'' ' ;
      end;
  end;


  // Wrap the text with pascal code that will load it into a string variable.
  if suffix='js' then
  begin
    TxtLines.Insert(0,'pas.UtilsJSCompile.'+filename+suffix+' = ');
  end
  else
    TxtLines.Insert(0,'pas.UtilsJSCompile.'+filename+suffix+' = '''' ');
  TxtLines.Insert(0,'asm');
  TxtLines.Add(';');
  TxtLines.Add('end;');
  //... and will put back the original quote chars
  if suffix='js' then
  begin
  end
  else
  begin
    TxtLines.Add(filename+suffix+':=MyStringReplace('+filename+suffix+',''&myapos;'','''''''',-1,-1);');
  end;

  // Save the new .inc file
  TxtLines.SaveToStream(TheStream);

  TheStream.Free;
  TxtLines.Free;
end;

procedure WriteRTLIncFile(filepath,filename:String);  overload;
begin
  WriteRTLIncFile(filepath,filename,'pas');
end;

procedure WriteRTLIncFiles;
// Write files required by the pas2js compiler
begin

  // minimal required rtl set....
 // WriteRTLIncFile('resources/rtl/','system');
 // WriteRTLIncFile('resources/rtl/','rtlconsts');
 // WriteRTLIncFile('resources/rtl/','js');
 // WriteRTLIncFile('resources/rtl/','sysutils');
 // WriteRTLIncFile('resources/rtl/','types');
 // WriteRTLIncFile('resources/rtl/','classes');

  // common rtl set.....
  WriteRTLIncFile('resources/rtl/','classes');
  WriteRTLIncFile('resources/rtl/','contnrs');
  WriteRTLIncFile('resources/rtl/','dateutils');
  WriteRTLIncFile('resources/rtl/','js');
  WriteRTLIncFile('resources/rtl/','math');
  WriteRTLIncFile('resources/rtl/','rtlconsts');
  WriteRTLIncFile('resources/rtl/','rtti');
  WriteRTLIncFile('resources/rtl/','strutils');
  WriteRTLIncFile('resources/rtl/','system');
  WriteRTLIncFile('resources/rtl/','sysutils');
  WriteRTLIncFile('resources/rtl/','types');
  WriteRTLIncFile('resources/rtl/','typinfo');

  //  WriteRTLIncFile('resources/rtl/','timer');
  //  WriteRTLIncFile('resources/rtl/','nodejs');
  //  WriteRTLIncFile('resources/rtl/','objpas');
  //  WriteRTLIncFile('resources/rtl/','libjquery');
  //  WriteRTLIncFile('resources/rtl/','hotreloadclient');
  //  WriteRTLIncFile('resources/rtl/','class2pas');
  //  WriteRTLIncFile('resources/rtl/','browserconsole');
  //  WriteRTLIncFile('resources/rtl/','web');
  //  WriteRTLIncFile('resources/rtl/','webaudio');
  //  WriteRTLIncFile('resources/rtl/','webbluetooth');
  //  WriteRTLIncFile('resources/rtl/','webgl');
  //  WriteRTLIncFile('resources/rtl/','webrouter');

  WriteRTLIncFile('resources/xcomponents/','gpu','js');
end;


{$else}
constructor TWebCompilerObj.Create;
begin
  FCompiler:=TPas2JSWebCompiler.Create;
end;

procedure TWebCompilerObj.DoLog(Sender: TObject; const Msg: String);
begin
  if myCodeEditor<>nil then
    myCodeEditor.MessageLines:=myCodeEditor.MessageLines+LineEnding+Msg;
end;
procedure TWebCompilerObj.OnUnitLoaded(Sender: TObject; aFileName: String; aError: string);
begin
  if myCodeEditor<>nil then
    if aError='' then
      myCodeEditor.MessageLines:=myCodeEditor.MessageLines+LineEnding+'Loaded: '+aFileName
    else
      myCodeEditor.MessageLines:=myCodeEditor.MessageLines+LineEnding+'Error Loading "'+aFileName+'": '+AError;
end;

procedure LoadRTLFilesForPas2JS(lWebFS : TPas2JSWebFS);  //Compiler:TPas2JSWebCompiler);    //TWebCompilerObj);
begin
  asm
    // minimal required rtl set....
    //lWebFS.SetFileContent('system.pas',pas.UtilsJSCompile.systempas);
    //lWebFS.SetFileContent('sysutils.pas',pas.UtilsJSCompile.sysutilspas);
    //lWebFS.SetFileContent('classes.pas',pas.UtilsJSCompile.classespas);
    //lWebFS.SetFileContent('rtlconsts.pas',pas.UtilsJSCompile.rtlconstspas);
    //lWebFS.SetFileContent('js.pas',pas.UtilsJSCompile.jspas);
    //lWebFS.SetFileContent('types.pas',pas.UtilsJSCompile.typespas);

    // common rtl set....
    lWebFS.SetFileContent('classes.pas',pas.UtilsJSCompile.classespas);
    lWebFS.SetFileContent('contnrs.pas',pas.UtilsJSCompile.contnrspas);
    lWebFS.SetFileContent('dateutils.pas',pas.UtilsJSCompile.dateutilspas);
    lWebFS.SetFileContent('js.pas',pas.UtilsJSCompile.jspas);
    lWebFS.SetFileContent('math.pas',pas.UtilsJSCompile.mathpas);
    lWebFS.SetFileContent('rtlconsts.pas',pas.UtilsJSCompile.rtlconstspas);
    lWebFS.SetFileContent('rtti.pas',pas.UtilsJSCompile.rttipas);
    lWebFS.SetFileContent('strutils.pas',pas.UtilsJSCompile.strutilspas);
    lWebFS.SetFileContent('system.pas',pas.UtilsJSCompile.systempas);
    lWebFS.SetFileContent('sysutils.pas',pas.UtilsJSCompile.sysutilspas);
    lWebFS.SetFileContent('types.pas',pas.UtilsJSCompile.typespas);
    lWebFS.SetFileContent('typinfo.pas',pas.UtilsJSCompile.typinfopas);

  //  lWebFS.SetFileContent('timer.pas',pas.UtilsJSCompile.timerpas);
  //  lWebFS.SetFileContent('nodejs.pas',pas.UtilsJSCompile.nodejspas);
  //  lWebFS.SetFileContent('objpas.pas',pas.UtilsJSCompile.objpaspas);
  //  lWebFS.SetFileContent('libjquery.pas',pas.UtilsJSCompile.libjquerypas);
  //  lWebFS.SetFileContent('hotreloadclient.pas',pas.UtilsJSCompile.hotreloadclientpas);
  //  lWebFS.SetFileContent('class2pas.pas',pas.UtilsJSCompile.class2paspas);
  //  lWebFS.SetFileContent('browserconsole.pas',pas.UtilsJSCompile.browserconsolepas);
  //  lWebFS.SetFileContent('web.pas',pas.UtilsJSCompile.webpas);
  //  lWebFS.SetFileContent('webaudio.pas',pas.UtilsJSCompile.webaudiopas);
  //  lWebFS.SetFileContent('webbluetooth.pas',pas.UtilsJSCompile.webbluetoothpas);
  //  lWebFS.SetFileContent('webgl.pas',pas.UtilsJSCompile.webglpas);
  //  lWebFS.SetFileContent('webrouter.pas',pas.UtilsJSCompile.webrouterpas);
  end;
end;

{$endif}


procedure WriteIncFile(Compiler:TObject;IncName,EventType,IncPath:String;
                       var MainCode:TStringList;IncCode:TStringList);
var
{$ifndef JScript}
   TheStream:TFileStream;
   {$endif}
   FileName:string;
begin
  if EventType<>'' then IncName:=IncName+'__'+EventType;

  {$ifndef JScript}
  FileName:=IncPath+IncName+'.inc';

  try
    TheStream:=TFileStream.Create(Filename,fmCreate or fmOpenRead or fmOpenWrite or fmShareDenyNone);
    IncCode.SaveToStream(TheStream);
    TheStream.Free;
  except
    showmessage('Failed to create include file '+FileName);
  end;
  {$else}
  FileName:=IncName+'.inc';
  // save the generated inc file
  TPas2JSWebCompiler(Compiler).WebFS.SetFileContent(FileName,IncCode.Text);
  {$endif}

  MainCode.Add('{$I '+IncName+'.inc}');
end;

function LoadIncludeFile(Compiler:TObject;FileName,IncPath:String):TStringList;
var
  tmp1:TStringList;
  tmp:String;
  {$ifndef JScript}
  TheStream:TFileStream;
  {$endif}
begin
  tmp1:=TStringList.Create;
  {$ifndef JScript}
  try
  // find and load the include file...
  TheStream:=TFileStream.Create(IncPath+FileName,fmOpenRead or fmShareDenyNone);
  tmp1.LoadFromStream(TheStream);
  TheStream.Free;
  except
    try
    // try once more with a .inc suffix (FPC keeps this, pas2js doesn't (!?).....
    TheStream:=TFileStream.Create(IncPath+FileName+'.inc',fmOpenRead or fmShareDenyNone);
    tmp1.LoadFromStream(TheStream);
    FileName:=FileName+'.inc';
    TheStream.Free;
    except
      showmessage('file '+IncPath+FileName+' not available');
      tmp1.Clear;
    end;
  end;
  {$else}
 // tmp:=MyWebCompiler.Compiler.WebFS.GetFileContent(FileName);
  tmp:=TPas2JSWebCompiler(Compiler).WebFS.GetFileContent(FileName);
  tmp1.Text:=tmp;
  {$endif}
  result:=tmp1;
end;



begin
    {$ifndef JScript}
    ProjectDirectory:= ExtractFilePath(Application.ExeName);
    {$else}
    MyWebCompiler := TWebCompilerObj.Create;
    {$endif}
    RequiredFolders:=TStringList.Create;
end.

