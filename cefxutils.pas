unit CEFXUtils;

(*
==================================================================================================
Note - Structure required for messaging from inside cef4 component out to the parent application:

1)  In Javascript, do an action that will raise an event that is handled in the chromium component.
    eg.  - change document title --> ontitlechange event handler
         - write to console (console.log) --> OnConsoleMessage event handler

2)  In the cef4 container component, handle the event in an OnConsoleMessage or OnTitleChange handler.
    In this handler, you can use 'normal' pascal application functions.
    If access to widgets or data inside the cef frame is needed, use SendProcessMessage(PID_RENDERER, .....), and ...

    3)  In the global app, handle the message in the OnProcessMessageReceived handler
        Here, you can use VisitDom to initiate access to the cef frame's internal document components.

    4)  In an OnDocAvailable handler, access the required internal document data (eg. see SimpleNodeSearch below)
        To send back the required data, Use SendProcessMessage(PID_BROWSER, .....)

    5)  In the cef4 container component, handle the message in an OnProcessMessageReceived handler.
        Here, use 'normal' application functions
======================================================================================================
*)
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,
  {$ifndef JScript}
  {$ifdef Chromium}
  uCEFApplication, uCEFInterfaces, uCEFConstants, uCEFTypes,
  uCEFProcessMessage, uCEFMiscFunctions,uCEFDOMVisitor,
  XHTMLEditor,
  {$endif}
  {$endif}
  NodeUtils,StringUtils,LazsUtils,TypInfo;

{$ifndef JScript}
{$ifdef Chromium}

procedure SetupCEF4(CEFLibDir:String);
procedure CloseCEF4;

procedure InitialiseCEFMessaging;
procedure GlobalCEFApp_OnProcessMessageReceived(const browser       : ICefBrowser;
                                                const frame         : ICefFrame;
                                                      sourceProcess : TCefProcessId;
                                                const message       : ICefProcessMessage;
                                                var   aHandled      : boolean);

{$endif}
{$endif}

implementation

{$ifndef JScript}
{$ifdef Chromium}
(*procedure SimpleDOMIteration(const aDocument: ICefDomDocument);
var
  TempHead, TempChild : ICefDomNode;
begin
  try
    if (aDocument <> nil) then
      begin
        TempHead := aDocument.Head;

        if (TempHead <> nil) then
          begin
            TempChild := TempHead.FirstChild;

            while (TempChild <> nil) do
              begin
                //CefLog('CEF4Delphi', 1, CEF_LOG_SEVERITY_ERROR, 'Head child element : ' + TempChild.Name);
                TempChild := TempChild.NextSibling;
              end;
          end;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('SimpleDOMIteration', e) then raise;
  end;
end;
*)

// NB. These cef messaging functions relate to the workings of the XHTMLEditor component, which is defined in XComponents.
// However, they will not work unless they are placed here in the main project unit...  (!! i think this is now ok here in a project unit...)
function SimpleNodeSearch(const aDocument: ICefDomDocument; NodeType,LookFor:String):String;
var
  TempNode : ICefDomNode;
  str:String;
  i:integer;
  function ExtractInnerHTML:String;
  begin
    str:=TempNode.AsMarkup;   //  ElementInnerText; ??
    // extract the inner html from the div...
    i:=FoundString(str,'>');
    if i>0 then Delete(str,1,i);
    if str[1]=chr(10) then Delete(str,1,1);
    i:=FoundString(str,'</div>');
    if i>0 then Delete(str,i,6);
    if str[length(str)]=chr(10) then Delete(str,length(str),1);
    result:=str;
  end;

begin
  // SimpleNodeSearch looks for the named element within the DOM for the cef4 component.
  try
    if (aDocument <> nil) then
    begin
     // CefLog('SimpleNodeSearch', 1, CEF_LOG_SEVERITY_INFO, LookFor );
      TempNode := aDocument.GetElementById(LookFor);
      if (TempNode <> nil) then
        begin
         // CefLog('SimpleNodeSearch', 1, CEF_LOG_SEVERITY_INFO, 'found' );
          //CefLog('CEF4Delphi', 1, CEF_LOG_SEVERITY_ERROR, LookFor + ' element value : ' + TempNode.GetValue);
          if NodeType='TXHTMLEditor' then
          begin
            result:=ExtractInnerHTML;
          end
          else if (NodeType='TXGPUCanvas') then
          begin
            result:=ExtractInnerHTML;
          end;
        end
      else
        CefLog('SimpleNodeSearch', 1, CEF_LOG_SEVERITY_INFO, 'not found' );
    end;
  except
    on e : exception do
      if CustomExceptionHandler('SimpleNodeSearch', e) then raise;
  end;
end;

procedure DOMVisitor_OnDocAvailable_TXHTMLEditor(const browser: ICefBrowser; const frame: ICefFrame; const document: ICefDomDocument);
var
  msg: ICefProcessMessage;
  txt:String;
begin
  // This function is called from a different process.
  // 'document' is only valid inside this function.
  // As an example, this function only writes the document title to the 'cefdebug.log' file.
 // CefDebugLog('document.Title : ' + document.Title);

  //if document.HasSelection then
  //  CefLog('CEF4Delphi', 1, CEF_LOG_SEVERITY_ERROR, 'document.SelectionAsText : ' + quotedstr(document.SelectionAsText))
  // else
  //  CefLog('CEF4Delphi', 1, CEF_LOG_SEVERITY_ERROR, 'document.HasSelection : False');

  // Simple DOM iteration example
  //SimpleDOMIteration(document);

  // Simple DOM searches
  txt:=SimpleNodeSearch(document,'TXHTMLEditor','my_wysiwyg_editor');

  // Send back results to the browser process
  // Notice that the XHTMLEDITOR_SEND_TEXT message name needs to be recognized in
  // Chromium OnProcessMessageReceived method
  msg := TCefProcessMessageRef.New(XHTMLEDITOR_SEND_TEXT);
  //msg.ArgumentList.SetString(0, 'document.Title : ' + document.Title);
  msg.ArgumentList.SetString(0, txt);
  //browser.SendProcessMessage(PID_BROWSER, msg);
  frame.SendProcessMessage(PID_BROWSER, msg);
end;

procedure DOMVisitor_OnDocAvailable_TXGPUCanvas(const browser: ICefBrowser; const frame: ICefFrame; const document: ICefDomDocument);
var
  msg: ICefProcessMessage;
  otxt,stxt,actxt:String;
begin
  otxt:=SimpleNodeSearch(document,'TXGPUCanvas','oarr');
  stxt:=SimpleNodeSearch(document,'TXGPUCanvas','sarr');
  actxt:=SimpleNodeSearch(document,'TXGPUCanvas','acdiv');
  CefLog('sending sendGPUarrays1', 1, CEF_LOG_SEVERITY_INFO, '' );
  //CefDebugLog('sending sendGPUarrays. sendGPUarrays1 ');
  // Send back results to the browser process
  // Notice that the 'sendGPUarrays' message name needs to be recognized in
  // a Chromium OnProcessMessageReceived method
  msg := TCefProcessMessageRef.New('sendGPUarrays1');
  msg.ArgumentList.SetString(0, otxt);
  msg.ArgumentList.SetString(1, stxt);
  msg.ArgumentList.SetString(2, actxt);
  frame.SendProcessMessage(PID_BROWSER, msg);
end;
procedure DOMVisitor_OnDocAvailable_TXGPUCanvasD(const browser: ICefBrowser; const frame: ICefFrame; const document: ICefDomDocument);
var
  msg: ICefProcessMessage;
  otxt,stxt,actxt:String;
begin
  otxt:=SimpleNodeSearch(document,'TXGPUCanvas','oarr');
  stxt:=SimpleNodeSearch(document,'TXGPUCanvas','sarr');
  actxt:=SimpleNodeSearch(document,'TXGPUCanvas','acdiv');
  CefLog('sending sendGPUarrays2', 1, CEF_LOG_SEVERITY_INFO, '' );
  //CefDebugLog('sending sendGPUarrays. sendGPUarrays2 '+stxt);
  // Send back results to the browser process
  // Notice that the 'sendGPUarrays' message name needs to be recognized in
  // a Chromium OnProcessMessageReceived method
  msg := TCefProcessMessageRef.New('sendGPUarrays2');
  msg.ArgumentList.SetString(0, otxt);
  msg.ArgumentList.SetString(1, stxt);
  msg.ArgumentList.SetString(2, actxt);
  frame.SendProcessMessage(PID_BROWSER, msg);
end;
procedure DOMVisitor_OnDocAvailable_TXGPUCanvas2(const browser: ICefBrowser; const frame: ICefFrame; const document: ICefDomDocument);
var
  msg: ICefProcessMessage;
  actxt:String;
begin
  actxt:=SimpleNodeSearch(document,'TXGPUCanvas','acdiv');

  // Send back results to the browser process
  // Notice that the 'sendGPUcounter' message name needs to be recognized in
  // a Chromium OnProcessMessageReceived method
  msg := TCefProcessMessageRef.New('sendGPUcounter');
  msg.ArgumentList.SetString(0, actxt);
  frame.SendProcessMessage(PID_BROWSER, msg);
end;

procedure GlobalCEFApp_OnProcessMessageReceived(const browser       : ICefBrowser;
                                                const frame       : ICefFrame;
                                                      sourceProcess : TCefProcessId;
                                                const message       : ICefProcessMessage;      // params: id, attrib ????
                                                var   aHandled      : boolean);
var
  TempFrame   : ICefFrame;
  TempVisitor : TCefFastDomVisitor2;
  a:integer;
  arg:String;
  thisNode:TDataNode;
  m:TMethod;
begin
  // Handle messages that are sent INTO this cef renderer
  aHandled := False;
  if (browser <> nil) then
    begin
      //CefLog('CEFXUtils OnProcessMessageReceived. ', 1, CEF_LOG_SEVERITY_ERROR, message.name);
      //CefDebugLog('CEFXUtils OnProcessMessageReceived.'+message.name);
      if (message.name = XHTMLEDITOR_GETTEXT) then
        begin
          //TempFrame := browser.MainFrame;
          TempFrame := frame;

          if (TempFrame <> nil) then
            begin
              TempVisitor := TCefFastDomVisitor2.Create(browser, frame, @DOMVisitor_OnDocAvailable_TXHTMLEditor);
              TempFrame.VisitDom(TempVisitor);
            end;
          aHandled := True;
        end
      else if (message.name = 'getGPUData1')  then
      begin
        TempFrame:=frame;
        if TempFrame<>nil then
        begin
          TempVisitor := TCefFastDomVisitor2.Create(browser, frame, @DOMVisitor_OnDocAvailable_TXGPUCanvas);
          TempFrame.VisitDom(TempVisitor);
        end;
        aHandled := True;
      end
      else if (message.name = 'getGPUData2')  then
      begin
        TempFrame:=frame;
        if TempFrame<>nil then
        begin
          TempVisitor := TCefFastDomVisitor2.Create(browser, frame, @DOMVisitor_OnDocAvailable_TXGPUCanvasD);
          TempFrame.VisitDom(TempVisitor);
        end;
        aHandled := True;
      end
      else if (message.name = 'getGPUCounter')  then
      begin
        TempFrame:=frame;
        if TempFrame<>nil then
        begin
          TempVisitor := TCefFastDomVisitor2.Create(browser, frame, @DOMVisitor_OnDocAvailable_TXGPUCanvas2);
          TempFrame.VisitDom(TempVisitor);
        end;
        aHandled := True;
      end;

//      a:=message.ArgumentList.GetSize;
//      CefDebugLog('message.ArgumentList.GetSize='+inttostr(a));
//      if message.ArgumentList<>nil then
//      begin
//        arg := message.ArgumentList.GetString(0);
//        CefDebugLog('message.ArgumentList.GetString(0)='+arg);
//        thisNode:=findDataNodeById(systemnodetree,arg,'',true);
//        if HasProperty(thisNode.ScreenObject,'DOMVisitor_OnDocAvailable') then
//        begin
//          m:=GetMethodProp(thisNode.ScreenObject,'DOMVisitor_OnDocAvailable');
//        end;
//      end;


    end;
end;

procedure InitialiseCEFMessaging;
begin
  GlobalCEFApp.OnProcessMessageReceived := @GlobalCEFApp_OnProcessMessageReceived;
end;

procedure SetupCEF4(CEFLibDir:String);
begin
GlobalCEFApp := TCefApplication.Create;
GlobalCEFApp.FrameworkDirPath:=CEFLibDir;
GlobalCEFApp.ResourcesDirPath:=CEFLibDir;
GlobalCEFApp.LocalesDirPath:=CEFLibDir+'\locales';
GlobalCEFApp.EnableGPU := True;                    // Enable hardware acceleration
DeleteFile('cefdebug.log');
GlobalCEFApp.LogFile              := 'cefdebug.log';
GlobalCEFApp.LogSeverity          := LOGSEVERITY_INFO;  //LOGSEVERITY_VERBOSE;

GlobalCEFApp.WindowlessRenderingEnabled := True;
// cef update GlobalCEFApp.EnableHighDPISupport       := True;
GlobalCEFApp.DisableFeatures            := 'NetworkService,OutOfBlinkCors';

InitialiseCefMessaging;
end;

procedure CloseCEF4;
begin
  GlobalCEFApp.Free;
  GlobalCEFApp := nil;
end;

{$endif}
{$endif}

end.


