unit __MAINUNIT__;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}
interface

uses
  Classes, SysUtils,
  {$ifndef JScript}
  FileUtil, Forms, Controls, Graphics, Dialogs, LCLIntf, LMessages,
  ExtCtrls, Menus, ComCtrls, StdCtrls, TypInfo, LazIDEIntf, LResources,
  ColorBox, Types, IpHtml, Ipfilebroker,
  CompilerLogUnit,  LazsUtils,
  {$ifdef Chromium}
  uCEFApplication, uCEFInterfaces, uCEFConstants, uCEFTypes,
  uCEFProcessMessage, uCEFMiscFunctions,uCEFDOMVisitor,
  {$endif}
{$else}
  HTMLUtils,
{$endif}
  // XComponents units...
  Events, EventsInterface,
  StringUtils, NodeUtils, PasteDialogUnit,
  UtilsJSCompile, XIFrame, XMenu, XScrollBox, XVBox, XHBox, XTree, XMemo,
  XTabControl, XButton, XLabel, XEditBox, XCheckBox, XHyperLink, XRadioBtns,
  XForm, XTable, XProgressBar, XNumericSlider, XNumberSpinner,
  XComboBox, XDatePicker, XColorPicker, XImage, XGroupBox, XCode, XStore,
  XBitMap, XTrapEvents, XHTMLText, XHTMLEditor;


{$ifdef JScript}
procedure InitialisePage(dummy:string);
{$endif}

{ T__MAINFORM__ }


type
T__MAINFORM__ = class(TXForm)

  {$ifndef JScript}
  // Lazarus-only Form components...
  WebMenu: TMenuItem;
  CompileToJS: TMenuItem;
  CompilerShowLog: TMenuItem;
  {$endif}
  __MAINFORM__MainMenu: TXMainMenu;
  MyRootDiv: TXScrollBox;

  {$ifndef JScript}
  // Lazarus-only methods...
  procedure CompilerShowLogClick(Sender: TObject);
  procedure CompileToJSClick(Sender: TObject);
  procedure FormCreate(Sender: TObject);
  procedure FormResize(Sender: TObject);
  {$endif}

  // Common Event Handlers - created at design time along with X components...
  procedure DummyPositionMarker;   // Do not delete this line.

private
  { private declarations }
public
  { public declarations }
end;

{$ifndef JScript}
{$ifdef Chromium}
type TGlbObject = class(TObject)
procedure GlobalCEFApp_OnProcessMessageReceived(const browser       : ICefBrowser;
                                                      sourceProcess : TCefProcessId;
                                                const message       : ICefProcessMessage;
                                                var   aHandled      : boolean);
end;
procedure InitialiseCEFMessaging;
var
  GlbObject:TGlbObject;
{$endif}
{$endif}

var
__MAINFORM__: T__MAINFORM__;

implementation

{$R *.lfm}

{$ifndef JScript}
{$ifdef Chromium}
function SimpleNodeSearch(const aDocument: ICefDomDocument; NodeType,LookFor:String):String;
var
  TempNode : ICefDomNode;
  str:String;
  i:integer;
begin
  try
    if (aDocument <> nil) then
    begin
      TempNode := aDocument.GetElementById(LookFor);
      if (TempNode <> nil) then
        begin
          //CefLog('CEF4Delphi', 1, CEF_LOG_SEVERITY_ERROR, LookFor + ' element name : ' + TempNode.Name);
          //CefLog('CEF4Delphi', 1, CEF_LOG_SEVERITY_ERROR, LookFor + ' element value : ' + TempNode.GetValue);
          if NodeType='TXHTMLEditor' then
          begin
            str:=TempNode.AsMarkup;
            // for TXHTMLEditor, have to extract the inner html from the div...
            i:=FoundString(str,'>');
            if i>0 then Delete(str,1,i);
            i:=FoundString(str,'</div>');
            if i>0 then Delete(str,i,6);
            result:=str;
          end;
        end;
    end;
  except
    on e : exception do
      if CustomExceptionHandler('SimpleNodeSearch', e) then raise;
  end;
end;

procedure DOMVisitor_OnDocAvailable_TXHTMLEditor(const browser: ICefBrowser; const document: ICefDomDocument);
var
  msg: ICefProcessMessage;
  txt:String;
begin
  // This function is called from a different process.
  // document is only valid inside this function.

  // Simple DOM search to find the html text editor content
  txt:=SimpleNodeSearch(document,'TXHTMLEditor','my_wysiwyg_editor');

  // Send back results to the browser process
  // Notice that the XHTMLEDITOR_SEND_TEXT message name needs to be recognized in
  // Chromium OnProcessMessageReceived method
  msg := TCefProcessMessageRef.New(XHTMLEDITOR_SEND_TEXT);
  msg.ArgumentList.SetString(0, txt);
  browser.SendProcessMessage(PID_BROWSER, msg);
end;

procedure TGlbObject.GlobalCEFApp_OnProcessMessageReceived(const browser       : ICefBrowser;
                                                      sourceProcess : TCefProcessId;
                                                const message       : ICefProcessMessage;
                                                var   aHandled      : boolean);
var
  TempFrame   : ICefFrame;
  TempVisitor : TCefFastDomVisitor2;
begin
  aHandled := False;
  if (browser <> nil) then
    begin
      CefLog('XIDE OnProcessMessageReceived. ', 1, CEF_LOG_SEVERITY_ERROR, message.name);
      if (message.name = XHTMLEDITOR_GETTEXT) then
        begin
          TempFrame := browser.MainFrame;

          if (TempFrame <> nil) then
            begin
              TempVisitor := TCefFastDomVisitor2.Create(browser, @DOMVisitor_OnDocAvailable_TXHTMLEditor);
              TempFrame.VisitDom(TempVisitor);
            end;
          aHandled := True;
      end;
    end;
end;

procedure InitialiseCEFMessaging;
begin
  GlbObject:=TGlbObject.Create;
  GlobalCEFApp.OnProcessMessageReceived := @GlbObject.GlobalCEFApp_OnProcessMessageReceived;
end;
{$endif}
{$endif}

procedure T__MAINFORM__.DummyPositionMarker;     // do not delete this procedure
begin
end;

{$ifndef JScript}


procedure T__MAINFORM__.FormCreate(Sender: TObject);
begin
  MainForm:=self;
  MainFormTopControl:=MyRootDiv;
  SystemNodeTree.ScreenObject:=nil;       // root node has no screen object.

  myNode:=DoXFormCreated(self);

  InitialiseXComponentsProject;
  InitialiseCompilerResources('__MAINUNIT__',ProjectDirectory);
end;

procedure T__MAINFORM__.CompileToJSClick(Sender: TObject);
begin
  CompileJSandExecute('');
  if not FileExists('__MAINUNIT__.js') then
    ShowCompilerLog;
end;

procedure T__MAINFORM__.CompilerShowLogClick(Sender: TObject);
begin
  ShowCompilerLog;
end;

procedure T__MAINFORM__.FormResize(Sender: TObject);
begin
  DoFormResize(self, MyRootDiv);
end;


{$else}
procedure InitialisePage(dummy:string);
begin

  StartingUp:=true;// suppress event handlers while starting up

  // this include file contains create statements for all the interface objects in main form and popup forms
  // Xform nodes are added as children of UIRootNode.
  {$I systemintface.inc}

  MainForm:=__MAINFORM__;
  UIRootNode.MyForm:=nil;

  // this include file contains the system description to be loaded at startup.
  {$I systemnodetree.inc}
  XMLToNodeTree(LoadedSystemString);   //! has been saved by the 'Run in Browser' menu button

  StartingUp:=false;
end;


{$endif}


begin
    MainUnitName:='__MAINUNIT__';
    {$ifndef JScript}
    {$I __RTLDir__/rtl.lrs}
    {$Else}
       asm
       try{
          // now do any Javascript specific start up code
          pas.HTMLUtils.addHandVBoxStyles();
          }catch(err) { alert(err.message+' in StartupCode');}
       end;
    {$endif}

end.

