unit Example3Unit;

{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,
  {$ifndef JScript}
  FileUtil, Forms, Controls, Graphics, Dialogs, LCLIntf,
  ExtCtrls, Menus, ComCtrls, StdCtrls, TypInfo, LazIDEIntf, LResources,
  Types, IpHtml, Ipfilebroker,
  CompilerLogUnit,  LazsUtils,
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
  XBitMap, XTrapEvents, XHTMLText, XHTMLEditor, XSVGContainer;


{$ifdef JScript}
procedure InitialisePage(dummy:string);
{$endif}

{ TExample3Form }


type
TExample3Form = class(TXForm)

  {$ifndef JScript}
  // Lazarus-only Form components...
  WebMenu: TMenuItem;
  CompileToJS: TMenuItem;
  CompilerShowLog: TMenuItem;
  {$endif}
  Example3FormMainMenu: TXMainMenu;
  MyRootDiv: TXScrollBox;
  XButton1: TXButton;
  XEditBox1: TXEditBox;
  XHBox1: TXHBox;
  XIFrame1: TXIFrame;
  XMemo1: TXMemo;

  {$ifndef JScript}
  // Lazarus-only methods...
  procedure CompilerShowLogClick(Sender: TObject);
  procedure CompileToJSClick(Sender: TObject);
  procedure FormCreate(Sender: TObject);
  procedure FormResize(Sender: TObject);
  {$endif}

  // Common Event Handlers - created at design time along with X components...
  procedure DummyPositionMarker;   // Do not delete this line.
  procedure XButton1HandleButtonClick(e: TEventStatus; nodeID: AnsiString;
    myValue: AnsiString);

private
  { private declarations }
public
  { public declarations }
end;

var
Example3Form: TExample3Form;

implementation

{$R *.lfm}


procedure TExample3Form.DummyPositionMarker;     // do not delete this procedure
begin
end;

procedure TExample3Form.XButton1HandleButtonClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
  XIFrame1.HTMLSource:=XEditBox1.ItemValue;
end;

{$ifndef JScript}


procedure TExample3Form.FormCreate(Sender: TObject);
begin
  MainForm:=self;
  MainFormTopControl:=MyRootDiv;
  SystemNodeTree.ScreenObject:=nil;       // root node has no screen object.

  myNode:=DoXFormCreated(self);

  InitialiseXComponentsProject;
  InitialiseCompilerResources('Example3Unit',ProjectDirectory);

  XMemo1.ItemValue:=LineEnding +
  'URL Examples: ' + LineEnding +
  LineEnding +
  'https://www.lazarus-ide.org/' + LineEnding +
  'https://www.google.com/maps/' + LineEnding +
  'https://www.google.com/maps/embed' + LineEnding +
  'http://www.bbc.co.uk/news/' + LineEnding +
  'http://www.bbc.co.uk/news/embed/' + LineEnding +
  'http://www.bbc.co.uk/news/technology-35731734/embed' + LineEnding;
end;

procedure TExample3Form.CompileToJSClick(Sender: TObject);
begin
  CompileJSandExecute('');
  if not FileExists('Example3Unit.js') then
    ShowCompilerLog;
end;

procedure TExample3Form.CompilerShowLogClick(Sender: TObject);
begin
  ShowCompilerLog;
end;

procedure TExample3Form.FormResize(Sender: TObject);
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

  MainForm:=Example3Form;
  UIRootNode.MyForm:=nil;

  // this include file contains the system description to be loaded at startup.
  {$I systemnodetree.inc}
  XMLToNodeTree(LoadedSystemString,UIRootNode);   //! has been saved by the 'Run in Browser' menu button

  StartingUp:=false;
end;


{$endif}


begin
    MainUnitName:='Example3Unit';
    {$ifndef JScript}
    Application.ShowHint:=true;
    {$I rtl.lrs}
    {$Else}
       asm
       try{
          // now do any Javascript specific start up code
          pas.HTMLUtils.addHandVBoxStyles();
          }catch(err) { alert(err.message+' in StartupCode');}
       end;
    {$endif}

end.

