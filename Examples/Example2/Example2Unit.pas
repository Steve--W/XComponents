unit Example2Unit;

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
  XBitMap, XTrapEvents, XThreads, XGPUCanvas,XHTMLText, XHTMLEditor;


{$ifdef JScript}
procedure InitialisePage(dummy:string);
{$endif}

{ TExample2Form }

type
TExample2Form = class(TXForm)

  {$ifndef JScript}
  // Lazarus-only Form properties...
  WebMenu: TMenuItem;
  CompileToJS: TMenuItem;
  CompilerShowLog: TMenuItem;
  {$endif}
  MainMenu1: TXMainMenu;
  MyRootDiv: TXScrollBox;
  XButton1: TXButton;
  XButton2: TXButton;
  XButton3: TXButton;
  XButton4: TXButton;
  XButton5: TXButton;
  XButton6: TXButton;
  XButton7: TXButton;
  XButton8: TXButton;
  XButton9: TXButton;
  XCheckBox1: TXCheckBox;
  XGroupBox1: TXGroupBox;
  XHBox1: TXHBox;
  XHBox2: TXHBox;
  XLabel1: TXLabel;
  XLabel2: TXLabel;
  XScrollBox1: TXScrollBox;
  XScrollBox2: TXScrollBox;
  XVBox1: TXVBox;
  XVBox2: TXVBox;
  XVBox3: TXVBox;
  XVBox4: TXVBox;

  {$ifndef JScript}
  // Lazarus-only methods...
  procedure CompilerShowLogClick(Sender: TObject);
  procedure CompileToJSClick(Sender: TObject);
  procedure MyRootDivHandleClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  procedure FormCreate(Sender: TObject);
  procedure FormResize(Sender: TObject);
  {$endif}

  // Common Event Handlers - created at design time along with X components...
  procedure DummyPositionMarker;   // DO not delete this line.

private
  { private declarations }
public
  { public declarations }
end;


var
Example2Form: TExample2Form;

implementation

{$R *.lfm}

procedure TExample2Form.DummyPositionMarker;     // do not delete this procedure
begin
end;

{$ifndef JScript}

procedure TExample2Form.FormCreate(Sender: TObject);
begin
  MainForm:=self;
  MainFormTopControl:=MyRootDiv;
  SystemNodeTree.ScreenObject:=nil;       // root node has no screen object.

  myNode:=DoXFormCreated(self);

  InitialiseXComponentsProject;
  InitialiseCompilerResources('Example2',ProjectDirectory);
end;


procedure TExample2Form.CompileToJSClick(Sender: TObject);
begin
  CompileJSandExecute('');
  if not FileExists('Example2Unit.js') then
    ShowCompilerLog;
end;

procedure TExample2Form.CompilerShowLogClick(Sender: TObject);
begin
  ShowCompilerLog;
end;

procedure TExample2Form.MyRootDivHandleClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
end;

procedure TExample2Form.FormResize(Sender: TObject);
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

  MainForm:=Example2Form;
  UIRootNode.MyForm:=nil;

  // this include file contains the system description to be loaded at startup.
  {$I systemnodetree.inc}
  XMLToNodeTree(LoadedSystemString);   //! has been saved by the 'Run in Browser' menu button

  StartingUp:=false;
end;


{$endif}

begin
    MainUnitName:='Example2Unit';
    {$ifndef JScript}
    {$I C:/Xcomponents/resources/rtl.lrs}
    {$Else}
       asm
       try{
          // now do any Javascript specific start up code
          pas.HTMLUtils.addHandVBoxStyles();
          }catch(err) { alert(err.message+' in StartupCode');}
       end;
    {$endif}

end.

