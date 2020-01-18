unit __MAINUNIT__;
//-------------------------------------------------------------------------
// After creating a new project from this template, do the following
// BEFORE attempting to add components to the form:
//
// 1) Project Inspector ---- Add requirement for package XComponents.
// 2) Project Options   ---- Config & Target : Check the -WG checkbox.
// 3) Project Options   ---- Custom Options/Conditionals : Add the line:
//                                IncPath += ';$PkgDir(XComponents)';    // location of rtl.lrs
// 4) IF you have installed XComponents to include CEF4 capability, then
//    Project Options   ---- Custom Options/Conditionals : Add the line:
//                                UnitPath += ';$PkgOutDir(CEF4Delphi_Lazarus)';
// 5) IF you wish to use CEF4 capability in this project, then
//    Project Options   ---- Custom Options: insert directive   -dChromium
//--------------------------------------------------------------------------
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
  XBitMap, XTrapEvents, XHTMLText, XHTMLEditor;


{$ifdef JScript}
procedure InitialisePage(dummy:string);
{$endif}

{ T__MAINFORM__ }


type
T__MAINFORM__ = class(TXForm)

  __MAINFORM__MainMenu: TXMainMenu;
  MyRootDiv: TXScrollBox;

  {$ifndef JScript}
  // Lazarus/Desktop-only Form components...
  WebMenu: TMenuItem;
  CompileToJS: TMenuItem;
  CompilerShowLog: TMenuItem;
  {$endif}

  {$ifndef JScript}
  // Lazarus/Desktop-only methods...
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

var
__MAINFORM__: T__MAINFORM__;

implementation

{$R *.lfm}


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
  XMLToNodeTree(LoadedSystemString,UIRootNode);   //! has been saved by the 'Run in Browser' menu button

  StartingUp:=false;
end;


{$endif}


begin
    MainUnitName:='__MAINUNIT__';
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

