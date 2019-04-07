unit Example1Unit;

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

{ TExample1Form  }

type
TExample1Form = class(TXForm)

  {$ifndef JScript}
  // Lazarus-only Form properties...
  WebMenu: TMenuItem;
  CompileToJS: TMenuItem;
  CompilerShowLog: TMenuItem;
  {$endif}
  MainMenu1: TXMainMenu;
  Example1FormMyRootDiv: TXScrollBox;
  Example1FormXButton1: TXButton;
  Example1FormXButton2: TXButton;
  Example1FormXCheckBox1: TXCheckBox;
  Example1FormXEditBox1: TXEditBox;
  Example1FormXHBox1: TXHBox;
  Example1FormXHyperLink1: TXHyperLink;
  Example1FormXLabel1: TXLabel;
  Example1FormXRadioBtns1: TXRadioBtns;
  Example1FormXRadioBtns2: TXRadioBtns;
  Example1FormXTabControl1: TXTabControl;
  Example1FormXVBox1: TXVBox;
  Example1FormXVBox2: TXVBox;

  XTabSheet1: TXTabSheet;
  XTabSheet2: TXTabSheet;


  {$ifndef JScript}
  // Lazarus-only methods...
  procedure CompilerShowLogClick(Sender: TObject);
  procedure CompileToJSClick(Sender: TObject);
  procedure FormActivate(Sender: TObject);
  procedure FormCreate(Sender: TObject);
  procedure FormResize(Sender: TObject);
  {$endif}

  // Common Event Handlers - created at design time along with X components...
  procedure DummyPositionMarker;   // DO not delete this line.
  procedure Example1FormXButton1HandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  procedure Example1FormXButton2HandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  procedure Example1FormXCheckBox1HandleClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  procedure Example1FormXRadioBtns1HandleChange(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  procedure Example1FormXRadioBtns2HandleChange(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);

private
  { private declarations }
public
  { public declarations }
end;


var
Example1Form: TExample1Form;

implementation

{$R *.lfm}


procedure TExample1Form.DummyPositionMarker;     // do not delete this procedure
begin
end;

procedure TExample1Form.Example1FormXButton1HandleButtonClick(e:TEventStatus;nodeID: AnsiString;
  myValue: AnsiString);
begin
  if Example1FormXEditBox1.ReadOnly = true then
  begin
    Example1FormXEditBox1.ReadOnly:=false;
    Example1FormXButton1.Caption:='Disable EditBox';
  end
  else
  begin
    Example1FormXEditBox1.ReadOnly:=true;
    Example1FormXButton1.Caption:='Enable EditBox';
  end;
end;

procedure TExample1Form.Example1FormXButton2HandleButtonClick(e:TEventStatus;nodeID: AnsiString;
  myValue: AnsiString);
begin
  if Example1FormXHyperLink1.IsVisible then
  begin
    Example1FormXHyperLink1.IsVisible:=false;
    Example1FormXButton2.Caption:='Show BBC Link';
  end
  else
  begin
    Example1FormXHyperLink1.IsVisible:=true;
    Example1FormXButton2.Caption:='Hide BBC Link';
  end;
end;

procedure TExample1Form.Example1FormXCheckBox1HandleClick(e:TEventStatus;nodeID: AnsiString;
  myValue: AnsiString);
begin
  if Example1FormXCheckBox1.Checked then
  begin
    Example1FormXHBox1.IsVisible:=false;
  end
  else
  begin
    Example1FormXHBox1.IsVisible:=true;
  end;
end;


procedure TExample1Form.Example1FormXRadioBtns1HandleChange(e:TEventStatus;nodeID: AnsiString;
  myValue: AnsiString);
begin
  {$ifdef JScript}
  if myValue='0' then
  begin
    Example1FormXVBox1.BgColor:='#8080FF';
  end
  else if myValue='1' then
    Example1FormXVBox1.BgColor:='#6666AA'
  else
    Example1FormXVBox1.BgColor:='#000077';
  {$else}
  if myValue='0' then
    Example1FormXVBox1.BgColor:=$FF8080
  else if myValue='1' then
    Example1FormXVBox1.BgColor:=$AA6666
  else
    Example1FormXVBox1.BgColor:=$770000;
  {$endif}
end;

procedure TExample1Form.Example1FormXRadioBtns2HandleChange(e:TEventStatus;nodeID: AnsiString;
  myValue: AnsiString);
begin
  {$ifdef JScript}
  if myValue='0' then
  begin
    Example1FormXVBox2.BgColor:='#FFFF00';
  end
  else if myValue='1' then
    Example1FormXVBox2.BgColor:='#999900'
  else
    Example1FormXVBox2.BgColor:='#444400';
  {$else}
  if myValue='0' then
    Example1FormXVBox2.BgColor:=$00FFFF
  else if myValue='1' then
    Example1FormXVBox2.BgColor:=$009999
  else
    Example1FormXVBox2.BgColor:=$004444;
  {$endif}
end;

{$ifndef JScript}

procedure InitialiseProjectResources;
var
  i:integer;
// define project-specific resources.
begin
  RequiredFolders.Clear;
  RequiredFolders.Add('resources/project');

  for i:=0 to RequiredFolders.Count-1 do
  begin
    if not DirectoryExists(ProjectDirectory+RequiredFolders[i])  then ForceDirectories(ProjectDirectory+RequiredFolders[i]);
  end;
  // files needed for this project to be compiled by pas2js, to generate the project JS file...
  CopyFile(ProjectDirectory+'Example1Unit.pas',ProjectDirectory+'resources\project\'+'Example1Unit.pas');

end;


{ TForm1 }

procedure TExample1Form.FormCreate(Sender: TObject);
begin
  MainForm:=self;
  MainFormTopControl:=Example1FormMyRootDiv;
  SystemNodeTree.ScreenObject:=nil;       // root node has no screen object.

  myNode:=DoXFormCreated(self);

  InitialiseXComponentsProject;
  InitialiseCompilerResources('Example1',ProjectDirectory);
end;

procedure TExample1Form.CompilerShowLogClick(Sender: TObject);
begin
  ShowCompilerLog;
end;

procedure TExample1Form.CompileToJSClick(Sender: TObject);
begin
  CompileJSandExecute('');
  if not FileExists('Example1Unit.js') then
    ShowCompilerLog;
end;

procedure TExample1Form.FormActivate(Sender: TObject);
begin
end;

procedure TExample1Form.FormResize(Sender: TObject);
begin
  DoFormResize(self, Example1FormMyRootDiv);
end;

{$else}
procedure InitialisePage(dummy:string);
begin
  StartingUp:=true;// suppress event handlers while starting up

  // this include file contains create statements for all the interface objects in main form and popup forms
  // XForm nodes are added as children of UIRootNode.
  {$I systemintface.inc}
  MainForm:=Example1Form;
  UIRootNode.MyForm:=nil;

  // this include file contains the system description to be loaded at startup.
  {$I systemnodetree.inc}
  XMLToNodeTree(LoadedSystemString);   //! has been saved by the CompileToJS menu button

  StartingUp:=false;

end;

{$endif}


begin

  {$ifndef JScript}
  {$I C:/Xcomponents/resources/rtl.lrs}
    MainUnitName:='Example1Unit';

  {$Else}
     asm
     try{
        // now do any Javascript specific start up code
        pas.HTMLUtils.addHandVBoxStyles();
        }catch(err) { alert(err.message+' in StartupCode');}
     end;
  {$endif}

end.

