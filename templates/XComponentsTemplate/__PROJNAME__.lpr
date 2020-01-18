program __PROJNAME__;

{$mode objfpc}{$H+}
{$apptype gui}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  {$ifdef linux}
  cthreads,
  {$endif}
  {$ifdef Chromium}
  uCEFApplication, uCEFConstants,
  extctrls,
  {$endif}
  Interfaces, // this includes the LCL widgetset
  Forms, __MAINUNIT__,
  CEFXUtils;

{$R *.res}

{$ifdef Chromium}
{$IFDEF MSWINDOWS}
  // CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
  {$SetPEFlags $20}
{$ENDIF}
const
//  CEFLibDir:String = 'C:\cef4\FrameworkDir';
  CEFLibDir:String = '__CEF4Path__';
{$endif}

begin
  {$ifdef Chromium}
  SetupCEF4(CEFLibDir);

  if GlobalCEFApp.StartMainProcess then
  {$endif}
  begin
    RequireDerivedFormResource:=True;
    Application.Initialize;
    Application.CreateForm(T__MAINFORM__, __MAINFORM__);
    Application.Run;
  end;
  {$ifdef Chromium}
  CloseCEF4;
  {$endif}
end.


