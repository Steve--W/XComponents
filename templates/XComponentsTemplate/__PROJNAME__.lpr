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
  uCEFApplication,
  {$endif}
  Interfaces, // this includes the LCL widgetset
  Forms, __MAINUNIT__;

{$R *.res}

{$ifdef Chromium}
const
//  CEFLibDir:String = 'C:\cef4\FrameworkDir';
  CEFLibDir:String = '__CEFFrameworkDir__';
{$endif}

begin
  {$ifdef Chromium}
  GlobalCEFApp := TCefApplication.Create;
  GlobalCEFApp.FrameworkDirPath:=CEFLibDir;
  GlobalCEFApp.ResourcesDirPath:=CEFLibDir;
  GlobalCEFApp.LocalesDirPath:=CEFLibDir+'\locales';
  GlobalCEFApp.EnableGPU := True;                    // Enable hardware acceleration
  InitialiseCefMessaging;

  if GlobalCEFApp.StartMainProcess then
  {$endif}
  begin
    RequireDerivedFormResource:=True;
    Application.Initialize;
    Application.CreateForm(T__MAINFORM__, __MAINFORM__);
    Application.Run;
  end;
  {$ifdef Chromium}
  GlobalCEFApp.Free;
  GlobalCEFApp := nil;
  {$endif}
end.


