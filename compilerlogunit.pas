unit CompilerLogUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, UtilsJSCompile, XEditBox, XVBox, NodeUtils;
  //CompileUserCode;

type

  { TCompilerLogForm }

  TCompilerLogForm = class(TForm)
    CompilerPages: TPageControl;
    CompilerConfigPage: TTabSheet;
    CompilerConfigLabel: TLabel;
    CompilerLogMemo: TMemo;
    CompilerLogPage: TTabSheet;
//    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  CompilerLogForm: TCompilerLogForm;

procedure SetupCompilerLogForm;
procedure ShowCompilerLog;
//procedure ShowCompilerConfigPage;
//procedure LoadConfig;
//procedure LoadLog;

implementation

//{$R *.lfm}


procedure LoadData;
begin
//  LoadLog;
  //LoadConfig;
end;

procedure ShowCompilerLog;
begin
//  LoadData;
  CompilerLogForm.Show;
  CompilerLogForm.CompilerPages.ActivePageIndex:=1;

end;


{ TCompilerLogForm }


procedure SetupCompilerLogForm;
begin
  // Create the form
    CompilerLogForm:=TCompilerLogForm.CreateNew(MainForm);
    CompilerLogForm.Name:='CompilerLogForm';
    CompilerLogForm.Caption:='CompilerLogForm';
    CompilerLogForm.Top:=97;
    CompilerLogForm.Left:=332;
    CompilerLogForm.Height:=467;
    CompilerLogForm.Width:=693;

    CompilerLogForm.CompilerPages:=TPageControl.Create(CompilerLogForm);
    CompilerLogForm.CompilerPages.Name:='CompilerPages';
    CompilerLogForm.CompilerPages.Parent:=CompilerLogForm;
    CompilerLogForm.CompilerPages.Align:=alClient;

    CompilerLogForm.CompilerConfigPage:=TTabSheet.Create(CompilerLogForm);
    CompilerLogForm.CompilerConfigPage.Name:='CompilerConfigPage';
    CompilerLogForm.CompilerConfigPage.Parent:=CompilerLogForm.CompilerPages;
    CompilerLogForm.CompilerConfigPage.Caption:='Config';

    CompilerLogForm.CompilerConfigLabel:=TLabel.Create(CompilerLogForm);
    CompilerLogForm.CompilerConfigLabel.Name:='CompilerConfigLabel';
    CompilerLogForm.CompilerConfigLabel.Parent:=CompilerLogForm.CompilerConfigPage;
    CompilerLogForm.CompilerConfigLabel.Caption:='N/A';
    CompilerLogForm.CompilerConfigLabel.Top:=23;
    CompilerLogForm.CompilerConfigLabel.Left:=93;
    CompilerLogForm.CompilerConfigLabel.Height:=15;
    CompilerLogForm.CompilerConfigLabel.Width:=112;


    CompilerLogForm.CompilerLogPage:=TTabSheet.Create(CompilerLogForm);
    CompilerLogForm.CompilerLogPage.Name:='CompilerLogPage';
    CompilerLogForm.CompilerLogPage.Parent:=CompilerLogForm.CompilerPages;
    CompilerLogForm.CompilerLogPage.Caption:='Pas2JS Log';

    CompilerLogForm.CompilerLogMemo:=TMemo.Create(CompilerLogForm);
    CompilerLogForm.CompilerLogMemo.Name:='CompilerLogMemo';
    CompilerLogForm.CompilerLogMemo.Parent:=CompilerLogForm.CompilerLogPage;
    CompilerLogForm.CompilerLogMemo.Align:=alClient;
    CompilerLogForm.CompilerLogMemo.ScrollBars:=ssBoth;

    ProjectCompilerLogMemo:=CompilerLogForm.CompilerLogMemo;

end;

end.


