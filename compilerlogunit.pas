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
//    CompilerConfigfpcPath: TEdit;
    CompilerConfigLabel: TLabel;
    CompilerLogMemo: TMemo;
    CompilerLogPage: TTabSheet;
//    procedure FormCreate(Sender: TObject);
  private
//    procedure CompilerConficfpcPathChanged(Sender:TObject);

  public

  end;

var
  CompilerLogForm: TCompilerLogForm;
  ConfigfpcPath:String;

procedure SetupCompilerLogForm;
procedure ShowCompilerLog;
//procedure ShowCompilerConfigPage;
//procedure LoadConfig;
//procedure LoadLog;

implementation

//{$R *.lfm}

function DefaultConfig:TStringList;
var
  cfg:TStringList;
begin
  cfg:=TStringList.Create;

  //cfg.Add('resources/xcomponents');       // Location of the units of the XComponents package
  {$ifdef windows}
  cfg.Add('C:/fpcupdeluxe/fpc/bin/i386-win32');   // Location of Lazarus installation (needed for Free Pascal compilation of user events dll)
  {$endif}
  {$ifdef linux}
  cfg.Add('/home/pi/fpcupdeluxe/fpc/bin/arm-linux');
  {$endif}
  result:=cfg;

end;

(*
procedure LoadConfig;
var
  TheStream : TFileStream;
  TheLines:TStringList;
//  ConfigXPath:String;
begin
  // pick up the last-used config file config.dta
  TheLines:=TStringList.Create;
  try
    TheStream:=TFileStream.Create(ExtractFilePath(Application.ExeName) +'config.dta',fmOpenRead or fmShareDenyNone);
    TheLines.LoadFromStream(TheStream);
    TheStream.Free;
  except
    TheLines.AddStrings(DefaultConfig,true);
  end;
  if TheLines.Count=1 then
  begin
//    ConfigXPath:=TheLines[0];
    ConfigfpcPath:=TheLines[0];
  end;
  CompilerLogForm.CompilerConfigfpcPath.Text:=ConfigfpcPath;
//  CompilerLogForm.CompilerConfigXComponentsPath.Text:=ConfigXPath;

  FreeAndNil(TheLines);
end;
*)

//procedure LoadLog;
//var
//  TheStream : TFileStream;
//begin
//  // pick up the messages produced by the compiler and display in the memo box
//  try
//    TheStream:=TFileStream.Create(ExtractFilePath(Application.ExeName) +'stdout.txt',fmOpenRead or fmShareDenyNone);
//    CompilerLogForm.CompilerLog.Lines.LoadFromStream(TheStream);
//    TheStream.Free;
//  except
//    CompilerLogForm.CompilerLog.Lines.Clear;
//  end;
//end;

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

//procedure ShowCompilerConfigPage;
//begin
//  LoadData;
//  CompilerLogForm.Show;
//  CompilerLogForm.CompilerPages.ActivePageIndex:=1;
//end;

{ TCompilerLogForm }

(*procedure TCompilerLogForm.FormCreate(Sender: TObject);
begin
  ProjectCompilerLogMemo:=CompilerLogMemo;
  LoadConfig;
//  InitialiseCompilerResources(CompilerLogForm.CompilerConfigXComponentsPath.Text,CompilerLogForm.CompilerConfigLazPath.Text);
//InitialiseCompilerResources('XIDE',CompilerLogForm.CompilerConfigLazPath.Text);
//InitialiseCompilerResources('XIDE',ProjectDirectory);

end;
*)

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

//    CompilerLogForm.CompilerConfigfpcPath:=TEdit.Create(CompilerLogForm);
//    CompilerLogForm.CompilerConfigfpcPath.Name:='CompilerConfigLazPath';
//    CompilerLogForm.CompilerConfigfpcPath.Parent:=CompilerLogForm.CompilerConfigPage;
//    CompilerLogForm.CompilerConfigfpcPath.Top:=16;
//    CompilerLogForm.CompilerConfigfpcPath.Left:=216;
//    CompilerLogForm.CompilerConfigfpcPath.Height:=23;
//    CompilerLogForm.CompilerConfigfpcPath.Width:=440;
//    CompilerLogForm.CompilerConfigfpcPath.OnChange:=@CompilerLogForm.CompilerConficfpcPathChanged;

    CompilerLogForm.CompilerConfigLabel:=TLabel.Create(CompilerLogForm);
    CompilerLogForm.CompilerConfigLabel.Name:='CompilerConfigLabel';
    CompilerLogForm.CompilerConfigLabel.Parent:=CompilerLogForm.CompilerConfigPage;
    CompilerLogForm.CompilerConfigLabel.Caption:='N/A';                   //'Path for fpc compiler';
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
//    LoadConfig;

end;

//procedure TCompilerLogForm.CompilerConficfpcPathChanged(Sender:TObject);
//begin
//  ConfigfpcPath:=CompilerLogForm.CompilerConfigfpcPath.Text;
//end;
end.


