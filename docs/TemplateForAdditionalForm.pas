unit MyFormUnit;

{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
Classes, SysUtils, StringUtils, NodeUtils, XForm,
{$ifndef JScript}
Forms, Controls, Dialogs, LazsUtils,
{$else}
HTMLUtils,
{$endif}
WrapperPanel, EventsInterface, XVBox, XHBox, XMemo, XButton,
XLabel, XEditBox;


type

  { TMyForm }
  // ............ Define the form, based on type TXForm ...................
  TMyForm = class(TXForm)
  {.....XComponents added here.....}
    MyTopVBox: TXVBox;

    {$ifndef JScript}
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    {$endif}
  {.... event handlers added here ....}
    procedure DummyHandler(e:TEventStatus, nodeID: AnsiString; myValue: AnsiString);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MyForm: TMyForm;

implementation

{$R *.lfm}

{ TMyForm }

{$ifndef JScript}
procedure TMyForm.FormCreate(Sender: TObject);
begin
  myNode:=DoXFormCreated(self);
end;

procedure TMyForm.FormResize(Sender: TObject);
begin
  // call DoFormResize, passing in the top level container in the form...
  DoFormResize(self, MyTopVBox);
end;
{$endif}

procedure TMyForm.DummyHandler(e:TEventStatus, nodeID: AnsiString; myValue: AnsiString);
begin
end;


end.
