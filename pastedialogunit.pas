unit PasteDialogUnit;
interface
uses
  Classes, SysUtils,
  {$ifndef JScript}
  LazsUtils, Dialogs,
  {$endif}
  WrapperPanel,StringUtils, EventsInterface, XForm, XVBox, XMemo, XLabel, XButton, NodeUtils;

type TpasteDialogEvents = class(TObject)
  procedure PasteDoneBtnHandleButtonClick(e: TEventStatus;
    nodeID: AnsiString; myValue: AnsiString);
end;

var
  PasteDialog: TXForm;
  CompletionEvent:TEventStatus;
  PasteTarget:TXMemo;
  PasteDialogEvents:TPasteDialogEvents;
  PasteDoneBtn:TXButton;
  PasteLabel:TXLabel;

  procedure SetupPasteDialogForm;

implementation

procedure TPasteDialogEvents.PasteDoneBtnHandleButtonClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
var
  StringToCopy:String;
begin
  StringToCopy:=PasteTarget.ItemValue;
  {$ifdef JScript}
  asm
     pas.NodeUtils.myCopyToClip('HTML System',StringToCopy);
  end;
  {$endif}
  PasteDialog.Showing:='No';
end;

procedure SetupPasteDialogForm;
var
  FormNode:TdataNode;
  VBNode,MemoNode,BtnNode,LabelNode:TdataNode;
  VB:TXVBox;
begin
  // Create the form
    {$ifndef JScript}
    PasteDialog:=TXForm.CreateNew(MainForm);
    PasteDialog.Name:='PasteDialog';
    FormNode:=CreateFormNode(PasteDialog);
    {$else}
    FormNode:=AddDynamicWidget('TXForm',nil,nil,'PasteDialog','','Left',-1);
    PasteDialog:=TXForm(FormNode);
    AddChildToParentNode(UIRootNode,FormNode,-1);
    {$endif}
    PasteDialog.Caption:='PasteDialog';
    PasteDialog.Top:=214;
    PasteDialog.Left:=900;
    PasteDialog.Height:=106;
    PasteDialog.Width:=320;
    FormNode.IsDynamic:=false;              // so it's not deleted on system clear.

    VBNode:=AddDynamicWidget('TXVBox',PasteDialog,PasteDialog.myNode,'Popup1Root','','Left',-1);
    VBNode.IsDynamic:=false;              // so it's not deleted on system clear.
    VB:=TXVBox(VBNode.ScreenObject);
    VB.ContainerHeight:='100%';

    LabelNode:=AddDynamicWidget('TXLabel',PasteDialog,VBNode,'PasteLabel','','Left',-1);
    LabelNode.IsDynamic:=false;              // so it's not deleted on system clear.
    PasteLabel:=TXLabel(LabelNode.ScreenObject);
    PasteLabel.LabelCaption:='Waiting for a copy/paste action';

    MemoNode:=AddDynamicWidget('TXMemo',PasteDialog,VBNode,'PasteTarget','','Left',-1);
    MemoNode.IsDynamic:=false;              // so it's not deleted on system clear.
    PasteTarget:=TXMemo(MemoNode.ScreenObject);
    PasteTarget.MemoHeight:='40';
    PasteTarget.MemoWidth:='200';

    BtnNode:=AddDynamicWidget('TXButton',PasteDialog,VBNode,'PasteDoneBtn','','Left',-1);
    BtnNode.IsDynamic:=false;              // so it's not deleted on system clear.
    PasteDoneBtn:=TXButton(BtnNode.ScreenObject);
    PasteDoneBtn.myNode:=BtnNode;
    PasteDoneBtn.Caption:='Done';
    PasteDoneBtn.myNode.registerEvent('ButtonClick',@PasteDialogEvents.PasteDoneBtnHandleButtonClick);

end;
begin
  PasteDialogEvents:=TPasteDialogEvents.Create;
end.

