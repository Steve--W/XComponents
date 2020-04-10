(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XTable;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface
uses
  Classes, SysUtils, TypInfo, NodeUtils, StringUtils,EventsInterface,
  {$ifndef JScript}
  fpjson,    jsonparser,
  LResources, Forms, Controls, ComCtrls, StdCtrls, Grids,Graphics, Dialogs, ExtCtrls, Propedits,RTTICtrls,
  LazsUtils,
  {$else}
  HTMLUtils,
  {$endif}
  WrapperPanel,Events;

{$ifdef JScript}
procedure AddTableStyles(dummy:string);
procedure TableChange(Sender:TObject;NodeName,NameSpace:String) ;
function CellClick(target:TObject; NodeName,NameSpace:String):String;
function KeyDown(target:TObject; NodeName,NameSpace:String):Boolean;
{$endif}

type TTableCellsArray = Array of Array of String;

{$ifndef JScript}
type TStringGridAccess = class(TStringGrid) end;
{$endif}

type
  TXTable = class(TWrapperPanel)
  private
    { Private declarations }
    {$ifndef JScript}
    fHandleClick:TEventHandler;

    procedure Tableclick(Sender:TObject);
    procedure TableChange(Sender:TObject);
    //procedure TableCellChange(Sender:TObject;ARow,ACol:longint;const AValue:String);
    procedure TableMouseUp(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X,Y:Longint);
    procedure TableMouseDown(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X,Y:Longint);
    //procedure CellSelected(Sender: TObject; aCol, aRow: Integer);
    {$endif}
    procedure SetMyEventTypes;

    function GetReadOnly:Boolean;
    function GetTableWidth:string;
    function GetTableHeight:string;
    function GetTableData:string;
    function GetColWidth:integer;
    function GetNumCols:integer;
    function GetNumRows:integer;
    function GetSelectedRow:integer;
    function GetSelectedCol:integer;
    function GetSelectedValue:String;
    function GetHasHeaderRow:Boolean;
    function GetIsNumeric:Boolean;
    function GetIncludeDataInSave:Boolean;

    procedure SetReadOnly(AValue:Boolean);
    procedure SetTableWidth(AValue:string);
    procedure SetTableHeight(AValue:string);
    procedure SetTableData(AValue:string);
    procedure SetColWidth(AValue:integer);
    procedure SetSelectedRow(AValue:integer);
    procedure SetSelectedCol(AValue:integer);
    procedure SetSelectedValue(AValue:String);
    procedure SetNumRows(AValue:integer);
    procedure SetNumCols(AValue:integer);
    procedure SetHasHeaderRow(AValue:Boolean);
    procedure SetIsNumeric(AValue:Boolean);
    procedure SetIncludeDataInSave(AValue:Boolean);
  protected
    { Protected declarations }
//    procedure LinkLoadFromProperty(Sender: TObject);  override;
//    procedure LinkSaveToProperty(Sender: TObject);  override;
    {$ifndef JScript}
    procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
    {$endif}
  public
    { Public declarations }
    {$ifndef JScript}
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent;IsDynamic:Boolean); override;
    {$else}
    constructor Create(MyForm:TForm;NodeName,NameSpace:String);
    {$endif}

    {$ifndef JScript}
    procedure PopulateMeFromJSONData(GridString:String);
    procedure PopulateStringGrid(gridData:String);
    procedure AddRowFromJSONData(rownum:integer; jData : TJSONData);
    {$endif}
    function ConstructDataString:String;
    function ConstructTableStringFromArray(myArray:TTableCellsArray):String;
    function ConstructTableStringFromNumArray(myArray:T2DNumArray):String;
    function ConstructTableStringFromExcelCopy(CopiedString:String):String;
    procedure LoadTableFromExcelCopy(CopiedString:String);
    procedure LoadTableFromNumArray(NumArray:T2DNumArray);
    function GetCellValue(row,col:integer):string;
    procedure SetCellValue(row,col:integer;AValue:string);
    function GetCellsAsArray(SkipHeader:Boolean):TTableCellsArray;
    procedure AddTableRows(numRows:integer);
    procedure AddTableColumns(numCols:integer);
    procedure DeleteRow(r:integer);
    procedure DeleteColumn(c:integer);
    procedure DeleteSelectedRow;
    procedure DeleteSelectedColumn;
    function QuoteCellForJSON(cellval:String; rownum:integer):String;
  published
    { Published declarations }
    // Properties defined for this class...
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property TableHeight: String read GetTableHeight write SetTableHeight;
    property TableWidth: String read GetTableWidth write SetTableWidth;
    property TableData:String read GetTableData write SetTableData;
    property ColWidth:Integer read GetColWidth write SetColWidth;
    property SelectedRow:integer read GetSelectedRow write SetSelectedRow;
    property SelectedCol:integer read GetSelectedCol write SetSelectedCol;
    property SelectedValue:String read GetSelectedValue write SetSelectedValue;
    property NumCols:integer read GetNumCols write SetNumCols;
    property NumRows:integer read GetNumRows write SetNumRows;
    property HasHeaderRow:Boolean read GetHasHeaderRow write SetHasHeaderRow;
    property IsNumeric:Boolean read GetIsNumeric write SetIsNumeric;
    property IncludeDataInSave:Boolean read GetIncludeDataInSave write SetIncludeDataInSave;

    {$ifndef JScript}
    // Events to be visible in Lazarus IDE
    property HandleClick: TEventHandler read FHandleClick write FHandleClick;
    {$endif}
  end;

{$ifndef JScript}
procedure Register;
{$endif}

implementation

const MyNodeType='TXTable';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXTable.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
  MyEventTypes.Add('Change');
end;

{$ifndef JScript}
procedure Register;
begin
  {$I xtable_icon.lrs}
  RegisterComponents('XComponents',[TXTable]);

end;

constructor TXTable.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXTable.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXTable.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  self.BorderSpacing.Around:=glbBorderSpacing;

  myControl:=TStringGrid.Create(self);
  myControl.Parent:=self;

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];

  TStringGrid(myControl).OnEditingDone:=@self.myeditingDone;
  myControl.OnClick:=@self.TableClick;
  //TStringGrid(myControl).OnSetEditText:=@self.TableCellChange;
  //TStringGrid(myControl).OnSelectCell:=@self.CellSelected;
  //TStringGrid(myControl).OnSelection:=@self.CellSelected;
  TStringGrid(myControl).OnExit:=@self.TableChange;
  TStringGrid(myControl).OnMouseDown:=@self.TableMouseDown;
  TStringGrid(myControl).OnMouseUp:=@self.TableMouseUp;

  self.SetMyEventTypes;

  CreateComponentDataNode2(self,MyNodeType,myDefaultAttribs, self.myEventTypes, TheOwner,IsDynamic);

  self.ParentColor:=true;
  // Setting IsContainer false will prevent designer dropping new child controls into this one.
  self.IsContainer:=false;

  TStringGrid(myControl).Options:=[goColSizing,goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine,goSmoothScroll,goEditing];
  //  default - scrollbars will appear as required

  AddLabel(myControl);


end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXTable',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;

procedure TXTable.TableChange(Sender:TObject) ;
var
  newData:String;
begin
  // Fired via OnExit event...
  //showmessage('tablechange event');
  // get the table data contents
  newData:=self.ConstructDataString;
  // update the node attribute value
  if newData<>self.TableData then
  begin
    //showmessage('tablechange event Set TableData '+newData);
    myNode.SetAttributeValue('TableData',newData,'TableString');
    // adjust the selected cell...
    if (self.SelectedCol>-1)
    and (self.SelectedRow>-1) then
    begin
      if TStringGrid(self.myControl).ColCount < self.SelectedCol+1 then
        self.SelectedCol:=TStringGrid(self.myControl).ColCount-1;
      if TStringGrid(self.myControl).RowCount < self.SelectedRow+1 then
        self.SelectedRow:=TStringGrid(self.myControl).RowCount-1;
      self.SelectedValue:=TStringGrid(self.myControl).Cells[self.SelectedCol,self.SelectedRow];
    end;
    CallHandleEvent('Change',newData,Sender);
  end;
end;

procedure TXTable.TableMouseUp(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X,Y:Longint);
var
  r,c:integer;
begin
  TStringGrid(self.myControl).SetFocus;
  TStringGrid(Sender).MouseToCell(X,Y,c,r);
  self.SelectedRow:=r;
  self.SelectedCol:=c;
  self.SelectedValue:=self.GetCellValue(r,c);
end;
procedure TXTable.TableMouseDown(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X,Y:Longint);
// added this because trouble with mousedown vs. click events (order of firing???)
var
  r,c:integer;
begin
  TStringGrid(Sender).MouseToCell(X,Y,c,r);
  self.SelectedRow:=r;
  self.SelectedCol:=c;
  self.SelectedValue:=self.GetCellValue(r,c);
end;

procedure TXTable.TableClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
    //CallHandleEvent('Click',self.myNode.NodeName,self);
    CallHandleEvent('Click',intToStr(self.SelectedRow)+','+intToStr(self.SelectedCol),self);
end;

procedure TXTable.SetTableWidth(AValue:string);
var
  tc:TControl;
begin
  tc:=self.myControl;
  myNode.SetAttributeValue('TableWidth',AValue);
  SetHeightWidth(self.myNode,tc,'TableWidth','TableHeight');

end;

procedure TXTable.SetTableHeight(AValue:string);
var
  tc:TControl;
begin
  tc:=self.myControl;
  myNode.SetAttributeValue('TableHeight',AValue);
  SetHeightWidth(self.myNode,tc,'TableWidth','TableHeight');
end;

procedure TXTable.SetColWidth(AValue:integer);
var
  i:integer;
begin
  myNode.SetAttributeValue('ColWidth',IntToStr(AValue));
  //showmessage('colcount='+inttostr(TStringGrid(myControl).ColCount));
  for i:=0 to TStringGrid(myControl).ColCount-1 do
      TStringGrid(myControl).ColWidths[i]:=AValue;
end;

procedure TXTable.AddRowFromJSONData(rownum:integer; jData : TJSONData);
var
  JItem : TJSONData;
  i:integer;
  object_type:string;
begin
  // 1-D data is expected...
  for i:=0 to jData.Count-1 do
  begin
    if TStringGrid(myControl).ColCount < (i+1) then
      TStringGrid(myControl).ColCount:=i+1;
    jItem := jData.Items[i];
    object_type := GetEnumName(TypeInfo(TJSONtype), Ord(jItem.JSONType));
    if (object_type='jtString')
    or (object_type='jtNumber') then
    begin
      TStringGrid(myControl).Cells[i,rownum] :=  jItem.AsString;
    end
    else if object_type='jtArray' then
    begin
      TStringGrid(myControl).Cells[i,rownum] :=  '[array]';   //!! nested arrays are not handled
    end;
  end;
end;

procedure TXTable.PopulateMeFromJSONData(GridString:String);
var
   jData : TJSONData;
   i,j,RowCount,cw:integer;
begin
  // 2-D data is expected...
  // non-numeric cells must be quoted.
  cw:=self.ColWidth;
  try
    jData := GetJSON(GridString);
  except
    on E: Exception do
    begin
      showmessage('JSON error: '+e.Message);
      showmessage(GridString);
      jData := nil;
    end;
  end;
  if jData<>nil then
  begin
    TStringGrid(myControl).ColCount:=1;
    rowcount:=jData.Count;
    TStringGrid(myControl).RowCount:= rowcount;
    for i :=0 to rowcount-1 do
    begin
      AddRowFromJSONData(i, jData.Items[i]);
      if i=0 then
        for j:=0 to TStringGrid(myControl).ColCount-1 do
          TStringGrid(myControl).ColWidths[j]:=cw;
    end;
    if self.HasHeaderRow then
      TStringGrid(myControl).FixedRows:=1
    else
      TStringGrid(myControl).FixedRows:=0;
  end
  else
    TStringGrid(myControl).ColCount:=0;

  TStringGrid(myControl).FixedCols:=0;
end;


procedure  TXTable.PopulateStringGrid(gridData:String);
begin
  self.PopulateMeFromJSONData(gridData);
end;

function TXTable.GetCellsAsArray(SkipHeader:Boolean):TTableCellsArray;
var
  i,j,r:integer;
  myArray:TTableCellsArray;
begin
  if SkipHeader then
    SetLength(myArray, TStringGrid(myControl).RowCount-1)
  else
    SetLength(myArray, TStringGrid(myControl).RowCount);
  r:=-1;
  for i:= 0 to TStringGrid(myControl).RowCount-1 do
  begin
    if (Skipheader=false)
    or (i>0) then
    begin
      r:=r+1;
      setlength(myArray[r],TStringGrid(myControl).ColCount);
      for j:=0 to TStringGrid(myControl).ColCount-1 do
      begin
        myArray[r,j]:=TStringGrid(myControl).Cells[j, i];
      end;
    end;
  end;
  result:=myArray;
end;

function TXTable.ConstructDataString:String;
// Generate the data in string form from the table cell values
var
    i,j:integer;
    dataStr:String;
begin
  dataStr:='[';
  for i:= 0 to TStringGrid(myControl).RowCount-1 do
  begin
    if i>0 then dataStr:=dataStr+',';
    dataStr:=dataStr+'[';
    for j:=0 to TStringGrid(myControl).ColCount-1 do
    begin
      if j>0 then dataStr:=dataStr+',';
      dataStr:=dataStr+QuoteCellForJSON(TStringGrid(myControl).Cells[j,i],i);
    end;
    dataStr:=dataStr+']';
  end;
  dataStr:=dataStr+']';
  result:=dataStr;
end;

function TXTable.GetNumCols:integer;
var
    c:integer;
begin
  c:=TStringGrid(myControl).ColCount;
  result:=c;
end;

function TXTable.GetNumRows:integer;
var
      r:integer;
begin
  r:=TStringGrid(myControl).RowCount;
  result:=r;
end;

function TXTable.GetCellValue(row,col:integer):string;
begin
  if (TStringGrid(myControl).RowCount>row)
  and (TStringGrid(myControl).ColCount>col) then
    result:=TStringGrid(myControl).Cells[col,row]
  else
    result:='';
end;

{$else}
constructor TXTable.Create(MyForm:TForm;NodeName,NameSpace:String);
begin
  //showmessage('create table node');
  inherited Create(NodeName,NameSpace);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=false;

  SetNodePropDefaults(self,myDefaultAttribs);
end;


procedure TableChange(Sender:TObject; NodeName,NameSpace:String) ;
var
  newData,olddata:String;
  myNode:TDataNode;
begin
  //showmessage('tablechange event. NodeName='+NodeName);
  myNode:=FindDataNodeById(SystemNodeTree,NodeName,NameSpace,true);
  // get the table data contents
  newData:=TXTable(myNode).ConstructDataString;
//  showmessage('newdata='+newData);
//  olddata:=TXTable(myNode).TableData;
  // update the node attribute value
//  showmessage('olddata='+olddata);
  if newData<>TXTable(myNode).TableData then
  begin
//    showmessage('tablechange event Set TableData '+newData);
    myNode.SetAttributeValue('TableData',newData);
    // adjust the selected cell...
    if (TXTable(myNode).SelectedCol>-1)
    and (TXTable(myNode).SelectedRow>-1) then
    begin
      if TXTable(myNode).GetNumCols < TXTable(myNode).SelectedCol+1 then
        TXTable(myNode).SelectedCol:=TXTable(myNode).GetNumCols-1;
      if TXTable(myNode).GetNumRows < TXTable(myNode).SelectedRow+1 then
        TXTable(myNode).SelectedRow:=TXTable(myNode).GetNumRows-1;
     TXTable(myNode).SelectedValue:=TXTable(myNode).GetCellValue(TXTable(myNode).SelectedRow,TXTable(myNode).SelectedCol);
    end;
    HandleEvent('Change',myNode.NodeName,NameSpace,newData);
  end;
end;

function CellClick(target:TObject; NodeName,NameSpace:String):String;
var
  TargetNode:TXTable;
  col,row:integer;
  ok:boolean;
begin
  //showmessage('cellclick. node='+NodeName);
  TargetNode:=TXTable(FindDataNodeByid(SystemNodeTree,NodeName,NameSpace,true));
  row:=-1;
  col:=-1;
  ok:=true;

  asm
  //console.log('cellclick. node='+NodeName);
  var sel = window.getSelection();
  //console.log('selection='+sel.toString() + ' type:'+ sel.type);
  if (sel.type=='Range') {
    var anch=sel.anchorNode;
    var a1 = anch.parentNode;
    var foc =sel.focusNode;
    var f1 = foc.parentNode;
    //console.log('Range:  '+(a1==f1));
    if (a1!=f1) {ok=false;}
    };

  var closestByTag = function(el, clazz) {
      // Traverse the DOM up with a while loop
      while (el.tagName != clazz) {
          // Increment the loop to the parent node
          el = el.parentNode;
          if (!el) {
              return null;
          }
      }
      // At this point, the while loop has stopped and `el` represents the element that has
      // the class you specified in the second parameter of the function `clazz`

      // Then return the matched element
      return el;
  }

  if (ok==true) {
    // if the user has slid the mouse to select text, and mouseup is in a different cell from the selected text, then
    // td returns null below.  In this case, set the cell selection to -1,-1 so we can prevent new text from appearing
    // in the wrong cell.
    var tr = closestByTag(target,'TR');
//    if (tr!=null) {
//      console.log('rowindex is '+tr.rowIndex);  }
    var td = closestByTag(target,'TD');
//    if (td!=null) {
//      console.log('cellindex is '+td.cellIndex);  }
    if ((tr!=null)&&(td!=null)) {
      if (tr!=null) {row=tr.rowIndex;}  else {row=-1;}
      if (td!=null) {col=td.cellIndex;} else {col=-1;}
//      console.log('click at row='+row+' col='+col);
    }
  }
  else
  {
    //!! change the range selection here, to limit to one cell....
    document.getSelection().removeAllRanges();
    var range = new Range();
    range.setStart(a1,0);
    range.setEnd(a1,1);
    document.getSelection().addRange(range);

    a1.focus();
    var tr = closestByTag(a1,'TR');
//    if (tr!=null) {
//      console.log('rowindex is '+tr.rowIndex);  }
    td = closestByTag(a1,'TD');
//    if (td!=null) {
//      console.log('cellindex is '+td.cellIndex);  }
    if ((tr!=null)&&(td!=null)) {
      if (tr!=null) {row=tr.rowIndex;}  else {row=-1;}
      if (td!=null) {col=td.cellIndex;} else {col=-1;}
//      console.log('click at row='+row+' col='+col);
    }
  }
  end;

  TargetNode.SelectedRow:=row;
  TargetNode.SelectedCol:=col;
  if (row>-1) and (col>-1) then
  begin
    TargetNode.SelectedValue:=TargetNode.GetCellValue(row,col);
    result:=inttostr(row)+','+inttostr(col);
  end
  else
  begin
    TargetNode.SelectedValue:='';
    result:='';
  end;
end;

function KeyDown(target:TObject; NodeName,NameSpace:String):Boolean;
var
  TargetNode:TXTable;
  col,row:integer;
begin
  //showmessage('keydown. node='+NodeName);
  TargetNode:=TXTable(FindDataNodeByid(SystemNodeTree,NodeName,NameSpace,true));
  if (TargetNode.SelectedRow>-1)
  and (TargetNode.SelectedCol>-1)  then
    result:=true
  else
    result:=false;
end;

function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  Checked,ReadOnly,LabelText,LabelPos:string;
  OnChangeString, OnFocusOutString, OnClickString, OnKeyString:String;
begin
  //showmessage('create table widget.  current TableData is '+MyNode.getAttribute('TableData',true).AttribValue);
  LabelText:= MyNode.getAttribute('LabelText',true).AttribValue;
  ReadOnly:= MyNode.getAttribute('ReadOnly',true).AttribValue;

  OnClickString:='onclick="event.stopPropagation();var rc=pas.XTable.CellClick(event.target,'''+ScreenObjectName+''','''+NameSpace+''');'+
                          'if (rc!='''') { '+
                          'pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''','''+NameSpace+''', rc); }' +
                          '" ';
  OnFocusOutString:='onfocusout="pas.XTable.TableChange(this,'''+ScreenObjectName+''','''+NameSpace+''');"';
  OnKeyString:='onkeydown="if (!pas.XTable.KeyDown(event.target,'''+ScreenObjectName+''','''+NameSpace+''')) {return false;}"';
  asm
    try{
    pas.XTable.AddTableStyles('');

    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,NameSpace,$impl.MyNodeType,position);
    wrapper.style.display = 'flex';
    var goright =  'flex-e'+'nd';

    var HTMLString='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var wrapperid = NameSpace+ScreenObjectName;
    var MyObjectName=wrapperid+'Contents';

    var ReadOnlyString = '';
    if (ReadOnly=='False') { ReadOnlyString = ' contenteditable ';}

    var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'">'+LabelText+'</label>';

    var TableString = '<table id='+MyObjectName+ ' '+
                      OnClickString +
                      OnKeyString +
                       OnFocusOutString +
                       ' style="display:inline-block; overflow:scroll; width:100%; height:100%;" '+
                       ReadOnlyString+' >' +
                         '<tr>'+
                           '<th>1</th>'+
                           '<th>2</th>'+
                           '<th>3</th>'+
                         '</tr>'+
                         '<tbody></tbody>'+
                       '</table> ';

    HTMLString = labelstring+TableString;

    var wrapper=document.getElementById(wrapperid);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);
  }
  catch(err) { alert(err.message+'  in XCheckBox.CreateXCheckBox');}

end;

  MyNode.ScreenObject:=MyNode;
  //showmessage('create table widget - refresh props');
  //showmessage('current TableData is '+MyNode.getAttribute('TableData',true).AttribValue);
  RefreshComponentProps(myNode);

  //showmessage('create table widget - done');
  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXTable.Create(MyForm,NodeName,NameSpace));
end;

procedure TXTable.SetTableWidth(AValue:string);
begin
  //showmessage('Table width='+AValue);
  myNode.SetAttributeValue('TableWidth',AValue);
  asm
  var ob = document.getElementById(this.NameSpace+this.NodeName);
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
  end;
end;

procedure TXTable.SetTableHeight(AValue:string);
begin
  myNode.SetAttributeValue('TableHeight',AValue);
  asm
  var ob = document.getElementById(this.NameSpace+this.NodeName);
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'H',AValue);
  end;
end;

procedure TXTable.SetColWidth(AValue:integer);
var
  oldcw:integer;
begin
  //showmessage('SetColWidth '+inttostr(AValue));
  oldcw:=self.ColWidth;
  myNode.SetAttributeValue('ColWidth',IntToStr(AValue));
  if oldcw<>AValue then
    //rebuild the data to incorporate col width
    self.TableData:=self.TableData;
end;

function TXTable.GetCellsAsArray(SkipHeader:Boolean):TTableCellsArray;
var
  myArray:TTableCellsArray;
begin
    asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
      var r=-1;
      for (var i = 0, row; row = ob.rows[i]; i++) {
        if ((SkipHeader==false)||(i>0)) {
          r=r+1;
          myArray[r] = [];
          for (var j = 0, col; col = row.cells[j]; j++) {
            myArray[r][j] = row.cells[j].innerText;
          }
        }
      }
    }
    //alert(myArray);
  end;
  result:=myArray;
end;
function TXTable.ConstructDataString:String;
// Generate the data in string form from the table cell values
var
    i,j:integer;
    dataStr:String;
    NumbersOnly,HasHeader:Boolean;
begin
  NumbersOnly:=self.IsNumeric;
  HasHeader:=self.HasHeaderRow;
  dataStr:='[';
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
      //alert('ob is '+ob.id);
      for (var i = 0, row; row = ob.rows[i]; i++) {
        if (i>0) { dataStr=dataStr+',';}
        dataStr=dataStr+'[';
        for (var j = 0, col; col = row.cells[j]; j++) {
          if (j>0) { dataStr=dataStr+','; }
          var str = row.cells[j].innerText;
          str = this.QuoteCellForJSON(str,i);
          dataStr=dataStr+str;
        }
        dataStr=dataStr+']';
      }
    }
  end;
  dataStr:=dataStr+']';
  //showmessage('ConstructDataString '+dataStr);
  result:=dataStr;
end;

function TXTable.GetNumCols:integer;
var
    num:integer;
begin
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
      if (ob.rows.length > 0) {
        num=ob.rows[0].cells.length;
      }
      else {num=0};
    }
  end;
  result:=num;
end;

function TXTable.GetNumRows:integer;
var
    num:integer;
begin
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
    num=ob.rows.length;
    }
  end;
  result:=num;
end;

function TXTable.GetCellValue(row,col:integer):string;
var
  myval:string;
begin
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
      if (ob.rows.length > row) {
        if (ob.rows[row].cells.length > col) {
          var str=ob.rows[row].cells[col].innerText;
          //str=str.replace(/&nbsp;/g, " ");
          myval = str;
        }
      }
    }
  end;
  result:=myval;
end;

procedure AddTableStyles(dummy:string);
begin

  asm
    try{
        // ----------------------------------------check if the style has already been set
        //alert('AddTableStyles');
        var x = document.getElementsByTagName("STYLE");
        var StyleIsSet = false;
        if (x.length>0){
          for (var i=0; i<x.length; i++){
            var y= x[i].innerHTML;
            if (y.indexOf("table") !=-1) { StyleIsSet =true}
          }
        }
        if (StyleIsSet == false){
          var styletext = '<style>'+
                        'table, th, td { '+
                         ' background-color:#FFFFFF; '+
                         ' border: 1px solid black; '+
                         ' border-collapse: collapse; '+
                         '}'+
                        'th { '+
                         ' background-color:#DDDDDD; '+
                         ' text-align: left;'+
                          '}'+
                       '}</style>';
        //alert(styletext);
        //----------------------------- now append the style declarations to the head of the HTML page
           document.head.innerHTML = document.head.innerHTML+styletext;
        }
   }catch(err) {alert('Error in XTable.AddTableStyles '+ err.message);}
  end;

end;

{$endif}

function TXTable.QuoteCellForJSON(cellval:String; rownum:integer):String;
begin
  if (self.IsNumeric=false)
  or (cellval='')
  or ((rownum=0) and (self.HasHeaderRow=true))
  or ((rownum=0) and (IsStrFloatNum(cellVal)=false)) then
  begin
    result:=QuoteIt(cellval);
  end
  else
  begin
    result:=cellval;
  end;
end;

procedure TXTable.SetCellValue(row,col:integer;AValue:string);
begin
  if (row>=0) and (col>=0) then
  begin
  {$ifndef JScript}
    if (TStringGrid(myControl).ColCount>col)
    and (TStringGrid(myControl).RowCount>row)
    and (TStringGrid(myControl).Cells[col,row]<>AValue) then
    begin
      TStringGrid(myControl).Cells[col,row]:=AValue;
      if not SuppressEvents then
        self.TableChange(TStringGrid(myControl));
    end;
  {$else}
    asm
      var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
      if (ob!=null) {
        if (ob.rows.length > row) {
          if (ob.rows[row].cells.length > col) {
            if (ob.rows[row].cells[col].innerText != AValue) {
              ob.rows[row].cells[col].innerText = AValue;
              pas.XTable.TableChange(ob,this.NodeName,this.NameSpace);
            }
          }
        }
      }
    end;
  {$endif}
  end;
end;

function TXTable.ConstructTableStringFromArray(myArray:TTableCellsArray):String;
var
  i,j:integer;
  str:string;
  //'[["a","b","c"],["1","2","3"]]'
begin
  str:='[';
  for i:= 0 to length(myArray)-1 do
  begin
    if i>0 then str:=str+',';
    str:=str+'[';
    for j:=0 to length(myArray[i])-1 do
    begin
      if j>0 then str:=str+',';
      {$ifndef JScript}
      str:=str+QuoteCellForJSON(myArray[i,j],i);
      {$else}
      asm
        //console.log('i='+i+' j='+j+' cell='+myArray[i][j]);
        str=str+this.QuoteCellForJSON(myArray[i][j].toString(),i);
      end;
      {$endif}
    end;
    str:=str+']';
  end;
  str:=str+']';
  result:=str;
end;

function TXTable.ConstructTableStringFromNumArray(myArray:T2DNumarray):String;
var
  i,j,r,cols,maxcol:integer;
  str:string;
  //'[["a","b","c"],["1","2","3"]]'
begin
  str:='[';
  cols:=self.NumCols;
  maxcol:=cols;
  if maxcol > length(myArray[0]) then
    maxcol:=length(myArray[0]);
  if self.HasHeaderRow then
  begin
    // keep old headings
    str:=str+'[';
    for j:=0 to length(myArray[0])-1 do
    begin
      if j>0 then str:=str+',';
      if (j<maxcol)
      and (self.GetCellValue(0,j)<>'') then
        str:=str+'"'+self.GetCellValue(0,j)+'"'
      else
        str:=str+'"col'+inttostr(j)+'"';
    end;
    str:=str+']';
    r:=0;
  end
  else
    r:=-1;

  for i:= 0 to length(myArray)-1 do
  begin
    r:=r+1;
    if r>0 then str:=str+',';
    str:=str+'[';
    for j:=0 to length(myArray[i])-1 do
    begin
      if j>0 then str:=str+',';
      str:=str+QuoteCellForJSON(floattostr(myArray[i,j]),i);
    end;
    str:=str+']';
  end;
  str:=str+']';
  result:=str;
end;

function TXTable.ConstructTableStringFromExcelCopy(CopiedString:String):String;
var
  i,j,colcount:integer;
  rows,cells:TStringList;
  cstr,str:string;
  // row1col1value #9 row1col2value #9 row1col3value #13#10  ...etc
  //'[["a","b","c"],["1","2","3"]]'
begin
  colcount:=0;
//  ShowAllChars('CopiedString: '+CopiedString);

  rows:=StringUtils.stringsplit(CopiedString,LineEnding);
  str:='[';
//  showmessage('rows='+inttostr(rows.count));
  for i:= 0 to rows.count-1 do
  begin
    rows[i]:=StringUtils.DelChars(rows[i],chr(13));
    if (trim(rows[i])<>'')
    or (i<rows.count-1)
    or (colcount=1) then
    begin
      cells:=StringUtils.stringsplit(rows[i],chr(9),false);
      if i>0 then str:=str+',';
      str:=str+'[';
      if i=0 then colcount:=cells.count;
      for j:=0 to cells.count-1 do
      begin
        if j>0 then str:=str+',';
        str:=str+QuoteCellForJSON(cells[j],i);
      end;
      str:=str+']';
    end;
  end;
  str:=str+']';
//  ShowAllChars(str);
  result:=str;
end;

procedure TXTable.LoadTableFromExcelCopy(CopiedString:String);
var
  tdata:String;
begin
  tdata:=ConstructTableStringFromExcelCopy(CopiedString);
  self.TableData:=tdata;
end;

procedure TXTable.LoadTableFromNumArray(NumArray:T2DNumArray);
var
  tdata:String;
begin
  tdata:=ConstructTableStringFromNumArray(NumArray);
  self.TableData:=tdata;
end;

function TXTable.GetReadOnly:Boolean;
begin
  result:=MyStrToBool(MyNode.getAttribute('ReadOnly',true).AttribValue);
end;

function TXTable.GetTableHeight:string;
begin
  result:=MyNode.getAttribute('TableHeight',true).AttribValue;
end;
function TXTable.GetTableWidth:string;
begin
  result:=MyNode.getAttribute('TableWidth',true).AttribValue;
end;
function TXTable.GetTableData:string;
begin
  result:=MyNode.getAttribute('TableData',true).AttribValue;
end;
function TXTable.GetColWidth:integer;
begin
  result:=StrToInt(MyNode.getAttribute('ColWidth',true).AttribValue);
end;
function TXTable.GetHasHeaderRow:Boolean;
begin
  result:=MyStrToBool(MyNode.getAttribute('HasHeaderRow',true).AttribValue);
end;
function TXTable.GetIsNumeric:Boolean;
begin
  result:=MyStrToBool(MyNode.getAttribute('IsNumeric',true).AttribValue);
end;
function TXTable.GetIncludeDataInSave:Boolean;
begin
  result:=MyStrToBool(MyNode.getAttribute('IncludeDataInSave',true).AttribValue);
end;

function TXTable.GetSelectedRow:integer;
var
  str:String;
begin
  str:=MyNode.getAttribute('SelectedRow',true).AttribValue;
  if str<>'' then
    result:=StrToInt(str)
  else
    result:=-1;
end;
function TXTable.GetSelectedCol:integer;
var
  str:String;
begin
  str:=MyNode.getAttribute('SelectedCol',true).AttribValue;
  if str<>'' then
    result:=StrToInt(str)
  else
    result:=-1;
end;
function TXTable.GetSelectedValue:String;
var
  r,c:integer;
  v:String;
begin
  //result:=MyNode.getAttribute('SelectedValue',true).AttribValue;
  r:=self.SelectedRow;
  c:=self.SelectedCol;
  if (r>=0) and (c>=0) then
  begin
    v:=self.GetCellValue(r,c);
    //showmessage('GetSelectedValue '+inttostr(r)+' '+inttostr(c)+' = '+v);
    myNode.SetAttributeValue('SelectedValue',v,'String');
    result:=v;
  end
  else
    result:='';
end;

procedure TXTable.SetSelectedRow(AValue:integer);
var
  v:String;
begin
  myNode.SetAttributeValue('SelectedRow',IntToStr(AValue),'Integer');
  v:=self.GetSelectedValue;    // this sets the SelectedValue attribute
end;
procedure TXTable.SetSelectedCol(AValue:integer);
var
  v:String;
begin
  myNode.SetAttributeValue('SelectedCol',IntToStr(AValue),'Integer');
  v:=self.GetSelectedValue;    // this sets the SelectedValue attribute
end;
procedure TXTable.SetSelectedValue(AValue:String);
begin
  myNode.SetAttributeValue('SelectedValue',AValue,'String');
  //showmessage('setting cell '+inttostr(self.SelectedRow)+' '+inttostr(self.SelectedCol)+' '+AValue);
  self.SetCellValue(self.SelectedRow,self.SelectedCol,AValue);
end;
procedure TXTable.SetHasHeaderRow(AValue:Boolean);
var
  vchanged:boolean;
begin
  if self.HasHeaderRow<>AValue then
    vchanged:=true
  else
    vchanged:=false;
  myNode.SetAttributeValue('HasHeaderRow',MyBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  if AValue=true then
    TStringGrid(myControl).FixedRows:=1
  else
    TStringGrid(myControl).FixedRows:=0;
  {$else}
  if vchanged then
    // rebuild the table with/without header row
    self.TableData:=self.ConstructDataString;
  {$endif}
end;
procedure TXTable.SetIsNumeric(AValue:Boolean);
begin
  myNode.SetAttributeValue('IsNumeric',MyBoolToStr(AValue),'Boolean');
end;
procedure TXTable.SetIncludeDataInSave(AValue:Boolean);
begin
  myNode.SetAttributeValue('IncludeDataInSave',MyBoolToStr(AValue),'Boolean');
end;

procedure TXTable.AddTableRows(numRows:integer);
var
  TheArray:TTableCellsArray;
  c,r,r1:integer;
begin

  TheArray:=self.GetCellsAsArray(false);
  r1:=length(TheArray);
  SetLength(TheArray, length(TheArray)+numRows);
  for r:=r1 to length(TheArray)-1 do
  begin
    SetLength(TheArray[r], length(TheArray[0]));
    for c:=0 to length(TheArray[0])-1 do
    begin
      TheArray[r,c]:='';
    end;
  end;

  self.TableData:=self.ConstructTableStringFromArray(TheArray);
  self.NumRows:=self.NumRows;
end;
procedure TXTable.AddTableColumns(numCols:integer);
var
  TheArray:TTableCellsArray;
  i,j,j0:integer;
begin
  TheArray:=self.GetCellsAsArray(false);
  j0:=Length(TheArray[0]);
  for i:=0 to length(TheArray)-1 do
  begin
    SetLength(TheArray[i], j0+numCols);
    for j:=j0 to Length(TheArray[0])-1 do
      if i=0 then TheArray[i,j]:='hdr' else TheArray[i,j]:='';
  end;
  self.TableData:=self.ConstructTableStringFromArray(TheArray);
  self.NumCols:=self.NumCols;
end;

procedure TXTable.DeleteRow(r:integer);
begin
  if (r<0) or (r>self.NumRows-1) then
    showmessage('DeleteRow '+inttostr(r)+' out of range')
  else
  begin
    {$ifndef JScript}
    TStringGridAccess(TStringGrid(self.myControl)).DeleteRow(r);
    {$else}
    asm
      //alert('deleting row '+r);
        var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
        ob.deleteRow(r);
    end;
    {$endif}
    self.TableData:=self.ConstructDataString;
    {$ifndef JScript}
    TStringGrid(self.myControl).FixedRows:=0; //allow editing column headers
    {$endif}
    self.NumRows:=self.NumRows;
  end;
end;

procedure TXTable.DeleteColumn(c:Integer);
begin
  if (c<0) or (c>self.NumCols-1) then
    showmessage('DeleteColumn '+inttostr(c)+' out of range')
  else
  begin
    {$ifndef JScript}
    TStringGridAccess(TStringGrid(self.myControl)).DeleteCol(c);
    {$else}
    asm
        var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
        if (ob!=null) {
        for (var i = 0, row; row = ob.rows[i]; i++) {
                row.deleteCell(c);
            } }
    end;
    {$endif}
    self.TableData:=self.ConstructDataString;
    self.NumCols:=self.NumCols;
  end;
end;

procedure TXTable.DeleteSelectedRow;
var
  r,c:integer;
begin
  r:=self.SelectedRow;
  c:=self.SelectedCol;
  if (r<0) or (c<0) then
    showmessage('DeleteSelectedRow - row not selected')
  else if r<1 then
    showmessage('DeleteSelectedRow - cannot delete header row')
  else
  begin
    self.deleteRow(r);
  end;
end;
procedure TXTable.DeleteSelectedColumn;
var
  r,c:integer;
begin
  r:=self.SelectedRow;
  c:=self.SelectedCol;
  if (r<0) or (c<0) then
    showmessage('DeleteTableColumn - column not selected')
  else if (c=0) and (self.NumCols=1) then
    showmessage('DeleteTableColumn - cannot delete all columns')
  else
  begin
    self.DeleteColumn(c);
  end;
end;

procedure TXTable.SetNumRows(AValue:integer);
var
  r0,r:integer;
begin
  r0:=self.NumRows;
  if AValue>0 then
  begin
    if AValue>r0 then
      self.AddTableRows(AValue-r0)
    else if AValue<r0 then
      for r:=r0-1 downto AValue do
        self.DeleteRow(r);
    myNode.SetAttributeValue('NumRows',IntToStr(AValue),'Integer');
  end;
end;

procedure TXTable.SetNumCols(AValue:integer);
var
  c0,c:integer;
begin
  c0:=self.NumCols;
  if AValue>0 then
  begin
    if AValue>c0 then
      self.AddTableColumns(AValue-c0)
    else if AValue<c0 then
      for c:=c0-1 downto AValue do
        self.DeleteColumn(c);
    myNode.SetAttributeValue('NumCols',IntToStr(AValue),'Integer');
  end;
end;

procedure TXTable.SetReadOnly(AValue:Boolean);
begin
  myNode.SetAttributeValue('ReadOnly',myBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  TStringGrid(myControl).Enabled:=not AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NameSpace+this.NodeName);
    if (ob!=null) {
    if (AValue==true) {ob.disabled = true}
    else {ob.disabled = false }
    }
  end;
  {$endif}
end;

procedure TXTable.SetTableData(AValue:String);
var
  cw:String;
  hasHeaders:Boolean;
  i:integer;
begin
  //showmessage('table data : '+AValue);
  myNode.SetAttributeValue('TableData',AValue,'TableString');
  {$ifndef JScript}
  self.PopulateStringGrid(AValue);
  {$else}
  //showmessage(TXTable(self).NameSpace+TXTable(self).NodeName+' SetTableData : '+AValue);
  hasHeaders:=self.HasHeaderRow;
  cw:=myNode.GetAttribute('ColWidth',true).AttribValue;
  //showmessage('settabledata with colwidth '+cw);
  asm
    try {
    var ob = document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if ((ob!=null)&&(AValue!='')) {
      var localtestdata=JSON.parse(AValue);
      var RowCount =localtestdata.length;
      var ColCount = 0;
      for ( var i = 0; i < RowCount; i++ ) {
        if(localtestdata[i].length> ColCount)
          {ColCount = localtestdata[i].length }
      }
      //alert('rows='+RowCount+' cols='+ColCount);
      ob.innerHTML='';
      if (RowCount>0) {

        // first row is headers
        var toprow=document.createElement("tr");
        for (var j=0; j<ColCount; j++) {
          if (hasHeaders==true) {
            var hdr=document.createElement("th");
            }
          else {
            var hdr=document.createElement("td");}
          hdr.style.width=cw+'px';
          var textnode=document.createTextNode(localtestdata[0][j]);
          hdr.appendChild(textnode);
          toprow.appendChild(hdr);
        }
        ob.appendChild(toprow);

        var bdy=document.createElement("tbody");
        ob.appendChild(bdy);
        for (i = 1; i < RowCount; i++ ) {
           var row=document.createElement("tr");
           for (j=0; j<ColCount; j++) {
             var cell = document.createElement("td");
             //alert('cell value is '+localtestdata[i][j]);
             textnode=document.createTextNode(localtestdata[i][j]);
             cell.appendChild(textnode);
             row.appendChild(cell);
           }
           bdy.appendChild(row);
        }
      }
    }
    }
    catch(err) {  alert("Error in TXTable.SetTableData: "+ err.message); };

  end;
  {$endif}
  i:=self.NumCols;
  self.NumCols:=self.NumCols;
  self.NumRows:=self.NumRows;
  //{$ifdef JScript}showmessage(TXTable(self).NameSpace+TXTable(self).NodeName+' SetTableData done ');{$endif}
end;

begin
  // this is the set of node attributes that each TXTable instance will have.
  AddWrapperDefaultAttribs(myDefaultAttribs);
  AddDefaultAttribute(myDefaultAttribs,'TableWidth','String','150','',false);
  AddDefaultAttribute(myDefaultAttribs,'TableHeight','String','100','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','Top','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelText','String','Table','',false);
  AddDefaultAttribute(myDefaultAttribs,'ReadOnly','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'HasHeaderRow','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'IsNumeric','Boolean','False','If true, all cells (except header) must contain numeric data only',false);
  AddDefaultAttribute(myDefaultAttribs,'TableData','String','[["a","b","c"],["1","2","3"]]','',false);
  AddDefaultAttribute(myDefaultAttribs,'SelectedRow','Integer','-1','',false);
  AddDefaultAttribute(myDefaultAttribs,'SelectedCol','Integer','-1','',false);
  AddDefaultAttribute(myDefaultAttribs,'NumCols','Integer','3','',false,false);
  AddDefaultAttribute(myDefaultAttribs,'NumRows','Integer','2','',false,false);
  AddDefaultAttribute(myDefaultAttribs,'ColWidth','Integer','40','',false);
  AddDefaultAttribute(myDefaultAttribs,'SelectedValue','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'IncludeDataInSave','Boolean','True','If false, the table contents will be excluded from saved system data',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);


  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  AddAttribOptions(MyNodeType,'LabelPos',LabelPosOptions);
  {$ifndef JScript}
  RegisterClass(TXTable);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}
  SuppressDesignerProperty('TXTable','BgColor');
  SuppressDesignerProperty(MyNodeType,'ContainerHeight');
  SuppressDesignerProperty(MyNodeType,'ContainerWidth');
end.

