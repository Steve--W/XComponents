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
  Classes, SysUtils, TypInfo, NodeUtils, StringUtils,
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
procedure TableChange(Sender:TObject;NodeName:String) ;
procedure CellClick(target:TObject; NodeName:String);
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
    constructor Create(MyForm:TForm;NodeName:String);
    {$endif}

    {$ifndef JScript}
    procedure PopulateMeFromJSONData(GridString:String);
    procedure PopulateStringGrid(gridData:String);
    procedure AddRowFromJSONData(rownum:integer; jData : TJSONData);
    {$endif}
    function ConstructDataString:String;
    function ConstructTableStringFromArray(myArray:TTableCellsArray):String;
    function GetCellValue(row,col:integer):string;
    procedure SetCellValue(row,col:integer;AValue:string);
    function GetCellsAsArray:TTableCellsArray;
    procedure AddTableRows(numRows:integer);
    procedure AddTableColumns(numCols:integer);
    procedure DeleteRow(r:integer);
    procedure DeleteColumn(c:integer);
    procedure DeleteSelectedRow;
    procedure DeleteSelectedColumn;
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

  // inherited from TWrapperPanel, not required here

  // suppress some of the link properties
//  RegisterPropertyEditor(TypeInfo(TAliasStrings), TXPropertyLink, 'AliasValues', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(String), TXPropertyLink, 'TIElementName', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(TPropertyLinkOptions), TXPropertyLink, 'Options', THiddenPropertyEditor);
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

function CreateWidget(ParentNode:TDataNode;ScreenObjectName:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXTable',ParentNode.MyForm,ParentNode,ScreenObjectName,Alignment,position);
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
    CallHandleEvent('Click',self.myNode.NodeName,self);
end;

//procedure TXTable.LinkLoadFromProperty(Sender: TObject);
//begin
//  if Sender=nil then ;
//  if (Link.Editor=nil) then exit;
//  inherited  LinkLoadFromProperty(Sender);
//
//end;
//
//procedure TXTable.LinkSaveToProperty(Sender: TObject);
//begin
//  if Sender=nil then ;
//  if Link.Editor=nil then exit;
// // !!!! sort this out
// // Link.SetAsText(myBoolToStr(TCheckBox(myControl).Checked));
//
//end;

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
  for i:=0 to jData.Count-1 do
  begin
    if TStringGrid(myControl).ColCount < (i+1) then
      TStringGrid(myControl).ColCount:=i+1;
    jItem := jData.Items[i];
    object_type := GetEnumName(TypeInfo(TJSONtype), Ord(jItem.JSONType));
    if object_type='jtString' then
    begin
      TStringGrid(myControl).Cells[i,rownum] :=  jItem.AsString;
    end;
  end;
end;

procedure TXTable.PopulateMeFromJSONData(GridString:String);
var
   jData : TJSONData;
   i,j,RowCount,cw:integer;
begin
  cw:=self.ColWidth;
  jData := GetJSON(GridString);
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
    TStringGrid(myControl).FixedCols:=0;
end;


procedure  TXTable.PopulateStringGrid(gridData:String);
begin
  self.PopulateMeFromJSONData(gridData);
end;

function TXTable.GetCellsAsArray:TTableCellsArray;
var
  i,j:integer;
  myArray:TTableCellsArray;
begin
  SetLength(myArray, TStringGrid(myControl).RowCount);
  for i:= 0 to TStringGrid(myControl).RowCount-1 do
  begin
    setlength(myArray[i],TStringGrid(myControl).ColCount);
    for j:=0 to TStringGrid(myControl).ColCount-1 do
    begin
      myArray[i,j]:=TStringGrid(myControl).Cells[j, i];
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
      dataStr:=dataStr+'"'+TStringGrid(myControl).Cells[j, i]+'"';
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
constructor TXTable.Create(MyForm:TForm;NodeName:String);
begin
  //showmessage('create table node');
  inherited Create(NodeName);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=false;

  SetNodePropDefaults(self,myDefaultAttribs);
end;


procedure TableChange(Sender:TObject; NodeName:String) ;
var
  newData:String;
  myNode:TDataNode;
begin
  //showmessage('tablechange event');
  myNode:=FindDataNodeById(SystemNodeTree,NodeName,true);
  // get the table data contents
  newData:=TXTable(myNode).ConstructDataString;
  //showmessage('newdata='+newData);
  // update the node attribute value
  if newData<>TXTable(myNode).TableData then
  begin
    //showmessage('tablechange event Set TableData '+newData);
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
    HandleEvent('Change',myNode.NodeName,newData);
  end;
end;

procedure CellClick(target:TObject; NodeName:String);
var
  TargetNode:TXTable;
  col,row:integer;
begin
  //showmessage('cellclick. node='+NodeName);
  TargetNode:=TXTable(FindDataNodeByid(SystemNodeTree,NodeName,true));
  asm
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
  var tr = closestByTag(target,'TR');
  var td = closestByTag(target,'TD');
  //alert('rowindex is '+tr.rowIndex);
  if (tr!=null) {row=tr.rowIndex;}  else {row=0;}
  //alert('cellindex is '+td.cellIndex);
  if (tr!=null) {col=td.cellIndex;} else {col=0;}
  //alert('click at row='+row+' col='+col);
  end;
  TargetNode.SelectedRow:=row;
  TargetNode.SelectedCol:=col;
  TargetNode.SelectedValue:=TargetNode.GetCellValue(row,col);
end;

function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer;Alignment:String):TDataNode;
var
  Checked,ReadOnly,LabelText,LabelPos:string;
  OnChangeString, OnFocusOutString, OnClickString:String;
begin
  //showmessage('create table widget');
  LabelText:= MyNode.getAttribute('LabelText',true).AttribValue;
  ReadOnly:= MyNode.getAttribute('ReadOnly',true).AttribValue;

  //OnClickString:='onclick="event.stopPropagation();pas.XTable.CellClick($(event.target),'''+ScreenObjectName+''');'+
  OnClickString:='onclick="event.stopPropagation();pas.XTable.CellClick(event.target,'''+ScreenObjectName+''');'+
                          'pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''', '''');' +
                          '" ';
  OnFocusOutString:='onfocusout="pas.XTable.TableChange(this,'''+ScreenObjectName+''');"';
  asm
    try{
    pas.XTable.AddTableStyles('');

    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);
    wrapper.style.display = 'flex';
    var goright =  'flex-e'+'nd';

    var HTMLString='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var MyObjectName=ScreenObjectName+'Contents';

    var ReadOnlyString = '';
    if (ReadOnly=='False') { ReadOnlyString = ' contenteditable ';}

    var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'">'+LabelText+'</label>';

    var TableString = '<table id='+MyObjectName+ ' '+
                       OnClickString +
                       OnFocusOutString +
                       ' style="display:inline-block; overflow:scroll; width:100%; height:100%;" '+
                       ReadOnlyString+' >' +
                         '<tr>'+
                           '<th>1</th>'+
                           '<th>2</th>'+
                           '<th>3</th>'+
                         '</tr>'+
                       '</table> ';

    HTMLString = labelstring+TableString;

    var wrapper=document.getElementById(ScreenObjectName);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);
  }
  catch(err) { alert(err.message+'  in XCheckBox.CreateXCheckBox');}

end;

  MyNode.ScreenObject:=MyNode;
  RefreshComponentProps(myNode);

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName:String):TObject;
begin
  result:=TObject(TXTable.Create(MyForm,NodeName));
end;

//procedure TXTable.LinkLoadFromProperty(Sender: TObject);
//begin
//  inherited  LinkLoadFromProperty(Sender);
//end;
//
//procedure TXTable.LinkSaveToProperty(Sender: TObject);
//begin
//  if Sender=nil then ;
//  if Link=nil then exit;
//  if Link.TIObject=nil then exit;
////  showmessage('linksavetoproperty. '+Link.TIPropertyName+' '));
//
//  SetStringProp(Link.TIObject,Link.TIPropertyName,self.TableData);
//end;

procedure TXTable.SetTableWidth(AValue:string);
begin
  //showmessage('Table width='+AValue);
  myNode.SetAttributeValue('TableWidth',AValue);
  asm
  var ob = document.getElementById(this.NodeName);
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
  end;
end;

procedure TXTable.SetTableHeight(AValue:string);
begin
  myNode.SetAttributeValue('TableHeight',AValue);
  asm
  var ob = document.getElementById(this.NodeName);
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'H',AValue);
  end;
end;

procedure TXTable.SetColWidth(AValue:integer);
begin
  //showmessage('SetColWidth '+inttostr(AValue));
  myNode.SetAttributeValue('ColWidth',IntToStr(AValue));
  //rebuild the data to incorporate col width
  self.TableData:=self.TableData;
end;

function TXTable.GetCellsAsArray:TTableCellsArray;
var
  i,j:integer;
  myArray:TTableCellsArray;
begin
    asm
    var ob = document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
    for (var i = 0, row; row = ob.rows[i]; i++) {
       myArray[i] = [];
       for (var j = 0, col; col = row.cells[j]; j++) {
         myArray[i][j] = row.cells[j].innerText;
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
begin
  dataStr:='[';
  asm
    var ob = document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
      //alert('ob is '+ob.id);
      for (var i = 0, row; row = ob.rows[i]; i++) {
        if (i>0) { dataStr=dataStr+',';}
        dataStr=dataStr+'[';
        for (var j = 0, col; col = row.cells[j]; j++) {
          if (j>0) { dataStr=dataStr+','; }
          var str = row.cells[j].innerText;
          dataStr=dataStr+ '"'+str+'"';
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
    var ob = document.getElementById(this.NodeName+'Contents');
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
    var ob = document.getElementById(this.NodeName+'Contents');
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
    var ob = document.getElementById(this.NodeName+'Contents');
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
      var ob = document.getElementById(this.NodeName+'Contents');
      if (ob!=null) {
        if (ob.rows.length > row) {
          if (ob.rows[row].cells.length > col) {
            if (ob.rows[row].cells[col].innerText != AValue) {
              ob.rows[row].cells[col].innerText = AValue;
              pas.XTable.TableChange(ob,this.NodeName);
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
      str:=str+'"'+myArray[i,j]+'"';
    end;
    str:=str+']';
  end;
  str:=str+']';
  result:=str;
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
begin
  myNode.SetAttributeValue('HasHeaderRow',MyBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  if AValue=true then
    TStringGrid(myControl).FixedRows:=1
  else
    TStringGrid(myControl).FixedRows:=0;
  {$else}
    // rebuild the table with no header row
    self.TableData:=self.ConstructDataString;
  {$endif}
end;

procedure TXTable.AddTableRows(numRows:integer);
var
  TheArray:TTableCellsArray;
  c,r,r1:integer;
begin

  TheArray:=self.GetCellsAsArray;
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
  TheArray:=self.GetCellsAsArray;
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
        var ob = document.getElementById(this.NodeName+'Contents');
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
        var ob = document.getElementById(this.NodeName+'Contents');
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
    var ob = document.getElementById(this.NodeName);
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
  //showmessage('SetTableData : '+AValue);
  hasHeaders:=self.HasHeaderRow;
  cw:=myNode.GetAttribute('ColWidth',true).AttribValue;
  //showmessage('settabledata with colwidth '+cw);
  asm
    var ob = document.getElementById(this.NodeName+'Contents');
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
            var hdr=document.createElement("th"); }
          else {
            var hdr=document.createElement("td");}
          hdr.style.width=cw+'px';
          var textnode=document.createTextNode(localtestdata[0][j]);
          hdr.appendChild(textnode);
          toprow.appendChild(hdr);
        }
        ob.appendChild(toprow);

        for (i = 1; i < RowCount; i++ ) {
           var row=document.createElement("tr");
           for (j=0; j<ColCount; j++) {
             var cell = document.createElement("td");
             //alert('cell value is '+localtestdata[i][j]);
             textnode=document.createTextNode(localtestdata[i][j]);
             cell.appendChild(textnode);
             row.appendChild(cell);
           }
           ob.appendChild(row);
        }
      }
    }

  end;
  {$endif}
  i:=self.NumCols;
  self.NumCols:=self.NumCols;
  self.NumRows:=self.NumRows;
end;

begin
  // this is the set of node attributes that each TXTable instance will have.
  AddDefaultAttribute(myDefaultAttribs,'Alignment','String','Left','',false);
  AddDefaultAttribute(myDefaultAttribs,'Hint','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'IsVisible','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'TableWidth','String','150','',false);
  AddDefaultAttribute(myDefaultAttribs,'TableHeight','String','200','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','Top','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelText','String','Table','',false);
  AddDefaultAttribute(myDefaultAttribs,'ReadOnly','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'HasHeaderRow','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'TableData','String','[["a","b","c"],["1","2","3"]]','',false);
  AddDefaultAttribute(myDefaultAttribs,'SelectedRow','Integer','-1','',false);
  AddDefaultAttribute(myDefaultAttribs,'SelectedCol','Integer','-1','',false);
  AddDefaultAttribute(myDefaultAttribs,'NumCols','Integer','3','',false);
  AddDefaultAttribute(myDefaultAttribs,'NumRows','Integer','2','',false);
  AddDefaultAttribute(myDefaultAttribs,'ColWidth','Integer','40','',false);
  AddDefaultAttribute(myDefaultAttribs,'SelectedValue','String','','',false);
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

