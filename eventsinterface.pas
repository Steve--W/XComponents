(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit EventsInterface;

{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, Math;

type TNumArray = Array of real;
type TStringArray = Array of String;
type T2DNumArray = Array of TNumArray;
type T3DNumArray = Array of T2DNumArray;
type T2DStringArray = Array of TStringArray;

type TEventStatus = class(TObject)
public
  EventType:String;
  NodeId:String;
  NameSpace:String;
  InitRunning:Boolean;           // true for an event that is running (or has run) the initialisation phase, but not yet the main phase.
  AsyncProcsRunning:TStringList; // list of async procs fired off during the initialisation phase.
  ContinueAfterTrappers:Boolean; // is true unless the event was intercepted by a trapper, and stopped.
  ReturnString:String;           // used by TXThreads, and also for completion stage of async events (eg. in copyfromclip)
  eventValue:String;             // to carry original event value, for trapper events
  ValueObject:TObject;
  constructor Create(EvType,NdId:String);
  function EventHasWaitingAsyncProcs():Boolean;
  function ClearAsync(ProcName:string):Boolean;

end;
type TNodeEventValue = class(TObject)
  myTree:TObject;
  SourceName,SrcText,DstText:String;
  myNode:TObject;
end;
var
  glbEvent:TEventStatus;

implementation

constructor TEventStatus.Create(EvType,NdId:String);
begin
  self.InitRunning:=false;
  self.ContinueAfterTrappers:=true;
  self.EventType:=EvType;
  self.NodeId:=NdId;
  self.AsyncProcsRunning:=TStringList.Create;
end;

function TEventStatus.EventHasWaitingAsyncProcs():Boolean;
begin
  if self.AsyncProcsRunning.Count > 0 then
    result:=true
  else
    result:=false;
end;
function TEventStatus.ClearAsync(ProcName:string):Boolean;
// remove a async job name from the list in this event record.
var
  i:integer;
begin
  result:=false;
  i:=self.AsyncProcsRunning.IndexOf(ProcName);
  if i>-1 then
  begin
    self.AsyncProcsRunning.Delete(i);
    result:=true;
  end;
end;

end.

