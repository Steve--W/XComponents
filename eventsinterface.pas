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
type TImgArray = Array of String;
type TEventStatus = class(TObject)
public
  EventType:String;
  NodeId:String;
  InitRunning:Boolean;           // true for an event that is running (or has run) the initialisation phase, but not yet the main phase.
  AsyncProcsRunning:TStringList; // list of async procs fired off during the initialisation phase.
  ContinueAfterTrappers:Boolean; // is true unless the event was intercepted by a trapper, and stopped.
  ReturnString:String;           // used by TXThreads, and also for completion stage of async events (eg. in copyfromclip)
  eventValue:String;             // to carry original event value, for trapper events
  constructor Create(EvType,NdId:String);
  function EventHasWaitingAsyncProcs():Boolean;

end;

implementation

constructor TEventStatus.Create(EvType,NdId:String);
begin
  self.InitRunning:=false;
  self.AsyncProcsRunning:=TStringList.Create;
  self.ContinueAfterTrappers:=true;
  self.EventType:=EvType;
  self.NodeId:=NdId;
end;

function TEventStatus.EventHasWaitingAsyncProcs():Boolean;
begin
  if self.AsyncProcsRunning.Count > 0 then
    result:=true
  else
    result:=false;
end;

end.

