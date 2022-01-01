//------------------------------------------------------------------------------
//
//  Voxelizer
//  Copyright (C) 2021-2022 by Jim Valavanis
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  Thread class
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/voxelizer/
//------------------------------------------------------------------------------

unit vxl_threads;

interface

type
  threadfunc_t = function(p: pointer): integer; stdcall;

type
  TDThread = class;

  threadinfo_t = record
    thread: TDThread;
  end;
  Pthreadinfo_t = ^threadinfo_t;

  TDThread = class
  private
    suspended: boolean;
  protected
    ffunc: threadfunc_t;
    fparms: Pointer;
    fid: Integer;
    info: threadinfo_t;
    fstatus: integer;
    fterminated: boolean;
  public
    constructor Create(const func: threadfunc_t = nil);
    destructor Destroy; override;
    procedure Activate(const parms: pointer); overload;
    procedure Activate(const func: threadfunc_t; const parms: pointer); overload;
    procedure Wait;
    function CheckJobDone: Boolean;
    function IsIdle: Boolean;
    property func: threadfunc_t read ffunc write ffunc;
  end;

const
  THR_DEAD = 0;
  THR_ACTIVE = 1;
  THR_IDLE = 2;

implementation

uses
  Windows;

type
  process_t = function(p: pointer): LongInt; stdcall;

function I_CreateProcess(p: process_t; parm: pointer; suspended: boolean): integer;
var
  id: LongWord;
begin
  if suspended then
    result := CreateThread(nil, $1000, @p, parm, CREATE_SUSPENDED, id)
  else
    result := CreateThread(nil, $1000, @p, parm, 0, id);
end;


function ThreadWorker(p: Pointer): integer; stdcall;
begin
  result := 0;
  while true do
  begin
    while (Pthreadinfo_t(p).thread.fstatus = THR_IDLE) and (not Pthreadinfo_t(p).thread.fterminated) do
    begin
      sleep(0);
    end;
    if Pthreadinfo_t(p).thread.fterminated then
      exit;
    Pthreadinfo_t(p).thread.ffunc(Pthreadinfo_t(p).thread.fparms);
    if Pthreadinfo_t(p).thread.fterminated then
      exit;
    Pthreadinfo_t(p).thread.fstatus := THR_IDLE;
  end;
end;

constructor TDThread.Create(const func: threadfunc_t);
begin
  fterminated := false;
  ffunc := func;
  fparms := nil;
  fstatus := THR_IDLE;
  info.thread := Self;
  fid := I_CreateProcess(@ThreadWorker, @info, true);
  suspended := true;
end;

destructor TDThread.Destroy;
begin
  fterminated := true;
  fstatus := THR_DEAD;
  WaitForSingleObject(fid, 100);
  Inherited Destroy;
end;

procedure TDThread.Activate(const func: threadfunc_t; const parms: pointer);
begin
  ffunc := func;
  Activate(parms);
end;

// JVAL: Should check for fstatus, but it is not called while active
procedure TDThread.Activate(const parms: pointer);
begin
  fparms := parms;
  fstatus := THR_ACTIVE;
  suspended := false;
  ResumeThread(fid);
end;

procedure TDThread.Wait;
begin
  if suspended then
    Exit;

  while fstatus = THR_ACTIVE do
  begin
    sleep(0);
  end;
  suspended := true;
  SuspendThread(fid);
end;

function TDThread.CheckJobDone: Boolean;
begin
  if fstatus = THR_IDLE then
  begin
    if not suspended then
    begin
      suspended := true;
      SuspendThread(fid);
    end;
    result := true;
  end
  else
    result := false;
end;

function TDThread.IsIdle: Boolean;
begin
  result := fstatus = THR_IDLE;
end;

end.

