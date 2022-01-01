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
//  MultiThreading Utility functions
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/voxelizer/
//------------------------------------------------------------------------------

unit vxl_multithreading;

interface

uses
  vxl_threads;

procedure MT_Init;

procedure MT_ShutDown;

procedure MT_Execute(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t = nil; const parms2: pointer = nil
  ); overload;

procedure MT_Execute(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t = nil; const parms4: pointer = nil
  ); overload;

procedure MT_Execute(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t; const parms4: pointer;
  const func5: threadfunc_t; const parms5: pointer;
  const func6: threadfunc_t = nil; const parms6: pointer = nil
  ); overload;

procedure MT_Execute(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t; const parms4: pointer;
  const func5: threadfunc_t; const parms5: pointer;
  const func6: threadfunc_t; const parms6: pointer;
  const func7: threadfunc_t; const parms7: pointer;
  const func8: threadfunc_t = nil; const parms8: pointer = nil
  ); overload;

type
  mt_range_t = record
    start, finish: integer;
  end;

  mt_range_p = ^mt_range_t;

type
  iterator_t = record
    idx: integer;
    numidxs: integer;
    data: pointer;
  end;
  iterator_p = ^iterator_t;

type
  PProcedure = procedure;

procedure MT_Iterate(const func: threadfunc_t; const data: pointer;
  const nthreads: integer = 0);

// Background tasks
function MT_ScheduleTask(const proc: PProcedure): integer;

procedure MT_ExecutePendingTask(const id: integer);

procedure MT_ExecutePendingTasks;

procedure MT_WaitTask(const id: integer);

procedure MT_WaitTasks;

// JVAL: Execute code threads
const
  NUMEXECTHREADS = 16;

implementation

uses
  Windows, SysUtils, Classes;

var
  mt_initialized: boolean = false;

// JVAL: General purpose threads
const
  MAXGPTHREADS = 16;

var
  numgpthreads: integer;
  gp_threads: array[0..MAXGPTHREADS - 1] of TDThread;

var
  exec_threads: array[0..NUMEXECTHREADS - 1] of TDThread;

// JVAL: Tasks
const
  NUMTASKTHREADS = 8;

var
  task_threads: array[0..NUMTASKTHREADS - 1] of TDThread;

var
  numcpus: integer = -1;

procedure I_DetectCPU;
var
  info: TSystemInfo;
begin
  GetSystemInfo(info);
  numcpus := info.dwNumberOfProcessors;
end;

function I_GetNumCPUs: integer;
begin
  if numcpus < 0 then
    I_DetectCPU;
  result := numcpus;
end;

procedure MT_Init;
var
  i: integer;
begin
  numgpthreads := I_GetNumCPUs;
  if numgpthreads < 2 then
    numgpthreads := 2;
  if numgpthreads > MAXGPTHREADS then
    numgpthreads := MAXGPTHREADS;
  for i := 0 to numgpthreads - 1 do
    gp_threads[i] := TDThread.Create;
  for i := 0 to NUMEXECTHREADS - 1 do
    exec_threads[i] := TDThread.Create;
  for i := 0 to NUMTASKTHREADS - 1 do
    task_threads[i] := TDThread.Create;
  mt_initialized := true;
end;

procedure MT_ShutDown;
var
  i: integer;
begin
  for i := 0 to numgpthreads - 1 do
    gp_threads[i].Free;
  for i := 0 to NUMEXECTHREADS - 1 do
    exec_threads[i].Free;
  for i := 0 to NUMTASKTHREADS - 1 do
    task_threads[i].Free;
  mt_initialized := false;
end;

var
  mt_execute_fetched: boolean = False;

procedure MT_Execute(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t = nil; const parms2: pointer = nil
  );
begin
  if mt_execute_fetched then
    raise Exception.Create('MT_Execute(): Invalid recoursive call.'#13#10);

  mt_execute_fetched := True;

  if @func2 <> nil then
  begin
    exec_threads[0].Activate(func2, parms2);
    if @func1 <> nil then
      func1(parms1);
    exec_threads[0].Wait;
  end
  else if @func1 <> nil then
    func1(parms1);

  mt_execute_fetched := False;
end;

procedure MT_Execute(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t = nil; const parms4: pointer = nil
  );
var
  nt: integer;
  i: integer;
begin
  if mt_execute_fetched then
    raise Exception.Create('MT_Execute(): Invalid recoursive call.'#13#10);

  mt_execute_fetched := True;

  nt := 0;
  if @func2 <> nil then
  begin
    exec_threads[nt].Activate(func2, parms2);
    inc(nt);
  end;
  if @func3 <> nil then
  begin
    exec_threads[nt].Activate(func3, parms3);
    inc(nt);
  end;
  if @func4 <> nil then
  begin
    exec_threads[nt].Activate(func4, parms4);
    inc(nt);
  end;
  if @func1 <> nil then
    func1(parms1);
  for i := 0 to nt - 1 do
    exec_threads[i].Wait;
  mt_execute_fetched := False;
end;

procedure MT_Execute(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t; const parms4: pointer;
  const func5: threadfunc_t; const parms5: pointer;
  const func6: threadfunc_t = nil; const parms6: pointer = nil
  ); overload;
var
  nt: integer;
  i: integer;
begin
  if mt_execute_fetched then
    raise Exception.Create('MT_Execute(): Invalid recoursive call.'#13#10);

  mt_execute_fetched := True;

  nt := 0;
  if @func2 <> nil then
  begin
    exec_threads[nt].Activate(func2, parms2);
    inc(nt);
  end;
  if @func3 <> nil then
  begin
    exec_threads[nt].Activate(func3, parms3);
    inc(nt);
  end;
  if @func4 <> nil then
  begin
    exec_threads[nt].Activate(func4, parms4);
    inc(nt);
  end;
  if @func5 <> nil then
  begin
    exec_threads[nt].Activate(func5, parms5);
    inc(nt);
  end;
  if @func6 <> nil then
  begin
    exec_threads[nt].Activate(func6, parms6);
    inc(nt);
  end;
  if @func1 <> nil then
    func1(parms1);
  for i := 0 to nt - 1 do
    exec_threads[i].Wait;
  mt_execute_fetched := False;
end;

procedure MT_Execute(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t; const parms4: pointer;
  const func5: threadfunc_t; const parms5: pointer;
  const func6: threadfunc_t; const parms6: pointer;
  const func7: threadfunc_t; const parms7: pointer;
  const func8: threadfunc_t = nil; const parms8: pointer = nil
  ); overload;
var
  nt: integer;
  i: integer;
begin
  if mt_execute_fetched then
    raise Exception.Create('MT_Execute(): Invalid recoursive call.'#13#10);

  mt_execute_fetched := True;

  nt := 0;
  if @func2 <> nil then
  begin
    exec_threads[nt].Activate(func2, parms2);
    inc(nt);
  end;
  if @func3 <> nil then
  begin
    exec_threads[nt].Activate(func3, parms3);
    inc(nt);
  end;
  if @func4 <> nil then
  begin
    exec_threads[nt].Activate(func4, parms4);
    inc(nt);
  end;
  if @func5 <> nil then
  begin
    exec_threads[nt].Activate(func5, parms5);
    inc(nt);
  end;
  if @func6 <> nil then
  begin
    exec_threads[nt].Activate(func6, parms6);
    inc(nt);
  end;
  if @func7 <> nil then
  begin
    exec_threads[nt].Activate(func7, parms7);
    inc(nt);
  end;
  if @func8 <> nil then
  begin
    exec_threads[nt].Activate(func8, parms8);
    inc(nt);
  end;
  if @func1 <> nil then
    func1(parms1);
  for i := 0 to nt - 1 do
    exec_threads[i].Wait;
  mt_execute_fetched := False;
end;

procedure MT_Iterate(const func: threadfunc_t; const data: pointer;
  const nthreads: integer = 0);
var
  parms: array[0..NUMEXECTHREADS] of iterator_t;
  i, nt: integer;
begin
  if mt_execute_fetched then
    raise Exception.Create('MT_Iterate(): Invalid recoursive call.'#13#10);

  mt_execute_fetched := True;

  if nthreads > 0 then
    nt := nthreads
  else
    nt := I_GetNumCPUs;
  if nt < 2 then
    nt := 2
  else if nt > NUMEXECTHREADS + 1 then
    nt := NUMEXECTHREADS + 1;

  for i := 0 to nt - 2 do
  begin
    parms[i].idx := i;
    parms[i].numidxs := nt;
    parms[i].data := data;
    exec_threads[i].Activate(func, @parms[i]);
  end;
  parms[nt - 1].idx := nt - 1;
  parms[nt - 1].numidxs := nt;
  parms[nt - 1].data := data;
  func(@parms[nt - 1]);

  for i := 0 to nt - 2 do
    exec_threads[i].Wait;

  mt_execute_fetched := False;
end;

type
  taskinfo_t = record
    id: integer;
    proc: PProcedure;
  end;
  Ptaskinfo_t = ^taskinfo_t;

var
  tasks: array[0..NUMTASKTHREADS - 1] of taskinfo_t;

function _execute_task(p: pointer): integer; stdcall;
var
  pt: Ptaskinfo_t;
begin
  pt := p;
  pt.proc;
  result := pt.id;
  pt.id := -1;
  pt.proc := nil;
end;

function MT_ScheduleTask(const proc: PProcedure): integer;
var
  i: integer;
begin
  for i := 0 to NUMTASKTHREADS - 1 do
    if not Assigned(tasks[i].proc) then
    begin
      tasks[i].id := i;
      tasks[i].proc := proc;
      result := i;
      exit;
    end;
  proc;
  result := -1;
end;

procedure MT_ExecutePendingTask(const id: integer);
begin
  if Assigned(tasks[id].proc) then
    task_threads[id].Activate(_execute_task, @tasks[id]);
end;

procedure MT_ExecutePendingTasks;
var
  i: integer;
begin
  for i := 0 to NUMTASKTHREADS - 1 do
    if Assigned(tasks[i].proc) then
      task_threads[i].Activate(_execute_task, @tasks[i]);
end;


procedure MT_WaitTask(const id: integer);
begin
  if (id < 0) or (id >= NUMTASKTHREADS) then
    exit;
  if Assigned(tasks[id].proc) then
    task_threads[id].Wait;
end;

procedure MT_WaitTasks;
var
  i: integer;
begin
  for i := 0 to NUMTASKTHREADS - 1 do
    if Assigned(tasks[i].proc) then
      task_threads[i].Wait;
end;

end.

