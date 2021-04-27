//------------------------------------------------------------------------------
//
//  Voxelizer
//  Copyright (C) 2021 by Jim Valavanis
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
//  MD2 Model format
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/voxelizer/
//------------------------------------------------------------------------------

unit vxl_md2model;

interface

uses
  SysUtils, Classes, vxl_gltypes, models;

type
  TMD2Model = class(TObject)
  protected
    fNumFrames: integer;
    fNumVertexes: integer;
    frameNames: TStringList;
    TheVectorsArray: PGLVertexArraysP;
    fscale: single;
    fyoffset: single;
    UV: PGLTexcoordArray;
    fname: string;
  public
    constructor Create(const name: string); virtual;
    destructor Destroy; override;
    procedure DrawFrameToModel(const mdl: model_t; const frm: integer); virtual;
    function StartFrame(const i: integer): integer; overload; virtual;
    function EndFrame(const i: integer): integer; overload; virtual;
    function StartFrame(const frame: string): integer; overload; virtual;
    function EndFrame(const frame: string): integer; overload; virtual;
    function FrameName(const i: integer): string; virtual;
    function FrameIndex(const frame: string): integer; virtual;
  end;

implementation

uses
  dglOpenGL, vxl_utils;

//------------------------------------------------------------------------------
//--------------------------- MD2 File Format ----------------------------------
//------------------------------------------------------------------------------

const
  // Magic number that identifies MD2 files (ASCII: 'IDP2').
  MD2_MAGIC = $32504449;

type
  TMD2_Index_List = record
    a, b, c: Integer;
    a_s, a_t,
    b_s, b_t,
    c_s, c_t: Single;
  end;
  TMD2_Index_List_Array = array[0..$FFFF] of TMD2_Index_List;
  PMD2_Index_List_Array = ^TMD2_Index_List_Array;

  TMD2_Vertex_List = record
    x, y, z: Single;
  end;
  TMD2_Vertex_List_Array = array[0..$FFFF] of TMD2_Vertex_List;
  PMD2_Vertex_List_Array = ^TMD2_Vertex_List_Array;

  TMD2_Frame_List = record
    Vertex: PMD2_Vertex_List_Array;
  end;
  TMD2_Frame_List_Array = array[0..$FFFF] of TMD2_Frame_List;
  PMD2_Frame_List_Array = ^TMD2_Frame_List_Array;

  TMD2DstVert_T = record
    s: SmallInt;
    t: SmallInt;
  end;
  TMD2DstVert_TArray = array[0..$FFFF] of TMD2DstVert_T;
  PMD2DstVert_TArray = ^TMD2DstVert_TArray;

  TMD2Triangle_T = record
    index_xyz: array[0..2] of SmallInt;
    index_st: array[0..2] of SmallInt;
  end;

  TMD2Trivertx_T = record
    v: array[0..2] of Byte;
    lightnormalindex: byte;
  end;

  PMD2AliasFrame_T = ^TMD2AliasFrame_T;
  TMD2AliasFrame_T = record
    scale: array[0..2] of Single;
    translate: array[0..2] of Single;
    name: array[0..15] of Char;
    verts: array[0..0] of TMD2Trivertx_T;
  end;

  TDmd2_T = record
    ident: Integer;
    version: Integer;

    skinWidth: Integer;
    skinHeight: Integer;
    framesize: Integer;

    num_skins: Integer;
    num_xyz: Integer;
    num_st: Integer;
    num_tris: Integer;
    num_glcmds: Integer;
    num_frames: Integer;

    ofs_skins: Integer;
    ofs_st: Integer;
    ofs_tris: Integer;
    ofs_frames: Integer;
    ofs_glcmds: Integer;
    ofs_end: Integer;
  end;

type
  TFrameIndexInfo = class
    StartFrame: integer;
    EndFrame: integer;
  end;

constructor TMD2Model.Create(const name: string);
var
  strm: TFileStream;
  base_st: PMD2DstVert_TArray;
  tri: TMD2Triangle_T;
  out_t: PMD2AliasFrame_T;
  i, j, k, idx1: Integer;
  vert: PGLVertex;
  frameName: string;
  m_index_list: PMD2_Index_List_Array;
  m_frame_list: PMD2_Frame_List_Array;
  fm_iTriangles: integer;
  modelheader: TDmd2_T;
  m: TMD2Model;

  procedure _offsets;
  begin
    if abs(vert.x) > fscale then
      fscale := abs(vert.x);
    if abs(vert.y) > fscale then
      fscale := abs(vert.y);
    if abs(vert.z) > fscale then
      fscale := abs(vert.z);
    if vert.y < fyoffset then
      fyoffset := vert.y;
  end;

begin
  Inherited Create;
  fname := name;
  frameNames := TStringList.Create;
  UV := nil;
  TheVectorsArray := nil;
  fNumFrames := 0;
  fNumVertexes := 0;
  strm := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);

  fscale := 1.0;
  fyoffset := 0.0;

  strm.Read(modelheader, SizeOf(modelheader));

  if modelheader.ident <> MD2_MAGIC then
  begin
    strm.Free;
    Exit;
  end;

  fNumFrames := modelheader.num_frames;
  fm_iTriangles := modelheader.num_tris;
  fNumVertexes := fm_iTriangles * 3;

  GetMem(m_index_list, SizeOf(TMD2_Index_List) * modelheader.num_tris);
  GetMem(m_frame_list, SizeOf(TMD2_Frame_List) * modelheader.num_frames);

  for i := 0 to modelheader.num_frames - 1 do
    GetMem(m_frame_list[i].vertex, SizeOf(TMD2_Vertex_List) * modelheader.num_xyz);

  strm.Seek(modelheader.ofs_st, soFromBeginning);
  GetMem(base_st, modelheader.num_st * SizeOf(base_st[0]));
  strm.Read(base_st^, modelheader.num_st * SizeOf(base_st[0]));

  for i := 0 to modelheader.num_tris - 1 do
  begin
    strm.Read(Tri, SizeOf(TMD2Triangle_T));
    with m_index_list[i] do
    begin
      a := tri.index_xyz[2];
      b := tri.index_xyz[1];
      c := tri.index_xyz[0];
      a_s := base_st[tri.index_st[2]].s / modelheader.skinWidth;
      a_t := base_st[tri.index_st[2]].t / modelheader.skinHeight;
      b_s := base_st[tri.index_st[1]].s / modelheader.skinWidth;
      b_t := base_st[tri.index_st[1]].t / modelheader.skinHeight;
      c_s := base_st[tri.index_st[0]].s / modelheader.skinWidth;
      c_t := base_st[tri.index_st[0]].t / modelheader.skinHeight;
    end;
  end;
  FreeMem(base_st, modelheader.num_st * SizeOf(base_st[0]));

  GetMem(out_t, modelheader.framesize);
  for i := 0 to modelheader.num_frames - 1 do
  begin
    strm.Read(out_t^, modelheader.framesize);
    frameName := UpperCase(Trim(out_t^.name));
    if Copy(frameName, Length(frameName) - 1, 1)[1] in ['0'..'9'] then
      frameName := Copy(frameName, 1, Length(frameName) - 2)
    else
      frameName := Copy(frameName, 1, Length(frameName) - 1);
    if frameNames.IndexOf(frameName) < 0 then
    begin
      frameNames.AddObject(frameName, TFrameIndexInfo.Create);
      (frameNames.Objects[frameNames.Count - 1] as TFrameIndexInfo).StartFrame := i;
      (frameNames.Objects[frameNames.Count - 1] as TFrameIndexInfo).EndFrame := i;
    end
    else
      (frameNames.Objects[frameNames.IndexOf(frameName)] as TFrameIndexInfo).EndFrame := i;

    for j := 0 to modelheader.num_xyz - 1 do
    begin
      with m_frame_list[i].vertex[j] do
      begin
        x := (out_t^.verts[j].v[0] * out_t^.scale[0] + out_t^.translate[0]);
        y := (out_t^.verts[j].v[1] * out_t^.scale[1] + out_t^.translate[1]);
        z := (out_t^.verts[j].v[2] * out_t^.scale[2] + out_t^.translate[2]);
      end;
    end;
  end;
  FreeMem(out_t, modelheader.framesize);

  GetMem(UV, fNumVertexes * SizeOf(GLTexcoord));
  GetMem(TheVectorsArray, fNumFrames * SizeOf(PGLVertexArray));

  k := 0;
  for j := 0 to fm_iTriangles - 1 do
  begin
    UV[k].u := m_index_list[j].a_s;
    UV[k].v := m_index_list[j].a_t;
    inc(k);

    UV[k].u := m_index_list[j].b_s;
    UV[k].v := m_index_list[j].b_t;
    inc(k);

    UV[k].u := m_index_list[j].c_s;
    UV[k].v := m_index_list[j].c_t;
    inc(k);
  end;

  for i := 0 to fNumFrames - 1 do
  begin
    GetMem(TheVectorsArray[i], fNumVertexes * SizeOf(TGLVectorf3));
    vert := @TheVectorsArray[i][0];
    for j := 0 to fm_iTriangles - 1 do
    begin
      idx1 := m_index_list[j].a;
      vert.x := m_frame_list[i].vertex[idx1].y;
      vert.y := m_frame_list[i].vertex[idx1].z;
      vert.z := m_frame_list[i].vertex[idx1].x;
      _offsets;
      inc(vert);

      idx1 := m_index_list[j].b;
      vert.x := m_frame_list[i].vertex[idx1].y;
      vert.y := m_frame_list[i].vertex[idx1].z;
      vert.z := m_frame_list[i].vertex[idx1].x;
      _offsets;
      inc(vert);

      idx1 := m_index_list[j].c;
      vert.x := m_frame_list[i].vertex[idx1].y;
      vert.y := m_frame_list[i].vertex[idx1].z;
      vert.z := m_frame_list[i].vertex[idx1].x;
      _offsets;
      inc(vert);
    end;
  end;

  for i := 0 to fNumFrames - 1 do
    FreeMem(m_frame_list[i].vertex, SizeOf(TMD2_Vertex_List) * modelheader.num_xyz);
  FreeMem(m_frame_list, SizeOf(TMD2_Frame_List) * modelheader.num_frames);
  FreeMem(m_index_list, SizeOf(TMD2_Index_List) * modelheader.num_tris);

  strm.Free;
end;

//------------------------------------------------------------------------------

destructor TMD2Model.Destroy;
var
  i: integer;
begin
  FreeMem(UV, fNumVertexes * SizeOf(GLTexcoord));
  for i := 0 to fNumFrames - 1 do
    FreeMem(TheVectorsArray[i], fNumVertexes * SizeOf(TGLVectorf3));
  FreeMem(TheVectorsArray, fNumFrames * SizeOf(GLVertexArraysP));

  for i := 0 to frameNames.Count - 1 do
    frameNames.Objects[i].Free;
  frameNames.Free;

  Inherited;
end;

//------------------------------------------------------------------------------

procedure TMD2Model.DrawFrameToModel(const mdl: model_t; const frm: integer);
var
  i, k: integer;
  f1, f2, f3: integer;
begin
  for k := 0 to fNumVertexes div 3 - 1 do
  begin
    i := 3 * k;
    f1 := mdl.AddVert(
      TheVectorsArray[frm][i].x / fscale,
      (TheVectorsArray[frm][i].y - fyoffset) / fscale,
      TheVectorsArray[frm][i].z / fscale,
      UV[i].u, UV[i].v);
    inc(i);
    f2 := mdl.AddVert(
      TheVectorsArray[frm][i].x / fscale,
      (TheVectorsArray[frm][i].y - fyoffset) / fscale,
      TheVectorsArray[frm][i].z / fscale,
      UV[i].u, UV[i].v);
    inc(i);
    f3 := mdl.AddVert(
      TheVectorsArray[frm][i].x / fscale,
      (TheVectorsArray[frm][i].y - fyoffset) / fscale,
      TheVectorsArray[frm][i].z / fscale,
      UV[i].u, UV[i].v);
    mdl.AddFace(f1, f2, f3);
  end;
end;

//------------------------------------------------------------------------------

function TMD2Model.StartFrame(const i: integer): integer;
begin
  result := -1;
  if IsIntInRange(i, 0, frameNames.Count - 1) then
    if frameNames.Objects[i] <> nil then
      result := (frameNames.Objects[i] as TFrameIndexInfo).StartFrame;
end;

//------------------------------------------------------------------------------

function TMD2Model.EndFrame(const i: integer): integer;
begin
  result := -1;
  if IsIntInRange(i, 0, frameNames.Count - 1) then
    if frameNames.Objects[i] <> nil then
      result := (frameNames.Objects[i] as TFrameIndexInfo).EndFrame;
end;

//------------------------------------------------------------------------------

function TMD2Model.StartFrame(const frame: string): integer;
begin
  result := StartFrame(frameNames.IndexOf(frame));
  if result = -1 then
    result := StartFrame(frameNames.IndexOf(UpperCase(frame)));
end;

//------------------------------------------------------------------------------

function TMD2Model.EndFrame(const frame: string): integer;
begin
  result := EndFrame(frameNames.IndexOf(frame));
  if result = -1 then
    result := EndFrame(frameNames.IndexOf(UpperCase(frame)));
end;

//------------------------------------------------------------------------------

function TMD2Model.FrameName(const i: integer): string;
begin
  if IsIntInRange(i, 0, frameNames.Count - 1) then
    result := frameNames.Strings[i]
  else
    result := '';
end;

//------------------------------------------------------------------------------

function TMD2Model.FrameIndex(const frame: string): integer;
begin
  result := frameNames.IndexOf(frame);
  if result = -1 then
    result := frameNames.IndexOf(UpperCase(frame));
end;

//------------------------------------------------------------------------------

end.
