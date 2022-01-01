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
//  Export model to voxelbuffer
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/voxelizer/
//------------------------------------------------------------------------------

unit vxl_voxelexport;

interface

uses
  Windows,
  Classes,
  SysUtils,
  Graphics,
  vxl_voxels,
  models,
  vxl_voxelizer;

procedure DT_CreateVoxelFromModel(const t: model_t; const vox: voxelbuffer_p;
  const voxsize: integer; const modeltex: TBitmap; const mt: boolean = False);

implementation

uses
  vxl_defs,
  vxl_multithreading,
  vxl_texture;

procedure DT_CreateVoxelFacesFromModel(const mVertCount, mFaceCount: integer;
  const mVert: fvec5_pa; const mFace: ivec3_pa; const scale: single;
  const vox: voxelbuffer_p; const voxsize: integer; const tex: TTexture;
  const opaque: boolean);
var
  tri: meshtriangle_t;
  i: integer;
  ofs: integer;
  procedure _make_vertex(const r, g: integer);
  begin
    tri[g].x := mVert[r].x * scale + ofs;
    tri[g].y := voxsize - 1.0 - mVert[r].y * scale;
    tri[g].z := voxsize - 1.0 - mVert[r].z * scale - ofs;
    tri[g].u := mVert[r].u;
    tri[g].v := mVert[r].v;
  end;
begin
  ofs := voxsize div 2;
  for i := 0 to mFaceCount - 1 do
  begin
    _make_vertex(mFace[i].x, 0);
    _make_vertex(mFace[i].y, 1);
    _make_vertex(mFace[i].z, 2);
    DT_VoxelizeTri(@tri, tex, vox, voxsize, opaque);
  end;
end;

type
  th_createvoxelparams_t = record
    mVertCount, mFaceCount: integer;
    mVert: fvec5_pa;
    mFace: ivec3_pa;
    scale: single;
    vox: voxelbuffer_p;
    voxsize: integer;
    tex: TTexture;
    opaque: boolean;
  end;
  th_createvoxelparams_p = ^th_createvoxelparams_t;

procedure DT_CreateVoxelFacesFromModelMT(const idx, numidxs: integer;
  const mVertCount, mFaceCount: integer;
  const mVert: fvec5_pa; const mFace: ivec3_pa; const scale: single;
  const vox: voxelbuffer_p; const voxsize: integer; const tex: TTexture;
  const opaque: boolean);
var
  tri: meshtriangle_t;
  i: integer;
  ofs: integer;
  procedure _make_vertex(const r, g: integer);
  begin
    tri[g].x := mVert[r].x * scale + ofs;
    tri[g].y := voxsize - 1.0 - mVert[r].y * scale;
    tri[g].z := voxsize - 1.0 - mVert[r].z * scale - ofs;
    tri[g].u := mVert[r].u;
    tri[g].v := mVert[r].v;
  end;
begin
  ofs := voxsize div 2;
  for i := 0 to mFaceCount - 1 do
    if i mod numidxs = idx then
    begin
      _make_vertex(mFace[i].x, 0);
      _make_vertex(mFace[i].y, 1);
      _make_vertex(mFace[i].z, 2);
      DT_VoxelizeTri(@tri, tex, vox, voxsize, opaque);
    end;
end;

function DT_CreateVoxelFacesFromModel_thr(p: iterator_p): integer; stdcall;
var
  parms: th_createvoxelparams_p;
begin
  parms := p.data;
  DT_CreateVoxelFacesFromModelMT(p.idx, p.numidxs, parms.mVertCount, parms.mFaceCount,
    parms.mVert, parms.mFace, parms.scale, parms.vox, parms.voxsize, parms.tex, parms.opaque);
  Result := 0;
end;

procedure DT_CreateVoxelFromModel(const t: model_t; const vox: voxelbuffer_p;
  const voxsize: integer; const modeltex: TBitmap; const mt: boolean = False);
var
  xmin, xmax, ymin, ymax, zmin, zmax: single;
  i: integer;
  scale: single;
  cvparms: th_createvoxelparams_t;
  tex: TTexture;
begin
  if t.mVertCount = 0 then
    Exit;
  xmin := t.mVert[0].x;
  xmax := t.mVert[0].x;
  ymin := t.mVert[0].y;
  ymax := t.mVert[0].y;
  zmin := t.mVert[0].z;
  zmax := t.mVert[0].z;
  for i := 1 to t.mVertCount - 1 do
  begin
    if xmin > t.mVert[i].x then
      xmin := t.mVert[i].x
    else if xmax < t.mVert[i].x then
      xmax := t.mVert[i].x;
    if ymin > t.mVert[i].y then
      ymin := t.mVert[i].y
    else if ymax < t.mVert[i].y then
      ymax := t.mVert[i].y;
    if zmin > t.mVert[i].z then
      zmin := t.mVert[i].z
    else if zmax < t.mVert[i].z then
      zmax := t.mVert[i].z;
  end;
  ZeroMemory(vox, SizeOf(voxelbuffer_t));

  scale := abs(xmin);
  if abs(xmax) > scale then
    scale := abs(xmax);
  if abs(ymin) > scale then
    scale := abs(ymin);
  if abs(ymax) > scale then
    scale := abs(ymax);
  if abs(zmin) > scale then
    scale := abs(zmin);
  if abs(zmax) > scale then
    scale := abs(zmax);
  scale := 2 * scale;
  scale := 2 * t.maxcoord;
{  scale := xmax - xmin;
  if ymax - ymin > scale then
    scale := ymax - ymin;
  if zmax - zmin > scale then
    scale := zmax - zmin;}

  scale := (voxsize - 1) / scale;

  tex := TTexture.Create(modeltex);
  if mt then
  begin
    cvparms.mVertCount := t.mVertCount;
    cvparms.mFaceCount := t.mFaceCount;
    cvparms.mVert := t.mVert;
    cvparms.mFace := t.mFace;
    cvparms.scale := scale;
    cvparms.vox := vox;
    cvparms.voxsize := voxsize;
    cvparms.tex := tex;
    cvparms.opaque := true;
    MT_Iterate(@DT_CreateVoxelFacesFromModel_thr, @cvparms);
  end
  else
    DT_CreateVoxelFacesFromModel(t.mVertCount, t.mFaceCount, t.mVert, t.mFace,
      scale, vox, voxsize, tex, true);
  tex.Free;
end;

end.
