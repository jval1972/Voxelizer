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
//  Base model class
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/voxelizer/
//------------------------------------------------------------------------------

unit models;

interface

type
  fvec5_t = record
    x, y, z, u, v: single;
  end;
  fvec5_p = ^fvec5_t;
  fvec5_a = array[0..$FFFF] of fvec5_t;
  fvec5_pa = ^fvec5_a;

  ivec3_t = record
    x, y, z: integer;
  end;
  ivec3_p = ^ivec3_t;
  ivec3_a = array[0..$FFFF] of ivec3_t;
  ivec3_pa = ^ivec3_a;

type
  model_t = class
  public
    mVertCount: integer;
    mFaceCount: integer;

    mVert: array of fvec5_t;
    mFace: array of ivec3_t;

    mFrame: Integer;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure init;
    function AddVert(const x, y, z, u, v: single): integer;
    function maxcoord: single;
    function maxdiameter: single;
  end;

implementation

constructor model_t.Create;
begin
  SetLength(mVert, 0);
  SetLength(mFace, 0);

  mVertCount := 0;
  mFaceCount := 0;
  mFrame := 0;
end;

destructor model_t.Destroy;
begin
  SetLength(mVert, 0);
  SetLength(mFace, 0);
end;

const
  EPSILON = 0.000001;

function model_t.AddVert(const x, y, z, u, v: single): integer;
var
  i: integer;
begin
  for i := 0 to mVertCount - 1 do
    if abs(x - mVert[i].x) < EPSILON then
      if abs(y - mVert[i].y) < EPSILON then
        if abs(z - mVert[i].z) < EPSILON then
        begin
          Result := i;
          Exit;
        end;

  Result := mVertCount;
  inc(mVertCount);
  SetLength(mVert, mVertCount);
  mVert[Result].x := x;
  mVert[Result].y := y;
  mVert[Result].z := z;
  mVert[Result].u := u;
  mVert[Result].v := v;
end;

procedure model_t.init;
begin
  SetLength(mVert, 0);
  SetLength(mFace, 0);

  mVertCount := 0;
  mFaceCount := 0;
  mFrame := 0;
end;

function model_t.maxcoord: single;
var
  minx, maxx, miny, maxy, minz, maxz: single;
  i: integer;
begin
  minx := 100000.0;
  maxx := -100000.0;
  miny := 100000.0;
  maxy := -100000.0;
  minz := 100000.0;
  maxz := -100000.0;
  for i := 0 to mVertCount - 1 do
  begin
    if mVert[i].x < minx then
      minx := mVert[i].x;
    if mVert[i].x > maxx then
      maxx := mVert[i].x;
    if mVert[i].y < miny then
      miny := mVert[i].y;
    if mVert[i].y > maxy then
      maxy := mVert[i].y;
    if mVert[i].z < minz then
      minz := mVert[i].z;
    if mVert[i].z > maxz then
      maxz := mVert[i].z;
  end;
  minx := abs(minx);
  maxx := abs(maxx);
  miny := abs(miny);
  maxy := abs(maxy);
  minz := abs(minz);
  maxz := abs(maxz);
  Result := minx;
  if maxx > Result then
    Result := maxx;
  if miny > Result then
    Result := miny;
  if maxy > Result then
    Result := maxy;
  if minz > Result then
    Result := minz;
  if maxz > Result then
    Result := maxz;
end;

function model_t.maxdiameter: single;
var
  minx, maxx, miny, maxy, minz, maxz: single;
  dx, dy, dz: single;
  i: integer;
begin
  minx := 100000.0;
  maxx := -100000.0;
  miny := 100000.0;
  maxy := -100000.0;
  minz := 100000.0;
  maxz := -100000.0;
  for i := 0 to mVertCount - 1 do
  begin
    if mVert[i].x < minx then
      minx := mVert[i].x;
    if mVert[i].x > maxx then
      maxx := mVert[i].x;
    if mVert[i].y < miny then
      miny := mVert[i].y;
    if mVert[i].y > maxy then
      maxy := mVert[i].y;
    if mVert[i].z < minz then
      minz := mVert[i].z;
    if mVert[i].z > maxz then
      maxz := mVert[i].z;
  end;
  dx := maxx - minx;
  dy := maxy - miny;
  dz := maxz - minz;
  Result := dx;
  if dy > Result then
    Result := dy;
  if dz > Result then
    Result := dz;
end;

end.
