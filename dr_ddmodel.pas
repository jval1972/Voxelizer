//------------------------------------------------------------------------------
//
//  DOOMROCK: Doom Rock Sprite Generator
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
//  DDModel export
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/doom-model/
//------------------------------------------------------------------------------

unit dr_ddmodel;

interface

uses
  Windows, Classes, SysUtils, models;

function GetDDModelDeclaration(const model: model_t): string;

implementation

function GetDDModelDeclaration(const model: model_t): string;
var
  ret: string;
  minx, maxx, miny, maxy, minz, maxz: single;
  dx, dy, dz: single;
  scale: single;
  i: integer;

  procedure AddLine(const ln: string);
  begin
    ret := ret + ln + #13#10;
  end;

  procedure RenderFaces(const mVertCount, mFaceCount: integer;
    const mVert: array of fvec5_t; const mFace: array of ivec3_t);
  var
    ii: integer;
    procedure _render_rover(const r: integer);
    begin
      AddLine(Format('    glTexCoord2f(%1.6f, %1.6f);', [mVert[r].u, mVert[r].v]));
      AddLine(Format('    glvertex3f(%1.6f, %1.6f, %1.6f);', [(mVert[r].x{ - minx - dx / 2}) / scale, mVert[r].y / scale, (mVert[r].z{ - minz - dz / 2}) / scale]));
    end;
  begin
    AddLine('  glBegin(GL_TRIANGLES);');
      for ii := 0 to mFaceCount - 1 do
      begin
        _render_rover(mFace[ii].x);
        _render_rover(mFace[ii].y);
        _render_rover(mFace[ii].z);
      end;
    AddLine('  glEnd;');
  end;

begin
  minx := 100000.0;
  maxx := -100000.0;
  miny := 100000.0;
  maxy := -100000.0;
  minz := 100000.0;
  maxz := -100000.0;
  for i := 0 to model.mVertCount - 1 do
  begin
    if model.mVert[i].x < minx then
      minx := model.mVert[i].x;
    if model.mVert[i].x > maxx then
      maxx := model.mVert[i].x;
    if model.mVert[i].y < miny then
      miny := model.mVert[i].y;
    if model.mVert[i].y > maxy then
      maxy := model.mVert[i].y;
    if model.mVert[i].z < minz then
      minz := model.mVert[i].z;
    if model.mVert[i].z > maxz then
      maxz := model.mVert[i].z;
  end;
  dx := maxx - minx;
  dy := maxy - miny;
  dz := maxz - minz;

  scale := dx;
  if dy > scale then
    scale := dy;
  if dz > scale then
    scale := dz;

  ret := '';
  DecimalSeparator := '.';
  AddLine('model model1;');
  AddLine('begin');
  AddLine('  SetFrame(0);');
  RenderFaces(model.mVertCount, model.mFaceCount, model.mVert, model.mFace);
  AddLine('end.');
  Result := ret;
end;

end.
