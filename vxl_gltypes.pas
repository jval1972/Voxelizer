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
//  OpenGL types
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/voxelizer/
//------------------------------------------------------------------------------

unit vxl_gltypes;

interface

uses
  dglOpenGL;

type
  GLVertex = record
    x: TGLfloat;
    y: TGLfloat;
    z: TGLfloat;
  end;
  PGLVertex = ^GLVertex;
  GLVertexArray = array[0..$FFFF] of GLVertex;
  PGLVertexArray = ^GLVertexArray;
  GLVertexArraysP = array[0..$FFFF] of PGLVertexArray;
  PGLVertexArraysP = ^GLVertexArraysP;

  GLTexcoord = record
    u: TGLfloat;
    v: TGLfloat;
  end;
  PGLTexcoord = ^GLTexcoord;
  GLTexcoordArray = array[0..$FFFF] of GLTexcoord;
  PGLTexcoordArray = ^GLTexcoordArray;

  GLuintArray = array[0..$FFFF] of GLuint;
  PGLuintArray = ^GLuintArray;

  GLVertexUV = record
    x: TGLfloat;
    y: TGLfloat;
    z: TGLfloat;
    u: TGLfloat;
    v: TGLfloat;
  end;

function MakeGLVertex(const x, y, z: single): GLVertex;

implementation

function MakeGLVertex(const x, y, z: single): GLVertex;
begin
  result.x := x;
  result.y := y;
  result.z := z;
end;

end.
