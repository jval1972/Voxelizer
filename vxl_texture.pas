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
//  TTexture class
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/voxelizer/
//------------------------------------------------------------------------------

unit vxl_texture;

interface

uses
  Windows, SysUtils, Classes, Graphics;

type
  imageline_t = array[0..$FFF] of LongWord;
  imageline_p = ^imageline_t;
  imagepixels_t = array[0..$FFF] of imageline_p;
  imagepixels_p = ^imagepixels_t;

type
  TTexture = class(TObject)
  private
    fwidth: Integer;
    fheight: Integer;
    fpixels: imagepixels_p;
  protected
    function GetPixels(x, y: integer): LongWord;
  public
    constructor Create(const bm: TBitmap); virtual;
    destructor Destroy; override;
    property Width: Integer read fwidth;
    property Height: Integer read fheight;
    property Pixels[x, y: integer]: LongWord read GetPixels;
  end;

implementation

constructor TTexture.Create(const bm: TBitmap);
var
  i: integer;
begin
  Inherited Create;
  fwidth := bm.Width;
  fheight := bm.Height;
  GetMem(fpixels, fheight * SizeOf(imageline_p));
  bm.PixelFormat := pf32bit;
  for i := 0 to fheight - 1 do
    fpixels[i] := bm.ScanLine[i];
end;

destructor TTexture.Destroy;
begin
  FreeMem(fpixels);
  inherited;
end;

function TTexture.GetPixels(x, y: integer): LongWord;
var
  A: packed array[0..3] of byte;
  tmp: byte;
begin
  if (x < 0) or (x >= fwidth) then
  begin
    Result := 0;
    Exit;
  end;
  if (y < 0) or (y >= fheight) then
  begin
    Result := 0;
    Exit;
  end;
  PLongWord(@A)^ := fpixels[y][x];
  tmp := A[0];
  A[0] := A[2];
  A[2] := tmp;
  Result := PLongWord(@A)^;
end;

end.
