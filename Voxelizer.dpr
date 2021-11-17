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
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/voxelizer/
//------------------------------------------------------------------------------

program Voxelizer;

uses
  FastMM4 in 'FastMM4.pas',
  FastMM4Messages in 'FastMM4Messages.pas',
  Forms,
  main in 'main.pas' {Form1},
  dglOpenGL in 'dglOpenGL.pas',
  models in 'models.pas',
  vxl_gl in 'vxl_gl.pas',
  vxl_filemenuhistory in 'vxl_filemenuhistory.pas',
  vxl_utils in 'vxl_utils.pas',
  pngextra in 'pngextra.pas',
  pngimage in 'pngimage.pas',
  pnglang in 'pnglang.pas',
  xTGA in 'xTGA.pas',
  zBitmap in 'zBitmap.pas',
  zlibpas in 'zlibpas.pas',
  vxl_soft3d in 'vxl_soft3d.pas',
  frm_exportsprite in 'frm_exportsprite.pas' {ExportSpriteForm},
  frm_spriteprefix in 'frm_spriteprefix.pas' {SpritePrefixForm},
  vxl_palettes in 'vxl_palettes.pas',
  vxl_wadwriter in 'vxl_wadwriter.pas',
  vxl_wad in 'vxl_wad.pas',
  vxl_doompatch in 'vxl_doompatch.pas',
  vxl_defs in 'vxl_defs.pas',
  vxl_voxelizer in 'vxl_voxelizer.pas',
  vxl_voxels in 'vxl_voxels.pas',
  vxl_voxelexport in 'vxl_voxelexport.pas',
  frm_exportvoxel in 'frm_exportvoxel.pas' {ExportVoxelForm},
  vxl_pk3writer in 'vxl_pk3writer.pas',
  vxl_md2model in 'vxl_md2model.pas',
  vxl_gltypes in 'vxl_gltypes.pas',
  pcximage in 'pcximage.pas',
  vxl_zipfile in 'vxl_zipfile.pas',
  vxl_threads in 'vxl_threads.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Voxelizer';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

