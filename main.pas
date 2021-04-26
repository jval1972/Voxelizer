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
//  Main Form
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/doom-rock/
//------------------------------------------------------------------------------

unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, pngimage, xTGA, jpeg, zBitmap, ComCtrls, ExtCtrls, Buttons, Menus,
  StdCtrls, AppEvnts, ExtDlgs, clipbrd, ToolWin, dglOpenGL, models, dr_undo,
  dr_filemenuhistory, dr_slider;

type
  TForm1 = class(TForm)
    ColorDialog1: TColorDialog;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Open2: TMenuItem;
    Save1: TMenuItem;
    Savesa1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    ApplicationEvents1: TApplicationEvents;
    OpenDialog1: TOpenDialog;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    Timer1: TTimer;
    StatusBar1: TStatusBar;
    Options1: TMenuItem;
    SavePictureDialog1: TSavePictureDialog;
    N4: TMenuItem;
    Export1: TMenuItem;
    ExportObjModel1: TMenuItem;
    SaveDialog1: TSaveDialog;
    N5: TMenuItem;
    N8: TMenuItem;
    Copy1: TMenuItem;
    OpenPictureDialog2: TOpenPictureDialog;
    ToolBar1: TToolBar;
    OpenGLScrollBox: TScrollBox;
    OpenGLPanel: TPanel;
    Splitter1: TSplitter;
    SaveAsButton1: TSpeedButton;
    SaveButton1: TSpeedButton;
    OpenButton1: TSpeedButton;
    NewButton1: TSpeedButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    UndoButton1: TSpeedButton;
    RedoButton1: TSpeedButton;
    ToolButton4: TToolButton;
    AboutButton1: TSpeedButton;
    N7: TMenuItem;
    HistoryItem0: TMenuItem;
    HistoryItem1: TMenuItem;
    HistoryItem2: TMenuItem;
    HistoryItem3: TMenuItem;
    HistoryItem4: TMenuItem;
    HistoryItem5: TMenuItem;
    HistoryItem6: TMenuItem;
    HistoryItem7: TMenuItem;
    HistoryItem8: TMenuItem;
    HistoryItem9: TMenuItem;
    ExportScreenshot1: TMenuItem;
    Wireframe1: TMenuItem;
    Renderenviroment1: TMenuItem;
    SaveDialog2: TSaveDialog;
    Sprite1: TMenuItem;
    N1: TMenuItem;
    Voxel1: TMenuItem;
    SaveDialog3: TSaveDialog;
    N3: TMenuItem;
    MD2model1: TMenuItem;
    Panel1: TPanel;
    ExportSpriteSpeedButton: TSpeedButton;
    ExportVoxelSpeedButton: TSpeedButton;
    ExportMD2ModelSpeedButton1: TSpeedButton;
    Bevel1: TBevel;
    ScrollBox1: TScrollBox;
    PropertiesPanel: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    SeedSpeedButton1: TSpeedButton;
    SeedSpeedButton2: TSpeedButton;
    SeedEdit: TEdit;
    RockImage: TImage;
    LoadTrunkBitBtn1: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure NewButton1Click(Sender: TObject);
    procedure SaveButton1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AboutButton1Click(Sender: TObject);
    procedure SaveAsButton1Click(Sender: TObject);
    procedure ExitButton1Click(Sender: TObject);
    procedure OpenButton1Click(Sender: TObject);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure OpenGLPanelResize(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure OpenGLPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure OpenGLPanelDblClick(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure Redo1Click(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Options1Click(Sender: TObject);
    procedure Wireframe1Click(Sender: TObject);
    procedure RockImageDblClick(Sender: TObject);
    procedure Renderenviroment1Click(Sender: TObject);
    procedure SeedSpeedButton1Click(Sender: TObject);
    procedure SeedSpeedButton2Click(Sender: TObject);
    procedure SeedEditKeyPress(Sender: TObject; var Key: Char);
    procedure SeedEditChange(Sender: TObject);
    procedure ExportObjModel1Click(Sender: TObject);
    procedure ExportScreenshot1Click(Sender: TObject);
    procedure Sprite1Click(Sender: TObject);
    procedure Voxel1Click(Sender: TObject);
    procedure MD2ModelExportClick(Sender: TObject);
    procedure ApplicationEvents1Activate(Sender: TObject);
  private
    { Private declarations }
    ffilename: string;
    savepicturedata: boolean;
    changed: Boolean;
    rock: rock_t;
    rc: HGLRC;   // Rendering Context
    dc: HDC;     // Device Context
    glpanx, glpany: integer;
    glmousedown: integer;
    undoManager: TUndoRedoManager;
    filemenuhistory: TFileMenuHistory;
    glneedsupdate: boolean;
    needsrecalc: boolean;
    closing: boolean;
    procedure Idle(Sender: TObject; var Done: Boolean);
    function CheckCanClose: boolean;
    procedure DoNewRock(const seed: integer);
    procedure DoSaveRock(const fname: string);
    function DoLoadRock(const fname: string): boolean;
    procedure SetFileName(const fname: string);
    procedure DoSaveRockBinaryUndo(s: TStream);
    procedure DoLoadRockBinaryUndo(s: TStream);
    procedure SaveUndo;
    procedure UpdateStausbar;
    procedure UpdateEnable;
    procedure OnLoadRockFileMenuHistory(Sender: TObject; const fname: string);
    procedure DoRenderGL;
    procedure Get3dPreviewBitmap(const b: TBitmap);
    procedure RockToSliders;
    procedure SlidersToLabels;
    procedure RockToControls;
    procedure ControlsToRock(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  dr_gl,
  dr_defs,
  dr_utils,
  dr_voxels,
  dr_palettes,
  dr_md2,
  procrock_helpers,
  frm_exportsprite,
  frm_exportvoxel;

{$R *.dfm}

resourcestring
  rsTitle = 'Voxelizer';

procedure TForm1.FormCreate(Sender: TObject);
var
  pfd: TPIXELFORMATDESCRIPTOR;
  pf: Integer;
  doCreate: boolean;
  sdir: string;
begin
  Randomize;

  DecimalSeparator := '.';
  
  pt_LoadSettingFromFile(ChangeFileExt(ParamStr(0), '.ini'));

  closing := False;

  savepicturedata := False;

  PageControl1.ActivePageIndex := 0;

  undoManager := TUndoRedoManager.Create;
  undoManager.OnLoadFromStream := DoLoadRockBinaryUndo;
  undoManager.OnSaveToStream := DoSaveRockBinaryUndo;

  filemenuhistory := TFileMenuHistory.Create(self);
  filemenuhistory.MenuItem0 := HistoryItem0;
  filemenuhistory.MenuItem1 := HistoryItem1;
  filemenuhistory.MenuItem2 := HistoryItem2;
  filemenuhistory.MenuItem3 := HistoryItem3;
  filemenuhistory.MenuItem4 := HistoryItem4;
  filemenuhistory.MenuItem5 := HistoryItem5;
  filemenuhistory.MenuItem6 := HistoryItem6;
  filemenuhistory.MenuItem7 := HistoryItem7;
  filemenuhistory.MenuItem8 := HistoryItem8;
  filemenuhistory.MenuItem9 := HistoryItem9;
  filemenuhistory.OnOpen := OnLoadRockFileMenuHistory;

  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory9));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory8));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory7));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory6));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory5));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory4));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory3));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory2));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory1));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory0));

  sdir := ExtractFilePath(ParamStr(0));
  if sdir <> '' then
  begin
    if DirectoryExists(sdir) then
    begin
      SaveDialog1.InitialDir := sdir;
      SaveDialog2.InitialDir := sdir;
      SavePictureDialog1.InitialDir := sdir;
      OpenDialog1.InitialDir := sdir;
    end;
    if sdir[Length(sdir)] <> '\' then
      sdir := sdir + '\';
    if DirectoryExists(sdir + 'Data\Trunk') then
      OpenPictureDialog1.InitialDir := sdir + 'Data\Trunk';
    if DirectoryExists(sdir + 'Data\Twig') then
      OpenPictureDialog2.InitialDir := sdir + 'Data\Twig';
  end;

  rock := rock_t.Create;

  Scaled := False;

  OpenGLPanel.Width := 2 * Screen.Width div 3;
  OpenGLPanel.Height := 3 * Screen.Height div 4;
  OpenGLPanel.DoubleBuffered := True;

  glpanx := 0;
  glpany := 0;
  glmousedown := 0;

  InitOpenGL;
  ReadExtensions;
  ReadImplementationProperties;

  // OpenGL initialisieren
  dc := GetDC(OpenGLPanel.Handle);

  // PixelFormat
  pfd.nSize := SizeOf(pfd);
  pfd.nVersion := 1;
  pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  pfd.iPixelType := PFD_TYPE_RGBA;      // PFD_TYPE_RGBA or PFD_TYPEINDEX
  pfd.cColorBits := 32;

  pf := ChoosePixelFormat(dc, @pfd);   // Returns format that most closely matches above pixel format
  SetPixelFormat(dc, pf, @pfd);

  rc := wglCreateContext(dc);    // Rendering Context = window-glCreateContext
  wglMakeCurrent(dc, rc);        // Make the DC (Form1) the rendering Context

  // Initialize GL environment variables

  glInit;

  ResetCamera;

  OpenGLPanelResize(sender);    // sets up the perspective

  rocktexture := gld_CreateTexture(RockImage.Picture, False);

  glneedsupdate := True;

  needsrecalc := True;

  TabSheet1.DoubleBuffered := True;

  doCreate := True;
  if ParamCount > 0 then
    if DoLoadRock(ParamStr(1)) then
      doCreate := False;

  if DoCreate then
  begin
    SetFileName('');
    changed := False;
    RockToControls;
    glneedsupdate := True;
    needsrecalc := True;
    undoManager.Clear;
  end;

  // when the app has spare time, render the GL scene
  Application.OnIdle := Idle;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CheckCanClose;
end;

function TForm1.CheckCanClose: boolean;
var
  ret: integer;
begin
  if changed then
  begin
    ret := MessageBox(Handle, 'Do you want to save changes?', PChar(rsTitle), MB_YESNOCANCEL or MB_ICONQUESTION or MB_APPLMODAL);
    if ret = IDCANCEL	then
    begin
      Result := False;
      exit;
    end;
    if ret = IDNO	then
    begin
      Result := True;
      exit;
    end;
    if ret = IDYES then
    begin
      SaveButton1Click(self);
      Result := not changed;
      exit;
    end;
  end;
  Result := True;
end;

procedure TForm1.NewButton1Click(Sender: TObject);
begin
  if not CheckCanClose then
    Exit;

  DoNewRock(random($10000));
  ResetCamera;
end;

procedure TForm1.DoNewRock(const seed: integer);
begin
  SetFileName('');
  changed := False;
  rock.mProperties.DefaultValues(seed);
  RockToControls;
  glneedsupdate := True;
  needsrecalc := True;
  undoManager.Clear;
end;

procedure TForm1.SetFileName(const fname: string);
begin
  ffilename := fname;
  Caption := rsTitle;
  if ffilename <> '' then
    Caption := Caption + ' - ' + MkShortName(ffilename);
end;

procedure TForm1.SaveButton1Click(Sender: TObject);
begin
  if ffilename = '' then
  begin
    if SaveDialog1.Execute then
    begin
      ffilename := SaveDialog1.FileName;
      filemenuhistory.AddPath(ffilename);
    end
    else
    begin
      Beep;
      Exit;
    end;
  end;
  BackupFile(ffilename);
  DoSaveRock(ffilename);
end;

procedure TForm1.DoSaveRock(const fname: string);
var
  fs: TFileStream;
  m: TMemoryStream;
  sz: integer;
  z: TZBitmap;
begin
  SetFileName(fname);

  fs := TFileStream.Create(fname, fmCreate);
  try
    PT_SavePropertiesBinary(rock.mProperties, fs);

    if savepicturedata then
    begin
      m := TMemoryStream.Create;
      z := TZBitmap.Create;
      z.Assign(RockImage.Picture.Bitmap);
      z.PixelFormat := pf24bit;
      z.SaveToStream(m);
      z.Free;
      sz := m.size;
      fs.Write(sz, SizeOf(Integer));
      m.Position := 0;
      fs.CopyFrom(m, sz);
      m.Free;
    end;
  finally
    fs.Free;
  end;

  changed := False;
end;

function TForm1.DoLoadRock(const fname: string): boolean;
var
  fs: TFileStream;
  s: string;
  sz: integer;
  m: TMemoryStream;
  z: TZBitmap;
  oldp: integer;
begin
  if not FileExists(fname) then
  begin
    s := Format('File %s does not exist!', [MkShortName(fname)]);
    MessageBox(Handle, PChar(s), PChar(rsTitle), MB_OK or MB_ICONEXCLAMATION or MB_APPLMODAL);
    Result := False;
    exit;
  end;

  undoManager.Clear;

  fs := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);
  try
    PT_LoadPropertiesBinary(rock.mProperties, fs);

    if savepicturedata then
    begin
      oldp := fs.Position;
      if oldp < fs.Size then
      begin
        fs.Read(sz, SizeOf(Integer));
        if sz > 0 then
        begin
          m := TMemoryStream.Create;
          m.CopyFrom(fs, sz);
          m.Position := 0;
          z := TZBitmap.Create;
          z.LoadFromStream(m);
          z.PixelFormat := pf32bit;
          RockImage.Picture.Bitmap.Assign(z);
          z.Free;
          m.Free;
        end;
        fs.Position := oldp + sz + SizeOf(Integer);
      end;
    end;

  finally
    fs.Free;
  end;

  if savepicturedata then
  begin
    // Recreate OpenGL Textures
    glDeleteTextures(1, @rocktexture);
    rocktexture := gld_CreateTexture(RockImage.Picture, False);
  end;
  
  RockToControls;
  filemenuhistory.AddPath(fname);
  SetFileName(fname);
  glneedsupdate := True;
  needsrecalc := True;
  Result := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  closing := True;
  Timer1.Enabled := False;
  undoManager.Free;
  wglMakeCurrent(0, 0);
  wglDeleteContext(rc);

  glDeleteTextures(1, @rocktexture);

  stringtobigstring(filemenuhistory.PathStringIdx(0), @opt_filemenuhistory0);
  stringtobigstring(filemenuhistory.PathStringIdx(1), @opt_filemenuhistory1);
  stringtobigstring(filemenuhistory.PathStringIdx(2), @opt_filemenuhistory2);
  stringtobigstring(filemenuhistory.PathStringIdx(3), @opt_filemenuhistory3);
  stringtobigstring(filemenuhistory.PathStringIdx(4), @opt_filemenuhistory4);
  stringtobigstring(filemenuhistory.PathStringIdx(5), @opt_filemenuhistory5);
  stringtobigstring(filemenuhistory.PathStringIdx(6), @opt_filemenuhistory6);
  stringtobigstring(filemenuhistory.PathStringIdx(7), @opt_filemenuhistory7);
  stringtobigstring(filemenuhistory.PathStringIdx(8), @opt_filemenuhistory8);
  stringtobigstring(filemenuhistory.PathStringIdx(9), @opt_filemenuhistory9);
  pt_SaveSettingsToFile(ChangeFileExt(ParamStr(0), '.ini'));

  filemenuhistory.Free;

  rock.Free;
end;

procedure TForm1.AboutButton1Click(Sender: TObject);
begin
  MessageBox(
    Handle,
    PChar(Format('%s'#13#10 +
    'Version ' + I_VersionBuilt + #13#10 +
    'Copyright (c) 2021, jvalavanis@gmail.com'#13#10 +
    #13#10'A tool to create voxels from md2 models.'#13#10,
        [rsTitle])),
    PChar(rsTitle),
    MB_OK or MB_ICONINFORMATION or MB_APPLMODAL);
end;

procedure TForm1.SaveAsButton1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    filemenuhistory.AddPath(SaveDialog1.FileName);
    BackupFile(SaveDialog1.FileName);
    DoSaveRock(SaveDialog1.FileName);
  end;
end;

procedure TForm1.ExitButton1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.OpenButton1Click(Sender: TObject);
begin
  if not CheckCanClose then
    Exit;

  if OpenDialog1.Execute then
  begin
    DoLoadRock(OpenDialog1.FileName);
    ResetCamera;
  end;
end;

procedure TForm1.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  pt: TPoint;
  r: TRect;
  z: glfloat;
begin
  pt := OpenGLPanel.Parent.ScreenToClient(MousePos);
  r := OpenGLPanel.ClientRect;
  if r.Right > OpenGLScrollBox.Width then
    r.Right := OpenGLScrollBox.Width;
  if r.Bottom > OpenGLScrollBox.Height then
    r.Bottom := OpenGLScrollBox.Height;
  if PtInRect(r, pt) then
  begin
    z := camera.z - 0.5;
    z := z / 0.99;
    camera.z := z + 0.5;
    if camera.z < -20.0 then
      camera.z := -20.0;
    glneedsupdate := True;
  end;
end;

procedure TForm1.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  pt: TPoint;
  r: TRect;
  z: glfloat;
begin
  pt := OpenGLPanel.Parent.ScreenToClient(MousePos);
  r := OpenGLPanel.ClientRect;
  if r.Right > OpenGLScrollBox.Width then
    r.Right := OpenGLScrollBox.Width;
  if r.Bottom > OpenGLScrollBox.Height then
    r.Bottom := OpenGLScrollBox.Height;
  if PtInRect(r, pt) then
  begin
    z := camera.z - 0.5;
    z := z * 0.99;
    camera.z := z + 0.5;
    if camera.z > 0.5 then
      camera.z := 0.5;
    glneedsupdate := True;
  end;
end;

procedure TForm1.OpenGLPanelResize(Sender: TObject);
begin
  glViewport(0, 0, OpenGLPanel.Width, OpenGLPanel.Height);    // Set the viewport for the OpenGL window
  glMatrixMode(GL_PROJECTION);        // Change Matrix Mode to Projection
  glLoadIdentity;                     // Reset View
  gluPerspective(45.0, OpenGLPanel.Width / OpenGLPanel.Height, 1.0, 500.0);  // Do the perspective calculations. Last value = max clipping depth

  glMatrixMode(GL_MODELVIEW);         // Return to the modelview matrix
  glneedsupdate := True;
end;

procedure TForm1.Idle(Sender: TObject; var Done: Boolean);
begin
  if closing then
    Exit;

  Sleep(1);
  UpdateEnable;

  Done := False;

  if needsrecalc then
    glneedsupdate := True;

  if not glneedsupdate then
    // jval: We don't need to render
    Exit;

  UpdateStausbar;

  DoRenderGL;

  glneedsupdate := False;
end;

procedure TForm1.ApplicationEvents1Idle(Sender: TObject;
  var Done: Boolean);
begin
  Idle(Sender, Done);
end;

procedure TForm1.OpenGLPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button in [mbLeft, mbRight] then
  begin
    glpanx := X;
    glpany := Y;
    if Button = mbLeft then
      glmousedown := 1
    else
      glmousedown := 2;
    SetCapture(OpenGLPanel.Handle);
  end;
end;

procedure TForm1.OpenGLPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  glmousedown := 0;
  ReleaseCapture;
end;

procedure TForm1.OpenGLPanelMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if glmousedown = 0 then
    exit;

  if glmousedown = 1 then
  begin
    camera.ay := camera.ay + (glpanx - X) ;/// OpenGLPanel.Width {* 2 * pi};
    camera.ax := camera.ax + (glpany - Y) ; // / OpenGLPanel.Height {* 2 * pi};
  end
  else
  begin
    camera.x := camera.x + (glpanx - X) / OpenGLPanel.Width * (camera.z - 1.0);/// OpenGLPanel.Width {* 2 * pi};
    if camera.x < -6.0 then
      camera.x := -6.0
    else if camera.x > 6.0 then
      camera.x := 6.0;

    camera.y := camera.y - (glpany - Y) / OpenGLPanel.Width * (camera.z - 1.0); // / OpenGLPanel.Height {* 2 * pi};
    if camera.y < -6.0 then
      camera.y := -6.0
    else if camera.y > 6.0 then
      camera.y := 6.0;
  end;

  glneedsupdate := True;

  glpanx := X;
  glpany := Y;
end;

procedure TForm1.OpenGLPanelDblClick(Sender: TObject);
begin
  ResetCamera;
  glneedsupdate := True;
end;

procedure TForm1.Edit1Click(Sender: TObject);
begin
  Undo1.Enabled := undoManager.CanUndo;
  Redo1.Enabled := undoManager.CanRedo;
end;

procedure TForm1.Undo1Click(Sender: TObject);
begin
  if undoManager.CanUndo then
  begin
    undoManager.Undo;
    glneedsupdate := True;
    needsrecalc := True;
  end;
end;

procedure TForm1.Redo1Click(Sender: TObject);
begin
  if undoManager.CanRedo then
  begin
    undoManager.Redo;
    glneedsupdate := True;
    needsrecalc := True;
  end;
end;

procedure TForm1.DoSaveRockBinaryUndo(s: TStream);
begin
  PT_SavePropertiesBinary(rock.mProperties, s);
end;

procedure TForm1.DoLoadRockBinaryUndo(s: TStream);
begin
  PT_LoadPropertiesBinary(rock.mProperties, s);
  RockToControls;
  glneedsupdate := True;
  needsrecalc := True;
end;

procedure TForm1.SaveUndo;
begin
  undoManager.SaveUndo;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  glneedsupdate := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  glneedsupdate := True;
end;

procedure TForm1.UpdateStausbar;
begin
  StatusBar1.Panels[0].Text := Format('Camera(x=%2.2f, y=%2.2f, z=%2.2f)', [camera.x, camera.y, camera.z]);
  StatusBar1.Panels[1].Text := Format('Rendered triangles = %d', [pt_rendredtriangles]);
end;

procedure TForm1.UpdateEnable;
begin
  Undo1.Enabled := undoManager.CanUndo;
  Redo1.Enabled := undoManager.CanRedo;
  UndoButton1.Enabled := undoManager.CanUndo;
  RedoButton1.Enabled := undoManager.CanRedo;
end;

procedure TForm1.OnLoadRockFileMenuHistory(Sender: TObject; const fname: string);
begin
  if not CheckCanClose then
    Exit;

  DoLoadRock(fname);
  ResetCamera;
end;

procedure TForm1.File1Click(Sender: TObject);
begin
  filemenuhistory.RefreshMenuItems;
end;

procedure TForm1.DoRenderGL;
begin
  if glneedsupdate then
  begin
    glBeginScene(OpenGLPanel.Width, OpenGLPanel.Height);
    try
      if needsrecalc then
      begin
        rock.generate;
        needsrecalc := False;
      end;
      glRenderEnviroment;
      glRenderRock(rock);
    finally
      glEndScene(dc);
    end;
  end;
end;

procedure TForm1.Get3dPreviewBitmap(const b: TBitmap);
type
  long_a = array[0..$FFFF] of LongWord;
  Plong_a = ^long_a;
var
  L, buf: Plong_a;
  w, h: integer;
  i, j: integer;
  idx: integer;
begin
  w := OpenGLPanel.Width;
  h := OpenGLPanel.Height;
  b.Width := w;
  b.Height := h;
  b.PixelFormat := pf32bit;

  GetMem(L, w * h * SizeOf(LongWord));
  glReadPixels(0, 0, w, h, GL_BGRA, GL_UNSIGNED_BYTE, L);

  idx := 0;
  for j := 0 to h - 1 do
  begin
    buf := b.ScanLine[h - j - 1];
    for i := 0 to w - 1 do
    begin
      buf[i] := L[idx];
      Inc(idx);
    end;
  end;

  FreeMem(L, w * h * SizeOf(LongWord));
end;

procedure TForm1.Copy1Click(Sender: TObject);
var
  b: TBitmap;
begin
  b := TBitmap.Create;
  try
    DoRenderGL; // JVAL: For some unknown reason this must be called before glReadPixels
    Get3dPreviewBitmap(b);
    Clipboard.Assign(b);
  finally
    b.Free;
  end;
end;

procedure TForm1.Options1Click(Sender: TObject);
begin
  Renderenviroment1.Checked := opt_renderevniroment;
  Wireframe1.Checked := opt_renderwireframe;
end;

procedure TForm1.Wireframe1Click(Sender: TObject);
begin
  opt_renderwireframe := not opt_renderwireframe;
  glneedsupdate := True;
end;

procedure TForm1.RockImageDblClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    RockImage.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    glDeleteTextures(1, @rocktexture);
    rocktexture := gld_CreateTexture(RockImage.Picture, False);
    changed := True;
  end;
end;

procedure TForm1.Renderenviroment1Click(Sender: TObject);
begin
  opt_renderevniroment := not opt_renderevniroment;
  glneedsupdate := True;
end;

procedure TForm1.SeedSpeedButton1Click(Sender: TObject);
var
  x: integer;
begin
  x := StrToIntDef(SeedEdit.Text, -1);
  if x >= 0 then
    if x < MAXINT then
      SeedEdit.Text := IntToStr(x + 1);
end;

procedure TForm1.SeedSpeedButton2Click(Sender: TObject);
var
  x: integer;
begin
  x := StrToIntDef(SeedEdit.Text, -1);
  if x > 0 then
    SeedEdit.Text := IntToStr(x - 1);
end;

procedure TForm1.SeedEditKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then
    Key := #0;
end;

procedure TForm1.RockToSliders;
begin
end;

procedure TForm1.SlidersToLabels;
begin
end;

procedure TForm1.RockToControls;
begin
  if closing then
    Exit;

  SeedEdit.Text := IntToStr(rock.mProperties.mSeed);

  RockToSliders;
  SlidersToLabels;
end;

procedure TForm1.ControlsToRock(Sender: TObject);
begin
  if closing then
    Exit;

  SaveUndo;
  SlidersToLabels;
  rock.mProperties.mSeed := StrToIntDef(SeedEdit.Text, 661);

  needsrecalc := True;
  changed := True;
end;

procedure TForm1.SeedEditChange(Sender: TObject);
begin
  SaveUndo;
  rock.mProperties.mSeed := StrToIntDef(SeedEdit.Text, 661);
  needsrecalc := True;
  changed := True;
end;

procedure TForm1.ExportObjModel1Click(Sender: TObject);
var
  fs: TFileStream;
begin
  if SaveDialog2.Execute then
  begin
    BackupFile(SaveDialog2.FileName);
    fs := TFileStream.Create(SaveDialog2.FileName, fmCreate);
    try

      PT_SaveRockToObj(rock, fs);
    finally
      fs.Free;
    end;
  end;
end;

procedure TForm1.ExportScreenshot1Click(Sender: TObject);
var
  b: TBitmap;
begin
  if SavePictureDialog1.Execute then
  begin
    BackupFile(SavePictureDialog1.FileName);
    b := TBitmap.Create;
    try
      DoRenderGL;
      Get3dPreviewBitmap(b);
      Clipboard.Assign(b);
      b.SaveToFile(SavePictureDialog1.FileName);
    finally
      b.Free;
    end;
  end;
end;

procedure TForm1.Sprite1Click(Sender: TObject);
var
  f: TExportSpriteForm;
begin
  f := TExportSpriteForm.Create(nil);
  try
    f.rock := rock;
    f.rocktex.Canvas.StretchDraw(Rect(0, 0, f.rocktex.Width, f.rocktex.Height), RockImage.Picture.Graphic);
    f.PrepareTextures;
    f.ShowModal;
    if f.ModalResult = mrOK then
      f.DoExportSprite;
  finally
    f.Free;
  end;
end;

procedure TForm1.Voxel1Click(Sender: TObject);
var
  buf: voxelbuffer_p;
  ename: string;
  vox_typ: string;
  sz: integer;
  rocktex: TBitmap;
  f: TExportVoxelForm;
begin
  GetMem(buf, SizeOf(voxelbuffer_t));
  Screen.Cursor := crHourglass;
  try
    f := TExportVoxelForm.Create(nil);
    try
      rocktex := TBitmap.Create;
      rocktex.Width := 256;
      rocktex.Height := 256;
      rocktex.PixelFormat := pf32bit;
      rocktex.Canvas.StretchDraw(Rect(0, 0, rocktex.Width, rocktex.Height), RockImage.Picture.Graphic);
      f.SetRockVoxelParams(rock, buf, rocktex);
      f.ShowModal;
      if f.ModalResult = mrOK then
      begin
        ename := f.FileNameEdit.Text;
        sz := f.voxsize;
        vox_typ := UpperCase(ExtractFileExt(ename));
        if vox_typ = '.VOX' then
        begin
          case f.PatchRadioGroup.ItemIndex of
          0: VXE_ExportVoxelToSlab6VOX(buf, sz, @DoomPaletteRaw, ename);
          1: VXE_ExportVoxelToSlab6VOX(buf, sz, @HereticPaletteRaw, ename);
          2: VXE_ExportVoxelToSlab6VOX(buf, sz, @HexenPaletteRaw, ename);
          3: VXE_ExportVoxelToSlab6VOX(buf, sz, @StrifePaletteRaw, ename);
          else
            VXE_ExportVoxelToSlab6VOX(buf, sz, @RadixPaletteRaw, ename);
          end;
        end
        else
          VXE_ExportVoxelToDDVOX(buf, sz, ename);
      end;
      rocktex.Free;
    finally
      f.Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
  FreeMem(buf, SizeOf(voxelbuffer_t));
end;

procedure TForm1.MD2ModelExportClick(Sender: TObject);
var
  fs: TFileStream;
begin
  if SaveDialog3.Execute then
  begin
    fs := TFileStream.Create(SaveDialog3.FileName, fmCreate);
    SaveRockToMD2Stream(rock, fs, 'rock');
    fs.Free;
  end;
end;

procedure TForm1.ApplicationEvents1Activate(Sender: TObject);
begin
  glneedsupdate := True;
end;

end.

