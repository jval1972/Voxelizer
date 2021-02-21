//------------------------------------------------------------------------------
//
//  DOOMTREE: Doom Tree Sprite Generator
//  Copyright (C) 2021 by Jim Valavanis
//
// DESCRIPTION:
//  Export Sprite Form
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/doom-tree/
//------------------------------------------------------------------------------

unit frm_exportsprite;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, dt_soft3d, proctree, ComCtrls;

type
  TExportSpriteForm = class(TForm)
    Label1: TLabel;
    FileNameEdit: TEdit;
    SelectFileButton: TSpeedButton;
    GeneralGroupBox: TGroupBox;
    Label2: TLabel;
    SpritePrefixButton: TSpeedButton;
    PrefixEdit: TEdit;
    PatchRadioGroup: TRadioGroup;
    PreviewGroupBox: TGroupBox;
    Panel3: TPanel;
    PaintBox1: TPaintBox;
    Panel4: TPanel;
    ZoomInSpeedButton: TSpeedButton;
    ZoomOutSpeedButton: TSpeedButton;
    HourglassLabel: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    SaveDialog1: TSaveDialog;
    ZoomTrackBar: TTrackBar;
    RotateTrackBar: TTrackBar;
    Theta2IncButton1: TSpeedButton;
    Theta2DecButton1: TSpeedButton;
    ScriptParametersGroupBox: TGroupBox;
    ScriptRadioGroup: TRadioGroup;
    Label3: TLabel;
    ActorNameEdit: TEdit;
    Label4: TLabel;
    EditorNumberEdit: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    RadiusEdit: TEdit;
    HeightLabel: TLabel;
    HeightEdit: TEdit;
    Timer1: TTimer;
    procedure SpritePrefixButtonClick(Sender: TObject);
    procedure SelectFileButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FileNameEditChange(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure ZoomInSpeedButtonClick(Sender: TObject);
    procedure ZoomOutSpeedButtonClick(Sender: TObject);
    procedure ZoomTrackBarChange(Sender: TObject);
    procedure RotateTrackBarChange(Sender: TObject);
    procedure Theta2IncButton1Click(Sender: TObject);
    procedure Theta2DecButton1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CheckNumericEdit(Sender: TObject; var Key: Char);
    procedure ScriptRadioGroupClick(Sender: TObject);
  private
    { Private declarations }
    needs3dupdate: boolean;
    buffer: TBitmap;
    device: device_t;
    fdevicewidth, fdeviceheight: integer;
    fviewdist, ftheta, ftheta2: float;
    procedure RenderFaces(const mVertCount, mFaceCount: integer;
      const mVert, mNormal: array of fvec3_t; const mUV: array of fvec2_t;
      const mFace: array of ivec3_t; const tex: TBitmap);
    procedure DoUpdate3d;
    procedure UpdateControls;
    function CheckOKtoGO: boolean;
  public
    { Public declarations }
    twigtex, trunktex: TBitmap;
    tree: tree_t;
    procedure DoExportSpriteWAD;
  end;

implementation

{$R *.dfm}

uses
  Math,
  dt_defs,
  dt_utils,
  dt_wadwriter,
  dt_doompatch,
  dt_palettes,
  frm_spriteprefix;

const
  MINVIEWDIST = 1.0;
  MAXVIEWDIST = 16.0;

procedure TExportSpriteForm.SpritePrefixButtonClick(Sender: TObject);
var
  s: string;
begin
  s := PrefixEdit.Text;
  if GetSpritePrefix(s) then
    PrefixEdit.Text := s;
end;

procedure TExportSpriteForm.SelectFileButtonClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    FileNameEdit.Text := SaveDialog1.FileName;
    Button1.Enabled := True;
  end;
end;

procedure TExportSpriteForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  DoubleBuffered := True;
  for i := 0 to ComponentCount - 1 do
    if Components[i].InheritsFrom(TWinControl) then
      (Components[i] as TWinControl).DoubleBuffered := True;

  fdevicewidth := 256;
  fdeviceheight := 510;
  device_init(@device, fdevicewidth, fdeviceheight);
  buffer := TBitmap.Create;
  buffer.Width := fdevicewidth;
  buffer.Height := fdeviceheight;
  buffer.PixelFormat := pf32bit;
  needs3dupdate := True;
  twigtex := TBitmap.Create;
  twigtex.Width := 256;
  twigtex.Height := 256;
  twigtex.PixelFormat := pf32bit;
  trunktex := TBitmap.Create;
  trunktex.Width := 256;
  trunktex.Height := 256;
  trunktex.PixelFormat := pf32bit;

  fviewdist := opt_viewdist / OPT_TO_FLOAT;
  if fviewdist < MINVIEWDIST then
    fviewdist := MINVIEWDIST
  else if fviewdist > MAXVIEWDIST then
    fviewdist := MAXVIEWDIST;

  ftheta := opt_theta1 / OPT_TO_FLOAT;
  if ftheta < 0.0 then
    ftheta := 0.0
  else if ftheta > 2 * PI then
    ftheta := 2 * PI;

  ftheta2 := opt_theta2 / OPT_TO_FLOAT;
  if ftheta2 < 0.0 then
    ftheta2 := 0.0
  else if ftheta2 > PI / 4 then
    ftheta2 := PI / 4;

  UpdateControls;
end;

procedure TExportSpriteForm.FormDestroy(Sender: TObject);
begin
  device_destroy(@device);
  twigtex.Free;
  trunktex.Free;
  buffer.Free;

  opt_viewdist := Round(fviewdist * OPT_TO_FLOAT);
  opt_theta1 := Round(ftheta * OPT_TO_FLOAT);
  opt_theta2 := Round(ftheta2 * OPT_TO_FLOAT);
end;

procedure TExportSpriteForm.FileNameEditChange(Sender: TObject);
begin
  if Trim(FileNameEdit.Text) = '' then
    Button1.Enabled := False;
end;

procedure TExportSpriteForm.RenderFaces(const mVertCount, mFaceCount: integer;
  const mVert, mNormal: array of fvec3_t; const mUV: array of fvec2_t;
  const mFace: array of ivec3_t; const tex: TBitmap);
var
  v1, v2, v3: vertex_t;
  i: integer;
  procedure _make_vertex(const r: integer; const p: Pvertex_t);
  begin
    p.pos.x := mVert[r].x;
    p.pos.y := mVert[r].z;
    p.pos.z := mVert[r].y;
    p.pos.w := 1.0;
    p.tc.u := mUV[r].u;
    p.tc.v := mUV[r].v;
    p.color.r := 1.0;
    p.color.g := 1.0;
    p.color.g := 1.0;
    p.color.b := 1.0;
    p.rhw := 1.0;
  end;
begin
  device_set_texture(@device, tex);
  for i := 0 to mFaceCount - 1 do
  begin
    _make_vertex(mFace[i].x, @v1);
    _make_vertex(mFace[i].y, @v2);
    _make_vertex(mFace[i].z, @v3);
    device_draw_primitive(@device, @v1, @v2, @v3);
  end;
end;

procedure TExportSpriteForm.DoUpdate3d;
var
  x, y: integer;
  src, dest: PIUINT32Array;
  c, m, m2: matrix_t;
begin
  device_clear(@device);
  camera_at_zero(@device, fviewdist, 0, 0);
  matrix_set_rotate(@m, 0.0, 0.0, 1.0, ftheta);
  matrix_set_rotate(@m2, 0.0, 1.0, 0.0, ftheta2);
  matrix_mul(@c, @m, @m2);
	device.transform.world := c;
	transform_update(@device.transform);
  device.render_state := RENDER_STATE_TEXTURE_SOLID;
  RenderFaces(tree.mVertCount, tree.mFaceCount, tree.mVert, tree.mNormal,
    tree.mUV, tree.mFace, trunktex);
  if opt_rendertwig then
  begin
    device.render_state := RENDER_STATE_TEXTURE_ALPHAZERO;
    RenderFaces(tree.mTwigVertCount, tree.mTwigFaceCount, tree.mTwigVert, tree.mTwigNormal,
      tree.mTwigUV, tree.mTwigFace, twigtex);
  end;

  buffer.Canvas.StretchDraw(Rect(0, 0, buffer.Width, buffer.Height), device.bframebuffer);
  needs3dupdate := False;
{  buffer.Width := device.width;
  buffer.Height := device.Height;
  buffer.PixelFormat := pf32bit;
  for y := 0 to device.height - 1 do
  begin
    dest := buffer.ScanLine[y];
    src := device.framebuffer[y];
    for x := 0 to device.width - 1 do
      dest[x] := src[x];
  end;}
end;

procedure TExportSpriteForm.PaintBox1Paint(Sender: TObject);
begin
  if needs3dupdate then
    DoUpdate3d;
  PaintBox1.Canvas.Draw(0, 0, buffer);
end;

procedure TExportSpriteForm.ZoomInSpeedButtonClick(Sender: TObject);
begin
  if fviewdist > MINVIEWDIST then
  begin
    fviewdist := fviewdist - 0.5;
    if fviewdist < MINVIEWDIST then
      fviewdist := MINVIEWDIST;
    needs3dupdate := True;
    UpdateControls;
    PaintBox1.Invalidate;
  end;
end;

procedure TExportSpriteForm.ZoomOutSpeedButtonClick(Sender: TObject);
begin
  if fviewdist < MAXVIEWDIST then
  begin
    fviewdist := fviewdist + 0.5;
    if fviewdist > MAXVIEWDIST then
      fviewdist := MAXVIEWDIST;
    needs3dupdate := True;
    UpdateControls;
    PaintBox1.Invalidate;
  end;
end;

procedure TExportSpriteForm.UpdateControls;
begin
  ZoomTrackBar.Position := GetIntegerInRange(Round(fviewdist * 10), ZoomTrackBar.Min, ZoomTrackBar.Max);
  RotateTrackBar.Position := GetIntegerInRange(Round(ftheta / (2 * pi) * RotateTrackBar.Max), RotateTrackBar.Min, RotateTrackBar.Max);
end;

procedure TExportSpriteForm.ZoomTrackBarChange(Sender: TObject);
begin
  fviewdist := ZoomTrackBar.Position / 10;
  needs3dupdate := True;
  PaintBox1.Invalidate;
end;

procedure TExportSpriteForm.RotateTrackBarChange(Sender: TObject);
begin
  ftheta := RotateTrackBar.Position / RotateTrackBar.Max * 2 * PI;
  needs3dupdate := True;
  PaintBox1.Invalidate;
end;

procedure TExportSpriteForm.Theta2IncButton1Click(Sender: TObject);
begin
  if ftheta2 < PI / 4 then
  begin
    ftheta2 := ftheta2 + PI / 32;
    if ftheta2 > PI / 4 then
      ftheta2 := PI / 4;
    needs3dupdate := True;
    UpdateControls;
    PaintBox1.Invalidate;
  end;
end;

procedure TExportSpriteForm.Theta2DecButton1Click(Sender: TObject);
begin
  if ftheta2 > 0.0 then
  begin
    ftheta2 := ftheta2 - PI / 32;
    if ftheta2 < 0.0 then
      ftheta2 := 0.0;
    needs3dupdate := True;
    UpdateControls;
    PaintBox1.Invalidate;
  end;
end;

function TExportSpriteForm.CheckOKtoGO: boolean;
var
  s: string;
begin
  if Trim(FileNameEdit.Text) = '' then
  begin
    Result := False;
    Exit;
  end;

  if Length(Trim(PrefixEdit.Text)) <> 5 then
  begin
    Result := False;
    Exit;
  end;

  s := Trim(PrefixEdit.Text);
  if Pos(s[5], 'ABCDEFGHIJKLMNOPQRSTUVWXYZ\[]') = 0 then
  begin
    Result := False;
    Exit;
  end;

  if Pos(' ', s) > 0 then
  begin
    Result := False;
    Exit;
  end;


  if Length(Trim(ActorNameEdit.Text)) = 0 then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

procedure TExportSpriteForm.Timer1Timer(Sender: TObject);
begin
  Button1.Enabled := CheckOKtoGO;
end;

procedure TExportSpriteForm.CheckNumericEdit(Sender: TObject;
  var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then
  begin
    Key := #0;
    Exit;
  end;
end;

procedure TExportSpriteForm.ScriptRadioGroupClick(Sender: TObject);
begin
  ScriptParametersGroupBox.Visible := ScriptRadioGroup.ItemIndex <> 2;
end;

procedure TExportSpriteForm.DoExportSpriteWAD;
var
  wad: TWADWriter;
  script: TStringList;
  name: string;
  stmp: string;
  i: integer;
  ms: TMemoryStream;
  b: TBitmap;
begin
  Screen.Cursor := crHourGlass;
  wad := TWADWriter.Create;
  try
    script := TStringList.Create;
    try
      if ScriptRadioGroup.ItemIndex <> 2 then
      begin
        stmp := Trim(ActorNameEdit.Text);
        name := '';
        for i := 1 to Length(stmp) do
          if stmp[i] <> ' ' then
            name := name + stmp[i];
        script.Add('ACTOR ' + stmp + ' ' + EditorNumberEdit.Text);
        script.Add('{');
        script.Add('  Health 10000');
        script.Add('  Radius ' + RadiusEdit.Text);
        script.Add('  Height ' + HeightEdit.Text);
        script.Add('  Mass 100000');
        script.Add('  +SOLID');
        script.Add('  States');
        script.Add('  {');
        script.Add('  Spawn:');
        stmp := PrefixEdit.Text;
        script.Add('    ' + stmp[1] + stmp[2] + stmp[3] + stmp[4] + ' ' + stmp[5] + ' -1');
        script.Add('  }');
        script.Add('}');
        if ScriptRadioGroup.ItemIndex = 0 then
          wad.AddStringList('ACTORDEF', script)
        else
          wad.AddStringList('DECORATE', script);
      end;
    finally
      script.Free;
    end;
    wad.AddSeparator('S_START');

    b := TBitmap.Create;
    b.Width := 256;
    b.Height := 255;
    b.PixelFormat := pf32bit;
    b.Canvas.Draw(0, 0, buffer);
    for i := 0 to 255 do
      b.Canvas.Pixels[i, 0] := RGB(0, 0, 0);
    case PatchRadioGroup.ItemIndex of
    0: ms := BmpAsPatch(b, @DoomPaletteRaw);
    1: ms := BmpAsPatch(b, @HereticPaletteRaw);
    2: ms := BmpAsPatch(b, @HexenPaletteRaw);
    3: ms := BmpAsPatch(b, @StrifePaletteRaw);
    else
      ms := BmpAsPatch(b, @RadixPaletteRaw);
    end;
    wad.AddData(PrefixEdit.Text + '0', ms.Memory, ms.Size);
    ms.Free;
    b.Free;
    wad.AddSeparator('S_END');
    BackupFile(FileNameEdit.Text);
    wad.SaveToFile(FileNameEdit.Text);
  finally
    wad.Free;
  end;
  Screen.Cursor := crDefault;
end;

end.
