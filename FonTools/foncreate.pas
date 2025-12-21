{
  FONCreate - Windows FON Font Creation Library for Free Pascal
  
  Creates Windows NE format FON files (both bitmap and vector).
  For command-line applications - no Graphics unit dependency.

  By RetroNick - Initial Code Released Dec 19 - 2025
}
unit FONCreate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  MAX_GLYPHS = 256;
  MAX_STROKES = 1024;

type
  // Stroke command for vector fonts
  TStrokeCmd = (scMoveTo, scLineTo);
  
  TStrokePoint = record
    Cmd: TStrokeCmd;
    X, Y: Integer;
  end;
  
  // Bitmap glyph data
  TBitmapGlyph = record
    Defined: Boolean;
    Width: Integer;
    BitmapData: array of Byte;  // Planar format
  end;
  
  // Vector glyph data
  TVectorGlyph = record
    Defined: Boolean;
    Width: Integer;
    StrokeCount: Integer;
    Strokes: array of TStrokePoint;
  end;

  { TFONBitmapCreator - Creates bitmap FON files }
  TFONBitmapCreator = class
  private
    FGlyphs: array[0..255] of TBitmapGlyph;
    FFontName: string;
    FCopyright: string;
    FPointSize: Integer;
    FHeight: Integer;
    FAscent: Integer;
    FFirstChar: Byte;
    FLastChar: Byte;
    
    procedure WriteWord(Stream: TStream; Value: Word);
    procedure WriteDWord(Stream: TStream; Value: LongWord);
    procedure WriteByte(Stream: TStream; Value: Byte);
    procedure WriteString(Stream: TStream; const S: string; MaxLen: Integer);
    function BuildFNTResource: TMemoryStream;
    function BuildNEExecutable(FNTData: TMemoryStream): TMemoryStream;
    procedure UpdateCharRange;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Set character from raw bitmap data (planar format)
    procedure SetCharacterRaw(CharCode: Byte; CharWidth: Integer; const Data: array of Byte);
    
    // Set character from pattern rows (X=black, space=white)
    procedure SetCharacterPattern(CharCode: Byte; CharWidth: Integer; const Rows: array of string);
    
    // Clear character
    procedure ClearCharacter(CharCode: Byte);
    procedure ClearAll;
    
    // Check if defined
    function IsCharacterDefined(CharCode: Byte): Boolean;
    
    // Save to file
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    
    property FontName: string read FFontName write FFontName;
    property Copyright: string read FCopyright write FCopyright;
    property PointSize: Integer read FPointSize write FPointSize;
    property Height: Integer read FHeight write FHeight;
    property Ascent: Integer read FAscent write FAscent;
  end;

  { TFONVectorCreator - Creates vector FON files }
  TFONVectorCreator = class
  private
    FGlyphs: array[0..MAX_GLYPHS-1] of TVectorGlyph;
    FFontName: string;
    FCopyright: string;
    FPointSize: Integer;
    FHeight: Integer;
    FAscent: Integer;
    
    procedure WriteWord(Stream: TStream; W: Word);
    procedure WriteDWord(Stream: TStream; DW: LongWord);
    procedure WriteByte(Stream: TStream; B: Byte);
    procedure WriteString(Stream: TStream; const S: string; Len: Integer);
    function BuildFontResource: TMemoryStream;
    function BuildNEExecutable(FontRes: TMemoryStream): TMemoryStream;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Begin/End character definition
    procedure BeginChar(CharCode: Integer; CharWidth: Integer);
    procedure MoveTo(CharCode: Integer; X, Y: Integer);
    procedure LineTo(CharCode: Integer; X, Y: Integer);
    
    // Clear
    procedure ClearCharacter(CharCode: Integer);
    procedure ClearAll;
    
    // Check
    function HasCharacter(CharCode: Integer): Boolean;
    
    // Save
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    
    property FontName: string read FFontName write FFontName;
    property Copyright: string read FCopyright write FCopyright;
    property PointSize: Integer read FPointSize write FPointSize;
    property Height: Integer read FHeight write FHeight;
    property Ascent: Integer read FAscent write FAscent;
  end;

implementation

{ TFONBitmapCreator }

constructor TFONBitmapCreator.Create;
var
  I: Integer;
begin
  inherited Create;
  for I := 0 to 255 do
  begin
    FGlyphs[I].Defined := False;
    FGlyphs[I].Width := 0;
    SetLength(FGlyphs[I].BitmapData, 0);
  end;
  FFontName := 'Font';
  FCopyright := '';
  FPointSize := 10;
  FHeight := 16;
  FAscent := 13;
  FFirstChar := 32;
  FLastChar := 127;
end;

destructor TFONBitmapCreator.Destroy;
var
  I: Integer;
begin
  for I := 0 to 255 do
    SetLength(FGlyphs[I].BitmapData, 0);
  inherited Destroy;
end;

procedure TFONBitmapCreator.WriteWord(Stream: TStream; Value: Word);
begin
  Stream.WriteBuffer(Value, 2);
end;

procedure TFONBitmapCreator.WriteDWord(Stream: TStream; Value: LongWord);
begin
  Stream.WriteBuffer(Value, 4);
end;

procedure TFONBitmapCreator.WriteByte(Stream: TStream; Value: Byte);
begin
  Stream.WriteBuffer(Value, 1);
end;

procedure TFONBitmapCreator.WriteString(Stream: TStream; const S: string; MaxLen: Integer);
var
  I: Integer;
  B: Byte;
begin
  for I := 1 to MaxLen do
  begin
    if I <= Length(S) then B := Ord(S[I]) else B := 0;
    WriteByte(Stream, B);
  end;
end;

procedure TFONBitmapCreator.SetCharacterRaw(CharCode: Byte; CharWidth: Integer; const Data: array of Byte);
var
  I: Integer;
begin
  FGlyphs[CharCode].Width := CharWidth;
  SetLength(FGlyphs[CharCode].BitmapData, Length(Data));
  for I := 0 to High(Data) do
    FGlyphs[CharCode].BitmapData[I] := Data[I];
  FGlyphs[CharCode].Defined := True;
end;

procedure TFONBitmapCreator.SetCharacterPattern(CharCode: Byte; CharWidth: Integer; const Rows: array of string);
var
  Row, Col, Plane, NumPlanes, ByteIdx, BitIdx: Integer;
  BmpHeight: Integer;
begin
  BmpHeight := Length(Rows);
  if BmpHeight <> FHeight then
    raise Exception.CreateFmt('Row count (%d) does not match font height (%d)', [BmpHeight, FHeight]);
  
  FGlyphs[CharCode].Width := CharWidth;
  NumPlanes := (CharWidth + 7) div 8;
  SetLength(FGlyphs[CharCode].BitmapData, NumPlanes * BmpHeight);
  
  // Initialize to zero
  for ByteIdx := 0 to Length(FGlyphs[CharCode].BitmapData) - 1 do
    FGlyphs[CharCode].BitmapData[ByteIdx] := 0;
  
  // Convert patterns to planar format
  for Row := 0 to BmpHeight - 1 do
  begin
    for Col := 0 to CharWidth - 1 do
    begin
      if (Col < Length(Rows[Row])) and (Rows[Row][Col + 1] <> ' ') then
      begin
        Plane := Col div 8;
        ByteIdx := Plane * BmpHeight + Row;
        BitIdx := 7 - (Col mod 8);
        FGlyphs[CharCode].BitmapData[ByteIdx] := 
          FGlyphs[CharCode].BitmapData[ByteIdx] or (1 shl BitIdx);
      end;
    end;
  end;
  
  FGlyphs[CharCode].Defined := True;
end;

procedure TFONBitmapCreator.ClearCharacter(CharCode: Byte);
begin
  FGlyphs[CharCode].Defined := False;
  FGlyphs[CharCode].Width := 0;
  SetLength(FGlyphs[CharCode].BitmapData, 0);
end;

procedure TFONBitmapCreator.ClearAll;
var
  I: Integer;
begin
  for I := 0 to 255 do
    ClearCharacter(I);
end;

function TFONBitmapCreator.IsCharacterDefined(CharCode: Byte): Boolean;
begin
  Result := FGlyphs[CharCode].Defined;
end;

procedure TFONBitmapCreator.UpdateCharRange;
var
  I: Integer;
begin
  FFirstChar := 255;
  for I := 0 to 255 do
    if FGlyphs[I].Defined then
    begin
      FFirstChar := I;
      Break;
    end;
  
  FLastChar := 0;
  for I := 255 downto 0 do
    if FGlyphs[I].Defined then
    begin
      FLastChar := I;
      Break;
    end;
  
  if FFirstChar > FLastChar then
  begin
    FFirstChar := 32;
    FLastChar := 32;
  end;
end;

function TFONBitmapCreator.BuildFNTResource: TMemoryStream;
var
  FNT: TMemoryStream;
  CharTableStart, BitmapDataStart, FaceNameOffset, CurrentBitmapOffset: LongWord;
  CharCount, I, NumPlanes, MaxWidth, AvgWidth, TotalWidth, DefinedCount: Integer;
  FNTSize: LongWord;
begin
  FNT := TMemoryStream.Create;
  
  UpdateCharRange;
  CharCount := FLastChar - FFirstChar + 1;
  
  // Calculate widths
  MaxWidth := 0;
  TotalWidth := 0;
  DefinedCount := 0;
  for I := FFirstChar to FLastChar do
    if FGlyphs[I].Defined then
    begin
      if FGlyphs[I].Width > MaxWidth then MaxWidth := FGlyphs[I].Width;
      TotalWidth := TotalWidth + FGlyphs[I].Width;
      Inc(DefinedCount);
    end;
  if DefinedCount > 0 then AvgWidth := TotalWidth div DefinedCount else AvgWidth := 8;
  if MaxWidth = 0 then MaxWidth := 8;
  
  CharTableStart := 118;
  BitmapDataStart := CharTableStart + (CharCount + 1) * 4;
  
  CurrentBitmapOffset := BitmapDataStart;
  for I := FFirstChar to FLastChar do
  begin
    if FGlyphs[I].Defined then
      CurrentBitmapOffset := CurrentBitmapOffset + LongWord(Length(FGlyphs[I].BitmapData))
    else
      CurrentBitmapOffset := CurrentBitmapOffset + LongWord(FHeight);
  end;
  
  FaceNameOffset := CurrentBitmapOffset;
  FNTSize := FaceNameOffset + LongWord(Length(FFontName)) + 1;
  
  // FNT Header (118 bytes)
  WriteWord(FNT, $0200);                    // 0: Version
  WriteDWord(FNT, FNTSize);                 // 2: Size
  WriteString(FNT, FCopyright, 60);         // 6: Copyright
  WriteWord(FNT, $0000);                    // 66: Type (raster)
  WriteWord(FNT, FPointSize);               // 68: Points
  WriteWord(FNT, 96);                       // 70: VertRes
  WriteWord(FNT, 96);                       // 72: HorizRes
  WriteWord(FNT, FAscent);                  // 74: Ascent
  WriteWord(FNT, 0);                        // 76: InternalLeading
  WriteWord(FNT, 0);                        // 78: ExternalLeading
  WriteByte(FNT, 0);                        // 80: Italic
  WriteByte(FNT, 0);                        // 81: Underline
  WriteByte(FNT, 0);                        // 82: StrikeOut
  WriteWord(FNT, 400);                      // 83: Weight
  WriteByte(FNT, 0);                        // 85: CharSet
  WriteWord(FNT, 0);                        // 86: PixWidth
  WriteWord(FNT, FHeight);                  // 88: PixHeight
  WriteByte(FNT, 2);                        // 90: PitchAndFamily
  WriteWord(FNT, AvgWidth);                 // 91: AvgWidth
  WriteWord(FNT, MaxWidth);                 // 93: MaxWidth
  WriteByte(FNT, FFirstChar);               // 95: FirstChar
  WriteByte(FNT, FLastChar);                // 96: LastChar
  WriteByte(FNT, 32 - FFirstChar);          // 97: DefaultChar
  WriteByte(FNT, 32 - FFirstChar);          // 98: BreakChar
  WriteWord(FNT, 0);                        // 99: WidthBytes
  WriteDWord(FNT, 0);                       // 101: Device
  WriteDWord(FNT, FaceNameOffset);          // 105: Face
  WriteDWord(FNT, 0);                       // 109: BitsPointer
  WriteDWord(FNT, BitmapDataStart);         // 113: BitsOffset
  WriteByte(FNT, 0);                        // 117: Reserved
  
  // Character table
  CurrentBitmapOffset := BitmapDataStart;
  for I := FFirstChar to FLastChar do
  begin
    if FGlyphs[I].Defined then
    begin
      WriteWord(FNT, FGlyphs[I].Width);
      WriteWord(FNT, CurrentBitmapOffset);
      CurrentBitmapOffset := CurrentBitmapOffset + LongWord(Length(FGlyphs[I].BitmapData));
    end
    else
    begin
      WriteWord(FNT, 1);
      WriteWord(FNT, CurrentBitmapOffset);
      CurrentBitmapOffset := CurrentBitmapOffset + LongWord(FHeight);
    end;
  end;
  WriteWord(FNT, 0);
  WriteWord(FNT, CurrentBitmapOffset);
  
  // Bitmap data
  for I := FFirstChar to FLastChar do
  begin
    if FGlyphs[I].Defined then
      FNT.WriteBuffer(FGlyphs[I].BitmapData[0], Length(FGlyphs[I].BitmapData))
    else
      for NumPlanes := 1 to FHeight do
        WriteByte(FNT, 0);
  end;
  
  // Face name
  for I := 1 to Length(FFontName) do
    WriteByte(FNT, Ord(FFontName[I]));
  WriteByte(FNT, 0);
  
  Result := FNT;
end;

function TFONBitmapCreator.BuildNEExecutable(FNTData: TMemoryStream): TMemoryStream;
var
  EXE: TMemoryStream;
  NEHeaderOffset, FNTOffset, FNTSize, AlignShift: LongWord;
  I: Integer;
  NameLen: Byte;
begin
  EXE := TMemoryStream.Create;
  FNTSize := FNTData.Size;
  AlignShift := 4;
  NEHeaderOffset := $400;
  FNTOffset := $600;
  
  // MZ Header
  WriteWord(EXE, $5A4D);
  WriteWord(EXE, $0080);
  WriteWord(EXE, $0001);
  WriteWord(EXE, $0000);
  WriteWord(EXE, $0004);
  WriteWord(EXE, $0000);
  WriteWord(EXE, $FFFF);
  WriteWord(EXE, $0000);
  WriteWord(EXE, $00B8);
  WriteWord(EXE, $0000);
  WriteWord(EXE, $0000);
  WriteWord(EXE, $0000);
  WriteWord(EXE, $0040);
  WriteWord(EXE, $0000);
  for I := 1 to 16 do WriteWord(EXE, $0000);
  WriteDWord(EXE, NEHeaderOffset);
  while EXE.Position < $80 do WriteByte(EXE, 0);
  WriteString(EXE, 'Windows font file'#13#10'$', 32);
  while EXE.Position < NEHeaderOffset do WriteByte(EXE, 0);
  
  // NE Header
  WriteWord(EXE, $454E);
  WriteByte(EXE, $05);
  WriteByte(EXE, $0A);
  WriteWord(EXE, $70);    // Entry table offset
  WriteWord(EXE, $0000);
  WriteDWord(EXE, $00000000);
  WriteWord(EXE, $8000);
  WriteWord(EXE, $0000);
  WriteWord(EXE, $0000);
  WriteWord(EXE, $0000);
  WriteDWord(EXE, $00000000);
  WriteDWord(EXE, $00000000);
  WriteWord(EXE, $0000);
  WriteWord(EXE, $0000);
  WriteWord(EXE, $0000);
  WriteWord(EXE, $40);    // Segment table offset
  WriteWord(EXE, $40);    // Resource table offset
  WriteWord(EXE, $60);    // Resident name table offset
  WriteWord(EXE, $70);    // Module ref table offset
  WriteWord(EXE, $70);    // Import name table offset
  WriteDWord(EXE, NEHeaderOffset + $80);
  WriteWord(EXE, $0000);
  WriteWord(EXE, AlignShift);
  WriteWord(EXE, $0001);
  WriteByte(EXE, $02);
  WriteByte(EXE, $08);
  WriteWord(EXE, $0000);
  WriteWord(EXE, $0000);
  WriteWord(EXE, $0000);
  WriteWord(EXE, $0300);
  
  // Resource table
  WriteWord(EXE, AlignShift);
  WriteWord(EXE, $8008);
  WriteWord(EXE, $0001);
  WriteDWord(EXE, $00000000);
  WriteWord(EXE, FNTOffset shr AlignShift);
  WriteWord(EXE, (FNTSize + (1 shl AlignShift) - 1) shr AlignShift);
  WriteWord(EXE, $0C50);
  WriteWord(EXE, $0001);
  WriteDWord(EXE, $00000000);
  WriteWord(EXE, $8007);
  WriteWord(EXE, $0001);
  WriteDWord(EXE, $00000000);
  WriteWord(EXE, (FNTOffset + FNTSize + 15) shr AlignShift);
  WriteWord(EXE, $0010);
  WriteWord(EXE, $0C50);
  WriteWord(EXE, $0001);
  WriteDWord(EXE, $00000000);
  WriteWord(EXE, $0000);
  
  // Resident name table
  NameLen := Length(FFontName);
  if NameLen > 8 then NameLen := 8;
  WriteByte(EXE, NameLen);
  for I := 1 to NameLen do WriteByte(EXE, Ord(UpCase(FFontName[I])));
  WriteWord(EXE, $0000);
  WriteByte(EXE, $00);
  
  while EXE.Position < NEHeaderOffset + $70 do WriteByte(EXE, 0);
  WriteByte(EXE, $00);  // Entry table
  
  while EXE.Position < NEHeaderOffset + $80 do WriteByte(EXE, 0);
  
  // Non-resident name table
  NameLen := Length(FFontName);
  WriteByte(EXE, NameLen);
  for I := 1 to NameLen do WriteByte(EXE, Ord(FFontName[I]));
  WriteWord(EXE, $0000);
  WriteByte(EXE, $00);
  
  while EXE.Position < FNTOffset do WriteByte(EXE, 0);
  
  // FNT resource
  FNTData.Position := 0;
  EXE.CopyFrom(FNTData, FNTData.Size);
  
  while (EXE.Position mod (1 shl AlignShift)) <> 0 do WriteByte(EXE, 0);
  
  // FONTDIR
  WriteWord(EXE, $0001);
  WriteWord(EXE, $0001);
  FNTData.Position := 0;
  for I := 1 to 113 do
    if FNTData.Position < FNTData.Size then EXE.CopyFrom(FNTData, 1) else WriteByte(EXE, 0);
  WriteByte(EXE, 0);
  for I := 1 to Length(FFontName) do WriteByte(EXE, Ord(FFontName[I]));
  WriteByte(EXE, 0);
  
  Result := EXE;
end;

procedure TFONBitmapCreator.SaveToFile(const FileName: string);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TFONBitmapCreator.SaveToStream(Stream: TStream);
var
  FNTData, EXEData: TMemoryStream;
begin
  if FHeight = 0 then
    raise Exception.Create('Font height not set');
  
  FNTData := BuildFNTResource;
  try
    EXEData := BuildNEExecutable(FNTData);
    try
      EXEData.Position := 0;
      Stream.CopyFrom(EXEData, EXEData.Size);
    finally
      EXEData.Free;
    end;
  finally
    FNTData.Free;
  end;
end;

{ TFONVectorCreator }

constructor TFONVectorCreator.Create;
var
  I: Integer;
begin
  inherited Create;
  FFontName := 'Vector';
  FCopyright := '';
  FPointSize := 12;
  FHeight := 16;
  FAscent := 12;
  
  for I := 0 to MAX_GLYPHS - 1 do
  begin
    FGlyphs[I].Width := 0;
    FGlyphs[I].StrokeCount := 0;
    FGlyphs[I].Defined := False;
    SetLength(FGlyphs[I].Strokes, 0);
  end;
end;

destructor TFONVectorCreator.Destroy;
var
  I: Integer;
begin
  for I := 0 to MAX_GLYPHS - 1 do
    SetLength(FGlyphs[I].Strokes, 0);
  inherited Destroy;
end;

procedure TFONVectorCreator.WriteWord(Stream: TStream; W: Word);
begin
  Stream.WriteBuffer(W, 2);
end;

procedure TFONVectorCreator.WriteDWord(Stream: TStream; DW: LongWord);
begin
  Stream.WriteBuffer(DW, 4);
end;

procedure TFONVectorCreator.WriteByte(Stream: TStream; B: Byte);
begin
  Stream.WriteBuffer(B, 1);
end;

procedure TFONVectorCreator.WriteString(Stream: TStream; const S: string; Len: Integer);
var
  I: Integer;
  B: Byte;
begin
  for I := 1 to Len do
  begin
    if I <= Length(S) then B := Ord(S[I]) else B := 0;
    WriteByte(Stream, B);
  end;
end;

procedure TFONVectorCreator.BeginChar(CharCode: Integer; CharWidth: Integer);
begin
  if (CharCode < 0) or (CharCode >= MAX_GLYPHS) then Exit;
  FGlyphs[CharCode].Width := CharWidth;
  FGlyphs[CharCode].StrokeCount := 0;
  SetLength(FGlyphs[CharCode].Strokes, 0);
  FGlyphs[CharCode].Defined := True;
end;

procedure TFONVectorCreator.MoveTo(CharCode: Integer; X, Y: Integer);
var
  Idx: Integer;
begin
  if (CharCode < 0) or (CharCode >= MAX_GLYPHS) then Exit;
  if not FGlyphs[CharCode].Defined then Exit;
  
  Idx := FGlyphs[CharCode].StrokeCount;
  Inc(FGlyphs[CharCode].StrokeCount);
  SetLength(FGlyphs[CharCode].Strokes, FGlyphs[CharCode].StrokeCount);
  FGlyphs[CharCode].Strokes[Idx].Cmd := scMoveTo;
  FGlyphs[CharCode].Strokes[Idx].X := X;
  FGlyphs[CharCode].Strokes[Idx].Y := Y;
end;

procedure TFONVectorCreator.LineTo(CharCode: Integer; X, Y: Integer);
var
  Idx: Integer;
begin
  if (CharCode < 0) or (CharCode >= MAX_GLYPHS) then Exit;
  if not FGlyphs[CharCode].Defined then Exit;
  
  Idx := FGlyphs[CharCode].StrokeCount;
  Inc(FGlyphs[CharCode].StrokeCount);
  SetLength(FGlyphs[CharCode].Strokes, FGlyphs[CharCode].StrokeCount);
  FGlyphs[CharCode].Strokes[Idx].Cmd := scLineTo;
  FGlyphs[CharCode].Strokes[Idx].X := X;
  FGlyphs[CharCode].Strokes[Idx].Y := Y;
end;

procedure TFONVectorCreator.ClearCharacter(CharCode: Integer);
begin
  if (CharCode < 0) or (CharCode >= MAX_GLYPHS) then Exit;
  FGlyphs[CharCode].Width := 0;
  FGlyphs[CharCode].StrokeCount := 0;
  SetLength(FGlyphs[CharCode].Strokes, 0);
  FGlyphs[CharCode].Defined := False;
end;

procedure TFONVectorCreator.ClearAll;
var
  I: Integer;
begin
  for I := 0 to MAX_GLYPHS - 1 do
    ClearCharacter(I);
end;

function TFONVectorCreator.HasCharacter(CharCode: Integer): Boolean;
begin
  Result := (CharCode >= 0) and (CharCode < MAX_GLYPHS) and FGlyphs[CharCode].Defined;
end;

function TFONVectorCreator.BuildFontResource: TMemoryStream;
var
  I, J, CharCount, FirstDef, LastDef: Integer;
  AvgWidth, MaxWidth, WidthSum, DefCount: Integer;
  CurX, CurY, DX, DY: Integer;
  TotalStrokeBytes: Integer;
  StrokeOffsets: array of Word;
  StrokeBytes: array of Byte;
  CharTableOffset, StrokeDataOffset, DeviceNameOffset, FaceNameOffset: LongWord;
begin
  Result := TMemoryStream.Create;
  
  // Find character range
  FirstDef := 255;
  LastDef := 0;
  for I := 0 to 255 do
    if FGlyphs[I].Defined and (FGlyphs[I].StrokeCount > 0) then
    begin
      if I < FirstDef then FirstDef := I;
      if I > LastDef then LastDef := I;
    end;
  
  if FirstDef > LastDef then
  begin
    FirstDef := 32;
    LastDef := 126;
  end;
  
  CharCount := LastDef - FirstDef + 2;  // +1 for range, +1 for sentinel
  
  // Calculate widths
  WidthSum := 0;
  DefCount := 0;
  MaxWidth := 0;
  for I := FirstDef to LastDef do
    if FGlyphs[I].Defined then
    begin
      WidthSum := WidthSum + FGlyphs[I].Width;
      Inc(DefCount);
      if FGlyphs[I].Width > MaxWidth then MaxWidth := FGlyphs[I].Width;
    end;
  if DefCount > 0 then AvgWidth := WidthSum div DefCount else AvgWidth := 8;
  if MaxWidth = 0 then MaxWidth := 8;
  
  // Build stroke data
  SetLength(StrokeBytes, 0);
  SetLength(StrokeOffsets, CharCount);
  TotalStrokeBytes := 0;
  
  for I := 0 to CharCount - 1 do
  begin
    StrokeOffsets[I] := TotalStrokeBytes;
    
    if (FirstDef + I <= LastDef) and FGlyphs[FirstDef + I].Defined and (FGlyphs[FirstDef + I].StrokeCount > 0) then
    begin
      CurX := 0;
      CurY := 0;
      
      for J := 0 to FGlyphs[FirstDef + I].StrokeCount - 1 do
      begin
        DX := FGlyphs[FirstDef + I].Strokes[J].X - CurX;
        DY := FGlyphs[FirstDef + I].Strokes[J].Y - CurY;
        CurX := FGlyphs[FirstDef + I].Strokes[J].X;
        CurY := FGlyphs[FirstDef + I].Strokes[J].Y;
        
        if DX < -128 then DX := -128;
        if DX > 127 then DX := 127;
        if DY < -128 then DY := -128;
        if DY > 127 then DY := 127;
        
        if FGlyphs[FirstDef + I].Strokes[J].Cmd = scMoveTo then
        begin
          SetLength(StrokeBytes, Length(StrokeBytes) + 3);
          StrokeBytes[TotalStrokeBytes] := $80;
          StrokeBytes[TotalStrokeBytes + 1] := Byte(DX);
          StrokeBytes[TotalStrokeBytes + 2] := Byte(DY);
          TotalStrokeBytes := TotalStrokeBytes + 3;
        end
        else
        begin
          if Byte(DX) = $80 then DX := $7F;
          SetLength(StrokeBytes, Length(StrokeBytes) + 2);
          StrokeBytes[TotalStrokeBytes] := Byte(DX);
          StrokeBytes[TotalStrokeBytes + 1] := Byte(DY);
          TotalStrokeBytes := TotalStrokeBytes + 2;
        end;
      end;
    end;
  end;
  
  CharTableOffset := 117;
  StrokeDataOffset := CharTableOffset + LongWord(CharCount * 4);
  DeviceNameOffset := StrokeDataOffset + LongWord(TotalStrokeBytes);
  FaceNameOffset := DeviceNameOffset + 1;
  
  // Write header (117 bytes)
  WriteWord(Result, $0100);
  WriteDWord(Result, FaceNameOffset + LongWord(Length(FFontName)) + 1);
  WriteString(Result, FCopyright, 60);
  WriteWord(Result, $0001);                    // Type = vector
  WriteWord(Result, FPointSize * 20);
  WriteWord(Result, 96);
  WriteWord(Result, 96);
  WriteWord(Result, FAscent);
  WriteWord(Result, 0);
  WriteWord(Result, 0);
  WriteByte(Result, 0);
  WriteByte(Result, 0);
  WriteByte(Result, 0);
  WriteWord(Result, 400);
  WriteByte(Result, 0);
  WriteWord(Result, 0);
  WriteWord(Result, FHeight);
  WriteByte(Result, 2);
  WriteWord(Result, AvgWidth);
  WriteWord(Result, MaxWidth);
  WriteByte(Result, FirstDef);
  WriteByte(Result, LastDef);
  WriteByte(Result, Ord('.') - FirstDef);
  WriteByte(Result, Ord(' ') - FirstDef);
  WriteWord(Result, 0);
  WriteDWord(Result, DeviceNameOffset);
  WriteDWord(Result, FaceNameOffset);
  WriteDWord(Result, 0);
  WriteDWord(Result, StrokeDataOffset);
  
  // Character table
  for I := 0 to CharCount - 1 do
  begin
    WriteWord(Result, StrokeOffsets[I]);
    if (FirstDef + I <= LastDef) and FGlyphs[FirstDef + I].Defined then
      WriteWord(Result, FGlyphs[FirstDef + I].Width)
    else
      WriteWord(Result, AvgWidth);
  end;
  
  // Stroke data
  for I := 0 to TotalStrokeBytes - 1 do
    WriteByte(Result, StrokeBytes[I]);
  
  WriteByte(Result, 0);  // Device name
  
  WriteString(Result, FFontName, Length(FFontName));
  WriteByte(Result, 0);
  
  SetLength(StrokeOffsets, 0);
  SetLength(StrokeBytes, 0);
end;

function TFONVectorCreator.BuildNEExecutable(FontRes: TMemoryStream): TMemoryStream;
var
  FontResSize, FontResOffset, AlignShift: LongWord;
  I: Integer;
begin
  Result := TMemoryStream.Create;
  FontResSize := FontRes.Size;
  AlignShift := 4;
  FontResOffset := $0100;
  
  // MZ Header
  WriteWord(Result, $5A4D);
  WriteWord(Result, $0080);
  WriteWord(Result, $0001);
  WriteWord(Result, $0000);
  WriteWord(Result, $0004);
  WriteWord(Result, $0000);
  WriteWord(Result, $FFFF);
  WriteWord(Result, $0000);
  WriteWord(Result, $00B8);
  WriteWord(Result, $0000);
  WriteWord(Result, $0000);
  WriteWord(Result, $0000);
  WriteWord(Result, $0040);
  WriteWord(Result, $0000);
  for I := 0 to 15 do WriteWord(Result, 0);
  WriteDWord(Result, $00000080);
  while Result.Position < $80 do WriteByte(Result, 0);
  
  // NE Header
  WriteWord(Result, $454E);
  WriteByte(Result, 5);
  WriteByte(Result, 10);
  WriteWord(Result, $0040);
  WriteWord(Result, $0000);
  WriteDWord(Result, $00000000);
  WriteWord(Result, $8000);
  WriteWord(Result, $0000);
  WriteWord(Result, $0000);
  WriteWord(Result, $0000);
  WriteDWord(Result, $00000000);
  WriteDWord(Result, $00000000);
  WriteWord(Result, $0000);
  WriteWord(Result, $0000);
  WriteWord(Result, $0000);
  WriteWord(Result, $0040);
  WriteWord(Result, $0040);
  WriteWord(Result, $0058);
  WriteWord(Result, $0060);
  WriteWord(Result, $0060);
  WriteDWord(Result, $00000000);
  WriteWord(Result, $0000);
  WriteWord(Result, AlignShift);
  WriteWord(Result, $0001);
  WriteByte(Result, $02);
  WriteByte(Result, $00);
  WriteWord(Result, $0000);
  WriteWord(Result, $0000);
  WriteWord(Result, $0000);
  WriteWord(Result, $0300);
  
  // Resource table
  WriteWord(Result, AlignShift);
  WriteWord(Result, $8008);
  WriteWord(Result, $0001);
  WriteDWord(Result, $00000000);
  WriteWord(Result, FontResOffset shr AlignShift);
  WriteWord(Result, (FontResSize + 15) shr AlignShift);
  WriteWord(Result, $1C30);
  WriteWord(Result, $8001);
  WriteDWord(Result, $00000000);
  WriteWord(Result, $0000);
  
  // Resident name table
  WriteByte(Result, Length(FFontName));
  WriteString(Result, FFontName, Length(FFontName));
  WriteWord(Result, $0000);
  WriteByte(Result, $00);
  
  // Module/import tables (empty)
  WriteByte(Result, 0);
  
  // Pad to font resource
  while Result.Position < FontResOffset do WriteByte(Result, 0);
  
  // Font resource
  FontRes.Position := 0;
  Result.CopyFrom(FontRes, FontRes.Size);
end;

procedure TFONVectorCreator.SaveToFile(const FileName: string);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TFONVectorCreator.SaveToStream(Stream: TStream);
var
  FontRes, NEExe: TMemoryStream;
begin
  FontRes := BuildFontResource;
  try
    NEExe := BuildNEExecutable(FontRes);
    try
      NEExe.Position := 0;
      Stream.CopyFrom(NEExe, NEExe.Size);
    finally
      NEExe.Free;
    end;
  finally
    FontRes.Free;
  end;
end;

end.
