{ ChrInfo - Displays information about CHR Vector fonts
  Displays Font Name,Copyright,Width,Height, and various other bits of information
  
  By RetroNick - Initial Code Released Dec 19 - 2025
}
program CHRInfo;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

const
  ProgramName = 'ChrInfo v1.0 By RetroNick - Released Dec 19 - 2025';

type
  TCHRHeader = packed record
    Signature: array[0..1] of Char;
    Description: array[0..252] of Char;
    HeaderStart: Byte;
    HeaderSize: Word;
    FontName: array[0..3] of Char;
    FontSize: Word;
    FontMajor: Byte;
    FontMinor: Byte;
    RevisionMajor: Byte;
    RevisionMinor: Byte;
  end;

  TStrokeHeader = packed record
    StrokeCheck: Byte;
    CharacterCount: SmallInt;
    Undefined1: Byte;
    StartingChar: Byte;
    StrokesOffset: SmallInt;
    ScanFlag: ShortInt;
    OriginToAscender: ShortInt;
    OriginToBaseline: ShortInt;
    OriginToDescender: ShortInt;
    FontNameField: array[0..3] of Char;
    Undefined2: Byte;
  end;

  TPoint = record
    X, Y: Integer;
  end;

  TStroke = record
    Points: array of TPoint;
    IsMove: array of Boolean;
  end;

  TCHRFont = class
  private
    FStream: TFileStream;
    FHeader: TCHRHeader;
    FStrokeHeader: TStrokeHeader;
    FCharOffsets: array of Word;
    FCharWidths: array of Byte;
    FDescription: string;
    FStrokeDataStart: Int64;

    function ReadDescription: string;
    procedure LoadHeaders;
    function DecodeCharacter(CharIndex: Integer): TStroke;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;

    procedure DisplayInfo;
    procedure DisplayCharacter(Ch: Char; Scale: Integer = 1);
    procedure DisplayAllChars(Scale: Integer = 1);
    procedure DisplayCharacterTable;
    procedure DisplayStrokeCommands(Ch: Char);
    procedure DisplayUndefinedCharacters;

    property Description: string read FDescription;
    property StartingChar: Byte read FStrokeHeader.StartingChar;
    property CharacterCount: SmallInt read FStrokeHeader.CharacterCount;
    property OriginToAscender: ShortInt read FStrokeHeader.OriginToAscender;
    property OriginToDescender: ShortInt read FStrokeHeader.OriginToDescender;
  end;

constructor TCHRFont.Create(const FileName: string);
begin
  inherited Create;
  FStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  LoadHeaders;
end;

destructor TCHRFont.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

function TCHRFont.ReadDescription: string;
var
  Ch: Char;
  i: Integer;
begin
  Result := '';
  FStream.Position := 2;

  for i := 0 to 252 do
  begin
    FStream.ReadBuffer(Ch, 1);
    if Ch = #$1A then
      Break;
    if Ord(Ch) >= 32 then
      Result := Result + Ch;
  end;
end;

procedure TCHRFont.LoadHeaders;
var
  i: Integer;
  HeaderStart: Int64;
begin
  FStream.Position := 0;

  FDescription := ReadDescription;

  FStream.Position := 0;
  FStream.ReadBuffer(FHeader.Signature, 2);

  if (FHeader.Signature[0] <> 'P') or (FHeader.Signature[1] <> 'K') then
    raise Exception.Create('Invalid CHR file: wrong signature');

  while FStream.Position < 256 do
  begin
    FStream.ReadBuffer(FHeader.HeaderStart, 1);
    if FHeader.HeaderStart = $1A then
      Break;
  end;

  FStream.ReadBuffer(FHeader.HeaderSize, 2);
  FStream.ReadBuffer(FHeader.FontName, 4);
  FStream.ReadBuffer(FHeader.FontSize, 2);
  FStream.ReadBuffer(FHeader.FontMajor, 1);
  FStream.ReadBuffer(FHeader.FontMinor, 1);
  FStream.ReadBuffer(FHeader.RevisionMajor, 1);
  FStream.ReadBuffer(FHeader.RevisionMinor, 1);

  HeaderStart := FHeader.HeaderSize;
  FStream.Position := HeaderStart;

  FStream.ReadBuffer(FStrokeHeader, SizeOf(TStrokeHeader));

  if FStrokeHeader.StrokeCheck <> $2B then
    raise Exception.Create('Invalid CHR file: not a stroked font');

  SetLength(FCharOffsets, FStrokeHeader.CharacterCount);
  SetLength(FCharWidths, FStrokeHeader.CharacterCount);

  for i := 0 to FStrokeHeader.CharacterCount - 1 do
    FStream.ReadBuffer(FCharOffsets[i], 2);

  for i := 0 to FStrokeHeader.CharacterCount - 1 do
    FStream.ReadBuffer(FCharWidths[i], 1);

  FStrokeDataStart := HeaderStart + FStrokeHeader.StrokesOffset;
end;

function TCHRFont.DecodeCharacter(CharIndex: Integer): TStroke;
var
  XByte, YByte: Byte;
  XOp, YOp: Byte;
  XVal, YVal: ShortInt;
  Finished: Boolean;
  Count: Integer;
begin
  SetLength(Result.Points, 0);
  SetLength(Result.IsMove, 0);
  Count := 0;

  if (CharIndex < 0) or (CharIndex >= FStrokeHeader.CharacterCount) then
    Exit;

  FStream.Position := FStrokeDataStart + FCharOffsets[CharIndex];

  Finished := False;
  while not Finished do
  begin
    FStream.ReadBuffer(XByte, 1);
    FStream.ReadBuffer(YByte, 1);

    XOp := (XByte shr 7) and 1;
    YOp := (YByte shr 7) and 1;

    if (XOp = 0) and (YOp = 0) then
    begin
      Finished := True;
    end
    else if (XOp = 0) and (YOp = 1) then
    begin
      // Scan flag - ignore
    end
    else
    begin
      // CORRECTED: Decode 7-bit two's complement values
      // If bit 6 is set, sign extend from bit 6 to bit 7
      if (XByte and $40) <> 0 then
        XVal := ShortInt(XByte or $80)  // Sign extend
      else
        XVal := ShortInt(XByte and $7F); // Positive value

      if (YByte and $40) <> 0 then
        YVal := ShortInt(YByte or $80)  // Sign extend
      else
        YVal := ShortInt(YByte and $7F); // Positive value

      // Note: CHR format uses Y+ = up from baseline
      // For display purposes, we negate Y to convert to screen coordinates
      YVal := -YVal;

      SetLength(Result.Points, Count + 1);
      SetLength(Result.IsMove, Count + 1);

      Result.Points[Count].X := XVal;
      Result.Points[Count].Y := YVal;

      if (XOp = 1) and (YOp = 0) then
        Result.IsMove[Count] := True
      else
        Result.IsMove[Count] := False;

      Inc(Count);
    end;
  end;
end;

procedure TCHRFont.DisplayInfo;
var
  FontName: string;
  Height: Integer;
begin
  FontName := string(FHeader.FontName);
  Height := FStrokeHeader.OriginToAscender - FStrokeHeader.OriginToDescender;

  WriteLn('===========================================================');
  WriteLn('                    CHR FONT INFORMATION');
  WriteLn('===========================================================');
  WriteLn;
  WriteLn('Font Name:          ', FontName);
  WriteLn('Description:        ', FDescription);
  WriteLn('Header Size:        ', FHeader.HeaderSize, ' bytes');
  WriteLn('Font Data Size:     ', FHeader.FontSize, ' bytes');
  WriteLn('Version:            ', FHeader.FontMajor, '.', FHeader.FontMinor);
  WriteLn('Revision:           ', FHeader.RevisionMajor, '.', FHeader.RevisionMinor);
  WriteLn;
  WriteLn('Character Count:    ', FStrokeHeader.CharacterCount);
  WriteLn('Starting Character: ', FStrokeHeader.StartingChar, ' (''', Chr(FStrokeHeader.StartingChar), ''')');
  WriteLn('Character Height:   ', Height, ' units');
  WriteLn('Origin to Ascender: ', FStrokeHeader.OriginToAscender);
  WriteLn('Origin to Baseline: ', FStrokeHeader.OriginToBaseline);
  WriteLn('Origin to Descender:', FStrokeHeader.OriginToDescender);
  WriteLn('Scan Flag:          ', FStrokeHeader.ScanFlag);
  WriteLn;
  WriteLn('===========================================================');
  WriteLn;
end;

procedure TCHRFont.DisplayCharacter(Ch: Char; Scale: Integer = 1);
var
  CharIndex: Integer;
  Stroke: TStroke;
  i, x, y: Integer;
  MinX, MaxX, MinY, MaxY: Integer;
  Width, Height: Integer;
  Grid: array of array of Char;
  LastX, LastY: Integer;
  dx, dy, Steps, Step: Integer;
begin
  CharIndex := Ord(Ch) - FStrokeHeader.StartingChar;

  if (CharIndex < 0) or (CharIndex >= FStrokeHeader.CharacterCount) then
  begin
    WriteLn('Character ''', Ch, ''' not in font range');
    Exit;
  end;

  Stroke := DecodeCharacter(CharIndex);

  if Length(Stroke.Points) = 0 then
  begin
    WriteLn('Character ''', Ch, ''' has no stroke data');
    Exit;
  end;

  MinX := 9999; MaxX := -9999;
  MinY := 9999; MaxY := -9999;

  for i := 0 to High(Stroke.Points) do
  begin
    if Stroke.Points[i].X < MinX then MinX := Stroke.Points[i].X;
    if Stroke.Points[i].X > MaxX then MaxX := Stroke.Points[i].X;
    if Stroke.Points[i].Y < MinY then MinY := Stroke.Points[i].Y;
    if Stroke.Points[i].Y > MaxY then MaxY := Stroke.Points[i].Y;
  end;

  Width := (MaxX - MinX + 3) * Scale;
  Height := (MaxY - MinY + 3) * Scale;

  SetLength(Grid, Height, Width);
  for y := 0 to Height - 1 do
    for x := 0 to Width - 1 do
      Grid[y][x] := ' ';

  LastX := -1;
  LastY := -1;

  for i := 0 to High(Stroke.Points) do
  begin
    x := (Stroke.Points[i].X - MinX + 1) * Scale;
    y := (Stroke.Points[i].Y - MinY + 1) * Scale;

    if not Stroke.IsMove[i] and (LastX >= 0) and (LastY >= 0) then
    begin
      dx := Abs(x - LastX);
      dy := Abs(y - LastY);

      if dx > dy then
        Steps := dx
      else
        Steps := dy;

      for Step := 0 to Steps do
      begin
        if Steps > 0 then
        begin
          if (LastY + (y - LastY) * Step div Steps >= 0) and
             (LastY + (y - LastY) * Step div Steps < Height) and
             (LastX + (x - LastX) * Step div Steps >= 0) and
             (LastX + (x - LastX) * Step div Steps < Width) then
          begin
            Grid[LastY + (y - LastY) * Step div Steps]
                [LastX + (x - LastX) * Step div Steps] := '#';
          end;
        end;
      end;
    end;

    LastX := x;
    LastY := y;
  end;

  WriteLn('Character: ''', Ch, ''' (ASCII ', Ord(Ch), ')');
  WriteLn('Width: ', FCharWidths[CharIndex], ' units');
  WriteLn('Strokes: ', Length(Stroke.Points));
  WriteLn;

  for y := 0 to Height - 1 do
  begin
    for x := 0 to Width - 1 do
      Write(Grid[y][x]);
    WriteLn;
  end;
  WriteLn;
end;

procedure TCHRFont.DisplayAllChars(Scale: Integer = 1);
var
  Ch: Char;
  CharIndex: Integer;
begin
  for CharIndex := 0 to FStrokeHeader.CharacterCount - 1 do
  begin
    Ch := Chr(FStrokeHeader.StartingChar + CharIndex);
    if (Ord(Ch) >= 32) and (Ord(Ch) <= 126) then
      DisplayCharacter(Ch, Scale);
  end;
end;

procedure TCHRFont.DisplayCharacterTable;
var
  CharIndex: Integer;
  Ch: Char;
begin
  WriteLn('Character Width Table:');
  WriteLn('===========================================================');
  WriteLn(' Idx  Chr  ASCII  Width   Offset');
  WriteLn('===========================================================');
  
  for CharIndex := 0 to FStrokeHeader.CharacterCount - 1 do
  begin
    Ch := Chr(FStrokeHeader.StartingChar + CharIndex);

    Write(Format('%4d', [CharIndex]));
    Write('   ');

    if (Ord(Ch) >= 32) and (Ord(Ch) <= 126) then
      Write(Ch, '  ')
    else
      Write('?  ');

    Write(Format('%5d', [Ord(Ch)]));
    Write(Format('%7d', [FCharWidths[CharIndex]]));
    WriteLn(Format('%9d', [FCharOffsets[CharIndex]]));
  end;
  
  WriteLn('===========================================================');
  WriteLn;
end;

procedure TCHRFont.DisplayStrokeCommands(Ch: Char);
var
  CharIndex: Integer;
  XByte, YByte: Byte;
  XOp, YOp: Byte;
  XVal, YVal: ShortInt;
  Finished: Boolean;
  CmdNum: Integer;
  OpName: string;
  AbsolutePosition: Int64;
begin
  CharIndex := Ord(Ch) - FStrokeHeader.StartingChar;

  if (CharIndex < 0) or (CharIndex >= FStrokeHeader.CharacterCount) then
  begin
    WriteLn('Character ''', Ch, ''' not in font range');
    Exit;
  end;

  AbsolutePosition := FStrokeDataStart + FCharOffsets[CharIndex];

  WriteLn('Stroke Commands for Character ''', Ch, ''' (ASCII ', Ord(Ch), ')');
  WriteLn('===========================================================');
  WriteLn('Character Width: ', FCharWidths[CharIndex], ' units');
  WriteLn('Relative Offset: ', FCharOffsets[CharIndex], ' (from stroke data start)');
  WriteLn('Absolute File Position: ', AbsolutePosition, ' (0x', IntToHex(AbsolutePosition, 1), ')');
  WriteLn('  Calculation: Header(', FHeader.HeaderSize, ') + StrokesOffset(',
          FStrokeHeader.StrokesOffset, ') + CharOffset(', FCharOffsets[CharIndex], ')');
  WriteLn;
  WriteLn('  #  XByte YByte  X Opc Y Opc   X    Y   Command');
  WriteLn('===========================================================');
  FStream.Position := FStrokeDataStart + FCharOffsets[CharIndex];

  CmdNum := 0;
  Finished := False;

  while not Finished do
  begin
    FStream.ReadBuffer(XByte, 1);
    FStream.ReadBuffer(YByte, 1);

    XOp := (XByte shr 7) and 1;
    YOp := (YByte shr 7) and 1;

    Write(Format('%3d', [CmdNum]));
    Write('  ');
    Write(Format('0x%02X', [XByte]));
    Write(' ');
    Write(Format('0x%02X', [YByte]));
    Write('   ');
    Write(XOp);
    Write('     ');
    Write(YOp);
    Write('   ');

    if (XOp = 0) and (YOp = 0) then
    begin
      WriteLn('  -    -   END OF CHARACTER');
      Finished := True;
    end
    else if (XOp = 0) and (YOp = 1) then
    begin
      WriteLn('  -    -   SCAN (ignored)');
    end
    else
    begin
      // CORRECTED: Decode 7-bit two's complement values
      if (XByte and $40) <> 0 then
        XVal := ShortInt(XByte or $80)  // Sign extend
      else
        XVal := ShortInt(XByte and $7F); // Positive value

      if (YByte and $40) <> 0 then
        YVal := ShortInt(YByte or $80)  // Sign extend
      else
        YVal := ShortInt(YByte and $7F); // Positive value

      Write(Format('%4d', [XVal]));
      Write(' ');
      Write(Format('%4d', [YVal]));
      Write('   ');

      if (XOp = 1) and (YOp = 0) then
        OpName := 'MOVE to (' + IntToStr(XVal) + ',' + IntToStr(YVal) + ')'
      else if (XOp = 1) and (YOp = 1) then
        OpName := 'LINE to (' + IntToStr(XVal) + ',' + IntToStr(YVal) + ')'
      else
        OpName := 'UNKNOWN';

      WriteLn(OpName);
    end;

    Inc(CmdNum);
  end;

  WriteLn('===========================================================');
  WriteLn('Total commands: ', CmdNum);
  WriteLn;
end;

procedure TCHRFont.DisplayUndefinedCharacters;
var
  CharIndex, i: Integer;
  XByte, YByte: Byte;
  IsEmpty: Boolean;
  UndefinedCount: Integer;
  Ch: Char;
  SavePos: Int64;
begin
  WriteLn('Undefined/Empty Characters:');
  WriteLn('===========================================================');
  WriteLn(' Idx  Chr  ASCII  Width   Reason');
  WriteLn('===========================================================');

  UndefinedCount := 0;
  SavePos := FStream.Position;

  for CharIndex := 0 to FStrokeHeader.CharacterCount - 1 do
  begin
    Ch := Chr(FStrokeHeader.StartingChar + CharIndex);
    IsEmpty := False;

    // Check if character has zero width
    if FCharWidths[CharIndex] = 0 then
    begin
      Write(Format('%4d', [CharIndex]));
      Write('   ');

      if (Ord(Ch) >= 32) and (Ord(Ch) <= 126) then
        Write(Ch, '  ')
      else
        Write('?  ');

      Write(Format('%5d', [Ord(Ch)]));
      Write(Format('%7d', [FCharWidths[CharIndex]]));
      WriteLn('   Zero width');
      Inc(UndefinedCount);
      IsEmpty := True;
    end;

    // Check if stroke data is empty (immediately ends)
    if not IsEmpty then
    begin
      FStream.Position := FStrokeDataStart + FCharOffsets[CharIndex];
      FStream.ReadBuffer(XByte, 1);
      FStream.ReadBuffer(YByte, 1);

      // Check if first command is END (0x00, 0x00)
      if (XByte = 0) and (YByte = 0) then
      begin
        Write(Format('%4d', [CharIndex]));
        Write('   ');

        if (Ord(Ch) >= 32) and (Ord(Ch) <= 126) then
          Write(Ch, '  ')
        else
          Write('?  ');

        Write(Format('%5d', [Ord(Ch)]));
        Write(Format('%7d', [FCharWidths[CharIndex]]));
        WriteLn('   No stroke data (empty)');
        Inc(UndefinedCount);
      end;
    end;
  end;

  FStream.Position := SavePos;

  WriteLn('===========================================================');
  WriteLn('Total undefined characters: ', UndefinedCount, ' of ', FStrokeHeader.CharacterCount);
  WriteLn;
end;

var
  Font: TCHRFont;
  Ch: Char;
  i: Integer;

begin
  if ParamCount < 1 then
  begin
    WriteLn(ProgramName);
    WriteLn;
    WriteLn('Usage: chrinfo <font.chr> [options]');
    WriteLn;
    WriteLn('Options:');
    WriteLn('  --info           Show font information only');
    WriteLn('  --table          Show character width table');
    WriteLn('  --char <C>       Display specific character');
    WriteLn('  --strokes <C>    Show stroke commands for character');
    WriteLn('  --undefined      Show undefined/empty characters');
    WriteLn('  --sample         Display sample characters (A-Z, a-z, 0-9)');
    WriteLn('  --all            Display all characters');
    WriteLn('  --scale <N>      Scale factor (1-5, default: 1)');
    WriteLn;
    WriteLn('Examples:');
    WriteLn('  chrinfo SANS.CHR --info');
    WriteLn('  chrinfo GOTH.CHR --char A --scale 2');
    WriteLn('  chrinfo TRIP.CHR --strokes A');
    WriteLn('  chrinfo SANS.CHR --undefined');
    WriteLn('  chrinfo TRIP.CHR --sample');
    Exit;
  end;

  try
    Font := TCHRFont.Create(ParamStr(1));
    try
      if ParamCount = 1 then
      begin
        Font.DisplayInfo;
        Font.DisplayCharacterTable;

        WriteLn('Sample Characters:');
        WriteLn;
        for Ch := 'A' to 'E' do
          Font.DisplayCharacter(Ch, 1);
      end
      else
      begin
        i := 2;
        while i <= ParamCount do
        begin
          if ParamStr(i) = '--info' then
          begin
            Font.DisplayInfo;
          end
          else if ParamStr(i) = '--table' then
          begin
            Font.DisplayCharacterTable;
          end
          else if ParamStr(i) = '--char' then
          begin
            Inc(i);
            if i <= ParamCount then
              Font.DisplayCharacter(ParamStr(i)[1], 1);
          end
          else if ParamStr(i) = '--strokes' then
          begin
            Inc(i);
            if i <= ParamCount then
              Font.DisplayStrokeCommands(ParamStr(i)[1]);
          end
          else if ParamStr(i) = '--undefined' then
          begin
            Font.DisplayUndefinedCharacters;
          end
          else if ParamStr(i) = '--sample' then
          begin
            WriteLn('Sample Characters:');
            WriteLn;
            for Ch := 'A' to 'Z' do
              Font.DisplayCharacter(Ch, 1);
            for Ch := 'a' to 'z' do
              Font.DisplayCharacter(Ch, 1);
            for Ch := '0' to '9' do
              Font.DisplayCharacter(Ch, 1);
          end
          else if ParamStr(i) = '--all' then
          begin
            Font.DisplayAllChars(1);
          end
          else if ParamStr(i) = '--scale' then
          begin
            // Scale handled in display calls
          end;

          Inc(i);
        end;
      end;

    finally
      Font.Free;
    end;

  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end.
