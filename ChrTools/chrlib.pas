{  Library to assist in creating Borland CHR Font files
   using a command line interface or in memory.

   By RetroNick - Initial Code Released Dec 19 - 2025
}
unit chrlib;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  TStrokeOp = (soEndChar, soScan, soMoveTo, soLineTo);

  TStrokeCommand = packed record
    Operation: TStrokeOp;
    X, Y: ShortInt;
  end;

  TStrokeCommandArray = array of TStrokeCommand;

  TCharacterDef = packed record
    CharCode: Byte;
    Width: Byte;
    Commands: TStrokeCommandArray;
  end;

  TCHRFont = class
  private
    FSignature: array[0..1] of Char;
    FDescription: string;
    FHeaderSize: Word;
    FFontName: string;
    FFontMajor: Byte;
    FFontMinor: Byte;
    FRevisionMajor: Byte;
    FRevisionMinor: Byte;
    FCharacterCount: SmallInt;
    FStartingChar: Byte;
    FScanFlag: ShortInt;
    FOriginToAscender: ShortInt;
    FOriginToBaseline: ShortInt;
    FOriginToDescender: ShortInt;
    FCharacters: array of TCharacterDef;

    procedure WriteHeader(Stream: TStream);
    procedure WriteStrokeHeader(Stream: TStream);
    procedure WriteCharacterData(Stream: TStream);
    function EncodeStrokeCommand(const Cmd: TStrokeCommand): TBytes;
    function DecodeStrokeCommands(Stream: TStream; var Offset: Int64): TStrokeCommandArray;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    property Description: string read FDescription write FDescription;
    property FontName: string read FFontName write FFontName;
    property FontMajor: Byte read FFontMajor write FFontMajor;
    property FontMinor: Byte read FFontMinor write FFontMinor;
    property RevisionMajor: Byte read FRevisionMajor write FRevisionMajor;
    property RevisionMinor: Byte read FRevisionMinor write FRevisionMinor;
    property HeaderSize: Word read FHeaderSize write FHeaderSize;
    property CharacterCount: SmallInt read FCharacterCount;
    property StartingChar: Byte read FStartingChar write FStartingChar;
    property ScanFlag: ShortInt read FScanFlag write FScanFlag;
    property OriginToAscender: ShortInt read FOriginToAscender write FOriginToAscender;
    property OriginToBaseline: ShortInt read FOriginToBaseline write FOriginToBaseline;
    property OriginToDescender: ShortInt read FOriginToDescender write FOriginToDescender;

    procedure Clear;
    procedure InitializeFont(AStartingChar: Byte; ACharCount: Integer);
    function GetCharacter(CharCode: Byte): TCharacterDef;
    procedure SetCharacter(const CharDef: TCharacterDef);
    function GetCharacterByIndex(Index: Integer): TCharacterDef;
    procedure SetCharacterByIndex(Index: Integer; const CharDef: TCharacterDef);
    function CreateCharacter(CharCode: Byte; Width: Byte): TCharacterDef;
    procedure AddMoveTo(var CharDef: TCharacterDef; X, Y: ShortInt);
    procedure AddLineTo(var CharDef: TCharacterDef; X, Y: ShortInt);
    procedure AddScan(var CharDef: TCharacterDef);
    procedure AddMoveToNextChar(var CharDef: TCharacterDef);
    procedure AddMoveToNextChar(var CharDef: TCharacterDef; AdvanceWidth: ShortInt);
    procedure EndCharacter(var CharDef: TCharacterDef);
    function CharToIndex(CharCode: Byte): Integer;
    function GetCharacterHeight: Integer;
  end;

implementation

constructor TCHRFont.Create;
begin
  inherited Create;
  FSignature[0] := 'P';
  FSignature[1] := 'K';
  FDescription := '';
  FHeaderSize := $80;
  FFontName := '    ';
  FFontMajor := 1;
  FFontMinor := 0;
  FRevisionMajor := 1;
  FRevisionMinor := 0;
  FCharacterCount := 0;
  FStartingChar := 0;
  FScanFlag := 0;
  FOriginToAscender := 8;
  FOriginToBaseline := 0;
  FOriginToDescender := -2;
  SetLength(FCharacters, 0);
end;

destructor TCHRFont.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCHRFont.Clear;
begin
  SetLength(FCharacters, 0);
  FCharacterCount := 0;
end;

procedure TCHRFont.InitializeFont(AStartingChar: Byte; ACharCount: Integer);
var
  i: Integer;
  Cmd: TStrokeCommand;
begin
  Clear;
  FStartingChar := AStartingChar;
  FCharacterCount := ACharCount;
  SetLength(FCharacters, ACharCount);

  for i := 0 to ACharCount - 1 do
  begin
    FCharacters[i].CharCode := AStartingChar + i;
    FCharacters[i].Width := 8;
    // Initialize with END marker so empty characters are valid
    SetLength(FCharacters[i].Commands, 1);
    Cmd.Operation := soEndChar;
    Cmd.X := 0;
    Cmd.Y := 0;
    FCharacters[i].Commands[0] := Cmd;
  end;
end;

function TCHRFont.CharToIndex(CharCode: Byte): Integer;
begin
  Result := CharCode - FStartingChar;
  if (Result < 0) or (Result >= FCharacterCount) then
    Result := -1;
end;

function TCHRFont.GetCharacterHeight: Integer;
begin
  Result := FOriginToAscender - FOriginToDescender;
end;

function TCHRFont.GetCharacter(CharCode: Byte): TCharacterDef;
var
  Idx: Integer;
begin
  Idx := CharToIndex(CharCode);
  if Idx >= 0 then
    Result := FCharacters[Idx]
  else
  begin
    Result.CharCode := 0;
    Result.Width := 0;
    SetLength(Result.Commands, 0);
  end;
end;

procedure TCHRFont.SetCharacter(const CharDef: TCharacterDef);
var
  Idx: Integer;
begin
  Idx := CharToIndex(CharDef.CharCode);
  if Idx >= 0 then
    FCharacters[Idx] := CharDef;
end;

function TCHRFont.GetCharacterByIndex(Index: Integer): TCharacterDef;
begin
  if (Index >= 0) and (Index < FCharacterCount) then
    Result := FCharacters[Index]
  else
  begin
    Result.CharCode := 0;
    Result.Width := 0;
    SetLength(Result.Commands, 0);
  end;
end;

procedure TCHRFont.SetCharacterByIndex(Index: Integer; const CharDef: TCharacterDef);
begin
  if (Index >= 0) and (Index < FCharacterCount) then
    FCharacters[Index] := CharDef;
end;

function TCHRFont.CreateCharacter(CharCode: Byte; Width: Byte): TCharacterDef;
begin
  Result.CharCode := CharCode;
  Result.Width := Width;
  SetLength(Result.Commands, 0);
end;

procedure TCHRFont.AddMoveTo(var CharDef: TCharacterDef; X, Y: ShortInt);
var
  Cmd: TStrokeCommand;
begin
  Cmd.Operation := soMoveTo;
  Cmd.X := X;
  Cmd.Y := Y;
  SetLength(CharDef.Commands, Length(CharDef.Commands) + 1);
  CharDef.Commands[High(CharDef.Commands)] := Cmd;
end;

procedure TCHRFont.AddLineTo(var CharDef: TCharacterDef; X, Y: ShortInt);
var
  Cmd: TStrokeCommand;
begin
  Cmd.Operation := soLineTo;
  Cmd.X := X;
  Cmd.Y := Y;
  SetLength(CharDef.Commands, Length(CharDef.Commands) + 1);
  CharDef.Commands[High(CharDef.Commands)] := Cmd;
end;

procedure TCHRFont.AddScan(var CharDef: TCharacterDef);
var
  Cmd: TStrokeCommand;
begin
  Cmd.Operation := soScan;
  Cmd.X := 0;
  Cmd.Y := 0;
  SetLength(CharDef.Commands, Length(CharDef.Commands) + 1);
  CharDef.Commands[High(CharDef.Commands)] := Cmd;
end;

procedure TCHRFont.AddMoveToNextChar(var CharDef: TCharacterDef);
begin
  // Use the character's width as the advance
  AddMoveToNextChar(CharDef, CharDef.Width);
end;

procedure TCHRFont.AddMoveToNextChar(var CharDef: TCharacterDef; AdvanceWidth: ShortInt);
var
  Cmd: TStrokeCommand;
begin
  // Add MoveTo to advance cursor to next character position
  // This is REQUIRED in CHR format for proper character spacing
  Cmd.Operation := soMoveTo;
  Cmd.X := AdvanceWidth;
  Cmd.Y := 0;  // Stay on baseline
  SetLength(CharDef.Commands, Length(CharDef.Commands) + 1);
  CharDef.Commands[High(CharDef.Commands)] := Cmd;
end;

procedure TCHRFont.EndCharacter(var CharDef: TCharacterDef);
var
  Cmd: TStrokeCommand;
begin
  Cmd.Operation := soEndChar;
  Cmd.X := 0;
  Cmd.Y := 0;
  SetLength(CharDef.Commands, Length(CharDef.Commands) + 1);
  CharDef.Commands[High(CharDef.Commands)] := Cmd;
end;

function TCHRFont.EncodeStrokeCommand(const Cmd: TStrokeCommand): TBytes;
var
  XByte, YByte: Byte;
  XVal, YVal: ShortInt;
begin
  SetLength(Result, 2);

  case Cmd.Operation of
    soEndChar:
      begin
        Result[0] := $00;
        Result[1] := $00;
      end;

    soScan:
      begin
        Result[0] := $00;
        Result[1] := $80;
      end;

    soMoveTo:
      begin
        XVal := Cmd.X;
        YVal := Cmd.Y;

        // Encode as 7-bit two's complement + opcode bit
        // Bit 7 = 1 for all drawing commands (MoveTo/LineTo)
        // Bits 6-0 = signed coordinate in two's complement
        XByte := (XVal and $7F) or $80;  // Set bit 7 for draw opcode
        YByte := (YVal and $7F);          // Bit 7=0 distinguishes MoveTo from LineTo

        Result[0] := XByte;
        Result[1] := YByte;
      end;

    soLineTo:
      begin
        XVal := Cmd.X;
        YVal := Cmd.Y;

        // Encode as 7-bit two's complement + opcode bit
        XByte := (XVal and $7F) or $80;  // Set bit 7 for draw opcode
        YByte := (YVal and $7F) or $80;  // Set bit 7 for LineTo

        Result[0] := XByte;
        Result[1] := YByte;
      end;
  end;
end;

function TCHRFont.DecodeStrokeCommands(Stream: TStream; var Offset: Int64): TStrokeCommandArray;
var
  XByte, YByte: Byte;
  XOp, YOp: Byte;
  XVal, YVal: ShortInt;
  Cmd: TStrokeCommand;
  Commands: TStrokeCommandArray;
  Count: Integer;
  MaxIterations: Integer;
begin
  SetLength(Commands, 0);
  Count := 0;
  MaxIterations := 1000;  // Safety limit

  if Offset >= Stream.Size then
  begin
    WriteLn('WARNING: Offset ', Offset, ' >= stream size ', Stream.Size);
    Result := Commands;
    Exit;
  end;

  Stream.Position := Offset;

  while (Stream.Position + 2 <= Stream.Size) and (MaxIterations > 0) do
  begin
    Dec(MaxIterations);

    if Stream.Position + 2 > Stream.Size then
    begin
      WriteLn('WARNING: Not enough bytes for stroke command at position ', Stream.Position);
      Break;
    end;

    Stream.ReadBuffer(XByte, 1);
    Stream.ReadBuffer(YByte, 1);

    // Extract opcode bits (bit 7)
    XOp := (XByte shr 7) and 1;
    YOp := (YByte shr 7) and 1;

    if (XOp = 0) and (YOp = 0) then
    begin
      // End of character
      Cmd.Operation := soEndChar;
      Cmd.X := 0;
      Cmd.Y := 0;
      SetLength(Commands, Count + 1);
      Commands[Count] := Cmd;
      Break;
    end
    else if (XOp = 0) and (YOp = 1) then
    begin
      // Scan command
      Cmd.Operation := soScan;
      Cmd.X := 0;
      Cmd.Y := 0;
      SetLength(Commands, Count + 1);
      Commands[Count] := Cmd;
      Inc(Count);
    end
    else
    begin
      // MoveTo or LineTo command
      // Extract 7-bit two's complement values
      // If bit 6 is set, the value is negative (sign extend from bit 6)
      if (XByte and $40) <> 0 then
        XVal := ShortInt(XByte or $80)  // Sign extend: set bit 7 to make it negative
      else
        XVal := ShortInt(XByte and $7F); // Positive: just mask off the opcode bit

      if (YByte and $40) <> 0 then
        YVal := ShortInt(YByte or $80)  // Sign extend
      else
        YVal := ShortInt(YByte and $7F);

      // Determine operation type based on Y opcode bit
      if (XOp = 1) and (YOp = 0) then
        Cmd.Operation := soMoveTo
      else if (XOp = 1) and (YOp = 1) then
        Cmd.Operation := soLineTo
      else
        Cmd.Operation := soEndChar;  // Should not happen

      Cmd.X := XVal;
      Cmd.Y := YVal;

      SetLength(Commands, Count + 1);
      Commands[Count] := Cmd;
      Inc(Count);
    end;
  end;

  if MaxIterations = 0 then
    WriteLn('WARNING: Hit max iterations reading stroke data at offset ', Offset);

  Offset := Stream.Position;
  Result := Commands;
end;

procedure TCHRFont.LoadFromStream(Stream: TStream);
var
  Ch: Char;
  i: Integer;
  HeaderStart: Int64;
  StrokeCheck: Byte;
  Undefined: Byte;
  CharOffsets: array of Word;
  CharWidths: array of Byte;
  StrokesOffset: SmallInt;
  StrokeDataStart: Int64;
  CurrentOffset: Int64;
  FontNameField: array[0..3] of Char;
  FontSizeRead: Word;
begin
  Clear;

  Stream.ReadBuffer(FSignature, 2);
  if (FSignature[0] <> 'P') or (FSignature[1] <> 'K') then
    raise Exception.Create('Invalid CHR file');

  FDescription := '';
  while Stream.Position < 256 do
  begin
    Stream.ReadBuffer(Ch, 1);
    if Ch = #$1A then Break;
    if Ord(Ch) >= 32 then FDescription := FDescription + Ch;
  end;

  Stream.ReadBuffer(FHeaderSize, 2);
  Stream.ReadBuffer(FontNameField, 4);
  FFontName := string(FontNameField);
  Stream.ReadBuffer(FontSizeRead, 2);
  Stream.ReadBuffer(FFontMajor, 1);
  Stream.ReadBuffer(FFontMinor, 1);
  Stream.ReadBuffer(FRevisionMajor, 1);
  Stream.ReadBuffer(FRevisionMinor, 1);

  HeaderStart := FHeaderSize;
  Stream.Position := HeaderStart;

  Stream.ReadBuffer(StrokeCheck, 1);
  if StrokeCheck <> $2B then
    raise Exception.Create('Not a stroked font');

  Stream.ReadBuffer(FCharacterCount, 2);
  Stream.ReadBuffer(Undefined, 1);
  Stream.ReadBuffer(FStartingChar, 1);
  Stream.ReadBuffer(StrokesOffset, 2);
  Stream.ReadBuffer(FScanFlag, 1);
  Stream.ReadBuffer(FOriginToAscender, 1);
  Stream.ReadBuffer(FOriginToBaseline, 1);
  Stream.ReadBuffer(FOriginToDescender, 1);
  Stream.ReadBuffer(FontNameField, 4);
  Stream.ReadBuffer(Undefined, 1);

  SetLength(CharOffsets, FCharacterCount);
  for i := 0 to FCharacterCount - 1 do
    Stream.ReadBuffer(CharOffsets[i], 2);

  SetLength(CharWidths, FCharacterCount);
  for i := 0 to FCharacterCount - 1 do
    Stream.ReadBuffer(CharWidths[i], 1);

  StrokeDataStart := HeaderStart + StrokesOffset;

  WriteLn('DEBUG LoadFromStream:');
  WriteLn('  Header start: ', HeaderStart);
  WriteLn('  Strokes offset: ', StrokesOffset);
  WriteLn('  Stroke data start: ', StrokeDataStart);
  WriteLn('  Stream size: ', Stream.Size);
  if FCharacterCount > 0 then
  begin
    WriteLn('  First 3 char offsets: ', CharOffsets[0], ', ', CharOffsets[1], ', ', CharOffsets[2]);
    WriteLn('  First 3 absolute positions: ', StrokeDataStart + CharOffsets[0], ', ',
            StrokeDataStart + CharOffsets[1], ', ', StrokeDataStart + CharOffsets[2]);
  end;

  SetLength(FCharacters, FCharacterCount);
  for i := 0 to FCharacterCount - 1 do
  begin
    FCharacters[i].CharCode := FStartingChar + i;
    FCharacters[i].Width := CharWidths[i];
    CurrentOffset := StrokeDataStart + CharOffsets[i];
    FCharacters[i].Commands := DecodeStrokeCommands(Stream, CurrentOffset);
  end;
end;

procedure TCHRFont.LoadFromFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCHRFont.WriteHeader(Stream: TStream);
var
  DescWithMarker: string;
  Padding: array[0..127] of Byte;
  i: Integer;
  FontNameField: array[0..3] of Char;
  FontSize: Word;
begin
  Stream.WriteBuffer(FSignature, 2);

  DescWithMarker := FDescription + #$1A;
  Stream.Write(DescWithMarker[1], Length(DescWithMarker));

  Stream.WriteBuffer(FHeaderSize, 2);

  FontNameField := '    ';
  for i := 0 to Min(3, Length(FFontName) - 1) do
    FontNameField[i] := FFontName[i + 1];
  Stream.WriteBuffer(FontNameField, 4);

  FontSize := 0;
  Stream.WriteBuffer(FontSize, 2);
  Stream.WriteBuffer(FFontMajor, 1);
  Stream.WriteBuffer(FFontMinor, 1);
  Stream.WriteBuffer(FRevisionMajor, 1);
  Stream.WriteBuffer(FRevisionMinor, 1);

  FillChar(Padding, SizeOf(Padding), 0);
  i := Stream.Position;
  if i < FHeaderSize then
    Stream.Write(Padding, FHeaderSize - i);
end;

procedure TCHRFont.WriteStrokeHeader(Stream: TStream);
var
  StrokeCheck: Byte;
  Undefined: Byte;
  StrokesOffset: SmallInt;
  FontNameField: array[0..3] of Char;
  t : shortint;
begin
  StrokeCheck := $2B;
  Stream.WriteBuffer(StrokeCheck, 1);

  Stream.WriteBuffer(FCharacterCount, 2);

  Undefined := 0;
  Stream.WriteBuffer(Undefined, 1);

  Stream.WriteBuffer(FStartingChar, 1);

  // StrokesOffset = size of this header + offset table + width table
  // This header: 1 + 2 + 1 + 1 + 2 + 1 + 1 + 1 + 1 + 4 + 1 = 16 bytes
  StrokesOffset := 16 + (FCharacterCount * 2) + FCharacterCount;
  Stream.WriteBuffer(StrokesOffset, 2);

  Stream.WriteBuffer(FScanFlag, 1);
  Stream.WriteBuffer(FOriginToAscender, 1);
  Stream.WriteBuffer(FOriginToBaseline, 1);
//  t:=-3;
  Stream.WriteBuffer(FOriginToDescender, 1);
//  Stream.WriteBuffer(t, 1);

  FillChar(FontNameField, 4, 0);
  Stream.WriteBuffer(FontNameField, 4);
  Stream.WriteBuffer(Undefined, 1);
end;

procedure TCHRFont.WriteCharacterData(Stream: TStream);
var
  i, j, k: Integer;
  CharOffsets: array of Word;
  CurrentOffset: Word;
  EncodedCommands: array of TBytes;
  TotalSize: Integer;
  CmdBytes: TBytes;
  OffsetTableStart, WidthTableStart, StrokeDataStart: Int64;
begin
  SetLength(EncodedCommands, FCharacterCount);
  CurrentOffset := 0;
  SetLength(CharOffsets, FCharacterCount);

  WriteLn('DEBUG WriteCharacterData:');
  WriteLn('  Character count: ', FCharacterCount);

  // First, encode all character stroke data
  for i := 0 to FCharacterCount - 1 do
  begin
    CharOffsets[i] := CurrentOffset;
    SetLength(EncodedCommands[i], 0);
    TotalSize := 0;

    for j := 0 to High(FCharacters[i].Commands) do
    begin
      CmdBytes := EncodeStrokeCommand(FCharacters[i].Commands[j]);
      k := Length(EncodedCommands[i]);
      SetLength(EncodedCommands[i], k + Length(CmdBytes));
      Move(CmdBytes[0], EncodedCommands[i][k], Length(CmdBytes));
      TotalSize := TotalSize + Length(CmdBytes);
    end;

    CurrentOffset := CurrentOffset + TotalSize;
    if i < 3 then
      WriteLn('  Char ', i, ': offset=', CharOffsets[i], ' size=', TotalSize);
  end;

  // Write offset table
  OffsetTableStart := Stream.Position;
  WriteLn('  Offset table at: ', OffsetTableStart);
  for i := 0 to FCharacterCount - 1 do
    Stream.WriteBuffer(CharOffsets[i], 2);

  // Write width table
  WidthTableStart := Stream.Position;
  WriteLn('  Width table at: ', WidthTableStart);
  for i := 0 to FCharacterCount - 1 do
    Stream.WriteBuffer(FCharacters[i].Width, 1);

  // Write stroke data
  StrokeDataStart := Stream.Position;
  WriteLn('  Stroke data at: ', StrokeDataStart);
  for i := 0 to FCharacterCount - 1 do
  begin
    if Length(EncodedCommands[i]) > 0 then
      Stream.Write(EncodedCommands[i][0], Length(EncodedCommands[i]));
  end;

  WriteLn('  End position: ', Stream.Position);
end;

procedure TCHRFont.SaveToStream(Stream: TStream);
var
  FontSizePos, DataStartPos, DataEndPos: Int64;
  FontSize: Word;
  SavePos: Int64;
begin
  WriteHeader(Stream);

  FontSizePos := 2 + Length(FDescription) + 1 + 2 + 4;
  DataStartPos := Stream.Position;

  WriteStrokeHeader(Stream);
  WriteCharacterData(Stream);

  DataEndPos := Stream.Position;
  FontSize := DataEndPos - DataStartPos;

  SavePos := Stream.Position;
  Stream.Position := FontSizePos;
  Stream.WriteBuffer(FontSize, 2);
  Stream.Position := SavePos;
end;

procedure TCHRFont.SaveToFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

end.
