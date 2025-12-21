{
  FonInfo - Displays information about FON Vector and Bitmap (Raster) fonts
  Displays Font Name,Copyright,Width,Height, and various other bits of information
  
  By RetroNick - Initial Code Released Dec 19 - 2025
}

program FONInfo;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, WinFont;

const ProgramName = 'FonInfo v1.0 By RetroNick - Released Dec 19 - 2025';

type
  TFONViewer = class
  private
    FFont: TWinFont;
    procedure DisplayInfo;
    procedure DisplayCharacterTable;
    procedure DisplayCharacter(Ch: Char; Scale: Integer = 1);
    procedure DisplayAllChars(Scale: Integer = 1);
    procedure DisplayStrokeCommands(Ch: Char);
    procedure DisplayUndefinedCharacters;
    procedure DisplaySample;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadFromFile(const FileName: string): Boolean;
  end;

constructor TFONViewer.Create;
begin
  inherited Create;
  FFont := TWinFont.Create;
end;

destructor TFONViewer.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

function TFONViewer.LoadFromFile(const FileName: string): Boolean;
begin
  Result := FFont.LoadFromFile(FileName);
end;

procedure TFONViewer.DisplayInfo;
var
  FontTypeStr: string;
  Height: Integer;
begin
  if not FFont.Loaded then
  begin
    WriteLn('Font not loaded');
    Exit;
  end;

  case FFont.FontType of
    ftRaster: FontTypeStr := 'Raster (Bitmap)';
    ftVector: FontTypeStr := 'Vector (Stroke)';
    else FontTypeStr := 'Unknown';
  end;

  Height := FFont.Height;

  WriteLn('===========================================================');
  WriteLn('                   FON FONT INFORMATION');
  WriteLn('===========================================================');
  WriteLn;
  WriteLn('Font Name:          ', FFont.FontName);
  WriteLn('Copyright:          ', FFont.Copyright);
  WriteLn('Font Type:          ', FontTypeStr);
  WriteLn;
  WriteLn('Character Range:    ', FFont.FirstChar, ' - ', FFont.LastChar);
  WriteLn('  First Character:  ', FFont.FirstChar, ' (''', Chr(FFont.FirstChar), ''')');
  WriteLn('  Last Character:   ', FFont.LastChar, ' (''', Chr(FFont.LastChar), ''')');
  WriteLn('  Total Characters: ', FFont.LastChar - FFont.FirstChar + 1);
  WriteLn;
  WriteLn('Character Height:   ', Height, ' pixels');
  WriteLn('Ascent:             ', FFont.Ascent, ' pixels');
  WriteLn('Descent:            ', FFont.Descent, ' pixels');
  WriteLn;
  WriteLn('===========================================================');
  WriteLn;
end;

procedure TFONViewer.DisplayCharacterTable;
var
  CharCode: Integer;
  Ch: Char;
  Width: Integer;
  StrokeCount: Integer;
begin
  if not FFont.Loaded then Exit;

  WriteLn('Character Width Table:');
  WriteLn('===========================================================');

  if FFont.FontType = ftVector then
    WriteLn(' Idx  Chr  ASCII  Width  Strokes')
  else
    WriteLn(' Idx  Chr  ASCII  Width');

  WriteLn('===========================================================');

  for CharCode := FFont.FirstChar to FFont.LastChar do
  begin
    Ch := Chr(CharCode);
    Width := FFont.GetCharWidth(CharCode);

    Write(Format('%4d', [CharCode - FFont.FirstChar]));
    Write('   ');

    if (CharCode >= 32) and (CharCode <= 126) then
      Write(Ch, '  ')
    else
      Write('?  ');

    Write(Format('%5d', [CharCode]));
    Write(Format('%7d', [Width]));

    if FFont.FontType = ftVector then
    begin
      StrokeCount := FFont.GetStrokeCount(CharCode);
      Write(Format('%9d', [StrokeCount]));
    end;

    WriteLn;
  end;

  WriteLn('===========================================================');
  WriteLn;
end;

procedure TFONViewer.DisplayCharacter(Ch: Char; Scale: Integer = 1);
var
  CharCode: Integer;
  Width: Integer;
  StrokeCount: Integer;
  I: Integer;
  MinX, MaxX, MinY, MaxY: Integer;
  GridWidth, GridHeight: Integer;
  Grid: array of array of Char;
  x, y: Integer;
  LastX, LastY: Integer;
  Cmd: TStrokeCmd;
  CurX, CurY: Integer;
  dx, dy, Steps, Step: Integer;
//  TempBitmap: TBitmap;
  Row, Col: Integer;
  ScaleX, ScaleY: Integer;
begin
  CharCode := Ord(Ch);

  if (CharCode < FFont.FirstChar) or (CharCode > FFont.LastChar) then
  begin
    WriteLn('Character ''', Ch, ''' not in font range');
    Exit;
  end;

  Width := FFont.GetCharWidth(CharCode);

  if FFont.FontType = ftVector then
  begin
    StrokeCount := FFont.GetStrokeCount(CharCode);

    if StrokeCount = 0 then
    begin
      WriteLn('Character ''', Ch, ''' has no stroke data');
      Exit;
    end;

    WriteLn('Character: ''', Ch, ''' (ASCII ', CharCode, ')');
    WriteLn('Width: ', Width, ' units');
    WriteLn('Strokes: ', StrokeCount);
    WriteLn;

    // Find bounding box
    MinX := 9999; MaxX := -9999;
    MinY := 9999; MaxY := -9999;

    for I := 0 to StrokeCount - 1 do
    begin
      if FFont.GetStrokePoint(CharCode, I, Cmd, CurX, CurY) then
      begin
        if CurX < MinX then MinX := CurX;
        if CurX > MaxX then MaxX := CurX;
        if CurY < MinY then MinY := CurY;
        if CurY > MaxY then MaxY := CurY;
      end;
    end;

    GridWidth := (MaxX - MinX + 3) * Scale;
    GridHeight := (MaxY - MinY + 3) * Scale;

    SetLength(Grid, GridHeight, GridWidth);
    for y := 0 to GridHeight - 1 do
      for x := 0 to GridWidth - 1 do
        Grid[y][x] := ' ';

    LastX := -1;
    LastY := -1;

    for I := 0 to StrokeCount - 1 do
    begin
      if FFont.GetStrokePoint(CharCode, I, Cmd, CurX, CurY) then
      begin
        x := (CurX - MinX + 1) * Scale;
        y := (CurY - MinY + 1) * Scale;

        if (Cmd = scLineTo) and (LastX >= 0) and (LastY >= 0) then
        begin
          // Draw line using Bresenham
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
                 (LastY + (y - LastY) * Step div Steps < GridHeight) and
                 (LastX + (x - LastX) * Step div Steps >= 0) and
                 (LastX + (x - LastX) * Step div Steps < GridWidth) then
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
    end;

    for y := 0 to GridHeight - 1 do
    begin
      for x := 0 to GridWidth - 1 do
        Write(Grid[y][x]);
      WriteLn;
    end;
    WriteLn;
  end
  else if FFont.FontType = ftRaster then
  begin
    // Display bitmap font
    WriteLn('Character: ''', Ch, ''' (ASCII ', CharCode, ')');
    WriteLn('Width: ', Width, ' pixels');
    WriteLn('Height: ', FFont.Height, ' pixels');
    WriteLn;
    FFont.DrawRasterCharInTextMode(ord(ch));
  end
  else
  begin
    WriteLn('Character ''', Ch, ''' (ASCII ', CharCode, ')');
    WriteLn('Width: ', Width, ' pixels');
    WriteLn('(Unknown font type)');
    WriteLn;
  end;
end;

procedure TFONViewer.DisplayAllChars(Scale: Integer = 1);
var
  CharCode: Integer;
  Ch: Char;
begin
  for CharCode := FFont.FirstChar to FFont.LastChar do
  begin
    Ch := Chr(CharCode);
    if (CharCode >= 32) and (CharCode <= 126) then
      DisplayCharacter(Ch, Scale);
  end;
end;

procedure TFONViewer.DisplayStrokeCommands(Ch: Char);
var
  CharCode: Integer;
  StrokeCount: Integer;
  I: Integer;
  Cmd: TStrokeCmd;
  X, Y: Integer;
  CmdStr: string;
begin
  CharCode := Ord(Ch);

  if (CharCode < FFont.FirstChar) or (CharCode > FFont.LastChar) then
  begin
    WriteLn('Character ''', Ch, ''' not in font range');
    Exit;
  end;

  if FFont.FontType <> ftVector then
  begin
    WriteLn('Font is not a vector font');
    Exit;
  end;

  StrokeCount := FFont.GetStrokeCount(CharCode);

  if StrokeCount = 0 then
  begin
    WriteLn('Character ''', Ch, ''' has no stroke data');
    Exit;
  end;

  WriteLn('Stroke Commands for Character ''', Ch, ''' (ASCII ', CharCode, ')');
  WriteLn('===========================================================');
  WriteLn('Character Width: ', FFont.GetCharWidth(CharCode), ' units');
  WriteLn('Total Strokes: ', StrokeCount);
  WriteLn;
  WriteLn('  #    X    Y   Command');
  WriteLn('===========================================================');

  for I := 0 to StrokeCount - 1 do
  begin
    if FFont.GetStrokePoint(CharCode, I, Cmd, X, Y) then
    begin
      Write(Format('%3d', [I]));
      Write(' ');
      Write(Format('%4d', [X]));
      Write(' ');
      Write(Format('%4d', [Y]));
      Write('   ');

      case Cmd of
        scMoveTo: CmdStr := 'MOVE to (' + IntToStr(X) + ',' + IntToStr(Y) + ')';
        scLineTo: CmdStr := 'LINE to (' + IntToStr(X) + ',' + IntToStr(Y) + ')';
        scEnd:    CmdStr := 'END';
      end;

      WriteLn(CmdStr);
    end;
  end;
  WriteLn('===========================================================');
  WriteLn('Total commands: ', StrokeCount);
  WriteLn;
end;

procedure TFONViewer.DisplayUndefinedCharacters;
var
  CharCode: Integer;
  Ch: Char;
  Width: Integer;
  StrokeCount: Integer;
  UndefinedCount: Integer;
begin
  if not FFont.Loaded then Exit;

  WriteLn('Undefined/Empty Characters:');
  WriteLn('===========================================================');
  WriteLn(' Idx  Chr  ASCII  Width   Reason');
  WriteLn('===========================================================');

  UndefinedCount := 0;

  for CharCode := FFont.FirstChar to FFont.LastChar do
  begin
    Ch := Chr(CharCode);
    Width := FFont.GetCharWidth(CharCode);

    if FFont.FontType = ftVector then
    begin
      StrokeCount := FFont.GetStrokeCount(CharCode);

      if (Width = 0) or (StrokeCount = 0) then
      begin
        Write(Format('%4d', [CharCode - FFont.FirstChar]));
        Write('   ');

        if (CharCode >= 32) and (CharCode <= 126) then
          Write(Ch, '  ')
        else
          Write('?  ');

        Write(Format('%5d', [CharCode]));
        Write(Format('%7d', [Width]));

        if Width = 0 then
          WriteLn('   Zero width')
        else if StrokeCount = 0 then
          WriteLn('   No stroke data');

        Inc(UndefinedCount);
      end;
    end
    else
    begin
      // For raster fonts, just check width
      if Width = 0 then
      begin
        Write(Format('%4d', [CharCode - FFont.FirstChar]));
        Write('   ');

        if (CharCode >= 32) and (CharCode <= 126) then
          Write(Ch, '  ')
        else
          Write('?  ');

        Write(Format('%5d', [CharCode]));
        Write(Format('%7d', [Width]));
        WriteLn('   Zero width');

        Inc(UndefinedCount);
      end;
    end;
  end;

  WriteLn('===========================================================');
  WriteLn('Total undefined characters: ', UndefinedCount, ' of ',
          FFont.LastChar - FFont.FirstChar + 1);
  WriteLn;
end;

procedure TFONViewer.DisplaySample;
var
  Ch: Char;
begin
  WriteLn('Sample Characters:');
  WriteLn;

  // Display A-Z
  for Ch := 'A' to 'Z' do
    if (Ord(Ch) >= FFont.FirstChar) and (Ord(Ch) <= FFont.LastChar) then
      DisplayCharacter(Ch, 1);

  // Display a-z
  for Ch := 'a' to 'z' do
    if (Ord(Ch) >= FFont.FirstChar) and (Ord(Ch) <= FFont.LastChar) then
      DisplayCharacter(Ch, 1);

  // Display 0-9
  for Ch := '0' to '9' do
    if (Ord(Ch) >= FFont.FirstChar) and (Ord(Ch) <= FFont.LastChar) then
      DisplayCharacter(Ch, 1);
end;

var
  Viewer: TFONViewer;
  i: Integer;

begin
  if ParamCount < 1 then
  begin
    WriteLn(ProgramName);
    WriteLn;
    WriteLn('Usage: foninfo <font.fon> [options]');
    WriteLn;
    WriteLn('Options:');
    WriteLn('  --info           Show font information only');
    WriteLn('  --table          Show character width table');
    WriteLn('  --char <C>       Display specific character');
    WriteLn('  --strokes <C>    Show stroke commands for character (vector fonts)');
    WriteLn('  --undefined      Show undefined/empty characters');
    WriteLn('  --sample         Display sample characters (A-Z, a-z, 0-9)');
    WriteLn('  --all            Display all characters');
    WriteLn('  --scale <N>      Scale factor (1-5, default: 1)');
    WriteLn;
    WriteLn('Examples:');
    WriteLn('  foninfo ROMAN.FON --info');
    WriteLn('  foninfo SCRIPT.FON --char A --scale 2');
    WriteLn('  foninfo ROMAN.FON --strokes A');
    WriteLn('  foninfo MODERN.FON --undefined');
    WriteLn('  foninfo ROMAN.FON --sample');
    Exit;
  end;

  Viewer := TFONViewer.Create;
  try
    if not Viewer.LoadFromFile(ParamStr(1)) then
    begin
      WriteLn('Error: Could not load font file: ', ParamStr(1));
      Exit;
    end;

    if ParamCount = 1 then
    begin
      Viewer.DisplayInfo;
      Viewer.DisplayCharacterTable;

      if Viewer.FFont.FontType = ftVector then
      begin
        WriteLn('Sample Characters (Vector Font):');
        WriteLn;
        Viewer.DisplayCharacter('A', 1);
        Viewer.DisplayCharacter('B', 1);
        Viewer.DisplayCharacter('C', 1);
        Viewer.DisplayCharacter('D', 1);
        Viewer.DisplayCharacter('E', 1);
      end
      else if Viewer.FFont.FontType = ftRaster then
      begin
        WriteLn('Sample Characters (Bitmap Font):');
        WriteLn;
        Viewer.DisplayCharacter('A', 2);
        Viewer.DisplayCharacter('B', 2);
        Viewer.DisplayCharacter('C', 2);
        Viewer.DisplayCharacter('D', 2);
        Viewer.DisplayCharacter('E', 2);
      end;
    end
    else
    begin
      i := 2;
      while i <= ParamCount do
      begin
        if ParamStr(i) = '--info' then
        begin
          Viewer.DisplayInfo;
        end
        else if ParamStr(i) = '--table' then
        begin
          Viewer.DisplayCharacterTable;
        end
        else if ParamStr(i) = '--char' then
        begin
          Inc(i);
          if i <= ParamCount then
            Viewer.DisplayCharacter(ParamStr(i)[1], 1);
        end
        else if ParamStr(i) = '--strokes' then
        begin
          Inc(i);
          if i <= ParamCount then
            Viewer.DisplayStrokeCommands(ParamStr(i)[1]);
        end
        else if ParamStr(i) = '--undefined' then
        begin
          Viewer.DisplayUndefinedCharacters;
        end
        else if ParamStr(i) = '--sample' then
        begin
          Viewer.DisplaySample;
        end
        else if ParamStr(i) = '--all' then
        begin
          Viewer.DisplayAllChars(1);
        end
        else if ParamStr(i) = '--scale' then
        begin
          // Scale parameter handled in display calls
          // Dec 19 - 2025 - Future nick here - the idea with the command 
          // command line utilities was to have them also switch into 
          // graphics mode and display the fonts. Decided feature would take 
          // too long implement here. 
        end;

        Inc(i);
      end;
    end;

  finally
    Viewer.Free;
  end;
end.
