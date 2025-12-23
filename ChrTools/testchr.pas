{  TestChr - Minimal CHR font test program   
   Creates a few characters and saves them as Borland CHR font
   by using programming commands instead of a GUI editor 
   (GUI CHR Editor also available in my other projects)

   By RetroNick - Initial Code Released Dec 19 - 2025
}
program TestCHR;

{$mode objfpc}{$H+}

uses
  SysUtils, Math, CHRLib;

procedure CreateSampleFont;
var
  Font: TCHRFont;
  CharDef: TCharacterDef;
begin
  Font := TCHRFont.Create;
  try
    Font.Description := #8#8+'This Sample CHR Font was Created with TestCHR';    //the #8#8 are 2 back spaces to delete the"PK" header
    Font.FontName := 'SAMP';  //4 characters only                                //when you perform DOS command TYPE SAMP.CHR
    Font.OriginToAscender := 8;                                                  //is ascii code #26
    Font.OriginToBaseline := 0;
    Font.OriginToDescender:=-3;
    Font.InitializeFont(32, 96);  // 32 to 127 = 96 characters

    // create h under base line
    CharDef := Font.CreateCharacter(Ord('h'), 10);
    Font.AddMoveTo(CharDef, 0, -2);
    Font.AddLineTo(CharDef, 2, -2);
    Font.EndCharacter(CharDef);
    Font.SetCharacter(CharDef);

    // Create 'M' - defines top boundary (capital height)
    CharDef := Font.CreateCharacter(Ord('M'), 10);
    Font.AddMoveTo(CharDef, 0, 0);
    Font.AddLineTo(CharDef, 0, 8);
    Font.AddLineTo(CharDef, 5, 3);
    Font.AddLineTo(CharDef, 10, 8);
    Font.AddLineTo(CharDef, 10, 0);
    Font.EndCharacter(CharDef);
    Font.SetCharacter(CharDef);

    // Create 'q' - defines descender boundary
    CharDef := Font.CreateCharacter(Ord('q'), 8);
    Font.AddMoveTo(CharDef, 0, 0);
    Font.AddLineTo(CharDef, 0, 4);
    Font.AddLineTo(CharDef, 4, 4);
    Font.AddLineTo(CharDef, 4, 0);
    Font.AddLineTo(CharDef, 0, 0);
    Font.AddLineTo(CharDef, 0, -3);
    //    Font.AddLineTo(CharDef, 8, -2);
    Font.EndCharacter(CharDef);
    Font.SetCharacter(CharDef);

    // Create 'x' - defines x-height (lowercase)
    CharDef := Font.CreateCharacter(Ord('x'), 8);
    Font.AddMoveTo(CharDef, 0, 0);
    Font.AddLineTo(CharDef, 8, 4);
    Font.AddMoveTo(CharDef, 8, 0);
    Font.AddLineTo(CharDef, 0, 4);
    Font.EndCharacter(CharDef);
    Font.SetCharacter(CharDef);

    // Create 'A'
    CharDef := Font.CreateCharacter(Ord('A'), 10);
    Font.AddMoveTo(CharDef, 0, 0);
    Font.AddLineTo(CharDef, 4, 8);
    Font.AddLineTo(CharDef, 8, 0);
    Font.AddMoveTo(CharDef, 2, 3);
    Font.AddLineTo(CharDef, 6, 3);
    Font.AddMoveTo(CharDef, 0, 0);
    Font.AddMoveTo(CharDef, 10,0 );

    Font.EndCharacter(CharDef);
    Font.SetCharacter(CharDef);

    // Create 'B'
    CharDef := Font.CreateCharacter(Ord('B'), 8);
    Font.AddMoveTo(CharDef, 0, 0);
    Font.AddLineTo(CharDef, 0, 8);
    Font.AddLineTo(CharDef, 5, 8);
    Font.AddLineTo(CharDef, 6, 7);
    Font.AddLineTo(CharDef, 6, 5);
    Font.AddLineTo(CharDef, 5, 4);
    Font.AddLineTo(CharDef, 0, 4);
    Font.AddMoveTo(CharDef, 0, 4);
    Font.AddLineTo(CharDef, 5, 4);
    Font.AddLineTo(CharDef, 6, 3);
    Font.AddLineTo(CharDef, 6, 1);
    Font.AddLineTo(CharDef, 5, 0);
    Font.AddLineTo(CharDef, 0, 0);
    Font.AddMoveTo(CharDef, 8, 0);

    Font.EndCharacter(CharDef);
    Font.SetCharacter(CharDef);


    Font.SaveToFile('SAMP.CHR');
    WriteLn('Created SAMP.CHR (sample) with key reference characters');
    WriteLn('Font has ', Font.CharacterCount, ' characters (A-Z)');
    WriteLn('Reference characters for font editor:');
    WriteLn('  M - defines capital height (ascender)');
    WriteLn('  Q - defines descender depth');
    WriteLn('  X - defines x-height');
    WriteLn('Additional characters: A, B, C, E, H, L, O');

  finally
    Font.Free;
  end;
end;

procedure DisplayFontInfo(const FileName: string);
var
  Font: TCHRFont;
  i, j: Integer;
  CharDef: TCharacterDef;
  F: File;
  FileSize: Int64;
begin
  if not FileExists(FileName) then
  begin
    WriteLn('Error: File not found: ', FileName);
    Exit;
  end;
  Assign(F, FileName);
  Reset(F, 1);
  FileSize := System.FileSize(F);
  Close(F);
  WriteLn('File: ', FileName);
  WriteLn('File Size: ', FileSize, ' bytes');
  WriteLn;
  Font := TCHRFont.Create;
  try
    try
      Font.LoadFromFile(FileName);
    except
      on E: Exception do
      begin
        WriteLn('Error loading font: ', E.Message);
        Exit;
      end;
    end;
    WriteLn('Font Information:');
    WriteLn('=================');
    WriteLn('Description: ', Font.Description);
    WriteLn('Font Name: ', Font.FontName);
    WriteLn('Version: ', Font.FontMajor, '.', Font.FontMinor);
    WriteLn('Character Count: ', Font.CharacterCount);
    WriteLn('Starting Character: ', Font.StartingChar, ' (''', Chr(Font.StartingChar), ''')');
    WriteLn('Character Height: ', Font.GetCharacterHeight);
    WriteLn('Origin to Ascender: ', Font.OriginToAscender);
    WriteLn('Origin to Descender: ', Font.OriginToDescender);
    WriteLn;
    WriteLn('Character Details:');
    WriteLn('==================');
    for i := 0 to Min(9, Font.CharacterCount - 1) do
    begin
      CharDef := Font.GetCharacterByIndex(i);
      WriteLn('Character: ''', Chr(CharDef.CharCode), ''' (', CharDef.CharCode, ')');
      WriteLn('  Width: ', CharDef.Width);
      WriteLn('  Commands: ', Length(CharDef.Commands));
      for j := 0 to High(CharDef.Commands) do
      begin
        Write('    ');
        case CharDef.Commands[j].Operation of
          soEndChar: WriteLn('END');
          soScan: WriteLn('SCAN');
          soMoveTo: WriteLn('MOVE (', CharDef.Commands[j].X, ',', CharDef.Commands[j].Y, ')');
          soLineTo: WriteLn('LINE (', CharDef.Commands[j].X, ',', CharDef.Commands[j].Y, ')');
        end;
      end;
      WriteLn;
    end;
    if Font.CharacterCount > 10 then
      WriteLn('... (', Font.CharacterCount - 10, ' more characters)');
  finally
    Font.Free;
  end;
end;

begin
  if ParamCount < 1 then
  begin
    WriteLn('Usage:');
    WriteLn('  testchr --create           Create sample font');
    WriteLn('  testchr <font.chr>         Display font info');
    Exit;
  end;
  if ParamStr(1) = '--create' then
    CreateSampleFont
  else
    DisplayFontInfo(ParamStr(1));
end.
