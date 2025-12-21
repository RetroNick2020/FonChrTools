{ TestBitmap - Minimal bitmap FON test 
  Creates 3 characters and saves them as raster/bitmap font.
 
  By RetroNick - Initial Code Released Dec 19 - 2025
}  
program TestBitmap;

{$mode objfpc}{$H+}

uses
  SysUtils, FONCreate;

var
  Font: TFONBitmapCreator;
begin
  Font := TFONBitmapCreator.Create;
  try
    Font.FontName := 'TestBmp';
    Font.Copyright := 'Test';
    Font.Height := 8;
    Font.Ascent := 7;
    Font.PointSize := 8;
    
     // 'A'
    Font.SetCharacterPattern(65, 8, [
      '        ',
      '   XX   ',
      '  X  X  ',
      ' X    X ',
      ' XXXXXX ',
      ' X    X ',
      ' X    X ',
      '        '
    ]);
    
    // 'B'
    Font.SetCharacterPattern(66, 8, [
      '        ',
      ' XXXXX  ',
      ' X    X ',
      ' XXXXX  ',
      ' X    X ',
      ' X    X ',
      ' XXXXX  ',
      '        '
    ]);
    
    // 'C'
    Font.SetCharacterPattern(67, 8, [
      '        ',
      '  XXXX  ',
      ' X      ',
      ' X      ',
      ' X      ',
      ' X      ',
      '  XXXX  ',
      '        '
    ]);
    
    Font.SaveToFile('TESTBMP.FON');
    WriteLn('Created TESTBMP.FON');
    
  finally
    Font.Free;
  end;
end.
