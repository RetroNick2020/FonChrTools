{ TestVector - Minimal vector FON test.  
  Creates 3 characters and saves them as vector font.
  By RetroNick - Initial Code Released Dec 19 - 2025
}  
program TestVector;

{$mode objfpc}{$H+}

uses
  SysUtils, FONCreate;

var
  Font: TFONVectorCreator;
begin
  Font := TFONVectorCreator.Create;
  try
    Font.FontName := 'TestVec';
    Font.Copyright := 'Test';
    Font.Height := 16;
    Font.Ascent := 14;
    Font.PointSize := 12;
    
    
    // 'A' Upside down
    Font.BeginChar(65, 10);
    Font.MoveTo(65, 0, 0);     // Bottom left
    Font.LineTo(65, 5, 14);    // Up to apex
    Font.LineTo(65, 10, 0);    // Down to bottom right
    Font.MoveTo(65, 2, 5);     // Crossbar start
    Font.LineTo(65, 8, 5);     // Crossbar end
    Font.MoveTo(65, 10, 0);    // Trailing MoveTo
    
    // 'B'
    Font.BeginChar(ORD('B'), 9);
    Font.MoveTo(66, 0, 0);
    Font.LineTo(66, 0, 14);
    Font.LineTo(66, 6, 14);
    Font.LineTo(66, 8, 12);
    Font.LineTo(66, 8, 9);
    Font.LineTo(66, 6, 7);
    Font.LineTo(66, 0, 7);
    Font.MoveTo(66, 6, 7);
    Font.LineTo(66, 8, 5);
    Font.LineTo(66, 8, 2);
    Font.LineTo(66, 6, 0);
    Font.LineTo(66, 0, 0);
    Font.MoveTo(66, 9, 0);
    
    // 'C'
    Font.BeginChar(67, 10);
    Font.MoveTo(67, 9, 12);
    Font.LineTo(67, 7, 14);
    Font.LineTo(67, 3, 14);
    Font.LineTo(67, 0, 11);
    Font.LineTo(67, 0, 3);
    Font.LineTo(67, 3, 0);
    Font.LineTo(67, 7, 0);
    Font.LineTo(67, 9, 2);
    Font.MoveTo(67, 10, 0);
    
    Font.SaveToFile('TESTVEC.FON');
    WriteLn('Created TESTVEC.FON');
    
  finally
    Font.Free;
  end;
end.
