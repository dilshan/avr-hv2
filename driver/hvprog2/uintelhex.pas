{===============================================================================

  Arduino based AVR High Voltage Programmer 2
  [https://github.com/dilshan/avr-hv2]

  Copyright (c) 2020 Dilshan R Jayakody [jayakody2000lk at gmail dot com]

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
===============================================================================}

unit uintelhex;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TByteArray = array of byte;

  TIntelHex = class
  private
    function isChecksumPass(hexLine: string; recLength: integer) : boolean;
    function CalculateChecksum(line: string; startPos: integer; endPos: integer; offset: integer; initChecksum: integer) : string;
  public
    function LoadIntelHexFile(filename: string) : TByteArray;
    function SaveIntelHexFile(filename: string; data: TByteArray) : boolean;
  end;

implementation

function TIntelHex.isChecksumPass(hexLine: string; recLength: integer) : boolean;
var
  tmpStr: string;
  Check, CheckSum, pos: integer;
begin
  // Extract checksum value from record.
  tmpStr := '$' + Copy(hexLine, (Length(hexLine) - 1), 2);
  CheckSum:=StrToInt(tmpStr);
  Check := RecLength;

  // Prform checksum operation.
  for pos := 1 to RecLength + 3 do
  begin
    tmpStr := '$' + Copy(hexLine, (2 + pos * 2), 2);
    Check := Check + StrToInt(tmpStr);
  end;

  Check := 256 - (Check - (Check div 256) * 256);

  if Check = 256 then
  begin
    Check := 0;
  end;

  if Check <> CheckSum then
  begin
    // Checksum fail.
    result := false;
    exit;
  end;

  result := true;
end;

function TIntelHex.CalculateChecksum(line: string; startPos: integer; endPos: integer; offset: integer; initChecksum: integer) : string;
var
  pos, checkSum: integer;
begin
  checkSum := initChecksum;

  for pos := startPos to endPos do
  begin
    checkSum := checkSum + StrToInt('$' + Copy(line, offset + pos * 2, 2));
  end;

  checkSum:=256 - (checkSum - (checkSum div 256) * 256);

  if checkSum=256 then
  begin
    checkSum := 0;
  end;

  result := line + IntToHex(checkSum, 2);
end;

function TIntelHex.SaveIntelHexFile(filename: string; data: TByteArray) : boolean;
var
  offset, pos, tempPos : integer;
  outputFile: TStringList;
  Line : string;
  lenEnd, lenStart :integer;
begin
  result := true;

  if (Length(data) <= 0) then
  begin
    // No data is available to write.
    result := false;
  end;

  offset := 0;
  outputFile := TStringList.Create;

  for pos := 0 to Length(data) div 16 - 1 do
  begin
    if ((pos div $1000) > 0) and (Offset <> (pos div $1000)) then
    begin
      // Write an extended linear address record.
      Offset := pos div $1000;
      Line := ':02000004' + IntToHex(Offset, 4);
      Line := CalculateChecksum(Line, 1, 6, 0, 0);
      outputFile.Add(Line);
    end;

    Line := ':' + IntToHex(16, 2) + IntToHex(pos*16-Offset*$10000, 4) + '00';

    for tempPos := 0 to 15 do
    begin
      Line := Line + IntToHex(data[pos * 16 + tempPos], 2);
    end;

    // Add checksum to end of record.
    Line := CalculateChecksum(Line, 1, 19, 2, 16);
    outputFile.Add(Line);
  end;

  if (pos = 0) then
  begin
    pos := -1;
  end;

  // Setup end of file record.
  lenEnd := Length(data) div 16 * 16;
  lenStart := Length(data) ;
  Line := ':' + IntToHex(lenStart - lenEnd + 1, 2) + IntToHex((pos + 1) * 16 - Offset * $10000, 4) + '00';

  for tempPos := 0 to (lenStart - lenEnd) do
  begin
     Line := Line + IntToHex(data[lenEnd + tempPos], 2);
  end;

  Line := CalculateChecksum(Line, 0, lenStart - lenEnd + 4, 2, 0);
  outputFile.Add(Line);

  Line:=':00000001FF';
  outputFile.Add(Line);
  outputFile.SaveToFile(filename);
end;

function TIntelHex.LoadIntelHexFile(filename: string) : TByteArray;
var
  hexFile: TStringList;
  hexLine : string;
  recLength, recType, startAddr: integer;
  curAddr, pos, offset, hexPos : integer;
  buffer: TByteArray;

begin
  try
    hexPos := 0;
    offset := 0;
    curAddr := 0;

    hexFile := TStringList.Create;
    hexFile.LoadFromFile(filename);

    // Create temporary buffer to write binary data.
    setLength(buffer, 1024 * 1024 * 10);

    // Fill data buffer with 0xFFFF.
    while(hexPos < 1024 * 1024 * 10) do
    begin
      buffer[hexPos] := $FF;
      Inc(hexPos);
    end;

    hexPos := 0;

    // Start reading specified hex file.
    while(hexPos < hexFile.Count) do
    begin
      hexLine := hexFile[hexPos];

      // Check for valid hex file entry.
      if (Length(hexLine) = 0) or (hexLine[1] <> ':') then
      begin
        SetLength(buffer, 0);
        result := buffer;
        exit;
      end;

      recLength := StrToInt('$' + Copy(hexLine, 2, 2));
      startAddr :=StrToInt('$' + Copy(hexLine, 4, 4));
      recType := StrToInt(Copy(hexLine, 8, 2));

      // Check for valid record type.
      if not recType in [0..2, 4] then
      begin
        SetLength(buffer, 0);
        result := buffer;
        exit;
      end;

      case recType of
        0:
          // Process data record.
          begin
            for pos := 1 to RecLength do
            begin
              curAddr := startAddr + pos + offset - 1;

              if curAddr > Length(buffer) then
              begin
                // Out of buffer space.
                SetLength(buffer, 0);
                result := buffer;
                exit;
              end;

              buffer[curAddr]:=StrToInt('$' + Copy(hexLine, 10 + (pos - 1) * 2, 2));
            end;

            if not isChecksumPass(hexLine, RecLength) then
            begin
              // Checksum error detected.
              SetLength(buffer, 0);
              result := buffer;
              exit;
            end;
          end;

        2:
          // Process extended segment address record.
          begin
            if startAddr <> 0 then
            begin
              SetLength(buffer, 0);
              result := buffer;
              exit;
            end;

            offset := StrToInt('$' + Copy(hexLine, 10, 4) + '0');

            if not isChecksumPass(hexLine, RecLength) then
            begin
              // Checksum error detected.
              SetLength(buffer, 0);
              result := buffer;
              exit;
            end;
          end;

        4:
          // Process extended linear address record.
          begin
            if startAddr <> 0 then
            begin
              SetLength(buffer, 0);
              result := buffer;
              exit;
            end;

            offset := StrToInt('$' + Copy(hexLine, 10, 4) + '0000');

            if not isChecksumPass(hexLine, RecLength) then
            begin
              // Checksum error detected.
              SetLength(buffer, 0);
              result := buffer;
              exit;
            end;
          end;

        1:
          // Handle end of file record.
          begin
            if not isChecksumPass(hexLine, RecLength) then
            begin
              // Checksum error detected.
              SetLength(buffer, 0);
              result := buffer;
              exit;
            end;
          end;
      end;

      // Move to next line of the hex file.
      Inc(hexPos);
    end;

    // Shrink buffer to highest address range.
    SetLength(buffer, curAddr);
    result := buffer;
  except
    SetLength(buffer, 0);
    result := buffer;
  end;
end;

end.

