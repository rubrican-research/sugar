unit sugar.hexconv;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, base64;

type
    EHexConvError = class(Exception);

function ToHexLower(const bin: RawByteString): AnsiString; inline;
function HexToRaw(const hex: RawByteString): RawByteString;
function HexNibble(c: AnsiChar): Integer; inline;

function BytesToHex(const Bytes: TBytes): string;
function BytesToBase64(const Bytes: TBytes): string;
function BytesToBase64URLSafe(const Bytes: TBytes; StripPadding: boolean = True): string;


implementation

function ToHexLower(const bin: RawByteString): AnsiString; inline;
const
  HEX: array[0..15] of AnsiChar =
    ('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f');
var
  i, j: SizeInt; b: Byte;
begin
  SetLength(Result, Length(bin) * 2);
  j := 1;
  for i := 1 to Length(bin) do
  begin
    b := Byte(bin[i]);
    Result[j]   := HEX[b shr 4];
    Result[j+1] := HEX[b and $0F];
    Inc(j, 2);
  end;
end;

function HexToRaw(const hex: RawByteString): RawByteString;
var
  i, n, hi, lo: Integer;
begin
  n := Length(hex);
  if (n and 1) <> 0 then
    raise EHexConvError.Create('Odd-length hex');

  SetLength(Result, n div 2);
  for i := 0 to (n div 2) - 1 do
  begin
    hi := HexNibble(hex[i*2+1]);
    lo := HexNibble(hex[i*2+2]);
    if (hi < 0) or (lo < 0) then
      raise EHexConvError.Create('Invalid hex digit');
    Result[i+1] := AnsiChar((hi shl 4) or lo);
  end;
end;

function HexNibble(c: AnsiChar): Integer; inline;
begin
  case c of
    '0'..'9': exit(Ord(c)-Ord('0'));
    'a'..'f': exit(Ord(c)-Ord('a')+10);
    'A'..'F': exit(Ord(c)-Ord('A')+10);
  end;
  Result := -1;
end;

function BytesToHex(const Bytes: TBytes): string;
const
    HEX: array[0..15] of ansichar =
        ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
    i, j: SizeInt;
    b: byte;
begin
    SetLength(Result, Length(Bytes) * 2);
    j := 1;
    for i := 0 to High(Bytes) do
    begin
        b := Bytes[i];
        Result[j] := HEX[b shr 4];
        Result[j + 1] := HEX[b and $0F];
        //Result := Result + HEX[b shr 4] + HEX[b and $0F];
        Inc(j, 2);
    end;
end;

function BytesToBase64(const Bytes: TBytes): string;
var
    Stream: TMemoryStream;
    Base64Stream: TStringStream;
    Encoder: TBase64EncodingStream;
begin
    Base64Stream := TStringStream.Create('');
    try
        Encoder := TBase64EncodingStream.Create(Base64Stream);
        try
            Encoder.Write(Bytes[0], Length(Bytes));
        finally
            Encoder.Free;
        end;
        Result := Base64Stream.DataString;
    finally
        Base64Stream.Free;
    end;
end;

function BytesToBase64URLSafe(const Bytes: TBytes; StripPadding: boolean = True): string;
var
    hex: string;
begin
    hex := BytesToBase64(Bytes);
    hex := StringReplace(hex, '+', '-', [rfReplaceAll]);
    hex := StringReplace(hex, '/', '_', [rfReplaceAll]);

    if StripPadding then
        while hex.EndsWith('=') do
            Delete(hex, Length(hex), 1);

    Result := hex;
end;


end.

