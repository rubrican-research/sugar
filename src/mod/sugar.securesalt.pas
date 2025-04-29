unit sugar.securesalt;
{******************************************************************************
  This unit was generated with the assistance of ChatGPT (OpenAI)
  Purpose: Provides secure cryptographic salt generation utilities
           including Base64, URL-safe Base64, and hexadecimal encodings
  Compatible with: Lazarus / Free Pascal on Windows, Linux, macOS
******************************************************************************}

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Classes, {$IFDEF MSWINDOWS} JwaWinCrypt, {$ELSE} BaseUnix, {$ENDIF}
    base64;

{Crypto functions}
function GenerateSecureSalt(LengthInBytes: integer): TBytes; overload;
function BytesToHex(const Bytes: TBytes): string;
function BytesToBase64(const Bytes: TBytes): string;
function BytesToBase64URLSafe(const Bytes: TBytes; StripPadding: boolean = True): string;

{Convenience functions}
function genSecureSalt(LengthInBytes: integer = 48): string; overload;
{URLSafe}
function genSafeSecureSalt(LengthInBytes: integer = 48): string; overload;

implementation

function GenerateSecureSalt(LengthInBytes: integer): TBytes;
    {$IFDEF MSWINDOWS}
var
    //hProv: HCRYPTPROV;
    hProv: ULONG_PTR;

    {$ENDIF}
begin
    Result := [];
    SetLength(Result, LengthInBytes);

    {$IFDEF MSWINDOWS}
    if not CryptAcquireContext(hProv, nil, nil, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT) then
        raise Exception.Create('Failed to acquire crypto context');

    if not CryptGenRandom(hProv, LengthInBytes, @Result[0]) then
        raise Exception.Create('Failed to generate secure random bytes');

    CryptReleaseContext(hProv, 0);

    {$ELSE}
    var F: integer;
    F := fpOpen('/dev/urandom', O_RDONLY);
    if F < 0 then
        raise Exception.Create('Unable to open /dev/urandom');

    if fpRead(F, Result[0], LengthInBytes) <> LengthInBytes then
    begin
        fpClose(F);
        raise Exception.Create('Failed to read enough random bytes');
    end;

    fpClose(F);
    {$ENDIF}
end;

function BytesToHex(const Bytes: TBytes): string;
var
    i: integer;
begin
    Result := '';
    for i := 0 to High(Bytes) do
        Result := Result + IntToHex(Bytes[i], 2);
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
    Base64: string;
begin
    Base64 := BytesToBase64(Bytes);
    Base64 := StringReplace(Base64, '+', '-', [rfReplaceAll]);
    Base64 := StringReplace(Base64, '/', '_', [rfReplaceAll]);

    if StripPadding then
        while Base64.EndsWith('=') do
            Delete(Base64, Length(Base64), 1);

    Result := Base64;
end;

function genSecureSalt(LengthInBytes: integer): string;
begin
    Result := BytesToBase64(GenerateSecureSalt(LengthInBytes));
end;

function genSafeSecureSalt(LengthInBytes: integer): string;
begin
    Result := BytesToBase64URLSafe(GenerateSecureSalt(LengthInBytes));
end;

end.
