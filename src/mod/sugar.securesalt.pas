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

const
    __saltSize = 32;

{Crypto functions}
function GenerateSecureSalt(LengthInBytes: integer): TBytes; overload;
{Convenience functions}
function genSecureSaltB64(LengthInBytes: integer = __saltSize): string; overload;
function genSecureSaltHex(LengthInBytes: integer = __saltSize): string; overload;
{URLSafe}
function genSafeSecureSaltB64(LengthInBytes: integer = __saltSize): string; overload;


implementation
uses
    sugar.hexconv;


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



function genSecureSaltB64(LengthInBytes: integer): string;
begin
    Result := BytesToBase64(GenerateSecureSalt(LengthInBytes));
end;

function genSecureSaltHex(LengthInBytes: integer): string;
var
    _salt: TBytes;
begin
    _salt := GenerateSecureSalt(LengthInBytes);
    Result := BytesToHex(_salt);
end;

function genSafeSecureSaltB64(LengthInBytes: integer): string;
begin
    Result := BytesToBase64URLSafe(GenerateSecureSalt(LengthInBytes));
end;


end.
