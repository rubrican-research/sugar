unit sugar.securehash;

{$mode objfpc}{$H+}

{Generated with assistance from OpenAI ChatGPT 5}
{Quick, hardening checklist
    - Normalize + constant-time compares (no plain =).
    - Strict parsing: fixed algo tag (pbkdf2-sha256), even-length hex, sane bounds (salt len, DK len).
    - Parameter caps to avoid DoS: iters ∈ [min..max] (e.g., 10_000 .. 3_000_000).
    - Golden test vectors baked in (so refactors can’t silently break you).
}

interface

uses
  SysUtils,
  rhlCore, rhlSHA256; // matches your Rhl package list

const
  __algo = 'pbkdf2-sha256';
  __iter = 300000;  // default iterations
  __hashSize= 32; // default size of hash

// If you want the raw derived key bytes instead of hex:
function PBKDF2_SHA256_Bytes(const Password, Salt: RawByteString;
  Iterations, OutLen: Integer): RawByteString;

// Returns lowercase-hex PBKDF2-HMAC-SHA-256(Password, Salt, Iterations, OutLen)
function genSecureHashHex(pwd, salt: ansistring; iter: integer = __iter; size: integer=__hashSize): ansistring;

// Generates a hash and stores it in the format:
// <algorithm>$iterations$<hex-salt>$<hex-hash>
function genStorableSecureHashHex(pwd, salt: ansistring; iter: integer = __iter; size: integer=__hashSize): ansistring;

function verifyPassword(pwd, _storedHashHex: string): boolean;

implementation

uses
  sugar.utils, sugar.hexconv,  Math;

const
  __minIteration = 300;
  __maxIteration = 3000000;

type
  TSha256Digest = array[0..31] of Byte;

function DigestToStr(const D: TSha256Digest): RawByteString; inline;
begin
  SetLength(Result, SizeOf(D));
  Move(D[0], Result[1], SizeOf(D));
end;

// ---------------- HMAC-SHA256 via Rhl (string-only Update) -------------------

procedure HMAC_SHA256(const Key, Data: RawByteString; out Mac: TSha256Digest);
const
  BlockSize = 64; // SHA-256 block size
var
  k0, ipad, opad: RawByteString;
  i: Integer;
  sha: TrhlSHA256;
  tmp: TSha256Digest;
begin
  // 1) key -> k0 (hash if > blocksize; pad with zeros to blocksize)
  k0 := Key;
  if Length(k0) > BlockSize then
  begin
    sha := TrhlSHA256.Create;
    try
      sha.Init; sha.Update(k0); sha.Final(tmp);
    finally
      sha.Free;
    end;
    SetLength(k0, 32);
    Move(tmp[0], k0[1], 32);
  end;
  if Length(k0) < BlockSize then
    k0 := k0 + StringOfChar(#0, BlockSize - Length(k0));

  // 2) ipad/opad
  SetLength(ipad, BlockSize);
  SetLength(opad, BlockSize);
  for i := 1 to BlockSize do
  begin
    ipad[i] := AnsiChar(Byte(k0[i]) xor $36);
    opad[i] := AnsiChar(Byte(k0[i]) xor $5C);
  end;

  // 3) inner = H(ipad || data)
  sha := TrhlSHA256.Create;
  try
    sha.Init; sha.Update(ipad); sha.Update(Data); sha.Final(tmp);
  finally
    sha.Free;
  end;

  // 4) outer = H(opad || inner)
  sha := TrhlSHA256.Create;
  try
    sha.Init; sha.Update(opad); sha.Update(DigestToStr(tmp)); sha.Final(Mac);
  finally
    sha.Free;
  end;
end;

// ---------------- PBKDF2 (RFC 8018) with HMAC-SHA256 -------------------------
const
  __delim = '$';

function genStorableSecureHashHex(pwd, salt: ansistring; iter: integer;
	size: integer): ansistring;
begin
    // Generates a hash and stores it in the format:
    // <algorithm>$iterations$<hex-salt>$<hex-hash>
    Result :=   __algo
                + __delim + inttostr(iter)
                + __delim + toHexLower(salt)
                + __delim + genSecureHashHex(pwd,salt,iter,size);
end;

function verifyPassword(pwd, _storedHashHex: string): boolean;
var
	_iter: Longint;
	_params: TStringArray;
	_salt, _storedHash: String;
	_pwdHash: RawByteString;
begin
  // <algorithm>$iterations$<hex-salt>$<hex-hash>
    _params := toStringArray(_storedHashHex, __delim);
    if length(_params) <> 4 then exit(false);    // Invalid storedHashHex;
    if _params[0] <> __algo then exit(false);     // Invalid Algorithm;
    if not TryStrToInt(_params[1], _iter) or not InRange( _iter, __minIteration, __maxIteration) then exit(False); // Invalid number of iteration

    _salt := HexToRaw(_params[2]);
    _storedHash := HexToRaw(_params[3]);
    if odd(Length(_salt)) or odd(Length(_storedHash)) then exit(False); // Invalid salt and hash length

    //Calculate the hash for the password
    _pwdHash := PBKDF2_SHA256_Bytes(pwd, _salt, _iter, length(_storedHash));
    // compare the hashes
    Result := SecureEquals(_pwdHash, _storedHash);
end;

function PBKDF2_SHA256_Bytes(const Password, Salt: RawByteString;
  Iterations, OutLen: Integer): RawByteString;
var
  blocks, blk, j, need, offset: Integer;
  U, T: TSha256Digest;
  msg: RawByteString;
begin
  if (Iterations <= 0) or (OutLen <= 0) then
    raise Exception.Create('PBKDF2: bad parameters');

  blocks := (OutLen + 31) div 32;
  SetLength(Result, blocks * 32); // temporary, trimmed at the end

  for blk := 1 to blocks do
  begin
    // msg = Salt || INT_32_BE(blk)
    msg := Salt +
           AnsiChar((blk shr 24) and $FF) +
           AnsiChar((blk shr 16) and $FF) +
           AnsiChar((blk shr 8)  and $FF) +
           AnsiChar( blk         and $FF);

    // U1
    HMAC_SHA256(Password, msg, U);
    T := U;

    // U2..Uc
    for j := 2 to Iterations do
    begin
      HMAC_SHA256(Password, DigestToStr(U), U);
      // T ^= U (unrolled for speed/readability)
      T[0] := T[0] xor U[0];    T[1] := T[1] xor U[1];
      T[2] := T[2] xor U[2];    T[3] := T[3] xor U[3];
      T[4] := T[4] xor U[4];    T[5] := T[5] xor U[5];
      T[6] := T[6] xor U[6];    T[7] := T[7] xor U[7];
      T[8] := T[8] xor U[8];    T[9] := T[9] xor U[9];
      T[10]:= T[10]xor U[10];   T[11]:= T[11]xor U[11];
      T[12]:= T[12]xor U[12];   T[13]:= T[13]xor U[13];
      T[14]:= T[14]xor U[14];   T[15]:= T[15]xor U[15];
      T[16]:= T[16]xor U[16];   T[17]:= T[17]xor U[17];
      T[18]:= T[18]xor U[18];   T[19]:= T[19]xor U[19];
      T[20]:= T[20]xor U[20];   T[21]:= T[21]xor U[21];
      T[22]:= T[22]xor U[22];   T[23]:= T[23]xor U[23];
      T[24]:= T[24]xor U[24];   T[25]:= T[25]xor U[25];
      T[26]:= T[26]xor U[26];   T[27]:= T[27]xor U[27];
      T[28]:= T[28]xor U[28];   T[29]:= T[29]xor U[29];
      T[30]:= T[30]xor U[30];   T[31]:= T[31]xor U[31];
    end;

    // append T
    need := 32;
    if blk = blocks then
      need := ((OutLen - 1) mod 32) + 1;
    offset := (blk-1)*32;
    Move(T[0], Result[offset+1], need);
  end;

  SetLength(Result, OutLen); // trim
end;

function genSecureHashHex(pwd, salt: ansistring; iter: integer; size: integer
	): ansistring;
begin
  // PBKDF2 returns raw bytes; encode as lowercase hex for storage/compare
  Result := ToHexLower(PBKDF2_SHA256_Bytes(pwd, salt, iter, size));
end;

end.

