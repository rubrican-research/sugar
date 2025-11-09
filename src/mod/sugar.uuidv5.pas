unit sugar.uuidv5;

{$mode objfpc}{$H+}

interface

uses
    SysUtils;

function UUIDv5(const Namespace: TGUID; const NameUtf8: rawbytestring): TGUID;

implementation

{$IFDEF FPC}
uses sha1;
{$ENDIF}

type
    TBytes16 = array[0..15] of byte;
    TBytes20 = array[0..19] of byte;

function GuidToRFC4122Bytes(const G: TGUID): TBytes16;
var
    be: TBytes16;
begin
    // RFC 4122 requires network byte order for the input namespace
    be[0] := (G.D1 shr 24) and $FF;
    be[1] := (G.D1 shr 16) and $FF;
    be[2] := (G.D1 shr 8) and $FF;
    be[3] := G.D1 and $FF;

    be[4] := (G.D2 shr 8) and $FF;
    be[5] := G.D2 and $FF;

    be[6] := (G.D3 shr 8) and $FF;
    be[7] := G.D3 and $FF;

    Move(G.D4[0], be[8], 8);
    Result := be;
end;

function RFC4122BytesToGuid(const B: TBytes16): TGUID;
begin
    Result.D1 := (B[0] shl 24) or (B[1] shl 16) or (B[2] shl 8) or B[3];
    Result.D2 := (B[4] shl 8) or B[5];
    Result.D3 := (B[6] shl 8) or B[7];
    Move(B[8], Result.D4[0], 8);
end;

function SHA1Bytes(const Data: TBytes): TBytes20;
    {$IFDEF FPC}
var
    ctx: TSHA1Context;
    {$ENDIF}
var
    digest: TBytes20; // array[0..19] of byte;
begin
    {$IFDEF FPC}
    SHA1Init(ctx);
    if Length(Data) > 0 then
        SHA1Update(ctx, Data[0], Length(Data));
    SHA1Final(ctx, digest);
    {$ELSE}
    {$ERROR Provide your preferred SHA-1 implementation here}
    {$ENDIF}
    Result := digest;
end;

function UUIDv5(const Namespace: TGUID; const NameUtf8: rawbytestring): TGUID;
var
    ns: TBytes16;
    buf: TBytes;
    h: array[0..19] of byte;
    out16: TBytes16;
begin
    ns := GuidToRFC4122Bytes(Namespace);

    SetLength(buf, Length(ns) + Length(NameUtf8));
    if Length(ns) > 0 then Move(ns[0], buf[0], Length(ns));
    if Length(NameUtf8) > 0 then Move(NameUtf8[1], buf[Length(ns)], Length(NameUtf8));
    // RawByteString is 1-based

    h := SHA1Bytes(buf);

    // Take first 16 bytes
    Move(h[0], out16[0], 16);

    // Set version (0101)
    out16[6] := (out16[6] and $0F) or $50;

    // Set variant (10xx xxxx)
    out16[8] := (out16[8] and $3F) or $80;

    Result := RFC4122BytesToGuid(out16);
end;

end.
