unit sugar.utils;

{A subset of functions from rbutils. This is to remove dependencies.}

{********************************************************************}
{Uses the rhl package https://github.com/maciejkaczkowski/rhl}
{********************************************************************}

{$mode ObjFPC}{$H+}

interface

uses
     Classes, SysUtils, fpjson;

function appendPath(_paths: array of string; _delim: string = DirectorySeparator): string;
function appendURL(_paths: array of string): string;
function sanitizeFileName(_varName: string; _extension: string = '.txt'): string;

function getFileContent(const _filename: string; _touch: boolean = false): string;
function saveFileContent(const _filename: string; _content: string): integer;

{returns the mime type for this file}
function getMimeTypeFor(_fileName: string): string;
function getMimeType(_extension: string): string;


{Data conversion functions}
function encodeURL(const _url: string): string;
function decodeURL(const _url: string): string;
function decodeURLEntities(const _url: string): string;

function makeStringArray(_strings: TStrings): TStringArray;
function stringFromArray(_arr: array of string): string;

function toStringList(const _source: string; _delim: string): TStrings;
function toStringArray(const _source: string; _delim: string = ','): TStringArray;
function stringArrayToJSON(const _source: TStringArray): TJSONArray;
function JSONToStringArray(const _source: TJSONArray): TStringArray;
function getPChar(const _text: string): PChar; {allocates memory, returns pChar}

{Hashing}
function genHash(p, s: ansistring; i: integer = 13; size: integer = 196): ansistring;
function genHashUTF8(p, s: ansistring; i: integer = 13;
    size: integer = 196): RawByteString;

{Key generation}
const
    KEYGEN_NUM = $01;
    KEYGEN_SMALL_LETTERS = $02;
    KEYGEN_CAP_LETTERS = $04;
    KEYGEN_ALPHANUM = $07;
    KEYGEN_ALL = $0f;

function genRandomKey(_size: word; _strength: byte = 2): ansistring;
//function genRandomKey(_size: word; _strength: byte = KEYGEN_ALPHANUM; _numpass: word = 3): ansistring;
function genPassword(_size: word = 12; _strength: word = 3): ansistring;
function genSalt(_size: word = 18; _strength: word = 4): ansistring;

{raise an exception}
procedure trip(const _msg: string);

{Base64 encoding functions}
{Returns the content of a file as Base64 encoded string}
function FileToBase64(_fileName: string): string;
{Decodes a Base64 encoded string and saves it in a file}
procedure Base64ToFile(_b64String: string; _fileName: string);



implementation
uses
     fpmimetypes, variants, strutils, jsonparser, FileUtil,
     LazFileUtils, jsonscanner, rhlCore, rhlTiger2, RegExpr,
     math, base64, LazStringUtils;
	{nsort, MarkdownUtils,}

var
	{No need to Free this because it points to the singleton in fpmimetypes}
    myMimeTypes: TFPMimeTypes = nil;

function appendPath(_paths: array of string; _delim: string): string;
var
    _i, _count: integer;
    _tmpPath: string;
begin
    Result := '';
    _count := Length(_paths);
    for _i := 0 to pred(_count) do
    begin


        if (Result.Length > 0) and Result.EndsWith(_delim) then
            Result := Result.Remove(Result.Length - 1, 1);

        _tmpPath := _paths[_i];
        if not _tmpPath.isEmpty then
        begin
	        if _i =  0 then
            begin
                {This is the first item.
                Check for drive letter in case of windows.
                Don't append the directory separator}
			end
            else if (not _tmpPath.StartsWith(_delim)) then
                _tmpPath := _delim + _tmpPath;

            Result := Result + _tmpPath;
	    end;

    end;
end;

function appendURL(_paths: array of string): string;
begin
    Result := appendPath(_paths, '/');
end;

function sanitizeFileName(_varName: string; _extension: string): string;
var
    i: integer;
begin
    Result := '';
    {Replace invalid characters in one pass}
    for i := 1 to _varName.Length do
    begin
        case _varName[i] of
            ' ', ':', '.',
            DirectorySeparator: Result := Result + '-';
            else
                Result := Result + _varName[i];
        end;
    end;

    if _extension.IsEmpty then
        exit;

    {else}
    if not _extension.StartsWith('.') then
        Result := Result + '.';

    Result := Result + _extension;
end;

function getFileContent(const _filename: string; _touch: boolean): string;
begin
    Result := '';
    if FileExists(_filename) then
        with TStringList.Create do
        begin
            LoadFromFile(_filename);
            Result := Text;
            Free;
        end
    else if _touch then
    begin
        saveFileContent(_filename, '');
	end;
end;

function saveFileContent(const _filename: string; _content: string): integer;
begin
    Result:= 0;
    {$IFDEF windows}
    with TStringList.Create do
    begin
        Text:= _content;
        SaveToFile(_filename);
        Free;
    end;
    {$ELSE}
    TThreadFileWriter.put(_filename, _content);
    {$ENDIF}
end;

function getMimeTypeFor(_fileName: string): string;
begin
    Result:= getMimeType(ExtractFileExt(_fileName));
end;

function getMimeType(_extension: string): string;
begin
    {This is to load mimetypes only once}
    if not Assigned(myMimeTypes) then
    begin
        {myMimeTypes does not need to be freed explicitly because
        it the object is created in fpmimetypes and is freed there.
        We are only using a pointer to the created object through the
        factory funciton MimeTypes()}
        myMimeTypes:= MimeTypes;

        {$IFDEF windows}
        myMimeTypes.LoadKnownTypes;
        {$ELSE}
        try
            myMimeTypes.LoadKnownTypes;
		except
            ;
		end;
		{$ENDIF}
	end;
    try
        Result:= myMimeTypes.GetMimeType(_extension);
	except
        Result:= '';
	end;
end;


function encodeURL(const _url: string): string;
var
    i: integer;
begin
    i := _url.Length;
    Result := '';
    while (i > 0) do
    begin
        case _url[i] of
            ' ', '"', '%', '!', '#', '$', '&',
            '''', '(', ')', '*', '+', ',', '/',
            '-', '.', '<', '>', '\', '^', '_', '`',
            '{', '|', '}', '~', ';', '=', '?',
            '@', '[', ']':
                Result := '%' + Ord(_url[i]).ToHexString(2) + Result;
            #13: ; // Result := '%0A'+ Result;
            #10: Result := '%0A'+ Result;
            else
                Result := _url[i] + Result;
        end;
        Dec(i);
    end;
end;

function decodeURL(const _url: string): string;
const
    CODE_SIZE = 2;
var
    len, i, safeLim: integer;
    _code: string;
    safeToCheck: boolean;
    tmpStr: string;

begin
    Result := '';
    i := 1;
    len := _url.Length;
    safeLim := len - CODE_SIZE;
    while (i <= len) do
    begin
        safeToCheck := (i <= safeLim);
        tmpStr := _url[i];
        if (tmpStr = '%') and safeToCheck then
        begin
            tmpStr := char(StrToInt('$' + _url[i + 1] + _url[i + 2]));
            Inc(i, CODE_SIZE);
        end;
        Result := Result + tmpStr;
        Inc(i);
    end;
end;

function decodeURLEntities(const _url: string): string;
begin
    Result:= _url.Replace('&amp;', '&');
end;

function makeStringArray(_strings: TStrings): TStringArray;
var
    i: integer;
begin
    SetLength(Result, _strings.Count);
    for i := 0 to _strings.Count - 1 do
        Result[i] := _strings[i];
end;

function stringFromArray(_arr: array of string): string;
var
    s: string;
begin
    Result := '';
    for s in _arr do
    begin
        if not Result.IsEmpty then
            Result := Result + ' ';
        Result := Result + s;
    end;
end;

function toStringList(const _source: string; _delim: string): TStrings;
var
    _array: TStringArray;
    _count, _i: integer;
begin
    Result := TStringList.Create;
    _array := _source.Split([_delim]);
    _count := length(_array);
    for _i := 0 to _count - 1 do
    begin
        Result.Add(Trim(_array[_i]));
    end;
    SetLength(_array, 0);
end;

function toStringArray(const _source: string; _delim: string): TStringArray;
var
    i: integer;
begin
    if Trim(_source).isEmpty then
        setLength(Result, 0)
    else
    begin
        Result := _source.split(_delim);
        for i := 0 to High(Result) do
            Result[i] := Trim(Result[i]); {make sure that each item is trimmed}

	end;
end;

function stringArrayToJSON(const _source: TStringArray): TJSONArray;
var
	_val: String;
begin
    Result:= TJSONArray.Create;
    for _val in _source do
        Result.Add(_val);
end;

function JSONToStringArray(const _source: TJSONArray): TStringArray;
var
	_el: TJSONEnum;
begin
    SetLength(Result, _source.count);
    for _el in _source do
    begin
        case _el.Value.JSONType of
        jtString, jtNumber, jtBoolean:
            Result[_el.KeyNum] := _el.Value.AsString;
        jtArray, jtObject:
            Result[_el.KeyNum] := _el.Value.AsJSON;
		end;

	end;
end;

function getPChar(const _text: string): PChar;
begin
    Result := nil;
    if not _text.isEmpty then
    begin
        Result := StrAlloc(_text.Length + 1);
        StrPCopy(Result, _text);
    end;
end;


function genHash(p, s: ansistring; i: integer; size: integer): ansistring;
begin
    //Result:= p;
    Result := rhlPBKDF2(p, s, i, size, TrhlTiger2);
end;

function genHashUTF8(p, s: ansistring; i: integer; size: integer): RawByteString;
begin
    Result := AnsiToUtf8(genHash(p, s, i, size));
end;

{
function genRandomKey(_size: word; _strength: byte)
Strength is a number between 0 - 5
    0 = numbers
    1 = include small letters
    2 = include capital letters
    3 = include weak special characters
    4 = inlude special characters that contain programming language symbols}
function genRandomKey(_size: word; _strength: byte): ansistring;
const
    keyNumbersLen = 10;
    keyNumbers = '0123456789';

    keyCapLettersLen = 26;
    keySmallLetters = 'abcdefghijklmnopqrstuvwxyz';

    keySmallLettersLen = 26;
    keyCapLetters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';

    keySpecialCharsLen = 8;
    keySpecialChars = '!$()@[~]';

    keySpecialChars2Len = 14;
    keySpecialChars2 = '^&*=+-_<>?:;|\';
var
    i: integer;
    x: integer;

    function randomIndex(_len: integer): byte;
    begin
        Result := random(_len) + 1; {to avoid zero}
        if Result > _len then
            Result := _len; {to limit exceeding length}
    end;

begin
    Result := '';
    {limit the value of strength}
    if (_strength > 5) then
        _strength := 3;

    for x := 1 to _size do
    begin

        {Simple algorithm.
            1) Randomly select between types of keys (decided by _strength)
            2) Generate a random number less than string length
            3) Return the character at that index.
        Repeat until size is reached}

        i := Random(_strength);
        case i of
            0: AppendStr(Result, keyNumbers[randomIndex(keyNumbersLen)]);
            1: AppendStr(Result, keySmallLetters[randomIndex(keySmallLettersLen)]);
            2: AppendStr(Result, keyCapLetters[randomIndex(keyCapLettersLen)]);
            3: AppendStr(Result, keySpecialChars[randomIndex(keySpecialCharsLen)]);
            4: AppendStr(Result, keySpecialChars2[randomIndex(keySpecialChars2Len)]);
        end;
    end;
end;

function genPassword(_size: word; _strength: word): ansistring;
begin
    Result := genRandomKey(_size, _strength);
end;

function genSalt(_size: word; _strength: word): ansistring;
begin
    Result := genRandomKey(_size, _strength);
end;

procedure trip(const _msg: string);
begin
    raise Exception.Create(_msg);
end;


function FileToBase64(_fileName: string): string;
var
  sourceStream: TFileStream;
  destStream: TStringStream;
  b64Encoder: TBase64EncodingStream;
begin
    try
        sourceStream:= TFileStream.Create(_fileName, fmOpenRead);
        destStream:= TStringStream.Create('');
        b64Encoder:= TBase64EncodingStream.Create(destStream);
        b64Encoder.CopyFrom(sourceStream, sourceStream.Size);
        Result:= destStream.DataString;
    finally
        b64Encoder.Free;
        destStream.Free;
        sourceStream.Free;
	end;
end;

procedure Base64ToFile(_b64String: string; _fileName: string);
var
  sourceStream: TStringStream;
  destStream: TFileStream;
  b64Decoder: TBase64DecodingStream;
begin
    try
        sourceStream:= TStringStream.Create(_b64String);
        destStream:= TFileStream.Create(_fileName, fmCreate or fmOpenWrite);
        b64Decoder:= TBase64DecodingStream.Create(sourceStream);
        destStream.CopyFrom(b64Decoder, b64Decoder.Size);
	finally
        b64Decoder.Free;
        destStream.Free;
        sourceStream.Free;
	end;
end;




initialization
    randomize;
     //InitCriticalSection(myFilerCriticalSection);
     //writeln('This Commander Riker @ TID = ', GetThreadID);
     //InitCriticalSection(myLockerCriticalSection);

finalization
    //TTextFiler.shutdown;
    //DoneCriticalSection(myFilerCriticalSection);
    // DoneCriticalSection(myLockerCriticalSection);

end.
end.

