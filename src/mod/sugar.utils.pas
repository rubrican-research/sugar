unit sugar.utils;

{A subset of functions from rbutils. This is to remove dependencies.}

{********************************************************************}
{Uses the rhl package https://github.com/maciejkaczkowski/rhl}
{********************************************************************}

{$mode ObjFPC}{$H+}

interface

uses
     Classes, SysUtils, fpjson, sugar.collections, sugar.profiler;

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


{Copies the array and returns length}
function copyStringArray(const _source: array of string;
    var _dest: TStringArray): integer;
function copyVariantArray(const _source: array of variant;
    var _dest: TVariantArray): integer;
function copyStringToVariantArray(const _str: string; var _dest: TVariantArray): integer;
function getCommaSeparatedString(const _list: TStringArray): string;
function getCommaSeparatedString(const _list: TJSONArray): string;

function getDelimitedString(const _list: TStringArray; const _delim: string = ','): string; overload;
function getDelimitedString(const _list: TJSONArray; const _delim: string = ','): string; overload;

type
    THtmlTimeMode = (useLocalTime, useUniversalTime);

{Check what Time mode is being used}
function htmlTimeMode: THtmlTimeMode;

{Forces the htmlDateTime function to use local time}
procedure htmlUseLocalTime;
{Forces the htmlDateTime function to use universal time}
procedure htmlUseUniversalTime;


function htmlDateTime: string;
function htmlDateTime(_tstamp: TDateTime): string;
function htmlDate: string;
function htmlDate(_date: TDateTime): string;
function htmlTime: string;
function htmlTime(_time: TDateTime): string;
function isoTime(_time:TDateTime): string;


function date(_isoDate: string): TDateTime; overload;
function time(_isoTime: string): TDateTime; overload;

{somehow autodetect if it is date time, date or time. ...}
function readHtmlDateTime(_htmlDate: string): TDateTime;

function numToText(const _num: integer): string;

{Data validation functions}
function isValidEmail(const _email: string): boolean;
function isValidDate(const _date: string): boolean;
function isValidDateTime(const _datetime: string): boolean;
function isDecimal(const _num: string): boolean;

{Checks if the passed jsonData is a valid number and returns it}
function asNumber(_j: TJSONData; _default: integer = 0) : integer;

{String manipulation}
{Searches for _word in _source and returns true or false}
function hasWord(_word: string; _source: string): boolean;
function paddedNum(const _num: word; const _padCount: word = 3): string;
function limitWords(_str: string; _limit: word): string;

{immediate if. If condition is true, returns first parameter else returns second}
function iif(_condition: boolean; _trueString: string; _falseString: string = ''): string;

{Data functions}

{Compares _prev and _current to see if the change is within plus or minus _tolerancePercentage
default tolerance is 10%}
function hasDataChanged(_prev: real; _current: real; _tolerancePercentage: real = 0.1): boolean;

function number(_s: string): integer;
function float(_s: string): extended;

{initializes variables}
function initStr(const _val: string; _default: string): string;

{returns the float value formated as a date without separators}
function yyyymmdd(const _float: double): string; overload;
function yyyymmdd(const _dt: TDatetime): string; overload;

{increment a local variable -used for dynamic arrays}
function plus1(var i: integer): integer;
function profiler(const _name: string): TCodeProfiler;

// Get list of files
function getFiles(_dir: string; _filter: string): TStrings;


implementation
uses
     fpmimetypes, variants, strutils, jsonparser, FileUtil,
     LazFileUtils, jsonscanner, rhlCore, rhlTiger2, RegExpr,
     math, base64, LazStringUtils, DateUtils, sugar.logger;

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

function copyStringArray(const _source: array of string;
    var _dest: TStringArray): integer;
var
    i: integer;
begin
    Result := Length(_source); {return length of array}
    setLength(_dest, Result);
    for i := 0 to Result - 1 do
        _dest[i] := Trim(_source[i]);
end;

function copyVariantArray(const _source: array of variant;
    var _dest: TVariantArray): integer;
var
    i: integer;
begin
    Result := Length(_source); {return length of array}
    setLength(_dest, Result);
    for i := 0 to Result - 1 do
        _dest[i] := _source[i];

end;

function copyStringToVariantArray(const _str: string;
    var _dest: TVariantArray): integer;
var
    i: integer;
    len: integer;
    _sArray: TStringArray;
begin
    _sArray := _str.split(',');
    len := Length(_sArray);
    setLength(_dest, len);
    for i := 0 to len - 1 do
        _dest[i] := Trim(_sArray[i]);

    Result := len;
end;

function getCommaSeparatedString(const _list: TStringArray): string;
begin
    Result := getDelimitedString(_list, ',');
end;

function getCommaSeparatedString(const _list: TJSONArray): string;
begin
    Result:= getDelimitedString(_list, ',');
end;

function getDelimitedString(const _list: TStringArray; const _delim: string): string;
var
    _addComma: boolean;
    _item: string;
begin
    Result := '';
    _addComma := False;
    for _item in _list do
    begin
        if _addComma then
            Result := Result + _delim
        else
            _addComma := True;

        Result := Result + _item;
    end;
end;

function getDelimitedString(const _list: TJSONArray; const _delim: string
	): string;
var
	member: TJSONEnum;
    _addDelim: boolean = false;
begin
    Result:= '';
    for member in _list do
    begin
        if _addDelim then
            Result:= Result + _delim
        else
            _addDelim:= true;

        case member.Value.JSONType of
            jtUnknown, jtNull:
                ; {do nothing}

            jtNumber, jtString, jtBoolean:
                Result:= Result + member.value.AsString;

            jtArray, jtObject:
                Result:= Result + member.value.AsJSON;
        end;
	end;
end;

var
    myHtmlTimeMode: THtmlTimeMode;

function htmlTimeMode: THtmlTimeMode;
begin
    Result:= myHtmlTimeMode;
end;

procedure htmlUseLocalTime;
begin
    myHtmlTimeMode:= useLocalTime;
end;

procedure htmlUseUniversalTime;
begin
    myHtmlTimeMode:= useUniversalTime;
end;

function htmlDateTime: string;
begin
    Result := htmlDateTime(now);  // htmlDateTime(now);
end;

function htmlDateTime(_tstamp: TDateTime): string;
begin
    //Result := DateToISO8601(now, (htmlTimeMode = useUniversalTime));
    Result := htmlDate(_tstamp) + 'T' + htmlTime(_tstamp);
end;

function htmlDate: string;
begin
    Result := htmlDate(Now);
end;

function htmlDate(_date: TDateTime): string;
begin
    Result := FormatDateTime('yyyy-mm-dd', _date);
end;

function htmlTime: string;
begin
    Result := htmlTime(Now);
end;

function htmlTime(_time: TDateTime): string;
begin
    case myHtmlTimeMode of
        useLocalTime:      Result := FormatDateTime('HH:nn:ss', _time);    {Need to include timezone parsing}
        useUniversalTime:  Result := FormatDateTime('HH:nn:ss', LocalTimeToUniversal(_time));
    end;
end;

function isoTime(_time: TDateTime): string;
begin
    Result:= htmlDateTime(_time);
    Result:= Result.Substring((Pos('T', Result)));
end;

function date(_isoDate: string): TDateTime;
var
    _parts: TStringArray;
    _dt: TStringArray;
    _tm: TDateTime;

    y: integer = 1900;
    m: integer = 1;
    d: integer = 1;
    h: integer = 0;
    n: integer = 0;
    s: integer = 0;
    ms: integer= 0;
begin
    _parts:= toStringArray(_isoDate, 'T');
    if length(_parts) >= 1 then
    begin
        _dt:= toStringArray(_parts[0], '-');
        if length(_parts) = 2 then
        begin
            {Get the time}
            _tm:= time(_parts[1]);
		end;

        if length(_dt) >= 3 then
        begin
            y:= number(_dt[0]);
            m:= number(_dt[1]);
            d:= number(_dt[2]);
		end;
	end;

    if TryEncodeDate(y, m, d, Result) then
        {Join the date and time together}
        Result:= ComposeDateTime(Result, _tm)
    else
        Result:= EncodeDateTime(1900, 1, 1, 0, 0, 0, 0);
end;

function time(_isoTime: string): TDateTime;
var
  h: integer = 0;
  n: integer = 0;
  s: integer = 0;
  ms: integer= 0;
  _tm: TStringArray;
begin
    _tm := toStringArray(_isoTime, ':');
    if length(_tm) >= 3 then
    begin
        h:= number(_tm[0]);
        n:= number(_tm[1]);
        s:= number(_tm[2]);
        if length(_tm) = 4 then ms := number(_tm[3]);
	end;

    if not TryEncodeTime(h, n, s, ms, Result) then
        Result:= EncodeTime(0,0,0,0);
end;


function readHtmlDateTime_not_used(_htmlDate: string): TDateTime;
begin
    TryISO8601ToDate(_htmlDate, Result, (htmlTimeMode = useUniversalTime));
end;

function readHtmlDateTime(_htmlDate: string): TDateTime;
{ I am keeping this because this I really enjoyed writing this little procedure }
type
    DecodeStage = (dcNone, dcYear, dcMonth, dcDay, dcHours, dcMinutes,
        dcSeconds, dcTimeZone);
var
    _stage: DecodeStage = dcNone;

    _endDelims: array[DecodeStage] of string = (
        {None}      '',
        {Year}      '-',
        {Month}     '-',
        {Date}      'T',
        {Hours}     ':',
        {Minutes}   ':',
        {Seconds}   'Z+-',
        {TimeZone}  '');

    Y, M, D, H, N, S, T: string;
    i, len: integer;
    c: char;
    proceed: boolean;

    function _isValid(_target: string): boolean;
    begin
        case _stage of
            dcNone:
            Result:= true;

            dcYear:
            Result := (Length(_target) < 5); {Year cannot be greater than 4}

            dcMonth, dcDay, dcHours, dcMinutes, dcSeconds:
            Result := (Length(_target) < 3);

            dcTimeZone:
            Result:= (Length(_target)<6); {+00:00}
        end;
    end;

    function _decodeChar(var _target: string; _c: char): boolean;
    begin
        Result:= true;
        if pos(_c,_endDelims[_stage]) > 0 then
            Inc(_stage)
        else
        begin
            _target := _target + _c;
            Result  := _isValid(_target);
        end;
    end;

begin
    Result := TDateTime(Math.MaxDouble); //( = NullDate. Repeating this here to avoid dependency on unit datetimectrls;)
    len := Length(_htmlDate);
    _stage := dcYear;

    for i := 1 to len do
    begin
        c := _htmlDate[i];
        case _stage of
            dcNone: ;
            dcYear:    proceed:= _decodeChar(Y, c);
            dcMonth:   proceed:= _decodeChar(M, c);
            dcDay:     proceed:= _decodeChar(D, c);
            dcHours:   proceed:= _decodeChar(H, c);
            dcMinutes: proceed:= _decodeChar(N, c);
            dcSeconds: proceed:= _decodeChar(S, c);
            dcTimeZone:proceed:= _decodeChar(T, c);
        end;

        if not proceed then break;
    end;

    if proceed then
    begin
        // writeln('Y=',Y, ' M=',M,' D=',D,' H=',H,' N=',N,' S=',S);
        if H.isEmpty then
            H := '00';
        if N.isEmpty then
            N := '00';
        if S.isEmpty then
            S := '00';
        if T.isEmpty then
           T:='+0:00';
        try
            Result:= EncodeDateTime(Y.toInteger, M.ToInteger, D.ToInteger, H.ToInteger, N.ToInteger, S.ToInteger, 0);
            if myHtmlTimeMode = useUniversalTime then
                Result := UniversalTimeToLocal(Result);
        except
            on E: Exception do Log('readHtmlDateTime():: ' + e.Message);
        end;
    end
    else
        Log('readHtmlDateTime():: Could not parse _htmlDate: ' + _htmlDate);
end;


function numToText(const _num: integer): string;
begin
    Result := '';
    case _num of
        0: Result := 'zero';
        1: Result := 'one';
        2: Result := 'two';
        3: Result := 'three';
        4: Result := 'four';
        5: Result := 'five';
        6: Result := 'six';
        7: Result := 'seven';
        8: Result := 'eight';
        9: Result := 'nine';
        10: Result := 'ten';
        11: Result := 'eleven';
        12: Result := 'twelve';
        13: Result := 'thirteen';
        14: Result := 'fourteen';
        15: Result := 'fifteen';
        16: Result := 'sixteen';
        17: Result := 'seventeen';
        18: Result := 'eighteen';
        19: Result := 'nineteen';
        20: Result := 'twenty';
    end;
end;

function isValidEmail(const _email: string): boolean;
begin
    with TRegExpr.Create('^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$') do
    begin
        Result := Exec(_email);
        Free;
    end;
end;

function isValidDate(const _date: string): boolean;
begin
    Result := True;
    try
        StrToDate(_date);
    except
        Result := False;
    end;
end;

function isValidDateTime(const _datetime: string): boolean;
begin
    Result := True;
    try
        StrToDateTime(_datetime);
    except
        Result := False;
    end;
end;

function isDecimal(const _num: string): boolean;
begin
    if _num.isEmpty then
        Result:= false
    else
    begin
        _num.Replace(DefaultFormatSettings.DecimalSeparator, '');
        Result:= isNumber(_num.Replace('.', '').Replace(DefaultFormatSettings.DecimalSeparator, ''));
	end;
end;

function asNumber(_j: TJSONData; _default: integer): integer;
begin
  if _j.JSONType = jtNumber then
  begin
      //Log('_j is a number');
      Result:= _j.asInteger
  end
  else
  begin
      if isDecimal(_j.asString) then
      begin
        //Log('_j is a string but is a number');
        Result:= strToInt(_j.asString);
	  end
	  else
      begin
          //Log('_j is is not a number so applying default');
          Result:=_default;
	  end;
  end;
end;

function hasWord(_word: string; _source: string): boolean;
begin
    with TRegExpr.Create('\b' + _word + '\b') do
    begin
        Result := Exec(_source);
        Free;
    end;
end;

function paddedNum(const _num: word; const _padCount: word = 3): string;
var
  _fmtStr: string;
begin
    Result:= AddChar('0',_num.ToString, _padCount);
end;

function limitWords(_str: string; _limit: word): string;
var
    wPos: DWord;
begin
    wPos:= Pred(WordPosition(_limit, _str,StdWordDelims));
    Result:= LeftStr(_str, wPos);
    if wPos<_str.Length then
        Result:= Result + '...';
end;

function iif(_condition: boolean; _trueString: string; _falseString: string): string;
begin
    if _condition then
        Result := _trueString
    else
        Result := _falseString;
end;

function hasDataChanged(_prev: real; _current: real;
	_tolerancePercentage: real): boolean;
var
    _diff: real;
    _tolerance: real;
    _changePercentage: real;
begin
    {Check if the percentage of change is within +/- tolerance}
    _diff:= _prev - _current;
    if isZero(_diff) then
        Result:= False
    else
    begin
        {Diff and _previous are not zero}
        _tolerance:= _prev * _tolerancePercentage;
        Result:= not InRange(_current, (_prev - _tolerance), (_prev + _tolerance));
	end;
end;

function number(_s: string): integer;
begin
    try
        Result:= strToInt(_s);
	except
        Result:= 0;
	end;
end;

function float(_s: string): extended;
begin
    try
        Result:= StrToFloat(_s);
	except
        Result:= 0;
	end;

end;

function initStr(const _val: string; _default: string): string;
begin
    if _val.isEmpty then
        Result:= _default
    else
        Result:= _val;
end;

function yyyymmdd(const _float: double): string;
begin
    Result:= yyyymmdd(FloatToDateTime(_float));
end;

function yyyymmdd(const _dt: TDatetime): string;
var
    y, m, d : word;
begin
    try
        DecodeDate(_dt, y, m, d);
        Result := format('%4d%2d%2d',[y,m,d]);
	except
        Result:= '00000000';
	end;
end;

function plus1(var i: integer): integer;
begin
    // i := i + 1;
    Inc(i);
    Result := i;
end;

function profiler(const _name: string): TCodeProfiler;
begin
    Result := TCodeProfiler.Create(_name);
end;

{From uData.pas}
function getFiles(_dir: string; _filter: string): TStrings;
var
  fInfo: TSearchRec;
  found: boolean;
begin
  Result := TStringList.Create;

  found := FindFirst(_dir + {\}DirectorySeparator + _filter, faAnyFile, fInfo) = 0;
  if found then begin
    repeat
      Result.Add(_dir + {\}DirectorySeparator + FInfo.Name);
    until FindNext(fInfo) <> 0;
  end;
  FindClose(fInfo);

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

