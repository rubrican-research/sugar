unit sugar.ddldatatypes;
{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type
    RbRowID    = longint;   // alias

    {Describes the kind for field}
    DFieldType =    (ftUndefined, ftPrimaryKey, ftForeignKey, ftData,
                    ftLockInfo, ftSystemValue, ftCalculated, ftIgnored);

    DFieldTypes = set of DFieldType;

    {Describes the kind of relationship between data}
    DModelLinkType =    (LinkNotCreated, NoLink, OneToOne, OneToMany, ManyToOne, ManyToMany);

    {Describes the catagory of the datatype - or type affinity}
    DDataTypeCategory = (dtcUndefined, dtcNumeric, dtcText, dtcDate,dtcBoolean,
                         dtcBinary, dtcPointer, dtcStruct, dtcEnum,
                         dtcListedFrom);

    DDataType = (
        dtUnknown,
        dtBit,
        dtInteger,
        dtSmallInt,
        dtBigInt,
        dtWord,
        dtDWord,
        dtQWord,
        dtReal,
        dtSingle,
        dtDouble,
        dtNumeric,
        dtMoney,
        dtChar,
        dtVarchar,
        dtString,
        dtText,
        dtWideString,
        dtDate,
        dtTime,
        dtDateTime,
        dtTimestamp,
        dtBoolean,
        dtEnum,
        dtJSON,
        dtJSONObject,
        dtJSONArray,
        dtJSMap,
        dtJSONB,
        dtBinary,
        dtSerial,
        dtBigSerial,
        dtUUID,
        dtXML,
        {~ Pascal Types ~}
        dtClass,
        dtObject,
        dtRecord,
        dtInterface,
        dtPointer,
        dtMethodPointer,
        dtCustom);

    DDataTypes = array[DDataType] of string;

function getPascalDataTypes: DDataTypes;
function getPostgresDataTypes: DDataTypes;
function getSQLiteDataTypes: DDataTypes;
function getDatasetDataTypes : DDataTypes;

function getDDLDataType(_var_type: string; arr_data_types : DDataTypes): DDataType;
function getFormatedDBValue(const _value: string; const _dataType: DDataType): string;

function isStringType(const _dataType: DDataType): boolean;
function isNumberType(const _dataType: DDataType): boolean;
function isFloatType(const _dataType: DDataType): boolean;
function isBooleanType(const _dataType: DDataType): boolean;
function isBinaryType(const _dataType: DDataType): boolean;

var
    data_types_pascal: DDataTypes;
    data_types_postgres: DDataTypes;
    data_types_sqlite: DDataTypes;
    dataset_data_types : DDataTypes;

    non_primitive_types : set of DDataType = [dtClass, dtObject];

implementation

function getPascalDataTypes: DDataTypes;
var
    datatypes: DDataTypes;
begin
    datatypes[dtBit]        := 'Byte';
    datatypes[dtInteger]    := 'Integer';
    datatypes[dtSmallInt]   := 'Smallint';
    datatypes[dtBigInt]     := 'Int64';
    datatypes[dtWord]       := 'Word';
    datatypes[dtDWord]      := 'DWord';
    datatypes[dtQWord]      := 'QWord';
    datatypes[dtReal]       := 'Real';
    datatypes[dtSingle]     := 'Single';
    datatypes[dtDouble]     := 'Double';
    datatypes[dtNumeric]    := 'Currency';
    datatypes[dtMoney]      := 'Currency';
    datatypes[dtChar]       := 'array [0..%d] of char';
    datatypes[dtVarchar]    := 'string';
    datatypes[dtString]     := 'string';
    datatypes[dtText]       := 'widestring';
    datatypes[dtWideString] := 'widestring';
    datatypes[dtDate]       := 'TDateTime';
    datatypes[dtTime]       := 'TDateTime';
    datatypes[dtDateTime]   := 'TDateTime';
    datatypes[dtTimestamp]  := 'TDateTime';
    datatypes[dtBoolean]    := 'boolean';
    datatypes[dtEnum]       := '{create a type}(%s)';
    datatypes[dtJSON]       := '{use fpjson} TJSONData';
    datatypes[dtJSONObject] := '{use fpjson} TJSONObject';
    datatypes[dtJSONArray]  := '{use fpjson} TJSONArray';
    datatypes[dtJSMap]      := '{use fpjson} TJSONObject';
    datatypes[dtJSONB]      := '{use fpjson}TJSONData ';
    datatypes[dtBinary]     := 'pointer {to binary data}';
    datatypes[dtSerial]     := 'Int64';
    datatypes[dtBigSerial]  := datatypes[dtSerial];
    datatypes[dtUUID]       := 'string';
    datatypes[dtXML]        := 'string {not implemented}';
    dataTypes[dtClass]      := '';
    dataTypes[dtObject]     := '';
    dataTypes[dtRecord]     := '';
    dataTypes[dtEnum]       := '';
    dataTypes[dtInterface]  := '';
    dataTypes[dtPointer]    := '';
    dataTypes[dtMethodPointer] := '';
    dataTypes[dtCustom]     := '';
    Result := datatypes;
end;

function getPostgresDataTypes: DDataTypes;
begin
    // Specific to Postgres SQL

    Result[dtBit]           := 'BIT';
    Result[dtInteger]       := 'INTEGER';
    Result[dtSmallInt]      := 'SMALLINT';
    Result[dtBigInt]        := 'BIGINT';
    Result[dtWord]          := 'SMALLINT';
    Result[dtDWord]         := 'INTEGER';
    Result[dtQWord]         := 'BIGINT';
    Result[dtReal]          := 'REAL';
    Result[dtDouble]        := 'DOUBLE';
    Result[dtNumeric]       := 'NUMERIC(%d,%d)';
    Result[dtMoney]         := 'MONEY';
    Result[dtChar]          := 'CHAR(%d)';
    Result[dtString]        := 'CHAR(%d)';
    Result[dtVarchar]       := 'VARCHAR(%d)';
    Result[dtVarchar]       := 'VARCHAR(%d)';
    Result[dtText]          := 'TEXT';
    Result[dtWideString]    := 'TEXT';

    //Result[dtDate]          := 'DATE';
    //Result[dtTime]          := 'TIME';
    //Result[dtDateTime]      := 'TIMESTAMP WITH TIME ZONE';
    //Result[dtTimestamp]     := 'TIMESTAMP WITH TIME ZONE';

    {Although PostGres supports native DateTime, this makes it compatible with SQLite}
    Result[dtDate]          := 'CHAR(10)'; {YYYY-MM-DD}
    Result[dtTime]          := 'CHAR(12)'; {HH:MM:SS.sss}
    Result[dtDateTime]      := 'CHAR(32)'; {YYYY-MM-DDTHH:MM:SS.sssZ+00:00}
    Result[dtTimestamp]     := 'CHAR(32)'; {YYYY-MM-DDTHH:MM:SS.sssZ+00:00}

    Result[dtBoolean]       := 'BOOLEAN';
    Result[dtEnum]          := 'ENUM (%s)';
    Result[dtJSON]          := 'JSONB';
    Result[dtJSONObject]    := 'JSONB';
    Result[dtJSONArray]     := 'JSONB';
    Result[dtJSMap]         := 'JSONB';
    Result[dtJSONB]         := 'JSONB';
    Result[dtBinary]        := 'BLOB';
    Result[dtSerial]        := 'SERIAL';
    Result[dtBigSerial]     := 'BIGSERIAL';
    Result[dtUUID]          := 'UUID';
    Result[dtXML]           := 'XML';

    Result[dtClass]         := Result[dtText];
    Result[dtObject]        := Result[dtText];
    Result[dtRecord]        := Result[dtText];
    Result[dtEnum]          := Result[dtText];
    Result[dtInterface]     := Result[dtText];
    Result[dtPointer]       := Result[dtBigInt];
    Result[dtMethodPointer] := Result[dtBigInt];
    Result[dtCustom]        := Result[dtText];;

end;

function getSQLiteDataTypes: DDataTypes;
begin
    Result := getPostgresDataTypes;
    Result[dtSerial]    := 'INTEGER PRIMARY KEY AUTOINCREMENT';
    Result[dtBigSerial] := 'INTEGER PRIMARY KEY AUTOINCREMENT';

    Result[dtDate]          := 'CHAR(10)'; {YYYY-MM-DD}
    Result[dtTime]          := 'CHAR(12)'; {HH:MM:SS.sss}
    Result[dtDateTime]      := 'CHAR(32)'; {YYYY-MM-DDTHH:MM:SS.sssZ+00:00}
    Result[dtTimestamp]     := 'CHAR(32)'; {YYYY-MM-DDTHH:MM:SS.sssZ+00:00}


    Result[dtJSON]      := 'JSON';
    Result[dtJSONObject]:= 'JSON';
    Result[dtJSONArray] := 'JSON';
    Result[dtJSMap]     := 'JSON';
    Result[dtJSONB]     := 'JSON';

end;


function getDatasetDataTypes: DDataTypes;
begin
{~
AsBCD
AsBoolean
AsBytes
AsCurrency
AsDateTime
AsFloat
AsLongint
AsLargeInt
AsInteger
AsString
AsWideString
AsVariant

AsBCD
AsBlob
AsBoolean
AsBytes
AsCurrency
AsDate
AsDateTime
AsFloat
AsInteger
AsLargeInt
AsMemo
AsSmallInt
AsString
AsTime
AsWord
AsFMTBCD
AsWideString
~}

	  Result[dtUnknown]     := 'Unknown';
	  Result[dtBit]         := 'AsInteger';
	  Result[dtInteger]     := 'AsInteger';
	  Result[dtSmallInt]    := 'AsInteger';
	  Result[dtBigInt]      := 'AsLargeInt';
	  Result[dtWord]        := 'AsLargeInt';
	  Result[dtDWord]       := 'AsLargeInt';
	  Result[dtQWord]       := 'AsLargeInt';
	  Result[dtReal]        := 'AsFloat';
	  Result[dtSingle]      := 'AsFloat';
	  Result[dtDouble]      := 'AsFloat';
	  Result[dtNumeric]     := 'AsCurrency';
	  Result[dtMoney]       := 'AsCurrency';
	  Result[dtChar]        := 'AsString';
	  Result[dtVarchar]     := 'AsString';
   	  Result[dtString]      := 'AsString';
	  Result[dtText]        := 'AsString';
      Result[dtWideString]  := 'AsWideString';

      {SToring date as HTML TIME}
	  Result[dtDate]        := 'AsString';
	  Result[dtTime]        := 'AsString';
	  Result[dtDateTime]    := 'AsString';
	  Result[dtTimestamp]   := 'AsString';

      Result[dtBoolean]     := 'AsBoolean';
	  Result[dtEnum]        := 'AsBytes';
	  Result[dtJSON]        := 'AsString';
      Result[dtJSONObject]  := 'AsString';
      Result[dtJSONArray]   := 'AsString';
      Result[dtJSMap]       := 'AsString';
      Result[dtJSON]        := 'AsString';
	  Result[dtJSONB]       := 'AsBCD';
	  Result[dtBinary]      := 'AsBlob';
	  Result[dtSerial]      := 'AsInteger';
	  Result[dtBigSerial]   := 'AsLargeInt';
	  Result[dtUUID]        := 'AsString';
	  Result[dtXML]         := 'asString';

    Result[dtClass]         := '';
    Result[dtObject]        := '';
    Result[dtRecord]        := '';
    Result[dtEnum]          := '';
    Result[dtInterface]     := '';
    Result[dtPointer]       := '';
    Result[dtMethodPointer] := '';
    Result[dtCustom]        := '';


end;

function getDDLDataType(_var_type: string; arr_data_types : DDataTypes): DDataType;
var
    i, count : integer;
    found : boolean;
begin
    Result:= dtCustom;
    count := ord(high(arr_data_types));
    found := false;
    for i:= 0 to pred(count) do
    begin
      found := CompareText(arr_data_types[DDataType(i)],_var_type) = 0;
      if found then break;
	end;
    if found then
        Result := DDataType(i);
end;

function getFormatedDBValue(const _value: string; const _dataType: DDataType
	  ): string;
begin

end;

function isStringType(const _dataType: DDataType): boolean;
begin
    Result := false;
end;

function isNumberType(const _dataType: DDataType): boolean;
begin
    Result := false;
end;

function isFloatType(const _dataType: DDataType): boolean;
begin
    Result := false;
end;

function isBooleanType(const _dataType: DDataType): boolean;
begin
    Result := false;
end;

function isBinaryType(const _dataType: DDataType): boolean;
begin
    Result := false;
end;

initialization
    data_types_pascal   := getPascalDataTypes;
    data_types_postgres := getPostgresDataTypes;
    data_types_sqlite   := getSQLiteDataTypes;
    dataset_data_types  := getDatasetDataTypes;
end.

