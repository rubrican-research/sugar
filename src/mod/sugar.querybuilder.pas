unit sugar.querybuilder;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fgl, sugar.collections, sugar.ddldatatypes;

const
    ROWLIMIT = 20;

type

    {Query Builder}
    QBJoinType = (jLeftJoin, jRightJoin, jInnerJoin, jOuterJoin, jFullJoin, jCrossJoin);

    { RbQueryBuilderBase }
    RbQueryBuilderBase = class
	private
		  procedure clearColumns;
    protected
        myTables:  TStringList;
        myIndex:   TStringIndexMap;
        myColumns: TStringList;

        myWhere: string;

        myShouldFreeTables: boolean;
        myShouldFreeColumns: boolean;

        procedure reindex;
        function formatColumnValues(_formatStr: string='%s=%s'): string;
    public
        constructor Create;
        destructor Destroy; override;

        function tableCount: integer;
        function table(const _index: integer = 0): string;
        function tables(const _tables: TStringList): RbQueryBuilderBase; overload;
        {auto create a stringlist}
        function tables(const _tables: string): RbQueryBuilderBase; overload;
        function tables(const _tables: array of string): RbQueryBuilderBase; overload;

        function columns: TStringList; overload;
        function columnCount: integer;

        function columns(const _list: TStringList): RbQueryBuilderBase; overload;
        {auto create a stringlist}
        function columns(const _list: string):RbQueryBuilderBase; overload;
        function columns(const _list: array of string):RbQueryBuilderBase; overload;

        function sql: string; virtual;

        {Important: Call where() to clear the where clause}
        function where(const _where: string = ''): RbQueryBuilderBase;
        function whereID(const _id: RbRowID): RbQueryBuilderBase; virtual;
        procedure Clear; virtual;

        {If you want to free the tables list and columns list manually}
        procedure FreeLists;
        function isReady: boolean; virtual;
    end;


    { RbSelectQueryBuilder }
    RbTableJoinDef = class
        jType: QBJoinType;
        jTable: string;
        jOn: string;
    end;

    RbTableJoinDefsBase = specialize TFPGObjectList<RbTableJoinDef>;

    { RbTableJoinDefs }

    RbTableJoinDefs = class(RbTableJoinDefsBase)

    end;

    RbSelectQueryBuilder = class(RbQueryBuilderBase)
    private
        myJoins: RbTableJoinDefs;
        myFrom: TStringArray;
        myGroups: string;
        myOrder: string;
        myLimit: integer;
        myLimitOffset: integer;
        mySelectAll: boolean;
        myRowCount: integer;
        myIsDistinct: boolean;
        function getColumns: string;
        function getTables: string;
        function getWhere: string;
        function getJoins: string;
        function getGroup: string;
        function getOrder: string;
        function getLimit: string;
    public
        constructor Create;
        destructor Destroy; override;

        function select(const _columns: TStringList): RbSelectQueryBuilder;
        function selectAll: RbSelectQueryBuilder;
        function distinct(const _isDistinct: boolean = true): RbSelectQueryBuilder;

        function from(const _tables: TStringList): RbSelectQueryBuilder;
        function where(const _where: string = ''): RbSelectQueryBuilder; reintroduce;
        function whereID(const _id: RbRowID): RbSelectQueryBuilder; reintroduce;


        function join(const _jointype: QBJoinType; const _joinTable: string;
            const _on: string): RbSelectQueryBuilder;

        function group(const _group: string): RbSelectQueryBuilder;
        function order(const _order: string): RbSelectQueryBuilder;

        function limit(const _limit: integer = ROWLIMIT; const _offset: integer = 0): RbSelectQueryBuilder;
        function nolimit: RbSelectQueryBuilder;


        function sql: string; override;
        function sqlNextPage: string;
        function sqlPage(_page: integer): string;
        function sqlRowCount: string;

        procedure Clear; override; overload;
        procedure Clear(const _what: string); overload;
        function calcTotalPages(const _rowCount: integer = -1): integer;
        function currentPage: integer;

        function isReady: boolean; override;

    end;

    { RbInsertQueryBuilder }
    RbInsertQueryBuilder = class(RbQueryBuilderBase)
    protected
        myRecordCount: integer;
        myOnConflict: string;
        myDoNothing: boolean;
        myDoThis: string;
    public
        constructor Create;
        destructor Destroy; override;
        function insert(const _table: TStringList; const _columns: TStringList; const _values: TStringList = nil): RbInsertQueryBuilder;

        function sql: string; override;

        {## UPSERT FUNCTIONALITY ##}
        function hasUpsert: boolean; {says if upsert is fully defined}
        function onConflict(const _conflict: string): RbInsertQueryBuilder;
        procedure doNothing;

        {specify the entire command to do}
        procedure doThis(const _dothis: string);

        {specify the key values for updating}
        {updates the columns of the record with the new values}

        procedure doUpdate; overload; {update all the fields that have been in the insert}

        {these add the column, values to the update clause}
        procedure doUpdate (const _columns: array of string; const _values: array of string); overload;
        function  doUpdate (const _columns: string; const _values: string; const _delim: string = ','): RbInsertQueryBuilder; overload;

        {directly set the update clause. No parameters}
        function  doUpdate (const _keyvalues: string): RbInsertQueryBuilder; overload;

        function where(const _where: string=''): RbInsertQueryBuilder; reintroduce;
        function whereID(const _id: RbRowID): RbQueryBuilderBase; reintroduce;

        procedure Clear; override;
        function isReady: boolean; override;
    end;

    { RbUpdateQueryBuilder }

    RbUpdateQueryBuilder = class(RbQueryBuilderBase)
    public
        function where(const _where: string = ''): RbUpdateQueryBuilder; reintroduce;
        function whereID(const _id: RbRowID): RbUpdateQueryBuilder; reintroduce;
        function sql: string; override;
        function isReady: boolean; override;
    end;

		{ RbDeleteQueryBuilder }

    RbDeleteQueryBuilder = class(RbQueryBuilderBase)
	    function sql: string; override;
		function where(const _where: string): RbDeleteQueryBuilder; reintroduce;
        function whereID(const _id: RbRowID): RbDeleteQueryBuilder; reintroduce;
        function isReady: boolean; override;
  end;

    { RbCreateTableBuilder }

    RbCreateTableBuilder = class
    public
        function createTable(_table: string; _dropBeforeCreate: boolean
						): RbCreateTableBuilder;
        function sql: string;
    end;

    RbQueryBuilder = class
    private
        function getMyCreator: RbCreateTableBuilder;
        function getMyReader:  RbSelectQueryBuilder;
        function getMyUpdater: RbUpdateQueryBuilder;
        function getDeleter:   RbDeleteQueryBuilder;
        function getMyInserter:RbInsertQueryBuilder;
    protected
        myCreator: RbCreateTableBuilder;
        myReader: RbSelectQueryBuilder;
        myInserter: RbInsertQueryBuilder;
        myUpdater: RbUpdateQueryBuilder;
        myDeleter: RbDeleteQueryBuilder;

    public
        property creator: RbCreateTableBuilder read getMyCreator;
        property reader: RbSelectQueryBuilder read getMyReader;
        property inserter: RbInsertQueryBuilder read getMyInserter;
        property updater: RbUpdateQueryBuilder read getMyUpdater;
        property deleter: RbDeleteQueryBuilder read getDeleter;
        procedure Clear;
        destructor Destroy; override;
    end;

function quoted(const _value: string) : string; deprecated 'Use QuotedStr() instead';
function stripQuotes(const _value: string): string;
function sanitize(const _value: string): string;

var
    QUOTE_CHAR : string = '''';

implementation

uses
    sugar.ddlmodel, sugar.utils, sugar.logger;

const
    qSELECT = 'SELECT';
    qSELECT_DISTINCT = qSELECT + ' DISTINCT';
    qFROM = 'FROM';
    qINNER = 'INNER';
    qOUTER = 'OUTER';
    qCROSS = 'CROSS';
    qFULL = 'FULL';
    qLEFT = 'LEFT';
    qRIGHT = 'RIGHT';
    qJOIN = 'JOIN';
    qWHERE = 'WHERE';
    qGROUP = 'GROUP BY';
    qORDER = 'ORDER BY';
    qASCENDNG = 'ASC';
    qDESCENDING = 'DESC';
    qLIMIT = 'LIMIT';
    qDELETE = 'DELETE';
    qUPDATE = 'UPDATE';
    qCREATE = 'CREATE';
    qTABLE = 'TABLE';

function quoted(const _value: string): string;
begin
    //Result := QUOTE_CHAR + _value + QUOTE_CHAR;
    Result := QuotedStr(_value);
end;

function stripQuotes(const _value: string): string;
begin
    Result := _value.Replace(QUOTE_CHAR, '');
end;

function sanitize(const _value: string): string;
begin
    Result:= _value; {TODO Later}
end;


{ RbInsertQueryBuilder }

constructor RbInsertQueryBuilder.Create;
begin
    myRecordCount := 0;
    myDoNothing:=False;
end;

destructor RbInsertQueryBuilder.Destroy;
begin

    inherited Destroy;
end;

function RbInsertQueryBuilder.insert(const _table: TStringList;
	const _columns: TStringList; const _values: TStringList): RbInsertQueryBuilder;
begin
    tables(_table);
    columns(_columns);
    Result := self;
end;


function RbInsertQueryBuilder.sql: string;
var
    _columns, _values: string;
    i: integer;
    bAddComma: boolean = False;
begin
    Result := '';
    if table <> '' then
    begin
        _columns := '';
        _values  := '';
        for i := 0 to pred(columnCount) do
        begin
            if bAddComma then
            begin
       	        _columns:= _columns + COMMA;
                _values := _values  + COMMA;
            end
        	else
        	    bAddComma := True;

              _columns:= _columns + columns[i];
        	  _values := _values + ':' + columns[i];
        end;

        _values := '(' + _values + ')';

		Result := Format('INSERT INTO %s (%s) ' + sLinebreak +
            'VALUES ' + sLinebreak + '%s',
            [table, _columns, _values]);

        {## UPSERT FUNCTIONALITY ## }
        {Only if there are no records or there is only one record}
        if hasUpsert and (myRecordCount in [0, 1]) then
        begin
            Result+= ' ON CONFLICT (' + myOnConflict + ') ' + myDoThis;
		end;
    end;
end;

function RbInsertQueryBuilder.hasUpsert: boolean;
begin
    Result := not myOnConflict.IsEmpty;

    {NO UPSERT because OnConflict is empty}
    if not Result  then exit;

    {On Conflict is defined and there is something to be done}
    Result := Result and (not myDoThis.IsEmpty);
end;

function RbInsertQueryBuilder.onConflict(const _conflict: string
	  ): RbInsertQueryBuilder;
begin
    Result := Self;
    if myOnConflict = _conflict then exit;
    myOnConflict:=_conflict;
end;

procedure RbInsertQueryBuilder.doNothing;
begin
    myDoNothing:=True;
    myDoThis:= 'DO NOTHING';
end;

procedure RbInsertQueryBuilder.doThis(const _dothis: string);
begin
    if myDoNothing then
        myDoNothing:= false;

    myDoThis := 'DO';
    myDoThis += ' ' + _dothis;
end;

procedure RbInsertQueryBuilder.doUpdate;
var
    _columns: string = '';
    _params:  string = '';
    _column: string;
    bAddComma: boolean = false;
begin
    for _column in columns do
    begin
        if (_column = IDField) or (pos(_column, myOnConflict) <> 0) then
          {do not add update columns if they are
            - ID Field
            - member of the On Conflict Fields}
          continue
        else
        begin
            if bAddComma then
                _columns += COMMA
            else
                bAddComma:= true;

            _columns += _column;
		end;
	end;
    doUpdate(_columns, '');
end;

procedure RbInsertQueryBuilder.doUpdate(const _columns: array of string;
	  const _values: array of string);
var
    i: integer = 0;
    _col: string;
    _addComma : boolean = false;
    _valuesLen: integer;
begin

    if Length(_columns) = 0 then
    begin
        MyDoNothing:= true;
        exit;
	end;

	if myDoNothing then
        myDoNothing:= false;

    myDoThis := 'DO UPDATE SET ';

    _valuesLen:= Length(_values);
    for _col in _columns do
    begin
        if _addComma then
            myDoThis += COMMA
        else
            _addComma := True;

        {If a value is available use that}
        if i < _valuesLen then
            myDoThis += _col + ' = ' + QuotedStr(_values[i])
        else {Create a parameter with the same name}
            myDoThis += _col + ' = :' +_col;
        inc(i);
	end;

end;

function RbInsertQueryBuilder.doUpdate(const _columns: string;
	  const _values: string; const _delim: string): RbInsertQueryBuilder;
begin
    doUpdate(toStringArray(_columns,_delim),
             toStringArray(_values, _delim));
    Result := self;
end;

function RbInsertQueryBuilder.doUpdate(const _keyvalues: string
	): RbInsertQueryBuilder;
begin
    Result:= self;
    doThis('UPDATE SET ' + _keyvalues);
end;



function RbInsertQueryBuilder.where(const _where: string): RbInsertQueryBuilder;
begin
    inherited where(_where);
    Result := self;
end;

function RbInsertQueryBuilder.whereID(const _id: RbRowID): RbQueryBuilderBase;
begin
    inherited whereID(_id);
    Result := self;
end;

procedure RbInsertQueryBuilder.Clear;
begin
    myRecordCount := 0;
    inherited Clear;
end;

function RbInsertQueryBuilder.isReady: boolean;
begin
    Result:=inherited isReady and (columnCount>0);
end;

{ RbQueryBuilder }

function RbQueryBuilder.getDeleter: RbDeleteQueryBuilder;
begin
    if not Assigned(myDeleter) then
        myDeleter := RbDeleteQueryBuilder.Create;
    Result := myDeleter;
end;

function RbQueryBuilder.getMyInserter: RbInsertQueryBuilder;
begin
    if not Assigned(myInserter) then
        myInserter := RbInsertQueryBuilder.Create;
    Result := myInserter;
end;

procedure RbQueryBuilder.Clear;
begin
    inserter.Clear;
    reader.Clear;
    updater.Clear;
    deleter.Clear;
end;

destructor RbQueryBuilder.Destroy;
begin
    myCreator.Free;
    myReader.Free;
    myInserter.Free;
    myUpdater.Free;
    myDeleter.Free;
	inherited Destroy;
end;

function RbQueryBuilder.getMyCreator: RbCreateTableBuilder;
begin
    trip('RbUpdateQueryBuilder not yet implemented');
    if not Assigned(myCreator) then
        myCreator := RbCreateTableBuilder.Create;
    Result := myCreator;
end;

function RbQueryBuilder.getMyReader: RbSelectQueryBuilder;
begin
    if not Assigned(myReader) then
        myReader := RbSelectQueryBuilder.Create;
    Result := myReader;
end;

function RbQueryBuilder.getMyUpdater: RbUpdateQueryBuilder;
begin
    if not Assigned(myUpdater) then
        myUpdater := RbUpdateQueryBuilder.Create;
    Result := myUpdater;
end;

{ RbCreateTableBuilder }

function RbCreateTableBuilder.sql: string;
begin
    Result := '';
end;

function RbCreateTableBuilder.createTable(_table: string;
    _dropBeforeCreate: boolean): RbCreateTableBuilder;
begin
    Result := self;
end;

{ RbDeleteQueryBuilder }

function RbDeleteQueryBuilder.sql: string;
begin
    Result := '';
    if table <> '' then
    begin
        Result := qDELETE + ' ' + qFROM + ' ' + table;
        if myWhere <> '' then
            Result := Result + ' ' + qWHERE + ' ' + myWhere;
    end;
end;

function RbDeleteQueryBuilder.where(const _where: string): RbDeleteQueryBuilder;
begin
    inherited where(_where);
    Result := self;
end;

function RbDeleteQueryBuilder.whereID(const _id: RbRowID): RbDeleteQueryBuilder;
begin
    inherited whereID(_id);
    Result := self;
end;

function RbDeleteQueryBuilder.isReady: boolean;
begin
    Result:=inherited isReady and (not myWhere.isEmpty);
end;

{ RbUpdateQueryBuilder }

function RbUpdateQueryBuilder.sql: string;
const
    Q = 'UPDATE %s  SET %s';
var
    _setValues, _where: string;
begin
    Result := '';
    if table <> '' then
    begin
        _setValues := formatColumnValues;
        Result := Format(Q, [table, _setValues]);
        if myWhere <> '' then
            Result := Result + ' ' + qWHERE + ' ' + myWhere;
    end;
end;

function RbUpdateQueryBuilder.isReady: boolean;
begin
    Result:=inherited isReady and (columnCount>0);
end;


function RbUpdateQueryBuilder.where(const _where: string): RbUpdateQueryBuilder;
begin
    inherited where(_where);
    Result := self;
end;

function RbUpdateQueryBuilder.whereID(const _id: RbRowID): RbUpdateQueryBuilder;
begin
    inherited whereID(_id);
    Result := self;
end;


{ RbSelectQueryBuilder }

function RbSelectQueryBuilder.from(const _tables: TStringList):
RbSelectQueryBuilder;
begin
    tables(_tables);
    Result := Self;
end;

function RbSelectQueryBuilder.join(const _jointype: QBJoinType;
    const _joinTable: string; const _on: string): RbSelectQueryBuilder;
var
    _joinDef: RbTableJoinDef;
begin
    _joinDef := RbTableJoinDef.Create;
    _joinDef.jType := _jointype;
    _joinDef.jTable := _joinTable;
    _joinDef.jOn := _on;
    myJoins.Add(_joinDef);
    Result := self;
end;

function RbSelectQueryBuilder.group(const _group: string): RbSelectQueryBuilder;
begin
    myGroups := myGroups + ' ' + _group;
    Result := self;
end;

function RbSelectQueryBuilder.where(const _where: string): RbSelectQueryBuilder;
begin
    inherited where(_where);
    Result := self;
end;

function RbSelectQueryBuilder.whereID(const _id: RbRowID): RbSelectQueryBuilder;
begin
    inherited whereID(_id);
    Result := self;
end;

function RbSelectQueryBuilder.getColumns: string;
begin
    if myIsDistinct then
        Result := qSELECT_DISTINCT
    else
        Result := qSELECT;

    Result += ' ';
    if mySelectAll then
        Result += '*'
    else
        Result += myColumns.CommaText;
end;

function RbSelectQueryBuilder.getTables: string;
begin
    Result := myTables.CommaText;
    if Result <> '' then
        Result := qFROM + ' ' + Result;
end;

function RbSelectQueryBuilder.getWhere: string;
begin
    Result := '';
    if myWhere <> '' then
        Result := qWHERE + ' ' + myWhere;
end;

function RbSelectQueryBuilder.getJoins: string;
const
    fJOIN = '%s %s %s';
var
    i: integer;
    _jCount: integer;
    _jdef: RbTableJoinDef;
begin
    Result := '';
    _jCount := myJoins.Count;
    for i := 0 to _jCount - 1 do
    begin
        _jdef := myJoins.Items[i];
        case _jdef.jType of
            jLeftJoin: Result := Format(fJOIN, [Result, qLEFT, qJOIN]);
            jRightJoin: Result := Format(fJOIN, [Result, qRIGHT, qJOIN]);
            jInnerJoin: Result := Format(fJOIN, [Result, qINNER, qJOIN]);
            jOuterJoin: Result := Format(fJOIN, [Result, qOUTER, qJOIN]);
            jFullJoin: Result := Format(fJOIN, [Result, qFULL, qJOIN]);
            jCrossJoin: Result := Format(fJOIN, [Result, qCROSS, qJOIN]);
        end;
        Result := Format('%s %s ON %s', [Result, _jdef.jTable, _jdef.jOn]);
        if i < _jCount then
            Result := Result + ' ';
    end;
end;

function RbSelectQueryBuilder.getGroup: string;
begin
    Result := '';
    if myGroups <> '' then
        Result := qGROUP + ' ' + myGroups;
end;

function RbSelectQueryBuilder.getOrder: string;
begin
    Result := '';
    if myOrder <> '' then
        Result := qORDER + ' ' + myOrder;
end;

function RbSelectQueryBuilder.getLimit: string;
begin
    Result := '';
    {LIMIT}
    if myLimit > 0 then
        Result := qLIMIT + ' ' + myLimit.toString;
    {LIMIT OFFSET}
    if (Result <> '') and (myLimitOffset > 0) then
        Result := Result + ' OFFSET ' + myLimitOffset.toString;
end;


function RbSelectQueryBuilder.sql: string;
var
    _select, _from, _join, _where, _group, _order, _limit: string;
begin
    _select := getColumns;
    _from := getTables;
    _join := getJoins;
    _where := getWhere;
    _group := getGroup;
    _order := getOrder;
    _limit := getLimit;

    Result := _select + ' ' + _from + ' ' + _join + ' ' + _where +
        ' ' + _group + ' ' + _order + ' ' + _limit;
end;

function RbSelectQueryBuilder.sqlRowCount: string;
var
    _select, _from, _join, _where, _group, _order, _limit: string;
begin
    _select := qSELECT + ' count(1) as rowcount ';
    _from := getTables;
    _join := getJoins;
    _where := getWhere;
    _group := getGroup;
    _order := getOrder;
    {Limit will not give rowcount}
    // _limit := getLimit;

    Result := _select + ' ' + _from + ' ' + _join + ' ' + _where +
        ' ' + _group + ' ' + _order;
end;

function RbSelectQueryBuilder.sqlNextPage: string;
begin
    Limit(myLimit, myLimitOffset + myLimit);
    Result := sql;
end;

function RbSelectQueryBuilder.order(const _order: string): RbSelectQueryBuilder;
begin
    if not myOrder.IsEmpty then
        myOrder := myOrder + ', ' + _order
    else
        myOrder:= _order;
    Result := self;
end;


procedure RbSelectQueryBuilder.Clear;
begin
    myJoins.Free;
    myJoins := RbTableJoinDefs.Create;
    SetLength(myFrom, 0);
    myWhere := '';
    myGroups := '';
    myOrder := '';
    myRowCount := -1;
    {To set the limit, you have to call limit() again}
    myLimit := 0;
    myLimitOffset := 0;
    mySelectAll := False;
    inherited Clear;
end;

procedure RbSelectQueryBuilder.Clear(const _what: string);
begin
    case UpperCase(_what) of
        qJOIN:
        begin
            myJoins.Free;
            myJoins := RbTableJoinDefs.Create;
        end;
        qWHERE: myWhere := '';
        qGROUP: myGroups := '';
        qORDER: myOrder := '';
        qLIMIT:
        begin
            myLimit := 0;
            myLimitOffset := 0;
        end;
    end;
end;

{Return SQL to load records that should come on a given page}
function RbSelectQueryBuilder.sqlPage(_page: integer): string;
var
    _maxPages: integer;
begin
    if _page <= 0 then _page := 0;

    _maxPages := calcTotalPages(myRowCount);

    if _page > _maxPages then
        _page := _maxPages;

    limit(myLimit, ((_page - 1) * myLimit));

    Result := sql;
end;

function RbSelectQueryBuilder.calcTotalPages(const _rowCount: integer): integer;
begin
    if _rowCount > -1 then
        {assign myRowCount}
        if myRowCount <> _rowCount then
            myRowCount := _rowCount;

    if myLimit > 0 then
    begin
        Result := myRowCount div myLimit;
        if (myRowCount mod myLimit) > 0 then
            Inc(Result);
    end
    else
        Result := 1;
end;

function RbSelectQueryBuilder.currentPage: integer;
begin
    if myLimit = 0 then
        Result := 1
    else
        Result := (myLimitOffset div myLimit) + 1;
end;

function RbSelectQueryBuilder.isReady: boolean;
begin
    Result:=inherited isReady and ((columnCount > 0) or mySelectAll);

end;


function RbSelectQueryBuilder.selectAll: RbSelectQueryBuilder;
begin
    mySelectAll := True;
    Result := self;
end;

function RbSelectQueryBuilder.distinct(const _isDistinct: boolean
	): RbSelectQueryBuilder;
begin
    myIsDistinct:= _isDistinct;
    Result:= self;
end;

function RbSelectQueryBuilder.limit(const _limit: integer;
    const _offset: integer): RbSelectQueryBuilder;
begin
    myLimit       := _limit;
    myLimitOffset := _offset;
    Result := self;
end;

function RbSelectQueryBuilder.nolimit: RbSelectQueryBuilder;
begin
    myLimit := 0;
    myLimitOffset:=0;
    Result := self;
end;

constructor RbSelectQueryBuilder.Create;
begin
    inherited;
    myJoins:= RbTableJoinDefs.Create;
end;

destructor RbSelectQueryBuilder.Destroy;
begin
    myJoins.Free;
    inherited Destroy;
end;

function RbSelectQueryBuilder.select(const _columns: TStringList
		): RbSelectQueryBuilder;
begin
    columns(_columns);
    mySelectAll := False;
    Result := self;
end;


{ RbQueryBuilderBase }

procedure RbQueryBuilderBase.Clear;
begin
    myTables.Clear;
    myColumns.Clear;
end;

procedure RbQueryBuilderBase.FreeLists;
begin
     myColumns.Free;
     myTables.Free;
end;

function RbQueryBuilderBase.isReady: boolean;
begin
    Result:= tableCount>0;
end;

procedure RbQueryBuilderBase.clearColumns;
begin
    if myShouldFreeColumns then
        FreeAndNil(myColumns);
end;


function RbQueryBuilderBase.columns(const _list: TStringList): RbQueryBuilderBase;
var
    i: integer;
    s: string;
begin
     clearColumns;
     myShouldFreeColumns:= false;
     myColumns:= _list;
     reindex;
     Result := self;
end;

function RbQueryBuilderBase.columns(const _list: string): RbQueryBuilderBase;
begin
     clearColumns;
     myShouldFreeColumns:= True;

     myColumns  := TStringList.Create;
     myColumns.CommaText := _list;

     reindex;
     Result:= Self;
end;

function RbQueryBuilderBase.columns(const _list: array of string
		): RbQueryBuilderBase;
var
    _col: string;
begin
    clearColumns;
    myShouldFreeColumns:=True;

    myColumns := TStringList.Create;

    for _col in _list do
        myColumns.Add(_col);

    reindex;
    Result := Self;
end;


function RbQueryBuilderBase.sql: string;
begin
    Result := 'NOSQL';
end;


function RbQueryBuilderBase.where(const _where: string): RbQueryBuilderBase;
begin
    if _where = '' then
        myWhere := ''
    else
        myWhere := myWhere + ' ' + sanitize(_where);
    Result := self;
end;

function RbQueryBuilderBase.whereID(const _id: RbRowID): RbQueryBuilderBase;
begin
    if not myWhere.isEmpty then
        where('AND');

    where(Format('%s=%d',[IDField, _id]));
    Result := self;
end;

function RbQueryBuilderBase.columns: TStringList;
begin
    Result := myColumns;
end;

function RbQueryBuilderBase.columnCount: integer;
begin
    Result := 0;
    if Assigned(myColumns) then
        Result := myColumns.Count;
end;


function RbQueryBuilderBase.table(const _index: integer): string;
begin
    Result := '';
    if Assigned(myTables) then
        if _index < myTables.Count then
            Result := myTables[_index];
end;


function RbQueryBuilderBase.tables(const _tables: TStringList
		): RbQueryBuilderBase;
begin
    myTables := _tables;
    Result := self;
end;

function RbQueryBuilderBase.tables(const _tables: string): RbQueryBuilderBase;
begin
     myShouldFreeTables:=True;
     myTables := TStringList.Create;
     myTables.CommaText:=_tables;
     Result := Self;
end;

function RbQueryBuilderBase.tables(const _tables: array of string
		): RbQueryBuilderBase;
var
    _tbl: string;
begin
    myShouldFreeTables:=True;
    myTables:= TStringList.Create;
    for _tbl in _tables do
        myTables.Add(_tbl);
    Result:= Self;
end;


procedure RbQueryBuilderBase.reindex;
var
    _i: integer;
begin
   {I have to do this because
    myIndex.Clear isn't freeing memory completely}
    FreeAndNil(myIndex);
    myIndex := TStringIndexMap.Create;
    {-----}
    for _i := 0 to pred(columnCount) do
        myIndex.idx[myColumns[_i]] := _i;
end;

function RbQueryBuilderBase.formatColumnValues(_formatStr: string ='%s=%s'): string;
var
    bAddComma: boolean;
    i: integer;
    _value: string;
    _setValues: string;
begin
    _setValues := '';
    bAddComma := False;
    for i := 0 to columnCount - 1 do
    begin
        if bAddComma then
            _setValues := _setValues + ', '
        else
            bAddComma:= true;

        _value := ':' + myColumns[i];
        _setValues := _setValues + Format(_formatStr, [myColumns[i], _value]);
    end;
    Result := _setValues;
end;

constructor RbQueryBuilderBase.Create;
begin
    myShouldFreeTables := false;
    myShouldFreeColumns:= false;
    myIndex := TStringIndexMap.Create;
end;

destructor RbQueryBuilderBase.Destroy;
var
    _colFreeCount: integer = 0;
begin
    if myShouldFreeTables then myTables.Free;

    if assigned(myColumns) then
        inc(_colFreeCount);

    if myShouldFreeColumns then
    begin
        myColumns.Free;
        dec(_colFreeCount);
	end;

    if _colFreeCount <> 0 then
    begin
        Log('');
        Log('');
        Log('}-------------------------------------------------------------{');
        Log('');
        Log('--> This QueryBuilder has not freed myColums');
        Log('');
        Log('}-------------------------------------------------------------{');
	end;
	myIndex.Free;
    inherited Destroy;
end;

function RbQueryBuilderBase.tableCount: integer;
begin
    Result := myTables.Count;
end;

end.

