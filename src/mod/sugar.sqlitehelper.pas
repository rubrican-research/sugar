unit sugar.sqlitehelper;

	 {Reusable myDB Module for SQLite}
	 {Author: Stanley Stephen}
	 {stanley.stephen@gmail.com}
	 {-- July 2021}



{$mode objfpc}{$H+}

interface

uses
	 Classes, SysUtils, Controls, Forms, Dialogs, SQLDBLib, SQLDB, SQLite3Conn, fpjson;

const
	 DEFAULT_DB_FILE = 'myDB.db';
	 SEQUENCE_TABLE	= '_seq';

	 MIN_SQLITE_BIG_INT = -9223372036854775808;
	 NUM_FLAGS = 100;
	 {The number of values we want to use as flags that should not be used as ID values}

	 {Default ID starts from this value }
	 ID_START = MIN_SQLITE_BIG_INT + NUM_FLAGS;

	 {FLAGS to denote SQL search status etc}
	 ID_NOT_ASSIGNED = MIN_SQLITE_BIG_INT;
	 ID_NOT_FOUND		= MIN_SQLITE_BIG_INT + 1;

type

	 { TDBModule }
	 TScriptFunc	 = function: string;
	 TScriptMethod = function: string of object;

	TDBOpenState = (
		dbopenForbidden,// Could not open the DB
		dbopenLocked,
		// Could not open because it has been locked by another process
		dbopenReadOnly,
		// Open the DB in Read only mode. This is good if it is already open with dbopenShared
		dbopenShared,
		 // Open the DB in Write mode but allow other connections to read
		dbopenExclusive // Open the DB in exclusive mode. Do not allow reads
		);

    TDBStoreType = (
        dstFile,
        dtsMemory
    );

	 TDBModule = class(TDataModule)
			 libloader  : TSQLDBLibraryLoader;
			 myDB       : TSQLite3Connection;
			 transaction: TSQLTransaction;
			 qUpdateID  : TSQLQuery;
			 qGetNewID  : TSQLQuery;
			 procedure DataModuleCreate(Sender: TObject);
	 private
			 FDbFile:	 string;
			 FDbFolder: string;
			 myinitScriptFunc: TScriptFunc;
			 myupgradeScript: TScriptFunc;
			 myuseSequences: boolean;
		     myDbOpenState: TDBOpenState;
             myDBStoreType: TDbStoreType;
			 procedure SetDbFile(const _DbFile: string);
			 procedure SetDbFolder(const _DbFolder: string);
			 procedure SetinitScriptFunc(const _value: TScriptFunc);
			 procedure SetupgradeScript(const _value: TScriptFunc);
			 procedure setuseSequences(const _useSequences: boolean);
	 public
		constructor Create; overload;// So that we can use this with hash lists;

		{Creates the db file if it doesn't exist}
		function getSQLite3LibraryPath(): string;
		function getDBFileName(): string;

        // FileDB
		function openDB(_dbFile: string; _dbFolder: string = '';
			    _openState: TDBOpenState = dbopenExclusive): boolean;

		// Uses the current database confifguration values to initialize the db file.
		// call OpenDB() to use custom values;
		function initDB(_openState: TDBOpenState = dbopenExclusive): boolean;

        //Memory only DB
        function openMemDB(_dbFile: string; _openState: TDBOpenState = dbopenExclusive): boolean;
        function initMemDB(_openState: TDBOpenState = dbopenExclusive): boolean;

		 procedure doUpgradeDB;
		 function dropDB: boolean;
		 procedure makeDatabase;

		 function runScript(_script: string): boolean;

		 function DB: TSQLConnection;
		 function isTransactionActive: boolean;

		 {starts a transaction. If a previous transaction is active, it sleeps and tries again.}
		 function startTransaction: boolean;
		 {starts a transaction only when no transaction is active}
		 function safeStartTransaction: boolean;
		 function commit: boolean;
		 {Commits but does not close the transaction. You can do more commits to the same transaction}
		 function commitRetaining: boolean;
		 function rollback: boolean;
		 function rollbackRetaining: boolean;
		 function endTransaction: boolean;
		 function newQuery: TSQLQuery;

		 {Returns pointer to DB, sets it to active if not already}
		 function DbActive: TSQLConnection;
		 function newIDFor(_tableName: string): int64;

		 function getSequenceTableCreateScript: string; virtual;
		 function getSequenceUpsertScript: string; virtual;
		 function getSequenceSelectScript: string; virtual;

		 function getDBInitScript: string; virtual;
		 function getDBUpgradeScript: string; virtual;

		 {These set of functions check if records exist where a column has a particular value}
		 function qExists(_tbl: string; _key: string): TSQLQuery;
		 function exists(_tbl: string; _key: string; _value: string): boolean; overload;
		 function exists(_tbl: string; _key: string; _value: int64): boolean; overload;
		 function exists(_tbl: string; _key: string; _value: boolean): boolean; overload;
		 function exists(_tbl: string; _key: string; _value: double): boolean; overload;
		 function exists(_tbl: string; _where: string): boolean;

		 {Update a value in a table}
		 function qPut(_tbl: string; _id: int64; _fld: string;
				 _idFieldName: string = 'rid'): TSQLQuery;
		 function put(_tbl: string; _id: int64; _fld: string; _value: string): boolean;
		overload;
		 function put(_tbl: string; _id: int64; _fld: string; _value: int64): boolean;
		overload;
		 function put(_tbl: string; _id: int64; _fld: string; _value: boolean): boolean;
		overload;
		 function put(_tbl: string; _id: int64; _fld: string; _value: double): boolean;
		overload;
		 {Send multiple values as a json object. Parses the object, updates the table}
		 function put(_tbl: string; _id: int64; _json: TJSONObject): boolean; overload;

		 function qDelete(_tbl: string; _id: int64; _idFieldName: string = 'rid'): TSQLQuery;
		 function Delete(_tbl: string; _id: int64): boolean;

		 {Reads a value from a table}
		 function qselect(_tbl: string; _id: int64; _fld: string;
				 _idFieldName: string = 'rid'): TSQLQuery;
		 function select_str(_tbl: string; _id: int64; _fld: string): string;
		 function select_int(_tbl: string; _id: int64; _fld: string): int64;
		 function select_bool(_tbl: string; _id: int64; _fld: string): boolean;
		 function select_real(_tbl: string; _id: int64; _fld: string): double;

		 {Returns the last inserted id by calling select last_insert_rowid();}
		 function getSQLiteLastID(): int64;

			 // function execSQL(_sql: string): integer; //
		// Executes a fully formed SQL statement (not select)
		// Returns an integer:
		//		0: Could not initialize or start
		//		1: Successful
		//	   -1: There was an exception
		// Additionally, the out parameter _err contains the exception text.
		function execSQL(_sql: string; out _err: string): integer; overload;
		function execSQL(_sql: string): integer; overload;
		function execSQL(var _qry: TSQLQuery; out _err: string): integer; overload; // Does not free _qry
		function execSQL(var _qry: TSQLQuery): integer; overload; // Does not free _qry

	 public type

		{ TDBStore }

      	TDBStore = class
        protected
			tbl: string;
			dm:	TDBModule;
			function getTableCreateScript(_kvTable: string): string; virtual;
			function noInitException: Exception; virtual;
        public
            // Returns the exception message that is raised when the DBStore is not initialized;
            function noInitExceptionMsg: string; virtual;
        	constructor Create(_tbl: string; _dm: TDBModule); virtual;
			function makeTable(_tbl: string): integer; virtual;

			function exists(_key: string): boolean;
			function findKeysFor(_val: string; _limit: integer = 0): TStringArray; virtual;
			function findFirstKeyFor(_val: string): string; virtual;
			function renKey(_oldKey: string; _newKey: string): integer; virtual;
			function del(_key: string): integer; virtual;
			function deleteStore: integer; virtual;
			function Clear: integer; virtual;

	  	end;

		{ TDBKeyValueStore }
		{ FIELDS
        	k char(256)
			v text
        }
		TDBKeyValueStore = class(TDBStore)
        protected
			function getTableCreateScript(_kvTable: string): string; override;
		public
			constructor Create(_tbl: string; _dm: TDBModule); override;
			function get(_key: string): string;
			function put(_key: string; _val: string): integer; // Return -2 if tbl is empty

			function findKeysFor(_val: string; _limit: integer = 0): TStringArray; override;
			function findFirstKeyFor(_val: string): string; override;
		end;


		{ TDBKJSONStore }
		{ FIELDS
        	k char(256)
			v text
        }
        TDBKJSONStore = class(TDBStore)
        protected
			function getTableCreateScript(_kvTable: string): string; override;
		public
			constructor Create(_tbl: string; _dm: TDBModule); override;
			function get(_key: string): string;
			function put(_key: string; _val: string): integer; // Return -2 if tbl is empty

			{Find for complete JSON Doc}
            function findKeysFor(_val: TJSONData): TStringArray; reintroduce;
            function findFirstKeyFor(_val: TJSONData): string; reintroduce;

            { Find for path/value }
			function findKeysFor(_path: string; _val: TJSONData): TStringArray; overload;
			function findFirstKeyFor(_path: string; _val: TJSONData): string; overload;

		end;

	 public
			//KV Tables
	    function kv(_tbl: string): TDBModule.TDBKeyValueStore;

	 public
		property DbFile: string read FDbFile write SetDbFile;
		property DbFolder: string read FDbFolder write SetDbFolder;
		property useSequences: boolean read myuseSequences write setuseSequences;
		property initScriptFunc: TScriptFunc read myinitScriptFunc write SetinitScriptFunc;
		property upgradeScript: TScriptFunc read myupgradeScript write SetupgradeScript;
		property openMode: TDBOpenState read myDbOpenState;
	 end;

{Creates a new TDBModule object}
function newSqliteDB(_dbFile: string; _dbFolder: string;
	_initScriptFunc: TScriptFunc; _upgradeScriptFunc: TScriptFunc): TDBModule;

{ function sqliteDB(): V2
    This function instantiates a TDBModule object with the parameters supplied, ready for use.
    Internally, this unit maintains a hash list of the instantiated TDBModules and returns the object
    referred to by _dbFile (name). This way you can connect to multiple SQLite DB files by just calling
    DbModule(_file).

    NOTE: _dbFile parameter has a default value. So calling the function without any parameters
    returns an object that is instantiated to use a efault DB file in the folder where the exe is stored.
}

function sqliteDB (  _dbFile: string = DEFAULT_DB_FILE;
                    _dbFolder: string = '';
                    _initScriptFunc: TScriptFunc = nil;
                    _upgradeScriptFunc: TScriptFunc = nil): TDBModule;

function sqliteDBFree(_dbFile: string): boolean;


implementation

{$R *.lfm}
uses
	 sugar.utils, sugar.collections; //, uDefs;

type
	TDBModuleListBase = specialize GenericHashObjectList<TDBModule>;

	TDBModuleList = class(TDBModuleListBase)

	 end;

var
	 myDBModules: TDBModuleList;

function newSqliteDB(_dbFile: string; _dbFolder: string;
	_initScriptFunc: TScriptFunc; _upgradeScriptFunc: TScriptFunc): TDBModule;
begin
	Result := TDBModule.Create(nil);
	Result.DbFile := _dbFile;
	Result.DbFolder := _dbFolder;
	Result.initScriptFunc := _initScriptFunc;
	Result.upgradeScript := _upgradeScriptFunc;

end;

function sqliteDB(_dbFile: string; _dbFolder: string; _initScriptFunc: TScriptFunc;
	 _upgradeScriptFunc: TScriptFunc): TDBModule;
begin
	Result := myDBModules.find(_dbFile);
	 if not Assigned(Result) then
	 begin
	 	 Result := newSqliteDB(_dbFile, _dbFolder, _initScriptFunc, _upgradeScriptFunc);
		Result.initDB;
		myDBModules.add(_dbfile, Result);
	 end;

	if Result.openMode < dbopenReadOnly then
		raise Exception.Create(
			'It looks like Database "%s" could not be opened. Please check ');

end;

function sqliteDBFree(_dbFile: string): boolean;
begin
    Result:= myDBModules.delete(_dbFile);
end;

{ TDBModule.TDBKJSONStore }

function TDBModule.TDBKJSONStore.getTableCreateScript(_kvTable: string): string;
begin

end;

constructor TDBModule.TDBKJSONStore.Create(_tbl: string; _dm: TDBModule);
begin
	 inherited Create(_tbl, _dm);
end;

function TDBModule.TDBKJSONStore.get(_key: string): string;
begin

end;

function TDBModule.TDBKJSONStore.put(_key: string; _val: string): integer;
begin

end;

function TDBModule.TDBKJSONStore.findKeysFor(_val: TJSONData): TStringArray;
begin

end;

function TDBModule.TDBKJSONStore.findFirstKeyFor(_val: TJSONData): string;
begin

end;

function TDBModule.TDBKJSONStore.findKeysFor(_path: string; _val: TJSONData
	 ): TStringArray;
begin

end;

function TDBModule.TDBKJSONStore.findFirstKeyFor(_path: string; _val: TJSONData
	 ): string;
begin

end;

{ TDBModule.TDBStore }

function TDBModule.TDBStore.getTableCreateScript(_kvTable: string): string;
begin
     Result:= '';
end;

function TDBModule.TDBStore.noInitException: Exception;
begin
	Result := Exception.Create(noInitExceptionMsg());
end;

function TDBModule.TDBStore.noInitExceptionMsg: string;
begin
	Result:= ClassName + ' is not initialized';
end;

constructor TDBModule.TDBStore.Create(_tbl: string; _dm: TDBModule);
begin
    inherited Create;
   dm := _dm;
   if not _tbl.IsEmpty then makeTable(_tbl);
end;

function TDBModule.TDBStore.makeTable(_tbl: string): integer;
begin
	Result := dm.execSQL(getTableCreateScript(_tbl));
	case Result of
		-1, 0: tbl := '';
		1: tbl		 := _tbl;
	 end;
end;

function TDBModule.TDBStore.exists(_key: string): boolean;
begin
	 if tbl.isEmpty then
	begin
		Result := False;
		raise noInitException;
	end;
	 Result := dm.Exists(tbl, format('k = "%s"', [_key]));
end;

function TDBModule.TDBStore.findKeysFor(_val: string; _limit: integer
	): TStringArray;
begin
     Result := [];
end;

function TDBModule.TDBStore.findFirstKeyFor(_val: string): string;
begin
     Result:= '';
end;

function TDBModule.TDBStore.renKey(_oldKey: string; _newKey: string): integer;
const
	 Q = 'UPDATE %0:s SET k = :newkey WHERE k= :oldkey; ';
var
	 _qry: TSQLQuery;
begin
	if tbl.isEmpty then
	begin
			Result := -2;
		raise noInitException;
	end;

	Result := 0;
	 _qry := dm.newQuery();
	 try
		_qry.SQL.Text := Format(Q, [tbl]);
		_qry.params[0].AsString := _newKey;
		_qry.params[1].AsString := _oldKey;
		Result := dm.execSQL(_qry);
	 finally
			 _qry.Free;
	 end;
end;

function TDBModule.TDBStore.del(_key: string): integer;
const
	 Q = 'DELETE FROM %0:s WHERE k= :key;';
var
	 _qry: TSQLQuery;
begin
	if tbl.isEmpty then
	begin
			Result := -2;
		raise noInitException;
	end;

	Result := 0;
	 _qry	 := dm.newQuery();
	 try
	 	 _qry.SQL.Text := Format(Q, [tbl]);
		_qry.Params[0].AsString := _key;
		Result := dm.execSQL(_qry);
	 finally
			 _qry.Free;
	 end;
end;

function TDBModule.TDBStore.deleteStore: integer;
const
	 Q = 'DROP TABLE IF EXISTS %0:s;';
var
	 _qry: TSQLQuery;
begin
	if tbl.isEmpty then
	begin
			Result := -2;
		raise noInitException;
	end;

	Result := 0;
	 _qry	 := dm.newQuery();
	 try
	 	 _qry.SQL.Text := Format(Q, [tbl]);
		Result := dm.execSQL(_qry);
		tbl		:= '';
	 finally
	 	 _qry.Free;
	 end;
end;

function TDBModule.TDBStore.Clear: integer;
const
	 Q = 'DELETE FROM %0:s;';
var
	 _qry: TSQLQuery;
begin
	if tbl.isEmpty then
	begin
			Result := -2;
		raise noInitException;
	end;

	Result := 0;
	 _qry	 := dm.newQuery();
	 try
	 	 _qry.SQL.Text := Format(Q, [tbl]);
		Result := dm.execSQL(_qry);
		tbl		:= '';
	 finally
	 	 _qry.Free;
	 end;
end;

{ TDBModule.TDBKeyValueStore }



function TDBModule.TDBKeyValueStore.getTableCreateScript(_kvTable: string): string;
const
	 Q = 'CREATE TABLE IF NOT EXISTS %0:s ( ' + sLinebreak
        	+ '	 k char(256) primary key,' + sLinebreak
            + '	 v text ' + sLinebreak
     		+ ');' + sLinebreak + ' ' + sLinebreak

            + 'CREATE INDEX IF NOT EXISTS %0:s_v ON %0:s (v);' + sLinebreak;
begin
	 Result := Format(Q, [_kvTable]);
end;


constructor TDBModule.TDBKeyValueStore.Create(_tbl: string; _dm: TDBModule);
begin
    inherited Create(_tbl, _dm);
end;

function TDBModule.TDBKeyValueStore.get(_key: string): string;
const
	 Q = 'SELECT v from %s WHERE k = "%s"';
var
	 _qry: TSQLQuery;
begin

	 if tbl.IsEmpty then
	begin
		Result := 'Empty';
		raise noInitException;
	end;

	Result := '';
	 _qry	 := dm.newQuery();
	 try
			 _qry.SQL.Text := Format(Q, [tbl, _key]);
			 _qry.Open;
			 _qry.First;
			 if not _qry.Fields[0].IsNull then
					Result := _qry.Fields[0].AsString;
	 finally
			 _qry.Free;
	 end;
end;

function TDBModule.TDBKeyValueStore.put(_key: string; _val: string): integer;
const
	 Q = 'INSERT INTO %0:s (k,v) VALUES (:k, :v) ON CONFLICT (k) DO UPDATE SET v = excluded.v WHERE k=excluded.k;';
var
	 _qry: TSQLQuery;
begin
	 if tbl.IsEmpty then
	begin
		Result := -2;
		raise noInitException;
	end;

	Result := 0;
	 _qry	 := dm.newQuery();
	 try
	 	 _qry.SQL.Text := Format(Q, [tbl]);
		_qry.ParamByName('k').AsString := _key;
		_qry.ParamByName('v').AsString := _val;
		Result := dm.execSQL(_qry);
	 finally
	 	 _qry.Free;
	 end;
end;



function TDBModule.TDBKeyValueStore.findKeysFor(_val: string; _limit: integer
	): TStringArray;
const
	 Q = 'SELECT k from %s WHERE v = :val ORDER BY K ASC %s';
var
	 _qry: TSQLQuery;
	 _i: integer;
	_result: string = '';
	_comma: string = '';
	_addComma: boolean = False;
    _limStr : string = '';
begin
	if tbl.isEmpty then
	begin
		SetLength(Result, 0);
		raise noInitException;
	end;


	 _qry := dm.newQuery();
	 try
         if _limit > 0 then _limStr := Format(' LIMIT %d ', [_limit]);

	 	_qry.SQL.Text := Format(Q, [tbl, _limStr]);
		_qry.Params[0].AsString := _val;
		_qry.Open;

		SetLength(Result, _qry.RowsAffected);

		_qry.First;
		_i := 0;

		while not _qry.EOF do
		begin
			if not _qry.Fields[0].IsNull then
			begin
				_result := _result + _comma + _qry.Fields[0].AsString;
				Inc(_i);
				 end;
			_qry.Next;

			if not _addComma then
			begin
				_comma		:= ',';
				_addComma := True;
				 end;
			end;

			Result := _result.split(_comma);
	 finally
	 	 _qry.Free;
	 end;
end;

function TDBModule.TDBKeyValueStore.findFirstKeyFor(_val: string): string;
var
	_keys: TStringArray;
begin
	Result := '';
	 _keys	:= findKeysFor(_val, 1);
	if Length(_keys) > 0 then Result := _keys[0];
end;


{ TDBModule }

procedure TDBModule.DataModuleCreate(Sender: TObject);
begin
     {$IFDEF WINDOWS}
	 libLoader.ConnectionType := 'SQLite3';
	 libLoader.LibraryName    := getSQLite3LibraryPath();
	 libLoader.Enabled        := True;
     {$ENDIF}

     {$IF defined(UNIX)}
     {TODO}
	 libLoader.ConnectionType := 'SQLite3';
	 libLoader.LibraryName := getSQLite3LibraryPath();
	 libLoader.Enabled := True;
     {$ENDIF}

     {$IF defined(DARWIN)}
	 {TODO}
     libLoader.ConnectionType := 'SQLite3';
	 libLoader.LibraryName := getSQLite3LibraryPath();
	 libLoader.Enabled := True;
     {$ENDIF}

	 useSequences	    := True;
	 qUpdateID.SQL.Text := getSequenceUpsertScript();
	 qGetNewID.SQL.Text := getSequenceSelectScript();
	 myDbOpenState      := dbopenExclusive;
end;

procedure TDBModule.SetDbFile(const _DbFile: string);
begin
	 if FDbFile = _DbFile then	Exit;
	if not DB.Connected then
		FDbFile := _DbFile
	else
		raise Exception.Create(Format(
			'Database "%s" is currently open. You change DbFile value after the connection is closed.',
			[getDBFileName()]));
end;

procedure TDBModule.SetDbFolder(const _DbFolder: string);
begin
	 if FDbFolder = _DbFolder then Exit;
	if not DB.Connected then
		FDbFolder := _DbFolder
	else
		raise Exception.Create(Format(
			'Database "%s" is currently open. You change DbFolder value after the connection is closed.',
			[getDBFileName()]));
end;

function TDBModule.getSQLite3LibraryPath(): string;
begin
	{$IFDEF windows}
	 Result := ExpandFileName('sqlite3.dll');
	{$ELSE}

    {$ELSEIFDEF UNIX}
    Result := ExpandFileName('sqlite3.so');
    if not fileExists(Result) then
        Result := 'sqlite3.so';
    {$ELSEIFDEF DARWIN}

    {$ENDIF}

end;

function TDBModule.getDBFileName(): string;
begin
	 if DbFile.IsEmpty then
			 DbFile := DEFAULT_DB_FILE;

	 if DbFolder.IsEmpty then
			 DbFolder := ExpandFileName('');

	 Result := appendPath([dbFolder, DbFile]);
end;

procedure TDBModule.SetinitScriptFunc(const _value: TScriptFunc);
begin
	 if myinitScriptFunc = _value then Exit;
	 myinitScriptFunc := _value;
end;

procedure TDBModule.SetupgradeScript(const _value: TScriptFunc);
begin
	 if myupgradeScript = _value then Exit;
	 myupgradeScript := _value;
end;

procedure TDBModule.setuseSequences(const _useSequences: boolean);
begin
	 if myuseSequences = _useSequences then Exit;
	 myuseSequences := _useSequences;
end;

constructor TDBModule.Create;
begin
	Create(nil);
end;

function TDBModule.initDB(_openState: TDBOpenState): boolean;
var
	_DBInitScript: string;

	 function isExclusive(_db: TSQLConnection): boolean;
	const
		Q = 'PRAGMA locking_mode';
		_EXCLUSIVE = 'exclusive';
	var
		_qry: TSQLQuery;
	begin
		_qry := newQuery;
		try
				 _qry.SQL.Text := Q;
			_qry.Open;
			_qry.First;
			if not _qry.Fields[0].IsNull then
				Result := lowercase(_qry.Fields[0].AsString) = _EXCLUSIVE
			else
				Result := True; // assume exclusive because cannot open
			finally
			_qry.Free;
			end;
	 end;

	 procedure openReadOnly;
	begin
		if startTransaction then
		begin
			(*
		  	These settings allow the SQLite3 database to finish writing faster.
		  	See https://www.sqlite.org/pragma.html#pragma_synchronous
		  	Stanley.
		  	*)

				 {This is necessary workaround. https://wiki.freepascal.org/SQLite search for "Pragma and Vacuum" }
				 myDB.ExecuteDirect('END TRANSACTION;');

				 {Allow OS to handle writing. Simplifies commit}
				 myDB.ExecuteDirect('PRAGMA query_only = 1;');

				 {Exclusive File lock}
				 myDB.ExecuteDirect('PRAGMA locking_mode = ''NORMAL'';');

				 {Write Ahead //Log}
				 myDB.ExecuteDirect('PRAGMA journal_mode = ''WAL'';');

			{Lazarus workaround for executing PRAGMA. Done.}
				 myDB.ExecuteDirect('BEGIN TRANSACTION;');
				 endTransaction;
			end;
	 end;

	procedure openShared;
	begin
		if startTransaction then
		begin
				 	(*
            These settings allow the SQLite3 database to finish writing faster.
            See https://www.sqlite.org/pragma.html#pragma_synchronous
            Stanley.
            *)

			{This is necessary workaround. https://wiki.freepascal.org/SQLite search for "Pragma and Vacuum" }
			myDB.ExecuteDirect('END TRANSACTION;');

			{Allow OS to handle writing. Simplifies commit}
			myDB.ExecuteDirect('PRAGMA synchronous = 1;');

			{Exclusive File lock}
			myDB.ExecuteDirect('PRAGMA locking_mode = ''NORMAL'';');

			{Write Ahead //Log}
			myDB.ExecuteDirect('PRAGMA journal_mode = ''WAL'';');


			{Lazarus workaround for executing PRAGMA. Done.}
			myDB.ExecuteDirect('BEGIN TRANSACTION;');

			endTransaction;

			doUpgradeDB;
		end;

	 end;

	procedure openExclusive;
	begin
		if startTransaction then
		begin
				 	(*
            These settings allow the SQLite3 database to finish writing faster.
            See https://www.sqlite.org/pragma.html#pragma_synchronous
            Stanley.
            *)

			{This is necessary workaround. https://wiki.freepascal.org/SQLite search for "Pragma and Vacuum" }
			myDB.ExecuteDirect('END TRANSACTION;');

			{Allow OS to handle writing. Simplifies commit}
			myDB.ExecuteDirect('PRAGMA synchronous = 1;');

			{Exclusive File lock}
			myDB.ExecuteDirect('PRAGMA locking_mode = ''EXCLUSIVE'';');

			{Write Ahead //Log}
			myDB.ExecuteDirect('PRAGMA journal_mode = ''WAL'';');


			myDB.ExecuteDirect('BEGIN TRANSACTION;');
			{Lazarus workaround for executing PRAGMA. Done.}
			endTransaction;
			doUpgradeDB;
		end;
	 end;

begin

	Result := True;
	myDbOpenState := _openState;

    if myDB.Connected then myDB.Connected := False;

	 myDB.DatabaseName := getDBFileName();		{Gets the file name that was set}

	 { Does the DB File exist? }
	 if not fileExists(myDB.DatabaseName) then
	 begin
			{ Make the DB File }
	 	 myDB.CreateDB;
			_DBInitScript := getDBInitScript();
			if not _DBInitScript.isEmpty then
				 Result := runScript(_DBInitScript);
	 end;

	 { Connect to the DB }
	if Result then
	 begin
	 	 try
			DbActive;
			except
			if isExclusive(myDB) then
				myDbOpenState := dbopenLocked // The database is locked so we cannot open it
			//else
			//	myDbOpenState := dbopenForbidden;
			end;

		case myDbOpenState of
				 dbopenForbidden: ;
				 dbopenLocked: ;
			     dbopenReadOnly:    openReadOnly;
				 dbopenShared:      openShared;
				 dbopenExclusive:   openExclusive;
		end;

	 end;
end;

function TDBModule.openMemDB(_dbFile: string; _openState: TDBOpenState
	): boolean;
begin
    {This section is duplicated in OpenDB()}
  	 DbFile	 := _dbFile;
	 DbFolder := '';

	 if isTransactionActive then
	 begin
	 	 DB.EndTransaction;
		 DB.CloseDataSets;
	 end;
     {duplicate end}

    result:= initMemDB(_openState);
end;

function TDBModule.initMemDB(_openState: TDBOpenState): boolean;
begin
    myDbOpenState:= _openState;

    if myDB.Connected then myDB.Connected := False;

    with myDB do
    begin
      OpenFlags := [sofReadWrite, sofCreate, sofURI];
      DatabaseName := format('file:%s?mode=memory',[DbFile]);
    end;

end;

procedure TDBModule.doUpgradeDB;
begin

end;

function TDBModule.dropDB: boolean;
begin
	 if DB.Connected then
	 begin
			 if isTransactionActive then
			 begin
					 endTransaction;
			 end;
			 DB.Close();
	 end;
	 Result := DeleteFile(getDBFileName());
end;

procedure TDBModule.makeDatabase;
var
	 _shouldInitDB: boolean = True;
	 mr: TModalResult;
begin
	 if FileExists(getDBFileName()) then
	 begin
			 mr := MessageDlg('Question', 'Do you want to delete the existing DB file?',
					 mtConfirmation, [mbYes, mbNo], 0);

			 if mr = mrYes then
					 _shouldInitDB := dropDB
			 else
					 _shouldInitDB := False;
	 end;

	 if _shouldInitDB then
	 begin
			 initDB;
	 end;
end;

function TDBModule.openDB(_dbFile: string; _dbFolder: string;
	 _openState: TDBOpenState): boolean;
begin
	 DbFile	  := _dbFile;
	 DbFolder := _dbFolder;
	 if isTransactionActive then
	 begin
	 	 DB.EndTransaction;
		 DB.CloseDataSets;
	 end;

	 Result := initDB(_openState);
end;

function TDBModule.runScript(_script: string): boolean;
var
	 scripts: TSQLScript;
begin
	 if _script.isEmpty then
	 begin
			Result := False;
			exit;
	 end;
	 scripts := TSQLScript.Create(myDB);
	 scripts.DataBase := myDB;
	 scripts.Transaction := transaction;
	 try
			myDB.Open;
			if startTransaction then
		    begin
				 scripts.Script.add(_script);
			end;
			try
				 try
					 scripts.Execute;
				 	 commit;
				 	 Result := True;
				 except
						Result := False;
				 end;
			finally
				 endTransaction;
			end;
	 finally
			scripts.Free;
	 end;
end;

function TDBModule.DB: TSQLConnection;
begin
	 Result := myDB;
end;


function TDBModule.isTransactionActive: boolean;
begin
	 Result := myDB.Transaction.Active;
end;

function TDBModule.startTransaction: boolean;
const
	 NUM_TRIES = 3;
var
	 _attempt: byte = 0;
begin
	 Result := False;
	 while (not Result) and (_attempt < NUM_TRIES) do
	 begin
			 try
					 DbActive.StartTransaction;
					 Result := True;
			 except
					 Result := False;
			 end;
			 if not Result then
			 begin
					 Inc(_attempt);
					 sleep(570);
			 end;
	 end;
end;


function TDBModule.safeStartTransaction: boolean;
begin
	 Result := True;
	 if not isTransactionActive then
			 Result := startTransaction;
end;

function TDBModule.commit: boolean;
begin
	 try
			 myDB.Transaction.Commit;
			 Result := True;
	 except
			 Result := False;
	 end;
end;

function TDBModule.commitRetaining: boolean;
begin
	 try
			 myDB.Transaction.CommitRetaining;
			 Result := True;
	 except
			 Result := False;
	 end;

end;

function TDBModule.rollback: boolean;
begin
	 try
			 myDB.Transaction.RollbackRetaining;
			 Result := True;
	 except
			 Result := False;
	 end;
end;

function TDBModule.rollbackRetaining: boolean;
begin
	 try
			 myDB.Transaction.RollbackRetaining;
			 Result := True;
	 except
			 Result := False;
	 end;
end;

function TDBModule.endTransaction: boolean;
begin
	 try
			 myDB.Transaction.EndTransaction;
			 Result := True;
	 except
			 Result := False;
	 end;
end;

function TDBModule.newQuery: TSQLQuery;
begin
	 Result := TSQLQuery.Create(myDB);
     Result.DataBase := TSQLConnection(myDB);
	 Result.Transaction := transaction;
end;

function TDBModule.DbActive: TSQLConnection;
begin
	 Result := Db;
	 if not Result.Connected then
	begin
		try
	 			Result.Connected := True;
	 	 except
				 raise;
	 	 end;
	 end;
end;

function TDBModule.newIDFor(_tableName: string): int64;
var
	 _useNewTransaction: boolean;
begin

	 Result := ID_NOT_ASSIGNED;
	 _useNewTransaction := not isTransactionActive;

	 if _useNewTransaction then
			 startTransaction;

	 //Log('DM0001[D]=='+_prof.time('Transaction started'));
	 qUpdateID.Params[0].AsString	 := _tableName;
	 qUpdateID.Params[1].AsLargeInt := ID_START;
	 try
			 try
					 qUpdateID.ExecSQL;
					 if _useNewTransaction then
							 commitRetaining;

					 qGetNewID.Params[0].AsString := _tableName;
					 qGetNewID.Open;

					 {There is only one columm}
					 Result := qGetNewID.Fields[0].AsLargeInt;
					 qGetNewID.Close;

			 except
					 if _useNewTransaction then
							 rollback;

					 raise;
			 end;

	 finally
			 if _useNewTransaction then
					 endTransaction;
	 end;
end;

function TDBModule.getSequenceTableCreateScript: string;
begin
	 Result := format('CREATE TABLE IF NOT EXISTS %s (' + sLinebreak +
			 '   name CHAR(127) UNIQUE,' + sLinebreak + '  curr_id BIGINT' +
			 sLinebreak + ');', [SEQUENCE_TABLE]);
end;

function TDBModule.getSequenceUpsertScript: string;
begin
	 Result := format('INSERT INTO %s ' + sLinebreak + '(name, curr_id) ' +
			 sLinebreak + 'VALUES (:name, :start_id) ' + sLinebreak +
			 'ON CONFLICT (name) DO UPDATE ' + sLinebreak + 'set curr_id = curr_id + 1;',
			 [SEQUENCE_TABLE]);

end;

function TDBModule.getSequenceSelectScript: string;
begin
	 Result := format('SELECT CAST(curr_id as BIGINT) as ID from %s WHERE name = :name',
			 [SEQUENCE_TABLE]);
end;


function TDBModule.getDBInitScript: string;
begin
	 Result := getSequenceTableCreateScript() + sLineBreak;
	 if assigned(myinitScriptFunc) then
			 Result := Result + initScriptFunc();
end;

function TDBModule.getDBUpgradeScript: string;
begin
	 if assigned(myupgradeScript) then
			 Result := myupgradeScript()
	 else
			 Result := '';
end;

function TDBModule.qExists(_tbl: string; _key: string): TSQLQuery;
begin
	 Result := newQuery;
	 with Result do
	 begin
			 SQL.Text := Format('select true from %0:s WHERE %1:s = :%1:s LIMIT 1', [_tbl, _key]);
	 end;
end;

function TDBModule.exists(_tbl: string; _key: string; _value: string): boolean;
begin
	 with qExists(_tbl, _key) do
	 begin
			 Params[0].AsString := _value;
			 try
					 Open;
					 First;
					 if not ((BOF = EOF)) then
					 begin
							 if Fields[0].isNull then
									 Result := False
							 else
									 Result := True;
					 end
					 else
							 Result := False;
			 except
					 Result := False;
			 end;
			 Free;
	 end;
end;

function TDBModule.exists(_tbl: string; _key: string; _value: int64): boolean;
begin
	 with qExists(_tbl, _key) do
	 begin
			 Params[0].AsLargeInt := _value;
			 try
					 Open;
					 First;
					 if not ((BOF = EOF)) then
					 begin
							 if Fields[0].isNull then
									 Result := False
							 else
									 Result := True;
					 end
					 else
							 Result := False;
			 except
					 Result := False;
			 end;
			 Free;
	 end;
end;

function TDBModule.exists(_tbl: string; _key: string; _value: boolean): boolean;
begin
	 with qExists(_tbl, _key) do
	 begin
			 Params[0].AsBoolean := _value;
			 try
					 Open;
					 First;
					 if not ((BOF = EOF)) then
					 begin
							 if Fields[0].isNull then
									 Result := False
							 else
									 Result := True;
					 end
					 else
							 Result := False;
			 except
					 Result := False;
			 end;
			 Free;
	 end;
end;

function TDBModule.exists(_tbl: string; _key: string; _value: double): boolean;
begin
	 with qExists(_tbl, _key) do
	 begin
			 Params[0].AsFloat := _value;
			 try
					 Open;
					 First;
					 if not ((BOF = EOF)) then
					 begin
							 if Fields[0].isNull then
									 Result := False
							 else
									 Result := True;
					 end
					 else
							 Result := False;
			 except
					 Result := False;
			 end;
			 Free;
	 end;
end;

function TDBModule.exists(_tbl: string; _where: string): boolean;
begin
	 with newQuery do
	 begin
			 SQL.Text := Format('select true from %0:s WHERE %1:s;', [_tbl, _where]);
			 try
					 Open;
					 First;
					 if not ((BOF = EOF)) then
					 begin
							Result := not Fields[0].isNull;
					 end
					 else
				 		 Result := False;
			 except
					 Result := False;
			 end;
			 Free;
	 end;
end;

function TDBModule.qPut(_tbl: string; _id: int64; _fld: string;
	 _idFieldName: string): TSQLQuery;
begin
	 Result := newQuery;
	 Result.SQL.Text := format('UPDATE %0:s SET %1:s=:%1:s WHERE %2:s=:%2:s',
			 [_tbl, _fld, _idFieldName]);
	 Result.Params[1].AsLargeInt := _id;
end;

function TDBModule.put(_tbl: string; _id: int64; _fld: string; _value: string): boolean;
var
	 _qry: TSQLQuery;
begin
	 _qry := qPut(_tbl, _id, _fld);
	 try
			 _qry.params[0].AsString := _value;
			 try
					 _qry.ExecSQL;
					 Result := True;
			 except
					 Result := False;
			 end;
	 finally
			 _qry.Free;
	 end;
end;

function TDBModule.put(_tbl: string; _id: int64; _fld: string; _value: int64): boolean;
var
	 _qry: TSQLQuery;
begin
	 _qry := qPut(_tbl, _id, _fld);
	 try
			 _qry.params[0].AsLargeInt := _value;
			 try
					 _qry.ExecSQL;
					 Result := True;
			 except
					 Result := False;
			 end;
	 finally
			 _qry.Free;
	 end;
end;

function TDBModule.put(_tbl: string; _id: int64; _fld: string; _value: boolean): boolean;
var
	 _qry: TSQLQuery;
begin
	 _qry := qPut(_tbl, _id, _fld);
	 try
			 _qry.params[0].AsBoolean := _value;
			 try
					 _qry.ExecSQL;
					 Result := True;
			 except
					 Result := False;
			 end;
	 finally
			 _qry.Free;
	 end;
end;

function TDBModule.put(_tbl: string; _id: int64; _fld: string; _value: double): boolean;
var
	 _qry: TSQLQuery;
begin
	 _qry := qPut(_tbl, _id, _fld);
	 try
			 _qry.params[0].AsFloat := _value;
			 try
					 _qry.ExecSQL;
					 Result := True;
			 except
					 Result := False;
			 end;
	 finally
			 _qry.Free;
	 end;
end;

function TDBModule.put(_tbl: string; _id: int64; _json: TJSONObject): boolean;
begin
	 Result := False;
end;

function TDBModule.qDelete(_tbl: string; _id: int64; _idFieldName: string): TSQLQuery;
begin
	 Result := newQuery;
	 Result.SQL.Text := format('DELETE FROM %0:s WHERE %1:s=:%1:s',
			 [_tbl, _idFieldName, _idFieldName]);
	 Result.params[0].AsLargeInt := _id;
end;

function TDBModule.Delete(_tbl: string; _id: int64): boolean;
var
	 _qry: TSQLQuery;
begin
	 _qry := qDeletE(_tbl, _id);
	 try
			 _qry.ExecSQL;
			 Result := True;
	 except
			 Result := False;
	 end;
end;

function TDBModule.qselect(_tbl: string; _id: int64; _fld: string;
	 _idFieldName: string): TSQLQuery;
begin
	 Result := newQuery;
	 Result.SQL.Text := format('SELECT %s FROM %s WHERE %s=:%s LIMIT 1',
			 [_fld, _tbl, _idFieldName, _idFieldName]);
	 Result.params[0].AsLargeInt := _id;
end;

function TDBModule.select_str(_tbl: string; _id: int64; _fld: string): string;
var
	 _qry: TSQLQuery;
begin
	 Result := '';
	 _qry	 := qSelect(_tbl, _id, _fld);
	 try
			 _qry.Open;
			 {while loop serves as if statement. Only one record. }
			 while not _qry.EOF do
			 begin
					 Result := _qry.Fields[0].AsString;
					 _qry.Next;
			 end;
	 finally
			 _qry.Free;
	 end;
end;

function TDBModule.select_int(_tbl: string; _id: int64; _fld: string): int64;
var
	 _qry: TSQLQuery;
begin
	 Result := 0;
	 _qry	 := qSelect(_tbl, _id, _fld);
	 try
			 _qry.Open;
			 {while loop serves as if statement. Only one record. }
			 while not _qry.EOF do
			 begin
					 Result := _qry.Fields[0].AsLargeInt;
					 _qry.Next;
			 end;
	 finally
			 _qry.Free;
	 end;
end;

function TDBModule.select_bool(_tbl: string; _id: int64; _fld: string): boolean;
var
	 _qry: TSQLQuery;
begin
	 Result := False;
	 _qry	 := qSelect(_tbl, _id, _fld);
	 try
			 _qry.Open;
			 {while loop serves as if statement. Only one record. }
			 while not _qry.EOF do
			 begin
					 Result := _qry.Fields[0].AsBoolean;
					 _qry.Next;
			 end;
	 finally
			 _qry.Free;
	 end;
end;

function TDBModule.select_real(_tbl: string; _id: int64; _fld: string): double;
var
	 _qry: TSQLQuery;
begin
	 Result := 0;
	 _qry	 := qSelect(_tbl, _id, _fld);
	 try
			 _qry.Open;
			 {while loop serves as if statement. Only one record. }
			 while not _qry.EOF do
			 begin
					 Result := _qry.Fields[0].AsFloat;
					 _qry.Next;
			 end;
	 finally
			 _qry.Free;
	 end;
end;

function TDBModule.getSQLiteLastID(): int64;
const
	 Q = 'select last_insert_rowid()';
var
	 _qry: TSQLQuery;
begin
	 _qry := newQuery();
	 try
			 _qry.SQL.Text := Q;
			 _qry.Open;
			 Result := _qry.Fields[0].AsLargeInt;
	 finally
			 _qry.Free;
	 end;
end;

function TDBModule.execSQL(_sql: string; out _err: string): integer;
var
	 _qry: TSQLQuery;
begin
	 Result := 0;
	 _qry	 := newQuery();
	 try
	 	 _qry.SQL.Text := _sql;
		Result := execSQL(_qry, _err);
	 finally
	 	 _qry.Free;
	 end;
end;

function TDBModule.execSQL(_sql: string): integer;
var
	_err: string;
begin
	// Swallow _err;
	 Result := execSQL(_sql, _err);
end;

function TDBModule.execSQL(var _qry: TSQLQuery; out _err: string): integer;
begin
	 Result := 0;
	if safeStartTransaction then
	begin
			try
				 _qry.ExecSQL;
				 commit;
				 Result := 1;
			except
			on E: Exception do
			begin
				Result := -1;
				_err := E.ToString;
				rollback;
			end;
			end;
	end;

end;

function TDBModule.execSQL(var _qry: TSQLQuery): integer;
var
	_err: string;
begin
	// Swallow _err;
	 Result := execSQL(_qry, _err);
end;


function TDBModule.kv(_tbl: string): TDBModule.TDBKeyValueStore;
begin
	 Result := TDBModule.TDBKeyValueStore.Create(_tbl, self);
end;

initialization
	 myDBModules := TDBModuleList.Create();

finalization
	myDBModules.Free;
end.
