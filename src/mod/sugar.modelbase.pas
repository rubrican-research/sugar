unit sugar.modelbase;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
    Classes, SysUtils, DB, sqlDB, fgl, fpjson, sugar.utils, sugar.collections,
    sugar.ddldatatypes, sugar.ddlmodel, sugar.querybuilder, sugar.jsonlib;

const
    NEWID = 0;
    MODEL_VERSION = '1';
    DB_VERSION = MODEL_VERSION; // Alias;

    {When you export a model to JSON, this key value pair indicates that the
    JSON object is a rubrican model. There is another key that specifies the
    exact class name and the model name}
    JSON_MODELSIGNATURE_KEY = 'gen';
    JSON_MODELSIGNATURE_VALUE = 'rbmd';
    ROW_LOCK_SPAN = 15; {mins}
type

    TDbSaveStatus = (
          jdbsaveUnknown,
          jdbsaveNotLocked,      {The record was not locked previously}
          jdbsaveIsLocked,       {The record is currently locked}
          jdbsaveInvalidKey,     {The unlock key is incorrect or no longer valid}
          jdbsaveLockExtended,   {The record was saved and the lock was extended}
          jdbsaveUnlocked,       {The record was saved and then it was unlocked}
          jdbsaveCancelled,      {The edit was cancelled and the row was unlocked}
          jdbsaveError,          {There was an exception while saving}
          jdbSuccess,            {The operation was successful}
          jdbFail                {The operation failed}
    );

    { TJSONDbSaveStatusHelper }

    TJSONDbSaveStatusHelper = type helper for TDbSaveStatus
      function status: string;
      function message: string;
    end;


    RbModelDef = DTable;

    {DRowState----
    Inidicates the state of the row at any time. Remember to set the
    state whenever you invoke an action that affects state, like
    New, Edit, Delete, Save etc.}
    DRowState = (rowInit, rowReady, rowNew, rowEditing, rowSaving, rowCommitting, rowDeleting);

    {DRowType----
    Indicates the row type. When a row is read, we need to know if it is a
    DataRow = row that is displayed by the application
    DraftRow: This row is in the drafts table. This means that
                    - there is a corresponding DataRow
                    - the DataRow is being edited by a user
                   - the data has not yet been saved and published
    JournalRow: This is row is in the TblJournal. This means that
                    - there is a corresponding DataRow
                    - there is history, version.
    ArchiveRow: This row is frozen with a snapshot of all references.}
    DRowType = (rowTypeDefault, rowTypeData, rowTypeDraft, rowTypeJournal, rowTypeArchive);

    {DLockType-----
    A row (irrespective of RowType can be locked. The lock states are:
    lockNone: The row is not locked
    lockRead: The row can be read. Similar to function as lockNone until business
              rule changes
    lockReadForUpdate: The row has been read specifically for editing. This means
                     that other users may read this row but the should be aware
                     that someone is editing this record currently and the data
                     can change.
    lockWrite:  Typically, just before the app
               sends an update command, the lock should be set to lockWrite.
               Once the write is complete, the lock should be set to lockNone
               or lockRead
               Other read requests are not allowed to complete if the lock is
               set to lockWrite}
    DLockType = (lockNone, lockRead, lockReadForUpdate, lockWrite);

    {DUserStatus----
    userDisabled: The user is not allowed to login

    userOK: Everything is fine. The user can log in if needed

    userLoggedIn: The user is currently logged in

    userLoggedOut: The user has logged out of the system. The app can maintain
                   this state or set it to userOK

    userIdle: The user has logged in but there is no activity being done.
              Typically, at the end of every request, the user can be set to
              userIdle

    userLockedOut: If the user has entered wrong password 3 times, they can be
                   locked out for a specific amount of time after which the
                   system reverts to userOk. }

    DUserStatus = (userDisabled, userOk, userLoggedIn, userLoggedOut, userIdle,
        userLockedOut);

    DSortOrder = (sortDefault, sortASC, sortDESC, sortSOUNDEX, sortCustom);

    RbProcSequenceGenerator = function (_seqName: string): LongInt;

    RbRow = class;
    RbRowClass = class of RbRow;
    ProcRowChange = procedure(_row: RbRow; _field: string) of object;

    RbRowsBase = specialize TFPGObjectList<RbRow>;

    { RbRows }

    RbRows = class(RbRowsBase)
        function jsonArray: TJSONArray;
        function jsonArrayj(_theseCols: TStringArray): TJSONArray;
        function delete(_obj: RbRow): boolean; overload;
    end;

    RbIndexListBase = specialize GenericHashObjectList<TIntegerListMap>;

    RbIndexList = class(RbIndexListBase)

    end;

    RbModel = class;
    RbModelClass = class of RbModel;

    { RbModelLink }

    RbModelLink = class
        Name: string;
        owner: RbModel;
        linkedModel: RbModel;
        linkType: DModelLinkType;
        {currently looking at just one field}
        sourceField: DField;
        linkedField: DField;
        function jsonObj(_withlinks: boolean = False; _recursive: boolean = False): TJSONObject;
        function asJSON: string;
        function loadJSON(_json: TJSONStringType): boolean;
    end;

    RbModelLinkListBase = specialize GenericHashObjectList<RbModelLink>;

    { RbModelLinkList }

    RbModelLinkList = class(RbModelLinkListBase)
        function asJSONArray(_withlinks: boolean = False;
            _recursive: boolean = False): TJSONArray;
        function asJSON: string;
        function loadJSON(_json: TJSONStringType): boolean;
    end;

    { RbModel }
    RbModel = class
	private
		myletDBInsertIDField: boolean;
		procedure setletDBInsertIDField(const _letDBInsertPK: boolean);
		function getColumnListWithoutIDField(_columns: string): string;
    protected
        myModelDef: RbModelDef;
        myRows: RbRows;
        myIndices: RbIndexList;
        {everytime a value changes in the row and that row has been indexed then}
        {use the integer value as a boolean flag}
        myStaleIndexNames: TStringIndexMap;

        myIDIndex: TIntegerIndexMap;
        myIDIndexIsStale: boolean;

        myLinkedModels: RbModelLinkList;
        myParent: RbModel;
        myMandatoryColums: TStringArray;
        myTheseColumns: TStringArray;
        myNotes: string; {Primarily to document the use of the model in context}
        myQryObj: TSQLQuery;
        myFreeQryObj: boolean;

        procedure clearIDIndex;
        procedure buildIDIndex;
        procedure setModelDef(const _ModelDef: RbModelDef);

        {Factory to create a new Row object. Override in specific model}
        function rowObj: RbRow; virtual;

        {override to do model specific things}
        procedure OnChange(_row: RbRow; _field: string); virtual;

        {if the field has been indexed then remember that the index is now stale}
        procedure noteIndexChange(_field: string);

        {has something changed since the last index?}
        function isIndexStale(const _name: string): boolean;
    public
        Name: string;
        property ModelDef: RbModelDef read myModelDef write setModelDef;

        {Setting this to true will omit the PK values from the list of column in the insert
        query, if it is present in the list of whichColumns. This way, the DB will automatically assign
        the Autoincrement PK. If it is set to false then the apoplication/developer must assign a valid PK}
        property letDBInsertIDField: boolean read myletDBInsertIDField write setletDBInsertIDField;

        constructor Create; overload; virtual;
        constructor Create(const _name: string); overload; virtual;
        constructor Create(const _def: RbModelDef); overload; virtual;
        destructor Destroy; override;

        {## NOTES ##}
        {Allows you to describe what this model currently contains. Use
        primarily for documentation and clarity of code}
        function notes(_notes: string): RbModel; // call with '' to clear notes
        function notes: string;

        {Add in-memory indices for the specified fields.
        You can send multiple fields as a delimited string.
        Before you use indices, you must rebuild them}
        procedure buildIndex(const _indexName: string = '#all');
        procedure addIndex(const _fields: string; const _delim: string = COMMA);
        function hasIndex(const _field: string): boolean;
        function indexCount: integer;

        function findID(const _id: RbRowID): RbRow; virtual;
        function findFirst(const _fieldName: string; const Value: string): RbRow; virtual;

        {returns a list object. You should free the list after use
        but the rows won't be freed}
        function find(const _fieldName: string; const Value: string): RbRows;

        {Return the integer list for this index}
        function fetchIndex(const _fieldName: string; const Value: string): TIntegerList;

        {Just get the rows pointed at by this integerList. You have to Free the returned Rows}
        function theseRowsObj(const _iList: TIntegerList): RbRows;

        {Column filter. This allows for model JSON to be sent only using
        a subset of the colums.}

        {Fetch only these columns. Don't populate the others}
        function filteredColumns: boolean; {Are columns filtered?}
        function theseColumns(const _thesecols: array of string): RbModel;
        function theseColumns(const _thesecols: string; _delim: string = ','): RbModel;

        {When you filter columns, then these functions returns those
        otherwise it returns the default set of columns as a comma separated list}
        function whichColumns: string;
        function getRequiredMemberNames: string;

        {Convenience functions to list columns from ModelDef }
        function listTheseColumns(_colTypes: DFieldTypes): string;
        function listPrimaryKeys: string;
        function listLockInfoColumns: string;
        function listForeignKeys: string;
        function listDataColumns: string;
        function listSystemColumns: string;
        function listCalculatedColumns: string;

        {clear the column filter}
        function clearTheseColumns: RbModel;

        function sort(const _field: string; const _order: DSortOrder = sortASC): RbRows;

        {Access to data}
        function rowCount: integer;
        function row(const _index: integer=0): RbRow; virtual; {returnst the first row by default}
        procedure clearRows;

        {instantiate and init}
        function newRowObj: RbRow;

        {assign a new id to a row}
        function assignNewID(_row: RbRow): RbRow;

        {instantiate, init and get new id}
        function newRow: RbRow; virtual;

        {access to list of rows}
        function rows: RbRows; virtual;

        {Return this model as a JSON Object}
        function JSONObj(_withLinks: boolean = False; {export linked models}
            _recursive: boolean = False {export links inside linked models}):
            TJSONObject;

        function asJSON: TJSONStringType;

        class function isJSONValid(_jsonObj: TJSONObject;
            const _strict: boolean = true): boolean;

        function loadJSON(const _jsonObj: TJSONObject; _strict: boolean = False): RbModel;
            overload;

        function loadJSON(const _jsonstr: TJSONStringType;
            _strict: boolean = False): RbModel; overload;


        {## PARENT ##}
        function parent: RbModel;
        function parent(_parent: RbModel): RbModel;

        {## MODEL LINKS ##}
        {manage model Links}
        function links: RbModelLinkList;
        function linkCount: integer;
        function linkName(const _i: integer): string;
        function getLink(const _name: string): RbModelLink; overload;
        function getLink(const _i: integer): RbModelLink; overload;
        function getLink(const _modelClass: RbModelClass): RbModelLink; overload;

        {convenience functions}
        function linkType(const _name: string): DModelLinkType;
        function linkModel(const _name: string): RbModel;

        {## ADDING LINKS ##}
        {Linked Models will be freed when this model is destroyed}

        {don't use}
        function addLink(const _modelReference: RbModelLink): RbModel; overload;

        {don't use}
        function addLink(_name: string; _sourceField: DField;
            _refType: DModelLinkType; _refmodel: RbModel;
            _refField: DField): RbModel; overload;

        {Use this to add links to a model}
        function addLink(_refType: DModelLinkType; _source: string;
        {name of the field from this model that is linked}
            _refmodel: string; _reffield: string = IDField): RbModel; overload;

        {## EXPORTING LINKS ##}

        {This query object gives }
        function useThisQueryObj(constref _qry: TSQLQuery; const _freeOnDestroy: boolean = true) : RbModel;

        {Returns the CREATE SQL statement for this model}
        function createSQL: string;

        {## QUERY BUILDERS ##}
        {preformated SQL statements}
        protected
        myQBReaderObj   : RbSelectQueryBuilder;
        myQBInserterObj : RbInsertQueryBuilder;
        myQBUpdaterObj  : RbUpdateQueryBuilder;
        myQBDeleterObj  : RbDeleteQueryBuilder;

        {Query strings}
        myReadSQL, myInsertSQL, myUpdateSQL, myDeleteSQL: string;
        myReadQry, myInsertQry, myUpdateQry, myDeleteQry: TSQLQuery;

        public
        {Query Builder Objects}
        function QBReader: RbSelectQueryBuilder;
        function QBInserter: RbInsertQueryBuilder;
        function QBUpdater(const _rowNum: integer = -1): RbUpdateQueryBuilder;
        function QBDeleter(const _rowNum: integer = -1): RbDeleteQueryBuilder;


        {Specify the queries that will be used for CRUD}
        {If these SQLs are specified then the model will use this and not the
        one that the QueryBuilder holds in}

        function useThisReadSQL(_sql: string): RbModel;
        function useThisInsertSQL(_sql: string): RbModel;
        function useThisUpdateSQL(_sql: string): RbModel;
        function useThisDeleteSQL(_sql: string): RbModel;
        function resetSQLStatements: RbModel;

        {Returns the current query strings}
        function readSQL: string;
        function insertSQL: string;
        function updateSQL: string;
        function deleteSQL: string;

        {This gives you access to the qry object
        so that you specify parameters before executing the query.
        Remember to FreeAndNil() after use.}
        function readQry:   TSQLQuery;
        function insertQry: TSQLQuery;
        function updateQry: TSQLQuery;
        function deleteQry: TSQLQuery;

        procedure freeReadQry;
        procedure freeInsertQry;
        procedure freeUpdateQry;
        procedure freeDeleteQry;
        procedure freeQueryObjects;

        function dbRowCount: integer;

        {DB Calls}
        procedure startTransaction;
        procedure commit;
        procedure rollback;
        procedure endTransaction;

        function assignQueryParameters(constref _qry: TSQLQuery; constref _row: RbRow; const _columns: string): TSQLQuery;
        function loadFromDB(const _sql: string): integer; overload; deprecated 'Use loadFromDB() after preparing the qry() object';
        function loadFromDB: integer; overload;
        function load (_pageNumber: integer = 0): integer;
        function qry: TSQLQuery;
        function qryClone: TSQLQuery;
        function executeSQL(_sql: string): integer;

        {## CAUTION
            If you are inserting, then make sure that you
            set the theseColumns() to the columns whose values you are going
            to insert. Otherwise it will try to insert all fields,
            including the RID.

            If you are updating, then for each row, the qry Object is set with
            the columns that were changed using store_xxx calls and by default
            attaches the RID }

        function saveToDB(_lock_name: string = ''; _unlock_key: string= '') : integer; {saves only if lock is valid}
        function delete(const _rowNum: integer=-1): boolean; {delete this row number}
        function delete(constref _row: RbRow): boolean; {delete this row, if not locked}

        {row - locking}

        {FOR LOCKING}
        protected
            function unlock(_row: RbRow; _unlock_key: string): boolean;
            procedure resetLock(_row: RbRow);

            {release the lock if it is held by _lockName}
        public
            function islockedBy(_row: RbRow; _lockName: string): boolean;

            {is the row locked for edit?. If the lock time has exceeded reset the lock}
            function isLocked(_row: RbRow): boolean;

            {If "_name" has locked this, then the lock will be reset}
            function relinquishLock(_row: RbRow; _name: string) : boolean;

            {LOCK() a row / document
              -> Returns the key if locked by the same _name.
              -> Returns empty if cannot lock.
              -> Returns new key if it can be locked}
            function lock(_row: RbRow; _lockName: string) : string;

            {cancel the lock - use the key for cancelling}
            function cancelLock(_row: RbRow; _key: string): TDbSaveStatus;

            {Is the row in Edit mode?}
            function canEdit: boolean; overload;

            {Can this _lockName user edit this row?}
            function canEdit(_lockName: string): boolean; overload;


    end;

    RbModelListBase = specialize GenericHashObjectList<RbModel>;

    { RbModelList }

    RbModelList = class(RbModelListBase)
        function add(const _model: RbModel): RbModelList; overload;
    end;

    generic RbGenericModel<gRow: RbRow> = class(RbModel)
    protected
        function rowObj: gRow; reintroduce;
    public
        function row(const _index: integer): gRow; reintroduce;
        function newRow: gRow; reintroduce;
        function findID(const _id: RbRowID): gRow; reintroduce;
        function findFirst(const _fieldName: string; const Value: string): gRow;
            reintroduce;
    end;

    { RbRow }
    RbRow = class
    private
        myOnChange: ProcRowChange;
		mywatchChanges: boolean;
        procedure setonChange(const _onChange: ProcRowChange);
		procedure setwatchChanges(const _watchChanges: boolean);

    protected
        myModelDef: RbModelDef;

    protected
        myColumns: TDynaJSONObject;
        myLockStatus: DLockType;
        myEditableFlag: boolean;
        myState: DRowState;
        myChangedFields: TDynaJSONObject;

        {initialize the row with fields according to the modelDef}
        function initializeColumns(_modelDef: RbModelDef): RbRow;

         {This set of methods allows us to know which fields were
         changed and to build the query with just those changes. Reduces data traffic}
        procedure change(const _name: string);
        procedure clearChanges;

        {Full Text Search functionality}
        function getFtsSearchText: string;
    public

        {if watchChanges is true, then every change to fields is recorded}
        property watchChanges: boolean read mywatchChanges write setwatchChanges;
        property onChange: ProcRowChange read myonChange write setonChange;
        function hasChanged: boolean;

        {Column information}
        {Get the name of the nth Field}
        function Name(const _index: integer): string;
        function Value(const _index: integer): TJSONData; overload;
        function Value(const _name: string): TJSONData; overload;

        {Get the description of the nth Field}
        function description(_index: integer): string;
        {Get the description for the field name}
        function description(_name: string): string;

        function hint(_index: integer): string;
        function hint(_name: string): string;

        function Caption(_index: integer): string;
        function Caption(_name: string): string;

        function comment(_index: integer): string;
        function comment(_name: string): string;

        function columns: TJSONObject;
        function columnCount: integer;
        function cloneColumns: TJSONObject;

        {Return only these columns}
        function theseColumnsObj(_columns: TStringArray): TJSONObject;
        function theseColumnsObj(_columns: string; _delim: string= COMMA): TJSONObject;

        {You should free the object that is returned from this}
        function changedColumnsObj: TJSONObject;
        function changedColumnCount: integer;
        function changedColumnNames: string;

        //property canEdit: boolean read myEditableFlag write myEditableFlag;
        function lockStatus: DLockType;

        {update columns from another TJSONObject. Preserve Data Schema}
        function copyColumns(_source, _dest: TJSONObject): boolean; overload;
        function copyColumns(_columns: TJSONObject): boolean; overload;
        function copyColumns(_row: RbRow): boolean; overload;


        {CRUD operations}
        function isNew:boolean;
        function new: RbRowID; virtual; {creates a new record and returns an id}
        function Read(_id: RbRowID): boolean; virtual; {refreshes the record}
        function edit: boolean; virtual;   {sets a lock for edit}

        {writes the data to the table. Returns the ID}
        function save(const _fields: string = ''; const _values: string = ''; const delimiter: string = ','): RbRowID; virtual;

        function Delete: boolean; virtual;   {delete the record}
        function commit: boolean; virtual;
        {commit the changes. not implemented right now}
        function rollback: boolean; virtual; {rollback changes. not implemented right now}


        {## DATA SETTERS ##}
        procedure store(const _name: string; const _value: integer); overload;
        procedure store(const _name: string; const _value: int64); overload;
        procedure store(const _name: string; const _value: QWord); overload;
        procedure store(const _name: string; const _value: boolean); overload;
        procedure store(const _name: string; const _value: TJSONFloat); overload;
        procedure store(const _name: string; const _value: TJSONStringType); overload;
        procedure storeUnicode(const _name: string; const _value: TJSONUnicodeStringType); overload;
        procedure storeJSON(const _name: string; const _value: TJSONArray); overload;
        procedure storeJSON(const _name: string; const _value: TJSONObject); overload;
        procedure store(const _name: string; const _value: TDateTime); overload;

        {## DATA GETTERS ##}
        {string value is returned by default}
        function value_str(const _name: string): TJSONStringType;
        function value_int(const _name: string): integer;
        function value_int64(const _name: string): int64;
        function value_qword(const _name: string): QWord;
        function value_bool(const _name: string): boolean;
        function value_float(const _name: string): TJSONFloat;
        function value_unicode(const _name: string): TJSONUnicodeStringType;
        function value_jsonarray(const _name: string): TJSONArray;
        function value_jsonobj(const _name: string): TJSONObject;
        function value_datetime(const _name: string): TDateTime;

        {Data value properties}
        property str_val    [_name: string]: TJSONStringType read value_str write store;
        property int_val    [_name: string]: integer read value_int write store;
        property int64_val  [_name: string]: int64 read value_int64 write store;
        property qword_val  [_name: string]: qword read value_qword write store;
        property bool_val   [_name: string]: boolean read value_bool write store;
        property float_val  [_name: string]: TJSONFloat read value_float write store;
        property datetime_val[_name: string]: TDateTime read value_datetime write store;
        property unicode_val[_name: string]: TJSONUnicodeStringType read value_unicode write storeUnicode;
        property jarray_val [_name: string]: TJSONArray read value_jsonarray write storeJSON;
        property jobj_val   [_name: string]: TJSONObject read value_jsonobj write storeJSON;

        property fts_value: string read getFtsSearchText;

    public {Columns}
        function rid: RbRowID; overload; virtual;
        function rid(const _id: RbRowID): RbRow; overload; virtual;

        function db_verision: longint; overload; virtual;
        function db_version(const _db_version: longint): RbRow; overload; virtual;

        function row_version: longint; overload; virtual;
        function row_version(const _row_version: longint): RbRow; overload; virtual;

        function lock_type: DLockType; overload; virtual;
        function lock_type(const _lockType: DLockType): RbRow; overload; virtual;

        function locked_at: TDateTime; overload; virtual;
        function locked_at(const _locked_at: TDateTime): RbRow; overload; virtual;

        function locked_by(_locked_by : string) : RbRow; overload;
        function locked_by : string; overload;

        function lock_name(_lock_name : string) : RbRow; overload;
        function lock_name : string; overload;

        function unlock_key(_unlock_key : string) : RbRow; overload;
        function unlock_key : string; overload;

    public
        constructor Create(const _modeldef: RbModelDef);
        destructor Destroy; override;
        function modelDef: RbModelDef;

    {FOR LOCKING}
    protected
        function unlock(_lock_name: string; _unlock_key: string): boolean;
        procedure resetLock;
        procedure relinquishLock; virtual;   {unlock the row}

        {release the lock if it is held by _lockName}
    public
        function islockedBy(_lockName: string; _unlock_key: string): boolean;

        {is the row locked for edit?. If the lock time has exceeded reset the lock}
        function isLocked: boolean;

        {If "_name" has locked this, then the lock will be reset}
        function relinquishLock(_name: string; _unlock_key: string) : boolean;

        {LOCK() a row / document
          -> Returns the key if locked by the same _name.
          -> Returns empty if cannot lock.
          -> Returns new key if it can be locked}
        function lock(_lockName: string) : string;

        {cancel the lock - use the key for cancelling}
        function cancelLock(_key: string): TDbSaveStatus;

        {Can this _lockName user edit this row?}
        function canEdit(_lockName: string): boolean;
    end;


    DReferenceType = (refNone, refOneToOne, refOneToMany, refManyToOne, refManyToMany);

    DReferenceDef = class
        referenceType: DReferenceType;
        sourceModel: RbModelDef;
        referenceModel: RbModelDef;
        joinSQL: string; {write the SQL statement that joins the two statements}
    end;

    DReferencesBase = specialize GenericHashObjectList<DReferenceDef>;

    DReferences = class(DReferencesBase)

    end;

{IDs and Sequences}
function getNewSequence(const _sequenceName: string): LongInt;

{Model registrations associated with a modelDef}
procedure registerModel(_modelDef: RbModelDef; _modelClass: RbModelClass);
function modelCount: integer;
function getRegisteredModelObj(_i: word): RbModel;

{NEWLY INSTANTIATED MODELS by model name}
function getModelObj(const _name: string): RbModel; overload;
function getModelObj(const model: RbModelDef): RbModel; overload;
function getModelObj(const _json: TJSONObject): RbModel; overload;

{MODEL CLASS}
function getModelClass(const _name: string): RbModelClass; overload;
function getModelClass(const _model: RbModelDef): RbModelClass; overload;

{MODEL DEFINITION THAT IS REGISTERED}
function getModelDef(const _name: string): RbModelDef;

{factory functions so that we can add default values into the objects being created}
function getddlDBObj(const _name: string): DDatabase;
function getSchema(constref _db: DDatabase; const _name: string): DSchema;
function getTable(constref _schema: DSchema; const _name: string): RbModelDef;

{PARAMETER HELPERS FOR QUERY BUILDER:: FACADE FOR QUERYBUILDER}
function quoted(const _value: string) : string;
function stripQuotes(const _value: string): string;
function sanitize(const _value: string): string;


procedure initSequenceGenerator(_seqGen: RbProcSequenceGenerator);
function defaultSequenceGenerator(_sequenceName: string): Longint; {assigned on unit load }

implementation

uses
    jsonparser, dateutils, sugar.logger;

type
    RbModelFactoryData = class
        def: RbModelDef;
        modelClass: RbModelClass;
    end;

    RbModelFactory = specialize GenericHashObjectList<RbModelFactoryData>;

var
  mySequences : TStringIndexMap;
  myIDSequence: QWord = 0;
  myProcSequenceGenerator: RbProcSequenceGenerator = nil;
  myModelFactory: RbModelFactory;


{Create a new model object from the supplied _factory data}
function createModelObj( const _factoryData: RbModelFactoryData): RbModel;
begin
    Result := nil;
    if Assigned(_factoryData) then
        if Assigned(_factoryData.modelClass) then
        begin
            Result := _factoryData.modelClass.Create(_factoryData.def);
		end;
end;

function getNewSequence(const _sequenceName: string): LongInt;
begin
    if Assigned(myProcSequenceGenerator) then
        Result:= myProcSequenceGenerator(_sequenceName)
    else
        Trip('getNewID(): sequence generator not assigned');
end;

procedure registerModel(_modelDef: RbModelDef; _modelClass: RbModelClass);
var
    _factoryData: RbModelFactoryData;
    i: integer;
begin
    _factoryData:= myModelFactory.find(_modelDef.Name);
    if not Assigned(_factoryData) then
    begin
        _factoryData:= myModelFactory.get(_modelDef.Name);
        _factoryData.def:= _modelDef;
        _factoryData.modelClass := _modelClass;
	end;
end;

function modelCount: integer;
begin
    Result:= myModelFactory.Count;
end;

function getRegisteredModelObj(_i: word): RbModel;
begin
    Result:= nil;
    if _i < modelCount then
        Result := createModelObj(myModelFactory.Items[_i]);
end;

function getModelObj(const _name: string): RbModel;
begin
    Result := createModelObj(myModelFactory.find(_name));
end;

function getModelObj(const model: RbModelDef): RbModel;
begin
    Result := getModelObj(model.Name);
end;

function getModelObj(const _json: TJSONObject): RbModel;
var
    _modelDef: RbModelDef;
begin
    Result:= nil;
    {STEP 1: verify that the _json Object is indeed an RbModel}
    if not RbModel.isJSONValid(_json) then
       trip('getModelObj() _json does not contain RbModel definition');

    {STEP 2: verify that the ModelDef is registered and can be instantiated }
    _modelDef:= getModelDef(_json.Strings['def']);
    if not Assigned(_modelDef) then
        trip('getModelObj(): ModelDef is not registered: ' + _json.Strings['def']);

    {STEP 3: create the required object from the Model Factory}
    Result := getModelObj(_modelDef);
    Result.loadJSON(_json);
end;

function getModelClass(const _name: string): RbModelClass;
var
    _factoryData: RbModelFactoryData;
begin
    Result := nil;
    _factoryData:= myModelFactory.find(_name);
    if Assigned(_factoryData) then
            Result := _factoryData.modelClass;
end;

function getModelClass(const _model: RbModelDef): RbModelClass;
begin
    Result := getModelClass(_model.Name);
end;

function getModelDef(const _name: string): RbModelDef;
var
    _factoryData: RbModelFactoryData;
begin
    Result := nil;
    _factoryData:= myModelFactory.find(_name);
    if Assigned(_factoryData) then
            Result := _factoryData.def;
end;

function getddlDBObj(const _name: string): DDatabase;
begin
    Result := DDatabase.Create(_name);
end;

function getSchema(constref _db: DDatabase; const _name: string): DSchema;
begin
    Result := _db.Schema(_name);
end;

function getTable(constref _schema: DSchema; const _name: string): RbModelDef;
var
    isNewTable: boolean;
begin
    isNewTable := _schema.find(_name) < 0;
    Result := _schema.Table(_name); {adds the table if not found}
    if isNewTable then
    begin
        {Add default fields here}
        with Result do
        begin
            {APP VERSION}
            Number('db_version').FieldType(ftLockInfo)
                .DefaultValue(DB_VERSION)
                .caption('DB Version')
                .Description('Version of the app that created this record. System updated');

            {ROW VERSION}
            Number('row_version').FieldType(ftLockInfo)
                .caption('Row Version')
                .Description('Version number of the record');

            {ROW LOCK}
            Number('lock_type').FieldType(ftLockInfo)
                 .caption('Lock Type')
                .Description(
                'Contains the lock information for the record.' +
                '0: lockNone - The row is not locked' +
                '1: lockRead - The row can be read. Similar to function as lockNone until business rule changes' +
                '2: lockReadForUpdate - The row has been read specifically for editing.' +
                '3: lockWrite - The row is now being written.'
                );

            {ROW LOCK TIME}
            Timestamp('locked_at').FieldType(ftLockInfo)
                .caption('Locked At')
                .Description('The time at which the record was locked');

            {ROW LOCK NAME}
            shortText('lock_name').FieldType(ftLockInfo)
                .caption('Lock Name')
                .description('A short code that identifies the locker');

            {ROW UNLOCK KEY}
            shortText('unlock_key').FieldType(ftLockInfo)
                .caption('Unlock Key')
                .description('The key that is needed to unlock this row');

        end;
    end;
end;

function quoted(const _value: string): string;
begin
    // Result:= querybuilder.quoted(_value);
    Result:= QuotedStr(_value);
end;

function stripQuotes(const _value: string): string;
begin
    Result:= sugar.querybuilder.stripQuotes(_value);
end;

function sanitize(const _value: string): string;
begin
    Result:= sugar.querybuilder.sanitize(_value);
end;

procedure initSequenceGenerator(_seqGen: RbProcSequenceGenerator);
begin
    myProcSequenceGenerator:= _seqGen;
end;

{ RbModelLink }

function RbModelLink.jsonObj(_withlinks: boolean; _recursive: boolean): TJSONObject;
begin
    Result := TJSONObject.Create;
    with Result do
    begin
        Add('gen', 'RbModelLink');
        Add('name', Name);
        Add('owner', owner.ModelDef.Name);
        Add('src_fld', sourceField.Name);
        Add('link_type', Ord(linkType));
        Add('link_fld', linkedField.Name);
        Add('link_md', linkedModel.JSONObj(_withlinks, _recursive));
    end;
end;

function RbModelLink.asJSON: string;
var
    j: TJSONObject;
begin
    j := jsonObj;
    Result := j.AsJSON;
    j.Free;
end;

function RbModelLink.loadJSON(_json: TJSONStringType): boolean;
begin
    trip('RbModelLink.loadJSON not implemented');
end;

{ RbModelLinkList }

function RbModelLinkList.asJSONArray(_withlinks: boolean;
    _recursive: boolean): TJSONArray;
var
    i: integer;
begin
    Result := TJSONArray.Create;
    for i := 0 to pred(Count) do
        Result.Add(Items[i].jsonObj(_withlinks, _recursive));
end;

function RbModelLinkList.asJSON: string;
var
    _jarr: TJSONArray;
begin
    _jarr := asJSONArray;
    Result := _jarr.AsJSON;
    _jarr.Free;
end;

function RbModelLinkList.loadJSON(_json: TJSONStringType): boolean;
begin
    trip('RbModelLinkList.loadJSON not implemented');
end;

{ RbRows }


function RbRows.jsonArray: TJSONArray;
var
    i: integer;
begin
    Result := TJSONArray.Create;
    for i := 0 to pred(Count) do
        Result.Add(Items[i].columns.Clone);
end;

function RbRows.jsonArrayj(_theseCols: TStringArray): TJSONArray;
var
    i: integer;
begin
    Result := TJSONArray.Create;
    for i := 0 to pred(Count) do
        if Length(_theseCols) > 0 then
            Result.Add(Items[i].theseColumnsObj(_theseCols)); {cloned copies}

end;

function RbRows.delete(_obj: RbRow): boolean;
var
    i: integer;
begin
    i := IndexOf(_obj);
    Result := (i>-1) and (i < count);
    if Result then
    begin
        inherited delete(i);
	end;
end;

{ RbModelList }

function RbModelList.add(const _model: RbModel): RbModelList;
begin
    Result := self;
    inherited add(_model.Name, _model);
end;

{ RbModel }

function RbGenericModel.rowObj: gRow;
begin
    Result := gRow.Create(ModelDef);
end;

function RbGenericModel.row(const _index: integer): gRow;
begin
    Result := gRow(inherited row(_index));
end;

function RbGenericModel.newRow: gRow;
begin
    Result := gRow(inherited newRow);
end;

function RbGenericModel.findID(const _id: RbRowID): gRow;
begin
    Result := gRow(inherited findID(_id));
end;

function RbGenericModel.findFirst(const _fieldName: string; const Value: string): gRow;
begin
    Result := gRow(inherited findFirst(_fieldName, Value));
end;

{ RbModel.gRows }
procedure RbModel.setModelDef(const _ModelDef: RbModelDef);
begin
    if myModelDef = _ModelDef then
        Exit;
    myModelDef := _ModelDef;
end;

procedure RbModel.setletDBInsertIDField(const _letDBInsertPK: boolean);
begin
	if myletDBInsertIDField=_letDBInsertPK then Exit;
	myletDBInsertIDField:=_letDBInsertPK;
end;

procedure RbModel.clearIDIndex;
begin
    myIDIndex.Free;
    myIDIndex := TIntegerIndexMap.Create;
end;

{returns the number if indices available}
procedure RbModel.buildIndex(const _indexName: string);
var
    _index: TIntegerListMap;
    _i: integer;
begin
    if _indexName.isEmpty then
        exit;

    if _indexName = '#all' then
    begin
        {For each addIndex in the list}
        for _i := 0 to pred(myIndices.Count) do
            buildIndex(myIndices.Names[_i]);//recursive call
    end
    else


        // Control comes here when _indexName is available
    begin
        {_indexName is really the fieldName}
        _index := myIndices.find(_indexName);
        _index.Clear;
        for _i := 0 to Pred(myRows.Count) do // loop through the rows
        begin
            {map the field value to the list position}
            //_index.listOf[myRows.Items[_i].columns.Strings[_indexName]].Add(_i);
            _index.listOf[row(_i).str_val[_indexName]].Add(_i);
        end;
        myStaleIndexNames.idx[_indexName] := 0; {Not stale anymore}
    end;

end;

procedure RbModel.buildIDIndex;
var
    i: integer;
begin
    if myIDIndexIsStale then
    begin
		clearIDIndex;
		{map the ID value of the row to the list addIndex }
		for i := 0 to pred(myRows.Count) do
		    myIDIndex.idx[myRows.Items[i].rid] := i;

        myIDIndexIsStale := false;
	end;
end;

procedure RbModel.addIndex(const _fields: string; const _delim: string);
var
    _def: RbModelDef;
    _field: DField;
    _fieldName: string;
begin
    _def := modelDef; // so that I can use code completion

    {for each field in the list}
    for _fieldName in toStringArray(_fields, _delim) do
    begin
        _field := _def.field(Trim(_fieldName));
        {if the field actually exists}
        if Assigned(_field) then
        begin
            {if the index is not yet assigned}
            if not Assigned(myIndices.find(Trim(_fieldName))) then
            begin
                myIndices.add(Trim(_fieldName), TIntegerListMap.Create);
            end;
        end;
    end;
end;

function RbModel.hasIndex(const _field: string): boolean;
begin
    Result := Assigned(myIndices.find(_field));
end;

function RbModel.indexCount: integer;
begin
    Result := myIndices.Count;
end;

function RbModel.findID(const _id: RbRowID): RbRow;
var
    i: integer;
begin
    Result := nil;
    buildIDIndex; {builds it if it is stale}
    i := myIDIndex.idx[_id];
    if i <> NOINDEX then
        Result := myRows.Items[i];
end;

function RbModel.theseRowsObj(const _iList: TIntegerList): RbRows;
var
    i: integer;
begin
    Result := RbRows.Create(False); {don't delete objects when freed}
    if assigned(_ilist) then
    begin
        for i := 0 to pred(_iList.Count) do
            Result.Add(myRows.Items[_iList.Items[i]]);
    end;
end;

function RbModel.filteredColumns: boolean;
begin
    Result := Length(myTheseColumns) > 0;
end;

function RbModel.theseColumns(const _thesecols: array of string): RbModel;
begin
    copyStringArray(_thesecols, myTheseColumns);
    Result := Self;
end;

function RbModel.theseColumns(const _thesecols: string; _delim: string): RbModel;
begin
    Result := theseColumns(toStringArray(_thesecols, _delim));
end;

function RbModel.whichColumns: string;
begin
    if filteredColumns then
        Result := getDelimitedString(myTheseColumns)
    else
        Result := ModelDef.memberNames;
end;

function RbModel.clearTheseColumns: RbModel;
begin
    setLength(myTheseColumns, 0);
    Result := self;
end;

function RbModel.fetchIndex(const _fieldName: string; const Value: string): TIntegerList;
var
    _index: TIntegerListMap;
begin
    Result := nil;
    {Is there an addIndex for the field?}
    {Finds the row only if the addIndex is available }
    _index := myIndices.find(_fieldName);
    if Assigned(_index) then
    begin
        if isIndexStale(_fieldName) then
            buildIndex(_fieldName);
        Result := _index.listOf[Value]; {Return the row list}
    end;
end;

function RbModel.findFirst(const _fieldName: string; const Value: string): RbRow;
var
    _list: TIntegerList;
begin
    Result := nil;
    _list := fetchIndex(_fieldName, Value);
    if assigned(_list) then
        if _list.Count > 0 then
            Result := myRows.Items[_list.Items[0]];
end;

function RbModel.find(const _fieldName: string; const Value: string): RbRows;
begin
    Result := theseRowsObj(fetchIndex(_fieldName, Value));
end;


{ RbModel }

constructor RbModel.Create;
begin
    inherited;
    myRows := RbRows.Create;
    myIndices := RbIndexList.Create;
    myIDIndex := TIntegerIndexMap.Create;
    myLinkedModels := RbModelLinkList.Create; {linked models are not destroyed}
    myStaleIndexNames := TStringIndexMap.Create;
    myIDIndexIsStale:=True;
    myletDBInsertIDField:= True; {see letDBInsertIDField}
end;

constructor RbModel.Create(const _name: string);
begin
    Self.Create;
    Name := _name;
end;

constructor RbModel.Create(const _def: RbModelDef);
begin
    Self.Create(_def.Name);
    ModelDef := _def;
end;

destructor RbModel.Destroy;
var
    i: integer;
    _link: RbModelLink;
begin
    myStaleIndexNames.Free;
    myIndices.Free;
    myIDIndex.Free;
    myRows.Free;

    {free the models linked to this model}
    for i := 0 to pred(myLinkedModels.Count) do
    begin
        _link := myLinkedModels.Items[i];
        _link.linkedModel.Free;
    end;

    myLinkedModels.Free;

    if myFreeQryObj then
        myQryObj.Free;

    myQBReaderObj.Free;
    myQBInserterObj.Free;
    myQBUpdaterObj.Free;
    myQBDeleterObj.Free;
    freeQueryObjects;
    inherited Destroy;
end;

function RbModel.notes(_notes: string): RbModel;
begin
    Result := Self;
    if _notes= '' then
        myNotes:= '' {clear}
    else
        {Append to the notes}
        myNotes += _notes + sLineBreak;

end;

function RbModel.notes: string;
begin
    Result := myNotes;
end;

function RbModel.rowCount: integer;
begin
    Result := myRows.Count;
end;

function RbModel.row(const _index: integer): RbRow;
begin
    Result := nil;
    if (_index >= 0) and (_index < rowCount) then
        Result := myRows.Items[_index];
end;

procedure RbModel.clearRows;
begin
    myRows.Clear;
end;

function RbModel.rowObj: RbRow;
begin
    Result := RbRow.Create(ModelDef);
end;

procedure RbModel.OnChange(_row: RbRow; _field: string);
begin
    noteIndexChange(_field);
end;

procedure RbModel.noteIndexChange(_field: string);
begin

    {Allows the model to know that something changed in the index}
    if hasIndex(_field) then
    begin
        if not isIndexStale(_field) then
            myStaleIndexNames.idx[_field] := 1;
	end
    else {Check the the IDIndex is stale}
    if _field=IDField then
    begin
        if not myIDIndexIsStale then
            myIDIndexIsStale:= true;
	end;
end;

function RbModel.isIndexStale(const _name: string): boolean;
begin
    Result := False;
    if myStaleIndexNames.idx[_name] = 1 then
        Result := True;
end;

{Instantiates and inits a new row object. Used when
filling a model from JSON or from the Database.
ID sequence is not part of this}
function RbModel.newRowObj: RbRow;
begin
    Result := rowObj;
    Result.onChange := @OnChange;
    myIDIndex.idx[Result.columns.Integers[IDField]] := rowCount;
    myRows.Add(Result);
end;

function RbModel.assignNewID(_row: RbRow): RbRow;
begin
    Result := _row;
    Result.rid(NEWID);
    //Result.rid(getNewSequence(Name));
end;

{Called to insert a new Row. This creates a new ID}
function RbModel.newRow: RbRow;
begin
    Result := AssignNewID(newRowObj);
end;

function RbModel.rows: RbRows;
begin
    Result := myRows;
end;

function RbModel.asJSON: TJSONStringType;
var
    j: TJSONObject;
begin
    j := JSONObj;
    Result := j.AsJSON;
    j.Free;
end;

class function RbModel.isJSONValid(_jsonObj: TJSONObject; const _strict: boolean): boolean;
begin
    Result:= True;
    if _strict then
    begin
        if not (_jsonObj.Strings['model'] = ClassName) then
        begin
            Result:= False;
            log3(Format('RbModel.loadJSON: Strict check. Model mismatch. ' +
                sLinebreak + 'Model is: %s but JSONObject says  %s',
                [Self.ClassName, _jsonObj.Strings['model']]));
        end;

    end
    else
    if not (_jsonObj.Strings[JSON_MODELSIGNATURE_KEY] = JSON_MODELSIGNATURE_VALUE) then
    begin
        Result:= False;
        log3('RbModel.loadJSON: Not strict. Model mismatch.');
	end;
end;

function RbModel.JSONObj(_withLinks: boolean; _recursive: boolean): TJSONObject;
begin
    Result := TJSONObject.Create;
    with Result do
    begin
        {Exported data of the model}
        {generic signature for non-strict checking}
        Add(JSON_MODELSIGNATURE_KEY, JSON_MODELSIGNATURE_VALUE);
        Add('model', Self.ClassName);{Classname of the model}
        Add('def', ModelDef.Name);   {Name of the model definition}
        Add('ver', MODEL_VERSION);   {Model version}
        Add('notes', myNotes);       {Description of what this model contains}
        Add('tz', htmlDateTime);     {Timestamp}
        Add('fields', getRequiredMemberNames);{Lists the fields that have data}

        {ROWS}
        if filteredColumns then
            Add('rows', rows.jsonArrayj(myTheseColumns)) {Rows with filtered columns}
        else
            Add('rows', rows.jsonArray);  {Array of rows with all columns}

        {LINKS}
        if (linkCount > 0) and (_withLinks) then
            Add('links', myLinkedModels.asJSONArray(_recursive));
    end;
end;

{The caller must explicitly destroy _jsonObj}
function RbModel.loadJSON(const _jsonObj: TJSONObject; _strict: boolean): RbModel;
var
    i: integer;
    jrows: TJSONArray;
    _theseFields: string;
begin
    {STEP1: Validate the model is correct}
    if not isJSONValid(_jsonObj) then
        trip('RbModel.loadJSON() _jsonObj is not RbModel compatible');

    {STEP2: Check if the model definition in the JSONObject matches the current object}
    if not (_jsonObj.Strings['def'] = ModelDef.Name) then
        trip(Format('RbModel.loadJSON: ModelDef mismatch. ' +
            sLinebreak + 'ModelDef is: %s but JSONObject says  %s',
            [ModelDef.ClassName, _jsonObj.Strings['def']]));

    {STEP3: Load the data}
    theseColumns(_jsonObj.Strings['fields']);

    jrows := _jsonObj.Arrays['rows'];
    for i := 0 to pred(jrows.Count) do
        copyJSONObject(TJSONObject(jrows.Items[i]), newRow().columns);

    {DONE}
    Result := Self;
end;

function RbModel.loadJSON(const _jsonstr: TJSONStringType; _strict: boolean): RbModel;
var
    _jdata: TJSONData;
begin
    Result := self;
    _jdata := GetJSON(_jsonstr);
    if (_jdata.JSONType = jtObject) then
        Result := loadJSON(TJSONObject(_jdata), _strict)
    else
        trip('RbModel.loadJSON: The JSON string supplied is not a JSON Object');
    _jdata.Free;
end;

function RbModel.parent: RbModel;
begin
    Result := myParent;
end;

function RbModel.parent(_parent: RbModel): RbModel;
begin
    myParent := _parent;
    Result := Self;
end;

function RbModel.links: RbModelLinkList;
begin
    Result := myLinkedModels;
end;

function RbModel.linkCount: integer;
begin
    Result := myLinkedModels.Count;
end;

function RbModel.linkName(const _i: integer): string;
begin
    Result := myLinkedModels.Names[_i];
end;

function RbModel.getLink(const _name: string): RbModelLink;
begin
    Result := myLinkedModels.find(_name);
end;

function RbModel.getLink(const _i: integer): RbModelLink;
begin
    Result := myLinkedModels.Items[_i];
end;

function RbModel.getLink(const _modelClass: RbModelClass): RbModelLink;
begin
    Result := getLink(_modelClass.ClassName);
end;

function RbModel.linkType(const _name: string): DModelLinkType;
begin
    Result := DModelLinkType.LinkNotCreated;
    if Assigned(getLink(_name)) then
        Result := getLink(_name).linkType;
end;

function RbModel.linkModel(const _name: string): RbModel;
begin
    Result := nil;
    if Assigned(getLink(_name)) then
        Result := getLink(_name).linkedModel;
end;

function RbModel.addLink(const _modelReference: RbModelLink): RbModel;
begin
    myLinkedModels.add(_modelReference.Name, _modelReference);
    Result := self;
end;

function RbModel.addLink(_name: string; _sourceField: DField;
    _refType: DModelLinkType; _refmodel: RbModel; _refField: DField): RbModel;
begin
    with myLinkedModels.get(_name) as RbModelLink do
    begin
        Name := _name;
        owner := self;
        sourceField := _sourceField;
        linkType := _refType;
        linkedModel := _refModel;
        linkedField := _refField;
    end;
    _refModel.parent(self);
    Result := self;
end;

{Uses the model def's name name as the name of the link}
function RbModel.addLink(_refType: DModelLinkType; _source: string;
    _refmodel: string; _reffield: string): RbModel;
var
    _refmodelObj: RbModel;
    _sourceFieldObj: DField;
    _reffieldObj: DField;
begin
    Result := nil;
    {REFERENCED MODEL}
    _refmodelObj := getModelObj(_refmodel);

    {Instantiates the linked Model using the modeldef.Name}
    if not Assigned(_refmodelObj) then
    begin
        log3('RbModel.addLink: _refmodelObj was not instantiated: ' + _refmodel);
        trip('RbModel.addLink: _refmodelObj was not instantiated: ' + _refmodel);
        exit;
    end;

    {SOURCE FIELD}
    _sourceFieldObj := ModelDef.field(_source);
    if not Assigned(_sourceFieldObj) then
    begin
        log3('RbModel.addLink: _sourceFieldObj was not instantiated: ' + _source);
        trip('RbModel.addLink: _sourceFieldObj was not instantiated: ' + _source);
        exit;
    end;

    {REFERENCED FIELD}
    _reffieldObj := _refmodelObj.ModelDef.field(_reffield);
    if not Assigned(_reffieldObj) then
    begin
        log3('RbModel.addLink: _reffieldObj was not instantiated: ' + _reffield);
        trip('RbModel.addLink: _reffieldObj was not instantiated: ' + _reffield);
        exit;
    end;

    {NOW ADD THE LINK}
    Result := addLink(_refmodel, _sourceFieldObj,
        {auto freed when model is freed}
        _refType, _refModelObj, {free on RbModel.Destroy}
        _refFieldObj {auto freed when the model is freed}
        );
end;

function RbModel.createSQL: string;
begin
    Result := ModelDef.getCreateSQL();
end;

function RbModel.sort(const _field: string; const _order: DSortOrder): RbRows;
begin
    Result := myRows;
end;

function RbModel.getRequiredMemberNames: string;
begin
    Result:= whichColumns;
end;

function RbModel.listTheseColumns(_colTypes: DFieldTypes): string;
begin
    Result := modelDef.memberNames(_colTypes);
end;

function RbModel.listPrimaryKeys: string;
begin
    Result := modelDef.memberNames([ftPrimaryKey]);
end;

function RbModel.listLockInfoColumns: string;
begin
    Result := modelDef.memberNames([ftLockInfo]);
end;

function RbModel.listForeignKeys: string;
begin
    Result := modelDef.memberNames([ftForeignKey]);
end;

function RbModel.listDataColumns: string;
begin
    Result := modelDef.memberNames([ftData]);
end;

function RbModel.listSystemColumns: string;
begin
    Result := modelDef.memberNames([ftSystemValue]);
end;

function RbModel.listCalculatedColumns: string;
begin
    Result := modelDef.memberNames([ftCalculated]);
end;

function RbModel.QBReader: RbSelectQueryBuilder;
begin
    if not Assigned(myQBReaderObj) then
    begin
        myQBReaderObj := RbSelectQueryBuilder.Create;
        with myQBReaderObj do
        begin
            tables(ModelDef.Name);
            columns(getRequiredMemberNames);
            limit();
        end;
	end;
    Result:= myQBReaderObj;
end;

function RbModel.getColumnListWithoutIDField(_columns: string): string;
var
    _col: string;
    _bAddComma: boolean = false;
begin
    Result:= '';
    {Omit the ID field}
    for _col in toStringArray(_columns) do
    begin
        if (LowerCase(_col) = IDField) then continue; {ignore IDField}

        if _bAddComma then
            Result:= Result + COMMA
        else
            _bAddComma:= true;

        Result:= Result + _col;
	end;

end;

function RbModel.QBInserter: RbInsertQueryBuilder;
var
    _colList: string= '';
begin
    if not Assigned(myQBInserterObj) then
    begin
    	myQBInserterObj := RbInsertQueryBuilder.Create;
        with myQBInserterObj do
        begin
            tables(ModelDef.Name);

            if letDBInsertIDField then
                _colList:= getColumnListWithoutIDField(whichColumns)
            else
                _colList:= whichColumns;

			columns(_colList);
        end;
	end;
    Result:= myQBInserterObj;
end;


function RbModel.QBUpdater(const _rowNum: integer): RbUpdateQueryBuilder;
var
    i: integer;
    _changedColumns: TJSONObject;
    _row: RbRow;
begin
    if not Assigned(myQBUpdaterObj) then
    begin
	    myQBUpdaterObj := RbUpdateQueryBuilder.Create;
	    with myQBUpdaterObj do
	    begin
	        tables(ModelDef.Name);
            columns(modelDef.memberNames([ftForeignKey, ftData]));
            where(Format('%0:s = :%0:s', [IDField]));
		end;
	end;
    if (_rowNum>-1) and (_rowNum < rowCount) then
    begin
        _row := row(_rowNum);
    	if _row.hasChanged then
    	begin
    	    _changedColumns := _row.changedColumnsObj;
    	    myQBUpdaterObj.columns(getDelimitedKeys(_changedColumns));
    	    _changedColumns.Free;
    	end;
        myQBUpdaterObj.where(''); {clear the where clause}
    	myQBUpdaterObj.whereID(_row.rid);
    end;
	Result:= myQBUpdaterObj;
end;

function RbModel.QBDeleter(const _rowNum: integer): RbDeleteQueryBuilder;
begin
    if not assigned(myQBDeleterObj) then
    begin
	    myQBDeleterObj := RbDeleteQueryBuilder.Create;
	    with myQBDeleterObj do
	    begin
	        tables(ModelDef.Name);
            where(Format('%0:s = :%0:s', [IDField]));
	    end;
	end;

    if (_rowNum > -1) and (_rowNum < rowCount) then
    begin
        myQBDeleterObj.where(''); {Clear the where}
        myQBDeleterObj.whereID(row(_rowNum).rid)
	end;

    Result:= myQBDeleterObj;
end;

function RbModel.useThisQueryObj(constref _qry: TSQLQuery;
	const _freeOnDestroy: boolean): RbModel;
begin
    myQryObj := _qry;
    myFreeQryObj:= _freeOnDestroy;
    Result:= self;
    freeQueryObjects; {So that the next time they are used, it takes this new query Object}
end;

function RbModel.useThisReadSQL(_sql: string): RbModel;
begin
    myReadSQL:= _sql;
    Result:= self;
end;

function RbModel.useThisInsertSQL(_sql: string): RbModel;
begin
    myInsertSQL:= _sql;
    Result:= self;

end;

function RbModel.useThisUpdateSQL(_sql: string): RbModel;
begin
    myUpdateSQL:= _sql;
    Result:= self;
end;

function RbModel.useThisDeleteSQL(_sql: string): RbModel;
begin
    myDeleteSQL:= _sql;
    Result:= self;
end;

function RbModel.resetSQLStatements: RbModel;
begin
    myReadSQL:= '';
    myInsertSQL:= '';
    myUpdateSQL:= '';
    myDeleteSQL:= '';
end;

function RbModel.readSQL: string;
begin
    if myReadSQL.isEmpty then
        Result:= QBReader.sql
    else
        Result:= myReadSQL;
end;

function RbModel.insertSQL: string;
begin
    if myInsertSQL.isEmpty then
        Result:= QBInserter.sql
    else
        Result:= myInsertSQL;
end;

function RbModel.updateSQL: string;
begin
    if myUpdateSQL.isEmpty then
        Result:= QBUpdater.sql
    else
        Result:= myUpdateSQL;
end;

function RbModel.deleteSQL: string;
begin
    if myDeleteSQL.isEmpty then
        Result:= QBDeleter.sql
    else
        Result:= myDeleteSQL;
end;

function RbModel.readQry: TSQLQuery;
begin
    if not Assigned(myReadQry) then
    begin
        myReadQry:= qryClone;
        myReadQry.SQL.Text:= readSQL;
	end;
    Result:= myReadQry;
end;

function RbModel.insertQry: TSQLQuery;
begin
    if not Assigned(myInsertQry) then
    begin
        myInsertQry:= qryClone;
        myInsertQry.SQL.Text:= insertSQL;
	end;
    Result:= myInsertQry;
end;

function RbModel.updateQry: TSQLQuery;
begin
    if not Assigned(myUpdateQry) then
    begin
        myUpdateQry:= qryClone;
        myUpdateQry.SQL.Text:= updateSQL;
	end;
    Result:= myUpdateQry;
end;

function RbModel.deleteQry: TSQLQuery;
begin
    if not Assigned(myDeleteQry) then
    begin
        myDeleteQry:= qryClone;
        myDeleteQry.SQL.Text:= deleteSQL;
	end;
    Result:= myDeleteQry;
end;

procedure RbModel.freeReadQry;
begin
    FreeAndNil(myReadQry);
end;

procedure RbModel.freeInsertQry;
begin
    FreeAndNil(myInsertQry);
end;

procedure RbModel.freeUpdateQry;
begin
    FreeAndNil(myUpdateQry);
end;

procedure RbModel.freeDeleteQry;
begin
    FreeAndNil(myDeleteQry);
end;

procedure RbModel.freeQueryObjects;
begin
    freeReadQry;
    freeInsertQry;
    freeUpdateQry;
    freeDeleteQry;
end;

function RbModel.dbRowCount: integer;
var
    _q: TSQLQuery;
begin
    _q:= qryClone;
    _q.sql.text := QBReader.sqlRowCount;
    _q.Open;
    if _q.RecordCount = 1 then
    begin
        _q.First;
        Result:= _q.Fields[0].AsInteger; {Because this query returns only one field}
	end;
    _q.free;
end;

procedure RbModel.startTransaction;
begin
    if not qry.SQLConnection.Transaction.Active  then
            qry.SQLConnection.StartTransaction;
end;

procedure RbModel.commit;
begin
    if qry.SQLConnection.Transaction.Active then
            qry.SQLTransaction.Commit;
end;

procedure RbModel.rollback;
begin
    if qry.SQLConnection.Transaction.Active then
        qry.SQLTransaction.Rollback;
end;

procedure RbModel.endTransaction;
begin
    if qry.SQLConnection.Transaction.Active then
        qry.SQLConnection.EndTransaction;
end;

function RbModel.loadFromDB(const _sql: string): integer;
begin
    qry.SQL.text:= _sql;
    Result:= loadFromDB;
end;

function RbModel.loadFromDB: integer;
type
    QryFieldsHashList = specialize TFPGMapObject <string, TField>;
var
    _fieldName: string;
    _fieldDef: DField;
    _qryField: TField;
    _json: TJSONData;
    _fieldsHashList: QryFieldsHashList;
    i: integer;
begin
    Result:= -1;

    {CHECK THE QUERY}
    if not Assigned(myQryObj) then
    begin
        log('myQryObj is not assigned RbModel->' + Name);
        exit;
    end;

    try
        _fieldsHashList:= QryFieldsHashList.Create(false);

		{FETCH ROWS}
	    try
            clearRows;

            readQry.UniDirectional := True;
		    readQry.Open;

            {Create an in-memory index}
            for i:= 0 to pred(readQry.Fields.Count) do
            begin
                _qryField:=readQry.Fields[i];
                _fieldsHashList.add(_qryField.FieldDef.Name, _qryField);

			end;


		    while not readQry.EOF do
		    begin
		        with newRow do
		        begin
	                watchChanges:= False; {disable watching because you are loading all fields}

	                for i:= 0 to pred(_fieldsHashList.Count) do
	                begin
                        _fieldName:= _fieldsHashList.Keys[i];

                        {Check if the qry field is listed in the model def.
                         Ignore if it is not                           }
                        {----------------------------------------------}
                        if modelDef.find(_fieldName) = -1 then continue;
                        {----------------------------------------------}

                        _fieldDef := modelDef.field(_fieldName);

	                    {WIDE STRING}
	                    if _fieldDef.isWideString then
	                        storeUnicode(_fieldName, _fieldsHashList.KeyData[_fieldName].AsWideString)

	                    {TEXT FIELD}
	                    else if _fieldDef.isText then
	                        store(_fieldName, _fieldsHashList.KeyData[_fieldName].AsString)

	                    {NUMBER FIELD}
	                    else if _fieldDef.isNumber then
	                    begin
	                        {FLOAT}
	                        if _fieldDef.isFloat then
	                            store(_fieldName, _fieldsHashList.KeyData[_fieldName].AsFloat)
	                        else
	                        {INTEGER}
	                            store(_fieldName, _fieldsHashList.KeyData[_fieldName].AsInteger);
						end

	                    {DATE FIELD}
	                    else if _fieldDef.isDate then
	                        store(_fieldName, _fieldsHashList.KeyData[_fieldName].AsString)

	                    {BOOLEAN FIELD}
	                    else if _fieldDef.isBoolean then
	                        store(_fieldName, _fieldsHashList.KeyData[_fieldName].AsBoolean)

                        {JSON FIELD}
	                    else if _fieldDef.isJSON then
	                    begin
	                        _json:= GetJSON(_fieldsHashList.KeyData[_fieldName].AsString);
	                        if Assigned(_json) then
	                        begin
	                            if _json.JSONType = jtArray then
	                                storeJSON(_fieldName, TJSONArray(_json))
	                            else if _json.JSONType = jtObject then
	                                storeJSON(_fieldName, TJSONObject(_json));
							end;
						end
						else
	                    {SOMETHING IS WRONG}
	                        log(Format('RbModel.loadFromDB (%s): Field %s was not stored. ' + sLinebreak +
	                        'Datatype ord=%d not implemented ',[self.Name, _fieldName, ord(_fieldDef.FieldType)]));
					end;

	                {Now you enable watching for changes or the row will not record changed fields}
	                watchChanges:= True;
		        end;
		        readQry.Next;
		    end;
		    readQry.Close;
	        if not myIDIndexIsStale then myIDIndexIsStale := true;

		except
		    on E: Exception do
		    begin
		        log('RbModel.loadFromDB: ' + Name +
	            ' EXCEPTION:' + E.Message);
		    end;
	    end;

	finally
        _fieldsHashList.Free;
    	freeReadQry;
	end;
    Result:= rowCount;
end;

function RbModel.load(_pageNumber: integer): integer;
begin
    with QBReader do selectAll;
    log3('RbModel> Loading page with ' + readQry.SQL.Text);
    Result:= loadFromDB;
end;


function RbModel.qry: TSQLQuery;
begin
    Result:= myQryObj;
    if not Assigned(Result) then
        raise Exception.Create('RbModel.qry: You have not assigned the Query Object yet');
end;

function RbModel.qryClone: TSQLQuery;
begin
    Result:= TSQLQuery.Create(nil);
    Result.Database:= qry.DataBase;
    Result.SQLTransaction := qry.SQLTransaction;
end;

function RbModel.executeSQL(_sql: string): integer;
var
    _qry: TSQLQuery;
begin
    Result:= 0;
    _qry:= qryClone;
    try
	    with _qry do
	    begin
            SQL.Add(_sql);
            try
                ExecSQL;
            except
                on E: Exception do
                begin
                    log3('RbModel.executeSQL(): ' + E.Message);
                    Result:= -1;
                    exit;
			    end;
			end;
		end;
	finally
        _qry.Free;
	end;
end;

{SETS THE QUERY PARAMETERS
For each column name in the _columns parameter (delimited string), assign the
corresponding SQL parameter using the correct datatype and data contained in
the row. After the call, the _qry returned object will be have the parameters set }
function RbModel.assignQueryParameters(constref _qry: TSQLQuery; constref _row: RbRow; const _columns: string): TSQLQuery;
var
  _column: string;
  _columnIndex: TDynamicKeyValueStore;
  i: integer;
  _param: TParam;
begin

    Result:= _qry;
	if Result.SQL.Text.IsEmpty then
	  trip(Name + '->RbModel.saveToDB() called without setting SQL command');

    {Create a lookup index}
    _columnIndex:= TDynamicKeyValueStore.Create;
    for _column in toStringArray(_columns) do
        _columnIndex.put(_column, _column);

    try
		//for _column in toStringArray(_columns) do
		for i:= 0 to pred(Result.Params.Count) do
		begin
	        _param  := Result.Params[i];
	        _column := _param.Name;

	        {Check if the field is found                           }
	        {------------------------------------------------------}
	        if ModelDef.find(_column) = -1 then continue;
	        if _columnIndex.valueOf(_column).isEmpty then continue;
	        {------------------------------------------------------}

	        case ModelDef.field(_column).DataType of
			  dtUnknown:
	          Trip(Format('assignQueryParameters(): %s is %s', [_column,'dtUnknown']));

	          dtBit:
	          Trip(Format('assignQueryParameters(): %s is %s', [_column,'dtBit']));

	          dtInteger:
	          _param.asInteger:= _row.int_val[_column];

	          dtSmallInt:
	          _param.asInteger:= _row.int_val[_column];

	          dtBigInt:
	          _param.AsLargeInt:= _row.int64_val[_column];

	          dtWord:
	          _param.AsWord:= _row.qword_val[_column];

	          dtDWord:
	          _param.AsWord:= _row.qword_val[_column];

	          dtQWord:
	          _param.AsWord:= _row.qword_val[_column];

	          dtReal:
	          _param.AsFloat:= _row.float_val[_column];

	          dtSingle:
	          _param.AsFloat:= _row.float_val[_column];

	          dtDouble:
	          _param.AsFloat:= _row.float_val[_column];

	          dtNumeric:
	          _param.AsFloat:= _row.float_val[_column];

	          dtMoney:
	          _param.AsFloat:= _row.float_val[_column];

	          dtChar:
	          Trip(Format('assignQueryParameters(): %s is %s', [_column,'dtChar']));

	          dtVarchar:
	          _param.AsString:= _row.str_val[_column];

	          dtString:
	          _param.AsString:= _row.str_val[_column];

	          dtText:
	          _param.AsString:= _row.str_val[_column];

	          dtWideString:
	          _param.AsWideString:= _row.unicode_val[_column];

	          dtDate, dtTime, dtDateTime, dtTimestamp:
	          _param.AsString:= _row.str_val[_column];

	          dtBoolean:
	          _param.AsBoolean:= _row.bool_val[_column];

	          dtEnum:
	          Trip(Format('assignQueryParameters(): %s is %s', [_column,'dtEnum']));

	          dtJSON:
	          begin
	              case _row.Value(_column).JSONType of
	                  jtUnknown: ;
	                  jtNumber:  _param.AsString:= _row.str_val[_column];
	                  jtString:  _param.AsString:= _row.str_val[_column];
	                  jtBoolean: _param.AsString:= _row.str_val[_column];
	                  jtNull:    _param.AsString:= 'NULL';
	                  jtArray:   _param.asString:= row.jarray_val[_column].AsJSON;
	                  jtObject:  _param.AsString:= _row.jobj_val[_column].AsJSON;
	              end;
			  end;

			  dtJSONObject:
	          _param.AsString:= _row.jobj_val[_column].AsJSON;

	          dtJSONArray:
	          _param.AsString:= _row.jarray_val[_column].AsJSON;

	          dtJSONB:
	          Trip(Format('assignQueryParameters(): %s is %s', [_column,'dtJSONB']));

	          dtBinary:
	          Trip(Format('assignQueryParameters(): %s is %s', [_column,'dtBinary']));

	          dtSerial:
	          _param.asInteger:= _row.int_val[_column];

	          dtBigSerial:
	          _param.AsLargeInt:= _row.int64_val[_column];

	          dtUUID:
	          _param.AsString:= _row.str_val[_column];

	          dtXML:
	          _param.AsString:= _row.str_val[_column];

	          dtClass:
	          Trip(Format('assignQueryParameters(): %s is %s', [_column,'dtClass']));

	          dtObject:
	          Trip(Format('assignQueryParameters(): %s is %s', [_column,'dtObject']));

	          dtRecord:
	          Trip(Format('assignQueryParameters(): %s is %s', [_column,'dtRecord']));

	          dtInterface:
	          Trip(Format('assignQueryParameters(): %s is %s', [_column,'dtInterface']));

	          dtPointer:
	          Trip(Format('assignQueryParameters(): %s is %s', [_column,'dtPointer']));

	          dtMethodPointer:
	          Trip(Format('assignQueryParameters(): %s is %s', [_column,'dtMethodPointer']));

	          dtCustom:
	          Trip(Format('assignQueryParameters(): %s is %s', [_column,'dtCustom']));
			end;
		end;

	finally
	    _columnIndex.Free;
    end;

end;

function RbModel.saveToDB(_lock_name: string; _unlock_key: string): integer;
var
    i: integer;
    _row: RbRow;
    _insertCount: word = 0;
    _updateCount: word = 0;
    shouldInsert: boolean = false;
    shouldUpdate: boolean = false;
begin
    {Don't do anything if there are not rows in memory}
    if rowCount = 0 then exit;

    {PREPARE UPDATE AND INSERT QUERY OBJECTS}
    startTransaction;
    try
	    Result:= -1;
        {Looping through Rows}
	    for i:= 0 to pred(rowCount) do
	    begin
	        try
                {Assign the parameters}
		        _row:= row(i);
		        {INSERT ROW}
		        if _row.isNew then
		        begin
                    shouldInsert:=True;
                    if letDBInsertIDField then
		                assignQueryParameters(insertQry, _row, getColumnListWithoutIDField(whichColumns))
                    else
                        assignQueryParameters(insertQry, _row, whichColumns);

	                inc(_insertCount);
                    // log3('Insert SQL: ' + insertQry.SQL.Text);
		        end

		        {UPDATE ROW}
		        else if _row.hasChanged { and _row.islockedBy(_lock_name, _unlock_key)} then
		        begin
                    shouldUpdate:= True;
                    {make SQL for the current row}
                    updateQry.SQL.Text:= QBUpdater(i).sql;
					assignQueryParameters(updateQry, _row, _row.changedColumnNames);
                    //log3('UPDATE SQL: ' + _updateQryObj.SQL.Text);
	                inc(_updateCount);
				end;
	        except
		        on E:Exception do
		        begin
		            log3('RbModel.SaveToDB tripped while assigning parameters.');
		            log3(E.Message);
	                raise;
				end;
			end;

			try
	            if shouldInsert then
                begin
                    try
		            insertQry.ExecSQL;
                    shouldInsert:= false;
                    log3('## RbModel inserting "%s"', [Name]);
					except
                        on E:Exception do
                        begin
	    		            log3('RbModel.SaveToDB raised an exception while INSERTING.');
	    		            log3(E.Message);
	    		            log3('ModelName : %s; SQL: %s', [name, insertQry.sql.text]);
                            raise;
						end;
					end;
				end
	            else if shouldUpdate then
                begin
                    //if _row.unlock(_lock_name, _unlock_key) then
                    //begin
                        try
			                updateQry.ExecSQL;
		                    shouldUpdate:= false;
		                    log3('## RbModel updating "%s"', [Name, _row.rid]);
						except
	                        on E:Exception do
	                        begin
		    		            log3('RbModel.SaveToDB raised an exception while UPDATING.');
		    		            log3(E.Message);
		    		            log3('ModelName : %s; SQL: %s', [name, updateQry.sql.text]);
	                            raise;
							end;
						end;
                    //end;
				end;

                _row.clearChanges;

			except
		        on E:Exception do
		        begin
		            log3('RbModel.SaveToDB raised an exception.');
		            log3(E.Message);
	                raise;
				end;
			end;
		end; {Loop through Rows}

	    try
            commit;
		except
	        on E:Exception do
	        begin
                rollback;
	            log3('RbModel.SaveToDB Commit to DB raised an exception.');
	            log3(E.Message);
	            log3('ModelName : %s', [name]);
	            raise;
			end;
		end;
        Result:= i;

	finally
        endTransaction;
        freeUpdateQry;
        freeInsertQry;
	end;
end;


function RbModel.delete(const _rowNum: integer): boolean;
begin
    Result:= false;
    try
        deleteQry.SQL.Text:= QBDeleter(_rowNum).sql;

         //log3(_delQuery.SQL.Text);
        startTransaction;
        try
            deleteQry.ExecSQL;
            commit;
            Result:= true;
		except
            rollback;
		end;
	finally
		endTransaction;
        freeDeleteQry;
	end;
end;

function RbModel.delete(constref _row: RbRow): boolean;
var
    _delQuery: TSQLQuery;
begin
    Result:= false;
    try
        _delQuery:= qryClone;
        _delQuery.SQL.Text:= QBDeleter.where('').whereID(_row.rid).sql;

        startTransaction;
        try
            _delQuery.ExecSQL;
            commit;
            Result:= true;

		except
            rollback;
		end;

	finally
		endTransaction;
        _delQuery.Free;
	end;
end;

function RbModel.unlock(_row: RbRow; _unlock_key: string): boolean;
begin

end;

procedure RbModel.resetLock(_row: RbRow);
begin

end;

function RbModel.islockedBy(_row: RbRow; _lockName: string): boolean;
begin

end;

function RbModel.isLocked(_row: RbRow): boolean;
begin

end;

function RbModel.relinquishLock(_row: RbRow; _name: string): boolean;
begin

end;

function RbModel.lock(_row: RbRow; _lockName: string): string;
begin

end;

function RbModel.cancelLock(_row: RbRow; _key: string): TDbSaveStatus;
begin

end;

function RbModel.canEdit: boolean;
begin

end;

function RbModel.canEdit(_lockName: string): boolean;
begin

end;


{ RbRow }

procedure RbRow.setonChange(const _onChange: ProcRowChange);
begin
    if myonChange = _onChange then
        Exit;
    myonChange := _onChange;
end;

procedure RbRow.setwatchChanges(const _watchChanges: boolean);
begin
	if mywatchChanges=_watchChanges then Exit;
	mywatchChanges:=_watchChanges;
end;

function RbRow.getFtsSearchText: string;
var
    i: integer;
    _addComma: boolean = false;
begin
    Result:= '';
    if hasChanged then
    begin
	    for i:= 0 to pred(modelDef.ftsFields.count) do
	    begin
	        if _addComma then
	            Result += ' '
	        else
	            _addComma:= true;
	        Result += columns.Strings[modelDef.ftsFields.Items[i].Name];
		end;
	end;
end;

function RbRow.initializeColumns(_modelDef: RbModelDef): RbRow;
var
    i: integer;
begin
    myModelDef := _modeldef;

    if Assigned(myColumns) then
        myColumns.Free;

    myColumns  := myModelDef.defaultRow();
    myState    := rowInit;
    Result     := self;
end;

function RbRow.hasChanged: boolean;
begin
    Result := myChangedFields.Count > 0;
end;

procedure RbRow.change(const _name: string);
begin
    {check if the field name is in the modeldef}
    if modelDef.find(_name) < 0 then
        {Raise an exception}
        trip(Format('RbRow.change() Field "%s" is not in the model "%s"',
            [_name, modeldef.Name]));

    if watchChanges then
    begin
	    {Add this field to the list of changed fields}
	    if not Assigned(myChangedFields.Find(_name)) then
	    begin
	        // log3('RbRow.change() adding to changed fields ' + _name);
	        myChangedFields.Add(_name, True);
		end;

	    {Notify the change}
	    if Assigned(myOnChange) then
	        myOnChange(Self, _name);
	end;
end;

procedure RbRow.clearChanges;
begin
    {this does not work then you have to do this:}
    // myChangedFields.Clear;
    if hasChanged then
    begin
        myChangedFields.Free;
        myChangedFields := TDynaJSONObject.Create;
    end;
end;

procedure RbRow.store(const _name: string; const _value: integer);
begin
    if columns.Integers[_name] = _value then exit;
    change(_name);
    columns.Integers[_name] := _value;
end;

procedure RbRow.store(const _name: string; const _value: int64);
begin
    if columns.Int64s[_name] = _value then exit;
    change(_name);
    columns.Int64s[_name] := _value;
end;

procedure RbRow.store(const _name: string; const _value: QWord);
begin
    if columns.QWords[_name] = _value then exit;
    change(_name);
    columns.QWords[_name] := _value;
end;

procedure RbRow.store(const _name: string; const _value: boolean);
begin
    if columns.Booleans[_name] = _value then exit;
    change(_name);
    columns.Booleans[_name] := _value;
end;

procedure RbRow.store(const _name: string; const _value: TJSONFloat);
begin
    if columns.Floats[_name] = _value then exit;
    change(_name);
    columns.Floats[_name] := _value;
end;

procedure RbRow.store(const _name: string; const _value: TJSONStringType);
begin
    if columns.Strings[_name] = _value then exit;
    change(_name);
    columns.Strings[_name] := _value;
end;

procedure RbRow.storeUnicode(const _name: string; const _value: TJSONUnicodeStringType);
begin
    change(_name);
    columns.UnicodeStrings[_name] := _value;
end;

procedure RbRow.storeJSON(const _name: string; const _value: TJSONArray);
begin
    change(_name);
    columns.Arrays[_name] := _value; {previous value_jsonobj automatically freed}
end;

procedure RbRow.storeJSON(const _name: string; const _value: TJSONObject);
begin
    change(_name);
    columns.Objects[_name] := _value; {previous value_jsonobj automatically freed}
end;

procedure RbRow.store(const _name: string; const _value: TDateTime);
begin
    if columns.Strings[_name] = htmlDateTime(_value) then exit;
    change(_name);
    columns.Strings[_name] := htmlDateTime(_value); {previous value_jsonobj automatically freed}
end;

function RbRow.value_int(const _name: string): integer;
begin
    Result := columns.Integers[_name];
end;

function RbRow.value_int64(const _name: string): int64;
begin
    Result := columns.Int64s[_name];
end;

function RbRow.value_qword(const _name: string): QWord;
begin
    Result := columns.QWords[_name];
end;

function RbRow.value_bool(const _name: string): boolean;
begin
    Result := columns.Booleans[_name];
end;

function RbRow.value_float(const _name: string): TJSONFloat;
begin
    Result := columns.Floats[_name];
end;

function RbRow.value_str(const _name: string): TJSONStringType;
var
    _j: TJSONData;
begin
    _j:= columns.Find(_name);
    case _j.JSONType of
        jtUnknown:
        Result := '';

        jtNumber, jtString, jtBoolean:
        Result := columns.Strings[_name];

        jtNull:
        Result := '';

        jtArray:
        Result := columns.Arrays[_name].AsJSON;

        jtObject:
        Result := columns.Objects[_name].AsJSON;
    end;
end;

function RbRow.value_unicode(const _name: string): TJSONUnicodeStringType;
begin
    Result := columns.UnicodeStrings[_name];
end;

function RbRow.value_jsonarray(const _name: string): TJSONArray;
begin
    Result := columns.Arrays[_name];
end;

function RbRow.value_jsonobj(const _name: string): TJSONObject;
begin
    Result := columns.Objects[_name];
end;

function RbRow.value_datetime(const _name: string): TDateTime;
begin
    Result := readHtmlDateTime(myColumns.Strings[_name]);
end;

function RbRow.Name(const _index: integer): string;
begin
    Result := myColumns.Names[_index];
end;

function RbRow.Value(const _index: integer): TJSONData;
begin
    Result := columns.Items[_index];
end;

function RbRow.Value(const _name: string): TJSONData;
begin
    Result := columns.Find(_name);
end;

function RbRow.description(_index: integer): string;
begin
    Result := description(Name(_index));
end;

function RbRow.description(_name: string): string;
begin
    Result := myModelDef.field(_name).Description;
end;

function RbRow.hint(_index: integer): string;
begin
    Result := hint(Name(_index));
end;

function RbRow.hint(_name: string): string;
begin
    Result := myModelDef.field(_name).hint;
end;

function RbRow.Caption(_index: integer): string;
begin
    Result := Caption(Name(_index));
end;

function RbRow.Caption(_name: string): string;
begin
    Result := myModelDef.field(_name).Caption;
end;

function RbRow.comment(_index: integer): string;
begin
    Result := comment(Name(_index));
end;

function RbRow.comment(_name: string): string;
begin
    Result := myModelDef.field(_name).comment;
end;

function RbRow.rid: RbRowID;
begin
    Result := myColumns.Integers[IDField];
end;

function RbRow.rid(const _id: RbRowID): RbRow;
begin
    myColumns.Integers[IDField] := _id;
    Result := self;
end;

function RbRow.db_verision: longint;
begin
    Result := columns.Integers['db_version'];
end;

function RbRow.db_version(const _db_version: longint): RbRow;
begin
    store('db_version', _db_version);
    Result := self;
end;

function RbRow.row_version: longint;
begin
    Result := columns.integers['row_version'];
end;

function RbRow.row_version(const _row_version: longint): RbRow;
begin
    store('row_version', _row_version);
    Result := self;
end;

function RbRow.lock_type: DLockType;
begin
    Result := DLockType(columns.Integers['lock_type']);
end;

function RbRow.lock_type(const _lockType: DLockType): RbRow;
begin
    store('lock_type', Ord(_lockType));
    Result := self;
end;

function RbRow.locked_at: TDateTime;
begin
    Result := datetime_val['locked_at'];
end;

function RbRow.locked_at(const _locked_at: TDateTime): RbRow;
begin
    store('locked_at', _locked_at);
    Result := self;
end;

function RbRow.locked_by(_locked_by: string): RbRow;
begin
    Result:= self;
    str_val['locked_by']:= _locked_by;
end;

function RbRow.locked_by: string;
begin
    Result:= str_val['locked_by'];
end;

function RbRow.lock_name(_lock_name: string): RbRow;
begin
    Result:= self;
    str_val['lock_name']:= _lock_name;
end;

function RbRow.lock_name: string;
begin
    Result:= str_val['lock_name'];
end;

function RbRow.unlock_key(_unlock_key: string): RbRow;
begin
    Result:= self;
    str_val['unlock_key']:= _unlock_key;
end;

function RbRow.unlock_key: string;
begin
    result:= str_val['unlock_key'];
end;

function RbRow.columns: TJSONObject;
begin
    Result := myColumns;
end;

function RbRow.columnCount: integer;
begin
    Result := myColumns.Count;
end;

function RbRow.cloneColumns: TJSONObject;
begin
    Result:= TJSONObject(columns.Clone);
end;

function RbRow.theseColumnsObj(_columns: TStringArray): TJSONObject;
var
    _colName: string;
begin
    if Length(_columns) = 0 then
    begin
        {return all columns if no _columns are specified}
        Result:= columns.Clone as TJSONObject;
	end
    else
    begin
        Result := TJSONObject.Create;
        for _colName in _columns do
            Result.Add(_colName, myColumns.Elements[_colName].Clone);
	end;
end;

function RbRow.theseColumnsObj(_columns: string; _delim: string): TJSONObject;
begin
    Result:= theseColumnsObj(toStringArray(_columns, _delim));
end;

function RbRow.changedColumnsObj: TJSONObject;
var
    el: TJSONEnum;
begin
    Result := TJSONObject.Create;
    for el in myChangedFields do
    begin
        Result.Add(el.Key, columns.Elements[el.Key].Clone);
    end;
end;

function RbRow.changedColumnCount: integer;
begin
    Result := myChangedFields.Count;
end;

function RbRow.changedColumnNames: string;
var
    _changedColumns: TJSONObject;
begin
    try
        _changedColumns := changedColumnsObj;
        Result:= getDelimitedKeys(_changedColumns);
    finally
        _changedColumns.Free;
    end;
end;

function RbRow.lockStatus: DLockType;
begin
    Result := myLockStatus;
end;

function RbRow.copyColumns(_source, _dest: TJSONObject): boolean;
begin
    Result := copyJSONObject(_source, _dest);
    if not Result then
        log3('RbRow.copyColumns FAILED');
end;

function RbRow.copyColumns(_columns: TJSONObject): boolean;
begin
    Result := copyColumns(_columns, myColumns);
end;

function RbRow.copyColumns(_row: RbRow): boolean;
begin
    Result := copyColumns(_row.columns);
end;

function RbRow.isNew: boolean;
begin
    Result:= rid=NEWID;
end;

function RbRow.new: RbRowID;
begin
    clearChanges;
    Result := NEWID;
end;

function RbRow.Read(_id: RbRowID): boolean;
begin

end;

function RbRow.edit: boolean;
begin
    clearChanges;
end;

function RbRow.save(const _fields: string; const _values: string;
    const delimiter: string): RbRowID;
begin
    clearChanges;
end;

function RbRow.Delete: boolean;
begin

end;

function RbRow.commit: boolean;
begin

    clearChanges;
end;

function RbRow.rollback: boolean;
begin

    clearChanges;
end;

function RbRow.unlock(_lock_name: string; _unlock_key: string): boolean;
begin
    if islockedBy(_lock_name, _unlock_key) then
    begin
        resetLock();
        Result:= true;
	end
    else
        Result:= false;
end;

procedure RbRow.resetLock;
begin
    lock_type(lockNone);
    lock_name('');
    unlock_key('');
    locked_at(0.0);
end;

procedure RbRow.relinquishLock;
begin
    unlock(lock_name, unlock_key);
    clearChanges;
end;

constructor RbRow.Create(const _modeldef: RbModelDef);
begin
    inherited Create;
    initializeColumns(_modeldef);
    myChangedFields := TDynaJSONObject.Create;
    watchChanges:= true;
end;

destructor RbRow.Destroy;
begin
    myChangedFields.Free;
    myColumns.Free;
    inherited Destroy;
end;

function RbRow.modelDef: RbModelDef;
begin
    Result := myModelDef;
end;

function RbRow.islockedBy(_lockName: string; _unlock_key: string): boolean;
begin
    Result:= (_lockName = lock_name()) and (_unlock_key = unlock_key());
end;

function RbRow.isLocked: boolean;
begin
    Result:= (lock_type = lockReadForUpdate);
    if {file is locked }Result then
    begin
        {Check the time here. If it has been locked for too long, unlock it.}
        if not WithinPastMinutes(Now(), locked_at, ROW_LOCK_SPAN) then
        begin
            resetLock;
            Result:= false; {The file is no longer locked}
    	end;
    end;

end;

function RbRow.relinquishLock(_name: string; _unlock_key: string): boolean;
begin
    if isLockedby(_name, _unlock_key) then
        resetLock;
end;

function RbRow.lock(_lockName: string): string;
begin
    if not isLocked() then
    begin
        Result:= genRandomKey(12); {generate unlock key}
        unlock_key(Result);
        lock_name(_lockName);
        locked_at(Now());
        lock_type(lockReadForUpdate);
	end
    else
        Result:= ''; // don't return a key
end;

function RbRow.cancelLock(_key: string): TDbSaveStatus;
begin
    if (unlock_key = _key) then
    begin
        resetLock;
	end;
end;


function RbRow.canEdit(_lockName: string): boolean;
begin
    Result:= isLocked() {which also resets lock if timed out}
            and (lock_name = _lockName);
end;

function defaultSequenceGenerator(_sequenceName: string): Longint;
begin
    mySequences.Value[_sequenceName]:= mySequences.Value[_sequenceName] + 1;
    Result:= mySequences.Value[_sequenceName];
end;


{ TJSONDbSaveStatusHelper }
resourcestring
  ST_UNKNOWN='UNKNOWN';
  ST_FAIL = 'FAIL';
  ST_OK = 'OK';
  ST_ERR = 'ERR';

function TJSONDbSaveStatusHelper.status: string;
begin
    case self of
        jdbsaveUnknown:     Result:= ST_UNKNOWN;
        jdbsaveNotLocked:   Result:= ST_FAIL;
        jdbsaveIsLocked:    Result:= ST_OK;
        jdbsaveInvalidKey:  Result:= ST_FAIL;
        jdbsaveLockExtended:Result:= ST_OK;
        jdbsaveUnlocked:    Result:= ST_OK;
        jdbsaveCancelled:   Result:= ST_OK;
        jdbsaveError:       Result:= ST_ERR;
        jdbSuccess:         Result:= ST_OK;
        jdbFail:            Result:= ST_FAIL;
    end;
 end;

resourcestring
  ST_MSG_Unknown        = 'Lock status is unknown';
  ST_MSG_NotLocked      = 'Operation unsuccessful because this record was not locked before edit';
  ST_MSG_Locked         = 'Record is currently locked';
  ST_MSG_InvalidKey     = 'Save was unsuccessful because the lock key has expired or is invalid';
  ST_MSG_SaveLockExtended = 'Saved. Lock is extended';
  ST_MSG_SaveUnlocked   = 'Saved. Record is now unlocked';
  ST_MSG_SaveCancelled  = 'Cancelled. Record is now unlocked';
  ST_MSG_SaveError      = 'Error while Saving';
  ST_MSG_Ok             = 'OK';
  ST_MSG_Fail           = 'Failed';

function TJSONDbSaveStatusHelper.message: string;
begin
    case self of
        jdbsaveUnknown:
            Result:= ST_MSG_Unknown;

        jdbsaveNotLocked:
            Result:= ST_MSG_NotLocked;

        jdbsaveIsLocked:
            Result:= ST_MSG_Locked;

        jdbsaveInvalidKey:
            Result:= ST_MSG_InvalidKey;

        jdbsaveLockExtended:
            Result:= ST_MSG_SaveLockExtended;

        jdbsaveUnlocked:
            Result:= ST_MSG_SaveUnlocked ;

        jdbsaveCancelled:
            Result:= ST_MSG_SaveCancelled;

        jdbsaveError:
            Result:= ST_MSG_SaveError;

        jdbSuccess:
            Result:= ST_MSG_Ok;

        jdbFail:
            Result:= ST_MSG_Fail;
    end;
end;

initialization
    myModelFactory := RbModelFactory.Create;
    mySequences := TStringIndexMap.Create;
    initSequenceGenerator(@defaultSequenceGenerator);

finalization
    mySequences.Free;
    myModelFactory.Free;
    {These will be automatically freed when the db is freed}
    // myUserTable.Free;
    // myDataDictionaryTable.Free;
end.
