unit sugar.jsondb;
{$mode objfpc}{$H+}


interface

uses
    Classes, SysUtils, fpjson, sqlite3conn, sqldb,
    sugar.utils, sugar.collections, sugar.ddlmodel, sugar.modelbase,
    sugar.jsonlib, sugar.textfiler;
const
    ROW_PATH = 'store';
    JDB_EXTENSION = '.json';
    JDB_MANIFEST = '.manifest';
    JDB_FILTER = '*' + JDB_EXTENSION;
    JDB_MANIFEST_FILTER = '*' + JDB_MANIFEST;

    FILE_NOT_READ = -79;
    LOCK_SPAN     = 15; {mins}
    LOCK_TIME_INIT = 0.0;
    {CONTROL FIELDS}
    _UNLOCK_KEY = 'uq';                 {This carries the unlock key}
    _CAN_EDIT = 'can_edit';
    {Flags if the record can be edited. This means that it has been locked exclusively OR nobody is currently editing it right now}
    _LOCKED_BY = 'locked_by';          {Carries the user who has locked this}
    _LOCKED_AT = 'locked_at';          {Stores the time that the lock was placed}
    _IS_ROW_NEW = 'is_new';
    {Indicates if the data row is a new one being added}
    _IS_LOCKED_BY_ME = 'is_locked_by_me';
    {Indicates if the current user has acquired an exclusive lock on this row, A flag that is primarily used in the UI (not the best way. works for now) to determine if the current user owns the lock. Stored value is meaningless.}
    _MODEL_NAME = 'model';

    TBL_MODEL = 'models';
    TBL_ROWS = 'rows';
    TBL_EDGES = 'edges';
    TBL_EDGE_LIST = 'edgelist';
    TBL_KEY_SEARCHES = 'keysearches';
    TBL_KEY_SEARCHES_UNIQUE_KEY = 'model, rowID, key';

    composite_search = 'composite';
    composite_key_separator = '+';
    key_search = 'key';


type

    RbJSONDb = class;
    RbJSONDbModel = class;
    RbJSONDbRowList = class;

    RbJSONDbRowLink = class;
    RbJSONDbRowLinks = class;
    RbJSONDbRowDetailLink = class;
    RbJSONDbRowDetailLinks = class;
    RbJSONDbRow = class;
    RbJSONDbEdgeRowList = class;

    ProcJSONDataFactory = function: TJSONObject;
    ProcJSONDataFactoryMethod = function: TJSONObject of object;

    ProcRowDocMakerMethod = function(_default: TDynaJSONObject): TDynaJSONObject of
        object;
    ProcRowDocMakerFunc = function(_default: TDynaJSONObject): TDynaJSONObject;

    { RbEdge }
    RbDbEdgeCardinality = (edgeUndefined
        , edgeOne  {There is only one row in this edge}
        , edgeMany {There are many rows in this edge}
        );

    RbEdge = class
    private
        myHasChanged: boolean;
        myname: string;
        mydirected: boolean;
        myreverse_edge: string;
        myweight: word;
        mycost: word;
        myCardinality: RbDbEdgeCardinality;
        myVersion: word;
        myProperties: TDynaJSONObject;
        function getHasChanged: boolean;
        procedure setname(const _name: string);
        procedure setdirected(const _directed: boolean);
        procedure setreverse_edge(const _reverse_edge: string);
        procedure setweight(const _weight: word);
        procedure setcost(const _cost: word);
        procedure setcardinality(const _cardinality: RbDbEdgeCardinality);
        function getProperties: TJSONObject;
        procedure setProperties(const _properties: TJSONObject);


    public
        property hasChanged: boolean read getHasChanged;
        property Name: string read myname write setname;
        property directed: boolean read mydirected write setdirected;
        property reverse_edge: string read myreverse_edge write setreverse_edge;
        property weight: word read myweight write setweight;
        property cost: word read mycost write setcost;
        property cardinality: RbDbEdgeCardinality
            read mycardinality write setcardinality;
        property version: word read myVersion write myVersion;
        property properties: TJSONObject read getProperties write setProperties;

        procedure clearChanges;

        constructor Create;
        destructor Destroy; override;
    end;

    RbEdgeListBase = specialize GenericHashObjectList<RbEdge>;

    { RbEdgeList }

    RbEdgeList = class(RbEdgeListBase)
    private
        myDeletedEdges: TDynamicKeyValueStore;
    public
        function get(const _name: shortstring): RbEdge; reintroduce;

        {syntax sugar for "get()"}
        function edge(_name: string): RbEdge;
        function Delete(_name: string): boolean; overload;
        function deleteEdges: TStringArray;
        procedure resetDeletedEdges;
        constructor Create(FreeObjects: boolean = True); reintroduce;
        destructor Destroy; override;

    end;

    { RbRowEdge }

    RbRowEdge = class
    private
        myEdgeName: string;
        myDestModelName: string;
        myDestRowID: string;
        myVersion: word;
        function getSourceModelName: string;
        function getSourceRowID: string;
        procedure setDest(const _dest: RbJSONDbRow);
        procedure setEdge(const _edge: RbEdge);
        function getSource: RbJSONDbRow;
        function getDest: RbJSONDbRow;
        function getEdge: RbEdge;
        function getProperties: TJSONObject;
        procedure setProperties(const _properties: TJSONObject);

    protected
        mySource: RbJSONDbRow;
        myedge: RbEdge;
        mydest: RbJSONDbRow;
        mykey: string;
        myweight: word;
        mycost: word;
        myCardinality: RbDbEdgeCardinality;
        myProperties: TJSONObject;
        myHasChanged: boolean;
        procedure setSource(const _from: RbJSONDbRow);
        procedure setkey(const _key: string);
        procedure setweight(const _weight: word);
        procedure setcost(const _cost: word);
        procedure setChange(_field: string);

    public
        property key    : string read mykey write setkey;
        property Source : RbJSONDbRow read getSource write setSource;
        property dest   : RbJSONDbRow read getDest write setDest;
        property edge   : RbEdge read getEdge write setEdge;

        property sourceModelName    : string read getSourceModelName;
        property sourceRowID        : string read getSourceRowID;
        property edgeName           : string read myEdgeName;
        property destModelName      : string read myDestModelName;
        property destRowID          : string read myDestRowID;

        property cardinality    : RbDbEdgeCardinality read myCardinality;
        property weight         : word read myweight write setweight;
        property cost           : word read mycost write setcost;
        property version        : word read myVersion write myVersion;
        property properties     : TJSONObject read getProperties write setProperties;
        property hasChanged     : boolean read myHasChanged;

        function destObjAvailable: boolean;

        constructor Create;
        destructor Destroy; override;
        procedure clearChanges;
    end;

    {List of the edge definition for a row}
    RbRowEdgeListBase = specialize GenericHashObjectList<RbRowEdge>;

    { RbRowEdgeList }

    RbRowEdgeList = class(RbRowEdgeListBase)
    private
        mySource: RbJSONDbRow;
        myEdge: RbEdge;
        myEdgeName: string;
        myDeletedEdges: TDynamicKeyValueStore;
        procedure setSource(const _source: RbJSONDbRow);
        function add(_edge: RbEdge; _dest: RbJSONDbRow): string; overload;
        procedure setEdge(const _edge: RbEdge);
        function getEdgeName: string;
        function getEdge: RbEdge;
        function qryObj: TSQLQuery;
        function getSource: RbJSONDbRow;
    public
        {returns the key}
        property Source: RbJSONDbRow read getSource write setSource;
        property edge: RbEdge read getEdge write setEdge;
        property edgeName: string read getEdgeName;

        function makeKey(_from: RbJSONDbRow; _edge: RbEdge; _dest: RbJSONDbRow): string;
            overload;
        function makeKey(_dest: RbJSONDbRow): string; overload;
        {Adds a destination to this edge}
        function getEdge(_dest: RbJSONDbRow): RbRowEdge;
        function findEdgeFor(_dest: RbJSONDbRow): RbRowEdge;
        procedure Delete(Index: integer); reintroduce;

        function deletedEdges: string;
        function deletedEdgesArray: TStringArray;
        procedure resetDeletedEdges;

        constructor Create;
        destructor Destroy; override;

    end;

    {list of edges that a row has, looked up by edge name}
    RbListRowEdgesBase = specialize GenericHashObjectList<RbRowEdgeList>;

    { RbListRowEdgeList }
    {Named list of Row Edges}
    RbListRowEdgeList = class(RbListRowEdgesBase)
    private
        mySource: RbJSONDbRow;
        loadedEdgeCount: integer; {Counts the number edges that were loaded from the DB}
        procedure setSource(const _source: RbJSONDbRow);
        function qryObj: TSQLQuery;
        function getSource: RbJSONDbRow;
    public
        property Source: RbJSONDbRow read getSource write setSource;
        function load(_source: RbJSONDbRow): integer; {returns the row count}
        function save: integer; {returns row count that was "actually saved"}
        function get(const _edge: RbEdge): RbRowEdgeList; overload;
        function edgeCount: integer;

        destructor Destroy; override;
    end;

    { RbJSONDbRow }
    RbJSONDbRow = class
    private
        function getValue(_name: string): TJSONData;
    public
    const
        IDField = 'ID';

    protected {## FIELDS ## }

        myDataObject: TDynaJSONObject;

        myModel: RbJSONDbModel;
        myPath: string;
        myRootDir: string;

        myOnChange: TNotifyEvent;
        myDataHasChanged: boolean;

        myLookup: RbJSONDbRowLinks;
        myDetail: RbJSONDbRowDetailLinks;
        myRefs: RbJSONDbRowLinks;
        myEdges: RbListRowEdgeList;
        mykeySearches: TDynamicKeyValueStore;

        myDocMakerMethod: ProcRowDocMakerMethod;
        myDocMakerFunc: ProcRowDocMakerFunc;

        myName: string;
        myID: string;
        myCaption: string;
        myDescription: string;
        myLockKey: string;
        myLockName: string;
        myLockTime: TDateTime;

        myDataFiler: TTextFiler;
        myManifestFiler: TTextFiler;

    protected {## METHODS ## }
        function initialized: boolean;
        function Initialize: RbJSONDbRow;

        procedure setName(const _name: string);
        procedure setPath(const _path: string);


        function getMyLockName: string;
        procedure setMyLockName(const _myLockName: string);
        function getMyLockKey: string;
        procedure setMyLockKey(const _myLockKey: string);

        function getCaption: string;
        procedure setCaption(const _caption: string);
        function getDescription: string;
        procedure setDescription(const _description: string);
        procedure setOnChange(const _OnChange: TNotifyEvent);
        procedure NotifyModelOfMyChange;
        function getDataURI: string;
        function getManifestURI: string;
        function getLocked: boolean;
        function getMyData: TDynaJSONObject;
        procedure setID(const _id: string);
        procedure doUnlock;
        function getID: string;
        function getModelName: string;
        function getModel: RbJSONDbModel;
        procedure setModel(const _model: RbJSONDbModel);
        function getDB: RbJSONDb;
        procedure setkeySearches(const _keySearches: TDynamicKeyValueStore);
        function searchKeyChanged(_key: string; _value: string): boolean;
        function DeleteKeySearch: boolean;
        function DeleteEdges: boolean;
        procedure setDocMakerMethod(const _docMakerMethod: ProcRowDocMakerMethod);
        procedure setdocMakerFunc(const _docMakerFunc: ProcRowDocMakerFunc);
        function generateManifestJSON: string;
        function unlock(_key: string): boolean; {Returns true if unlocked}

        function doFileSave(): boolean; virtual;
        function doSaveToDB: boolean; virtual;
        function doSaveEdges: boolean; virtual;
        function doSaveKeySearches(constref _data: TJSONObject): boolean; virtual;
        procedure setControlFields; virtual; overload;
        procedure setControlFields(constref _data: TJSONObject); virtual; overload;
        procedure loadControlFields; virtual;

        {Force unlock of this row}
        function resetLock: boolean;

        function newFilerObject: TTextFiler;

    public {## PROPERTIES ## }
        property ID: string read getID write setID;
        property Name: string read myName write setName;
        property Caption: string read myCaption write setCaption;
        property description: string read myDescription write setDescription;
        property locked: boolean read getLocked;
        property Data: TDynaJSONObject read getMyData;
        property DbModel: RbJSONDbModel read getModel write setModel;
        property db: RbJSONDb read getDB;
        property path: string read myPath write setPath;
        property dataURI: string read getDataURI;
        property manifestURI: string read getManifestURI;
        property OnChange: TNotifyEvent read myOnChange write setOnChange;
        property Lookup: RbJSONDbRowLinks read myLookup;
        property Detail: RbJSONDbRowDetailLinks read myDetail;
        property Refs: RbJSONDbRowLinks read myRefs;
        property ModelName: string read getModelName;
        property keySearches: TDynamicKeyValueStore
            read mykeySearches write setkeySearches;
        property docMakerMethod: ProcRowDocMakerMethod
            read myDocMakerMethod write setDocMakerMethod;
        property docMakerFunc: ProcRowDocMakerFunc
            read myDocMakerFunc write setdocMakerFunc;
        property Value[_name: string]: TJSONData read getValue; default;

    public
        function dataClone: TDynaJSONObject;
        {Returns a clone of the data. Use this as much as possible}
        function load: boolean;
        procedure Close;

        {Is this is a brand new row?}
        function isNew: boolean;
        function refresh: RbJSONDbRow; virtual; {overide to compute custom field values }

      {SAVE(key, _unlock)
        save if the key is valid.
        unlock the row if _unlock is true, otherwise extend lock}
        function save(_data: TJSONObject; _key: string; _unlock: boolean = True;
            _updateModelManifest: boolean = False): TDbSaveStatus; overload;

        function save(_key: string; _unlock: boolean = True): TDbSaveStatus;
            overload;{Saves whatever is in the data Object, provided it was locked properly before}

        {Delete this row from disk}
        function Delete: boolean;

        {release the lock if it is held by _lockName}
        function lockedBy(_lockName: string): boolean;
        {is the row locked for edit?}
        function isLocked: boolean;
        {If "_name" has locked this, then the lock will be reset}
        function relinquishLock(_name: string): boolean;

      {LOCK() a row / document
        -> Returns the key if locked by the same _name.
        -> Returns empty if cannot lock.
        -> Returns new key if it can be locked}
        function lock(_lockName: string): string;
        {cancel the lock - use the key for cancelling}
        function cancelLock(_key: string): TDbSaveStatus;

        {Is the row in Edit mode (that means it is locked by someone)?}
        function canEdit: boolean; overload;
        {Can this _lockName user edit this row?}
        function canEdit(_lockName: string): boolean; overload;

        function edge(_edgeName: string; _toRow: RbJSONDbRow): RbRowEdge; overload;
        function edge(_edgeName: string; _model: string; _rowID: string): RbRowEdge;
            overload;
        function findEdge(_toRow: RbJSONDbRow): RbRowEdge; {not yet implemented}


        function edgeNames: TStringArray;
        function edgeCount: integer;

      {List of rows that are connected to this row. This contains rows from different models as well
      so you can inspect the row, find the model and do searches on the model}
        function edgeRowsObj(_edgeName: string): RbJSONDbRowList; deprecated;

        {returns the edge list for this edge name}
        function edges(_edgeName: string): RbRowEdgeList;

        {functions to remove edges}
        function removeEdge(_edgeName: string; _toRow: RbJSONDbRow): boolean; overload;
        function removeEdge(_edgeName: string; _model: string; _toRowID: string): boolean;
            overload;

        {clears the edges in memory. Does not delete from DB}
        procedure resetEdges;

        {Override this method to create a row with a default schema}
        function newDocument: TDynaJSONObject; virtual;


    public
        constructor Create;
        destructor Destroy; override;

    end;

    RbJSONDBRowListBase = specialize GenericHashObjectList<RbJSONDbRow>;

    { RbJSONDbRowList }

    RbJSONDbRowList = class(RbJSONDbRowListBase)
        modelbase: RbJSONDbModel;
        Name: string;
        path: string;
    end;

    RbJSONDbEdgeRowList = class(RbJSONDbRowListBase)

    end;



    { RbJSONDbModel }

    RbJSONDbModel = class
    public
    const
        {These are field names}
        IDLength = 12;
        __model_name = _MODEL_NAME;
        __IDField = 'ID';                    {db - field name that holds the ID string}
        __can_edit = _CAN_EDIT;
        {db - is the row editable - which means that it is either new or it is locked}
        __unlock_key = _UNLOCK_KEY;
        {db - if row was locked, then this is the key}
        __locked_by = _LOCKED_BY;              {db - row was locked using this name}
        __locked_at = _LOCKED_AT;              {db - row was locked at this time}
        __is_row_new = _IS_ROW_NEW;             {db - is this model new?}
        __is_locked_by_me = _IS_LOCKED_BY_ME;      {db - is row locked by me?}
        __uistate = 'uistate';               {JSONObject that store uistate variables}

    protected
        myName: string;
        myFullName: string;
        myCaption: string;
        myDescription: string;
        myPath: string;
        myURI: string;
        myDB: RbJSONDb;
        myModelDef: DTable; {model definition}
        myFreeModelDef: boolean;
        myLockName: string;
        myLockKey: string;
        myRows: RbJSONDbRowList;
        myDeletedRows: RbJSONDbRowList; {Keeps track of deleted rows}
        myLocks: TStringIntegerMap;
        myOnChange: TNotifyEvent;
        myKeySearches: TDynamicKeyValueStore;

        {Manifest}
        myModelManifestFiler: TTextFiler;
        myModelManifest: TJSONObject;
        isLoadingFromManifest: boolean;
        myrowBuilder: ProcJSONDataFactory;
        myrowBuilderMethod: ProcJSONDataFactoryMethod;

        function getPath: string;

        function getName: string;
        procedure setName(const _name: string);

        function getDB: RbJSONDb; virtual;
        procedure setDB(const _DB: RbJSONDb); virtual;

        function getCaption: string;
        procedure setCaption(const _caption: string);

        function getDescription: string;
        procedure setDescription(const _description: string);

        procedure setOnChange(const _OnChange: TNotifyEvent);

        procedure NotifyDBOfMyChange;
        procedure OnRowChanged(Sender: TObject);
        function getURI: string;
        procedure setManifestLocation;
        procedure saveManifestRows;
        function loadFromDir: integer;
        function hardload: integer;

        function getModelDef: RbModelDef;
        procedure setrowBuilder(const _rowBuilder: ProcJSONDataFactory);
        procedure setrowBuilderMethod(const _rowBuilderMethod: ProcJSONDataFactoryMethod);
        function getFullName: string;
        function initializeRow(constref _row: RbJSONDbRow; _rowID: string): RbJSONDbRow;

        function loadRow(_rowID: string): RbJSONDbRow;

    public
        property Name: string read getName write setName;
        {Returns the name as DBName.ModelName}
        property fullName: string read getFullName;
        property Caption: string read getCaption write setCaption;
        property description: string read getDescription write setDescription;
        property DB: RbJSONDb read getDB write setDB;
        property path: string read getPath;
        property uri: string read getURI;

        {Row generators. Order by priority}
        {priority 1} property rowBuilder: ProcJSONDataFactory
            read myrowBuilder write setrowBuilder;
        {priority 2} property rowBuilderMethod: ProcJSONDataFactoryMethod
            read myrowBuilderMethod write setrowBuilderMethod;
        {priority 3} property modelDef: RbModelDef read getModelDef;

        {Force this model to use a different modelDef}
        procedure useThisModelDef(_modelDef: RbModelDef; _freeOnDestroy: boolean);

        {instantiates a new row based on what is returned by rowBuilder.
        Need to refactor}
        function applyDataSchema(constref _row: RbJSONDbRow): boolean;

        {Returns a new row if successful. If ID is empty, then a new ID is generated.
        If the id supplied already exists, then newRow returns nil}
        function newRow(_ID: string = ''): RbJSONDbRow;
        function newID: string;

        property OnChange: TNotifyEvent read myOnChange write setOnChange;

        function lockedBy: string;
        function isLocked: boolean; overload;
        function isLocked(_name: string): boolean; overload;
        function lock(_name: string): string; {Returns a key if locked}
        function relinquishLock(_name: string): boolean;
        {If "_name" has locked this, then the lock will be reset}
        function unlock(_key: string): boolean; {Returns true if unlocked}
        {force the lock to be removed}
        function resetLock: boolean;

        function rowCount: integer;
        function get(_rowId: string): RbJSONDbRow; overload; virtual;
        function get(_i: integer): RbJSONDbRow; overload; virtual;
        function getRowID(_i: integer): string;

        function getManifestJSON: string;
        function updateManifestWith(_row: RbJSONDbRow): boolean;

        {searching}

        {creates an index with search values from the document}
        procedure addSearch(_key: string); overload;


    {Array of fields to make a composite search key. The key is
    stored as field1+field2 and the values are concatenated field values - with no delimiter}
        procedure addSearch(_keys: TStringArray); overload; // Add a composite search

        procedure initKeySearches;

    {Returns the row only if it exists}
        function findRow(_id: string): RbJSONDbRow;

    {Only keys that match the _keys used with addSearch are valid. No rows if not found.
    This function is best suited for composite key searches.
    The order of the keys must also match the order supplied in addSearch()}
        function findRows(_keys: TStringArray; _values: TStringArray;
            _matchPattern: boolean = False; {allows for wild card search}
            _limit: integer = -1;           {allows for pagination}
            _offset: integer = -1): RbJSONDbRowList; overload;

        function findRows(_key: string; _value: string; _matchPattern: boolean = False;
        {allows for wild card search}
            _limit: integer = -1;           {allows for pagination}
            _offset: integer = -1): RbJSONDbRowList; overload;


        function findFirst(_key: string; _value: string; _matchPattern: boolean = False): RbJSONDbRow; overload;
        function findFirst(_key: string; _value: integer; _matchPattern: boolean = False): RbJSONDbRow; overload;
        function findFirst(_key: string; _value: double; _matchPattern: boolean = False): RbJSONDbRow; overload;
        function findFirst(_key: string; _value: boolean; _matchPattern: boolean = False): RbJSONDbRow; overload;
        function findFirst(_keys: TStringArray; _values: TStringArray; _matchPattern: boolean = False): RbJSONDbRow; overload;

        function searchFor(_whereClause: string): RbJSONDbRowList;

    {If the sortField is included in Keysearch, then this returns the sorted rows. Otherwise it returns an empty list.
     Sort order takes SQL ASC or DESC}
        //function sortedRows(_sortField: string; _sortOrder: string = 'ASC'): RbJSONDbRowList;
        {sorts natural: uses internal typecasts}
        function sortedRows(_sortField: string; _sortOrder: string = 'ASC';
            _asNumber: boolean = False): RbJSONDbRowList;

        {syntax sugar for get()}
        function row(_rowID: string): RbJSONDbRow; overload; virtual;
        function row(_i: integer): RbJSONDbRow; overload; virtual;
        function rowExists(_rowID: string): boolean;

        {Returns a clone of the the data, prepped with lock information, it it was possible to lock}
        function rowPayload(_rowID: string; _lockName: string): TDynaJSONObject; overload; deprecated; {<-- not used}

        function load: integer; overload;
        function load(_dataURI: string): RbJSONDbRow; overload;
        procedure loadManifest;

        function post(_id: string; _data: TJSONObject; _name: string;
            _caption: string = ''; _description: string = ''; _unlock: boolean = True;
            _freeDataObject: boolean = False): TDbSaveStatus; virtual; overload;
        function post(_row: RbJSONDbRow; _name: string; _caption: string = '';
            _description: string = ''; _unlock: boolean = True): TDbSaveStatus; virtual; overload;
        function saveSuccess(_saveResult: TDbSaveStatus): boolean;
        function cancelEdit(_id: string; _userID: string): TDbSaveStatus;

        function rowUnlockKeyValid(_id: string; _unlockKey: string): boolean;

        function Delete(_id: string): integer; virtual;
        {deletes the file. --- planned: mark as deleted. don't delete physically}
        function deleteAllRows: integer;

        // function loadDeletedRow(_id: string): integer;
        function deletedRowIDs: TStringArray;
        {returns a comma separated list of deleted rows}
        function deletedRows: RbJSONDbRowList;

        function deletedRowCount: integer;
        function deletedRow(_rowID: string): RbJSONDbRow; overload;
        function deletedRow(_i: integer): RbJSONDbRow; overload;
        function undelete(_rowID: string): boolean;

        {delete physically}
        function purge(_id: string): integer;
        procedure Close;
        function modelExists: boolean;

    public
        constructor Create; virtual;
        constructor Create(_modelName: string); virtual;
        destructor Destroy; override;

    public
    {You have to create child classes to use these functions meaningfully. That is the only way to
    ensure that the schema for each model can be defined and the overridden getDefaultRow() will return
    the correct empty row for the model. Also, the class Var}
        function getDefaultModelDef: RbModelDef; virtual;
        function getDefaultRow: TDynaJSONObject; virtual;
        function defineKeySearches: boolean; virtual;
    end;

    RbJSONDbModelClass = class of RbJSONDbModel;


    RbJSONModelListBase = specialize GenericHashObjectList<RbJSONDbModel>;

    { RbJSONModelList }

    RbJSONModelList = class(RbJSONModelListBase)
        DB: RbJSONDb;
        function get(const s: shortstring): RbJSONDbModel; reintroduce;
        function get(const s: shortstring; M: RbJSONDbModelClass): RbJSONDbModel;
            overload;

        function modelObj(_name: string): RbJSONDbModel; overload;
        function modelObj(_name: string; _modelClass: RbJSONDbModelClass): RbJSONDbModel;
            overload;

        procedure Delete(Index: integer); reintroduce;

        // function load: integer;
        // function save: integer;
    end;



    RbJSONDb = class
    private
        function getURI: string;

    protected
        myDB: TSQLite3Connection;
        myEdges: RbEdgeList;
        myLoadedEdgeCount: integer;

        myFullPath: string;
        myPath: string;
        myName: string;
        myManifestFile: string;
        myManifestFileAge: longint;
        myModels: RbJSONModelList;

        function initDB(_name: string; _path: string): boolean;
        function closeDB: boolean;

        {saves the manifest for the db}
        function getPath: string;
        procedure OnModelChange(Sender: TObject);
        function getSQLModel(_name: string): RbModel;

    public
        property Name: string read myName;
        property path: string read getPath;
        property uri: string read getURI; {full identifier to the resource}

        function model(_name: string): RbJSONDbModel; overload;
        function model(_name: string; _modelClass: RbJSONDbModelClass): RbJSONDbModel;
            overload;

        function modelObj(_name: string): RbJSONDbModel; overload;
        function modelObj(_name: string; _modelClass: RbJSONDbModelClass): RbJSONDbModel;
            overload;

        function model(_i: integer): RbJSONDbModel;
        function modelExists(_name: string): boolean;
        function findModel(_name: string): RbJSONDbModel;
        function modelCount: integer;

        function findRow(_model: string; _rowID: string): RbJSONDbRow;

        function edge(_edgeName: string): RbEdge; overload;
        function edge(_index: integer): RbEdge; overload;
        function findEdge(_edgeName: string): RbEdge;
        function edgeExists(_edgeName: string): boolean;
        function edgeCount: integer;
        function saveEdges: integer; {returns number of edges saved}
        function loadEdges: integer; {returns number of edges loaded}
        function removeEdge(_edgeName: string): boolean;

        function clearEdges: boolean;

        procedure load;
        procedure save;

        function newQuery: TSQLQuery;

        procedure DBStartTransaction;
        procedure DBCommit;
        procedure DBRollback;
        procedure DBEndTransaction;

        constructor Create(_name: string; _path: string = '');
        destructor Destroy; override;

        function DBCreateSQL: string;
    end;

    {## LINKS AND REFERENCES }

    { RbJSONDbRowLink }

    RbJSONDbRowLink = class
    private
        myRow: RbJSONDbRow;
        myURI: string;
        myFields: TStringArray;
    public
        Name: string;
        property uri: string read myURI;
        property fields: TStringArray read myFields;
        function row: RbJSONDbRow;
        function getLinkedDataObj: TDynaJSONObject;
        {persistence}
        function Initialize(_name: string; _uri: string; _fields: string = '*';
            _delim: string = ','): RbJSONDbRowLink;
        function loadFromManifiest(_manifestJSON: TJSONObject): RbJSONDbRowLink;
        function getManifestJSONObj: TJSONObject;
        function getManifestJSON: string;
    end;

    RbJSONDbRowLinkListBase = specialize GenericHashObjectList<RbJSONDbRowLink>;

    { RbJSONDbRowLinks }

    RbJSONDbRowLinks = class(RbJSONDbRowLinkListBase)
        function row(_name: string): RbJSONDbRow;
        function row(_i: integer): RbJSONDbRow;
        function loadFromManifiest(_manifestJSON: TJSONObject): RbJSONDbRowLink;
        function getManifestJSONObj: TJSONObject;
        function getManifestJSON: string;
    end;

    { RbJSONDbRowDetailLink }
    RbJSONDbRowDetailLink = class
    private
        myLinkedRows: RbJSONDbRowLinks;
        myModelURI: string;
        myFields: TStringArray;
    public
        Name: string;
        property modelURI: string read myModelURI;
        property fields: TStringArray read myFields;
        function linkedRows: RbJSONDbRowLinks;
        function getLinkedDataArray: TJSONArray;
        function row(_name: string): RbJSONDbRow;
        function row(_i: integer): RbJSONDbRow;
        {persistence}
        function Initialize(_name: string; _modelURI: string;
            _fields: string = '*'; _linkRowURIs: string = '';
            _delim: string = ','): RbJSONDbRowLink;
        function addLink(_rowURI: string): RbJSONDbRowDetailLink;
        function loadFromManifiest(_manifestJSON: TJSONObject): RbJSONDbRowLink;
        function getManifestJSONObj: TJSONObject;
        function getManifestJSON: string;
    end;

    RbJSONDbRowDetailLinkListBase = specialize GenericHashObjectList<
        RbJSONDbRowDetailLink>;

    { RbJSONDbRowDetailLinks }

    RbJSONDbRowDetailLinks = class(RbJSONDbRowDetailLinkListBase)
        function loadFromManifiest(_manifestJSON: TJSONObject): RbJSONDbRowLink;
        function getManifestJSONObj: TJSONObject;
        function getManifestJSON: string;
    end;

function DefaultRBJSONDBModelDef: RbModelDef;
function defaultRBJSONDBDocument: TDynaJSONObject;
{returns the default object members required for rbJSONDB}
function convertToRBJSONDBRow(_data: TJSONObject): TJSONObject;
{converts a JSONObject to an rbJSONDBRow}
function defaultRowManifestObject: TJSONObject;
{returns a default manifest row object}


   {Checks if the Model exists in the JSONDbModel. If it does, then load it otherwise return the default model
   Also sets the lockKey}
function instantiateJSONDbModel(_jsonmodel: RbJSONDbModel; _id: string;
    _lockName: string; _defaultRow: TJSONObject;
    _freeDefault: boolean = False): TJSONObject; overload;
function instantiateJSONDbModel(_jsonmodel: RbJSONDbModel; _id: string;
    _lockName: string; _defaultRowBuilder: ProcJSONDataFactory): TJSONObject; overload;
function instantiateJSONDbModel(_jsonmodel: RbJSONDbModel; _id: string;
    _lockName: string; _defaultRowBuilder: ProcJSONDataFactoryMethod): TJSONObject;
    overload;

function loadRow(_db: RbJSONDb; _model: string; _rowID: string): RbJSONDbRow;

{Table definitions for making edges}
function getTableDef(_name: string): RbModelDef;
function defModels: RbModelDef;
function defRows: RbModelDef;
function defKeySearches: RbModelDef;
function defEdges: RbModelDef;
function defEdgeList: RbModelDef;
function dbDef: DDatabase;
function dbSchema: DSchema;

{This function returns the composite key string with fields separated by the separator char}
function buildSearchKey(_keys: TStringArray): string;

{This function returns the composite value string with. Current implementation is concatenation}
function buildSearchValue(_values: TStringArray): string;

implementation

uses
    FileUtil, LazFileUtils, dateutils, sugar.ddldatatypes, sugar.logger;

var
    myDbdef: DDatabase;

function buildSearchKey(_keys: TStringArray): string;
begin
    Result := getDelimitedString(_keys, composite_key_separator);
end;

function buildSearchValue(_values: TStringArray): string;
var
    _v: string;
begin
    {See formatCompositeValue() inside doSaveKeySearches().
    Any change in implementation must be reflected in both places}

    Result := '';
    for _v in _values do
        Result := Result + _v;
end;


function defaultRBJSONDBModelDef: RbModelDef;
begin
    {IF YOU CHANGE THIS, YOU HAVE TO UPDATE RbJSONDbRow.setControlFields()}
    Result := RbModelDef.Create('RbJSONDbModel');
    with Result, RbJSONDbModel do
    begin
        textcode(__IDField, IDLength);
        shortText(__model_name).comment('Name of the model');

        Bool(__can_edit).DefaultValue('true')
            .comment(
            'This flag is true if the row is either new or has been locked by the current user for editing. It is false if another user has locked it');

        shortText(__locked_by)
            .comment(
            'Name of the user who has locked this record');

        shortText(__unlock_key)
            .comment(
            'When the row has been locked, this field contains the unlock key. Saving only happens if the key matches the key stored in the record and if the person who locked it is also the person requesting the save');

        Timestamp(__locked_at)
            .comment('Stores the timestamp of when the row was locked');

        bool(__is_row_new).DefaultValue('true')
            .comment('indicates whether this is a new row');

        bool(__is_locked_by_me).DefaultValue('false')
            .comment(
            'Convenience flag. This is true if the row is locked by the current user. Use this to show visual indication that the current record is locked by the user');

        jsonObject(__uistate)
            .comment(
            'This is a default object to hold any ui states that may be required in VueApp objects');
    end;
end;

function defaultRBJSONDBDocument: TDynaJSONObject;
var
    _model: TJSONData;
begin
    {IF YOU CHANGE THIS, YOU HAVE TO UPDATE RbJSONDbRow.setControlFields()}
    with defaultRBJSONDBModelDef() do
    begin
        Result := defaultRow();
        Free();
    end;
end;

function convertToRBJSONDBRow(_data: TJSONObject): TJSONObject;
var
    _defaultRBJSONDBDocument: TDynaJSONObject;
begin
    _defaultRBJSONDBDocument := defaultRBJSONDBDocument();
    copyJSONObject(_defaultRBJSONDBDocument, _data);
    Result := _data;
    _defaultRBJSONDBDocument.Free;
end;

function defaultModelManifest: TJSONObject;
begin
    Result := TJSONObject.Create;
    with Result do
    begin
        add('name', '');
        add('rows', '');
        add('deleted', '');
        add('searchkeys', '');
    end;
end;


function defaultRowManifestObject: TJSONObject;
begin
    Result := TJSONObject.Create(['model', '', RBJSONDbRow.IDField,
        '', 'name', '', 'caption', '', 'description', '', 'version', 0,
        'lock_name', '', 'lock_key', '', 'lock_time', '', 'lookup',
        TJSONArray.Create(), {Many to One relationship}
        'detail', TJSONArray.Create(), {One to Many relationship}
        'refs', TJSONArray.Create()  {Many to one relationship}
        ]);
end;

function instantiateJSONDbModel(_jsonmodel: RbJSONDbModel;
    _id: string; _lockName: string; _defaultRow: TJSONObject;
    _freeDefault: boolean): TJSONObject;
var
    _key: string = '';
begin

    if _id.isEmpty then
        {myKey is blank here. So, when it returns from the browser, the save will automatically run through
        because there is no locking on a new record.}
        Result := convertToRBJSONDBRow(_defaultRow.Clone as TJSONObject)

    else if _jsonmodel.rowExists(_id) then
    begin
        with _jsonmodel.get(_id) do
        begin
            if not _lockName.isEmpty then
                {make it empty only for new records then no locking}
            begin
                //// log('instantiateJSONDbModel:: lockName is %s', [_lockName]);
                if canEdit(_lockName) then
                    _key := lock(_lockName);
                //// log('instantiateJSONDbModel:: lockKey is %s', [_key]);
            end
            else
            ;//// log('instantiateJSONDbModel:: lockName is empty');

            Result := Data.clone as TJSONObject;
            with Result do
            begin
                //Strings[IDField]        := ID; {already set in the call to get()}
                Booleans[_CAN_EDIT]         := canEdit(_lockName);
                Strings[_UNLOCK_KEY]        := _key;
                Booleans[_IS_LOCKED_BY_ME]  := lockedBy(_lockName);
                Strings[_LOCKED_BY]         := ''; {Don't send the lock name??}
            end;
        end;
    end
    else {Model does not exist}
        Result := nil;

    if _freeDefault then
        _defaultRow.Free;
end;

function instantiateJSONDbModel(_jsonmodel: RbJSONDbModel; _id: string;
    _lockName: string; _defaultRowBuilder: ProcJSONDataFactory): TJSONObject;
begin
    Result := instantiateJSONDbModel(_jsonmodel, _id, _lockName,
        _defaultRowBuilder(), True);
end;

function instantiateJSONDbModel(_jsonmodel: RbJSONDbModel; _id: string;
    _lockName: string; _defaultRowBuilder: ProcJSONDataFactoryMethod): TJSONObject;
begin
    Result := instantiateJSONDbModel(_jsonmodel, _id, _lockName,
        _defaultRowBuilder(), True);
end;

function loadRow(_db: RbJSONDb; _model: string; _rowID: string): RbJSONDbRow;
begin
    if _db.modelExists(_model) then
    begin
        Result := _db.model(_model).load(_rowID);
    end
    else
        Result := nil;
end;

function getTableDef(_name: string): RbModelDef;
begin
    Result := dbSchema.findObj(_name);
    if not Assigned(Result) then
        Result := dbSchema.Table(_name);
end;

function defModels: RbModelDef;
const
    M = TBL_MODEL;
begin
    Result := dbSchema.FindObj(M);
    if not Assigned(Result) then
    begin
        Result := dbSchema.Table(M);
        with Result do
        begin
            shortText('name').unique(True);
            Number('rcount');
            JSON('edges');
        end;
    end;
end;

function defRows: RbModelDef;
const
    M = TBL_ROWS;
begin
    Result := dbSchema.FindObj(M);
    if not Assigned(Result) then
    begin
        Result := dbSchema.Table(M);
        with Result do
        begin
            shortText('key').unique(True);
            shortText('model').btIndex();
            shortText('name').btIndex();
            Text('caption');
            Text('description', 4096);
            Number('version');
            shortText('lock_name');
            shortText('lock_key');
            Timestamp('lock_time');
            JSONObject('jdata');
            JSONArray('edges');

        end;
    end;
end;

function defKeySearches: RbModelDef;
const
    M = TBL_KEY_SEARCHES;
begin
    Result := dbSchema.FindObj(M);
    if not Assigned(Result) then
    begin
        Result := dbSchema.Table(M);
        with Result do
        begin
            shortText('model').btIndex();
            shortText('rowID').btIndex();
            shortText('key').btIndex();
            shortText('value').btIndex();
            btIndex(toStringArray(TBL_KEY_SEARCHES_UNIQUE_KEY), idxUnique);
        end;
    end;
end;

function defEdges: RbModelDef;
const
    M = TBL_EDGES;
begin
    Result := dbSchema.FindObj(M);
    if not Assigned(Result) then
    begin
        Result := dbSchema.Table(M);
        with Result do
        begin
            shortText('key').unique(True);
            shortText('source_model').btIndex();
            shortText('source_row').btIndex();
            shortText('dest_model').btIndex();
            shortText('dest_row').btIndex();
            shortText('edge').btIndex();
            number('cardinality');
            number('weight');
            number('cost');
            number('version');
            JSONObject('properties');
            btIndex(['source_model', 'source_row']);
            btIndex(['dest_model', 'dest_row']);
        end;
    end;
end;

function defEdgeList: RbModelDef;
const
    M = TBL_EDGE_LIST;
begin
    Result := dbSchema.FindObj(M);
    if not Assigned(Result) then
    begin
        Result := dbSchema.Table(M);
        with Result do
        begin
            shortText('edge').unique(True);
            Number('version');
            Bool('directed').DefaultValue('false');
            shortText('reverse_edge');
            number('weight');
            number('cost');
            number('cardinality');
            JSONObject('properties');
        end;
    end;
end;

function dbDef: DDatabase;
begin
    if not Assigned(myDbdef) then
        myDbDef := DDatabase.Create('jsondb');

    Result := myDbdef;
end;

function dbSchema: DSchema;
begin
    Result := dbDef.Schema('main');
end;

{ RbListRowEdgeList }

procedure RbListRowEdgeList.setSource(const _source: RbJSONDbRow);
begin
    if mySource = _source then
        Exit;
    mySource := _source;
end;

function RbListRowEdgeList.qryObj: TSQLQuery;
begin
    Result := Source.DbModel.DB.newQuery;
end;

function RbListRowEdgeList.getSource: RbJSONDbRow;
begin
    if Assigned(mySource) then
        Result := mySource
    else
        Trip('RbJSONDbRowEdges.getSource():: Source has not been assigned');
end;



function RbListRowEdgeList.load(_source: RbJSONDbRow): integer;
var
    m: RbModel;
    i: integer;
    _rowEdge: RbRowEdge;
    _row: RbRow;
begin
    Source := _source;
    m := getModelObj(TBL_EDGES);
    m.useThisQueryObj(qryObj);
    try
        m.QBReader
            .nolimit
            .where('')
            .where(Format('source_model=%s and source_row = %s',
            [QuotedStr(Source.DbModel.Name), QuotedStr(Source.ID)]));

        //log('');
        //log('--------- LOADING EDGES -------------');
        //log(m.QBReader.sql);
        //log('---------               -------------');
        //log('');

        m.loadFromDB;

        {Clear the list}
        Clear;
        Result := 0;
        for i := 0 to pred(m.rowCount) do
        begin
            _rowEdge := RbRowEdge.Create;
            _row := m.row(i);

            _rowEdge.Source         := _source;
            _rowEdge.key            := _row.str_val['key'];
            _rowEdge.myEdgeName     := _row.str_val['edge'];
            _rowEdge.myDestModelName:= _row.str_val['dest_model'];
            _rowEdge.myDestRowID    := _row.str_val['dest_row'];
            _rowEdge.myWeight       := _row.qword_val['weight'];
            _rowEdge.myCost         := _row.qword_val['cost'];
            _rowEdge.myVersion      := _row.qword_val['version'];
            _rowEdge.myCardinality  := RbDBEdgeCardinality(_row.qword_val['cardinality']);
            _rowEdge.Properties     := _row.jobj_val['properties'];

            _rowEdge.clearChanges;

            {Edgename -> List of Row Edges}
            with get(_rowEdge.myEdgeName) do
            begin
                add(_rowEdge.key, _rowEdge);
            end;
            Inc(Result);
        end;
    finally
        loadedEdgeCount := Result;
        m.Free;
    end;
end;

function RbListRowEdgeList.save: integer;
var
    m: RbModel;
    i, j: integer;
    _rowEdge: RbRowEdge;
    _row: RbRow;
    _rowEdges: RbRowEdgeList;
    _edge: RbRowEdge;
    _bDoSave: boolean;
    _e, _deletedEdges, _deletedEdge: string;
    _bAddcomma: boolean = False;
    _currentEdgeCount: integer = 0;
begin
    Result := -1;
    m := getModelObj(TBL_EDGES);
    m.useThisQueryObj(qryObj);

    m.QBReader
        .nolimit
        .where('')
        .where(Format('source_model=%s and source_row = %s',
        [QuotedStr(Source.DbModel.Name), QuotedStr(Source.ID)]));

    m.QBInserter.onConflict('key').doUpdate;

    m.loadFromDB;

    { We need to locate rows by the value of "key"
      so add an in-memory index}

    m.addIndex('key');
    m.buildIndex();

    try
        _deletedEdges := '';
        for i := 0 to pred(Count) do {Loop through Edge Names}
        begin
            _rowEdges := Items[i];

            for j := 0 to pred(_rowEdges.Count) do {Loop through the row edges.}
            begin
                _edge := _rowEdges.Items[j];
                _row := m.findFirst('key', _edge.key);

                //log3('## Row-Edge: %s', [_edge.key]);

                {Determine whether this is a new record}
                if (assigned(_row)) then
                begin
                    _bDoSave :=
                        (_row.qword_val['version'] = _edge.version) and (_edge.hasChanged);
                end
                else
                begin
                    _row := m.newRow;
                    _bDoSave := True;
                end;

                if _bDoSave then
                begin
                    Inc(_edge.myVersion);

                    _row.store('key', _edge.key);
                    _row.store('source_model', _edge.sourceModelName);
                    _row.store('source_row', _edge.sourceRowID);
                    _row.store('dest_model', _edge.destModelName);
                    _row.store('dest_row', _edge.destRowID);
                    _row.store('edge', _edge.edgeName);
                    _row.store('weight', _edge.weight);
                    _row.store('cost', _edge.cost);
                    _row.store('version', _edge.version);
                    _row.storeJSON('properties', _edge.properties.clone as TJSONObject);

                    log3('saving Row-Edge: %s', [_edge.key]);
                    Inc(Result);
                end;
            end;

            {Keep track of the rows that were saved}
            for _deletedEdge in _rowEdges.deletedEdgesArray do
            begin
                if _bAddcomma then
                    _deletedEdges := _deletedEdges + COMMA
                else
                    _bAddcomma := True;

                _deletedEdges := _deletedEdges + Format('"%s"', [_deletedEdge]);
            end;
            _rowEdges.resetDeletedEdges;

            {Tally up the total number of edges}
            Inc(_currentEdgeCount, _rowEdges.Count);
        end;

        {Now save all the changes}
        m.saveToDB();

        {ISSUE THE DELETE COMMAND ONLY WHEN THERE IS A CHANGE IN THE NUMBER OF EDGES}
        if not _deletedEdges.isEmpty then
        begin
            {Delete all records that are not in this memory list}
            m.QBDeleter()
                .where('')
                .where(Format('source_model=%s and source_row = %s',
                [QuotedStr(Source.DbModel.Name), QuotedStr(Source.ID)]))
                .where(Format('and key in (%s)', [_deletedEdges]));
            log3('');
            log3('DELETING EDGES --> SQL');
            log3(m.deleteSQL);
            log3('');
            m.Delete();
        end;

    finally
        m.Free;
    end;

end;


function RbListRowEdgeList.get(const _edge: RbEdge): RbRowEdgeList;
begin
    {Edge list for this _edge}

    Result := find(_edge.Name);

    if not Assigned(Result) then
        Result := inherited get(_edge.Name);

    if not Assigned(Result.mySource) then
        Result.mySource := self.Source;

    if not Assigned(Result.myEdge) then
        Result.myEdge := _edge;

end;

function RbListRowEdgeList.edgeCount: integer;
var
    i: integer;
begin
    Result := 0;
    for i := 0 to pred(Count) do
    begin
        Inc(Result, Items[i].Count);
    end;
end;

destructor RbListRowEdgeList.Destroy;
begin
    inherited Destroy;
end;




{ RbRowEdgeList }

procedure RbRowEdgeList.setSource(const _source: RbJSONDbRow);
begin
    if mySource = _source then
        Exit;
    mySource := _source;
end;


function RbRowEdgeList.makeKey(_from: RbJSONDbRow; _edge: RbEdge;
    _dest: RbJSONDbRow): string;
begin
    {edgeName:from_id:dest_id}
    Result := Format('%s:%s-%s:%s-%s', [_edge.Name, _from.DbModel.Name,
        _from.ID, _dest.DbModel.Name, _dest.ID]);
end;

function RbRowEdgeList.makeKey(_dest: RbJSONDbRow): string;
begin
    {edgeName:from_id:dest_id}
    Result := makeKey(Source, edge, _dest);
end;

function RbRowEdgeList.add(_edge: RbEdge; _dest: RbJSONDbRow): string;
begin
    Result := makeKey(Source, _edge, _dest);
    if not exists(Result) then
    begin
        with get(Result) as RbRowEdge do
        begin
            key := Result;
            Source := self.Source;
            myEdgeName := _edge.Name;
            myDestModelName := _dest.DbModel.Name;
            weight := _edge.weight;
            cost := _edge.cost;
        end;
    end;
end;

procedure RbRowEdgeList.setEdge(const _edge: RbEdge);
begin
    if myedge = _edge then
        Exit;
    myedge := _edge;
end;

function RbRowEdgeList.getEdgeName: string;
begin
    Result := edge.Name;
end;

function RbRowEdgeList.getEdge: RbEdge;
begin
    if Assigned(myEdge) then
        Result := myEdge
    else
        trip('RbJSONDbRowEdgeList.getEdge()::  Edge has not been assigned');
end;

function RbRowEdgeList.qryObj: TSQLQuery;
begin
    Result := Source.DbModel.DB.newQuery;
end;

function RbRowEdgeList.getSource: RbJSONDbRow;
begin
    if Assigned(mySource) then
        Result := mySource
    else
        Trip('RbJSONDbRowEdgeList.getSource():: Source has not been assigned');
end;

function RbRowEdgeList.getEdge(_dest: RbJSONDbRow): RbRowEdge;
var
    _edgeObj: RbRowEdge;
    _key: string;
begin
    _key := makeKey(_dest);
    Result := find(_key);
    if not Assigned(Result) then
    begin
        _edgeObj := RbRowEdge.Create;
        _edgeObj.key := _key;
        _edgeObj.Source := self.Source;
        _edgeObj.dest := _dest;
        _edgeObj.edge := self.edge;

        _edgeObj.myEdgeName := self.edgeName;
        _edgeObj.myDestModelName := _dest.DbModel.Name;
        _edgeObj.myweight := edge.weight;
        _edgeObj.mycost := edge.cost;

        add(_key, _edgeObj);
    end;
end;

function RbRowEdgeList.findEdgeFor(_dest: RbJSONDbRow): RbRowEdge;
begin
    Result := find(makeKey(_dest));
end;

procedure RbRowEdgeList.Delete(Index: integer);
begin
    myDeletedEdges.put(Items[index].key, '');
    inherited Delete(Index);
end;

function RbRowEdgeList.deletedEdges: string;
begin
    Result := getDelimitedString(deletedEdgesArray);
end;

function RbRowEdgeList.deletedEdgesArray: TStringArray;
begin
    Result := myDeletedEdges.keys;
end;

procedure RbRowEdgeList.resetDeletedEdges;
begin
    myDeletedEdges.Clear;
end;

constructor RbRowEdgeList.Create;
begin
    inherited Create;
    myDeletedEdges := TDynamicKeyValueStore.Create;
end;

destructor RbRowEdgeList.Destroy;
begin
    myDeletedEdges.Free;
    inherited Destroy;
end;


{ RbRowEdge }

procedure RbRowEdge.setkey(const _key: string);
begin
    if mykey = _key then
        Exit;
    mykey := _key;
    setChange('key');
end;



procedure RbRowEdge.setcost(const _cost: word);
begin
    if mycost = _cost then
        Exit;
    mycost := _cost;
    setChange('cost');
end;

procedure RbRowEdge.setChange(_field: string);
begin
    {_field specific changes to be implemented}
    myHasChanged := True;
end;

function RbRowEdge.destObjAvailable: boolean;
begin
    Result := Assigned(mydest);
end;

procedure RbRowEdge.setweight(const _weight: word);
begin
    if myweight = _weight then
        Exit;
    myweight := _weight;
    setChange('weight');
end;

function RbRowEdge.getSourceModelName: string;
begin
    if Assigned(Source) then
        Result := Source.DbModel.Name
    else
        Result := '';
end;

function RbRowEdge.getSourceRowID: string;
begin
    if Assigned(Source) then
        Result := Source.ID
    else
        Result := '';
end;

procedure RbRowEdge.setDest(const _dest: RbJSONDbRow);
begin
    if myDest = _dest then
        Exit;
    myDest := _dest;
    myDestModelName := _dest.DbModel.Name;
    myDestRowID := _dest.ID;
    setChange('dest');
end;

procedure RbRowEdge.setEdge(const _edge: RbEdge);
begin
    if myEdge = _edge then
        Exit;
    myEdge := _edge;
    myEdgeName := _edge.Name;
    setChange('edge');
end;

function RbRowEdge.getDest: RbJSONDbRow;
begin
    Result := myDest;
    if not Assigned(Result) then
    begin
        Result := Source.DB.model(myDestModelName).load(myDestRowID);
        if Assigned(Result) then
            myDest := Result;
    end;
end;

function RbRowEdge.getEdge: RbEdge;
begin
    Result := myedge;
    if not Assigned(Result) then
    begin
        Result := Source.db.findEdge(myEdgeName);
        if Assigned(Result) then
            myEdge := Result;
    end;
end;

function RbRowEdge.getProperties: TJSONObject;
begin
    Result := myProperties;

    {because when you directly access this object,
    it is read from here, but the values are changing in the object.}
    setChange('properties');
end;

procedure RbRowEdge.setProperties(const _properties: TJSONObject);
begin
    copyJSONObject(_properties, myProperties, True);
    setChange('properties');
end;

function RbRowEdge.getSource: RbJSONDbRow;
begin
    Result := mySource;
    if not Assigned(Result) then
        Trip('RbJSONDbRowEdge.getSource() Source is not assigned');
end;

procedure RbRowEdge.setSource(const _from: RbJSONDbRow);
begin
    if mySource = _from then
        Exit;
    mySource := _from;
end;

constructor RbRowEdge.Create;
begin
    inherited;
    mySource := nil;
    myedge := nil;
    mydest := nil;
    mykey := '';
    myweight := 0;
    mycost := 0;
    myCardinality := edgeUndefined;
    myHasChanged := False;
    myEdgeName := '';
    myDestModelName := '';
    myDestRowID := '';
    myVersion := 0;
    myProperties := TDynaJSONObject.Create;
end;

destructor RbRowEdge.Destroy;
begin
    myProperties.Free;
    inherited Destroy;
end;



procedure RbRowEdge.clearChanges;
begin
    myHasChanged := False;
end;

{ RbEdge }

function RbEdge.getHasChanged: boolean;
begin
    Result := myHasChanged;
end;

procedure RbEdge.setcost(const _cost: word);
begin
    if mycost = _cost then
        Exit;
    mycost := _cost;
    myHasChanged := True;
end;

procedure RbEdge.setcardinality(const _cardinality: RbDbEdgeCardinality);
begin
    if mycardinality = _cardinality then
        Exit;
    mycardinality := _cardinality;
end;

function RbEdge.getProperties: TJSONObject;
begin
    Result := myProperties;
end;

procedure RbEdge.setProperties(const _properties: TJSONObject);
begin
    copyJSONObject(_properties, properties, True);
    myHasChanged := True;
end;


procedure RbEdge.clearChanges;
begin
    myHasChanged := False;
end;



procedure RbEdge.setdirected(const _directed: boolean);
begin
    if mydirected = _directed then
        Exit;
    mydirected := _directed;
end;

procedure RbEdge.setname(const _name: string);
begin
    if myname = _name then
        Exit;
    myname := _name;
    myHasChanged := True;
end;

procedure RbEdge.setreverse_edge(const _reverse_edge: string);
begin
    if myreverse_edge = _reverse_edge then
        Exit;
    myreverse_edge := _reverse_edge;
    myHasChanged := True;
end;

procedure RbEdge.setweight(const _weight: word);
begin
    if myweight = _weight then
        Exit;
    myweight := _weight;
    myHasChanged := True;
end;

constructor RbEdge.Create;
begin
    inherited;
    mydirected := False;
    myreverse_edge := '';
    myweight := 0;
    mycost := 0;
    myCardinality := edgeUndefined;
    myVersion := 0;
    myProperties := TDynaJSONObject.Create;
    clearChanges;
end;

destructor RbEdge.Destroy;
begin
    myProperties.Free;
    inherited Destroy;
end;

{ RbEdgeList }

function RbEdgeList.get(const _name: shortstring): RbEdge;
begin
    Result := inherited get(_name);
    if Result.Name <> _name then
        Result.Name := _name;
end;

function RbEdgeList.edge(_name: string): RbEdge;
begin
    Result := get(_name);
end;

function RbEdgeList.Delete(_name: string): boolean;
var
    i: integer = -1;
begin
    if exists(_name) then
        i := indexOf(find(_name));

    Result := i > -1;
    if Result then
    begin
        myDeletedEdges.put(_name, '');
        Delete(i);
    end;
end;

function RbEdgeList.deleteEdges: TStringArray;
begin
    Result := myDeletedEdges.keys;
end;

procedure RbEdgeList.resetDeletedEdges;
begin
    myDeletedEdges.Clear;
end;

constructor RbEdgeList.Create(FreeObjects: boolean);
begin
    inherited Create(FreeObjects);
    myDeletedEdges := TDynamicKeyValueStore.Create;
end;

destructor RbEdgeList.Destroy;
begin
    myDeletedEdges.Free;
    inherited Destroy;
end;



{ RbJSONDbRowDetailLinks }

function RbJSONDbRowDetailLinks.loadFromManifiest(_manifestJSON: TJSONObject):
RbJSONDbRowLink;
begin

end;

function RbJSONDbRowDetailLinks.getManifestJSONObj: TJSONObject;
begin

end;

function RbJSONDbRowDetailLinks.getManifestJSON: string;
begin

end;

{ RbJSONDbRowDetailLink }

function RbJSONDbRowDetailLink.linkedRows: RbJSONDbRowLinks;
begin

end;

function RbJSONDbRowDetailLink.getLinkedDataArray: TJSONArray;
begin

end;

function RbJSONDbRowDetailLink.row(_name: string): RbJSONDbRow;
begin

end;

function RbJSONDbRowDetailLink.row(_i: integer): RbJSONDbRow;
begin

end;

function RbJSONDbRowDetailLink.Initialize(_name: string; _modelURI: string;
    _fields: string; _linkRowURIs: string; _delim: string): RbJSONDbRowLink;
begin

end;

function RbJSONDbRowDetailLink.addLink(_rowURI: string): RbJSONDbRowDetailLink;
begin

end;

function RbJSONDbRowDetailLink.loadFromManifiest(_manifestJSON: TJSONObject):
RbJSONDbRowLink;
begin

end;

function RbJSONDbRowDetailLink.getManifestJSONObj: TJSONObject;
begin

end;

function RbJSONDbRowDetailLink.getManifestJSON: string;
begin

end;

{ RbJSONDbRowLinks }

function RbJSONDbRowLinks.row(_name: string): RbJSONDbRow;
begin

end;

function RbJSONDbRowLinks.row(_i: integer): RbJSONDbRow;
begin

end;

function RbJSONDbRowLinks.loadFromManifiest(_manifestJSON: TJSONObject):
RbJSONDbRowLink;
begin

end;

function RbJSONDbRowLinks.getManifestJSONObj: TJSONObject;
begin

end;

function RbJSONDbRowLinks.getManifestJSON: string;
begin

end;

{ RbJSONDbRowLink }

function RbJSONDbRowLink.row: RbJSONDbRow;
begin

end;

function RbJSONDbRowLink.getLinkedDataObj: TDynaJSONObject;
begin

end;

function RbJSONDbRowLink.Initialize(_name: string; _uri: string;
    _fields: string; _delim: string): RbJSONDbRowLink;
begin

end;

function RbJSONDbRowLink.loadFromManifiest(_manifestJSON: TJSONObject): RbJSONDbRowLink;
begin

end;

function RbJSONDbRowLink.getManifestJSONObj: TJSONObject;
begin

end;

function RbJSONDbRowLink.getManifestJSON: string;
begin

end;



{ RbJSONDbRow }


procedure RbJSONDbRow.setName(const _name: string);
begin
    if myName = _name then
        exit;
    myName := _name;
end;

procedure RbJSONDbRow.setPath(const _path: string);
begin
    if myPath = _path then
        Exit;

    myPath := _path;

    if not DirectoryExists(myPath) then
        if not ForceDirectories(myPath) then
            trip('RbJSONDbRow.setPath: Cannot force directories >> ' + myPath);

    myDataFiler.rootDir := myPath;
    myManifestFiler.rootDir := myPath;
end;

function RbJSONDbRow.doSaveToDB: boolean;
var
    m: RbModel;
begin
    m := getModelObj(TBL_ROWS);
    if assigned(m) then
    begin
        try
            m.theseColumns(m.listDataColumns());
            m.useThisQueryObj(DbModel.DB.newQuery());
            with m.QBInserter do
                onConflict('key').doUpdate;

            with m.newRow do
            begin
                store('key', ID);
                store('model', self.modelName);
                store('name', self.Name);
                store('caption', self.Caption);
                store('description', self.description);
                storeJSON('jdata', TJSONObject(Data.Clone));
                store('version', value_int('version') + 1);
                store('lock_name', myLockName);
                store('lock_key', myLockKey);
                store('lock_time', htmlDateTime(myLockTime));
                storeJSON('edges', TJSONArray.Create());
            end;
            log3('Saving Row %s->%s', [self.modelName, ID]);
            m.saveToDB();
            Result := True;
        finally
            m.Free;
        end;
    end;
end;

function RbJSONDbRow.doSaveEdges: boolean;
begin
    Result := myEdges.save() > -1;
end;

function RbJSONDbRow.searchKeyChanged(_key: string; _value: string): boolean;
var
    m: RbModel;
    i: integer = 0;
begin
    Result := True;
    m := getModelObj(TBL_KEY_SEARCHES);
    if assigned(m) then
    begin
        m.useThisQueryObj(DB.newQuery);
        try
            m.QBReader
                .limit(1)
                .columns('value')
                .where {to clear the where}
                .where(Format('model=%s', [QuotedStr(DbModel.Name)]))

                .where('and')
                .where(Format('rowID=%s', [QuotedStr(ID)]))

                .where('and')
                .where(Format('key=%s', [QuotedStr(_key)]));

            //.where('and')
            //   .where(Format('value=%s', [QuotedStr(_value)]));

            with m.readQry do
            begin
                Open;
                {Work around to make sure that there were no records}
                while not EOF do
                begin
                    Result := not (Fields[0].AsString = _value);
                    Inc(i);
                    Next;
                end;

                Close;
            end;
        finally
            m.Free;
        end;
    end;

end;


function RbJSONDbRow.doSaveKeySearches(constref _data: TJSONObject): boolean;
const
    NO_VALUE = '-';
var
    m: RbModel;
    i, j: integer;
    _key: string = '';
    _searchType: string;
    _value: string = '';
    _jdata: TJSONData;

    function extractValue(_j: TJSONData): string;
    begin
        if assigned(_j) then
        begin
            case _j.JSONType of
                jtUnknown: Result := NO_VALUE;
                jtNumber: Result := _j.AsString;
                jtString: Result := _j.AsString;
                jtBoolean: Result := _j.AsString;
                jtNull: Result := NO_VALUE;
                jtArray: Result := _j.FormatJSON();
                jtObject: Result := _j.FormatJSON();
            end;
        end
        else
            Result := NO_VALUE;
    end;

    function extractCompositeValue(_composite_key: string): string;
    var
        _k: string;
        _j: TJSONData;
    begin
        Result := '';
        for _k in toStringArray(_composite_key, composite_key_separator) do
        begin
            if _data.Find(_k, _j) then
            begin
                {See also buildSearchValue()  - any change in implementation must be reflected here}
                Result := Result + extractValue(_j);
            end;
        end;
    end;

begin
    j := -1;

    m := getModelObj(TBL_KEY_SEARCHES);
    m.useThisQueryObj(DB.newQuery);
    try
        m.theseColumns(m.listDataColumns);
        m.QBInserter.onConflict(TBL_KEY_SEARCHES_UNIQUE_KEY).doUpdate;
        for i := 0 to pred(mykeySearches.Count) do
        begin
            _key := mykeySearches.Names[i];
            _searchType := myKeySearches.stringVal(i);

            case _searchType of
                key_search:
                begin
                    _jData := _data.Find(_key);
                    _value := extractValue(_jData);
                end;

                composite_search:
                begin
                    _value := extractCompositeValue(_key);
				end;
            end;

            if _value <> NO_VALUE then
            begin
                {Don't update the index if there is no change}
                if searchKeyChanged(_key, _value) then
                begin
                    with m.newRow do
                    begin
                        store('model', DbModel.Name);
                        store('rowID', ID);
                        store('key', _key);
                        store('value', _value);
                        Inc(j);
                    end;
                end
                else
                    ;

            end;
        end;

        m.saveToDB();

    finally
        Result := (j > -1);
        m.Free;
    end;
end;

function RbJSONDbRow.doFileSave(): boolean;
var
    sl: TStringList;
begin
    {Because the data is cleared before copying,
    you need up update control fields before saving.
    This step is necessary to ensure that whenever data is
    saved, the current object state is also saved.}

    try
        {Saving data}
        if myDataHasChanged then
        begin
            setControlFields(myDataObject);
            myDataFiler.content := myDataObject.FormatJSON();
        end;

    except
        on E: Exception do
        begin
            log('===> ERROR:: RbJSONDbRow.doFileSave - CONTENT');
            log(E.Message);
        end;
    end;

    try
        {Saving Manifest}
        myManifestFiler.content := generateManifestJSON;

    except
        on E: Exception do
        begin
            log('===> ERROR:: RbJSONDbRow.doFileSave - MANIFEST');
            log(E.Message);
        end;
    end;

    myDataHasChanged := False;
    Result := True;
end;


function RbJSONDbRow.getMyLockKey: string;
begin
    Result := myLockKey;
end;

function RbJSONDbRow.getMyLockName: string;
begin
    Result := myLockName;
end;

procedure RbJSONDbRow.setMyLockKey(const _myLockKey: string);
begin
    if myLockKey = _myLockKey then
        exit;
    myLockKey := _myLockKey;
end;



function RbJSONDbRow.getCaption: string;
begin
    Result := myCaption;
end;

function RbJSONDbRow.getDescription: string;
begin
    Result := myDescription;
end;

procedure RbJSONDbRow.setCaption(const _caption: string);
begin
    if myCaption = _caption then
        exit;
    myCaption := _caption;
    NotifyModelOfMyChange;
end;

procedure RbJSONDbRow.setDescription(const _description: string);
begin
    if myDescription = _description then
        exit;
    myDescription := _description;
    NotifyModelOfMyChange;
end;

procedure RbJSONDbRow.setOnChange(const _OnChange: TNotifyEvent);
begin
    if myOnChange = _OnChange then
        Exit;
    myOnChange := _OnChange;
end;

procedure RbJSONDbRow.NotifyModelOfMyChange;
begin
    //if Assigned(myOnChange) then
    //    myOnChange(Self);

    if not myDataHasChanged then
        myDataHasChanged := True;
end;

function RbJSONDbRow.getDataURI: string;
begin
    Result := myDataFiler.filePath;
end;

function RbJSONDbRow.getManifestURI: string;
begin
    Result := myManifestFiler.filePath;
end;


function RbJSONDbRow.getLocked: boolean;
begin
    Result := canEdit();
end;

function RbJSONDbRow.getValue(_name: string): TJSONData;
begin
    Result := Data.Find(_name);
end;

function RbJSONDbRow.initialized: boolean;
begin
    Result := Assigned(myDataObject);
end;

function RbJSONDbRow.Initialize: RbJSONDbRow;
begin
    if not Assigned(myDataObject) then
    begin
        myDataObject := newDocument();
    end;
    Result := self;
end;

function RbJSONDbRow.getMyData: TDynaJSONObject;
var
    _source: TJSONData;
begin
    Initialize;
    if myDataFiler.fileChanged then
    begin
        // writeLn(ID, ' myDataFile has changed');
        _source := GetJSON(myDataFiler.content);
        if Assigned(_source) then
        begin
            try
                if _source.JSONType = jtObject then
                begin
                    copyJSONObject(TJSONObject(_source), myDataObject);
                    setControlFields;
                end;

            finally
                _source.Free;
            end;
        end;
    end;
    Result := myDataObject;
end;

procedure RbJSONDbRow.setID(const _id: string);
begin
    if myID = _id then
        Exit;
    myID := _id;
    myDataFiler.fileName := _id + JDB_EXTENSION;
    myManifestFiler.fileName := _id + JDB_MANIFEST;
    Data.strings[IDField] := myID;
end;

procedure RbJSONDbRow.doUnlock;
begin
    myLockName := '';
    myLockKey := '';
    myLockTime := LOCK_TIME_INIT;

    Data.Strings[_UNLOCK_KEY] := '';
    Data.Strings[_LOCKED_BY] := '';
    Data.Strings[_LOCKED_AT] := htmlDateTime(LOCK_TIME_INIT);
    Data.Booleans[_CAN_EDIT] := False;
    Data.Booleans[_IS_LOCKED_BY_ME] := False;
end;

function RbJSONDbRow.getID: string;
begin
    Result := myID;
end;

function RbJSONDbRow.getModelName: string;
begin
    if Assigned(myModel) then
        Result := myModel.Name
    else
        Result := '';
end;

function RbJSONDbRow.getModel: RbJSONDbModel;
begin
    Result := myModel;
end;

procedure RbJSONDbRow.setModel(const _model: RbJSONDbModel);
begin
    myModel := _model;
end;

function RbJSONDbRow.getDB: RbJSONDb;
begin
    Result := DbModel.Db;
end;

procedure RbJSONDbRow.setkeySearches(const _keySearches: TDynamicKeyValueStore);
begin
    if mykeySearches = _keySearches then Exit;
    mykeySearches := _keySearches;
end;


procedure RbJSONDbRow.setMyLockName(const _myLockName: string);
begin
    myLockName := _myLockName;
end;

function RbJSONDbRow.generateManifestJSON: string;
begin
    with defaultRowManifestObject do
    begin
        strings['model'] := myModel.Name;
        strings[IDField] := ID;
        strings['name'] := Name;
        strings['caption'] := Caption;
        strings['description'] := description;
        strings['lock_name'] := myLockName;
        strings['lock_key'] := myLockKey;
        strings['lock_time'] := htmlDateTime(myLockTime);
        Result := FormatJSON();
        Free;
    end;
end;


procedure RbJSONDbRow.setControlFields;
begin
    setControlFields(myDataObject);
end;

procedure RbJSONDbRow.setControlFields(constref _data: TJSONObject);
var
    _lockTime: string;
begin
    {Assign all control fields to the _data objects}
    {UPDATE ALL FIELDS SET IN newDocument()}
    if Assigned(_data.Find(_MODEL_NAME)) then
    begin
        if _data.strings[_MODEL_NAME] <> ModelName then
            _data.strings[_MODEL_NAME] := ModelName;
    end
    else
        _data.Add(_MODEL_NAME, ModelName);

    if Assigned(_data.Find(IDField)) then
    begin
        if _data.strings[IDField] <> ID then
            _data.strings[IDField] := ID;
    end
    else
        _data.Add(IDField, ID);


    if Assigned(_data.Find(IDField)) then
    begin
        if _data.strings[IDField] <> ID then
            _data.strings[IDField] := ID;
    end
    else
        _data.Add(IDField, ID);

    if Assigned(_data.find(_CAN_EDIT)) then
        _data.Booleans[_CAN_EDIT] := canEdit
    else
        _data.Add(_CAN_EDIT, canEdit);

    if Assigned(_data.Find(_UNLOCK_KEY)) then
    begin
        if _data.Strings[_UNLOCK_KEY] <> myLockKey then
            _data.Strings[_UNLOCK_KEY] := myLockKey;
    end
    else
        _data.Add(_UNLOCK_KEY, myLockKey);

    if Assigned(_data.Find(_LOCKED_BY)) then
    begin
        if _data.Strings[_LOCKED_BY] <> myLockName then
            _data.Strings[_LOCKED_BY] := myLockName;
    end
    else
        _data.Add(_LOCKED_BY, myLockName);

    _lockTime := htmlDateTime(myLockTime);
    if Assigned(_data.Find(_LOCKED_AT)) then
    begin
        if _data.Strings[_LOCKED_AT] <> _lockTime then
            _data.Strings[_LOCKED_AT] := _lockTime;
    end
    else
        _data.Add(_LOCKED_AT, htmlDateTime(myLockTime));

    if Assigned(_data.Find(_IS_ROW_NEW)) then
        _data.Booleans[_IS_ROW_NEW] := isNew
    else
        _data.Add(_IS_ROW_NEW, isNew);

end;

procedure RbJSONDbRow.loadControlFields;
begin
    if Assigned(Data.Find(_UNLOCK_KEY)) then
        myLockKey := Data.Strings[_UNLOCK_KEY];

    if Assigned(Data.Find(_LOCKED_BY)) then
        myLockName := Data.Strings[_LOCKED_BY];

    if Assigned(Data.Find(_LOCKED_AT)) then
        myLockTime := readHtmlDateTime(Data.Strings[_LOCKED_AT]);

end;

function RbJSONDbRow.save(_data: TJSONObject; _key: string; _unlock: boolean;
    _updateModelManifest: boolean): TDbSaveStatus;
var
    _proceed: boolean;
begin
    Result := jdbsaveUnknown;
    {When you are saving a new record, then myLockKey and key are empty.
    The default value of canEdit is true and this if condition is met.

    When you are saving an existing record, then unless it has been locked,
    }

    Log('... in Save()');
    Log('  == LockName: "%s" = "%s"; UnlockKey: "%s" = "%s"', [myLockName, myDataObject.strings[_LOCKED_BY], myLockKey, _key]);
    _proceed := isNew;

    if not _proceed then {is an existing record}
        _proceed := (isLocked() and (myLockKey = _key));

    if _proceed then
    begin
        if _unlock then
        begin
            doUnlock;
            Result := jdbsaveUnlocked;
            //log('Unlocked');
        end
        else
        begin
            myLockTime := Now(); {extend the lock time}
            Result := jdbsaveLockExtended;
            //log('Extended the lock');
        end;

        try
            if Assigned(_data) then
            begin
                //Log3('Copying data before saving');
                //saveFileContent(ExpandFileName('source.json'), _data.FormatJSON());
                //saveFileContent(ExpandFileName('dest.json'), data.FormatJSON());
                copyJSONObject(_data, myDataObject, {clearFirst = }True);
                //saveFileContent(ExpandFileName('dest-after-copy.json'), data.FormatJSON());
                NotifyModelOfMyChange;
                {---------------------------------------------------------------------------------------------------------}
                {## SECTION:: SAVE DATA HERE ##}
                {---------------------------------------------------------------------------------------------------------}
                try
                    {Save the data} doFileSave;
                    {update model manifest}
                    if _updateModelManifest then
                    begin
                        myModel.updateManifestWith(self);
                    end;
                except
                    on E: Exception do
                    begin
                        Result := jdbsaveError;
                        log('RbJSONDbRow.save: Error while doFileSave() "%s"-> %s',
                            [Name, E.Message]);
                    end;
                end;

                try

                    {*************************************}
                    {Save to data to DB}    doSaveToDB; // Eventually use this instead of filesystem.
                    {*************************************}

                    {Save edgeNames}        doSaveEdges;
                except
                    on E: Exception do
                    begin
                        Result := jdbsaveError;
                        log('RbJSONDbRow.save: Error while doSaveEdges "%s"-> %s',
                            [Name, E.Message]);
                    end;
                end;

                try
                    {Save KeySearches}      doSaveKeySearches(_data);
                except
                    on E: Exception do
                    begin
                        Result := jdbsaveError;
                        log('RbJSONDbRow.save: Error while doSaveKeySearches "%s"-> %s',
                            [Name, E.Message]);
                    end;
                end;
            end
            else
                log('RbJSONDbRow.save() :: Data is empty...');

            //'Saved %s', [ID]);
        except
            on E: Exception do
            begin
                Result := jdbsaveError;
                log('RbJSONDbRow.save: Error while saving "%s"-> %s', [Name, E.Message]);
            end;
        end;
    end
    else
    begin
        // log('Didn''t save because not properly locked');
        Result := jdbsaveNotLocked;
        Log('---> Record not locked');
    end;
end;

function RbJSONDbRow.save(_key: string; _unlock: boolean): TDbSaveStatus;
var
    tmpData: TJSONObject;
begin
    tmpData := myDataObject.Clone;
    Result := save(tmpData, _key, _unlock, True);
    tmpData.Free;
end;

function RbJSONDbRow.cancelLock(_key: string): TDbSaveStatus;
begin
    if isLocked then
        Result := jdbsaveIsLocked
    else
        Result := jdbsaveNotLocked;

    if (Result = jdbsaveIsLocked) and (myLockKey = _key) then
    begin
        if resetLock then
        begin
            log3('Unlocked');
            Result := jdbsaveCancelled;
        end;
    end;
end;

function RbJSONDbRow.DeleteEdges: boolean;
var
    m: RbModel;
begin
    Result := False;
    m := getModelObj(TBL_EDGES);
    if not assigned(m) then
        exit;

    m.useThisQueryObj(DB.newQuery);

    try
        with m.QBDeleter do
        begin
            where('');
            (*  Delete all edge rows where source or dest points to this row *)
            where('(');
            where(Format('source_model = %s', [QuotedStr(DbModel.Name)]));
            where('and');
            where(Format('source_row = %s', [QuotedStr(ID)]));

            where(') OR (');
            where(Format('dest_model = %s', [QuotedStr(DbModel.Name)]));
            where('and');
            where(Format('dest_row = %s', [QuotedStr(ID)]));
            where(')');

        end;
        log3('Deleting Edges');
        log3(m.deleteSQL);
        Result := m.Delete();
        resetEdges;
    finally
        m.Free;
    end;
end;

procedure RbJSONDbRow.setdocMakerFunc(const _docMakerFunc: ProcRowDocMakerFunc);
begin
    if myDocMakerFunc = _docMakerFunc then
        Exit;
    myDocMakerFunc := _docMakerFunc;
    myDocMakerMethod := nil;
end;

procedure RbJSONDbRow.setDocMakerMethod(const _docMakerMethod: ProcRowDocMakerMethod);
begin
    if myDocMakerMethod = _docMakerMethod then
        Exit;
    myDocMakerMethod := _docMakerMethod;
    myDocMakerFunc := nil;
end;

function RbJSONDbRow.DeleteKeySearch: boolean;
var
    m: RbModel;
begin
    Result := False;
    m := getModelObj(TBL_KEY_SEARCHES);
    if not assigned(m) then
        exit;

    m.useThisQueryObj(DB.newQuery);

    with m.QBDeleter do
    begin
        where('');
        where(Format('model = %s', [QuotedStr(DbModel.Name)]));
        where('and');
        where(Format('rowID = %s', [QuotedStr(ID)]));
    end;
    Result := m.Delete();
    m.Free;
end;

function RbJSONDbRow.Delete: boolean;
begin
    Result := DeleteFile(dataURI);
    Result := DeleteFile(manifestURI);
    Result := DeleteEdges();
    Result := DeleteKeySearch();
end;

function RbJSONDbRow.load: boolean;
var
    _j: TJSONObject;

    procedure loadManifestData;
    begin
        _j := GetJSON(myManifestFiler.content) as TJSONObject;
        if Assigned(_j) then
        begin
            Result := True;
            try
                {Verify the ID}
                if myID <> _j.Strings[IDField] then
                begin
                    Result := False;
                    log3('RbJSONDbRow.load(): ID is different in manifest file myID=%s | manifest.ID = %s', [myID, _j.strings[IDField]]);
                end;

                {Verify the Model}
                if (ModelName <> _j.Strings['model']) then
                begin
                    Result := False;
                    log3('RbJSONDbRow.load(): Trying to load "%s" inside a "%s" model',
                        [_j.Strings['model'], ModelName]);
                end;

                myName          := _j.Strings['name'];
                myCaption       := _j.Strings['caption'];
                myDescription   := _j.Strings['description'];
                myLockName      := _j.Strings['lock_name'];
                myLockKey       := _j.Strings['lock_key'];
                myLockTime      := readHtmlDateTime(_j.Strings['lock_time']);

                // {Load the data}
                // getMyData;

            except
                on E: Exception do
                begin
                    log3('RbJSONDbRow.load(): ERROR:: %s', [E.Message]);
                    Result := False;
                end;
            end;
            _j.Free;
        end;
    end;

    procedure loadEdges;
    begin
        myEdges.Clear;
        myEdges.load(Self);
    end;

begin
    Result := False;
    if myManifestFiler.fileChanged then
    begin
        loadManifestData;
        loadEdges;
        Result := True;
    end;
end;

procedure RbJSONDbRow.Close;
begin

    {Just free the memory of the data object. We can reload it when you need it again.
    The Manifest stays so we know what this record is about}

    FreeAndNil(myDataObject);
    FreeAndNil(myDataFiler);
    myDataFiler := newFilerObject();
end;

function RbJSONDbRow.lockedBy(_lockName: string): boolean;
begin
    if _lockName.isEmpty then
        Result := False
    else
        Result := (myLockName = _lockName);
end;

function RbJSONDbRow.isLocked: boolean;
begin
    Result := not myLockKey.isEmpty;
    if {file is locked }Result then
    begin
        {Check the time here. If it has been locked for too long, unlock it.}
        if not WithinPastMinutes(Now(), myLockTime, LOCK_SPAN) then
        begin
            log('  == Oh, I am reseting the lock because it has expired');
            Log('  == myLockTime is "%s"', [DateTimeToStr(myLockTime)]);
            resetLock;
            Result := False; {The file is no longer locked}
        end;
    end;
end;

function RbJSONDbRow.canEdit: boolean;
begin
    {Well, we can edit this file only if it is locked. So this makes sense.}
    Result := isLocked;
end;


function RbJSONDbRow.lock(_lockName: string): string;
var
    _refreshLockInfo: boolean = true;
begin

    load; {First load the lastest version of the data from the disk}

    if _lockName.isEmpty then
    begin
        //log('RbJSONDbRow.lock() --> Not locking because lock name is empty');
        Result := '';
    end
    else
    begin
	    if not isLocked then
	    begin
	        myLockName := _lockName;
	        myLockKey  := genRandomKey(12, 3);
	        myLockTime := Now();

	        Result := myLockKey;
	        try
	            NotifyModelOfMyChange;
	            doFileSave;
                _refreshLockInfo:= false;
	            //log('RbJSONDbRow.lock --> row has been locked with "%s"', [myLockKey]);
	        except
	            on E: Exception do
	                log3(E.Message);
	        end;
	    end
	    else
	    begin
	        {If a lock is requested again by the same name, return the key
	        This allows you to navigate back and forth between pages in a session
	        and still have access to the locked Row}
	        if _lockName = myLockName then
	        begin
	            //log('RbJSONDbRow.lock --> row was already locked by "%s" : Key = "%s"', [myLockName, myLockKey]);
	            Result := myLockKey;
	        end
	        else
	            Result := ''; {it cannot be locked}
	    end;
	end;

    {We need to keep updating these flags to represet the lock status in the
    current context - which is the object where lock() was called. }
    if _refreshLockInfo then
    begin
	    with data do
	    begin
	        booleans[_can_edit]       := canEdit(_lockName);
	        booleans[_IS_LOCKED_BY_ME]:= lockedBy(_lockName);
	        booleans[_IS_ROW_NEW]     := isNew;
            Strings[_LOCKED_BY]       := '';      {Don't send the lock name??}

	        if booleans[_can_edit] then
	            Strings[_UNLOCK_KEY] := myLockKey
	        else
	            Strings[_UNLOCK_KEY] := ''; {Don't send unlock key if the row is not locked by _lockName}
		end;
    end;
end;

function RbJSONDbRow.canEdit(_lockName: string): boolean;
begin
    {You are allowed to edit this row if:
        - It is not locked by anyone else
        - if it is locked, then it has been locked but this _lockName}
    //Log ('canEdit()...');
    //if isLocked then log ('    isLocked: true') else log ('    isLocked: false');
    //if lockedBy(_lockName) then log ('    lockedBy(%s): true', [_lockName]) else log ('    lockedBy(%s): false', [_lockName]);
    //if isNew then log ('    isNew: true', [_lockName]) else log ('    isNew: false', [_lockName]);

    if (not isLocked) or (isNew) then
        Result := True
    else if (isLocked and lockedBy(_lockName)) then
        Result := True
    else
        Result := False;


    //Result:= not (isLocked) or (isLocked and lockedBy(_lockName)) or isNew;
end;

function RbJSONDbRow.isNew: boolean;
begin
    Result := not fileExists(myDataFiler.filePath);
end;

function RbJSONDbRow.refresh: RbJSONDbRow;
begin
    getMyData;{Force data to be reloaded}
    Result := self;
end;

function RbJSONDbRow.relinquishLock(_name: string): boolean;
begin
    if myLockName = _name then
        Result := resetLock;
end;

function RbJSONDbRow.resetLock: boolean;
begin
    Result := unlock(myLockKey);
    if Result then
    begin
        doFileSave;
    end;
end;

function RbJSONDbRow.newFilerObject: TTextFiler;
begin
    Result := TTextFiler.Create;
    Result.autorefresh := True;
end;

function RbJSONDbRow.dataClone: TDynaJSONObject;
begin
    Result := Data.Clone;
end;


function RbJSONDbRow.unlock(_key: string): boolean;
begin
    Result := False;
    if myLockKey = _key then
    begin
        doUnlock;
        Result := True; {Yes. We were able to unlock the file}
        NotifyModelOfMyChange;
    end;
end;

constructor RbJSONDbRow.Create;
begin
    inherited;

    myDataObject := nil;
    myDocMakerFunc := nil;
    myDocMakerMethod := nil;

    myEdges := RbListRowEdgeList.Create();
    myEdges.Source := self;

    myDataFiler := newFilerObject();
    myManifestFiler := newFilerObject();
end;

destructor RbJSONDbRow.Destroy;
begin
    try
        myDataObject.Free;
        myDataFiler.Free;
        myManifestFiler.Free;
        myEdges.Free;
    except
        on E: Exception do
            log3('RbJSONDbRow.Destroy:: %s', [E.Message]);
    end;
    inherited Destroy;
end;



function RbJSONDbRow.edge(_edgeName: string; _toRow: RbJSONDbRow): RbRowEdge;
var
    _edge: RbEdge;
    _key: string;
    _edgeList: RbRowEdgeList;

begin
    Result := nil;
    {Locate the edge Object from DB}
    _edge := DB.edge(_edgeName);
    if Assigned(_edge) then
    begin
        _edgeList := myEdges.get(_edge);
        if Assigned(_toRow) then
        begin
            Result := _edgeList.getEdge(_toRow);
            doSaveEdges;
            if _edge.directed then
            begin
                _toRow.edge(_edge.reverse_edge, self);
                _toRow.doSaveEdges;
            end;
        end;
        //else
        //    Trip('RbJSONDbRow.edge():: Destination row is nil');
    end;
end;

function RbJSONDbRow.edge(_edgeName: string; _model: string; _rowID: string): RbRowEdge;
begin
    Result := edge(_edgeName, DB.findRow(_model, _rowID));
end;

function RbJSONDbRow.findEdge(_toRow: RbJSONDbRow): RbRowEdge;
begin
    trip('RbJSONDbRow.findEdge() not implemented');
end;

function RbJSONDbRow.edgeRowsObj(_edgeName: string): RbJSONDbRowList;
var
    i: integer;
    _edgeList: RbRowEdgeList;
    _edge: RbRowEdge;
begin
    Result := RbJSONDbRowList.Create(False);
    _edgeList := myEdges.get(_edgeName);
    for i := 0 to pred(_edgeList.Count) do
    begin
        _edge := _edgeList.Items[i];

        if not Assigned(_edge.dest) then
        begin
            _edge.dest := DB.model(_edge.destModelName).load(_edge.destRowID);
        end;

        Result.add(_edge.dest.ID, _edge.dest);
    end;
end;

function RbJSONDbRow.edges(_edgeName: string): RbRowEdgeList;
begin
    Result := myEdges.find(_edgeName);

    if not Assigned(Result) then
        Result := RbRowEdgeList.Create; {Return an empty edge list}
end;

function RbJSONDbRow.edgeNames: TStringArray;
begin
    Result := myEdges.keys;
end;

function RbJSONDbRow.edgeCount: integer;
begin
    Result := myEdges.edgeCount;
end;

procedure RbJSONDbRow.resetEdges;
begin
    myEdges.Clear;
end;

function RbJSONDbRow.removeEdge(_edgeName: string; _toRow: RbJSONDbRow): boolean;
var
    _edge: RbEdge;
    _elst: RbRowEdgeList;
    _index: integer;
begin
    Result := False;
    if Assigned(_toRow) then
    begin
        _edge := DB.edge(_edgeName);
        if Assigned(_edge) then
        begin
            _elst := myEdges.get(_edge);
            if Assigned(_elst) then
            begin
                _index := _elst.FindIndexOf(_elst.makeKey(_toRow));
                if _index > -1 then
                begin
                    _elst.Delete(_index);
                    _toRow.doSaveEdges;
                    doSaveEdges;
                    Result := True;
                end;
            end;
        end;
    end;
end;

function RbJSONDbRow.removeEdge(_edgeName: string; _model: string;
    _toRowID: string): boolean;
begin
    Result := removeEdge(_edgeName, DB.findRow(_model, _toRowID));
end;

function RbJSONDbRow.newDocument: TDynaJSONObject;
begin
    if Assigned(myDocMakerMethod) then
        Result := myDocMakerMethod(defaultRBJSONDBDocument())

    else if Assigned(myDocMakerFunc) then
        Result := myDocMakerFunc(defaultRBJSONDBDocument())

    else
        Result := myModel.getDefaultRow();
end;


{ RbJSONDbModel }

function RbJSONDbModel.getDB: RbJSONDb;
begin
    Result := myDB;
end;

function RbJSONDbModel.getPath: string;
begin
    if myPath.isEmpty then
    begin
        if Assigned(myDB) then
            myPath := myDB.uri
        else
            myPath := ExpandFileName('');
    end;
    Result := myPath;
end;

procedure RbJSONDbModel.setDB(const _DB: RbJSONDb);
begin
    if _DB = myDB then
        exit;
    myDB := _DB;
    myFullName := '';
end;

function RbJSONDbModel.getCaption: string;
begin
    if myCaption.isEmpty then
        Result := myName
    else
        Result := myCaption;
end;

function RbJSONDbModel.getDescription: string;
begin
    if myDescription.isEmpty then
        Result := ''
    else
        Result := myDescription;
end;

procedure RbJSONDbModel.setCaption(const _caption: string);
begin
    if myCaption = _caption then
        exit;
    myCaption := _caption;
    NotifyDBOfMyChange;
end;

procedure RbJSONDbModel.setDescription(const _description: string);
begin
    if myDescription = _description then
        exit;
    myDescription := _description;
    NotifyDBOfMyChange;
end;

procedure RbJSONDbModel.setOnChange(const _OnChange: TNotifyEvent);
begin
    if myOnChange = _OnChange then
        Exit;
    myOnChange := _OnChange;
end;


procedure RbJSONDbModel.OnRowChanged(Sender: TObject);
begin
    if Sender is RBJSONDbRow then
    begin

    end;
end;

function RbJSONDbModel.getURI: string;
begin
    if myURI.isEmpty then
        myURI := AppendPath([path, Name]);
    Result := myURI;
end;

procedure RbJSONDbModel.setManifestLocation;
begin
    myModelManifestFiler.fileName := myName + JDB_MANIFEST;
    myModelManifestFiler.rootDir := path;
end;


function RbJSONDbModel.post(_id: string; _data: TJSONObject; _name: string;
    _caption: string; _description: string; _unlock: boolean;
    _freeDataObject: boolean): TDbSaveStatus;
var
    _key: string;
    _row: RBJSONDbRow;
    _rowExists: boolean;
    _prevCaption: string;
    _prevDescription, _prevName: string;
begin
    // log('RbJSONDbModel.Post():: ID= %s name= "%s" caption="%s"', [_id, name, _caption]);

    if _id.isEmpty then
        _id := _data.Strings[__IDField];

    _rowExists := rowExists(_id);

    if _id.isEmpty and _rowExists then
        log('OK! This should not happen at all!!! id is empty but it says it exists');

    if _rowExists then
        log(' -->Existing record')
    else
    begin
        log(' -->New record');
        if _id.isEmpty then _id := newID;
    end;

    _row := get(_id); {Puts this row into the list, assigns ID}

    with _row do
    begin
        if _rowExists then
            {Extract the unlock key from the data payload}
            _key := _data.Get(_UNLOCK_KEY, '')
        else
            {Get a new unlock key}
            _key := _row.lock(genRandomKey(12));

        if _row.myLockKey = _key then
        begin
            log('Getting ready to save...');

            _prevName := _row.Name;
            _prevCaption := _row.Caption;
            _prevDescription := _row.description;

            _row.Name := _name;
            _row.Caption := _caption;
            _row.description := _description;

            Result := _row.save(_data, _key, _unlock);

            if not saveSuccess(Result) then
            begin
                _row.Name := _prevName;
                _row.Caption := _prevCaption;
                _row.description := _prevDescription;
                log('Something went wrong saving. Reverting to previous state');
                log('Model: %s; ID: %s; SaveResult: %s', [ModelName, ID, result.message]);

            end
            else
            begin
                {If the row didn't exist before, not it is saved, so updated model manifest}
                if not _rowExists then
                    saveManifestRows();
            end;
        end
        else
        begin
            log('Invalid Key. Not saved');
            Result := jdbsaveInvalidKey;
        end;
    end;


    if _freeDataObject then
        _data.Free;

end;

function RbJSONDbModel.post(_row: RbJSONDbRow; _name: string;
    _caption: string; _description: string; _unlock: boolean): TDbSaveStatus;
begin
    Result := post(_row.ID, _row.Data.Clone,
        _name, _caption, _description,
        _unlock, True {<--free this data clone});
end;

function RbJSONDbModel.saveSuccess(_saveResult: TDbSaveStatus): boolean;
begin
    case _saveResult of
        jdbsaveUnknown:         {×} Result := False;
        jdbsaveNotLocked:       {×} Result := False;
        jdbsaveIsLocked:        {×} Result := False;
        jdbsaveInvalidKey:      {×} Result := False;
        jdbsaveLockExtended:    {✓} Result := True;
        jdbsaveUnlocked:        {✓} Result := True;
        jdbsaveCancelled:       {×} Result := False;
        jdbsaveError:           {×} Result := False;
        jdbSuccess:             {✓} Result := True;
        jdbFail:                {×} Result := False;
    end;
end;

function RbJSONDbModel.cancelEdit(_id: string; _userID: string): TDbSaveStatus;
var
    _row: RBJSONDbRow;
begin
    log('Cancelling edit for %s', [_id]);
    if _id.isEmpty then
        Result:= jdbSuccess

    else if rowExists(_id) then
    begin
	    _row := row(_id);
	    if _row.relinquishLock(_userID) then
	    begin
	        Result := jdbsaveCancelled;
	        log3('Cancel success');
	    end
	    else
	    begin
	        if _row.locked then
	        begin
	            Result := jdbsaveIsLocked;
	            log3('Cancel:: Row was not locked by %s', [_userID]);
	        end
	        else
	        begin
	            Result := jdbsaveNotLocked;
	            log3('Cancel:: Row was already unlocked');
	        end;
	    end;
	end
    else
        Result := jdbsaveUnknown;
end;

function RbJSONDbModel.rowUnlockKeyValid(_id: string; _unlockKey: string
	): boolean;
begin
    if _id.isEmpty then
        Result:= true

    else if rowExists(_id) then
        Result:= (row(_id).getMyLockKey = _unlockKey)

    else
        Result:= false;
end;

procedure RbJSONDbModel.saveManifestRows;
var
    bAddcomma: boolean = False;
    _rows: string = '';
    id: string;
begin
    for id in toStringArray(myRows.getNames()) do
    begin
        if rowExists(id) then
        begin
            if bAddComma then
                _rows := _rows + ','
            else
                bAddComma := True;
            _rows := _rows + id;
        end;
    end;
    myModelManifest.Strings['rows']       := _rows;
    myModelManifest.Strings['deleted']    := myDeletedRows.getNames();
    myModelManifest.Strings['searchkeys'] := myKeySearches.getNames();
    myModelManifestFiler.content          := myModelManifest.AsJSON;
end;

function RbJSONDbModel.Delete(_id: string): integer;
var
    _i: integer;
    _row: RbJSONDbRow;
begin
    Result := -2; {Not found}
    if rowExists(_id) then
    begin

        Result := -1; {Not deleted}
        _row := get(_id);

        //if _row.isLocked then {If I can see the button, it means that I have locked this. Not very clear. Refactor required!!}
        begin
            _row.Delete;
            {not used -->}
            {Clarification: We should look for the key value here
            because we are not adding the object (it is nil) we are only storing the key.
            Also, we are using this object list so that we can load the deleted rows into this list
            and manipulated them}
            //if myDeletedRows.FindIndexOf(_row.ID) = -1 then
            //   myDeletedRows.add(_row.ID, nil); {So we have the id and we can store it}
            {<--}

            {delete from the row list}
            if myRows.Delete(_row) then
            begin
                Result := 0; {Success}
            end;
            saveManifestRows;
            log('Deleted "%s"."%s"', [Name, _id]);
        end;
    end;
end;

function RbJSONDbModel.deleteAllRows: integer;
begin
    Result := rowCount;
    while rowCount > 0 do
        Delete(row(0).ID);
end;

//function RbJSONDbModel.loadDeletedRow(_id: string): integer;
//begin
//    // myDeletedRows.get(_id);
//end;

function RbJSONDbModel.deletedRowIDs: TStringArray;
begin
    Result := toStringArray(DeletedRows.getNames());
end;

function RbJSONDbModel.deletedRows: RbJSONDbRowList;
begin
    Result := myDeletedRows;
end;

function RbJSONDbModel.deletedRowCount: integer;
begin
    Result := myDeletedRows.Count;
end;

function RbJSONDbModel.deletedRow(_rowID: string): RbJSONDbRow;
begin
    if myDeletedRows.FindIndexOf(_rowID) > -1 then
    begin
        Result := loadRow(_rowID);
    end
    else
        Result := nil;
end;

function RbJSONDbModel.deletedRow(_i: integer): RbJSONDbRow;
begin
    if (_i > -1) and (_i < deletedRowCount) then
        Result := loadRow(myDeletedRows.Names[_i])
    else
        Result := nil;
end;

function RbJSONDbModel.undelete(_rowID: string): boolean;
begin
    Log('RbJSONDbModel.undelete: TODO');
    Result := False;
end;

function RbJSONDbModel.purge(_id: string): integer;
var
    _row: RbJSONDbRow;
begin
    Result := -3; {Not found}
    _row := myDeletedRows.find(_id);
    if Assigned(_row) then
    begin
        {physically delete the file}
        Result := -2; {Not physically deleted}
        if _row.Delete then
        begin
            Result := -1; {Not removed from list}
            if myDeletedRows.Delete(_id) then
            begin
                saveManifestRows;
                Result := 0; {Successfully deleted}
            end;
        end;
    end;
end;

procedure RbJSONDbModel.Close;
begin

end;

function RbJSONDbModel.modelExists: boolean;
begin
    Result := DB.modelExists(Name);
    //Log('model "%s" exists? %s.', [Name, yesNo(Result)]);
end;


procedure RbJSONDbModel.NotifyDBOfMyChange;
begin
    if Assigned(myOnChange) then
        myOnChange(Self);
end;

function RbJSONDbModel.isLocked: boolean;
begin
    Result := not myLockKey.IsEmpty;
end;

function RbJSONDbModel.isLocked(_name: string): boolean;
begin
    Result := (myLockName = _name);
end;

function RbJSONDbModel.lockedBy: string;
begin
    Result := myLockName;
end;


function RbJSONDbModel.getName: string;
begin
    Result := myName;
end;


procedure RbJSONDbModel.setName(const _name: string);
begin
    if myName = _name then
        exit;
    myName := _name;
    setManifestLocation;
    NotifyDBOfMyChange;
    myFullName := '';
end;

function RbJSONDbModel.lock(_name: string): string;
begin
    Result := '';
    if not isLocked then
    begin
        myLockName := _name;
        myLockKey := genRandomKey(12, 4);
        Result := myLockKey;
        NotifyDBOfMyChange;
    end;
end;

function RbJSONDbModel.relinquishLock(_name: string): boolean;
begin
    if myLockName = _name then
        Result := resetLock;
end;

function RbJSONDbModel.unlock(_key: string): boolean;
begin
    Result := False;
    if isLocked then
    begin
        if myLockKey = _key then
        begin
            myLockKey := '';
            myLockName := '';
            Result := True;
            NotifyDBOfMyChange;
        end;
    end;
end;

function RbJSONDbModel.resetLock: boolean;
begin
    Result := unlock(myLockKey);
end;

function RbJSONDbModel.rowCount: integer;
begin
    load;
    Result := myRows.Count;
    //Log('%s rows array = "%s"', [name, myModelManifest.Strings['rows']]);
    //Result:= Length(toStringArray(myModelManifest.Strings['rows']))
end;


function RbJSONDbModel.initializeRow(constref _row: RbJSONDbRow;
    _rowID: string): RbJSONDbRow;
begin
    if _row.path.isEmpty then
    begin
        {This means that the row hasjust been created and
        we must initialize it }
        _row.path := AppendPath([Path, Name]);
        _row.DbModel := Self;
        _row.OnChange := @OnRowChanged;
    end;

    if _row.isNew then
        _row.Initialize; {Creates a new row Object}

    if not Assigned(_row.keySearches) then
        _row.keySearches := myKeySearches;

    if _row.ID.isEmpty then
        _row.ID := _rowID;

    _row.refresh;

    Result := _row;
end;

function RbJSONDbModel.loadRow(_rowID: string): RbJSONDbRow;
begin

    if rowExists(_rowID) then
    begin
        Result := RbJSONDbRow.Create;
        Result := initializeRow(Result, _rowID);
        Result.load;
    end
    else
        Result := nil;

end;

function RbJSONDbModel.get(_rowId: string): RbJSONDbRow;
begin
    Result := myRows.get(_rowID);
    Result := initializeRow(Result, _rowId);
    if not Result.isNew then
        Result.Load;
end;

function RbJSONDbModel.get(_i: integer): RbJSONDbRow;
begin
    Result := get(getRowID(_i));
end;

function RbJSONDbModel.getRowID(_i: integer): string;
begin
    if (_i >= 0) and (_i < rowCount) then
        Result := myRows.Names[_i]
    else
        Result := '';
end;

function RbJSONDbModel.getManifestJSON: string;
begin
    Result := myModelManifest.FormatJSON();
end;

function RbJSONDbModel.updateManifestWith(_row: RbJSONDbRow): boolean;
begin
    if not myRows.exists(_row.id) then
    begin
        myRows.add(_row.id, _row);
    end;
    saveManifestRows();
end;

procedure RbJSONDbModel.addSearch(_key: string);
var
    _searchType: string;
begin
    if pos(composite_key_separator, _key) = 0 then
        _searchType := key_search
    else
        _searchType := composite_search;

    myKeySearches.put(_key, _searchType);

end;

procedure RbJSONDbModel.addSearch(_keys: TStringArray);
begin
    myKeySearches.put(buildSearchKey(_keys), composite_search);
end;

procedure RbJSONDbModel.initKeySearches;
var
    _key: string;
begin
	for _key in toStringArray(myModelManifest.Strings['searchkeys']) do
    begin
        addSearch(_key);
    end;
end;

function RbJSONDbModel.findRow(_id: string): RbJSONDbRow;
begin
    if rowExists(_id) then
        Result:= row(_id)
    else
        Result:= nil;
end;

function RbJSONDbModel.findRows(_key: string; _value: string;
    _matchPattern: boolean; _limit: integer; _offset: integer): RbJSONDbRowList;
begin
    {If for some reason this function is used to search for ID
     then don't do an Index search }

    if _key = RbJSONDbRow.IDField then
    begin
        Result := RbJSONDbRowList.Create(False);
        if rowExists(_value) then
            Result.add(_value, row(_value));
    end
    else
        Result := findRows([_key], [_value], _matchPattern, _limit, _offset);
end;

function RbJSONDbModel.findRows(_keys: TStringArray; _values: TStringArray;
    _matchPattern: boolean; _limit: integer; _offset: integer): RbJSONDbRowList;
var
    m: RbModel;
    _row: RbJSONDbRow;
    _fieldCount, i: integer;
    _key, _value: string;
begin
    Result := RbJSONDbRowList.Create(False);
    _fieldCount := length(_keys);

    if length(_values) <> _fieldCount then
    begin
        trip('RbJSONDbModel.findRows():: Number of keys not equal to number of values. Cannot search');
        {##}{ exit; }{##}
    end;


    {We are looking for a record by a non-ID Key Value pair}
    m := getModelObj(TBL_KEY_SEARCHES);
    if assigned(m) then
    begin
        m.useThisQueryObj(DB.newQuery);
        try
            with m.QBReader do
            begin
                {Check if we should limit the results}
                if (_limit = -1) and (_offset = -1) then
                    nolimit
                else
                    limit(_limit, _offset);

                {Define the search query}
                columns('rowID');
                where; {to clear the where}
                where(Format('model=%s', [QuotedStr(Name)]));

                {search key}
                where('and');
                where(Format('key=%s', [QuotedStr(buildSearchKey(_keys))]));

                {search values}
                where('and');
                if _matchPattern then
                    where(Format('value like %s',
                        [QuotedStr(buildSearchValue(_values))]))
                else
                    where(Format('value=%s', [QuotedStr(buildSearchValue(_values))]));
            end;

            with m.readQry do
            begin
                //'KEY SEARCH SQL');
                //log(m.readSQL);
                Open;
                while not EOF do
                begin
                    //log('Loading results. Found %s', [Fields[0].AsString]);
                    _row := load(Fields[0].AsString);
                    if assigned(_row) then
                    begin
                        //log('Row %s [%s]', [_row.name, _row.ID]);
                        Result.Add(_row.ID, _row);
                    end
                    else
                        log('==> KEYSEARCH:: _row was not loaded');
                    Next;
                end;
                //log('KEY SEARCH DONE');
                //log('');
            end;

        finally
            m.Free;
        end;
    end;

end;

function RbJSONDbModel.findFirst(_key: string; _value: string;
    _matchPattern: boolean): RbJSONDbRow;
begin
    Result := nil;
    with findRows(_key, _value, _matchPattern, 1) do
    begin
        if Count > 0 then
            Result := Items[0];
        Free;
    end;
end;

function RbJSONDbModel.findFirst(_key: string; _value: integer;
	_matchPattern: boolean): RbJSONDbRow;
begin
    Result:= findFirst(_key, inttostr(_value), _matchPattern);
end;

function RbJSONDbModel.findFirst(_key: string; _value: double;
	_matchPattern: boolean): RbJSONDbRow;
begin
    Result:= findFirst(_key, FloatToStr(_value), _matchPattern);
end;

function RbJSONDbModel.findFirst(_key: string; _value: boolean;
	_matchPattern: boolean): RbJSONDbRow;
begin
    {BoolToStr is what is used by TJSONData to store boolean. See doSaveKeySearches()}
    Result:= findFirst(_key, BoolToStr(_value,true), _matchPattern);
end;

function RbJSONDbModel.findFirst(_keys: TStringArray; _values: TStringArray;
	_matchPattern: boolean): RbJSONDbRow;
begin
    Result := nil;
    with findRows(_keys, _values, _matchPattern, 1) do
    begin
        if Count > 0 then
            Result := Items[0];
        Free;
    end;
end;

function RbJSONDbModel.searchFor(_whereClause: string): RbJSONDbRowList;
begin

end;

//function RbJSONDbModel.sortedRows(_sortField: string; _sortOrder: string
//    ): RbJSONDbRowList;
//var
//  m: RbModel;
//  _row: RbJSONDbRow;
//  _fieldCount, i: integer;
//begin
//    Result:= RbJSONDbRowList.Create(false);

//    if not myKeySearches.exists(_sortField) then
//    begin
//        Log('You know, this sort field is not in key searches: %s', [_sortField]);
//        exit;
//    end;

//    {We are looking for a record by a non-ID Key Value pair}
//    m:= getModelObj(TBL_KEY_SEARCHES);
//    if assigned(m) then
//    begin
//        m.useThisQueryObj(DB.newQuery);
//        try
//            with m.QBReader do
//            begin

//                {Define the search query}
//                columns('rowID, value');
//                where; {to clear the where}
//                where(Format('model=%s', [QuotedStr(name)]));

//                where('and');
//                where(Format('key=%s', [QuotedStr(_sortField)]));

//                order(' value ' + _sortOrder);
//                nolimit;

//            end;

//            with m.readQry do
//            begin

//                //log('SORT SEARCH SQL');
//                //log(m.readSQL);
//                Open;

//                while not EOF do
//                begin
//                    //log('Loading results. Found %s', [Fields[0].AsString]);
//                    _row := load(Fields[0].AsString);
//                    if assigned (_row) then
//                    begin
//                        //log('Row %s [%s]', [_row.name, _row.ID]);
//                        Result.Add(_row.ID, _row)
//                    end
//                    else
//                        log('==> SORTEDROWS:: _row was not loaded');
//                    Next;
//                end;
//                 //log('SORT ROWS DONE');
//                 //log('');
//            end;

//        finally
//            m.Free;
//        end;

//    end;
//end;

function RbJSONDbModel.sortedRows(_sortField: string; _sortOrder: string;
    _asNumber: boolean): RbJSONDbRowList;
    {IMPORTANT - CODE IS DUPLICATED from sortedRows() }
var
    m: RbModel;
    _row: RbJSONDbRow;
    _fieldCount, i: integer;
begin
    Result := RbJSONDbRowList.Create(False);

    if not myKeySearches.exists(_sortField) then
    begin
        Log('You know, this sort field is not in key searches: %s', [_sortField]);
        exit;
    end;

    {We are looking for a record by a non-ID Key Value pair}
    m := getModelObj(TBL_KEY_SEARCHES);
    if assigned(m) then
    begin
        m.useThisQueryObj(DB.newQuery);
        try
            with m.QBReader do
            begin

                {Define the search query}
                columns('rowID, value');
                nolimit;
                where; {to clear the where}
                where(Format('model=%s', [QuotedStr(Name)]));

                where('and');
                where(Format('key=%s', [QuotedStr(_sortField)]));

                if assigned(modelDef.field(_sortField)) then
                begin
                    case modelDef.field(_sortField).DataTypeCategory of
                        dtcNumeric: order(' CAST(value as float) ' + _sortOrder);
                        dtcDate: order(' julianday(value) ' + _sortOrder);
                        dtcBoolean: order(' CAST(value as boolean) ' + _sortOrder);
                        else
                            order(' value ' + _sortOrder);
                            ;
                    end;
                end
                else
                if _asNumber then
                    order(' CAST(value as float) ' + _sortOrder)
                else
                    order(' value ' + _sortOrder);

            end;

            with m.readQry do
            begin

                //log('SORT SEARCH SQL');
                //log(m.readSQL);
                Open;

                while not EOF do
                begin
                    //log('Loading results. Found %s', [Fields[0].AsString]);
                    _row := load(Fields[0].AsString);
                    if assigned(_row) then
                    begin
                        //log('Row %s [%s]', [_row.name, _row.ID]);
                        Result.Add(_row.ID, _row);
                    end
                    else
                        log('==> SORTEDROWS:: _row was not loaded because ' + format('id="%s" and value="%s"',[Fields[0].AsString, Fields[1].AsString]));
                    Next;
                end;
                //log('SORT ROWS DONE');
                //log('');
            end;

        finally
            m.Free;
        end;

    end;
end;

function RbJSONDbModel.row(_rowID: string): RbJSONDbRow;
begin
    if _rowID.isEmpty then
        Result:= nil
    else
        Result := get(_rowID);
end;

function RbJSONDbModel.rowPayload(_rowID: string; _lockName: string): TDynaJSONObject;
begin
    Log('Preparing RowPayload for _RowID: "%s"', [_rowID]);

    if _rowID.isEmpty then
    begin
        Result := modelDef.defaultRow;
        Log('Sending empty row');
        with Result do
        begin
            Booleans[_IS_ROW_NEW] := True;
            Booleans[_CAN_EDIT] := True;
            Strings[_UNLOCK_KEY] := '';
            Booleans[_IS_LOCKED_BY_ME] := False;
            Strings[_LOCKED_BY] := ''; {Don't send the lock name??}
        end;

    end
    else
        with row(_rowID) do
        begin
            Result := dataClone;
            with Result do
            begin
                Booleans[_IS_ROW_NEW] := isNew;
                Booleans[_CAN_EDIT] := canEdit(_lockName);
                Strings[_UNLOCK_KEY] := lock(_lockName);
                Booleans[_IS_LOCKED_BY_ME] := lockedBy(_lockName);
                Strings[_LOCKED_BY] := ''; {Don't send the lock name??}
            end;
        end;
end;

function RbJSONDbModel.row(_i: integer): RbJSONDbRow;
begin
    Result := get(_i);
end;

function RbJSONDbModel.rowExists(_rowID: string): boolean;
begin
    if _rowID.isEmpty then
        Result := False
    else
    begin
        {Check if a file with the ID as file name rowExists}
        Result := FileExists(AppendPath([path, Name, _rowID + JDB_EXTENSION]));
    end;
end;

function RbJSONDbModel.newID: string;
begin
    repeat
        Result := 'i' + genRandomKey(IDLength);
    until not rowExists(Result); {ensure that the id is unique}
end;

procedure RbJSONDbModel.useThisModelDef(_modelDef: RbModelDef; _freeOnDestroy: boolean);
begin
    {If modelDef is being set, then it means that the internal should be freed}
    if assigned(myModelDef) then
    begin
        if myFreeModelDef then
            myModelDef.Free;
    end;
    myModelDef := _modelDef;
    myFreeModelDef := _freeOnDestroy;
end;

function RbJSONDbModel.getModelDef: RbModelDef;
begin
    if not assigned(myModelDef) then
    begin
        myModelDef := getDefaultModelDef();
        myFreeModelDef := True;
    end;
    Result := myModelDef;
end;

function RbJSONDbModel.applyDataSchema(constref _row: RbJSONDbRow): boolean;
var
    _newRowObj: TJSONObject = nil;
begin
    Result := False;

    {This must happen always}
    convertToRBJSONDBRow(_row.Data);

    {Row generators. Order by priority}

    if assigned(rowBuilder) then {priority 1}
    begin
        //log('initDataSchema:: building from rowBuilder');
        _newRowObj := rowBuilder();
    end
    else if assigned(rowBuilderMethod) then {priority 2}
    begin
        //log('initDataSchema:: building from rowBuilderMethod');
        _newRowObj := rowBuilderMethod();
    end
    else if assigned(modelDef) then {priority 3}
    begin
        //log('initDataSchema:: building from modelDef');
        _newRowObj := modelDef.defaultRow;
    end;

    if assigned(_newRowObj) then
    begin
        //log('preparing row.data');
        copyJSONObject(_newRowObj, _row.Data);
        _newRowObj.Free;
        Result := True;
    end
    else
        log('initDataSchema did not create a new row');
end;


function RbJSONDbModel.newRow(_ID: string): RbJSONDbRow;
begin
    if _ID.IsEmpty then
        _ID := newID();
    if not rowExists(_ID) then
    begin
        Result := get(_ID);
        // applyDataSchema(Result);
        {Keep this new row in memory. Don't save it until it has been explicitly set}
        // NotifyDBOfMyChange;
        // Result.doFileSave;
    end
    else
        {The ID is already existing. So, don't return anything}
        Result := nil;
end;

function RbJSONDbModel.hardload: integer;
var
    fileList: TStringList;
    fileName: string;
begin
    fileList := FindAllFiles(uri, JDB_MANIFEST_FILTER, False);
    for fileName in fileList do
    begin
        get(ExtractFileNameOnly(fileName)).load;
    end;
    Result := fileList.Count;
    fileList.Free;
    NotifyDBOfMyChange;
end;


procedure RbJSONDbModel.setrowBuilder(const _rowBuilder: ProcJSONDataFactory);
begin
    myrowBuilder := _rowBuilder;
end;

procedure RbJSONDbModel.setrowBuilderMethod(
    const _rowBuilderMethod: ProcJSONDataFactoryMethod);
begin
    myrowBuilderMethod := _rowBuilderMethod;
end;

function RbJSONDbModel.getFullName: string;
begin
    if myFullName.IsEmpty then
    begin
        myFullName := Format('%s.%s', [DB.Name, Name]);
    end;
    Result := myFullName;
end;


function RbJSONDbModel.load: integer;
var
    rowID: string;
begin
    if myModelManifestFiler.fileChanged then
    begin
        isLoadingFromManifest := True; {prevent saving manifest with each get()}

        myRows.Clear;

        loadManifest;

        {for each id in the "strings" property, add to the list}
        for rowID in toStringArray(myModelManifest.Strings['rows']) do
        begin
            if not rowID.isEmpty then
                get(rowID); {adding to the list}
        end;

        myDeletedRows.Clear;
        for rowID in toStringArray(myModelManifest.Strings['deleted']) do
        begin
            if not rowID.isEmpty then
            begin
                myDeletedRows.add(rowID, nil);
                {We only need the list of names. No need to store the object}
            end;
        end;

        isLoadingFromManifest := False;

    end;
    Result := myRows.Count;
end;

function RbJSONDbModel.load(_dataURI: string): RbJSONDbRow;
var
    _rowID: string;
begin
    _rowID := StringReplace(ExtractFileName(_dataURI), JDB_MANIFEST,
        '', [rfReplaceAll]).Replace(JDB_EXTENSION, '');

    if rowExists(_rowID) then
    begin
        Result := get(_rowID);
        // log('DBModel.loading() : %s -> ID = %s', [name, _rowID]);
    end
    else
    begin
        Result := nil;
        // log('DBModel.load() : Alas... rowID does not exist');
    end;
end;

function RbJSONDbModel.loadFromDir: integer;
var
    fileList: TStringList;
    fileName: string;
begin
    log3('RbJSONDbModel.load() %s | %s ', [uri, JDB_MANIFEST_FILTER]);
    fileList := FindAllFiles(uri, JDB_MANIFEST_FILTER, False);
    for fileName in fileList do
    begin
        get(ExtractFileNameOnly(fileName));
        // log3('---> %s', [ExtractFileNameOnly(fileName)]);
    end;
    Result := fileList.Count;
    fileList.Free;
    NotifyDBOfMyChange;
end;

procedure RbJSONDbModel.loadManifest;
var
    jobj: TJSONData;
begin
    log3('Loading Manifest for: %s', [Name]);
    try
        if not myModelManifestFiler.content.isEmpty then
        begin
            try
                jobj := GetJSON(myModelManifestFiler.content);
                if jobj.JSONType = jtObject then
                begin
                    copyJSONObject(TJSONObject(jobj), myModelManifest);
                    initKeySearches;
                end;
            finally
                jobj.Free;
            end;
        end
        else
        begin
            writeln('Empty manifest ', myModelManifestFiler.filePath);
        end;
    except
        on E: Exception do
        begin
            log3('RbJSONDbModel.loadManifest() :: %s', [E.Message]);
        end;
    end;

end;


constructor RbJSONDbModel.Create;
begin
    inherited Create;

    myRows := RbJSONDbRowList.Create;
    myRows.modelbase := self;
    myDeletedRows := RbJSONDbRowList.Create;
    myDeletedRows.modelbase := self;

    myModelManifest := defaultModelManifest;
    myModelManifestFiler := TTextFiler.Create;
    myModelManifestFiler.autorefresh := True;
    myKeySearches := TDynamicKeyValueStore.Create;

    {For ModelDef}
    myFreeModelDef := False;
    myModelDef := nil;
    defineKeySearches;
end;

constructor RbJSONDbModel.Create(_modelName: string);
begin
    Create;
    Name := _modelName;
end;

destructor RbJSONDbModel.Destroy;
begin
    try
        myModelManifestFiler.Free;
        myModelManifest.Free;
    except
        on E: Exception do
            log3('RbJSONDbModel.Destroy (%s) 1: %s', [Name, E.Message]);
    end;

    try
        myRows.Free;
    except
        on E: Exception do
            log3('RbJSONDbModel.Destroy (%s) 2: %s', [Name, E.Message]);
    end;

    try
        myDeletedRows.Free;
    except
        on E: Exception do
            log3('RbJSONDbModel.Destroy (%s) 2.1: %s', [Name, E.Message]);
    end;

    try
        if myFreeModelDef then
            myModelDef.Free;
    except
         on E: Exception do
             log3('RbJSONDbModel.Destroy (%s) 3: %s', [Name, E.Message]);
     end;

    try
        myKeySearches.Free;
    except
        on E: Exception do
            log3('RbJSONDbModel.Destroy (%s) 4: %s', [Name, E.Message]);
    end;

    inherited;
end;

function RbJSONDbModel.getDefaultModelDef: RbModelDef;
begin
    {IF YOU CHANGE THIS, YOU HAVE TO UPDATE RbJSONDbRow.setControlFields()}
    Result := defaultRBJSONDBModelDef();
    Result.Name(self.Name);
end;

function RbJSONDbModel.getDefaultRow: TDynaJSONObject;
var
    _jData: TJSONData;
begin
    Result := modelDef.defaultRow();
    if Result.find(__model_name, _jData) then
        _jData.AsString := Self.Name;
end;

function RbJSONDbModel.defineKeySearches: boolean;
begin
    Result := False;
end;

{ RbJSONDb }

function RbJSONDb.getURI: string;
begin
    if myFullPath.isEmpty then
        myFullPath := AppendPath([myPath, myName]);

    Result := myFullPath;
end;

function RbJSONDb.initDB(_name: string; _path: string): boolean;
var
    scripts: TSQLScript;
begin
    myName := initStr(ExtractFileNameWithoutExt(_name), 'rbJSONDB');
    myPath := initStr(_path, ExpandFileName(''));
    try
        if not DirectoryExists(uri) then
            ForceDirectories(uri);

        registerModel(defModels(), RbModel);
        registerModel(defRows(), RbModel);
        registerModel(defEdges(), RbModel);
        registerModel(defEdgeList(), RbModel);
        registerModel(defKeySearches(), RbModel);

        {Create the DB}
        closeDB;
        myDB.DatabaseName := AppendPath([uri, sanitizeFileName(myName, '.db')]);
        if not FileExists(myDB.DatabaseName) then
        begin
            myDB.CreateDB;
            myDB.Connected := True;

            scripts := TSQLScript.Create(nil);
            scripts.DataBase := myDB;
            scripts.Transaction := myDB.Transaction;
            try
                myDB.StartTransaction;
                scripts.Script.Text := dbDef.getCreateSQL();
                // scripts.Script.SaveToFile();
                scripts.UseCommit := True;
                try
                    scripts.ExecuteScript;
                    myDB.Transaction.Commit;
                except
                    myDB.Transaction.Rollback;
                end;
            finally
                myDB.EndTransaction;
                scripts.Free;
            end;
        end
        else
        begin
            myDB.Connected := True;
            {We get here only if we were not previously connected. First line exists if connected = true}

            DBStartTransaction;
                (*
                    These settings allow the SQLite3 database to finish writing faster.
                    See https://www.sqlite.org/pragma.html#pragma_synchronous
                    Stanley.
                *)
            myDB.ExecuteDirect('END TRANSACTION;');
            {This is necessary workaround. https://wiki.freepascal.org/SQLite search for "Pragma and Vacuum" }

            myDB.ExecuteDirect('PRAGMA synchronous = ''1'';');
            {Allow OS to handle writing. Simplifies commit}
            myDB.ExecuteDirect('PRAGMA locking_mode = ''EXCLUSIVE'';');
            {Exclusive File lock}
            myDB.ExecuteDirect('PRAGMA journal_mode = ''WAL'';');
            {Write Ahead //Log}

            myDB.ExecuteDirect('BEGIN TRANSACTION;');
            {Lazarus workaround for executing PRAGMA. Done.}

            DBEndTransaction;

        end;

        Result := True;
    except
        on E: Exception do
        begin
            // log('RbJSONDb.initDB("%s", "%s") FAIL:: %', [_name, _path, E.Message]);
            Result := False;
        end;

    end;

end;

function RbJSONDb.closeDB: boolean;
begin

    if myDB.Connected then
    begin
        myDB.CloseTransactions;
        myDB.CloseDataSets;
        myDB.Close();
        Result := True;
    end
    else
        Result := False;

end;

function RbJSONDb.getPath: string;
begin
    Result := myPath;
end;

procedure RbJSONDb.load;
var
    Info: TSearchRec;
begin
    //writeln('loading models from path ', path);
    if FindFirst(AppendPath([path, Name, JDB_MANIFEST_FILTER]), faAnyFile, Info) = 0 then
    begin
        repeat
            //Log('loading models...');
            with Info do
            begin
                Model(StringReplace(Name, JDB_MANIFEST, '', [rfReplaceAll]));
                //Log('%40s %15.0d', [Name, Size]);
            end;
        until FindNext(info) <> 0;
        FindClose(Info);
    end;
end;



procedure RbJSONDb.save;
begin
    // myModels.save;
end;


function RbJSONDb.model(_name: string): RbJSONDbModel;
begin
    Result := model(_name, RbJSONDbModel);
end;

function RbJSONDb.model(_name: string; _modelClass: RbJSONDbModelClass): RbJSONDbModel;
begin
    if not _name.isEmpty then
        Result := myModels.get(_name, _modelClass)
    else
        Result := nil;

end;

function RbJSONDb.modelObj(_name: string): RbJSONDbModel;
begin
    Result := modelObj(_name, RbJSONDbModel);
end;

function RbJSONDb.modelObj(_name: string;
    _modelClass: RbJSONDbModelClass): RbJSONDbModel;
begin
    Result := myModels.modelObj(_name, _modelClass);
end;

function RbJSONDb.model(_i: integer): RbJSONDbModel;
begin
    Result := nil;
    if (_i >= 0) and (_i < modelCount) then
        Result := myModels.Items[_i];
end;

function RbJSONDb.modelExists(_name: string): boolean;
begin
    Result := False;
    if not _name.isEmpty then
        Result := DirectoryExists(AppendPath([uri, _name]));
end;

function RbJSONDb.findModel(_name: string): RbJSONDbModel;
begin
    Result := myModels.find(_name);
end;

function RbJSONDb.edge(_edgeName: string): RbEdge;
begin
    Result := findEdge(_edgeName);
    if not Assigned(Result) then
        Result := myEdges.edge(_edgeName);
end;

function RbJSONDb.edge(_index: integer): RbEdge;
begin
    if myEdges.validIndex(_index) then
        Result := myEdges.Items[_index]
    else
        Result := nil;
end;

function RbJSONDb.findEdge(_edgeName: string): RbEdge;
begin
    Result := myEdges.find(_edgeName);
end;

function RbJSONDb.edgeExists(_edgeName: string): boolean;
begin
    Result := Assigned(findEdge(_edgeName));
end;

function RbJSONDb.edgeCount: integer;
begin
    Result := myEdges.Count;
end;

function RbJSONDb.saveEdges: integer;
var
    m: RbModel;
    i: integer;
    _row: RbRow;
    _edge: RbEdge;
    _doSave: boolean = False;
    _deletedEdges: string = '';
    _bAddComma: boolean = False;
    _deletedEdge: string;
begin

(*
* Load records from the database [With LOCK for EDIT]
* so that the save happens to the correct set of records
*
* Build an in-memory index, so that we can assign changes to the correct record
*
* If the "edge" is already in the database, then check if we have the correct version loaded in our memory
* if it is found, increment the version
*
* If there is no edge, then add a new row to the model list
*
* After assigning all the values, run the save.
*)

    Result := -1;
    m := getSQLModel(defEdgeList.Name);
    try
        if assigned(m) then
        begin
            // m.theseColumns(m.listDataColumns);
            m.QBReader.where('').where('edge = :edge');
            m.QBInserter.onConflict('edge').doUpdate;

            for i := 0 to pred(edgeCount) do
            begin
                _edge := edge(i);

                m.readQry.ParamByName('edge').AsString := _edge.Name;
                m.loadFromDB;
                _row := m.row(0); {This will return only one row if present}

                log3('saving Edge: "%s"', [_edge.Name]);
                if not assigned(_row) then
                begin
                    _row := m.newRow;
                    {This is a new row, so we have to save}
                    _doSave := True;
                end;

                _row.store('edge', _edge.Name);
                _row.store('directed', _edge.directed);
                _row.store('reverse_edge', _edge.reverse_edge);
                _row.store('weight', _edge.weight);
                _row.store('cost', _edge.cost);
                _row.store('cardinality', Ord(_edge.cardinality));
                _row.storeJSON('properties', _edge.properties.clone as TJSONObject);

                {Check if this is a new row, in which case, we have to save}
                _doSave := _doSave or ((_row.int_val['version'] = _edge.version) and
                    (_row.hasChanged));

                if _doSave then
                begin
                    Inc(_edge.myVersion);
                    _row.store('version', _edge.version);
                    m.saveToDB();
                    Inc(Result);
                end;
            end;

            {Make a list of deleted edges}
            for _deletedEdge in myEdges.deleteEdges do
            begin
                if _bAddComma then
                    _deletedEdges := _deletedEdges + COMMA
                else
                    _bAddComma := True;
                _deletedEdges := _deletedEdges + Format('"%s"', [_deletedEdge]);
            end;

            {Delete the edtes}
            if not _deletedEdges.isEmpty then
            begin
                {Delete edges that are not in memory}
                m.QBDeleter()
                    .where('')
                    .where(Format('edge in (%s)', [_deletedEdges]));

                m.Delete();
                myEdges.resetDeletedEdges;
                log3('Deleted Model Edges: %s', [_deletedEdges]);
            end;

        end;
    finally
        m.Free;
    end;

end;

function RbJSONDb.loadEdges: integer;
var
    m: RbModel;
    _row: RbRow;
    _edge: RbEdge;
    i: integer;
begin

    Result := -1;
    m := getSQLModel(defEdgeList.Name);
    try
        if assigned(m) then
        begin
            m.QBReader.nolimit;
            m.loadFromDB;
            for i := 0 to pred(m.rowCount) do
            begin
                _row := m.row(i);
                _edge := myEdges.edge(_row.str_val['edge']);

                _edge.Name := _row.str_val['edge'];
                _edge.version := _row.qword_val['version'];
                _edge.directed := _row.bool_val['directed'];
                _edge.reverse_edge := _row.str_val['reverse_edge'];
                _edge.weight := _row.qword_val['weight'];
                _edge.cost := _row.qword_val['cost'];
                _edge.cardinality := RbDbEdgeCardinality(_row.qword_val['cardinality']);
                _edge.properties := _row.jobj_val['properties'];
                _edge.clearChanges; {To set this as the reference point}
                Inc(Result);
            end;
        end;
    finally
        myLoadedEdgeCount := Result;
        m.Free;
    end;
end;

function RbJSONDb.removeEdge(_edgeName: string): boolean;
begin
    Result := myEdges.Delete(_edgeName);
end;

function RbJSONDb.clearEdges: boolean;
begin
    Result := True;
    myEdges.Clear;
end;


procedure RbJSONDb.OnModelChange(Sender: TObject);
begin

end;

function RbJSONDb.getSQLModel(_name: string): RbModel;
begin
    Result := getModelObj(_name);
    if Assigned(Result) then
        Result.useThisQueryObj(newQuery);
end;



procedure RbJSONDb.DBStartTransaction;
begin
    if not myDB.Transaction.Active then
    begin
        myDB.StartTransaction;
    end;
end;

procedure RbJSONDb.DBCommit;
begin
    if myDB.Transaction.Active then
        myDB.Transaction.Commit;
end;

procedure RbJSONDb.DBRollback;
begin
    if myDB.Transaction.Active then
        myDB.Transaction.Rollback;
end;

procedure RbJSONDb.DBEndTransaction;
begin
    if myDB.Transaction.Active then
        myDB.EndTransaction;
end;

function RbJSONDb.newQuery: TSQLQuery;
begin
    Result := TSQLQuery.Create(nil);
    with Result do
    begin
        DataBase := myDB;
        Transaction := myDB.Transaction;
    end;
end;


function RbJSONDb.modelCount: integer;
begin
    Result := myModels.Count;
end;

function RbJSONDb.findRow(_model: string; _rowID: string): RbJSONDbRow;
var
    _m: RbJSONDbModel;
begin
    Result := nil;
    _m := findModel(_model);
    if assigned(_m) then
        Result := _m.load(_rowID);
end;

constructor RbJSONDb.Create(_name: string; _path: string);
begin
    inherited Create;
    myDB := TSQLite3Connection.Create(nil);
    myDb.Transaction := TSQLTransaction.Create(myDB);
    myModels := RbJSONModelList.Create;
    myEdges := RbEdgeList.Create;
    myModels.DB := self;
    myManifestFileAge := FILE_NOT_READ;
    initDB(_name, _path);
    Log('Creating DB: "%s"', [_name]);
end;

destructor RbJSONDb.Destroy;
begin
    try
        if myLoadedEdgeCount <> edgeCount then
            saveEdges;

        myModels.Free;
        myDB.Free;
        myEdges.Free;

        Log('Closing DB: "%s"', [Name]);

    except
        on E: Exception do
            log3('RbJSONDb.Destroy 1: %s', [E.Message]);
    end;
    inherited Destroy;
end;

function RbJSONDb.DBCreateSQL: string;
var
    i: integer;
begin
    Result := '';
    for i := 0 to pred(modelCount) do
    begin
        with model(i) do
        begin
            Result := Result + sLineBreak + modelDef.getCreateSQL();
        end;
    end;
end;

{ RbJSONModelList }

function RbJSONModelList.get(const s: shortstring): RbJSONDbModel;
begin
    Result := get(s, RbJSONDbModel);
end;

function RbJSONModelList.get(const s: shortstring; M: RbJSONDbModelClass): RbJSONDbModel;
begin
    {searching for the object}
    Result := find(s);
    if not Assigned(Result) then
    begin
        Result := modelObj(s, M);
        Add(s, Result);
    end;
end;

function RbJSONModelList.modelObj(_name: string): RbJSONDbModel;
begin
    Result := modelObj(_name, RbJSONDbModel);
end;

function RbJSONModelList.modelObj(_name: string;
    _modelClass: RbJSONDbModelClass): RbJSONDbModel;
begin
    Result := _modelClass.Create;
    Result.DB := DB;
    Result.OnChange := @DB.OnModelChange;
    Result.Name := _name;
    Result.load;
end;

procedure RbJSONModelList.Delete(Index: integer);
begin
    inherited Delete(Index);
end;

finalization
    myDbdef.Free;
end.
