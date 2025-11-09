unit sugar.safeJson;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, fpJSON, jsonparser;

const
    EXT_JSON = '.json';

type

	{ TSafeJSONArray }

    TSafeJSONArray = class(TJSONArray)
    private
        myLock: TMultiReadExclusiveWriteSynchronizer;
		mythreadSafe: boolean;
        procedure setthreadSafe(const _value: boolean);

    public
        constructor Create; virtual;
        constructor CreateSafe; virtual;

        constructor CreateFrom(const _jsonStr: string); virtual;
        constructor CreateSafeFrom(const _jsonStr: string); virtual;
        function loadFrom(_jsonStr: string): boolean; virtual;

        destructor Destroy; override;

        // initializes all fields.
        // override this in children.
        // This will be called in the constructor
        procedure initFields; virtual;

    public
        procedure lockRead; inline;
        procedure unlockRead; inline;
        procedure lockWrite; inline;
        procedure unlockWrite; inline;
        procedure assignFrom(const Src: TJSONArray); virtual;

        // Optional: expose explicit locking for multi-step operations
        property _Lock: TMultiReadExclusiveWriteSynchronizer read myLock;
        property threadSafe : boolean read mythreadSafe; // True if CreateSafe... constructors were called. _Lock used.
	end;

    { Thread-safe base with RW lock }

	{ TSafeJSONObject }

    TSafeJSONObject = class(TJSONObject)
    public
    const
        __classID = 'classID';
    private
        myLock: TMultiReadExclusiveWriteSynchronizer;
		mythreadSafe: boolean;
        myChanges : set of Byte; // assuming that we won't have a class with more than 255 members

		procedure setthreadSafe(const _value: boolean);

    public
        // thread safe setters and getters
        function getStr(const _key: string): string;
        function setStr(const _key: string; const _value: TJSONStringType): TJSONStringType;

        function getBool(const _key: string): boolean;
        function setBool(const _key: string; const _value: boolean): boolean;

        function getFloat(const _key: string): TJSONFloat;
        function setFloat(const _key: string; const _value: TJSONFloat): TJSONFloat;

		function getUnicodeString(const _key: string): TJSONUnicodeStringType;
		function setUnicodeString(const _key: string; const _value: TJSONUnicodeStringType): TJSONUnicodeStringType;

		function getInt64(const _key: string): int64;
		function setInt64(const _key: string; const _value: int64): int64;

		function getQWord(const _key: string): QWord;
		function setQWord(const _key: string; const _value: QWord): QWord;

		function getInt(const _key: string): integer;
		function setInt(const _key: string; const _value: integer): integer;

		function getLargeInt(const _key: string): TJSONLargeInt;
		function setLargeInt(const _key: string; const _value: TJSONLargeInt): TJSONLargeInt;

        // LIMITATION: Array member changes are not safe and
        // changes to members are not detected.
		function getArray(const _key: string): TJSONArray;
		function setArray(const _key: string; constref _value: TJSONArray): TJSONArray;

        function getObject(const _key: string): TSafeJSONObject;
		function setObject(const _key: string; constref _value: TSafeJSONObject): TSafeJSONObject;

        procedure setChanged(_key: string);
        function hasChanged: boolean;
        function changedFields: TStringArray;
        procedure clearChanges;

    public
        class function classID: string; virtual;

        constructor Create; virtual;
        constructor CreateSafe; virtual;

        constructor CreateFrom(const _jsonStr: string); virtual;
        constructor CreateSafeFrom(const _jsonStr: string); virtual;
        function loadFrom(_jsonStr: string): boolean; virtual;

        destructor Destroy; override;

        // initializes all fields.
        // override this in children.
        // This will be called in the constructor
        procedure initFields; virtual;

    public
        procedure lockRead; inline;
        procedure unlockRead; inline;
        procedure lockWrite; inline;
        procedure unlockWrite; inline;
        procedure assignFrom(const Src: TJSONObject); virtual;

        // Optional: expose explicit locking for multi-step operations
        property _Lock: TMultiReadExclusiveWriteSynchronizer read myLock;
        property threadSafe : boolean read mythreadSafe; // True if CreateSafe... constructors were called. _Lock used.


    end;

    { Enumerator that holds a READ lock for the duration of the loop }

	{ TLockedEnumerator }

    generic TLockedEnumerator<ItemObj: TSafeJSONObject> = class
    private
        myOwnerLock: TMultiReadExclusiveWriteSynchronizer;
        myArr: TJSONArray;
        FIdx: integer;
        myFreeArray: boolean;
    public
        constructor Create(constref _arr: TJSONArray;
            constref _parentLock: TMultiReadExclusiveWriteSynchronizer = nil;
            const _freeArray: boolean = false);

        destructor Destroy; override;
        function MoveNext: boolean;
        function GetCurrent: ItemObj;
        property Current: ItemObj read GetCurrent;
    end;

    { Thread-safe generic collection over TSTSBase descendants }

const
    __items = 'items';

type
	{ TSafeJSONObjectCollection }
    generic TSafeJSONObjectCollection<ItemObj: TSafeJSONObject> = class
    public type
        TItemClass = class of ItemObj;
        TSafeJSONObjectCollectionsEnumerator = class(specialize TLockedEnumerator<ItemObj>);


    protected
        myJSONContainer : TSafeJSONObject;
        function GetItem(Index: integer): ItemObj;
        procedure SetItem(Index: integer; const AValue: ItemObj);

    public
        constructor Create(constref _container: TSafeJSONObject); virtual;
        destructor Destroy; override;
        procedure setItems(_items: TJSONArray);
        procedure initFields;
        function itemCount: integer;
        procedure AddItem(const AItem: ItemObj);
        function AddNewItem: ItemObj; virtual;
        procedure Insert(Index: integer; const AItem: ItemObj);
        procedure Delete(Index: integer);
        procedure Clear;

        function indexOf(_item: ItemObj): integer;

        function formatJSON(Options : TFormatOptions = DefaultFormat; Indentsize : Integer = DefaultIndentSize) : TJSONStringType;

        // for..in with lock held throughout
        function GetEnumerator: TSafeJSONObjectCollectionsEnumerator; reintroduce; overload;
        // Alternative: safe snapshot if you prefer no held lock (then deep-clone items here).

    public
        property items[index: integer] : ItemObj read GetItem write SetItem;
    end;

{
property id: string read getId write setId;
property name: string read getName write setName;
property description: string read getDescription write setDescription;
property order: integer read getOrder write setOrder;

}

implementation

uses sugar.logger, sugar.jsonlib;

{ TSafeJSONArray }

procedure TSafeJSONArray.setthreadSafe(const _value: boolean);
begin
    mythreadSafe := _value;
end;

constructor TSafeJSONArray.Create;
begin
    inherited Create;
    myLock := TMultiReadExclusiveWriteSynchronizer.Create;
    mythreadSafe := false;
    initFields;
end;

constructor TSafeJSONArray.CreateSafe;
begin
    Create;
    mythreadSafe := true;
end;

constructor TSafeJSONArray.CreateFrom(const _jsonStr: string);
begin
    Create;
    loadFrom(_jsonStr)
end;

constructor TSafeJSONArray.CreateSafeFrom(const _jsonStr: string);
begin
    CreateFrom(_jsonStr);
    mythreadSafe := true;
end;

function TSafeJSONArray.loadFrom(_jsonStr: string): boolean;
var
    J: TJSONData = nil;
    _arr: TJSONArray = nil;
begin
    Result := false;
    try
        if Trim(_jsonStr) = '' then
            raise Exception.CreateFmt('%s.CreateFrom():: empty JSON', [ClassName]);
        //log('=============================================================================================');
        //log('TSafeJSONObject.loadFrom():: "%s"',[_jsonStr]);
        //log('=============================================================================================');
        J := GetJSON(_jsonStr);
        if not (J is TJSONArray) then
            raise Exception.CreateFmt('%s.CreateFrom():: JSON must be an array', [ClassName]);

        _arr := TJSONArray(J);
        assignFrom(_arr); // you can override this for custom assignment
        Result := true;

    finally
        J.Free;
    end;
end;

destructor TSafeJSONArray.Destroy;
begin
    FreeAndNil(myLock);
	inherited Destroy;
end;

procedure TSafeJSONArray.initFields;
begin

end;

procedure TSafeJSONArray.lockRead;
begin
    if threadSafe then
        myLock.BeginRead;
end;

procedure TSafeJSONArray.unlockRead;
begin
    if threadSafe then
        myLock.EndRead;
end;

procedure TSafeJSONArray.lockWrite;
begin
     if threadSafe then
        myLock.BeginWrite;
end;

procedure TSafeJSONArray.unlockWrite;
begin
     if threadSafe then
         myLock.EndWrite;
end;

procedure TSafeJSONArray.assignFrom(const Src: TJSONArray);
begin
     lockWrite; // Caller should hold WRITE lock.
     try
         copyJSONArray(src, self);
     finally
         unlockWrite;
 	 end;
end;

{ ===== TSafeJSONObject ===== }

procedure TSafeJSONObject.setthreadSafe(const _value: boolean);
begin
	if mythreadSafe=_value then Exit;
	mythreadSafe:=_value;
end;

procedure TSafeJSONObject.lockRead; inline;
begin
    if threadSafe then
        myLock.BeginRead;
end;

procedure TSafeJSONObject.unlockRead; inline;
begin
    if threadSafe then
        myLock.EndRead;
end;

procedure TSafeJSONObject.lockWrite; inline;
begin
    if threadSafe then
        if not myLock.BeginWrite then
            raise Exception.Create('TSafeJSONObject could not lock for writing');
end;

procedure TSafeJSONObject.unlockWrite; inline;
begin
    if threadSafe then
        myLock.EndWrite;
end;

class function TSafeJSONObject.classID: string;
begin
    Result := ClassName;
end;


procedure TSafeJSONObject.assignFrom(const Src: TJSONObject);
begin
    if (Src.IndexOfName(__classID) = -1) or  (Src.Strings[__classID] <> classID) then
        raise Exception.CreateFmt(
            '%s.CreateFrom():: ClassName mismatch (got "%s", need "%s")',
            [ClassName, Src.Strings[__classID], classID]);

    lockWrite; // Caller should hold WRITE lock.
    try
        copyJSONObject(src, self);
    finally
        unlockWrite;
	end;
end;

function TSafeJSONObject.getStr(const _key: string): string;
begin
    lockRead;
    try
        Result := strings[_key];
	finally
        unLockRead;
	end;
end;

function TSafeJSONObject.setStr(const _key: string;
	const _value: TJSONStringType): TJSONStringType;
begin
    lockWrite;
    try
        strings[_key] := _value;
        Result := _value;
        setChanged(_key);
	finally
        unLockWrite;
	end;
end;

function TSafeJSONObject.getBool(const _key: string): boolean;
begin
    lockRead;
    try
        Result := Booleans[_key];
        setChanged(_key);
	finally
        unLockRead;
	end;
end;

function TSafeJSONObject.setBool(const _key: string; const _value: boolean
	): boolean;
begin
    lockWrite;
    try
        Booleans[_key] := _value;
        Result := _value;
        setChanged(_key);
	finally
        unLockWrite;
	end;
end;

function TSafeJSONObject.getFloat(const _key: string): TJSONFloat;
begin
    lockRead;
    try
        Result := Floats[_key];
	finally
        unLockRead;
	end;
end;

function TSafeJSONObject.setFloat(const _key: string; const _value: TJSONFloat
	): TJSONFloat;
begin
    lockWrite;
    try
        Floats[_key] := _value;
        Result := _value;
        setChanged(_key);
	finally
        unLockWrite;
	end;
end;

function TSafeJSONObject.getUnicodeString(const _key: string
	): TJSONUnicodeStringType;
begin
    lockRead;
    try
        Result := Unicodestrings[_key];
	finally
        unLockRead;
	end;
end;

function TSafeJSONObject.setUnicodeString(const _key: string;
	const _value: TJSONUnicodeStringType): TJSONUnicodeStringType;
begin
    lockWrite;
    try
        UnicodeStrings[_key] := _value;
        Result := _value;
        setChanged(_key);
	finally
        unLockWrite;
	end;
end;

function TSafeJSONObject.getInt64(const _key: string): int64;
begin
    lockRead;
    try
        Result := Int64s[_key];
	finally
        unLockRead;
	end;
end;

function TSafeJSONObject.setInt64(const _key: string; const _value: int64
	): int64;
begin
    lockWrite;
    try
        Int64s[_key] := _value;
        Result := _value;
        setChanged(_key);
	finally
        unLockWrite;
	end;
end;

function TSafeJSONObject.getQWord(const _key: string): QWord;
begin
    lockRead;
    try
        Result := QWords[_key];
	finally
        unLockRead;
	end;
end;

function TSafeJSONObject.setQWord(const _key: string; const _value: QWord
	): QWord;
begin
    lockWrite;
    try
        QWords[_key] := _value;
        Result := _value;
        setChanged(_key);
	finally
        unLockWrite;
	end;
end;

function TSafeJSONObject.getInt(const _key: string): integer;
begin
    lockRead;
    try
        Result := Integers[_key];
	finally
        unLockRead;
	end;
end;

function TSafeJSONObject.setInt(const _key: string; const _value: integer
	): integer;
begin
    lockWrite;
    try
        Integers[_key] := _value;
        Result := _value;
        setChanged(_key);
	finally
        unLockWrite;
	end;
end;

function TSafeJSONObject.getLargeInt(const _key: string): TJSONLargeInt;
begin
    lockRead;
    try
        Result := LargeInts[_key];
	finally
        unLockRead;
	end;
end;

function TSafeJSONObject.setLargeInt(const _key: string;
	const _value: TJSONLargeInt): TJSONLargeInt;
begin
    lockWrite;
    try
        LargeInts[_key] := _value;
        Result := _value;
        setChanged(_key);
	finally
        unLockWrite;
	end;
end;

function TSafeJSONObject.getArray(const _key: string): TJSONArray;
begin
    lockRead;
    try
        Result := Arrays[_key];
	finally
        unLockRead;
	end;
end;

function TSafeJSONObject.setArray(const _key: string; constref
	_value: TJSONArray): TJSONArray;
begin
    lockWrite;
    try
        Arrays[_key] := _value;
        Result := _value;
        setChanged(_key);
	finally
        unLockWrite;
	end;
end;

function TSafeJSONObject.getObject(const _key: string): TSafeJSONObject;
begin
    lockRead;
    try
        Result := TSafeJSONObject(Objects[_key]);
	finally
        unLockRead;
	end;
end;

function TSafeJSONObject.setObject(const _key: string; constref
	_value: TSafeJSONObject): TSafeJSONObject;
begin
    lockWrite;
    try
        Objects[_key] := _value;
        Result := _value;
        setChanged(_key);
	finally
        unLockWrite;
	end;
end;

procedure TSafeJSONObject.setChanged(_key: string);
begin
    Include(myChanges, IndexOfName(_key));
end;

function TSafeJSONObject.hasChanged: boolean;
begin
    lockRead;
    try
        Result := not (myChanges = []);
	finally
        unlockRead;
	end;
end;

function TSafeJSONObject.changedFields: TStringArray;
var
    _count, _fIndex : byte;

begin
    lockRead;
    try
	    Result := [];
	    _count := 0;
	    SetLength(Result, Count);
	    for _fIndex in myChanges do begin
	        Result[_count] := Names[_fIndex];
	        inc(_count);
		end;
	    SetLength(Result, _count);
	finally
        unlockRead;
	end;
end;

procedure TSafeJSONObject.clearChanges;
begin
    lockWrite;
    try
        myChanges := [];
	finally
        unlockWrite;
	end;
end;

constructor TSafeJSONObject.Create;
begin
    inherited Create;
    //log('TSafeJSONObject:::  %s.Create', [ClassName]);
    myLock := TMultiReadExclusiveWriteSynchronizer.Create;
    mythreadSafe := false;
    initFields;
end;

constructor TSafeJSONObject.CreateSafe;
begin
    Create;
    mythreadSafe := true;
end;

constructor TSafeJSONObject.CreateFrom(const _jsonStr: string);
begin
    Create;
    loadFrom(_jsonStr)
end;

constructor TSafeJSONObject.CreateSafeFrom(const _jsonStr: string);
begin
    CreateFrom(_jsonStr);
    mythreadSafe := true;
end;

function TSafeJSONObject.loadFrom(_jsonStr: string): boolean;
var
    J: TJSONData = nil;
    Obj: TJSONObject = nil;
begin
    Result := false;
    try
        if Trim(_jsonStr) = '' then
            raise Exception.CreateFmt('%s.CreateFrom():: empty JSON', [ClassName]);
        //log('=============================================================================================');
        //log('TSafeJSONObject.loadFrom():: "%s"',[_jsonStr]);
        //log('=============================================================================================');
        J := GetJSON(_jsonStr);
        if not (J is TJSONObject) then
            raise Exception.CreateFmt('%s.CreateFrom():: JSON must be object', [ClassName]);
        Obj := TJSONObject(J);
        assignFrom(Obj); // you can override this for custom assignment
        Result := true;

    finally
        J.Free;
    end;

end;

destructor TSafeJSONObject.Destroy;
begin
    FreeAndNil(myLock);
    inherited Destroy;
end;

procedure TSafeJSONObject.initFields;
begin
    //log('%s.initFields', [classID]);
    myChanges := [];
    setStr(__classID, classID);
end;

{ ===== TLockedEnumerator<ItemObj> ===== }

constructor TLockedEnumerator.Create(constref _arr: TJSONArray; constref
	_parentLock: TMultiReadExclusiveWriteSynchronizer; const _freeArray: boolean);
begin
    inherited Create;
    myArr := _arr;
    myOwnerLock := _parentLock;
    myFreeArray := _freeArray;
    FIdx := -1;

    if myOwnerLock <> nil then
        myOwnerLock.BeginRead; // hold read lock for the whole iteration
end;

destructor TLockedEnumerator.Destroy;
begin
    if myFreeArray then
        myArr.Free;
    if myOwnerLock <> nil then
        myOwnerLock.EndRead;
    inherited Destroy;
end;

function TLockedEnumerator.MoveNext: boolean;
begin
    Result := (myArr <> nil) and (FIdx < myArr.Count - 1);
    if Result then Inc(FIdx);
end;

function TLockedEnumerator.GetCurrent: ItemObj;
begin
    Result := ItemObj(TJSONObject(myArr.Items[FIdx]));
end;

{ ===== TSafeJSONObjectCollection<ItemObj> ===== }


constructor TSafeJSONObjectCollection.Create(constref
	_container: TSafeJSONObject);
begin
    inherited Create;
    myJSONContainer := _container;
    initFields;
end;


destructor TSafeJSONObjectCollection.Destroy;
begin
	inherited Destroy;
end;

procedure TSafeJSONObjectCollection.setItems(_items: TJSONArray);
begin
    myJSONContainer.setArray(__items, _items);
end;

procedure TSafeJSONObjectCollection.initFields;
begin
    if myJSONContainer.IndexOfName(__items) = -1 then
        myJSONContainer.setArray(__items, TJSONArray.Create);
end;

function TSafeJSONObjectCollection.itemCount: integer;
begin
    Result := myJSONContainer.getArray(__items).Count;
end;

procedure TSafeJSONObjectCollection.AddItem(const AItem: ItemObj);
begin
    myJSONContainer.getArray(__items).Add(AItem);
end;

function TSafeJSONObjectCollection.AddNewItem: ItemObj;
begin
    Result := TItemClass.CreateSafe;
    myJSONContainer.getArray(__items).Add(Result);
end;

procedure TSafeJSONObjectCollection.Insert(Index: integer; const AItem: ItemObj);
begin
    myJSONContainer.getArray(__items).Insert(Index, AItem);
end;

procedure TSafeJSONObjectCollection.Delete(Index: integer);
begin
    myJSONContainer.getArray(__items).Delete(Index);
end;

procedure TSafeJSONObjectCollection.Clear;
begin
    myJSONContainer.getArray(__items).Clear;
end;

function TSafeJSONObjectCollection.indexOf(_item: ItemObj): integer;
begin
    Result := myJSONContainer.getArray(__items).IndexOf(_item);
end;

function TSafeJSONObjectCollection.formatJSON(Options: TFormatOptions;
	Indentsize: Integer): TJSONStringType;
begin
    Result := myJSONContainer.FormatJSON(Options, Indentsize);
end;

function TSafeJSONObjectCollection.GetItem(Index: integer): ItemObj;
var
	_tmp: TJSONData;
	_arr: TJSONArray;
begin
     _tmp := myJSONContainer.getArray(__items).Items[Index];
    if not (_tmp is ItemObj) then begin
        _arr := myJSONContainer.getArray(__items);
        _tmp := TItemClass.CreateSafeFrom(_tmp.FormatJSON());
        _arr.Insert(index, _tmp);
        _arr.Delete(succ(Index));
	end;
    Result := ItemObj(_tmp);
    exit;

    if (Index < 0) or (Index >= myJSONContainer.getArray(__items).Count) then
        Result := nil
    else
        Result := ItemObj(myJSONContainer.getArray(__items).Items[Index]);
end;

procedure TSafeJSONObjectCollection.SetItem(Index: integer; const AValue: ItemObj);
begin
    myJSONContainer.getArray(__items).Items[Index] := AValue;
end;

function TSafeJSONObjectCollection.GetEnumerator: TSafeJSONObjectCollectionsEnumerator;
begin
    // Hold READ lock across enumeration to prevent concurrent mutation of array
    //if threadSafe then
    //    Result := TSafeJSONObjectCollectionsEnumerator.Create(myJSONContainer.getArray(__items), _Lock)
    //else
    Result := TSafeJSONObjectCollectionsEnumerator.Create(myJSONContainer.getArray(__items), nil); // No lock
end;

end.
