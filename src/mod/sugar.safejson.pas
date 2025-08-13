unit sugar.safeJson;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, fpJSON, jsonparser;

type

    { Thread-safe base with RW lock }

	{ TSafeJSONObject }

    TSafeJSONObject = class(TJSONObject)
    public
    const
        __classID = 'classID';
    private
        myLock: TMultiReadExclusiveWriteSynchronizer;
		mythreadSafe: boolean;
		procedure setthreadSafe(const _value: boolean);
    protected
        procedure lockRead; inline;
        procedure unlockRead; inline;
        procedure lockWrite; inline;
        procedure unlockWrite; inline;
        procedure assignFrom(const Src: TJSONObject); virtual;
    protected
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

		function getArray(const _key: string): TJSONArray;
		function setArray(const _key: string; constref _value: TJSONArray): TJSONArray;

        function getObject(const _key: string): TSafeJSONObject;
		function setObject(const _key: string; constref _value: TSafeJSONObject): TSafeJSONObject;

    public
        class function classID: string; virtual;

        constructor Create; virtual;
        constructor CreateSafe; virtual;

        constructor CreateFrom(const _jsonStr: string); virtual;
        constructor CreateSafeFrom(const _jsonStr: string); virtual;

        destructor Destroy; override;

        // initializes all fields.
        // override this in children.
        // This will be called in the constructor
        procedure initFields; virtual;

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

	{ TSafeJSONObjectCollection }

    generic TSafeJSONObjectCollection<ItemObj: TSafeJSONObject> = class
    public type
        TItemClass = class of ItemObj;
        TSafeJSONObjectCollectionsEnumerator = class(specialize TLockedEnumerator<ItemObj>);
    const
        __items = 'items';
    protected
        myJSONContainer : TSafeJSONObject;
        function GetItem(Index: integer): ItemObj;
        procedure SetItem(Index: integer; const AValue: ItemObj);
    public
        constructor Create(constref _container: TSafeJSONObject);
        destructor Destroy; override;

        procedure initFields;

        function itemCount: integer;
        procedure AddItem(const AItem: ItemObj);
        function AddNewItem: ItemObj; virtual;
        procedure Insert(Index: integer; const AItem: ItemObj);
        procedure Delete(Index: integer);
        procedure Clear;

        function formatJSON(Options : TFormatOptions = DefaultFormat; Indentsize : Integer = DefaultIndentSize) : TJSONStringType;

        // for..in with lock held throughout
        function GetEnumerator: TSafeJSONObjectCollectionsEnumerator; reintroduce; overload;
        // Alternative: safe snapshot if you prefer no held lock (then deep-clone items here).
    end;

{
property id: string read getId write setId;
property name: string read getName write setName;
property description: string read getDescription write setDescription;
property order: integer read getOrder write setOrder;

}

implementation

uses sugar.logger;
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
        myLock.BeginWrite;
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
var
    i: integer;
    key: string;
    S, D: TJSONData;
begin
    // Caller should hold WRITE lock.
    for i := 0 to Pred(Count) do
    begin
        key := Names[i];
        if SameText(key, __classID) then Continue;
        if Src.IndexOfName(key) > -1 then
        begin
            D := Elements[key];
            S := Src.Elements[key];
            if (D.JSONType = jtNumber) and (S.JSONType = jtNumber) then
                Elements[key] := S.Clone
            else if D.ClassType = S.ClassType then
                Elements[key] := S.Clone;
        end;
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
	finally
        unLockWrite;
	end;
end;

function TSafeJSONObject.getBool(const _key: string): boolean;
begin
    lockRead;
    try
        Result := Booleans[_key];
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
	finally
        unLockWrite;
	end;
end;

constructor TSafeJSONObject.Create;
begin
    inherited Create;
    log('TSafeJSONObject:::  %s.Create', [ClassName]);
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
var
    J: TJSONData = nil;
    Obj: TJSONObject = nil;
begin
    Create;
    try
        if Trim(_jsonStr) = '' then
            raise Exception.CreateFmt('%s.CreateFrom():: empty JSON', [ClassName]);

        J := GetJSON(_jsonStr);
        if not (J is TJSONObject) then
            raise Exception.CreateFmt('%s.CreateFrom():: JSON must be object', [ClassName]);

        Obj := TJSONObject(J);
        lockWrite;
        try
            if (Obj.IndexOfName(__classID) = -1) or
                (Obj.Strings[__classID] <> classID) then
                raise Exception.CreateFmt(
                    '%s.CreateFrom():: ClassName mismatch (got "%s", need "%s")',
                    [ClassName, Obj.Strings[__classID], classID]);
            assignFrom(Obj);
        finally
            unlockWrite;
        end;
    finally
        J.Free;
    end;
end;

constructor TSafeJSONObject.CreateSafeFrom(const _jsonStr: string);
begin
    CreateFrom(_jsonStr);
    mythreadSafe := true;
end;

destructor TSafeJSONObject.Destroy;
begin
    FreeAndNil(myLock);
    inherited Destroy;
end;

procedure TSafeJSONObject.initFields;
begin
    log('TSafeJSONObject.initFields');
    lockWrite;
    try
        strings[__classID] := classID;
	finally
        unlockWrite
	end;
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

procedure TSafeJSONObjectCollection.initFields;
begin
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
    Result := TItemClass.Create;
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

function TSafeJSONObjectCollection.formatJSON(Options: TFormatOptions;
	Indentsize: Integer): TJSONStringType;
begin
    Result := myJSONContainer.FormatJSON(Options, Indentsize);
end;

function TSafeJSONObjectCollection.GetItem(Index: integer): ItemObj;
begin
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
