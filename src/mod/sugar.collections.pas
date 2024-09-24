unit sugar.collections;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
    Classes, SysUtils, contnrs, fgl;

const
    NOINDEX = -9928;


type
    TVariantArray = array of variant;

    TIntegerListBase = specialize TFPGList<integer>;

    { TIntegerList }
    TIntegerList = class(TIntegerListBase)
        procedure sort; overload;
    end;

    {String variable encapsulated in a class}

    { TStringObject }
    TStringObject = class
    public
        Value: string;
        function add(_text: string): string; {appends}
        function isEmpty: boolean;
        function length: integer;
        procedure Clear;
        constructor Create;
    end;

    { TIntegerObject }

    TIntegerObject = class
    public
        Value: integer;
        constructor Create;
    end;

type
    { GenericHashList }
    generic GenericHashList<PtrType: TObject> = class(TFPHashList)
    private
        myautoFree: boolean;
        function getValue(_key: string): PtrType;
        procedure setValue(_key: string; const _value: PtrType);
        procedure setautoFree(const _autoFree: boolean);
        function getName(index: integer): shortstring;
        function getIndexValue(index: integer): PtrType;
        procedure setIndexValue(index: integer; const _indexValue: PtrType);
    public
        property Name[index: integer]: shortstring read getName;
        property indexValue[index: integer]: PtrType read getIndexValue write setIndexValue;
        property Value[_key: string]: PtrType read getValue write setValue;
        {default is true}
        property autoFree: boolean read myautoFree write setautoFree;
        {adds the pointer. if autofree is true, will free the object on destroy}
        function names: TStringArray;
        function valueOf(_i: integer): PtrType;
        function add(const _key: shortstring; Item: PtrType): integer; reintroduce;
        function find(const AName: shortstring): ptrType; reintroduce;
        constructor Create;
        destructor Destroy; override;


    end;
    TFPHashObjectListClass = class of TFPHashObjectList;
    { GenericHashObjectList }
    generic GenericHashObjectList<GObj> = class(TFPHashObjectList)
	public type
        TObjectConstructor = function: GObj of object;

    protected
        myCurrentIndex: integer;
        myInWhileLoop: boolean;
        myonFind: TNotifyEvent;
        myonCreateObj: TNotifyEvent;
        myObjConstructor: TObjectConstructor;
        procedure setonFind(const _onFind: TNotifyEvent);
        procedure setonCreateObj(const _onCreateObj: TNotifyEvent);
        function getName(Index: integer): string;
        procedure setName(Index: integer; const _Names: string);
        function GetItem(Index: integer): GObj; reintroduce;
        procedure SetItem(Index: integer; AObject: GObj); reintroduce;
        function defaultCreateObj: GObj; virtual;
    protected type
        NListContents = (listCommon, listDifference);
        {Returns an object that contains the difference between two lists.
            IMPORTANT: Because this is a generic class, we have to supply the
            classType of the list that is being processed so that the returned
            list will be the same class type}
        function listCompare(constref _l1, _l2: TFPHashObjectList; _returnType: NListContents; _classOf: TFPHashObjectListClass): TFPHashObjecTList;

    public
        Parent: GObj; {Use if required}
        constructor Create(FreeObjects: boolean = True); reintroduce; virtual;
        function getNames(const _delim: string = ','): string;
        function keys: TStringArray;
        function add(const AName: shortstring; AObject: GObj): integer; reintroduce; virtual;
        function new: GObj;
        function find(const s: shortstring): GObj; reintroduce; virtual;
        function exists(_s: shortstring): boolean;
        function validIndex(_index: integer) : boolean;

        {Returns the object. Creates an object if it is not already in the list}
        function get(const s: shortstring): GObj; virtual;

        {functions to delete}
        function delete(const _obj: GObj) : boolean     {True if found and deleted} overload; virtual;
        function delete(const s: shortstring) : boolean {True if found and deleted} overload; virtual;

        function updateHashFor(_obj: GObj; _newName: string): integer; {returns the index of the object updated}

        {Returns a list that has the difference between this list and the passed list.
        The result is a new list object but the objects in the list still belong to their respective original lists}
        function diff(_list: TFPHashObjectList; _classOf: TFPHashObjectListClass): TFPHashObjectList;

        {Returns a list that has the common elements between this list and the passed list
        The result is a new list object but the objects in the list still belong to their respective original lists}
        function common(_list: TFPHashObjectList; _classOf: TFPHashObjectListClass): TFPHashObjectList;

    {## ENUMERATOR SUPPORT - TODO!!!  ##}
        procedure doWhile; {initialize the while loop}
        function EOL: boolean; {is End-Of-List?}
        function current: GObj;
        function Next: boolean; {True if successful}
        procedure endWhile; {if you need to break}
    {-----------------------------------------------}

    public
        property Items[Index: integer]: GObj read GetItem write SetItem;
        property Names[Index: integer]: string read getName;
        property Value[_key: shortstring]: GObj read get; default;

        property onFind: TNotifyEvent write setonFind;
        property onCreateObj: TNotifyEvent write setonCreateObj;
        property objConstructor: TObjectConstructor write myObjConstructor;
    end;

    TDynamicKeyValueStoreBase = specialize GenericHashObjectList<TStringObject>;

    { TDynamicKeyValueStore }
    { A key-value store using string objects. add() and put() creates
      new objects. These objects are destroyed on free}
    TDynamicKeyValueStore = class(TDynamicKeyValueStoreBase)
    public
        constructor Create; reintroduce;
        destructor Destroy; override;

        {creates an object and adds it to the list}
        function add(const _key: string; _value: string): integer; overload;

        {updates existing value. If not existing, adds the key-value pair}
        function put(const _key: string; _value: string): integer;

        function valueOf(const key: string): string;
        function stringVal(const _i: integer): string;
        function asString(const _delim: string = ','): string;
        function delete(_key: string): boolean; overload;
    end;

    TStringMap = TDynamicKeyValueStore;
    TStringMapClass = class of TStringMap;

    TStringIntegerMapBase = specialize GenericHashObjectList<TIntegerObject>;

	{ TStringIntegerMap }

    TStringIntegerMap = class(TStringIntegerMapBase)
    private
        function getValue(_key: string): integer;
        procedure setValue(_key: string; const _value: integer);
    public
        function defaultValue: integer; virtual;
        property Value[_key: string]: integer read getValue write setValue;
    end;

    { TStringIndexMap }

    TStringIndexMap = class(TStringIntegerMap)
    private
        function getIndex(_key: string): integer;
        procedure setIndex(_key: string; const _indexOf: integer);
    public
        function defaultValue: integer; override;
        property idx[_key: string]: integer read getIndex write setIndex;
    end;

    { TIntegerIndexMap }
    TIntegerIndexMap = class(TStringIndexMap)
    private
        function getIDIndex(_id: longint): integer;
        procedure setIDIndex(_id: longint; const _IDIndex: integer);
    public
        {typecasts the _id to string and uses inherited property idx}
        property idx[_id: longint]: integer read getIDIndex write setIDIndex;
        function exists(_id: longint): boolean;
    end;

    THashedIntegerListBase = specialize GenericHashObjectList<TIntegerIndexMap>;
    THashedIntegerlist = class(THashedIntegerListBase)


    end;

    TKeyValueListBase = specialize GenericHashObjectList <TDynamicKeyValueStore>;
    TKeyValueList = class(TKeyValueListBase)

	end;



    RbTreeNode = class

    end;

    { GTreeNodeBase }

    generic GTreeNodeBase<GNodeData: RbTreeNode> = class
        type
        GChildren = specialize GenericHashObjectList<GTreeNodeBase>;
    private
        initialized: boolean;
        myName: string;
        myParent: GTreeNodeBase;
        myChildren: GChildren;
        myData: GNodeData;
        function doRender(_node: GTreeNodeBase; indent: string = ''): string;
        function doFind(_name: string; _node: GTreeNodeBase): GTreeNodeBase;
    public
        constructor Create;
        constructor Create(_name: string; _parent: GTreeNodeBase = nil);
        destructor Destroy; override;
        function init(_name: string; _parent: GTreeNodeBase = nil): GTreeNodeBase;
        function parent: GTreeNodeBase;
        function Name: string;
        function Data: GNodeData;
        function hasChildren: boolean;
        function children: GChildren;
        function child(_index: integer): GTreeNodeBase;
        function child(_name: string): GTreeNodeBase;
        function find(_name: string): GTreeNodeBase;
        function render: string; virtual;
    end;


    { TKeyValueStore }

    TKeyValueStore = class(TFPStringHashTable)
    private
        function getValue(_key: string): string;
        procedure setValue(_key: string; const _value: string);
    public
        property Value[_key: string]: string read getValue write setValue;
        function exists(_key: string): boolean;
        function Count: integer;
    end;

    TIntegerListMapBase = specialize GenericHashObjectList<TIntegerList>;

    { TIntegerListMap }
    {Map -> List of integers}
    TIntegerListMap = class(TIntegerListMapBase)
	private
	    function getListOf(_key: string): TIntegerList;
	public
	    property listOf[_key: string]: TIntegerList read getListOf;
	end;

    function freeItems(constref _list: TFPSList): integer;


implementation

uses
    sugar.utils;

{ TIntegerList }

function sortCompare_IntegerList(const _a, _b: integer): integer;
begin
    if _a = _b then
        Result := 0   {values are same}
    else if _a > _b then
        Result := 1   {first value is greater}
    else
        Result := -1; {second value is greater}
end;

function freeItems(constref _list: TFPSList): integer;
var
    i: integer;
begin
    Result:= 0;
    for i:= 0 to pred(_list.Count) do
    begin
        TObject(_list.Items[i]).Free;
        Inc(Result);
	end;
end;

{ TIntegerObject }

constructor TIntegerObject.Create;
begin
     inherited;
end;

{ TStringIntegerMap }

function TStringIntegerMap.getValue(_key: string): integer;
var
    _tmp: TIntegerObject;
begin
    Result := DefaultValue;
    _tmp := find(_key);
    if assigned(_tmp) then
        Result := _tmp.Value;
end;

procedure TStringIntegerMap.setValue(_key: string; const _value: integer);
var
    _tmp: TIntegerObject;
begin
    _tmp := find(_key);
    if assigned(_tmp) then
    begin
        _tmp.Value := _value;
    end
    else
    begin
        _tmp := TIntegerObject.Create;
        _tmp.Value := _value;
        add(_key, _tmp);
    end;
end;

function TStringIntegerMap.defaultValue: integer;
begin
    Result:= -1;
end;

{ TIntegerListMap }

function TIntegerListMap.getListOf(_key: string): TIntegerList;
begin
     Result := find(_key);
     if not Assigned(Result) then
     begin
         Result := TIntegerList.Create;
         add(_key, Result);
	 end;
end;

{ TIntegerIndexMap }

function TIntegerIndexMap.getIDIndex(_id: longint): integer;
begin
    Result := inherited idx[_id.ToString];
end;

procedure TIntegerIndexMap.setIDIndex(_id: longint; const _IDIndex: integer);
begin
    inherited idx[_id.ToString] := _IDIndex;
end;


function TIntegerIndexMap.exists(_id: longint): boolean;
begin
    Result:= assigned(Find(_id.ToString));
end;

{ TStringIndexMap }

function TStringIndexMap.getIndex(_key: string): integer;
begin
    result:= getValue(_key);
end;

procedure TStringIndexMap.setIndex(_key: string; const _indexOf: integer);
begin
    setValue(_key, _indexOf);
end;

function TStringIndexMap.defaultValue: integer;
begin
	Result:=NOINDEX;
end;


{ TStringObject }

function TStringObject.add(_text: string): string;
begin
    Value := Value + _text;
    Result := Value;
end;

function TStringObject.isEmpty: boolean;
begin
    Result := Value.IsEmpty;
end;

function TStringObject.length: integer;
begin
    Result := Value.Length;
end;

procedure TStringObject.Clear;
begin
    Value := '';
end;

constructor TStringObject.Create;
begin
  inherited;
end;

procedure TIntegerList.sort;
begin
    inherited Sort(@sortCompare_IntegerList);
end;


{ TDynamicKeyValueStore }

constructor TDynamicKeyValueStore.Create;
begin
    inherited Create(true);
end;

destructor TDynamicKeyValueStore.Destroy;
begin
    inherited Destroy;
end;

function TDynamicKeyValueStore.add(const _key: string; _value: string): integer;
var
    _tmp: TStringObject;
begin
    _tmp := TStringObject.Create;
    _tmp.Value := _value;
    try
        Result := inherited add(_key, _tmp);
    except
        _tmp.Free; {if add was not successful, then free the created object}
    end;
end;

function TDynamicKeyValueStore.put(const _key: string; _value: string): integer;

begin
    Result := FindIndexOf(_key);
    if Result > -1 then
    begin
        if valueOf(_key) <> _value then {change the value}
            TStringObject(Items[Result]).Value := _value;
    end
    else
        Result := Add(_key, _value);
end;

function TDynamicKeyValueStore.valueOf(const key: string): string;
var
    _strObj: TStringObject;
begin
    Result := '';
    _strObj := find(key);
    if Assigned(_strObj) then
        Result := _strObj.Value;
end;

function TDynamicKeyValueStore.stringVal(const _i: integer): string;
begin
    Result:= '';
    if (_i > -1) and (_i < count) then
        Result:= Items[_i].Value;
end;

function TDynamicKeyValueStore.asString(const _delim: string): string;
var
    i: integer;
    addDelim: boolean = false;
begin
    Result:= '';
    for i:= 0 to pred(Count) do
    begin
        if addDelim then
            Result += _delim
        else
            addDelim := true;

        Result += '"' + Names[i] + '" = "' + valueOf(Names[i]) + '"';
	end;
end;

function TDynamicKeyValueStore.delete(_key: string): boolean;
begin
    Result:= delete(find(_key));
end;

{ GenericHashList }
function GenericHashList.getValue(_key: string): PtrType;
begin
    Result := find(_key);
end;

procedure GenericHashList.setValue(_key: string; const _value: PtrType);
var
    _index: integer;
begin
    _index := FindIndexOf(_key);
    if _index > -1 then
        Items[_index] := @_value
    else
        add(_key, _value);
end;

procedure GenericHashList.setautoFree(const _autoFree: boolean);
begin
    if myautoFree = _autoFree then
        Exit;
    myautoFree := _autoFree;
end;

function GenericHashList.getIndexValue(index: integer): PtrType;
begin
    Result := PtrType(Items[index]);
end;

function GenericHashList.getName(index: integer): shortstring;
begin
    Result := NameOfIndex(index);
end;

procedure GenericHashList.setIndexValue(index: integer; const _indexValue: PtrType);
begin

    Items[index] := @_indexValue;

end;

function GenericHashList.names: TStringArray;
var
	i: Integer;
begin
    SetLength(Result, Count);
    for i:= 0 to pred(Count) do
        Result[i]:= Name[i];
end;

function GenericHashList.valueOf(_i: integer): PtrType;
begin
    Result:= Value[Name[_i]];
end;


function GenericHashList.add(const _key: shortstring; Item: PtrType): integer;
begin
    Result := inherited add(_key, @Item);
end;

function GenericHashList.find(const AName: shortstring): ptrType;
begin
    Result := PtrType(inherited find(AName));
end;

constructor GenericHashList.Create;
begin
    inherited;
    autoFree := True;
end;

destructor GenericHashList.Destroy;
var
    i: integer;
begin
    if autoFree then
        for i := 0 to Count - 1 do
            PtrType(Items[i]).Free;
    inherited Destroy;
end;

{ TKeyValueStore }

function TKeyValueStore.getValue(_key: string): string;
begin
    Result := Items[_key];
end;

procedure TKeyValueStore.setValue(_key: string; const _value: string);
begin
    Items[_key] := _value;
end;

function TKeyValueStore.exists(_key: string): boolean;
begin
    Result := Assigned(Find(_key));
end;

function TKeyValueStore.Count: integer;
begin
    Result := inherited Count;
end;

{ GenericHashTreeNodeBase }

constructor GTreeNodeBase.Create;
begin
    inherited;
    initialized := False;
    myData := GNodeData.Create;
    myChildren := GChildren.Create;
end;

constructor GTreeNodeBase.Create(_name: string; _parent: GTreeNodeBase);
begin
    Create;
    init(_name, _parent);
end;

destructor GTreeNodeBase.Destroy;
begin
    FreeAndNil(myChildren);
    FreeAndNil(myData);
    inherited Destroy;
end;

function GTreeNodeBase.init(_name: string; _parent: GTreeNodeBase): GTreeNodeBase;
begin
    if not initialized then
    begin
        myName := _name;
        myParent := _parent;
        initialized := True;
    end;
    Result := self;
end;

function GTreeNodeBase.parent: GTreeNodeBase;
begin
    Result := myParent;
end;

function GTreeNodeBase.Name: string;
begin
    Result := myName;
end;

function GTreeNodeBase.Data: GNodeData;
begin
    Result := myData;
end;

function GTreeNodeBase.hasChildren: boolean;
begin
    Result := myChildren.Count > 0;
end;

function GTreeNodeBase.children: GChildren;
begin
    Result := myChildren;
end;

function GTreeNodeBase.child(_index: integer): GTreeNodeBase;
begin
    Result := children.Items[_index];
end;

function GTreeNodeBase.child(_name: string): GTreeNodeBase;
begin
    Result := myChildren.get(_name).init(_name, self);
end;

function GTreeNodeBase.find(_name: string): GTreeNodeBase;
begin
    Result := doFind(_name, self);
end;

function GTreeNodeBase.doRender(_node: GTreeNodeBase; indent: string = ''): string;
var
    i: integer;
begin
    Result := _node.Name + sLineBreak;
    if _node.hasChildren then
    begin
        for i := 0 to _node.children.Count - 1 do
            Result := Result + doRender(_node.child(i), indent + '    ');
    end;
end;

function GTreeNodeBase.doFind(_name: string; _node: GTreeNodeBase): GTreeNodeBase;
var
    _found: boolean;
    _i: integer;
begin
    Result := nil;
    _found := CompareText(_node.Name, _name) = 0;
    if not _found then
    begin
        if _node.hasChildren then
            for _i := 0 to _node.children.Count - 1 do
            begin
                Result := doFind(_name, _node.child(_i));
                if Assigned(Result) then
                    break;
            end;
    end
    else
        Result := Self;
end;


function GTreeNodeBase.render: string;
begin
    Result := doRender(self);
end;

procedure GenericHashObjectList.setonCreateObj(const _onCreateObj: TNotifyEvent);
begin
    if myonCreateObj = _onCreateObj then Exit;
    myonCreateObj := _onCreateObj;
end;


procedure GenericHashObjectList.setonFind(const _onFind: TNotifyEvent);
begin
    if myonFind = _onFind then
        Exit;
    myonFind := _onFind;
end;

function GenericHashObjectList.getName(Index: integer): string;
begin
    Result := NameOfIndex(Index);
end;

procedure GenericHashObjectList.setName(Index: integer; const _Names: string);
begin
    raise Exception.Create('GenericHashObjectList.setName() :: Not Implemented');
end;

{ GenericHashObjectList }
function GenericHashObjectList.GetItem(Index: integer): GObj;
begin
    Result := GObj(inherited GetItem(index));
end;

procedure GenericHashObjectList.SetItem(Index: integer; AObject: GObj);
begin
    inherited SetItem(Index, AObject);
end;

function GenericHashObjectList.defaultCreateObj: GObj;
begin
    {
    This assumes that the templated class has a
    constructor Create; If not, then you should
    assign objConstructor property with the function
    that creates a valid instance of the class.

    Ideally, do that in the constructor of the specalized list.
    }
    Result := GObj.Create;
end;

function GenericHashObjectList.listCompare(constref _l1, _l2: TFPHashObjectList;
	_returnType: NListContents; _classOf: TFPHashObjectListClass
	): TFPHashObjecTList;
var
    i : integer;
    longerlist, shorterlist, commonList, diffList: TFPHashObjectList;
    longerListDeletedNames: TStringMap;
begin

    if _l1.Count <= _l2.Count then
    begin
        longerList  := _l2;
        shorterList := _l1;
    end
    else
    begin
        longerList  := _l1;
        shorterList := _l2;
    end;

    longerListDeletedNames := TStringMap.Create;
    commonList  := _classOf.Create(false);
    diffList    := _classOf.Create(false);

    try
        for i:= 0 to pred(shorterList.Count) do
        begin
            if Assigned(longerList.find(shorterList.NameOfIndex(i))) then
            begin
                commonList.add(shorterList.NameOfIndex(i), shorterList.Items[i]);
                longerListDeletedNames.Put(shorterList.NameOfIndex(i), '');
            end
            else
                {This does not exist in the the other list, so it should
                be added to the difference list}
                diffList.add(shorterList.NameOfIndex(i), shorterList.Items[i]);
		end;

        for i := 0 to pred(longerList.Count) do
        begin
            {Whatever items are remaining in the longer list
            should be added to the difference list}
            if not longerListDeletedNames.exists(longerList.NameOfIndex(i)) then
                diffList.add(longerList.NameOfIndex(i), longerList.Items[i]);
		end;

	finally
        case _returnType of
            listCommon:
            begin
                Result:= commonList;
                diffList.Free;
			end;

            listDifference:
            begin
                Result:= diffList;
                commonList.Free;
			end;
        end;
        longerListDeletedNames.Free;
    end;
end;


constructor GenericHashObjectList.Create(FreeObjects: boolean);
begin
    inherited Create(FreeObjects);
    myObjConstructor := @defaultCreateObj; {default constructor}
    myInWhileLoop:= false;
end;

function GenericHashObjectList.add(const AName: shortstring; AObject: GObj): integer;
begin
    Result := inherited Add(AName, AObject);
end;

function GenericHashObjectList.new: GObj;
begin
    Result := get(Count.ToString);
end;

function GenericHashObjectList.find(const s: shortstring): GObj;
begin
    Result := GObj(inherited Find(s));
    if Assigned(Result) and Assigned(myonFind) then
        myOnFind(Result);
end;

function GenericHashObjectList.exists(_s: shortstring): boolean;
begin
    Result:= Assigned(find(_s));
end;

function GenericHashObjectList.validIndex(_index: integer): boolean;
begin
    Result:= (_index > -1) and (_index < Count);
end;

{This function finds an existing object or ADDS if not found.}
function GenericHashObjectList.get(const s: shortstring): GObj;
begin
    {searching for the object}
    Result := find(s);

    if not Assigned(Result) then
    begin
        if Assigned(myObjConstructor) then
        begin
            Result := myObjConstructor();

            if Assigned(myonCreateObj) then
                myonCreateObj(Result);

            {Adding a new object here}
            add(s, Result);
        end
        else
            raise Exception.Create('GenericHashObjectList.get(): ' +
                sLinebreak + 'ObjConstructor has not been assigned.');
    end;
end;

function GenericHashObjectList.delete(const _obj: GObj): boolean;
var
    i: integer;
begin
    if Assigned(_obj) then
    begin
	    i := IndexOf(_obj);
	    Result := (i > -1);
	    if Result then
	        inherited delete(i);
	end
    else
        Result:= false;
end;

function GenericHashObjectList.delete(const s: shortstring): boolean;
var
    i: integer;
begin
    i:= FindIndexOf(s);
    Result:= (i > -1);
    if Result then
        inherited delete(i);
end;

function GenericHashObjectList.updateHashFor(_obj: GObj; _newName: string
	): integer;
begin
    Result:= IndexOf(_obj);
    if Result > -1 then
        Rename(Names[Result],_newName);
end;

function GenericHashObjectList.diff(_list: TFPHashObjectList;
	_classOf: TFPHashObjectListClass): TFPHashObjectList;
begin
    Result:= listCompare(self, _list, listDifference, _classOf);
end;

function GenericHashObjectList.common(_list: TFPHashObjectList;
	_classOf: TFPHashObjectListClass): TFPHashObjectList;
begin
    Result:= listCompare(self, _list, listCommon, _classOf);
end;

procedure GenericHashObjectList.doWhile;
begin
    if Count > 0 then
    begin
        myInWhileLoop:= True;
        myCurrentIndex:= 0;
	end
    else
    begin
        myInWhileLoop:= false;
        myCurrentIndex:= -1;
	end;
end;

function GenericHashObjectList.EOL: boolean;
begin
    Result:= myInWhileLoop and (myCurrentIndex < Count);
end;

function GenericHashObjectList.current: GObj;
begin
    if validIndex(myCurrentIndex) then
        Result:= Items[myCurrentIndex]
    else
        Result:= nil;
end;

function GenericHashObjectList.Next: boolean;
begin
    if not myInWhileLoop then
        Result:= false
	else
    begin
	    inc(myCurrentIndex);
        Result:= not EOL;
	end;
end;

procedure GenericHashObjectList.endWhile;
begin
    myInWhileLoop:= False;
    myCurrentIndex:= -1;
end;

function GenericHashObjectList.getNames(const _delim: string): string;
var
    i: integer;
    _addDelim: boolean = False;
begin
    Result := '';
    for i := 0 to Count - 1 do
    begin
        if _addDelim then
            Result := Result + _delim
        else
            _addDelim := True;
        Result := Result + Names[i];
    end;
end;

function GenericHashObjectList.keys: TStringArray;
var
	i: Integer;
begin
    SetLength(Result, Count);
    for i:= 0 to pred(Count) do
        Result[i]:= Names[i];
end;

end.
