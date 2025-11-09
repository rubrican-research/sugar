unit sugar.jsonschema;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, fpjson, fgl;

type
	{ TJSONSchemaDef }
    TJSONSchemaDef = class(TJSONObject)
    private const
        __ID                  = 'ID';
        __Name                = 'Name';
        __Def                 = 'Def';
        __ParentClassName     = 'ParentClassName';
        __CollectionClassName = 'CollectionClassName';
        __Pos                 = 'Pos';
        __StoreName           = 'StoreName';

	private
        myChanges: set of Byte;

		function getCollectionClassName: string;
		function getDef: string;
		function getID: string;
		function getName: string;
		function getParentClassName: string;
		function getPos: integer;
		function getStoreName: string;
		procedure setCollectionClassName(const _value: string);
		procedure setDef(const _value: string);
		procedure setID(const _value: string);
		procedure setName(const _value: string);
		procedure setParentClassName(const _value: string);
		procedure setPos(const _value: integer);
		procedure setStoreName(const _value: string);

	published
        property ID : string read getID write setID;
        property Name : string read getName write setName;
        property Def  : string read getDef write setDef;
        property ParentClassName : string read getParentClassName write setParentClassName;
        property CollectionClassName : string read getCollectionClassName write setCollectionClassName;
        property Pos : integer read getPos write setPos;
        property storeName: string read getStoreName write setStoreName;

    public
        constructor Create; reintroduce;
        constructor Create(_from: TJSONObject);
        procedure copyFrom(_source: TJSONSchemaDef);

        procedure init;
        function hasChanged: boolean;
        procedure clearChanges;
	end;

    TJSONSchemaIndex = class(specialize TFPGMapObject<string, TJSONSchemaDef>);

  	{ TJSONSchemaList }
    TJSONSchemaList = class(TJSONObject)
	private
        function getprops(_id: string): TJSONSchemaDef;
		function getSettings: TJSONObject;
		procedure setprops(_id: string; const _value: TJSONSchemaDef);
		procedure setSettings(const _value: TJSONObject);

    public
        property props[_id: string] : TJSONSchemaDef read getprops write setprops;
        property settings: TJSONObject read getSettings write setSettings;

    public
        constructor Create; reintroduce;
        constructor Create(_from: TJSONObject);
        destructor Destroy; override;
        function rename(_oldID, _newID: string): TJSONSchemaList;
        function exists(_id: string): boolean;
    end;

    function makeObject(constref _schema: TJSONSchemaDef) : TJSONObject;

implementation

uses
    sugar.ulid, sugar.safeJson;

function getJSONAccessor(_symbol: string; _type: string; _default: string
	): string;
begin
    _type := lowercase(_type);
    case _type of
        'int8', 'shortint', 'int16', 'smallint', 'integer', 'int32', 'nativeint':
            Result := 'Integers';

        'longint':
            Result := 'LargeInts';

        'int64':
            Result := 'Int64s';

        'uint8', 'byte', 'uint16', 'word', 'nativeuint', 'dword', 'cardinal',
        'uint32', 'longword', 'uint64', 'qword':
            Result := 'QWords';

        'single', 'real', 'real48', 'double', 'extended', 'comp', 'currency':
            Result := 'Floats';

        'boolean', 'bytebool', 'wordbool', 'longbool':
            Result := 'Booleans';

        'shortstring', 'string', 'pchar', 'ansistring', 'pansichar':
            Result := 'Strings';

        'rawbytestring', 'widestring', 'pwidechar', 'unicodechar',
        'unicodestring', 'punicodechar':
            Result := 'UnicodeStrings';

        'tjsonarray', 'tsafejsonarray':
            Result := 'Arrays';

        'tjsonobject', 'tsafejsonobject':
            Result := 'Objects';
        else
            Result := 'Strings';
    end;
    Result := Format('%s[''%s'']', [Result, _symbol]);
    if _default <> '' then
        Result := Result + ' := ' + _default + ';';
end;

function makeObject(constref _schema: TJSONSchemaDef): TJSONObject;
var
    _def: TStringList;
    _line, _propName, _propType , _type: string;
    _arr : TStringArray;
begin
    Result := TJSONObject.Create;
    if not assigned(_def) then exit;
    _def := TStringList.Create;
    try
	    _def.Text := _schema.def;
	    for _line in _def do
	    begin
	        _arr := _line.Split([':']);
	        if length(_arr) <> 2 then continue;

	        // Property
	        _propname := trim(_arr[0]);
	        _proptype := trim(_arr[1]).Replace(';', '');

            _type := lowercase(_proptype);
            case _type of
                'int8', 'shortint', 'int16', 'smallint', 'integer', 'int32', 'nativeint':
                    Result.Integers[_propName] := 0;

                'longint':
                    Result.LargeInts[_propName] := 0;

                'int64':
                    Result.Int64s[_propName] := 0;

                'uint8', 'byte', 'uint16', 'word', 'nativeuint', 'dword', 'cardinal',
                'uint32', 'longword', 'uint64', 'qword':
                    Result.QWords[_propName] := 0;

                'single', 'real', 'real48', 'double', 'extended', 'comp', 'currency':
                    Result.Floats[_propName] := 0.0;

                'boolean', 'bytebool', 'wordbool', 'longbool':
                    Result.Booleans[_propName] := false;

                'shortstring', 'string', 'pchar', 'ansistring', 'pansichar':
                    Result.Strings[_propName] := '';

                'rawbytestring', 'widestring', 'pwidechar', 'unicodechar',
                'unicodestring', 'punicodechar':
                    Result.UnicodeStrings[_propName] := '';

                'tjsonarray', 'tsafejsonarray':
                    Result.Arrays[_propName] := TJSONArray.Create();

                'tjsonobject':
                    Result.Objects[_propName] := TJSONObject.Create;

                'tsafejsonobject':
                    Result.Objects[_propName] := TSafeJSONObject.Create;

                else
                    Result.Strings[_propName] := '';
            end;
		end;
	finally
        _def.Free;
	end;
end;

{ TJSONSchemaDef }

function TJSONSchemaDef.getCollectionClassName: string;
begin
    Result := strings[__CollectionClassName];
end;

function TJSONSchemaDef.getDef: string;
begin
    Result := Strings[__Def];
end;

function TJSONSchemaDef.getID: string;
begin
    Result := strings[__ID];
end;

function TJSONSchemaDef.getName: string;
begin
    Result := Strings[__Name];
end;

function TJSONSchemaDef.getParentClassName: string;
begin
    Result := Strings[__ParentClassName];
end;

function TJSONSchemaDef.getPos: integer;
begin
    Result := Integers[__Pos];
end;

function TJSONSchemaDef.getStoreName: string;
begin
    Result := Strings[__StoreName];
end;

procedure TJSONSchemaDef.setCollectionClassName(const _value: string);
begin
    Strings[__CollectionClassName] := _value;
    include(myChanges, IndexOfName(__CollectionClassName));
end;

procedure TJSONSchemaDef.setDef(const _value: string);
begin
    Strings[__Def] := _value;
    include(myChanges, IndexOfName(__Def));
end;

procedure TJSONSchemaDef.setID(const _value: string);
begin
    Strings[__ID] := _value;
    include(myChanges, IndexOfName(__ID));
end;

procedure TJSONSchemaDef.setName(const _value: string);
begin
    Strings[__Name] := _value;
    include(myChanges, IndexOfName(__NAME));
end;

procedure TJSONSchemaDef.setParentClassName(const _value: string);
begin
    Strings[__ParentClassName] := _value;
    include(myChanges, IndexOfName(__ParentClassName));
end;

procedure TJSONSchemaDef.setPos(const _value: integer);
begin
    Integers[__Pos] := _value;
    include(myChanges, IndexOfName(__Pos));
end;

procedure TJSONSchemaDef.setStoreName(const _value: string);
begin
    Strings[__StoreName] := _value;
    include(myChanges, IndexOfName(__StoreName));
end;

constructor TJSONSchemaDef.Create;
begin
    inherited Create;
    init;
end;

constructor TJSONSchemaDef.Create(_from: TJSONObject);
begin
    Create;
    ID                  := _from.get(__ID, ID);
    Name                := _from.get(__Name, Name);
    Def                 := _from.get(__Def, Def);
    ParentClassName     := _from.get(__ParentClassName, ParentClassName);
    CollectionClassName := _from.get(__CollectionClassName, CollectionClassName);
    Pos                 := _from.get(__Pos, Pos);
    storeName           := _from.get(__StoreName, storeName);
end;

procedure TJSONSchemaDef.copyFrom(_source: TJSONSchemaDef);
begin
    Name                := _source.Name;
    Def                 := _source.Def;
    ParentClassName     := _source.ParentClassName;
    CollectionClassName := _source.CollectionClassName;
    Pos                 := _source.Pos;
end;

procedure TJSONSchemaDef.init;
begin
    ID   := '';
    Name := '';
    Def  := '';
    ParentClassName     := 'TSafeJSONObject';
    CollectionClassName := '';
    Pos  := -1;
    storeName := '';
end;

function TJSONSchemaDef.hasChanged: boolean;
begin
    Result := not(myChanges = []);
end;

procedure TJSONSchemaDef.clearChanges;
begin
    myChanges := [];
end;

{ TJSONSchemaList }


function TJSONSchemaList.getprops(_id: string): TJSONSchemaDef;
begin
    Result := TJSONSchemaDef(objects[_id]);
end;

function TJSONSchemaList.getSettings: TJSONObject;
begin
    Result := objects['settings'];
end;

procedure TJSONSchemaList.setprops(_id: string; const _value: TJSONSchemaDef
	);
begin
    objects[_id] := _value;
end;

procedure TJSONSchemaList.setSettings(const _value: TJSONObject);
begin
    Objects['settings'] := _value;
end;

constructor TJSONSchemaList.Create;
begin
    inherited Create;
    settings := TJSONObject.Create;
end;

constructor TJSONSchemaList.Create(_from: TJSONObject);
var
	v: TJSONEnum;
	_obj: TJSONObject;
begin
    Create;
    for v in _from do begin
        if v.key = 'settings' then begin
            settings := TJSONObject(v.Value.Clone);
            continue;
		end;
		_obj := TJSONObject(v.value);
        props[v.key] := TJSONSchemaDef.Create(_obj);
	end;
end;

destructor TJSONSchemaList.Destroy;
begin
	inherited Destroy;
end;

function TJSONSchemaList.rename(_oldID, _newID: string): TJSONSchemaList;
begin
    if not exists(_newID) then
        props[_newID] := TJSONSchemaDef(Extract(_oldID));
    Result := self;
end;

function TJSONSchemaList.exists(_id: string): boolean;
begin
    Result := IndexOfName(_id) <> -1;
end;

end.

