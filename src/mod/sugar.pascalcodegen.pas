unit sugar.pascalcodegen;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, fpjson, fgl, sugar.jsonschema;

type

    { TPascalSection }

    TPascalSection = class
        Name: string;
        uses_: string;
        code: TStringList;
        constructor Create;
        destructor Destroy; override;
        function genCode: string;
    end;

    TMapStringList = class(specialize TFPGMapObject<string, TStringList>);

    { TPascalClass }

    TPascalClass = class
        Name: string;
        parent: string;
        definition: TMapStringList;
        implementation_: TStringList;
        constructor Create;
        destructor Destroy; override;
        function signature: string;
        function body: string;
    end;

    { TPascalMethod }

    TPascalMethod = class
        class_: string;
        Name: string;
        params: string;
        returns: string;
        override: boolean;
        consts: TStringList;
        vars: TStringList;
        methBody: TStringList;
        function keyword: string; virtual;
        function procName: string;
        function signature: string; virtual;
        function body: string; virtual;
        function addVars(constref _body: TStringList): TStringList;
        constructor Create; virtual;
        destructor Destroy; override;
    end;

    { TPascalFunction }

    TPascalFunction = class(TPascalMethod)
        function keyword: string; override;
        function signature: string; override;
        function body: string; override;
    end;

    { TPascalProcedure }

    TPascalProcedure = class(TPascalMethod)
        function keyword: string; override;
        function signature: string; override;
        function body: string; override;
    end;

	{ TPascalConstructor }

    TPascalConstructor = class(TPascalMethod)
        function keyword: string; override;
        function signature: string; override;
        function body: string; override;
        constructor Create; override;
    end;

	{ TPascalDestructor }

    TPascalDestructor = class(TPascalMethod)
        function keyword: string; override;
        function signature: string; override;
        function body: string; override;
        constructor Create; override;
    end;


    { TPascalUnit }

    TPascalUnit = class
        Name: string;
        interface_: TPascalSection;
        implementation_: TPascalSection;
        initialization_: TStringList;
        finalization_: TStringList;
        constructor Create;
        destructor Destroy; override;
        function genCode: string;
    end;

// all classes in one file
function genFPCCode(_unitName: string; _schema: TJSONSchemaList): string;

// single file per schema
function genFPCCode(_unitPrefix: string; constref _schemaDef: TJSONSchemaDef;
    out _fileName: string): string;

function genReadFromDB (_funcName: string; _class: string; _table: string; _def: TJSONSchemaDef; _key: string; _ignore: array of string) : TPascalFunction; overload;
function genReadFromDB (_funcName: string; _class: string; _table: string; _data: TJSONObject; _key: string; _ignore: array of string) : TPascalFunction; overload;

function getJSONAccessor(_symbol: string; _type: string; _default: string =''): string;
function getTypeReadFunc(_symbol: string; _type: string): string;
function getTypeWriteFunc(_symbol: string; _type: string; _valueVar: string): string;
function genVarInit(_symbol: string; _type: string): string;
function getReaderImpl(_symbol: string; _type: string): string;
function getWriterImpl(_symbol: string; _type: string): string;

procedure addSchemaObj(constref _unit: TPascalUnit; _schema: TJSONSchemaDef);

var
    defaultInfUses : string = 'Classes, SysUtils, fpjson, sugar.safeJson';


implementation

uses
    sugar.sqlitehelper, rtti;

{
    getStr(const _key: string)                                                  : string;
    setStr(const _key: string; const _value: TJSONStringType)                   : TJSONStringType;
    getBool(const _key: string)                                                 : boolean;
    setBool(const _key: string; const _value: boolean)                          : boolean;
    getFloat(const _key: string)                                                : TJSONFloat;
    setFloat(const _key: string; const _value: TJSONFloat)                      : TJSONFloat;
      getUnicodeString(const _key: string)                                        : TJSONUnicodeStringType;
      setUnicodeString(const _key: string; const _value: TJSONUnicodeStringType)  : TJSONUnicodeStringType;
      getInt64(const _key: string)                                                : int64;
      setInt64(const _key: string; const _value: int64)                           : int64;
      getQWord(const _key: string)                                                : QWord;
      setQWord(const _key: string; const _value: QWord)                           : QWord;
      getInt(const _key: string)                                                  : integer;
      setInt(const _key: string; const _value: integer)                           : integer;
      getLargeInt(const _key: string)                                             : TJSONLargeInt;
      setLargeInt(const _key: string; const _value: TJSONLargeInt)                : TJSONLargeInt;
      getArray(const _key: string)                                                : TJSONArray;
      setArray(const _key: string; constref _value: TJSONArray)                   : TJSONArray;
       getObject(const _key: string)                                               : TSafeJSONObject;
      setObject(const _key: string; constref _value: TSafeJSONObject)             : TSafeJSONObject;
}

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

function getTypeReadFunc(_symbol: string; _type: string): string;
begin
    _type := lowercase(_type);
    case _type of
        'int8', 'shortint', 'int16', 'smallint', 'integer', 'int32', 'nativeint':
            Result := 'getInt';

        'longint':
            Result := 'getLargeInt';

        'int64':
            Result := 'getInt64';

        'uint8', 'byte', 'uint16', 'word', 'nativeuint', 'dword', 'cardinal',
        'uint32', 'longword', 'uint64', 'qword':
            Result := 'getQWord';

        'single', 'real', 'real48', 'double', 'extended', 'comp', 'currency':
            Result := 'getFloat';

        'boolean', 'bytebool', 'wordbool', 'longbool':
            Result := 'getBool';

        'shortstring', 'string', 'pchar', 'ansistring', 'pansichar':
            Result := 'getStr';

        'rawbytestring', 'widestring', 'pwidechar', 'unicodechar',
        'unicodestring', 'punicodechar':
            Result := 'getUnicodeString';

        'tjsonarray', 'tsafejsonarray':
            Result := 'getArray';

        'tjsonobject', 'tsafejsonobject':
            Result := 'getObject';

        else
            if _type.StartsWith('t') then
            begin // this is an object
                Result := Format('%s(getObject(%s));', [_type, _symbol]);
                exit;
            end
            else
                Result := 'getStr';
    end;
    Result := Format('%s(%s);', [Result, _symbol]);
end;

function getTypeWriteFunc(_symbol: string; _type: string; _valueVar: string): string;
begin
    _type := lowercase(_type);
    case _type of
        'int8', 'shortint', 'int16', 'smallint', 'integer', 'int32', 'nativeint':
            Result := 'setInt';

        'longint':
            Result := 'setLargeInt';

        'int64':
            Result := 'setInt64';

        'uint8', 'byte', 'uint16', 'word', 'nativeuint', 'dword', 'cardinal',
        'uint32', 'longword', 'uint64', 'qword':
            Result := 'setQWord';

        'single', 'real', 'real48', 'double', 'extended', 'comp', 'currency':
            Result := 'setFloat';

        'boolean', 'bytebool', 'wordbool', 'longbool':
            Result := 'setBool';

        'shortstring', 'string', 'pchar', 'ansistring', 'pansichar':
            Result := 'setStr';

        'rawbytestring', 'widestring', 'pwidechar', 'unicodechar',
        'unicodestring', 'punicodechar':
            Result := 'setUnicodeString';

        'tjsonarray', 'tsafejsonarray':
            Result := 'setArray';

        'tjsonobject', 'tsafejsonobject':
            Result := 'setObject';

        else
            if _type.StartsWith('t') then Result := 'setObject'// this is an object

            else
                Result := 'setStr';
    end;
    Result := Format('%s(%s, %s);', [Result, _symbol, _valueVar]);
end;

function genVarInit(_symbol: string; _type: string): string;
var
	_default, _ltype: String;
begin
    _ltype := lowercase(_type);
    case _ltype of
        'int8', 'shortint', 'int16', 'smallint', 'integer', 'int32', 'nativeint',
        'longint', 'int64', 'uint8', 'byte', 'uint16', 'word', 'nativeuint', 'dword', 'cardinal',
        'uint32', 'longword', 'uint64', 'qword':
            _default := '0';

        'single', 'real', 'real48', 'double', 'extended', 'comp', 'currency':
            _default := '0.0';

        'boolean', 'bytebool', 'wordbool', 'longbool':
            _default := 'false';

        'shortstring', 'string', 'pchar', 'ansistring', 'pansichar',
        'rawbytestring', 'widestring', 'pwidechar', 'unicodechar',
        'unicodestring', 'punicodechar':
            _default := '''''';

        'tjsonarray', 'tsafejsonarray':
            _default := 'TJSONArray.Create()';

        'tjsonobject':
            _default := 'TJSONObject.Create()';

         'tsafejsonobject':
             _default := 'TSafeJSONObject.CreateSafe()';

        else
            if _type.StartsWith('t') then
                _default := _type  + '.Create' // this is an object
            else
                _default := '''''';
    end;
    Result := Format('%s := %s;', [_symbol, _default]);
end;


function getReaderImpl(_symbol: string; _type: string): string;
begin
    Result := 'Result := ' + getTypeReadFunc(_symbol, _type);
end;

function getWriterImpl(_symbol: string; _type: string): string;
begin
    Result := getTypeWriteFunc(_symbol, _type, '_value');
end;

procedure addSchemaObj(constref _unit: TPascalUnit; _schema: TJSONSchemaDef);
var
    _def, _classConsts, _classPrivateMethods, _classProps, _classImpl,
		_classPublicMethods: TStringList;
    _line, _propname, _proptype, _storeName: string;
    _arr: TStringArray;
    _schemaClass: TPascalClass;
    _getFunc: TPascalFunction;
    _setProc: TPascalProcedure;
    _dbSelectFunc : TPascalFunction;
    _initFieldsProc: TPascalProcedure;
begin
    _schemaClass := TPascalClass.Create;


    _classConsts := TStringList.Create;
    _schemaClass.definition.add('public consts', _classConsts);
    _classConsts.Add('public');

    _classPrivateMethods := TStringList.Create;
    _schemaClass.definition.add('private methods', _classPrivateMethods);
    _classPrivateMethods.Add('private');

    _classProps := TStringList.Create;
    _schemaClass.definition.add('public properties', _classProps);
    _classProps.Add('public');

    _classPublicMethods := TStringList.Create;
    _schemaClass.definition.add('public methods', _classPublicMethods);
    _classPublicMethods.Add('public');

    _classImpl := _schemaClass.implementation_;

    _def := TStringList.Create;
    _getFunc := TPascalFunction.Create;
    _setProc := TPascalProcedure.Create;
    _storeName := _schema.storeName;

    if _storeName = '' then _storeName := _schema.Name;
    _dbSelectFunc := genReadFromDB(
                        {funcName} 'dbLoad' + _schema.Name + 'Array',
                        {class}    '',
                        {table}    _storeName,
                        {schema}   _schema,
                        {Key}      '',
                        {ignore}   []
                    );

    _initFieldsProc := TPascalProcedure.Create;
    _initFieldsProc.class_ := _schema.Name;
    _initFieldsProc.name := 'initFields';
    _initFieldsProc.override:= true;
    _initFieldsProc.methBody.Add('inherited;');

    _classPublicMethods.add(_initFieldsProc.signature);

    try
        _schemaClass.Name   := _schema.Name;
        _schemaClass.parent := _schema.ParentClassName;

        _def.Text := _schema.def;

        for _line in _def do
        begin

            _arr := _line.Split([':']);
            if length(_arr) <> 2 then continue;

            // Property
            _propname := trim(_arr[0]);
            _proptype := trim(_arr[1]).Replace(';', '');

            _initFieldsProc.methBody.Add(genVarInit(_propName{name constant}, _proptype));
            if _classConsts.Count = 1 then _classConsts.Add('const');
            _classConsts.Add(Format('__%0:s = ''%0:s'';', [_propName]));

            _getFunc.Name := 'get' + _propName;
            _getFunc.class_ := _schema.Name;
            _getFunc.params := '';
            _getFunc.returns := _propType;
            _getFunc.methBody.Add(getReaderImpl('__' + _propName{name constant}, _proptype));

            _classPrivateMethods.Add(_getFunc.signature);
            _classImpl.Add(_getFunc.body);

            _setProc.Name := 'set' + _propName;
            _setProc.class_ := _schema.Name;
            _setProc.params := '_value: ' + _propType;
            _setProc.returns := '';
            _setProc.methBody.Add(getWriterImpl('__' + _propName{name constant}, _proptype));

            _classPrivateMethods.Add(_setProc.signature);
            _classImpl.Add(_setProc.body);

            _classProps.Add(Format('property %s : %s read %s write %s;',
                [_propName, _propType, _getFunc.Name, _setProc.Name]));

            _getFunc.methBody.Clear;
            _setProc.methBody.Clear;
        end;

        _classImpl.Add(_initFieldsProc.body);

        if _unit.interface_.code.Count = 0 then
            _unit.interface_.code.add('type');

        _unit.interface_.code.Add(_schemaClass.signature);
        _unit.interface_.code.add(_dbSelectFunc.signature);
        _unit.implementation_.code.Add(_schemaClass.body);
        _unit.implementation_.code.add(_dbSelectFunc.body);


    finally
        _schemaClass.Free;
        _getFunc.Free;
        _setProc.Free;
        _dbSelectFunc.Free;
        _initFieldsProc.Free;
        _def.Free;
    end;
end;

function genFPCCode(_unitName: string; _schema: TJSONSchemaList): string;
var
    _ens: TJSONEnum;
    _s: TJSONSchemaDef;
    _fpcUnit: TPascalUnit;
begin
    try
        _fpcUnit := TPascalUnit.Create;
        _fpcUnit.Name := _unitName;
        _fpcUnit.interface_.uses_ := defaultInfUses;
        for _ens in _schema do
        begin
            if _ens.Key = 'settings' then continue;
            _s := TJSONSchemaDef(_ens.Value);
            addSchemaObj(_fpcUnit, _s);
        end;
        Result := _fpcUnit.genCode;
    finally
        _fpcUnit.Free;
    end;
end;

function genFPCCode(_unitPrefix: string; constref _schemaDef: TJSONSchemaDef;
    out _fileName: string): string;
var
    _ens: TJSONEnum;
    _s: TJSONSchemaDef;
    _fpcUnit: TPascalUnit;

    function formatUnitName(_className: string): string;
    begin
        if _schemaDef.Name.StartsWith('T', False) then
            Result := _schemaDef.Name.Substring(1)
        else
            Result := _schemaDef.Name;

        if _unitPrefix = '' then
            _unitPrefix := 'schema';

        Result := _unitPrefix + '.' + Result;
    end;

begin
    try
        _fpcUnit := TPascalUnit.Create;
        _fpcUnit.Name := formatUnitName(_schemaDef.Name);
        _fileName := _fpcUnit.Name + '.pas';
        _fpcUnit.interface_.uses_ := defaultInfUses;
        addSchemaObj(_fpcUnit, _schemaDef);
        Result := _fpcUnit.genCode;
    finally
        _fpcUnit.Free;
    end;
end;

function genReadFromDB(_funcName: string; _class: string; _table: string; _def: TJSONSchemaDef;
	_key: string; _ignore: array of string): TPascalFunction;
var
	_data: TJSONObject;
begin
    _data := makeObject(_def);
    try
        Result := genReadFromDB(_funcName, _class, _table, _data, _key, _ignore);
	finally
        _data.Free;
	end;
end;

function genReadFromDB(_funcName: string; _class: string; _table: string; _data: TJSONObject; _key: string; _ignore: array of string): TPascalFunction;
var
	_i, _col : Integer;
	_val: TJSONData;
	_num: TJSONNumber;
	_ignoreIndex: TJSONObject;
    _ig, _var, _codeLine: string;

    function shouldIgnore(_f: string): boolean;
    begin
        Result := _ignoreIndex.IndexOfName(lowercase(_f)) <> -1;
	end;

begin

    if (_funcName = '') or (not isValidIdent(_funcName)) then raise Exception.Create('genReadFromDB:: Invalid function name');
    if _table = '' then raise Exception.Create('genReadFromDB:: Table name is empty');
    if not assigned(_data) then raise Exception.Create('genReadFromDB:: _data object not assigned ');


    _ignoreIndex := TJSONObject.Create();
    try
        // Prepare _ignore index
	    for _ig in _ignore do
            _ignoreIndex.strings[lowercase(_ig)] := _ig;

		Result := TPascalFunction.Create;
	    Result.class_ := _class;
	    Result.Name := _funcName;
	    Result.params:='constref _qry: TSQLQuery; const _where: string = ''''; _rowClass: TJSONObjectClass = TJSONObject';
	    Result.returns:= 'TJSONArray';

	    with Result.consts do begin
	        add(Format('Q = ''%s'';', [genSelectSQL(_table, _data, _key, _ignore)]));
		end;

	    with Result.vars do begin
	        add('_row : ' + _data.ClassName  + ';');
		end;

	    with Result.methBody do begin
	        add('Result := TJSONArray.Create();');

            add('if _where = '''' then ');
            add('   _qry.SQL.Text := Q');
            add('else ');
            add('   _qry.SQL.Text := Q + '' WHERE '' + _where;');

	        add('try');
	        add('   _qry.Open;');
	        add('   while not _qry.EOF do begin');
	        add('       _row := ' + _data.ClassName + '.Create;');
		    add('       Result.Add(_row);');
	        _col := 0;
	        for _i := 0 to pred(_data.Count) do begin

                _var := _data.Names[_i];
	            _val := _data.Items[_i];
                _codeLine := '';

	            if shouldIgnore(_var) then continue;

	            case _val.JSONType of
	            	jtUnknown: ;
	                jtNumber: begin
	                    _num := TJSONNumber(_val);
	                    case _num.NumberType of
	                    	ntFloat     : _codeLine := Format('_row.Floats[''%s''] := _qry.Fields[%d].AsFloat;', [_var, _col]);
	                        ntInteger   : _codeLine := Format('_row.Integers[''%s''] := _qry.Fields[%d].AsInteger;', [_var, _col]);
	                        ntInt64     : _codeLine := Format('_row.Int64s[''%s''] := _qry.Fields[%d].AsLargeInt;', [_var, _col]);
	                        ntQWord     : _codeLine := Format('_row.QWords[''%s''] := _qry.Fields[%d].AsLargeInt;', [_var, _col]);
	                    end;
	                end;
	                jtString    : _codeLine := Format('_row.Strings[''%s''] := _qry.Fields[%d].AsString;', [_var, _col]);
	                jtBoolean   : _codeLine := Format('_row.Booleans[''%s''] := _qry.Fields[%d].AsBoolean;', [_var, _col]);
	                jtNull      : ;
	                jtArray     : _codeLine := Format('_row.Arrays[''%s''] := TJSONArray(getJSON(_qry.Fields[%d].AsString));', [_var, _col]);
	                jtObject    : _codeLine := Format('_row.Objects[''%s''] := TJSONObject(getJSON(_qry.Fields[%d].AsString));', [_var, _col]) ;
	            end;

                if _codeLine <> '' then
		            add(_codeLine);
	            inc(_col);
			end;
			add('       _qry.Next;');
	        add('   end;');   // While Loop
	        add('finally');
	        add('   _qry.Free;');
	        add('end;');
		end;
	finally
        _ignoreIndex.Free;
	end;
end;

{ TPascalSection }

constructor TPascalSection.Create;
begin
    inherited;
    code := TStringList.Create;
end;

destructor TPascalSection.Destroy;
begin
    code.Free;
    inherited;
end;

function TPascalSection.genCode: string;
var
    s: TStringList;
begin
    s := TStringList.Create;
    try
        s.Add(Name);
        if uses_ <> '' then
        begin
            s.Add('uses');
            if not uses_.EndsWith(';') then uses_ := uses_ + ';';
            s.Add(uses_);
        end;
        s.AddStrings(code, False);
    finally
        Result := s.Text;
        s.Free;
    end;
end;

{ TPascalClass }

constructor TPascalClass.Create;
begin
    inherited;
    definition := TMapStringList.Create(True);
    implementation_ := TStringList.Create;
end;

destructor TPascalClass.Destroy;
begin
    definition.Free;
    implementation_.Free;
    inherited;
end;

function TPascalClass.signature: string;
var
    s: TStringList;
    i: integer;
begin
    s := TStringList.Create;
    try

        if parent <> '' then
            s.Add(Format('%s = class(%s)', [Name, parent]))
        else
            s.Add(Format('%s = class(TSafeJSONObject)', [Name]));

        for i := 0 to pred(definition.Count) do s.AddStrings(definition.Data[i], False);
        s.Add('end;');
    finally
        Result := s.Text;
        s.Free;
    end;
end;

function TPascalClass.body: string;
begin
    Result := implementation_.Text;
end;

{ TPascalMethod }

function TPascalMethod.keyword: string;
begin
    Result := '';
end;

function TPascalMethod.procName: string;
begin
    Result := '';
    if class_ <> '' then
        Result := class_ + '.';
    Result := Result + Name;
end;

function TPascalMethod.signature: string;
begin
    Result := '';
end;

function TPascalMethod.body: string;
begin
    Result := '';
end;

function TPascalMethod.addVars(constref _body: TStringList): TStringList;
begin
    Result := _body;
    if consts.Count > 0 then begin
        _body.add('const');
        _body.AddStrings(consts, false);
	end;

    if vars.count > 0 then begin
        _body.add('var');
        _body.AddStrings(vars, false);
	end;
end;

constructor TPascalMethod.Create;
begin
    inherited;
    consts := TStringList.Create;
    vars   := TStringList.Create;
    methBody := TStringList.Create;
end;

destructor TPascalMethod.Destroy;
begin
    consts.Free;
    vars.Free;
    methBody.Free;
	inherited Destroy;
end;

{ TPascalFunction }

function TPascalFunction.keyword: string;
begin
    Result := 'function';
end;

function TPascalFunction.signature: string;
begin
    if returns = '' then returns := 'boolean';
    Result := Format('%s %s(%s): %s;', [keyword, Name, params, returns]);
    if override then
        Result := Result + ' override;';
end;

function TPascalFunction.body: string;
var
    s: TStringList;
begin
    s := TStringList.Create;
    if returns = '' then returns := 'boolean';
    try
        s.Add(format('%s %s(%s) : %s;', [keyword, procName, params, returns]));
        addVars(s);
        s.add('begin');
        s.AddStrings(methBody, false);
        s.add('end;');
    finally
        Result := s.Text;
        s.Free;
    end;
end;

{ TPascalProcedure }

function TPascalProcedure.keyword: string;
begin
    Result := 'procedure';
end;

function TPascalProcedure.signature: string;
begin
    Result := Format('%s %s(%s);', [keyword, Name, params]);
    if override then
        Result := Result + ' override;';
end;

function TPascalProcedure.body: string;
var
    s: TStringList;
begin
    s := TStringList.Create;
    try
        s.Add(format('%s %s(%s);', [keyword, procName, params]));
        addVars(s);
        s.add('begin');
        s.addStrings(methBody, false);
        s.add('end;');
    finally
        Result := s.Text;
        s.Free;
    end;
end;

{ TPascalConstructor }

function TPascalConstructor.keyword: string;
begin
	Result := 'constructor';
end;

function TPascalConstructor.signature: string;
begin
    if Name = '' then Name := 'Create';
	Result := Format('%s %s(%s);', [Keyword, Name, params]);
    if override then
        Result := Result + ' override;';
end;

function TPascalConstructor.body: string;
var
    s: TStringList;
begin
    s := TStringList.Create;
    try
        s.Add(format('%s %s(%s);', [Keyword, procName, params]));
        addVars(s);
        s.add('begin');
        s.add('inherited;');
        s.addStrings(methBody, false);
        s.add('end;');
    finally
        Result := s.Text;
        s.Free;
    end;
end;

constructor TPascalConstructor.Create;
begin
	inherited Create;
    Name := 'Create';
    override := true;
end;

{ TPascalDestructor }

function TPascalDestructor.keyword: string;
begin
	Result:= 'destructor';
end;

function TPascalDestructor.signature: string;
begin
    if Name = '' then Name := 'Destroy';
	Result := Format('%s %s; override;', [Keyword, Name]);
end;

function TPascalDestructor.body: string;
var
    s: TStringList;
begin
    s := TStringList.Create;
    try
        s.Add(format('%s %s; override;', [Keyword, procName]));
        addVars(s);
        s.add('begin');
        s.addStrings(methBody,false);
        s.add('inherited Destroy;');
        s.add('end;');
    finally
        Result := s.Text;
        s.Free;
    end;
end;

constructor TPascalDestructor.Create;
begin
	inherited Create;
    Name := 'Destroy';
end;

{ TPascalUnit }

constructor TPascalUnit.Create;
begin
    inherited;
    interface_ := TPascalSection.Create;
    interface_.Name := 'interface';

    implementation_ := TPascalSection.Create;
    implementation_.Name := 'implementation';

    initialization_ := TStringList.Create;
    finalization_ := TStringList.Create;
end;

destructor TPascalUnit.Destroy;
begin
    interface_.Free;
    implementation_.Free;
    initialization_.Free;
    finalization_.Free;
    inherited;
end;

function TPascalUnit.genCode: string;
var
    s: TStringList;
begin
    s := TStringList.Create;
    try
        s.Add('unit ' + Name + ';');
        s.Add(interface_.genCode);
        s.Add(implementation_.genCode);
        s.Add('initialization');
        s.Add(initialization_.Text);
        s.Add('finalization');
        s.Add(finalization_.Text);
        s.Add('end.');
    finally
        Result := s.Text;
        s.Free;
    end;

end;


end.
