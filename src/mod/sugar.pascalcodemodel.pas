unit sugar.pascalcodemodel;
interface

uses
    Classes, SysUtils, sugar.ddldatatypes, sugar.collections;

type

    RbProcType = (procProcedure, procFunction, procConstructor, procDestructor);
    RbProcGenType = (pgtInterface, pgtImplementation, pgtBoth);
    RbPropType = (propReadOnly, propReadWrite, propChainable);
    RpVisibility = (vPrivate, vProtected, vPublic, vPublished);

    { RbStringList }

    RbStringList = class(TStringList)
        function Add(const S: string): integer; override;
    end;

    { RbCodeGen }
    RbCodeGen = class
    private
        class var myVersion: word;
    private
        FOwner: RbCodeGen;
    public
        function Owner(_Owner: RbCodeGen): RbCodeGen; overload;
        function Owner: RbCodeGen; overload;
    private
        FName: string;
    public
        function Name(_Name: string): RbCodeGen; overload;
        function Name: string; overload;
    private
        FComment: RbStringList;
    public
        {Send empty string to clear comment. Otherwise, each call appends to the comment}
        function Comment(_comment: string): RbCodeGen; overload;
        function CommentList: RbStringList; overload;
        function Comment: string;

    public
        constructor Create; virtual;
        destructor Destroy; override;
        constructor New(_name: string); virtual;
        function genCode(_genType: RbProcGenType = pgtInterface): string;
            virtual; abstract;

        function version: word;
        procedure setVersion(_ver: word);
    end;

    { RbList }

    generic RbList<Rb> = class
    private

        function initKeyStore: boolean;
        function storeKey(_name: string; index: integer): boolean;
    protected type
        GObjectList = specialize GenericHashObjectList<Rb>;
    protected

        //FNames: RbStringList;
        //FObjects: TList;
        FOwner: RbCodeGen;
        FObjectList: GObjectList;
    public
        constructor Create; overload;
        {Destroy everything}
        destructor Destroy; override;
        {Destroy only list object. Don't destroy members}
        destructor ReleaseList;

        function get(_name: string): Rb; // object factory
        function add(_name: string; _object: Rb): integer; overload;
        function add(_object: Rb): integer; overload;
        function getObject(_name: string): Rb; virtual; overload;
        function getObject(_index: integer): Rb; virtual; overload;
        function getName(_index: integer): string; virtual; overload;
        function Count: integer;

        procedure Owner(_owner: RbCodeGen); overload;
        function Owner: RbCodeGen; overload;

        function genCode(_genType: RbProcGenType = pgtInterface): string; virtual;
        {Returns a section declaration like "type" or "const" or "var"}
        class function declaration: string; virtual;
    end;


    RbUnit = class;
    RbUnitListBase = specialize RbList<RbUnit>;

    { RbUnitListHelper }

    RbUnitList = class(RbUnitListBase)
        function genCode(_genType: RbProcGenType = pgtInterface): string; override;
    end;

    RbProc = class;
    RbProcListBase = specialize RbList<RbProc>;

    { RbProcList }
    RbProcList = class(RbProcListBase)
        function construct(_name: string = 'Create'): RbProc;
        function destruct(_name: string = 'Destroy'): RbProc;
        function func(_name: string): RbProc;
        function proc(_name: string): RbProc;
        function getProcType(_procType: RbProcType): RbProcList;
    end;

    RbClass = class;
    RbClassList = specialize RbList<RbClass>;

    RbRecord = class;
    RbRecordList = specialize RbList<RbRecord>;

    RbObject = class;
    RbObjectList = specialize RbList<RbObject>;

    RbType = class;
    RbTypeList = specialize RbList<RbType>;

    RbConst = class;
    RbConstListBase = specialize RbList<RbConst>;

	{ RbConstList }

    RbConstList = class(RbConstListBase)
        function genCode(_genType: RbProcGenType = pgtInterface): string; override;
        function new_(_name: string): RbConst;
        class function declaration: string; override;
	end;

    RbResourceString = class;
    RbResourceStringListBase = specialize RbList<RbResourceString>;

	{ RbResourceStringList }

    RbResourceStringList = class(RbResourceStringListBase)
        function genCode(_genType: RbProcGenType = pgtInterface): string;
        function new_(_name: string): RbResourceString;
        class function declaration: string; override;
    end;


    RbEnum = class;
    RbEnumList = specialize RbList<RbEnum>;

    RbVar = class;
    RbVarListBase = specialize RbList<RbVar>;

    { RbVarList }

    RbVarList = class(RbVarListBase)
        function genCode(_genType: RbProcGenType = pgtInterface): string;
        function new_(_name: string): RbVar;
    end;

    RbProp = class;
    RbPropListBase = specialize RbList<RbProp>;

    { RbPropList }

    RbPropList = class(RbPropListBase)
        function new_(_name: string): RbProp;
        function putMembersInClass(_owner: RbClass): integer;
        function getPropertyDefinitions: string;
    end;



    { RbPascalProject }

    RbPascalProject = class(RbCodeGen)
    private
        FUnits: RbUnitList;
    public
        constructor Create; override;
        destructor Destroy; override;

        function genCode(_genType: RbProcGenType = pgtInterface): string; override;

        function unit_(_name: string): RbUnit;
        function unit_(_index: integer): RbUnit;
        function unitCount: integer;
        function unitList: RbUnitList;
        function saveUnits(target_folder: string = './'): integer;
        // returns number of files saved
    end;

    { RbUnit }

    RbUnit = class(RbCodeGen)
    private
        FMembers: TList;
    public
        function Members: TList;

    public
        constructor Create; override;
        destructor Destroy; override;

    private
        Fintf_consts: RbConstList;
    public
        function intf_consts(_intf_consts: RbConstList): RbUnit; overload;
        function intf_consts: RbConstList; overload;

    private
        Fintf_resource_strings: RbResourceStringList;
    public
        function intf_resource_strings(_intf_resource_strings: RbResourceStringList
			): RbUnit; overload;
        function intf_resource_strings: RbResourceStringList; overload;

    private
        Fintf_types: RbTypeList;
    public
        function intf_types(_intf_types: RbTypeList): RbUnit; overload;
        function intf_types: RbTypeList; overload;

    private
        Fintf_enum: RbEnumList;
    public
        function intf_enum(_intf_enum: RbEnumList): RbUnit; overload;
        function intf_enum: RbEnumList; overload;
    private
        Fintf_records: RbRecordList;
    public
        function intf_records(_intf_records: RbRecordList): RbUnit; overload;
        function intf_records: RbRecordList; overload;

    private
        Fintf_classes: RbClassList;
    public
        function classes(_intf_classes: RbClassList): RbUnit; overload;
        function classes: RbClassList; overload;

    private
        Fintf_vars: RbVarList;
    public
        function intf_vars(_intf_vars: RbVarList): RbUnit; overload;
        function intf_vars: RbVarList; overload;

    private
        Fimpl_consts: RbConstList;
    public
        function impl_consts(_impl_consts: RbConstList): RbUnit; overload;
        function impl_consts: RbConstList; overload;
    private
        Fimpl_types: RbTypeList;
    public
        function impl_types(_impl_types: RbTypeList): RbUnit; overload;
        function impl_types: RbTypeList; overload;

    private
        Fimpl_records: RbRecordList;
    public
        function impl_records(_impl_records: RbRecordList): RbUnit; overload;
        function impl_records: RbRecordList; overload;
    private
        Fimpl_vars: RbVarList;
    public
        function impl_vars(_impl_vars: RbVarList): RbUnit; overload;
        function impl_vars: RbVarList; overload;

    private
        Fintf_uses: string;
    public
        function intf_uses(_intf_uses: string): RbUnit; overload;
        function intf_uses: string; overload;
    private
        Fintf_code: string;
    public
        function intf_code(_intf_code: string): RbUnit; overload;
        function intf_code: string; overload;

    private
        Fintf_procs: RbProcList;
    public
        function intf_procs(_intf_procs: RbProcList): RbUnit; overload;
        function intf_procs: RbProcList; overload;

    private
        Fimpl_uses: string;
    public
        function impl_uses(_impl_uses: string): RbUnit; overload;
        function impl_uses: string; overload;

    private
        Fimpl_procs: RbProcList;
    public
        function impl_procs(_impl_procs: RbProcList): RbUnit; overload;
        function impl_procs: RbProcList; overload;

    private
        Fimpl_code: string;
    public
        function impl_code(_impl_code: string): RbUnit; overload;
        function impl_code: string; overload;
    private
        Finit_code: string;
    public
        function init_code(_init_code: string): RbUnit; overload;
        function init_code: string; overload;
    private
        Ffinl_code: string;
    public
        function finl_code(_finl_code: string): RbUnit; overload;
        function finl_code: string; overload;

    public
        function genCode(_genType: RbProcGenType = pgtInterface): string; override;
        function saveUnit(_target_folder: string = '.'): string;

        function unit_header: string;

    end;

    { RbType }

    RbType = class(RbCodeGen)
    private
        FtypeText: string;
    public
        function typeText(_typeText: string): RbType; overload;
        function typeText: string; overload;

        function genCode(_genType: RbProcGenType = pgtInterface): string; override;
    end;

    RbClassMembers = class;
    RbPublicClassMembers = class;
    RbPrivateClassMembers = class;
    RbProtectedClassMembers = class;

    { RbClass }
    RbClass = class(RbType)
    protected
        FAutoInstantiate: RbVarList;
        Fpubl: RbPublicClassMembers;
        Fpriv: RbPrivateClassMembers;
        Fprot: RbProtectedClassMembers;

        function intf_code: string;
        function impl_code: string;
        procedure AutoInstantiateObjects;



    public
        function publ: RbPublicClassMembers;
        function priv: RbPrivateClassMembers;
        function prot: RbProtectedClassMembers;

        constructor Create; override;
        destructor Destroy; override;
        function classDefinition: string; virtual;
        function genCode(_genType: RbProcGenType): string; override;

    protected
        Finherits: RbClass;
        Finherits_str: string;
    public
        function inherits(_inherits: RbClass): RbClass; overload;
        function inheritsObj: RbClass; overload;
        function inherits(_inherits_str: string): RbClass; overload;
        function inherits: string; overload;
        function inheritedClass: string;

        function inUnit: RbUnit;  {~ Returns the unit in which this class appears ~}
        function depends: string;
        {~ Returns a list of units that this class depends on ~}

        {~ adding properties ~}

        {~ READONLY ~}
        function {chainable} crProp(_name: string; _data_type: string;
            _comment: string = ''): RbClass; overload;
        function {chainable} crProp(_name: string; _data_type: DDataType;
            _comment: string = ''): RbClass; overload;
        function {chainable} crProp(_name: string; _dataObj: RbType;
            _comment: string = ''): RbClass; overload;
        function {chainable} crProp(_var: RbVar): RbClass; overload;

        {~ READ/WRITE ~}
        function {chainable} crwProp(_name: string; _data_type: string;
            _comment: string = ''; _addField: boolean = True): RbClass; overload;
        function {chainable} crwProp(_name: string; _data_type: DDataType;
            _comment: string = ''; _addField: boolean = True): RbClass; overload;
        function {chainable} crwProp(_name: string; _dataObj: RbType;
            _comment: string = ''; _addField: boolean = True): RbClass; overload;
        function {chainable} crwProp(_var: RbVar): RbClass; overload;

        {~ READONLY ~}
        function {assignable}rProp(_name: string; _data_type: string;
            _comment: string = ''): RbClass; overload;
        function {assignable}rProp(_name: string; _data_type: DDataType;
            _comment: string = ''): RbClass; overload;
        function {assignable}rProp(_name: string; _dataObj: RbType;
            _comment: string = ''): RbClass; overload;
        function {assignable}rProp(_var: RbVar): RbClass; overload;

        {~ READ/WRITE ~}
        function {assignable}rwProp(_name: string; _data_type: string;
            _comment: string = ''): RbClass; overload;
        function {assignable}rwProp(_name: string; _data_type: DDataType;
            _comment: string = ''): RbClass; overload;
        function {assignable}rwProp(_name: string; _dataObj: RbType;
            _comment: string = ''): RbClass; overload;
        function {assignable}rwProp(_var: RbVar): RbClass; overload;
    end;

    { RbHelperClass }

    RbHelperClass = class(RbClass)
    private
        myHelperFor: string;
    public
        function HelperFor(_HelperFor: string): RbHelperClass; overload;
        function HelperFor: string; overload;
    end;

    { RbTypeHelper }
    RbTypeHelper = class(RbHelperClass)
        function classDefinition: string; override;
    end;

    { RbRecordHelper }

    RbRecordHelper = class(RbHelperClass)
        function classDefinition: string; override;
    end;

    { RbClassHelper }

    RbClassHelper = class(RbHelperClass)
        function classDefinition: string; override;
    end;

    { RbRecord }

    RbRecord = class(RbType)
    private
        myMembers: RbVarList;
    public
        constructor Create; override;
        destructor Destroy; override;
        function add: RbVar;
        function genCode(_genType: RbProcGenType = pgtInterface): string; override;
    end;

    { RbObject }

    RbObject = class(RbClass)
        function classDefinition: string; override;
    end;

    RbConst = class(RbType)
    private
        myValue: string;
    public
        function str_(_value: string): RbConst; virtual;
        function literal(_value: string): RbConst; virtual;
        function genCode(_genType: RbProcGenType = pgtInterface): string; override;
    end;

	{ RbResourceString }

    RbResourceString = class(RbConst)

	end;

    { RbEnum }

    RbEnum = class(RbType)
        Fmembers: RbStringList;
        constructor Create; override;
        destructor Destroy; override;
        function add(_enum: string): RbEnum;
        function Count: integer;
        function get(index: integer): string;
        function members: string;
        function members(_mem: string): RbEnum;

        function genCode(_genType: RbProcGenType = pgtInterface): string; override;
    end;

    { RbVar }
    RbVar = class(RbType)
    private
        FdataType: DDataType;
        FdataTypeText: string;
        FSize: integer;
        function hasPrefix(_name: string): boolean; overload;
    public
        function dataType(_var_type: DDataType): RbVar; overload;
        function dataType(_var_type: RbType): RbVar; overload;
        function dataType(_var_type: string): RbVar; overload;
        function dataType: DDataType; overload;

        function dataTypeText(_dataTypeText: string): RbVar; overload;
        function dataTypeText: string; overload;

        function Size(_Size: integer): RbVar; overload;
        function Size: integer; overload;

        function genCode(_genType: RbProcGenType = pgtInterface): string; override;
        function NameNoPrefix: string;
        function hasPrefix: boolean; overload;

        constructor Create; override;
        constructor Create(_name: string; _varType: string);
        constructor Create(_name: string; _varObj: RbType);
        constructor Create(_name: string; _varType: DDataType);

    end;

    {Properties}

    { RbProp }

    { RbProc }
    RbProc = class(RbCodeGen)
    const
        _SIGN = '%s%s(%s);%s';
        PROC_SIGN = 'procedure ' + _SIGN;
        CNST_SIGN = 'constructor ' + _SIGN;
        DSTR_SIGN = 'destructor ' + _SIGN;
        FUNC_SIGN = 'function %s%s(%s):%s;%s';
    protected
        function getSignature: string;
        function getCode(genType: RbProcGenType = pgtInterface): string;
    public
        function genCode(_genType: RbProcGenType = pgtInterface): string; override;
    private
        FProcType: RbProcType;
    public
        function ProcType(_ProcType: RbProcType): RbProc; overload;
        function ProcType: RbProcType; overload;

    private
        Fparams: string;
    public
        function params(_params: string): RbProc; overload;
        function params: string; overload;
    private
        Freturn_type: string;
    public
        function return_type(_return_type: string): RbProc; overload;
        function return_type: string; overload;
    private
        Fcode: string;
    public
        function code(_code: string): RbProc; overload;
        function code: string; overload;
    private
        Fclass_: RbClass;
    public
        function class_(_class: RbClass): RbProc; overload;
        function class_: RbClass; overload;
    private
        Fmodifiers: string;
    public
        function modifiers(_modifiers: string): RbProc; overload;
        function modifiers: string; overload;
    private
        FcodeDescription: string;
    public
        function codeDescription(_codeDescription: string): RbProc; overload;
        function codeDescription: string; overload;

    public
        constructor Create; override;
    end;

    RbProp = class(RbVar)
    private
        bDestroy: boolean;
        myReadProc: RbProc;
        myWriteProc: RbProc;
        function genInterfaceCode: string;
        function genImplementationCode: string;
    public
        type
        RbPropertyType = (propReadOnlyField, propReadOnlyProc,
            propReadFieldWriteProc, propReadProcWriteProc, propReadProcWriteField);
    public
        propType: RbPropertyType;
        // owner: RbClass;
        function readerCode(_code: string): RbProp;
        function writerCode(_code: string): RbProp;
        function genCode(_genType: RbProcGenType = pgtInterface): string; override;

        function putMembersInClass: boolean;
        function getPropertyDefinition: string;

        constructor Create; override;
        constructor Create(_owner: RbClass);
        destructor Destroy; override;
    end;

    { RbClassMembers }

    RbClassMembers = class(RbCodeGen)
    private
        FVars_: RbVarList;
    public
        function vars: RbVarList;

    private
        FConsts_: RbConstList;
    public
        function consts: RbConstList;

    private
        FProcs_: RbProcList;
    public
        function procs: RbProcList; overload;
        function Owner(_Owner: RbClass): RbClassMembers; reintroduce;
        function Owner: RbClass; reintroduce;

    private
        FProps_: RbPropList;
    public
        function props: RbPropList;

    public
        function genCode(_genType: RbProcGenType = pgtInterface): string; override;
        function visibility: string; virtual; abstract;
        constructor Create; override;
        destructor Destroy; override;
    end;

    { RbPrivateClassMembers }

    RbPrivateClassMembers = class(RbClassMembers)
    public
        function visibility: string; override;
    end;

    { RbProtectedClassMembers }

    RbProtectedClassMembers = class(RbClassMembers)
    public
        function visibility: string; override;
    end;

    { RbPublicClassMembers }

    RbPublicClassMembers = class(RbClassMembers)
    public
        function visibility: string; override;
        function getConstructor: RbProc;
        function getDestructor: RbProc;
    end;


{~  This returns the type associated with the DDataType enum ~}
function VarType(_var_type: DDataType): RbType;

procedure codeFormat(_active: boolean);
function codeFormat: boolean;

procedure jcfPath(_path: string);
function jcfPath: string;

{Generates a pascal string literal with quotes and linebreaks}
function formatPascalString(_source: string): string;


implementation

const
    PRIVATE_DECL = 'private';
    PROTECTED_DECL = 'protected';
    PUBLIC_DECL = 'public';
    PRIVATE_FIELD_PREFIX = 'my';

    DEFINITION_CLASS = '%s = class%s';
    DEFINITION_OBJECT = '%s = object';
    DEFINITION_HELPER_CLASS = '%s = %s helper%s for %s';

    {e.g.: const _name : string}
    CHAIN_PROP_FUNCTION_SIGNATURE = 'const _%s: %s';
    {e.g. myName := _name; Result:= Self;}
    CHAIN_PROP_SET_FUNCTION_BODY = '%0:s := _%0:s; Result := Self;';
    {e.g. Result := _name;}
    CHAIN_PROP_GET_FUNCTION_BODY = 'Result := %s;';
    {$IFDEF Unix}
    JCF_PATH  = './jcf';
    //JCF_FLAGS = '-config=/home/stanley/.lazarus/jcfsettings.cfg -inplace -y ';
    JCF_FLAGS = '-config=./jcfsettings.cfg -inplace -y ';
    {$ELSE}
    JCF_PATH = './jcf.exe';
    JCF_FLAGS = '-config=./jcfsettings.cfg -inplace -y ';
    {$ENDIF}
var
    myCodeFormat: boolean = false;
    myJCFPath: string = JCF_PATH;

{~  This returns the type associated with the DDataType enum ~}
function VarType(_var_type: DDataType): RbType;
begin
    {
        dtInterface,
        dtPointer,
        dtMethodPointer,
        dtCustom)
    }

    case _var_type of
        dtRecord: Result := RbRecord.Create;
        dtObject: Result := RbObject.Create;
        dtClass: Result := RbClass.Create;
        dtEnum: Result := RbEnum.Create;
        else
        begin
            Result := RbVar.Create;
            RbVar(Result).dataType(_var_type);
        end;
    end;
end;

procedure codeFormat(_active: boolean);
begin
    myCodeFormat:= _active;
end;

function codeFormat: boolean;
begin
    Result:= myCodeFormat;
end;

procedure jcfPath(_path: string);
begin
    myJCFPath:= _path;
end;

function jcfPath: string;
begin
    Result:= myJCFPath;
end;

function formatPascalString(_source: string): string;
var
    s: TSTringList;
    _line: string;
    bAddTerminator: boolean = false;
begin
    Result:= '';
    s:= TStringList.Create;
    s.Text:= _source;

    for _line in s do
    begin
        if bAddTerminator then
        begin
            {This is on the next line, so end the quote, add linebreak and add +}
            Result:= Result + format(''' + sLinebreak %s + ''',[sLineBreak]);
        end
        else
            bAddTerminator:= true;
        Result:= Result + _line.Replace('''',''''''); {escape quotes if the string has it}
    end;
    {Put quotes}
    Result:= ''''+ Result +'''';
    s.Free;
end;

{ RbResourceStringList }

function RbResourceStringList.genCode(_genType: RbProcGenType): string;
begin
    Result := inherited genCode(_genType);
end;

function RbResourceStringList.new_(_name: string): RbResourceString;
begin
    Result := get(_name);
end;

class function RbResourceStringList.declaration: string;
begin
	Result:='resourcestring';
end;

{ RbResourceString }



{ RbConst }

function RbConst.str_(_value: string): RbConst;
begin
    Result:= self;
    myValue:= '''' + _value + '''';
end;

function RbConst.literal(_value: string): RbConst;
begin
    Result:= self;
    myValue:= _value;
end;

function RbConst.genCode(_genType: RbProcGenType): string;
begin
    {
        {COMMENT}
        const=value;
    }
	Result := Format('{ %s }%s%s = %s; ', [Comment, sLineBreak, Name, myValue]);
end;

{ RbConstList }

function RbConstList.genCode(_genType: RbProcGenType): string;
begin
    Result := inherited genCode(_genType);
end;

function RbConstList.new_(_name: string): RbConst;
begin
    Result := get(_name);
end;

class function RbConstList.declaration: string;
begin
    Result:='const';
end;

{ RbPropList }

function RbPropList.new_(_name: string): RbProp;
begin
    Result := get(_name);
end;

function RbPropList.putMembersInClass(_owner: RbClass): integer;
var
    i: integer;
begin
    Result:= Count;
    for i:= 0 to Pred(Result) do
    begin
        with getObject(i) do
        begin
            owner(_owner);
            putMembersInClass;
		end;
	end;
end;

function RbPropList.getPropertyDefinitions: string;
var
    i: integer;
begin
    Result:= '';
    for i:= 0 to Pred(Count) do
    begin
        with getObject(i) do
        begin
            Result:= Result + getPropertyDefinition + sLineBreak;
		end;
    end;
end;

{ RbProp }

function RbProp.readerCode(_code: string): RbProp;
begin
    Result := self;
    myReadProc.code(_code);
end;

function RbProp.writerCode(_code: string): RbProp;
begin
    Result := self;
    myWriteProc.Code(_code);
end;

function RbProp.genInterfaceCode: string;
var
    propertyDefinition: string = '';
    _genType: RbProcGenType = pgtInterface;
begin

    Result := '';
    exit;

    //propertyDefinition := 'property ' + NameNoPrefix + ':' +  getPascalDataTypes[dataType];
    //
    //{read from field}
    //if propType in [propReadOnlyField, propReadFieldWriteProc] then
    //begin
    //    Result := 'protected' + sLinebreak + inherited genCode(_genType);
    //    {the variable}
    //    propertyDefinition := propertyDefinition + ' read ' + Name;
    //end;
    //
    //{read from function}
    //if propType in [propReadOnlyProc, propReadProcWriteProc] then
    //begin
    //    Result := 'protected' + sLinebreak + myReadProc.genCode(_genType);
    //    propertyDefinition := propertyDefinition + ' read ' + myReadProc.Name;
    //end;
    //
    //{write to procedure}
    //if propType in [propReadFieldWriteProc, propReadProcWriteProc] then
    //begin
    //    Result := Result + sLineBreak + myWriteProc.genCode(_genType);
    //    propertyDefinition := propertyDefinition + ' write ' + myWriteProc.Name;
    //end;
    //
    //{write to field}
    //if (propType = propReadProcWriteField) then
    //begin
    //    propertyDefinition := propertyDefinition + ' write ' + Name;
    //end;
    //
    //propertyDefinition := propertyDefinition + ';';
    //
    //Result := Result + sLineBreak + '{???} public' + sLineBreak + propertyDefinition;
end;

function RbProp.genImplementationCode: string;
var
_genType: RbProcGenType = pgtImplementation;
begin
    Result := '';
    exit;
    //if propType in [propReadOnlyProc, propReadProcWriteProc] then
    //begin
    //    Result := Result + myReadProc.genCode(_genType) + sLineBreak;
    //end;
    //if propType in [propReadFieldWriteProc, propReadProcWriteProc] then
    //begin
    //    Result := Result + myWriteProc.genCode(_genType);
    //end;
end;

function RbProp.genCode(_genType: RbProcGenType): string;
begin
    myReadProc.Name('get' + NameNoPrefix);
    myReadProc.Owner(FOwner);
    myReadProc.ProcType(procFunction);
    myReadProc.return_type(dataTypeText);

    myWriteProc.Name('set' + NameNoPrefix);
    myWriteProc.Owner(FOwner);
    myWriteProc.params('const _' + NameNoPrefix + ':' + dataTypeText);
    myWriteProc.ProcType(procProcedure);

    case _genType of
        pgtInterface:
        Result := genInterfaceCode;

        pgtImplementation:
        Result := genImplementationCode;

        pgtBoth:
        Result := genInterfaceCode + sLineBreak + genImplementationCode;
    end;
end;

function RbProp.putMembersInClass: boolean;
begin
    myReadProc.Name('get' + NameNoPrefix);
    myReadProc.Owner(FOwner);
    myReadProc.ProcType(procFunction);
    myReadProc.return_type(dataTypeText);

    myWriteProc.Name('set' + NameNoPrefix);
    myWriteProc.Owner(FOwner);
    myWriteProc.params('const _' + NameNoPrefix + ':' + dataTypeText);
    myWriteProc.ProcType(procProcedure);

    {read from field}
    if propType in [propReadOnlyField, propReadFieldWriteProc] then
    begin
        RbClass(owner).prot.vars.add(self);
    end;

    RbClass(owner).prot.procs.add(myReadProc);
    RbClass(owner).prot.procs.add(myWriteProc);
    bDestroy:= false;
end;

function RbProp.getPropertyDefinition: string;
var
    propertyDefinition: string = '';
begin
    Result := '';
    propertyDefinition := 'property ' + NameNoPrefix + ':' +  getPascalDataTypes[dataType];

    {read from field}
    if propType in [propReadOnlyField, propReadFieldWriteProc] then
    begin
       propertyDefinition := propertyDefinition + ' read ' + Name;
    end;

    {read from function}
    if propType in [propReadOnlyProc, propReadProcWriteProc] then
    begin
        propertyDefinition := propertyDefinition + ' read ' + myReadProc.Name;
    end;

    {write to procedure}
    if propType in [propReadFieldWriteProc, propReadProcWriteProc] then
    begin
       propertyDefinition := propertyDefinition + ' write ' + myWriteProc.Name;
    end;

    {write to field}
    if (propType = propReadProcWriteField) then
    begin
        propertyDefinition := propertyDefinition + ' write ' + Name;
    end;
    propertyDefinition := propertyDefinition + ';';

    Result:= propertyDefinition;
end;

constructor RbProp.Create;
begin
    inherited Create;
    propType := propReadFieldWriteProc;
    myReadProc := RbProc.Create;
    myWriteProc := RbProc.Create;
    bDestroy:= true;
end;

constructor RbProp.Create(_owner: RbClass);
begin
    Create;
    Owner(_owner);
end;

destructor RbProp.Destroy;
begin
    if bDestroy then
    begin
        myReadProc.Free;
        myWriteProc.Free;
	end;
	inherited Destroy;
end;

{ RbStringList }

function RbStringList.Add(const S: string): integer;
begin
    if (S.Length > 0) then
        Result := inherited Add(S);
end;

{ RbHelperClass }

function RbHelperClass.HelperFor(_HelperFor: string): RbHelperClass;
begin
    myHelperFor := _HelperFor;
    Result := Self;
end;

function RbHelperClass.HelperFor: string;
begin
    Result := myHelperFor;
end;

{ RbClassHelper }

function RbClassHelper.classDefinition: string;
begin
    Result := Format(DEFINITION_HELPER_CLASS, [Name, 'class',
        inheritedClass, HelperFor]);
end;

{ RbRecordHelper }

function RbRecordHelper.classDefinition: string;
begin
    Result := Format(DEFINITION_HELPER_CLASS, [Name, 'record',
        inheritedClass, HelperFor]);
end;

{ RbTypeHelper }

function RbTypeHelper.classDefinition: string;
begin
    Result := Format(DEFINITION_HELPER_CLASS, [Name, 'type', inheritedClass, HelperFor]);
end;

{RbList}
constructor RbList.Create;
var
    a: integer;
begin
    inherited Create;
    //FObjects := TList.Create;
    //initKeyStore;
    FObjectList:= GObjectList.Create();
end;


destructor RbList.Destroy;
var
    index: integer;
    obj: TObject;
begin
    FObjectList.Free;
    //for index := 0 to FObjects.Count - 1 do
    //begin
    //    obj := TObject(FObjects.Items[index]);
    //    if Assigned(obj) then
    //        FreeAndNil(obj);
    //end;
    //
    //FObjects.Destroy;
    //FNames.Destroy;

    inherited Destroy;
end;

destructor RbList.ReleaseList;
begin
    {Only release the list, not members}
    FObjectList.OwnsObjects:= False;
    FObjectList.Free;
    //FObjects.Destroy;
    //FNames.Destroy;
    inherited Destroy;
end;

function RbList.get(_name: string): Rb;
begin
    Result:= FObjectList.find(_name);
    if not assigned(Result) then
    begin
        Result:= FObjectList.get(_name);
        Result.Owner(FOwner);
        Result.Name(_name);
	end;

//    Result := getObject(_name);
//    if Result = nil then
//    begin
//        Result := Rb.Create;
//        Result.Owner(FOwner);
//        Result.Name(_name);
//        add(_name, Result);
//    end;
end;

function RbList.initKeyStore: boolean;
begin
    // Define the look up table
    //FNames := RbStringList.Create;
    //FNames.Sorted := True;
    Result := True;
end;

function RbList.storeKey(_name: string; index: integer): boolean;
begin
    //FNames.Sorted := False;
    //FNames.Values[_name] := index.ToString;
    //FNames.Sorted := True;
    Result := True;
end;


function RbList.add(_name: string; _object: Rb): integer;
var
    _pName: PChar;
begin
    if _object = nil then
        raise Exception.CreateFmt(
            'add(_name: string; obj: Rb)::  Trying to add Nil object to %s',
            [ClassName]);
    _object.Name(_name); // assign name to the object
    Result := add(_object);
end;

function RbList.add(_object: Rb): integer;
begin
    if _object = nil then
        raise Exception.CreateFmt('add(obj: Rb):: Trying to add Nil object to %s',
            [ClassName]);

    FObjectList.add(_object.Name, _object);
//
//    Result := FObjects.Add(_object);
//    storeKey(_object.Name, Result{contains index});
end;

function RbList.getObject(_name: string): Rb;
var
    index: integer = -1;
    Value: string;
begin
    Result:= Rb(FObjectList.find(_name));
    //Result := nil;
    //Value := FNames.Values[_name];
    //if Value <> '' then
    //begin
    //    index := Value.ToInteger;
    //    Result := Rb(FObjects.Items[index]);
    //end;
end;

function RbList.getObject(_index: integer): Rb;
begin
    if _index < FObjectList.Count then
        Result:= Rb(FObjectList.Items[_index])
    else
        Result := nil;
	//Result := Rb(FObjects.Items[_index]);
end;

function RbList.getName(_index: integer): string;
begin
    //Result := FNames.Strings[_index];
    if _index < FObjectList.Count then
        Result := FObjectList.Names[_index]
    else
        Result:= '';
end;

function RbList.Count: integer;
begin
    //Result := FObjects.Count;
    Result := FObjectList.Count;
end;

procedure RbList.Owner(_owner: RbCodeGen);
begin
    FOwner := _owner;
end;

function RbList.Owner: RbCodeGen;
begin
    Result := FOwner;
end;

function RbList.genCode(_genType: RbProcGenType): string;
var
    _code_block: RbCodeGen;
    index: integer;
begin
    Result := '';
    for index := 0 to pred(Count) do
    begin
        _code_block := getObject(index);
        //WriteLn(Format('Generating %s=>%s', [_code_block.ClassName, _code_block.Name]));
        Result := Result + _code_block.genCode(_genType);
        if index < (Count - 1) then {No linebreak for last line}
            Result := Result + sLineBreak;
    end;
end;

class function RbList.declaration: string;
begin
    Result:= '';
end;

{ RbType }

function RbType.typeText(_typeText: string): RbType;
begin
    FtypeText := _typeText;
    Result := Self;
end;

function RbType.typeText: string;
begin
    Result := FtypeText;
end;

function RbType.genCode(_genType: RbProcGenType): string;
begin
    Result := Format('%s = %s; {%s}', [Name, typeText, comment]);
end;

{ RbEnum }

constructor RbEnum.Create;
begin
    inherited Create;
    FMembers := RbStringList.Create;
end;

destructor RbEnum.Destroy;
begin
    FreeAndNil(FMembers);
    inherited Destroy;
end;

function RbEnum.add(_enum: string): RbEnum;
begin
    Fmembers.Add(_enum);
    Result := Self;
end;

function RbEnum.Count: integer;
begin
    Result := FMembers.Count;
end;

function RbEnum.get(index: integer): string;
begin
    Result := FMembers.Strings[index];
end;

function RbEnum.members: string;
begin
    Result := FMembers.CommaText;
end;

function RbEnum.members(_mem: string): RbEnum;
begin
    FMembers.CommaText := _mem;
    Result := Self;
end;

function RbEnum.genCode(_genType: RbProcGenType): string;
begin
    Result := Format('%s = (%s);', [Name, Members]);
end;

{ RbObject }
function RbObject.classDefinition: string;
begin
    Result := Format(DEFINITION_OBJECT, [Name]);
end;

{ RbRecord }

constructor RbRecord.Create;
begin
    inherited Create;
    myMembers := RbVarList.Create;
end;

destructor RbRecord.Destroy;
begin
    FreeAndNil(myMembers);
    inherited Destroy;
end;

function RbRecord.add: RbVar;
begin
    Result := RbVar.Create;
    myMembers.add(Result);
end;

function RbRecord.genCode(_genType: RbProcGenType): string;
begin
    Result := Format('%s = record %s end;', [Name, myMembers.genCode(_genType)]);
end;


{ RbPublicClassMembers }

function RbPublicClassMembers.visibility: string;
begin
    Result := PUBLIC_DECL; // 'public' or 'published';
end;

{Returns the first constructor}
function RbPublicClassMembers.getConstructor: RbProc;
var
    _procList: RbProcList;
begin
    Result := nil;
    _procList := procs.getProcType(procConstructor);
    if _procList.Count > 0 then
        Result := _procList.getObject(0);
    _procList.ReleaseList;
end;

{Returns the first destructor}
function RbPublicClassMembers.getDestructor: RbProc;
var
    _procList: RbProcList;
begin
    Result := nil;
    _procList := procs.getProcType(procDestructor);
    if _procList.Count > 0 then
        Result := _procList.getObject(0);
    _procList.ReleaseList;
end;

{ RbProtectedClassMembers }
function RbProtectedClassMembers.visibility: string;
begin
    Result := PROTECTED_DECL; // 'protected'
end;

{ RbPrivateClassMembers }

function RbPrivateClassMembers.visibility: string;
begin
    Result := PRIVATE_DECL; // 'private';
end;

{ RbPrivateClassMembers }


{ RbVar }

function RbVar.dataType(_var_type: DDataType): RbVar;
begin
    FdataType := _var_type;
    FdataTypeText := data_types_pascal[_var_type];
    Result := Self;
end;

function RbVar.dataType(_var_type: RbType): RbVar;
begin
    FdataTypeText := _var_type.Name;
    if _var_type is RbVar then
    begin
        FdataType := RbVar(_var_type).dataType;
        FdataTypeText := RbVar(_var_type).dataTypeText;
    end
    else if _var_type is RbRecord then
        FdataType := dtRecord
    else if _var_type is RbObject then
        FdataType := dtClass
    else if _var_type is RbClass then
        FdataType := dtClass
    else if _var_type is RbEnum then
        FdataType := dtEnum
    else
        raise Exception.Create('Cannot assign dataType for ' + _var_type.ClassName);

    Result := Self;
end;

function RbVar.dataType(_var_type: string): RbVar;
begin
    fdataType:= getDDLDataType(_var_type, data_types_pascal);
    fdataTypeText := _var_type;
    Result := Self;
end;

function RbVar.dataType: DDataType;
begin
    Result := FdataType;
end;

function RbVar.dataTypeText(_dataTypeText: string): RbVar;
begin
    FdataTypeText := _dataTypeText;
    Result := Self;
end;

function RbVar.dataTypeText: string;
begin
    Result := Format(FdataTypeText, [size]);
end;

function RbVar.Size(_Size: integer): RbVar;
begin
    FSize := _Size;
    Result := Self;
end;

function RbVar.Size: integer;
begin
    Result := FSize;
end;

function RbVar.genCode(_genType: RbProcGenType): string;
begin
    Result := Format('{%s}%s%s: %s;', [Comment, sLineBreak, Name, dataTypeText]);
end;

function RbVar.hasPrefix(_name: string): boolean;
var
    _len_prefix: integer;
begin
    _len_prefix := Length(PRIVATE_FIELD_PREFIX);
    Result := CompareStr(PRIVATE_FIELD_PREFIX, Copy(_name, 1, _len_prefix)) = 0;
end;

function RbVar.NameNoPrefix: string;
var
    _len_prefix: integer;
begin
    Result := Name;
    if hasPrefix(Result) then
    begin
        _len_prefix := Length(PRIVATE_FIELD_PREFIX);
        Result := Copy(Name, _len_prefix + 1, {because this is not zero based array}
            Length(Name) - _len_prefix);
    end;
end;

function RbVar.hasPrefix: boolean;
begin
    Result := hasPrefix(Name);
end;

constructor RbVar.Create;
begin
    inherited Create;
    dataType(DDataType.dtUnknown);
    FSize := 1;
end;

constructor RbVar.Create(_name: string; _varType: string);
begin
    inherited Create;
    Name(_name);
    dataType(_varType);
end;

constructor RbVar.Create(_name: string; _varObj: RbType);
begin
    inherited Create;
    Name(_name);
    dataType(_varObj);
end;

constructor RbVar.Create(_name: string; _varType: DDataType);
begin
    inherited Create;
    Name(_name);
    dataType(_varType);
end;

{ RbVarList }

function RbVarList.genCode(_genType: RbProcGenType): string;
begin
    Result := inherited genCode(_genType);
end;

function RbVarList.new_(_name: string): RbVar;
begin
    Result := get(_name);
end;


{ RbUnitListHelper }

function RbUnitList.genCode(_genType: RbProcGenType): string;
begin
    Result := inherited genCode(_genType);
end;

{ RbUnitListHelper }


{ RbClassMembers }

function RbClassMembers.genCode(_genType: RbProcGenType): string;
begin

end;

function RbClassMembers.vars: RbVarList;
begin
    Result := FVars_;
end;

function RbClassMembers.consts: RbConstList;
begin
    Result := FConsts_;
end;

function RbClassMembers.procs: RbProcList;
begin
    Result := FProcs_;
end;


function RbClassMembers.Owner(_Owner: RbClass): RbClassMembers;
begin
    inherited Owner(_Owner);
    FVars_.Owner(_Owner);
    FProcs_.Owner(_Owner);
    FProps_.Owner(_Owner);
    Result := self;
end;

function RbClassMembers.Owner: RbClass;
begin
    Result := RbClass(inherited Owner);
end;

function RbClassMembers.props: RbPropList;
begin
    Result := FProps_;
end;

constructor RbClassMembers.Create;
begin
    inherited Create;
    FVars_ := RbVarList.Create;
    FProcs_ := RbProcList.Create;
    FProps_ := RbPropList.Create;
    FConsts_ := RbConstList.Create;
    ;
end;

destructor RbClassMembers.Destroy;
begin

    try
        FreeAndNil(FVars_);
	except
        on E: Exception do
            writeln(classname, ' destroy FVars ', E.Message);
	end;

    try
        FreeAndNil(FConsts_);
	except
        on E: Exception do
            writeln(classname, ' destroy FConsts ', E.Message);
	end;

    try
	FreeAndNil(FProcs_);
	except
        on E: Exception do
            writeln(classname, ' destroy FProcs ', E.Message);
	end;

    try
    FreeAndNil(FProps_);
	except
        on E: Exception do
            writeln(classname, ' destroy FProps ', E.Message);
	end;

    try
    inherited Destroy;
	except
        on E: Exception do
            writeln(classname, 'destroy inherited ', E.Message);
	end;
    //writeln(Classname, ' has been destroyed');
end;

{ RbClass }

function RbClass.publ: RbPublicClassMembers;
begin
    Result := Fpubl;
end;

function RbClass.priv: RbPrivateClassMembers;
begin
    Result := Fpriv;
end;

function RbClass.prot: RbProtectedClassMembers;
begin
    Result := Fprot;
end;

constructor RbClass.Create;
begin
    inherited Create;
    FAutoInstantiate := RbVarList.Create;
    Fpubl := RbPublicClassMembers.Create;
    Fpubl.Owner(Self);

    Fprot := RbProtectedClassMembers.Create;
    Fprot.Owner(Self);

    Fpriv := RbPrivateClassMembers.Create;
    Fpriv.Owner(Self);
end;

destructor RbClass.Destroy;
begin
    FAutoInstantiate.ReleaseList; {because this contains references}
    FreeAndNil(Fpubl);
    FreeAndNil(Fprot);
    FreeAndNil(Fpriv);

    inherited Destroy;
end;

function RbClass.classDefinition: string;
begin
    {~  Class definition ~}
    Result := Format(DEFINITION_CLASS, [Name, inheritedClass]);
end;

function RbClass.genCode(_genType: RbProcGenType): string;
type
    TMemberList = specialize GenericHashObjectList<RBClassMembers>;
var
    _intf: RbStringList;
    _impl: RbStringList;
    _member_list: TMemberList;

    _class_members: RbClassMembers;
    _vars: RbVarList;
    _procs: RbProcList;
    _props: RbPropList;

    _obj: pointer;
    _inherits: string = '';
    i: integer;
    j: integer;

begin
    _intf := RbStringList.Create;
    _impl := RbStringList.Create;

    _member_list := TMemberList.Create(False);

    {~ Class Definition ~}
    _intf.Add(classDefinition);

    {~ Adding class members ~}
    try
        _member_list.Add('private', Fpriv); {~ Private Members ~}
        _member_list.Add('protected', Fprot); {~ Protected Members ~}
        _member_list.Add('public', Fpubl); {~ Public Members ~}

        if _genType = pgtInterface then
            AutoInstantiateObjects; // calls this only once.

        {~ Add object creation and destroy to constructor and destructor ~}

        for i := 0 to pred(_member_list.Count) do {~ Loop visibilty ~}
        begin

            _intf.Add(_member_list.NameOfIndex(i)); {visibility text}

            _class_members := _member_list.items[i];

            _vars  := _class_members.vars; {~ Variables ~}
            _procs := _class_members.procs;{~ Procedures and Functions ~}
            _props := _class_members.props;

            {~ Prepare class declaration  ~}

            //if (_vars.Count > 0) or (_procs.Count > 0) then
            //    _intf.Add(_class_members.visibility);

            if (_vars.Count > 0) then
                _intf.Add(_vars.genCode(pgtInterface));

            if (_procs.Count > 0) then
                _intf.Add(_procs.genCode(pgtInterface));

            if (_props.Count > 0) then
            begin
                _intf.Add(_props.getPropertyDefinitions);
            end;

            {~ Prepare class implementations ~}
            // _impl.Add(_props.genCode(pgtImplementation));
            _impl.Add(_procs.genCode(pgtImplementation));

        end; {~ Loop visibilty ~}

        {~ End of class ~}
        _intf.Add('end;');

        {~ Prepare function result ~}
        case _genType of
            pgtInterface:
                Result := _intf.Text;

            pgtImplementation:
                Result := _impl.Text;

            pgtBoth:
                Result := _intf.Text + sLineBreak + _impl.Text;

            else
                Result := '';
        end;
    finally
        FreeAndNil(_intf);
        FreeAndNil(_impl);
        FreeAndNil(_member_list);
    end;
end;

function RbClass.inherits(_inherits: RbClass): RbClass;
begin
    Finherits := _inherits;
    Result := Self;
end;

function RbClass.inheritsObj: RbClass;
begin
    Result := Finherits;
end;


function RbClass.inherits(_inherits_str: string): RbClass;
begin
    Finherits_str := _inherits_str;
    Result := self;
end;

function RbClass.inherits: string;
begin
    Result := Finherits_str;
    if Result.IsEmpty then
        if Assigned(Finherits) then
            Result := Finherits.Name;
end;

function RbClass.inheritedClass: string;
begin
    Result := '';
    {~ Generating class definition. Adds inheritance if available ~}
    if inherits <> '' then
        Result := format('(%s)', [inherits]);
end;

{~ Name of the unit that has this class ~}
function RbClass.inUnit: RbUnit;
begin
    Result := Owner as RbUnit;
end;

function RbClass.depends: string;
begin

end;

function RbClass.crProp(_name: string; _data_type: string; _comment: string): RbClass;
var
    _var: RbVar;
begin
    {~ Chainable Readonly Property ~}
    _var := RbVar.Create(_name, _data_type);
    _var.Comment(_comment);
    Result := crProp(_var);
end;

function RbClass.crProp(_name: string; _data_type: DDataType;
    _comment: string): RbClass;
var
    _var: RbVar;
begin
    _var := RbVar.Create(_name, _data_type);
    _var.Comment(_comment);
    Result := crProp(_var);
end;

function RbClass.crProp(_name: string; _dataObj: RbType; _comment: string): RbClass;
var
    _var: RbVar;
begin
    _var := RbVar.Create(_name, _dataObj);
    _var.Comment(_comment);
    Result := crProp(_var);
end;

function RbClass.crProp(_var: RbVar): RbClass;
var
    _temp_name: string;
    p: RbProc;
begin
    if not _var.hasPrefix then
    begin
        _temp_name := _var.Name;
        _var.Name(PRIVATE_FIELD_PREFIX + _temp_name);
    end;

    priv.vars.add(_var);

    if _var.dataType in non_primitive_types then
        FAutoInstantiate.add(_var);

    p := RbProc.Create;
    p.Owner(self);
    p.Comment('GET: ' + _var.Comment);

    p.ProcType(RbProcType.procFunction)
        .return_type(_var.dataTypeText)
        .modifiers('overload;')
        .code(Format(CHAIN_PROP_GET_FUNCTION_BODY, [_var.Name]));

    publ.procs.add('{get}' + _var.NameNoPrefix, p); // Sets the proc's name here.

    Result := self;
end;

function RbClass.crwProp(_name: string; _data_type: string; _comment: string;
    _addField: boolean): RbClass;
var
    _var: RbVar;
begin
    _var := RbVar.Create(_name, _data_type);
    _var.Comment(_comment);
    Result := crwProp(_var);
end;

function RbClass.crwProp(_name: string; _data_type: DDataType;
    _comment: string; _addField: boolean): RbClass;
var
    _var: RbVar;
begin
    _var := RbVar.Create(_name, _data_type);
    _var.Comment(_comment);
    Result := crwProp(_var);
end;

function RbClass.crwProp(_name: string; _dataObj: RbType; _comment: string;
    _addField: boolean): RbClass;
var
    _var: RbVar;
begin
    _var := RbVar.Create(_name, _dataObj);
    _var.Comment(_comment);
    Result := crwProp(_var);
end;

function RbClass.crwProp(_var: RbVar): RbClass;
var
    _temp_name: string;
    p: RbProc;
begin
    if not _var.hasPrefix then
    begin
        _temp_name := _var.Name;
        _var.Name(PRIVATE_FIELD_PREFIX + _temp_name);
    end;

    {~ Creates the private variable and read function ~}
    crProp(_var);

    {~ Now create the assignment function that sets the field and returns the class ~}
    p := RbProc.Create;
    p.Owner(self);
    p.Comment('SET: ' + _var.Comment);

    p.ProcType(RbProcType.procFunction)
        .modifiers('overload;')
    // Function return type
        .return_type(Self.Name)
    // Function parameters
        .params(Format(CHAIN_PROP_FUNCTION_SIGNATURE, [_var.Name, _var.dataTypeText]))
    // Function body
        .code(Format(CHAIN_PROP_SET_FUNCTION_BODY, [_var.Name]));

    publ.procs.add('{set}' + _var.NameNoPrefix, p);
    Result := Self;

end;

function RbClass.rProp(_name: string; _data_type: string; _comment: string): RbClass;
begin

end;

function RbClass.rProp(_name: string; _data_type: DDataType;
    _comment: string): RbClass;
begin

end;

function RbClass.rProp(_name: string; _dataObj: RbType; _comment: string): RbClass;
begin

end;

function RbClass.rProp(_var: RbVar): RbClass;
begin

end;

function RbClass.rwProp(_name: string; _data_type: string; _comment: string): RbClass;
begin

end;

function RbClass.rwProp(_name: string; _data_type: DDataType;
    _comment: string): RbClass;
begin

end;

function RbClass.rwProp(_name: string; _dataObj: RbType; _comment: string): RbClass;
begin

end;

function RbClass.rwProp(_var: RbVar): RbClass;
begin

end;

function RbClass.intf_code: string;
var
    code: RbStringList;
begin
    code := RbStringList.Create;
    try
        code.Add(Format('%s = class', [Name]));
        code.Add('// Class definition');
        code.Add('end;');
        Result := code.Text;
    finally
        FreeAndNil(code);
    end;

end;

function RbClass.impl_code: string;
begin
    Result := Format('// %s: impl_code not implemented', [ClassName]);
end;

procedure RbClass.AutoInstantiateObjects;
var
    _constructor: RbProc;
    _destructor: RbProc;
    i, j: integer;
    code_constructor: string = '';
    code_destructor: string = '';
    vars: RbVarList;
    _var: RbVar;
    instantiated: boolean = False;
    _members: TList;
    props: RbPropList;
begin

    _members:= TList.Create;
    _members.add(Fpriv);
    _members.add(Fprot);
    _members.add(Fpubl);

    _constructor := publ.getConstructor;
    if Assigned(_constructor) then
        code_constructor := _constructor.code;

    _destructor := publ.getDestructor;
    if Assigned(_destructor) then
        code_destructor := _destructor.code;

    for i := 0 to _members.Count - 1 do
    begin
        props:= RbClassMembers(_members.Items[i]).props;
        props.putMembersInClass(Self);

        vars := RbClassMembers(_members.Items[i]).vars;
        for j := 0 to vars.Count - 1 do
        begin

            _var := vars.getObject(j);
            if _var.dataType in non_primitive_types then
            begin
                instantiated := True;
                if not Assigned(_constructor) then
                    _constructor := publ.procs.construct('Create');


                code_constructor :=
                    code_constructor + sLineBreak +
                    Format('%s := %s.Create;', [_var.Name, _var.dataTypeText]);

                if not Assigned(_destructor) then
                    _destructor := publ.procs.destruct('Destroy');

                code_destructor :=
                    Format('%s.Free;', [_var.Name]) + sLineBreak + code_destructor;
            end;
        end;
    end;

    if instantiated then
    begin
        _constructor.code(code_constructor);
        _destructor.code(code_destructor);
    end;

    _members.Free;
end;

{ RbProc }

function RbProc.getSignature: string;
begin
    case FProcType of
        procConstructor: Result := CNST_SIGN;
        procDestructor: Result := DSTR_SIGN;
        procProcedure: Result := PROC_SIGN;
        procFunction: Result := FUNC_SIGN;
        else;
    end;
end;

function RbProc.getCode(genType: RbProcGenType): string;
var
    proc_signature: string;
    class_name: string = '';
    proc_body: string = '';
    _modifiers: string = '';
    _return_type: string = 'variant';
begin
    _modifiers := Fmodifiers;

    // store the modifiers. Don't include in implementations code

    if Length(return_type) > 0 then
        _return_type := return_type;

    proc_signature := getSignature();

    if genType = pgtImplementation then
    begin
        if Assigned(class_) then
            class_name := class_.Name + '.';
        proc_body := sLineBreak + 'begin' + sLineBreak + code +
            sLineBreak + 'end;';
        _modifiers := '';
    end;

    {~ Build the proc/func signature ~}
    if FProcType = procFunction then
    begin
        Result := Format(proc_signature, [class_name, Name, params,
            _return_type, _modifiers]);
    end
    else
    begin
        Result := Format(proc_signature, [class_name, Name, params, _modifiers]);
    end;

    {~ Build the output line. proc_body is empty if gentype is interface ~}

    if Comment <> '' then
        Result := format('{~ %s ~}', [Comment]) //Add comment before proc
            + sLineBreak + Result;
    Result := Result + proc_body;

end;


function RbProc.genCode(_genType: RbProcGenType): string;
begin
    Result := getCode(_genType);
end;

function RbProc.ProcType(_ProcType: RbProcType): RbProc;
begin
    FProcType := _ProcType;
    Result := Self;
end;

function RbProc.ProcType: RbProcType;
begin
    Result := FProcType;
end;

function RbProc.params(_params: string): RbProc;
begin
    Fparams := _params;
    Result := Self;
end;

function RbProc.params: string;
begin
    Result := Fparams;
end;

function RbProc.return_type(_return_type: string): RbProc;
begin
    Freturn_type := _return_type;
    Result := Self;
end;

function RbProc.return_type: string;
begin
    Result := Freturn_type;
end;

function RbProc.code(_code: string): RbProc;
begin
    Fcode := _code;
    Result := Self;
end;

function RbProc.code: string;
begin
    Result := Fcode;

    if (codeDescription <> '') then
        Result := Format('{%s}', [codeDescription]) + sLineBreak + Result;

    Result := Trim(Result);
end;

function RbProc.class_(_class: RbClass): RbProc;
begin
    Fclass_ := _class;
    Result := Self;
end;

function RbProc.class_: RbClass;
begin
    Result := nil;
    if Assigned(Fclass_) then
        Result := Fclass_
    else
    begin
        if Owner is RbClass then
            Result := RbClass(Owner);
    end;
end;

function RbProc.modifiers(_modifiers: string): RbProc;
begin
    Fmodifiers := _modifiers;
    Result := Self;
end;

function RbProc.modifiers: string;
begin
    Result := Fmodifiers;
end;

function RbProc.codeDescription(_codeDescription: string): RbProc;
begin
    FcodeDescription := _codeDescription;
    Result := Self;
end;

function RbProc.codeDescription: string;
begin
    Result := FcodeDescription;
end;

function RbProcList.proc(_name: string): RbProc;
begin
    Result := get(_name);
    Result.ProcType(procProcedure);
end;

function RbProcList.getProcType(_procType: RbProcType): RbProcList;
var
    i: integer;
    p: RbProc;
begin
    Result := RbProcList.Create;
    for i := 0 to Count - 1 do
    begin
        p := getObject(i);
        if p.ProcType = _procType then
        begin
            Result.add(p);
        end;
    end;
end;

function RbProcList.func(_name: string): RbProc;
begin
    Result := get(_name);
    Result.ProcType(procFunction);
end;

function RbProcList.construct(_name: string): RbProc;
begin
    Result := get(_name);
    Result.ProcType(procConstructor);
    Result.code('inherited;');
end;

function RbProcList.destruct(_name: string): RbProc;
begin
    Result := get(_name);
    Result.ProcType(procDestructor);
    Result.modifiers('override;').code('inherited Destroy;');
end;

constructor RbProc.Create;
begin
    inherited Create;
    FProcType := procProcedure;
end;

{ RbUnit }

function RbUnit.genCode(_genType: RbProcGenType): string;
var
    _intf: RbStringList;
    _impl: RbStringList;
begin
    try
        _intf := RbStringList.Create;
        _impl := RbStringList.Create;

        _intf.Add(unit_header);
        _intf.Add(Format('{~ Version %d ~}', [version]));

        // INTERFACE

        _intf.Add('');
        _intf.Add('interface');
        _intf.Add(intf_uses);

        if intf_consts.Count > 0 then
        begin
            _intf.Add(intf_consts.declaration());
            _intf.Add(intf_consts.genCode(pgtInterface));
		end;

        if intf_resource_strings.Count > 0 then
        begin
            _intf.Add(intf_resource_strings.declaration());
            _intf.Add(intf_resource_strings.genCode(pgtInterface));
		end;

        {~ Decide if we need to add type declaration ~}
        if (intf_records.Count > 0) or (intf_types.Count > 0) or
            (Classes.Count > 0) then
            _intf.Add('type');

        {begin records, types and classes}
        if intf_types.Count > 0 then
            _intf.Add(intf_types.genCode(pgtInterface));

        if intf_enum.Count > 0 then
            _intf.Add(intf_enum.genCode(pgtInterface));

        if intf_records.Count > 0 then
            _intf.Add(intf_records.genCode(pgtInterface));

        if Classes.Count > 0 then
            _intf.Add(Classes.genCode(pgtInterface));
        {end}

        {~ Global procs ~}
        if intf_procs.Count > 0 then
            _intf.Add(intf_procs.genCode(pgtInterface));

        {~ Global variables ~}
        if intf_vars.Count > 0 then
        begin
            _intf.Add('var');
            _intf.Add(intf_vars.genCode(pgtInterface));
        end;

        // IMPLEMENTATION
        _impl.Add('');
        _impl.Add('implementation');
        {~ Uses clause ~}
        _impl.Add(impl_uses);
        {~ Variables ~}
        if impl_vars.Count > 0 then
            _impl.Add(impl_vars.genCode(pgtImplementation));

        {~ Types ~}
        if (impl_types.Count > 0) then
        begin
            _impl.Add('type');
            _impl.Add(impl_types.genCode(pgtImplementation));
        end;

        {~ Class function bodies ~}
        _impl.Add(Classes.genCode(pgtImplementation));
        {~ Global procs ~}
        _impl.Add(intf_procs.genCode(pgtImplementation));
        {~ Unit procs ~}
        _impl.Add(impl_procs.genCode(pgtImplementation));

        _impl.Add('initialization');
        _impl.Add(init_code);
        _impl.Add('');
        _impl.Add('finalization');
        _impl.Add(finl_code);
        _impl.Add('end.'); // Last line

        Result := _intf.Text + sLineBreak + _impl.Text;
    finally
        FreeAndNil(_intf);
        FreeAndNil(_impl);
    end;
end;



function RbUnit.intf_code: string;
begin
    Result := Fintf_code;
end;

function RbUnit.intf_procs(_intf_procs: RbProcList): RbUnit;
begin
    Fintf_procs := _intf_procs;
    Result := Self;
end;

function RbUnit.intf_procs: RbProcList;
begin
    Result := Fintf_procs;
end;

function RbUnit.Members: TList;
begin
    Result := FMembers;
end;

constructor RbUnit.Create;
begin
    inherited Create;
    FMembers := TList.Create;

    Fintf_consts := RbConstList.Create;
    Fintf_consts.Owner(Owner);
    Members.Add(Fintf_consts);

    Fintf_resource_strings := RbResourceStringList.Create;
    Fintf_resource_strings.Owner(Owner);
    Members.Add(Fintf_resource_strings);

    Fintf_types := RbTypeList.Create;
    Fintf_types.Owner(Owner);
    Members.Add(Fintf_types);

    Fintf_enum := RbEnumList.Create;
    Fintf_enum.Owner(Owner);
    Members.Add(Fintf_enum);

    Fintf_records := RbRecordList.Create;
    Fintf_records.Owner(Owner);
    Members.Add(Fintf_records);

    Fintf_classes := RbClassList.Create;
    Fintf_classes.Owner(Owner);
    Members.Add(Fintf_classes);

    Fintf_procs := RbProcList.Create;
    Fintf_procs.Owner(Owner);
    Members.Add(Fintf_procs);

    Fintf_vars := RbVarList.Create;
    Fintf_vars.Owner(Owner);
    Members.Add(Fintf_vars);

    Fimpl_consts := RbConstList.Create;
    Fimpl_consts.Owner(Owner);
    Members.Add(fimpl_consts);

    Fimpl_types := RbTypeList.Create;
    Fimpl_types.Owner(Owner);

    Fimpl_records := RbRecordList.Create;
    Fimpl_records.Owner(Owner);
    Members.Add(fImpl_records);

    Fimpl_procs := RbProcList.Create;
    Fimpl_procs.Owner(Owner);
    Members.Add(Fimpl_procs);

    Fimpl_vars := RbVarList.Create;
    Fimpl_vars.Owner(Owner);
    Members.Add(Fimpl_vars);

end;

destructor RbUnit.Destroy;
begin
    FreeAndNil(Fintf_consts);
    FreeAndNil(Fintf_resource_strings);
    FreeAndNil(Fintf_types);
    FreeAndNil(Fintf_enum);
    FreeAndNil(Fintf_records);
    FreeAndNil(Fintf_classes);
    FreeAndNil(Fintf_vars);
    FreeAndNil(Fintf_procs);

    FreeAndNil(Fimpl_consts);
    FreeAndNil(Fimpl_types);
    FreeAndNil(Fimpl_records);
    FreeAndNil(Fimpl_vars);
    FreeAndNil(Fimpl_procs);
    FreeAndNil(FMembers);

    inherited Destroy;
end;

function RbUnit.intf_consts(_intf_consts: RbConstList): RbUnit;
begin
    Fintf_consts := _intf_consts;
    Result := Self;
end;

function RbUnit.intf_consts: RbConstList;
begin
    Result := Fintf_consts;
end;

function RbUnit.intf_resource_strings(_intf_resource_strings: RbResourceStringList
	): RbUnit;
begin
    Fintf_resource_strings:= _intf_resource_strings;
    Result:= self;
end;

function RbUnit.intf_resource_strings: RbResourceStringList;
begin
    Result:= Fintf_resource_strings;
end;

function RbUnit.intf_types(_intf_types: RbTypeList): RbUnit;
begin
    Fintf_types := _intf_types;
    Result := Self;
end;

function RbUnit.intf_types: RbTypeList;
begin
    Result := Fintf_types;
end;

function RbUnit.intf_enum(_intf_enum: RbEnumList): RbUnit;
begin
    Fintf_enum := _intf_enum;
    Result := Self;
end;

function RbUnit.intf_enum: RbEnumList;
begin
    Result := Fintf_enum;
end;

function RbUnit.intf_records(_intf_records: RbRecordList): RbUnit;
begin
    Fintf_records := _intf_records;
    Result := Self;
end;

function RbUnit.intf_records: RbRecordList;
begin
    Result := Fintf_records;
end;

function RbUnit.classes(_intf_classes: RbClassList): RbUnit;
begin
    Fintf_classes := _intf_classes;
    Result := Self;
end;

function RbUnit.classes: RbClassList;
begin
    Result := Fintf_classes;
end;


function RbUnit.intf_vars(_intf_vars: RbVarList): RbUnit;
begin
    Fintf_vars := _intf_vars;
    Result := Self;
end;

function RbUnit.intf_vars: RbVarList;
begin
    Result := Fintf_vars;
end;

function RbUnit.impl_consts(_impl_consts: RbConstList): RbUnit;
begin
    Fimpl_consts := _impl_consts;
    Result := Self;
end;

function RbUnit.impl_consts: RbConstList;
begin
    Result := Fimpl_consts;
end;

function RbUnit.impl_types(_impl_types: RbTypeList): RbUnit;
begin
    Fimpl_types := _impl_types;
    Result := Self;
end;

function RbUnit.impl_types: RbTypeList;
begin
    Result := Fimpl_types;
end;

function RbUnit.impl_records(_impl_records: RbRecordList): RbUnit;
begin
    Fimpl_records := _impl_records;
    Result := Self;
end;

function RbUnit.impl_records: RbRecordList;
begin
    Result := Fimpl_records;
end;

function RbUnit.impl_uses(_impl_uses: string): RbUnit;
begin
    Fimpl_uses := _impl_uses;
    Result := Self;
end;

function RbUnit.impl_uses: string;
begin
    Result := Fimpl_uses;
end;

function RbUnit.impl_procs(_impl_procs: RbProcList): RbUnit;
begin
    Fimpl_procs := _impl_procs;
    Result := Self;
end;

function RbUnit.impl_procs: RbProcList;
begin
    Result := Fimpl_procs;
end;

function RbUnit.impl_vars(_impl_vars: RbVarList): RbUnit;
begin
    Fimpl_vars := _impl_vars;
    Result := Self;
end;

function RbUnit.impl_vars: RbVarList;
begin
    Result := Fimpl_vars;
end;

function RbUnit.impl_code(_impl_code: string): RbUnit;
begin
    Fimpl_code := _impl_code;
    Result := Self;
end;

function RbUnit.impl_code: string;
begin
    if Fimpl_code = '' then
        Result := Format('// %s: impl_code not implemented', [ClassName])
    else
        Result := Fimpl_code;
end;

function RbUnit.init_code(_init_code: string): RbUnit;
begin
    Finit_code := _init_code;
    Result := Self;
end;

function RbUnit.init_code: string;
begin
    Result := '// init_code';
end;

function RbUnit.finl_code(_finl_code: string): RbUnit;
begin
    Ffinl_code := _finl_code;
    Result := Self;
end;

function RbUnit.finl_code: string;
begin
    Result := '// final_code';
end;


function RbUnit.saveUnit(_target_folder: string): string;
var
    code: RbStringList;
begin
    try
        Result := format('%s/%s.pas', [_target_folder, Name]);
        code := RbStringList.Create;
        code.Add(genCode);
        code.SaveToFile(Result);
        // Format the code;
        //WriteLn(Format('Calling JCF: %s %s %s ',[JCF, FLAGS, Result]));
        if codeFormat then
            ExecuteProcess(jcfPath(), JCF_FLAGS + Result);
    finally
        code.Free;
    end;
end;

function RbUnit.intf_uses(_intf_uses: string): RbUnit;
begin
    {concatenate units by default. Clear if you pass empty string}
    if _intf_uses = '' then
        Fintf_uses := ''
    else
    begin
        if Fintf_uses <> '' then
            Fintf_uses := Fintf_uses + ',';
        Fintf_uses := Fintf_uses + _intf_uses;
    end;
    Result := Self;
end;

function RbUnit.intf_uses: string;
begin
    Result := '';
    if Fintf_uses <> '' then
        Result := 'uses' + sLineBreak + Fintf_uses + ';';
end;

function RbUnit.intf_code(_intf_code: string): RbUnit;
begin
    Fintf_code := _intf_code;
    Result := Self;
end;

function RbUnit.unit_header: string;
begin
    Result := format('unit %s;', [Name]) + sLineBreak;
    Result := Result + '{$mode objfpc}{$H+}' + sLineBreak;
    Result := Result + '{$MODESWITCH TYPEHELPERS}' + sLineBreak;
    Result := Result + '{' + Comment + '}';
end;

{ RbPascalProject }

constructor RbPascalProject.Create;
begin
    inherited Create;
    FUnits := RbUnitList.Create;
end;

destructor RbPascalProject.Destroy;
begin
    FreeAndNil(FUnits);
    inherited Destroy;
end;

function RbPascalProject.genCode(_genType: RbProcGenType): string;
begin
    Result := FUnits.genCode(); // Do nothing in project
end;

function RbPascalProject.unit_(_name: string): RbUnit;
begin
    // Create a new Unit and add it to the list.
    // Return the pointer to the object
    Result := FUnits.get(_name);
    Result.Name(_name).Owner(Self);
end;

function RbPascalProject.unit_(_index: integer): RbUnit;
begin
    Result := FUnits.getObject(_index);
end;

function RbPascalProject.unitCount: integer;
begin
    Result := FUnits.Count;
end;

function RbPascalProject.unitList: RbUnitList;
begin
    Result := FUnits;
end;

function RbPascalProject.saveUnits(target_folder: string): integer;
var
    index: integer;
    current_unit: RbUnit;
begin
    if target_folder[target_folder.Length] <> DirectorySeparator then
        target_folder := target_folder + DirectorySeparator;

    {Project Folder}
    target_folder := Format('%s%s', [target_folder, Name]);
    if not DirectoryExists(target_folder) then
        CreateDir(target_folder);

    {Version folder}
    target_folder := Format('%s/%d', [target_folder, version]);
    if not DirectoryExists(target_folder) then
        CreateDir(target_folder);

    for index := 0 to FUnits.Count - 1 do
    begin
        current_unit := FUnits.getObject(index);
        current_unit.saveUnit(target_folder);
    end;
    Result := FUnits.Count;
end;

{ RbCodeGen }

function RbCodeGen.Owner(_Owner: RbCodeGen): RbCodeGen;
begin
    FOwner := _Owner;
    Result := Self;
end;

function RbCodeGen.Owner: RbCodeGen;
begin
    Result := FOwner;
end;

function RbCodeGen.Name(_Name: string): RbCodeGen;
begin
    FName := _Name;
    Result := Self;
end;

function RbCodeGen.Name: string;
begin
    Result := FName;
end;

{~ Append lines to the comment RbStringList ~}
function RbCodeGen.Comment(_comment: string): RbCodeGen;
begin
    FComment.Add(_comment);
    Result := Self;
end;

function RbCodeGen.CommentList: RbStringList;
begin
    Result := FComment;
end;

{~ Remove the last Linebreak from RbStringList.Text ~}
function RbCodeGen.Comment: string;
begin
    Result := Copy(FComment.Text, 0, Length(FComment.Text) - 1);
end;

constructor RbCodeGen.Create;
begin
    inherited Create;
    FComment := RbStringList.Create;
end;

destructor RbCodeGen.Destroy;
begin
    FreeAndNil(FComment);
    inherited Destroy;
end;

constructor RbCodeGen.New(_name: string);
begin
    inherited Create;
    Name(_name);
end;

function RbCodeGen.version: word;
begin
    Result := myVersion;
end;

procedure RbCodeGen.setVersion(_ver: word);
begin
    {Allow this to be set only from the Project}
    if self is RbPascalProject then
        myVersion := _ver;
end;

end.
