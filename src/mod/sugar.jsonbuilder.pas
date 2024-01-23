unit sugar.jsonbuilder;
{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, sugar.collections;
const
  DEFAULT_INDENT = '   ';

type

    RbJSONObject = class;
    RbJSONObjectArray = class;

    RbJSONData = class(TStringList)
    public
        function str(_name: string; _value: string): RbJSONData;
        function arr(_name: string; _value: array of string): RbJSONData;
        function number(_name: string; _value: integer): RbJSONData;
        function decimal(_name: string; _value: double; _prec: integer = 2): RbJSONData;
        function bool(_name: string; _value: boolean): RbJSONData;

        {syntax sugar for adding values}
        function val(_name: string): string; overload;
        function val(_name: string; _value: string): RbJSONData; overload;
        function val(_name: string; _value: integer): RbJSONData; overload;
        function val(_name: string; _value: double; _prec: integer = 2): RbJSONData; overload;
        function val(_name: string; _value: boolean): RbJSONData; overload;

        {add a named method}
        function method(_name: string; _code: string): RbJSONData; overload;
        function method(_name: string): RbJSONData; overload; {loads from a file named as _name.js}
        function methodFrom(_name: string; _filename: string): RbJSONData;

        {add another json object}
        function json(_name: string; _json: RBJSONObject): RbJSONData; overload;
        function json(_json: RBJSONObject): RbJSONData; overload;

        {add arrays}
        function jarray(_name: string; _json: RbJSONObjectArray): RbJSONData;overload;
        function jarray(_json: RbJSONObjectArray): RbJSONData;overload;

        {get the json code}
        function code(const _indent: string = ''): string;
        constructor Create;
		end;

    RbJSONObject = class(RbJSONData)
    private
        myName : string;
    public
        function Name(_Name : string) : RbJSONObject; overload;
        function Name : string; overload;
        function code(const _named: boolean = true; {includes object name}
                      const _indent: string = DEFAULT_INDENT): string;
		end;

    RbJSONObjectArrayBase = specialize GenericHashObjectList<RbJSONObject>;

    { RbJSONObjectArray }

    RbJSONObjectArray = class(RbJSONObjectArrayBase)
        name: string;
        function code(const _named: boolean = true; {includes object name}
                      const _indent: string = DEFAULT_INDENT): string;

		end;

implementation
uses
    sugar.utils, sugar.textfiler;

const
  JSON_STRING = '%s: %s';
  COMMA = ',';

{ RbJSONObjectArray }

function RbJSONObjectArray.code(const _named: boolean; const _indent: string
		): string;
var
  i : integer;
begin
     Result := _indent + '[' + sLineBreak;
     for i := 0 to count - 1 do
     begin
          Result := Result + _indent + DEFAULT_INDENT + '{' + sLineBreak;
          Result := Result + Items[i].code(_named, _indent+ DEFAULT_INDENT + DEFAULT_INDENT);
          Result := Result +  sLineBreak;
          Result := Result +_indent + DEFAULT_INDENT + '}';
          if i < count - 1 then
          Result := Result + COMMA + sLineBreak;
		 end;
		 Result := Result + sLineBreak + _indent + ']';
end;


{ RbJSONData }

function RbJSONData.code(const _indent: string): string;
var
  i: integer;
begin
    Result := '';
    for i := 0 to Count - 1 do
    begin
         Result := Result
                   + _indent
                   + Format(JSON_STRING, [Names[i], ValueFromIndex[i]]);

         if i < Count-1 then
            Result := Result + Comma;

         Result := Result + sLineBreak;
 	 end;
end;

function RbJSONData.str(_name: string; _value: string): RbJSONData;
begin
     Values[_name]:= '"' + _value + '"';
     Result := self;
end;

function RbJSONData.arr(_name: string; _value: array of string): RbJSONData;
var
  _str: string = '';
  _arr: string = '';
begin
    for _str in _value do
    begin
         if not _arr.isEmpty then
            _arr := _arr + ',';
         _arr := _arr + '{ value: "' + _str + '"}';
		end;
		Values[_name]:= '[' + _arr + ']';
    Result := self;
end;

function RbJSONData.number(_name: string; _value: integer): RbJSONData;
begin
    Values[_name]:= _value.ToString;
    Result := self;
end;

function RbJSONData.decimal(_name: string; _value: double; _prec: integer): RbJSONData;
var
  f: string = '';
  i : integer;
begin
     if _prec>0 then
     begin
        for i := 1 to _prec do
            f := f + '#';

        f := '.' +  f;
		 end;
		 f:= '#' + f;
     Values[_name]:= FormatFloat(f, _value);
     Result := Self;
end;

function RbJSONData.bool(_name: string; _value: boolean): RbJSONData;
begin
     case _value of
       True:  Values[_name]:= 'true';
       False: Values[_name]:= 'false';
     end;
     Result:= self;
end;

function RbJSONData.val(_name: string): string;
begin
     Result := Values[_name];
end;

function RbJSONData.val(_name: string; _value: string): RbJSONData;
begin
     Result := str(_name, _value);
end;

function RbJSONData.val(_name: string; _value: integer): RbJSONData;
begin
     Result := number(_name, _value);
end;

function RbJSONData.val(_name: string; _value: double; _prec: integer): RbJSONData;
begin
     Result := decimal(_name, _value, _prec);
end;

function RbJSONData.val(_name: string; _value: boolean): RbJSONData;
begin
     Result := bool(_name, _value);
end;

function RbJSONData.method(_name: string; _code: string): RbJSONData;
begin
    Values[_name]:= _code;
    Result := self;
end;

function RbJSONData.method(_name: string): RbJSONData;
begin
     Result := methodFrom(_name, _name+'.js');
end;

function RbJSONData.methodFrom(_name: string; _filename: string): RbJSONData;
begin
     with TTextFiler.Create do
     begin
          rootDir:=appendPath([GetCurrentDir, 'assets']);
          fileName:= _fileName;
          autorefresh:=True;
          touch;
          if content.IsEmpty then
             content := 'function () {}';
          Result:= method(_name, content);
          Free;
		 end;
end;

function RbJSONData.json(_name: string; _json: RBJSONObject): RbJSONData;
begin
     Result := Self;
     Values[_name]:= _json.code({named}false);
end;

function RbJSONData.json(_json: RBJSONObject): RbJSONData;
begin
    Result := json(_json.Name, _json);
end;

function RbJSONData.jarray(_name: string; _json: RbJSONObjectArray): RbJSONData;
begin
    Result := Self;
    Values[_name]:= _json.code;
end;

function RbJSONData.jarray(_json: RbJSONObjectArray): RbJSONData;
begin
     Result := jarray(_json.Name, _json);
end;

constructor RbJSONData.Create;
begin
     inherited;
     Delimiter:='~';
     NameValueSeparator:='|';
end;

{ RbJSONObject }

function RbJSONObject.Name(_Name: string): RbJSONObject;
begin
myName := _Name;
Result := Self;
end;

function RbJSONObject.Name: string;
begin
Result := myName;
end;

function RbJSONObject.code(const _named: boolean; const _indent: string
		): string;
var
  i : integer;
begin
     Result := _indent;
     if _named then
        Result := Result + Name + ': ';
     Result := Result + '{' + sLineBreak;

     Result := Result + inherited code (_indent + DEFAULT_INDENT);
     Result := Result + _indent + '}';
end;

end.

