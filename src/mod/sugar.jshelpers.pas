unit sugar.jshelpers;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

{List of Javascript helpers}

interface

uses
    Classes, SysUtils, fpjson, sugar.htmlbuilder, sugar.httphelper, sugar.collections;
type
    JSPrimitiveDataType = (jstypeUndefined, jstypeBoolean, jstypeNumber, jstypeString, jstypeBigInt, jstypeFloat, jstypeDate, jstypeTime, jstypeDateTime, jstypeTimeOfDay, jstypeArray, jstypeObject, jstypeSymbol);

	{ JSPrimitiveDataTypesHelper }

    JSPrimitiveDataTypesHelper = type helper for JSPrimitiveDataType
        function asString: string;
        function fromString(_typeName: string) : JSPrimitiveDataType;
        function defineVariable(_varName: string): string;
	end;

    JSFuncType = (jsfuncNamed, jsFuncObject, jsFuncLambda, jsFuncArrowSimple, jsFuncArrow);


{ TJSFunction }

	TJSFunction = class
		name: string;
		params: string;
		body: string;
		funcType: JSFuncType;
		function code: string;
		constructor Create;
	end;

{REQUEST METHODS}
    TRequestMethod = (reqGET, reqPOST, reqPUT, reqPATCH, reqDELETE, reqOPTIONS, reqHEAD);
const
    RequestMethodStrings: array[TRequestMethod] of string = ('GET', 'POST', 'PUT', 'PATCH', 'DELETE', 'OPTIONS', 'HEAD');

{REQUEST MODE}
type
    TRequestMode = (reqCors, reqNoCors, reqSameOrigin);
const
    RequestModeStrings : array[TRequestMode] of string = ('cors', 'no-cors', 'same-origin');

type
    TRequestCredentials = (credOmit, credSameOrigin, credInclude);
const
    RequestCreditalsStrings: array[TRequestCredentials] of string  = ('omit', 'same-origin', 'include');

type
    TRequestRedirect = (redirectFollow, redirectError, redirectManual);
const
    RequestRedirectStrings : array[TRequestRedirect] of string = ('follow', 'error', 'manual');

{FETCH API - Implements the fetch()}
type

	{ TJSPromise }

    TJSPromise = class
        name: string; {of the promise variable}
        init: string;
        thenDo: TJSFunction;
        thenDo2: TJSFunction;
        catch: TJSFunction;
        function code: string;
    end;

	{ TJSFetch }

    TJSFetch = class
        url: string;
        method: string; {GET, POST, PUT, PATCH, DELETE, OPTIONS, HEAD}
        headers: TStringMap;
        body: TJSONObject;
        mode: TRequestMode;
        credentials: TRequestCredentials;
        cache: string;
        redirect: TRequestRedirect;
        useAbortController: boolean;
        promise: TJSPromise;
        function code: string;
    end;

{generates a for loop in JS}
function forloop(_init: string; _condition: string; _increment: string; _code: string): string; overload;
function forloop(_count: integer; _code: string): string; overload;

{generates a JS representation of an object }
function initObj(_name: string; _data: TJSONObject): string;

{generates a named JS function}
function jsFunc(_name: string; _params: string; _body: string; _type: JSFuncType = jsFuncObject): string; overload;
function jsFunc(_params: string; _body: string; _immediate: boolean = false): string; overload;

{generates a JS Variable}
function jsVar(_name: string; _value: string): string;

{Generates code to call a js function}
function jsCall(_fnName: string; _params: string): string;

{Generates a lambda:: function () {}}
function jsLambda(_params: string; _body: string): string;

{Generates a simple arrow function:: x => code;}
function jsLambdaSimple(_params: string; _body: string): string;

{Generates an if statement}
function jsIf(_condition: string; _true: string; _else: string = ''): string;

{Generates a variable declaration}
function varDef(_name: string; _value: string): string;

{generates JS code for concat strings. use comma separated values}
function jsConcat(_str: string; _joinWith: string): string;
function jsConcat(_str_arr: array of string; _joinWith: string): string;

implementation
uses
    sugar.utils;

function forloop(_init: string; _condition: string; _increment: string;
	_code: string): string;
begin
    Result:= Format('for(%s; %s; %s){%s}', [_init, _condition, _increment, _code]);
end;

function forloop(_count: integer; _code: string): string;
var
  _name: string;
begin
    _name:= genRandomKey(3);
    Result:= forloop(
    Format('let i%s=0', [_name]),
    Format('i%s<%d', [_name, _count]),
    Format('i%s++', [_name]),
    _code
    );
end;

function initObj(_name: string; _data: TJSONObject): string;
begin
    Result:= Format('%s: %s',[_name, _data.AsJSON]);
end;

function jsFunc(_name: string; _params: string; _body: string; _type: JSFuncType
	): string;
begin
    with TJSFunction.Create do
    begin
        name:= _name;
        params:= _params;
        body:= _body;
        funcType:= _type;
        Result:= code;
        Free;
    end;
end;

function jsFunc(_params: string; _body: string; _immediate: boolean): string;
begin
    Result:= jsFunc('', _params, _body, jsFuncLambda );
    if _immediate then
        Result:= Result + '()';
end;

function jsVar(_name: string; _value: string): string;
begin
    Result:= Format('var %s = %s;', [_name, _value]);
end;

function jsCall(_fnName: string; _params: string): string;
begin
    Result:= Format('%s(%s)', [_fnName, _params]);
end;

function jsLambda(_params: string; _body: string): string;
begin
    with TJSFunction.Create do
    begin
        params  := _params;
        body    := _body;
        funcType:= jsFuncArrow;
        Result  := code;
        Free;
    end;
end;

function jsLambdaSimple(_params: string; _body: string): string;
begin
    with TJSFunction.Create do
    begin
        params  := _params;
        body    := _body;
        funcType:= jsFuncArrowSimple;
        Result  := code;
        Free;
    end;
end;

function jsIf(_condition: string; _true: string; _else: string): string;
begin
    Result:= format('if (%s){_%s}',[_condition, _true]);
    if not _else.IsEmpty then
        Result:= Result + Format('else {%s}',[_else]);
end;

function varDef(_name: string; _value: string): string;
begin
    Result:= Format('var %s = %s;', [_name, _value]);
end;

function jsConcat(_str: string; _joinWith: string): string;
const
    F = '[%s].join(''%s'')';
begin
    Result:= Format(F, [_str, _joinWith]);
end;

function jsConcat(_str_arr: array of string; _joinWith: string): string;
var
  sa: TStringArray;
begin
    copyStringArray(_str_arr, sa);
    Result:= jsConcat(getCommaSeparatedString(sa), _joinWith);
end;

{ JSPrimitiveDataTypesHelper }

function JSPrimitiveDataTypesHelper.asString: string;
begin
    case self of
        jstypeUndefined: Result:= 'undefined';
        jstypeBoolean: Result:= 'boolean';
        jstypeNumber: Result:= 'number';
        jstypeString: Result:= 'string';
        jstypeBigInt: Result:= 'bigint';
        jstypeDate: Result:= 'date';
        jstypeTime: Result:= 'date';
        jstypeDateTime: Result:= 'date';
        jstypeTimeOfDay: Result:= 'timeofday';
        jstypeArray: Result:= '[]';
        jstypeObject: Result:= '{}';
        jstypeSymbol: Result:= 'symbol';
        else trip('JSPrimitiveDataTypesHelper.asString Not implemented');
    end;
end;

function JSPrimitiveDataTypesHelper.fromString(_typeName: string
	): JSPrimitiveDataType;
begin
    case LowerCase(_typeName) of
        'undefined': Self:= jstypeUndefined;
        'boolean': Self:= jstypeBoolean;
        'bigint': Self:= jstypeBigInt;
        'number': Self:= jstypeNumber;
        'string': Self:= jstypeString;
        'Date': Self:= jstypeDate;
        '[]': Self:= jstypeArray;
        '{}': Self:= jstypeObject;
        'symbol': Self:= jstypeSymbol
        else trip('JSPrimitiveDataTypesHelper.fromString Not implemented');

	end;
    Result:= Self;
end;

function JSPrimitiveDataTypesHelper.defineVariable(_varName: string): string;
begin
    trip ('not implemented');
end;

{ TJSFetch }

function TJSFetch.code: string;
begin
    Result:='TJSFetch.code is not implemented';
end;

{ TJSPromise }

function TJSPromise.code: string;
begin
    Result:= Format('const %s = %s;', [name, init]);
    Result:= Format('%s.then(%s)',[name, thenDo.code]);
    if assigned(thenDo2) then
        Result:=Result + Format('.then(%s)',[thenDo2.code]);
    if assigned(catch) then
        Result:=Result + Format('.catch(%s)',[catch.code]);
    Result:= Result + ';';
end;


{ TJSFunction }

function TJSFunction.code: string;
begin
    case funcType of
        jsfuncNamed:
          Result:= Format('function %s(%s){%s}', [name, params, body]);

        jsFuncObject:
          Result:= Format('%s: function(%s){%s}',[name, params, body]);

        jsFuncLambda:
          Result:= Format('function (%s){%s}', [params, body]);

        jsFuncArrowSimple:
          Result:= Format('%s=>%s', [params, body]);

        jsFuncArrow:
          Result:= Format('(%s) => {%s}', [params, body]);
    end;
end;

constructor TJSFunction.Create;
begin
    inherited;
    name:= '';
    params:= '';
    body:='';
    funcType:= jsFuncObject;
end;

end.

