unit sugar.jsonlib;

{$mode objfpc}{$H+}

interface

uses
	 Classes, SysUtils, fpjson;
type
  { TJSONUnquotedString }

      TJSONUnquotedString = class (TJSONString)
      protected
          function GetAsJSON: TJSONStringType; override;
  	end;

  	{ TJSONFormatedFloat }

      TJSONFormatedFloat = class (TJSONFloatNumber)
      public const DefaultFormat = '0.0###';
      private
          function getFormatter: string;

      public
          class var formatter: string;
          function GetAsString: TJSONStringType; override;
          procedure SetAsString(const AValue: TJSONStringType); override;
          constructor Create(AValue: TJSONFloat); reintroduce;

  	end;


  	{ TDynaJSONObject }
      {Syntax sugar for TJSONObject that allows you to
      create JSON object with nested values using simpler syntax}
      TDynaJSONObject = class(TJSONObject)

          {adds a new object with this name if not found}
          function addObjectMember(_objectName: string):TDynaJSONObject;

          {adds a new array with this name if not found}
          function addArrayMember(_arrayName: string):TJSONArray;

          {adds a new object to an array with this name. If array is not found, it will be added}
          function addObjToArray(_name: string):TDynaJSONObject;
          function addJSCode(_name: string; _value: string): TDynaJSONObject;
          constructor CreateJS;

          {*** aliases using shorter names ***}
          {adds a new object if not found}
          function obj(_objectName: string):TDynaJSONObject;
          {adds a new array if not found}
          function arr(_arrayName: string):TJSONArray;
          {adds a new object to the array}
          function objArr(_name: string):TDynaJSONObject;

          {adds a value that won't contain quotes. This is good to generate JSON string that that is
          valid JS Code for methods}
          function addJS(_name: string; _value: string):TDynaJSONObject;
          function putJS(_name: string; _value: string):TDynaJSONObject;
          function Add(const AName: TJSONStringType; AValue: TJSONFloat): Integer; overload;

          function Clone: TDynaJSONObject; reintroduce;
  	end;

	 {JSON Helper functions}
	 {Returns a delimited string of keys contained in a JSONObject}
	 function getDelimitedKeys(const _json: TJSONObject; _delim: string = ','): string;
	 {parses a string and outputs JSON}
	 function parseJSON(const _json: string): TJSONData;
	 {Loads a file, parses it and returns JSONData}
	 function loadJSON(const _file: string): TJSONData;

     function GetJSONStr(constref _j: TJSONData; const _free: boolean = true): string; // Frees the object

     procedure CopyJSONValue(constref _source, _dest: TJSONData; var Success: boolean);
     function copyJSONMembers(constref _target, _source: TJSONObject): TJSONObject;
     {Copies a JSON Object. Source -> Destination. Returns True if successful}
	 function copyJSONObject(constref _source, _dest: TJSONObject; _clearFirst: boolean = false): boolean;
	 {Copies a JSON Array Source -> Destination. Returns True if successful}
	 function copyJSONArray(constref _source, _dest: TJSONArray; _clearFirst: boolean = false): boolean;

	 function initJSONArray(_num: integer; _val: string): TJSONArray;

	 {Takes a JSON object and initializes it to default values. Particularly useful
	 when you want to re-initialize an object i.e. clear all the data but keep the structure}
	 function clearCloneJSON(constref _obj: TJSONData; _clone: boolean = true): TJSONData;

     function makeMemberName: string;
     function newJSONRandomObj(_addObj: boolean = true): TJSONObject;
     function newJSONRandomArray(_addObj: boolean = true): TJSONArray;



implementation
uses
     jsonparser, jsonscanner, sugar.utils, Math;

{ TJSONUnquotedString }

function TJSONUnquotedString.GetAsJSON: TJSONStringType;
begin
	Result:=GetAsString; // no escaping. Render text as is for code
end;

{ TJSONFormatedFloat }

function TJSONFormatedFloat.getFormatter: string;
var
    _recompute: boolean;
begin
    if (formatter.isEmpty) then
        formatter:= DefaultFormat;

    Result:= formatter
end;

function TJSONFormatedFloat.GetAsString: TJSONStringType;
var
    _settings: TFormatSettings;
begin
    _settings := DefaultFormatSettings;
    _settings.DecimalSeparator:= '.';
    Result:= FormatFloat(getFormatter(), GetAsFloat, _settings);
end;

procedure TJSONFormatedFloat.SetAsString(const AValue: TJSONStringType);
begin
	inherited SetAsString(AValue);
end;

constructor TJSONFormatedFloat.Create(AValue: TJSONFloat);
begin
    inherited Create(AValue);
end;

{ TDynaJSONObject }

function TDynaJSONObject.addObjectMember(_objectName: string): TDynaJSONObject;
begin
	if not Assigned(Find(_objectName)) then
	begin
	    Result:= TDynaJSONObject.Create;
	    add(_objectName, Result);
	end
	else
	    Result:= Objects[_objectName] as TDynaJSONObject;
end;

function TDynaJSONObject.addArrayMember(_arrayName: string): TJSONArray;
begin
    if not Assigned(Find(_arrayName)) then
    begin
        Result:= TJSONArray.Create;
        add(_arrayName, Result);
	end
    else
        Result:= Arrays[_arrayName];
end;

function TDynaJSONObject.addObjToArray(_name: string): TDynaJSONObject;
begin
    Result:= TDynaJSONObject.Create;
    addArrayMember(_name).Add(Result);
end;

function TDynaJSONObject.addJSCode(_name: string; _value: string
	): TDynaJSONObject;
begin
    Result:= Self;
    add(_name, TJSONUnquotedString.Create(_value));
end;

constructor TDynaJSONObject.CreateJS;
begin
    Create;
    UnquotedMemberNames:= true;
end;

function TDynaJSONObject.obj(_objectName: string): TDynaJSONObject;
begin
    Result:= addObjectMember(_objectName);
end;

function TDynaJSONObject.arr(_arrayName: string): TJSONArray;
begin
    Result:= addArrayMember(_arrayName);
end;

function TDynaJSONObject.objArr(_name: string): TDynaJSONObject;
begin
    Result:= addObjToArray(_name);
end;

function TDynaJSONObject.addJS(_name: string; _value: string): TDynaJSONObject;
begin
    Result:= Self;
    Result.add(_name, TJSONUnquotedString.Create(_value));
end;

function TDynaJSONObject.putJS(_name: string; _value: string): TDynaJSONObject;
begin
    Result:= self;
    Result.Elements[_name]:= TJSONUnquotedString.Create(_value);
end;

function TDynaJSONObject.Add(const AName: TJSONStringType; AValue: TJSONFloat
	): Integer;
begin
    Result:= Add(AName, TJSONFormatedFloat.Create(AValue));
end;

function TDynaJSONObject.Clone: TDynaJSONObject;
begin
    Result:= inherited clone as TDynaJSONObject;
end;

{Returns a delimited string of keys contained in a JSONObject}
function getDelimitedKeys(const _json: TJSONObject; _delim: string): string;
var
    el: TJSONEnum;
    addComma: boolean = False;
begin
    Result := '';
    for el in _json do
    begin
        if addComma then
            Result := Result + _delim
        else
            addComma := True;
        Result := Result + el.key;
    end;
end;

function parseJSON(const _json: string): TJSONData;
begin
	with TJSONParser.Create(_json, [joUTF8]) do
    begin
        try
            try
                Result := Parse;
			except
                Result:= nil;
			end;
		finally
           Free;
		end;
    end;

end;

function loadJSON(const _file: string): TJSONData;
begin
    Result := nil;
    with TJSONParser.Create(getFileContent(ExpandFileName(_file)), [joUTF8]) do
    begin
        try
            Result := Parse;
		finally
           Free;
		end;
    end;
end;

function GetJSONStr(constref _j: TJSONData; const _free: boolean): string;
begin
    Result := _j.FormatJSON();
    if _free then _j.Free;
end;

procedure CopyJSONValue(constref _source, _dest: TJSONData; var Success: boolean);
begin
    Success := True;

    case _source.JSONType of
        jtUnknown:
            ; // Don't do anything

        jtNumber:
        begin
            if _source is TJSONFloatNumber then
                _dest.AsFloat := _source.AsFloat
            else
                _dest.AsInteger := _source.AsInteger;
        end;

        jtString:
            _dest.AsString := _source.AsString;

        jtBoolean:
            _dest.AsBoolean := _source.AsBoolean;

        jtNull:
            ; // Don't do anything

        jtArray:
        begin
            Success := copyJSONArray(TJSONArray(_source), TJSONArray(_dest));
        end;

        jtObject:
        begin
            Success := copyJSONObject(TJSONObject(_source), TJSONObject(_dest));
        end;
    end;
end;

function copyJSONMembers(constref _target, _source: TJSONObject): TJSONObject;
var
    member: TJSONEnum;
    jsource, jdest: TJSONData;
    copySuccess: boolean;
begin
    Result := _target;

    if not (Assigned(_target) and Assigned(_source)) then
        Exit;

    for member in _target do
    begin
        jsource := _source.Find(member.key);
        jdest := _target.Find(member.key);

        if Assigned(jsource) and Assigned(jdest) then
        begin
            if jsource.JSONType = jdest.JSONType then
            begin
                CopyJSONValue(jsource, jdest, copySuccess);
                // We continue even if one member fails, but could log this failure
            end
            else if (jsource.JSONType in [jtString, jtNumber, jtBoolean]) and
                (jdest.JSONType in [jtString, jtNumber, jtBoolean]) then
            begin
                // Handle compatible primitive types
                try
                    jdest.AsString := jsource.AsString;
                except
                    // Handle conversion error, but continue with other members
                end;
            end;
        end;
    end;
end;


function copyJSONObject(constref _source, _dest: TJSONObject; _clearFirst: boolean = False): boolean;
var
    i: integer;
    _name: string;
    _currSource, _currDest: TJSONData;
    _j: TJSONData;
begin
    Result := True;

    if not Assigned(_source) or not Assigned(_dest) then
    begin
        Result := False;
        Exit;
    end;

    if _clearFirst then
        clearCloneJSON(_dest, False);

    if _source.Count = 0 then
    begin
        // Empty JSON Object, nothing to copy
        Exit;
    end;

    for i := 0 to pred(_source.Count) do
    begin
        _name := _source.Names[i];
        _currSource := _source.Items[i];

        // If the destination doesn't have this element, add it
        if not Assigned(_dest.Find(_name)) then
        begin
            _dest.Add(_name, _currSource.Clone);
            Continue;
        end;

        // Get the existing destination element
        _currDest := _dest.Elements[_name];
        if not Assigned(_currDest) then
        begin
            Result := False;
            Exit;
        end;

        // Handle type compatibility
        if (_currSource.JSONType = _currDest.JSONType) then
        begin
            // Same types, direct copy
            CopyJSONValue(_currSource, _currDest, Result);
            if not Result then
                Exit;
        end
        else if (_currSource.JSONType = jtString) and
            (_currDest.JSONType in [jtArray, jtObject]) then
        begin
            // Try to parse string as JSON
            _j := nil;
            try
                try
                    _j := GetJSON(_currSource.AsString);
                    if not Assigned(_j) then
                    begin
                        Result := False;
                        Exit;
                    end;

                    // Handle parsed JSON type
                    if (_j.JSONType = jtArray) and (_currDest.JSONType = jtArray) then
                    begin
                        Result := copyJSONArray(TJSONArray(_j), TJSONArray(_currDest));
                        if not Result then
                            Exit;
                    end
                    else if (_j.JSONType = jtObject) and (_currDest.JSONType = jtObject) then
                    begin
                        // Fixed: Copy from parsed object to destination (not to itself)
                        Result := copyJSONObject(TJSONObject(_j), TJSONObject(_currDest));
                        if not Result then
                            Exit;
                    end
                    else
                    begin
                        Result := False;
                        Exit;
                    end;
                except
                    Result := False;
                    Exit;
                end;
            finally
                FreeAndNil(_j);
            end;
        end
        else
        begin
            // Incompatible types
            Result := False;
            Exit;
        end;
    end;
end;

{Only copy values if both arrays have same number of items}
function copyJSONArray(constref _source, _dest: TJSONArray;
    _clearFirst: boolean = False): boolean;
var
    i: integer;
    _currSource, _currDest: TJSONData;
begin
    Result := False;

    if not (Assigned(_source) and Assigned(_dest)) then
        Exit;

    if (_source.JSONType <> jtArray) or (_dest.JSONType <> jtArray) then
        Exit;

    Result := True;

    if _clearFirst then
        _dest.Clear;

    for i := 0 to pred(_source.Count) do
    begin
        _currSource := _source.Items[i];

        // If source has more items than destination, add new items
        if i >= _dest.Count then
        begin
            _dest.Add(_currSource.Clone);
            Continue;
        end;

        // Get current destination item
        _currDest := _dest.Items[i];

        // Check type compatibility
        if (_currSource.JSONType <> _currDest.JSONType) then
        begin
            Result := False;
            Exit;
        end;

        // Copy values based on type
        CopyJSONValue(_currSource, _currDest, Result);
        if not Result then
            Exit;
    end;
end;

function initJSONArray(_num: integer; _val: string): TJSONArray;
var
	i: Integer;
begin
    Result:= TJSONArray.Create;
    for i := 0 to pred(_num) do
        Result.add(_val);
end;

function clearCloneJSON(constref _obj: TJSONData; _clone: boolean): TJSONData;
var
    json: TJSONEnum;
begin
    if _clone then
        Result:= _obj.Clone
    else
        Result:= _obj;

    case Result.JSONType of
        jtUnknown: ;
        jtNumber: Result.asFloat:= 0;
        jtString: Result.AsString:= '';
        jtBoolean:Result.AsBoolean:= true;
        jtNull: ;
        jtArray: Result.Clear;
        jtObject:
        begin
            for json in Result do
                clearCloneJSON(TJSONObject(Result).Items[json.KeyNum], false); {Clear but don't clone}
		end;
    end;
end;


function newJSONRandomArray(_addObj: boolean = true): TJSONArray;
var
    _numMembers: integer;
    _type, i: integer;
begin
    Result := TJSONArray.Create;
    _numMembers := Random(20);
    for i := 0 to pred(_numMembers) do begin
        _type := random(5);
        case _type of
            0:  {Integer}Result.add(Random(1000));
            1:  {Float}  Result.add(Random(1000) + Random());
            2:  {String} Result.add(genRandomKey(Max(12, Random(120))));
            3:  {object} if _addObj then Result.add(newJSONRandomObj(false));
            4:  {array}  if _addObj then Result.add(newJSONRandomArray(false));
		end;
	end;
end;

function makeMemberName: string;
begin
    Result := '';
    while Result.isEmpty do begin
        Result := genRandomKey(Max(4, Random(12)))
	end;
end;

function newJSONRandomObj(_addObj: boolean = true): TJSONObject;
var
    _numMembers: integer;
    _type, i: integer;
begin
    Result := TJSONObject.Create;
    _numMembers := Random(20);
    for i := 0 to pred(_numMembers) do begin
        _type := random(5);
        case _type of
            0:  {Integer}Result.add(makeMemberName, Random(1000));
            1:  {Float}  Result.add(makeMemberName, Random(1000) + Random());
            2:  {String} Result.add(makeMemberName, genRandomKey(Max(12, Random(120))));
            3:  {object} if _addObj then Result.add(makeMemberName, newJSONRandomObj(false));
            4:  {array}  if _addObj then Result.add(makeMemberName, newJSONRandomArray(false));
		end;
	end;
end;

end.

