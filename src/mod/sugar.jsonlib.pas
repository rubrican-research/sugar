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
	 {Copies a JSON Object. Source -> Destination. Returns True if successful}
	 function copyJSONObject(constref _source, _dest: TJSONObject; _clearFirst: boolean = false): boolean;
	 {Copies a JSON Array Source -> Destination. Returns True if successful}
	 function copyJSONArray(constref _source, _dest: TJSONArray; _clearFirst: boolean = false): boolean;

	 function copyJSONMembers(constref _target, _source: TJSONObject): TJSONObject;
	 function initJSONArray(_num: integer; _val: string): TJSONArray;

	 {Takes a JSON object and initializes it to default values. Particularly useful
	 when you want to re-initialize an object i.e. clear all the data but keep the structure}
	 function clearCloneJSON(constref _obj: TJSONData; _clone: boolean = true): TJSONData;


implementation
uses
     jsonparser, jsonscanner, sugar.utils;

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

function copyJSONObject(constref _source, _dest: TJSONObject; _clearFirst: boolean
	): boolean;
var
    i: integer;
    _name: string;
    _currSource, _currDest: TJSONData;
    _j: TJSONData;
begin
    Result := True;
    //--> log('');
    //--> log('enter copyJSONObject -->');
    //--> log(_source.formatJSON());

    if _clearFirst then
        clearCloneJSON(_dest, false);

    if _source.Count = 0 then
    begin
        {Empty JSON Object}
        //--> log('Source is empty object. Exit');
        exit;
	end;

	for i := 0 to pred(_source.Count) do
    begin
        _name := _source.Names[i];
        _currSource := _source.Items[i];
        //log('copyJSONObject is looking at %s', [_name]);

        if Assigned(_dest.find(_name)) then
            _currDest := _dest.Elements[_name]
        else
        begin
             //--> log(' --> adding ' + _name);
            _dest.Add(_name, _currSource.clone);

            //case _currSource.JSONType of
	           //	jtUnknown: {don't do anything};
            //
	           //	jtNumber: _dest.Add(_name, _currSource.AsString);
            //
	           //	jtString: _dest.Add(_name, _currSource.AsString);
            //
	           //	jtBoolean: _dest.Add(_name, _currSource.AsBoolean);
            //
	           //	jtNull: {don't do anything};
            //
	           //	jtArray:    _dest.Add(_name, TJSONArray(_currSource).Clone);
            //
	           // {recursive call}
	           //	jtObject:   _dest.Add(_name, TJSONObject(_currSource).Clone);
            //end;

            continue;
		end;

        {Check if destination contains the element}
        Result := Assigned(_currDest);
        if not Result then
        begin
            trip('copyJSONObject: _currDest is not assigned: ' + _name);
            //--> log('copyJSONObject: _currDest is not assigned: ' + _name);
            break;
        end;

        {check if source and destination types are same}
        Result := (_currSource.JSONType = _currDest.JSONType);
        if not Result then
        begin

            if (_currSource.JSONType = jtString) and (_currDest.JSONType in [jtArray, jtObject]) then
            begin
                //--> log('     --> source is string, dest is array or object');
                try
                    _j:= GetJSON(_currSource.AsString);
                    if Assigned(_j) then
                    begin
                        if (_j.JSONType = jtArray) and (_currDest.JSONType = jtArray) then
                        begin
                            Result:= copyJSONArray(TJSONArray(_j), TJSONArray(_currDest));
                            if Result then
                            begin
                                //--> log('     --> copied array from string');
                                continue;
                            end
                            else
                                //--> log('     --> copy Array failed()');

						end
                        else if (_j.JSONType = jtObject) then
                        begin
                            Result:= copyJSONObject(TJSONObject(_j), TJSONObject(_j));
                            if Result then
                            begin
                                //--> log('     --> copied object from string');
                                continue;
                            end
                            else
                                //--> log('     --> copy Object failed()');
						end
						else
                        begin
                            Result:= false;
                        end;
					end;

				finally
                    _j.Free;
				end;
			end
            else
                Result:= (_currSource.JSONType = jtString); {If it is a string then it can be converted}

            if not Result then
            begin
	            //--> log('copyJSONObject: Source and destination JSONTypes are not the same for field <' + _name + '>' + sLinebreak + 'source type=' + JSONTypeName(_currSource.JSONType) + ' dest type=' + JSONTypeName(_currDest.JSONType));

	            trip('copyJSONObject: Source and destination JSONTypes are not the same for field <'
	                + _name + '>' + sLinebreak + 'source type=' +
	                JSONTypeName(_currSource.JSONType) + ' dest type=' +
	                JSONTypeName(_currDest.JSONType));

	            break;
			end;
		end;



        begin

	        {Copy the data here}
	        //--> log('copyJSONObject is assigning ' + _name);
	        case _currSource.JSONType of
	            jtUnknown: {don't do anything};

	            jtNumber: _currDest.AsString:= _currSource.AsString;

	            jtString: _currDest.AsString := _currSource.AsString;

	            jtBoolean: _currDest.AsBoolean := _currSource.AsBoolean;

	            jtNull: {don't do anything};

	            jtArray: begin
	                Result := copyJSONArray(TJSONArray(_currSource), TJSONArray(_currDest));
	                if not Result then {either copyJSONArray or copyJSONObject has failed}
	                begin
	                    //--> log('      --> copyJSONArray failed');
	        		end;

				end;

	            {recursive call}
	            jtObject:
	            begin
	                Result := copyJSONObject(TJSONObject(_currSource), TJSONObject(_currDest));
	                if not Result then {either copyJSONArray or copyJSONObject has failed}
	                begin
	                    //--> log('     --> copyJSONObject has failed}');
	                end;
				end;
			end;


	        if not Result then {either copyJSONArray or copyJSONObject has failed}
	        begin
	            //--> log('{either copyJSONArray or copyJSONObject has failed}');
	            break;
			end;

	        {All is well here}
	        Result := True;

		end;
	end;
end;

{Only copy values if both arrays have same number of items}
function copyJSONArray(constref _source, _dest: TJSONArray; _clearFirst: boolean
	): boolean;
var
    i: integer;
    _currSource, _currDest: TJSONData;
begin
    Result := True;
    //--> log('enter copyJSONArray --->');
    if assigned(_source) and assigned(_dest) and (_source.JSONType = jtArray) and (_dest.JSONType = jtArray) then
    begin
        //{Check if we ought to just add the source Data to the destination}
        //if _dest.Count = 0 then
        //begin
        //    _dest.Add(_source.Clone);
        //    Result:= True;
        //    exit;
        //{================  EXIT =================}
        //end;

        if _clearFirst then _dest.Clear;
        for i := 0 to pred(_source.Count) do
        begin
            //--> log('copying %d of %d', [succ(i), _source.Count ]);
            _currSource := _source.Items[i];


            {if source has more then add to the array}
            if i >= _dest.Count then
            begin
                //--> log('adding %s', [_currSource.FormatJSON()]);
                _dest.Add(_currSource.clone);
                continue;
			end;

            {check if source and destination types are same}
            _currDest := _dest.Items[i];
            Result := (_currSource.JSONType = _currDest.JSONType);

            if not Result then
            begin
                //--> log('copyJSONArray: Source and destination JSONTypes are not the same ' + sLinebreak + 'source type=' + JSONTypeName(_currSource.JSONType) + ' dest type=' + JSONTypeName(_currDest.JSONType));
                break;
                {================  EXIT HERE =================}
            end;

            case _currSource.JSONType of
                jtUnknown: {don't do anything};

                jtNumber: _currDest.AsInteger := _currSource.AsInteger;

                jtString: _currDest.AsString := _currSource.AsString;

                jtBoolean: _currDest.AsBoolean := _currSource.AsBoolean;

                jtNull: {don't do anything};

                {recursive call}
                jtArray:
                begin
                    //--> log('   --> Recursive call to copyJSONArray() ->');
                    Result:= copyJSONArray(TJSONArray(_currSource), TJSONArray(_currDest));
				end;

                jtObject:
                begin
                    //--> log('   --> calling copyJSONObject with %s', [_currSource.FormatJSON()]);
                    Result:= copyJSONObject(TJSONObject(_currSource), TJSONObject(_currDest));
				end;
			end;
        end;
    end;
end;

function copyJSONMembers(constref _target, _source: TJSONObject): TJSONObject;
var
    member: TJSONEnum;
    jsource, jdest: TJSONData;
begin
    for member in _target do
    begin
        jsource := _source.find(member.key);
        jdest   := _target.Find(member.key);
        if assigned(jsource) then
        begin
	        if jsource.JSONType = jtArray then
	            copyJSONArray(TJSONArray(jSource), TJSONArray(jdest))
	        else if jsource.JSONType = jtObject then
	            copyJSONObject(TJSONObject(jSource), TJSONObject(jdest))
	        else
	            jdest.AsString:= jsource.AsString;
		end;
	end;
    Result:= _target;
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

end.

