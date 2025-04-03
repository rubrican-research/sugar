unit sugar.templater;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, strutils, sugar.collections
    ;

type

    { TRbTemplater }

    {This is a Moustache template replacer.
     The Parse function breaks the document into a list of TStringObject Pointers
     Document strands are parts of the document and are stored as TStringObjects in myDocStrands.
     Fields are TStringObject pointers that are held by myFields
     Finally, myDoc contains a list DocStrands and Fields. This way, field values are changed in on object (myFields)
     But they will reflect in all locations in myDoc where that particular object is referenced.

    The Text function just appends the contents of each item in myDocs.

    The good thing is that if there are fields that are not referenced by the document, it won't show up, nor will there be an exception.
    }

    TRbTemplater = class
    private
        myFields: TDynamicKeyValueStore;{stores the field as TStringObject value}
        myDocStrands: TList;            {store document strands as list of TStringObject}
        myDoc: TList;                   {assembled document made of strands}
        myprefix: string;
        mypostfix: string;
        myprefixlen: integer;
        mypostfixlen: integer;
        myTemplate: string;
        myReplacedText: string;

        procedure setField(_key: string; const _field: string);
        function getField(_key: string): string;

        procedure setprefix(const _prefix: string);
        procedure setpostfix(const _postfix: string);

        procedure setMyTemplate(const _template: string);
        procedure setHasChanged;
        procedure clearDoc;
        procedure initDoc;

    public
        property prefix: string read myprefix write setPrefix;
        property postfix: string read mypostfix write setPostfix;
        property template: string read myTemplate write setMyTemplate;
        property field[_key: string]: string read getField write setField;

        function fields: TStringArray;
        function fieldCount: integer;
        function parse: boolean;
        function Text: string; {replaced with template}
        function debug: string;

        {Loads the template file}
        function load(_file: string):TRbTemplater;

        constructor Create;
        destructor Destroy; override;
    end;

		{ THtmlTemplater }

    THtmlTemplater = class(TRbTemplater)
        function html: string; {syntax sugar for Text()}
    end;

    THtmlTemplateListBase = specialize GenericHashObjectList<THtmlTemplater>;

    THtmlTemplateList = class(THtmlTemplateListBase)

	end;

implementation

uses
    sugar.htmlbuilder, sugar.utils, sugar.textfiler, LazUTF8;

{ THtmlTemplater }

function THtmlTemplater.html: string;
begin
     Result := Text;
end;


{ TRbTemplater }

procedure TRbTemplater.setField(_key: string; const _field: string);
begin
    myFields.put({key}   myprefix + _key + mypostfix,
                 {value} _field );
    setHasChanged;
end;

function TRbTemplater.getField(_key: string): string;
begin
    Result := myFields.Value[myprefix + _key + mypostfix].Value;
end;


procedure TRbTemplater.setMyTemplate(const _template: string);
begin
    myTemplate := _template;
    parse;
    setHasChanged;
end;

procedure TRbTemplater.setHasChanged;
begin
    if Length(myReplacedText)<>0 then
        myReplacedText:='';
end;

procedure TRbTemplater.clearDoc;
var
    _p: pointer;
begin
    myDoc.Free;
    for _p in myDocStrands do
        TObject(_p).Free;
    myDocStrands.Free;
end;

procedure TRbTemplater.initDoc;
begin
    myDoc := TList.Create;  {store document as pChar }
    myDocStrands := TList.Create;  {store document as pChar }
end;

function TRbTemplater.fields: TStringArray;
begin
    Result:= myFields.keys;
end;

function TRbTemplater.fieldCount: integer;
begin
    Result:= myFields.Count;
end;


procedure TRbTemplater.setpostfix(const _postfix: string);
begin
    if mypostfix = _postfix then
        Exit;
    mypostfix := _postfix;
    mypostfixlen := Length(mypostfix);
end;

procedure TRbTemplater.setprefix(const _prefix: string);
begin
    if myprefix = _prefix then
        Exit;
    myprefix := _prefix;
    myprefixlen:= Length(myprefix);
end;

constructor TRbTemplater.Create;
begin
    prefix := TEMPLATE_PREFIX;
    postfix := TEMPLATE_POSTFIX;
    myReplacedText := '';
    myTemplate:='';
    initDoc;
    myFields := TDynamicKeyValueStore.Create; {stores the field}
    {initializing the parser}
end;

destructor TRbTemplater.Destroy;
begin
    clearDoc;
    myFields.Free;
    inherited Destroy;
end;

function TRbTemplater.parse: boolean;
type
    TRecFieldMatch = record
        id: integer;
        field: string;
        matches: SizeIntArray;
    end;
    PRecFieldMatch = ^TRecFieldMatch;
var
    _template: string;
    _start, _len, _prefixPos, _postfixPos: integer;
    _strand: string;
    _strandlen: integer;
    _extractedfield: string;
    _fieldlen: integer;
    _tmpStringObj: TStringObject;
begin
    Result := false;
    {No template defined so don't parse}
    _len := UTF8Length(myTemplate);
    if _len = 0 then exit;

    {start parsing}
    _template := myTemplate;
    _start := 1;

    clearDoc;
    initDoc;

    {Field 1}
    repeat
        _prefixPos := UTF8Pos(myprefix, _template, _start);
        if _prefixPos > 0 then
        begin
            _postfixPos := UTF8Pos(mypostfix, _template, _prefixPos + myprefixLen);
            if _postfixPos < 1 then
            begin
                {don't look for fields}
                _prefixPos  := _len + 1;
                _postFixPos := _len + 1;
                _fieldLen   := 0;
            end
            else
                _fieldlen := _postFixPos - _prefixPos + myprefixlen;
        end
        else
        begin
            {don't look for fields}
            _prefixPos := _len + 1;
            _postFixPos := _len + 1;
            _fieldLen := 0;
        end;

        {push the document strand}
        _strandlen := _prefixPos - _start;
        _strand := UTF8Copy(_template, _start, _strandlen);

        if not _strand.isEmpty then
        begin
            _tmpStringObj := TStringObject.Create;
            _tmpStringObj.Value := _strand;
            myDocStrands.Add(_tmpStringObj);
            myDoc.Add(_tmpStringObj);
        end;

        {push the Field}
        if _fieldlen > 0 then
        begin
            _extractedField := UTF8Copy(_template, _prefixPos, _fieldlen);

            if not myFields.exists(_extractedField) then
                myFields.put(_extractedField, _extractedField);

            myDoc.Add(myFields.Value[_extractedField]);
        end;

        _extractedField := '';
        _start := _prefixPos + _fieldlen;

    until _fieldLen = 0;
    Result := true;
end;

function TRbTemplater.Text: string;
var
    _strand: PChar;
    i: integer;
begin
    if myReplacedText = '' then
    begin
	    for i := 0 to myDoc.Count - 1 do
	        myReplacedText := myReplacedText + TStringObject(myDoc.Items[i]).Value;
	end;
    Result := myReplacedText;
end;

function TRbTemplater.debug: string;
begin
    Result := Format('MyDoc.count = %d' + sLinebreak +
        'myDocStrands.count = %d' + sLinebreak + 'myFields.count = %d',
        [myDoc.Count, myDocStrands.Count, myFields.Count]);
end;

function TRbTemplater.load(_file: string): TRbTemplater;
begin
     Result := self;
     with TTextFiler.Create do
     begin
          autorefresh:=True;
          fileName:= _file;
          template:= content;
          Free;
	 end;
end;

end.
