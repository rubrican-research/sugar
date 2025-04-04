unit sugar.url;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, sugar.collections;

type
  { TUrl }

     TUrl = class
     protected
         mycaption: string;
         myrootDir: string;
         myfileName: string;
         mywebRoot: string;
         myFilePath: string;
         procedure setcaption(const _caption: string);
         procedure setrootDir(const _rootDir: string); virtual;
         procedure setfileName(const _fileName: string); virtual;
         procedure setwebRoot(const _webRoot: string);
         function geturl(_prefix: string = ''): string; virtual;
     public
         property Caption: string read mycaption write setcaption;
         property rootDir: string read myrootDir write setrootDir;
         property webRoot: string read mywebRoot write setwebRoot;
         property fileName: string read myfileName write setfileName;
         function filePath: string; // gets the file path with root dir
         function mimeType: string;
         function url: string; virtual; final;
     end;
     { TWebURL }
     TWebURLParamType = (urlparamKeyValue, urlparamPath);

     TWebURL = class(TUrl)
     private
         myserver: string;
         myParams: TStringList;
         myToken: string;
         myJWT: string;
         myisCurrent: boolean;
         procedure settoken(const _token: string);
         procedure setjwt(const _jwt: string);
         function getparams(const _name: string): string;
         procedure setparams(const _name: string; const _params: string);
         procedure setisCurrent(const _isCurrent: boolean);

     public
         paramType: TWebURLParamType;
         paramsRequired: string;

         property params[_name: string]: string read getparams write setparams;
         {comma delimited list of parameters that arerequired.
         Throws an exception while generating url if any required parameter is missing }

         {if these are set, then are always added as url parameters. not as urlparamPath}

         property token: string read mytoken write settoken;
         property jwt: string read myjwt write setjwt;

         property server: string read myserver write myserver;
         property isCurrent: boolean read myisCurrent write setisCurrent;

         function geturl(_prefix: string = ''): string; override;
         function paramCount: integer;
         function paramName(const _index: integer): string;
         function paramValue(const _index: integer): string;
         function paramNames: TStrings;

         procedure addRequiredParam(_param: string);
         procedure decode(const _url: string);

         constructor Create;
         destructor Destroy; override;
     end;

implementation

uses
     sugar.utils, sugar.logger, sugar.threadwriter;
{ TUrl }

procedure TUrl.setfileName(const _fileName: string);
begin
    if myfileName = _fileName then
        Exit;
    myfileName := _fileName;
    myFilePath:= '';
end;

procedure TUrl.setwebRoot(const _webRoot: string);
begin
    if mywebRoot = _webRoot then
        Exit;
    mywebRoot := _webRoot;
end;

procedure TUrl.setcaption(const _caption: string);
begin
    if mycaption = _caption then
        Exit;
    mycaption := _caption;
end;

procedure TUrl.setrootDir(const _rootDir: string);
begin
    if myrootDir = _rootDir then  Exit;
    if not ForceDirectories(_rootDir) then
    begin
        Writeln('TTextFileLoader.setrootDir(): ' + sLinebreak +
            'Could not force directories:' + _rootDir);
    end;
    myrootDir := _rootDir;
    myFilePath:= '';
end;

function TUrl.filePath: string;
begin
    if myFilePath.isEmpty then
        myFilePath := appendPath([rootDir, fileName]);

    Result:= myFilePath;
end;

function TUrl.mimeType: string;
begin
    Result:= getMimeTypeFor(fileName);
end;

function TUrl.geturl(_prefix: string): string;
begin
    if mywebRoot.isEmpty then
        Result := 'EmptyRoot'
    else
        Result := _prefix + appendURL([mywebRoot, myfileName]);
end;

function TUrl.url: string;
begin
    Result := geturl;
end;

{ TWebURL }

procedure TWebURL.setjwt(const _jwt: string);
begin
    if myjwt = _jwt then
        Exit;
    myjwt := _jwt;
end;

procedure TWebURL.settoken(const _token: string);
begin
    if mytoken = _token then
        Exit;
    mytoken := _token;
end;

function TWebURL.getparams(const _name: string): string;
begin
    Result := decodeURL(myParams.Values[decodeURL(_name)]);
end;

procedure TWebURL.setparams(const _name: string; const _params: string);
begin
    myParams.Values[encodeURL(_name)] := encodeURL(_params);
end;

procedure TWebURL.setisCurrent(const _isCurrent: boolean);
begin
    if myisCurrent = _isCurrent then
        Exit;
    myisCurrent := _isCurrent;
end;

function TWebURL.geturl(_prefix: string): string;

    function hasRequiredParameters: boolean;
    var
        _params, _paramsRequired: TStrings;
        _i, _count: integer;
    begin
        Result := True;

        _params := paramNames;
        _paramsRequired := toStringList(paramsRequired, ',');
        _count := _paramsRequired.Count;
        _i := 0;

        while Result and (_i < _count) do
        begin
            Result := _params.IndexOf(_paramsRequired[_i]) <> -1;
            Inc(_i);
        end;
        if Result = False then
        begin
            raise Exception.Create(
                'TWebURL: Required parameters are not in the parameter list >>' +
                webRoot + '::' + _paramsRequired[_i - 1]);
        end;

        _params.Free;
        _paramsRequired.Free;
    end;

    function getParamsAsKeyValue: string;
    begin
        Result := myParams.DelimitedText;
    end;

    function getParamsAsPath: string;
    var
        _i: integer;
    begin
        Result := '';
        for _i := 0 to paramCount - 1 do
            Result := Result + format('/%s/%s',
                [myParams.Names[_i], myParams.ValueFromIndex[_i]]);
    end;

begin

    Result := server + inherited geturl(_prefix);
    {check if required parameters are in the list of parameters.
    Throws and exception if required parameters are not found}
    if (myParams.Count > 0) and hasRequiredParameters then
    begin
        case paramType of
            urlparamKeyValue: Result := Result + '?' + getParamsAsKeyValue;
            urlparamPath: Result := appendPath([Result, getParamsAsPath]);
        end;
    end;
end;

function TWebURL.paramCount: integer;
begin
    Result := myParams.Count;
end;

function TWebURL.paramName(const _index: integer): string;
begin
    Result := decodeURL(myParams.Names[_index]);
end;

function TWebURL.paramValue(const _index: integer): string;
begin
    Result := decodeURL(myParams.ValueFromIndex[_index]);
end;

function TWebURL.paramNames: TStrings;
var
    _i: integer;
begin
    Result := TStringList.Create;
    for _i := 0 to paramCount - 1 do
        Result.Add(paramName(_i));
end;

procedure TWebURL.addRequiredParam(_param: string);
begin
    if not paramsRequired.IsEmpty then
        paramsRequired := paramsRequired + ',';
    paramsRequired := paramsRequired + _param;
end;

procedure TWebURL.decode(const _url: string);

    procedure decodeKeyValue;
    var
        i: integer;
        part: integer = 1;
        _params: string = '';
    begin
        if _url.IsEmpty then
            exit;

        myWebRoot := '';
        for i := 1 to _url.Length do
        begin
            if _url[i] = '?' then
            begin
                part := 2;
                continue;
            end;

            case part of
                1: mywebRoot := myWebRoot + _url[i];
                2: _params := _params + _url[i];
                else;
            end;
        end;
        myParams.DelimitedText := _params;

    end;

    procedure decodePath;
    var
        _fields, _webrootFields: TStrings;
        _i, _count: integer;
        _params: string = '';

    begin
        if mywebRoot.isEmpty then
            raise Exception.Create(
                'TWebURL.decode: Cannot decodePath() when myWebRoot is empty');

        _fields := toStringList(_url, '/');
        _webrootFields := toStringList(myWebRoot, '/');

        _count := _fields.Count;

        {index of the start of parameters}
        _i := _webrootFields.Count + 1; {because leading / causes an empty 1st field}

        while (_i < _count) do
        begin
            if _i = (_count - 1) then
                {This is the case where the last parameter is empty}
                myParams.Values[_fields[_i]] := ''
            else
                {store as is. Assumed that the url contains encoded info}
                myParams.Values[_fields[_i]] := _fields[_i + 1];

            {next iteration}
            Inc(_i, 2);
        end;
        _webrootFields.Free;
        _fields.Free;
    end;

begin
    case paramType of
        urlparamKeyValue: decodeKeyValue;
        urlparamPath: decodePath;
    end;
end;

constructor TWebURL.Create;
begin
    myParams := TStringList.Create;
    myParams.Delimiter := '&';
    paramType := urlparamKeyValue;
end;

destructor TWebURL.Destroy;
begin
    FreeAndNil(myParams);
    inherited Destroy;
end;


end.

