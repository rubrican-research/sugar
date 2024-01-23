unit sugar.textfiler;

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

     TFileURI = class(TUrl)

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

     { TTextFiler }
     TTextFiler = class;

     // TTextFilerList = specialize GenericHashObjectList<TTextFiler>;

     TTextFiler = class(TFileURI)
     private
     const
         FILE_NOT_READ = -79;
         MAX_ATTEMPTS  = 1000;
         WAIT_TIME     = 64; {milli seconds}
     type
         FilerStatus = (filerNone, filerOpen, filerClosed);

 	procedure updateContentVersion;
     public
         nameTag: string; {Convenience variable to store debug information}
 	    function awaitFileLock: boolean;
         procedure unlockFile;
         function isLocked: boolean;
         function isLockedByMe: boolean;

     private
         class var objCount: DWord;
         class var lockList: TStringMap;
         class var versionLog: TStringMap; {List of open files. If value is empty then file has not changed since last read.}
         class procedure initLockList;
         class function isLocked(_fileName: string): boolean;
         class function isLockedByMe(_fileName: string): boolean; {Checks if the current thread has locked the file}
         class function lock(_fileName: string): boolean;
         class function unlock(_fileName: string): boolean;
         class procedure freeLockList;

     public class procedure shutdown;
     public class function memReport: string;

     private
         myObjectKey: string; {HashIndex for the filer list}
         myContentVersion: string;
         mystatus: FilerStatus;
         touched: boolean;
         myLastReadFileAge: longint;
         myContentBuffer: string;
         myAutoRefresh: boolean;
         myContentsSynched: boolean;
         myalwaysOverwrite: boolean;
         function getContents: string;
         procedure setContents(const _content: string);
         procedure setAutoRefresh(const _cached: boolean);
         procedure setalwaysOverwrite(const _alwaysOverwrite: boolean);
 		procedure setContentSynched(const _contentsSynched: boolean);
     protected
         procedure setrootDir(const _rootDir: string); override;
         procedure setfileName(const _fileName: string); override;
     public
         constructor Create;
         destructor Destroy; override;

         property content: string read getContents write setContents;
         property autorefresh: boolean read myAutoRefresh write setAutoRefresh;
         property contentsSynched: boolean read myContentsSynched write setContentSynched;
         property alwaysOverwrite: boolean read myalwaysOverwrite write setalwaysOverwrite;

         procedure touch; // creates a file if it doesn't exist;
         procedure forceSave;
         function fileChanged: boolean;

         {releases the content but does not clear the object }
         procedure close;
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

{ TTextFiler }
procedure TTextFiler.updateContentVersion;
begin
    myContentVersion  := genRandomKey(16,3);
    versionLog.put(filePath, myContentVersion);
end;

function TTextFiler.getContents: string;
var
    _filePath: string;
    _fileAge: longint;
	_readComplete: Boolean;
    _waitCount: integer = 0;
begin
    //EnterCriticalSection(myFilerCriticalSection);
{
~ getContents() ~
  INITIAL INVOCATION
      Check if the file exists by looking at fileAge <> -1
      Read the file
      Store contents in myContentBuffer
      forceSave the current FileAge in my myLastReadFileAge
  SUBSEQUENT CALLS
      Check if the current fileAge has changed from myLastReadFileAge
      If it is different, then read the file,
      forceSave it to myContentBuffer and update myLastReadFileAge
}

    Result := myContentBuffer; {Empty on initial call}
    _filePath:= filePath;

    if not FileExists(_filePath) then exit;

    {IF  autorefresh    &     no errors      &      local file is newer          }
    if (myAutoRefresh) and fileChanged then
    begin
        if awaitFileLock then
        begin
            //EnterCriticalSection(myFilerCriticalSection);
	        with TStringList.Create do
	        begin
	            try
	                LoadFromFile(_filePath);
	                Result := Text;
	            finally
	                Free;
	            end;
	        end;
	        {Update the timestamp and buffer }
	        myLastReadFileAge := FileAge(_filePath);
	        myContentVersion  := versionLog.valueOf(_filePath); {Mark as read}

            if myContentVersion.isEmpty then {This is initial read.}
                updateContentVersion;        {To ensure that fileChanged does not fail... }

	        myContentBuffer   := Result;
	        myContentsSynched := True;
            unlockFile;
            //LeaveCriticalSection(myFilerCriticalSection);
        end
        else
            Log('%s: GetContents could not lock', [fileName]);
	end
    else
        Log('%s:: skipped read', [fileName]);
    {Otherwise Don't read the file. Just return whatever is in the buffer    }

    //LeaveCriticalSection(myFilerCriticalSection);
end;

procedure TTextFiler.setContents(const _content: string);
var
    _filepath: string;
    _fileAge: LongInt;
begin
    //EnterCriticalSection(myFilerCriticalSection);

    _filepath := filePath;
    _fileAge := FileAge(_filepath);
    {Don't overwrite a file that
        - already exists
        - has been modified after it was read by TTextFiler }
    if _fileAge > -1 then {File exists}
    begin
        if _fileAge > myLastReadFileAge then
        begin
            {file has been modified since it was last read}
            // Log(fileName + ' on disk is newer.');
            if not alwaysOverwrite then
            begin
                {DON'T overwrite!!}
                myContentsSynched := False;
                exit; {****}
            end
            else
                ; // Log('Overwriting ' + fileName + ' because alwaysOverwrite = true');
        end;
    end;
{

}    {
      Control comes here if
        1) writing immediately after object is created.
        2) file has not been changed since the last read.
    }

    {This is not necessary because setting RootDIR already forces directories}
    //if not DirectoryExists(rootDir) then ForceDirectories(rootDir);
    try
	    if awaitFileLock() then
	    begin
            //EnterCriticalSection(myFilerCriticalSection);
	        with TStringList.Create do {This is the easiest, surest way}
		    begin
		        Text := _content;
		        SaveToFile(_filePath);
		        Free;
		    end;

	        {Mark the file as changed}
	        myContentsSynched := True;
		    myLastReadFileAge := FileAge(_filepath);
		    myContentBuffer   := _content;
            updateContentVersion;
	        unlockFile;
            Log(']:--> Saved "%s"', [fileName]);
            //LeaveCriticalSection(myFilerCriticalSection);
		end
        else
            Log('TTextFiler did not save "%s" because fileLock timed out', [fileName]);

	finally
        ;
	end;
    //LeaveCriticalSection(myFilerCriticalSection);
end;

class procedure TTextFiler.initLockList;
begin
    if not assigned(lockList) then
    begin
        objCount:= 0; {initialize}
        lockList:= TStringMap.Create;
        versionLog:= TStringMap.Create;

	end;
end;

class function TTextFiler.isLocked(_fileName: string): boolean;
begin
    if assigned(lockList) then
        Result:= lockList.exists(_fileName)
    else
        Result:= false;
end;

class function TTextFiler.isLockedByMe(_fileName: string): boolean;
begin

    Result:= (lockList.valueOf(_fileName) = IntToStr(ThreadID));
end;

class function TTextFiler.lock(_fileName: string): boolean;
begin
    //EnterCriticalSection(myLockerCriticalSection);
    //EnterCriticalSection(myFilerCriticalSection);
    try
        Result:= isLockedByMe(_fileName);
        if not Result then {It is not locked by me}
        begin
            if not isLocked(_fileName) then
	        begin
                lockList.put(_fileName, IntToStr(ThreadID));
                Result:= true;
		    end
	        else
            begin
	            Result:= false;
		    end;
        end;
	finally
        //LeaveCriticalSection(myLockerCriticalSection);
        //LeaveCriticalSection(myFilerCriticalSection);
	end;
end;

class function TTextFiler.unlock(_fileName: string): boolean;
begin
    //EnterCriticalSection(myLockerCriticalSection);
    //EnterCriticalSection(myFilerCriticalSection);
    try
        if isLockedByMe(_fileName) then
        begin
            Result:= lockList.delete(_fileName);
            if isLocked(_fileName) then Log('Blooper: I just delete lock info');
		end
		else
        begin
            Log('unlock failed because not locked by me');
            Result:= false;
		end;

	finally
        //LeaveCriticalSection(myLockerCriticalSection);
        //LeaveCriticalSection(myFilerCriticalSection);
	end;
end;

class procedure TTextFiler.freeLockList;
begin
    if objCount < 1 then
    begin
        FreeAndNil(lockList);
        FreeAndNil(versionLog);
        shutdown;
        // Log('freeLockList()');
	end
    else
        ; //Log('TTextFiler:: is being used by %d people ',[objCount]);
end;

class function TTextFiler.memReport: string;
begin
    Result:= format('TTextFiler:: is being used by %d people ',[objCount]);
end;

class procedure TTextFiler.shutdown;
begin

end;

function TTextFiler.awaitFileLock: boolean;
var
//    _lockFilePath: string;
    _waitCount : DWord = 0;
begin
    //_lockFilePath:= getLockFileName;
    Result:= false;
    repeat
        if lock(filePath) then
        begin
            // saveFileContent(_lockFilePath, htmlDateTime);
            Result:= true;
		end
        else
        begin
            if _waitCount < MAX_ATTEMPTS then
            begin
                {Try again}
                inc(_waitCount);
                sleep(Random(WAIT_TIME));
                // Log('%s is waiting for lock', [nameTag]);
			end
            else
            begin
                Log('TTextFiler.awaitFileLock has timed out...(%d)', [_waitCount]);
                break;
			end;
		end;
	until Result;
end;

procedure TTextFiler.unlockFile;
begin
    unlock(filePath);
end;

function TTextFiler.isLocked: boolean;
begin
    Result:= isLocked(filePath);
end;

function TTextFiler.isLockedByMe: boolean;
begin
    Result:= isLockedByMe(filePath);
end;


procedure TTextFiler.setAutoRefresh(const _cached: boolean);
begin
    if myAutoRefresh = _cached then Exit;
    myAutoRefresh := _cached;
end;

procedure TTextFiler.setalwaysOverwrite(const _alwaysOverwrite: boolean);
begin
    if myalwaysOverwrite = _alwaysOverwrite then Exit;
    myalwaysOverwrite := _alwaysOverwrite;
end;

procedure TTextFiler.setContentSynched(const _contentsSynched: boolean);
begin
	if myContentsSynched=_contentsSynched then Exit;
	myContentsSynched:=_contentsSynched;
end;

procedure TTextFiler.setrootDir(const _rootDir: string);
begin
	inherited setrootDir(_rootDir);
end;

procedure TTextFiler.setfileName(const _fileName: string);
begin
	inherited setfileName(_fileName);
end;

constructor TTextFiler.Create;
var
    _done: boolean = false;
begin
    inherited;
    initLockList;
    myContentBuffer := '';
    myLastReadFileAge := FILE_NOT_READ;
    myAutoRefresh := False;
    myalwaysOverwrite := True;
    touched := False;
    InterlockedIncrement(objCount);
    myObjectKey:= genRandomKey(16, 3);
end;

destructor TTextFiler.Destroy;
begin
    Close;
    InterlockedDecrement(objCount);
    freeLockList;
	inherited Destroy;
end;

procedure TTextFiler.touch;
begin
    if not touched then
    begin
        TThreadFileWriter.touch(filePath);
        touched := True;
    end;
end;

procedure TTextFiler.forceSave;
begin
    setContents(myContentBuffer);
end;

function TTextFiler.fileChanged: boolean;
var
    _threadName: string;
    _myContentVersion, _myLoggedVersion: string;
	_myLastReadFileAge: LongInt;
    b: boolean;

begin
    //Result:= (versionLog.valueOf(filePath)= '')
    //         or (versionLog.valueOf(filePath) <> myContentVersion);

    // EnterCriticalSection(myFilerCriticalSection);
    try
	    _threadName         := nameTag;
        _myContentVersion   := myContentVersion;
        _myLastReadFileAge  := myLastReadFileAge;
        _myLoggedVersion    := versionLog.valueOf(filePath);

	    Result:= (_myLastReadFileAge <> FileAge(filePath));
	    Result:= Result or (_myLoggedVersion.isEmpty);
	    Result:= Result or (CompareStr(_myLoggedVersion, _myContentVersion) <> 0);

	    //if not Result then
	    //begin
		   // Log('>> %s:: Filechanged = %s because --->', [_threadName, YesNo(Result)]);
     //       b:= (_myLastReadFileAge <> FileAge(filePath));
		   // Log('>> %s:: (myLastReadFileAge <> FileAge(filePath)) = %s [Result := %s]' ,        [_threadName,YesNo((_myLastReadFileAge <> FileAge(filePath))), yesno(b)]);
     //
     //       b:= b or (versionLog.valueOf(filePath)= '');
     //       Log('>> %s:: (myCurrentReadKey.isEmpty) = %s [Result := %s]',                [_threadName,YesNo(_myLoggedVersion.isEmpty), yesno(b)]);
     //
     //       b:= b or (CompareStr(versionLog.valueOf(filePath),_myContentVersion) <> 0);
		   // Log('>> %s:: (myCurrentReadKey <> myLastReadKey) = %s [Result := %s]',   [_threadName,YesNo((CompareStr(_myLoggedVersion, _myContentVersion) <> 0)), yesno(b)]);
     //
     //       Log('>> %s::', [_threadName]);
	    //    Log('>> %s:: (changeLog.valueOf(filePath) = "%s"', [_threadName,_myLoggedVersion]);
	    //    Log('>> %s:: myLastReadKey = "%s"',                [_threadName,_myContentVersion]);
     //
		   // Log('');
		   // Log('');
	    //end;

    finally
       ;// LeaveCriticalSection(myFilerCriticalSection);
    end;

    //if Result then
    //    Log('TTextFiler.fileChanged():: %s has changed. lastAge: %d <-> %d current', [fileName, myLastReadFileAge,FileAge(filePath) ]);
end;

procedure TTextFiler.close;
begin
    // Thinking. Not implemented yet.
    // Log('}-p Closed "%s"',[fileName]);
end;



end.

