unit sugar.textfiler;

{$mode objfpc}{$H+}

interface

uses
	 Classes, SysUtils, sugar.collections, sugar.url;

type

     TFileURI = class(TUrl)

     end;

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

