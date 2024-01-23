unit sugar.threadwriter;

{$mode objfpc}{$H+}

interface

uses
	 Classes, SysUtils;

type
	 { TThreadFileWriter }
	 TThreadFileWriter = class(TThread)
	 private
	     function dirExists: boolean;
	     procedure doTouch;
	     procedure doOverwrite;
	     procedure doAdd;
	 protected
	     writeString: string;
	     fileName: string;
	     command: string;
	     procedure Execute; override;
	 public
	     {All these are freed on terminate so you can call them without
	     needed to call the destructor }
	     constructor Create(_fileName: string; _writeString: string);
	     constructor add(_fileName: string; _writeString: string);
	     constructor put(_fileName: string; _writeString: string);
	     constructor touch(_fileName: string);
	 end;


implementation

uses
     sugar.logger;

{ TThreadFileWriter }

procedure TThreadFileWriter.Execute;
begin
    case command of
        'add': doAdd;
        'touch': doTouch;
        'overwrite': doOverwrite;
    end;
end;

constructor TThreadFileWriter.Create(_fileName: string; _writeString: string);
begin

    inherited Create({suspended = } True);
    FreeOnTerminate := True;
    fileName := _fileName;
    writeString := _writeString;

    if not dirExists then
        raise Exception.Create ('TThreadWriter.doAdd() ForceDirectories failed ' + fileName);

    doTouch;
end;

function TThreadFileWriter.dirExists: boolean;
var
    _dir: string;
begin
    Result:= True;
    _dir:= ExtractFileDir(fileName);
    if not DirectoryExists(_dir) then
        Result:= ForceDirectories(_dir);
end;

procedure TThreadFileWriter.doTouch;
var
    logfile: Text;
begin
    if FileExists(fileName) then exit;

	AssignFile(logfile, fileName);
    {$I-}
    try
        Rewrite(logfile); {create the file if not exists}
    {$I+}

	finally
	    CloseFile(logfile);
    end;

end;

procedure TThreadFileWriter.doOverwrite;
var
    logfile: Text;
    _done: boolean = false;
    _turn: byte = 0;
begin
    if not dirExists then
    begin
        Log('TThreadWriter.doOverwrite() ForceDirectories failed ' + fileName);
        exit;
	end;
    AssignFile(logfile, fileName);
    try
        {$I-}
        repeat
            Rewrite(logfile); {create the file if not exists}
            if IOResult = 0 then
            begin
                Write(logfile, writeString);
                _done:= (IOResult = 0);
			end
			else
            begin
                {$IFDEF linux}
                writeln('~~~ TThreadFileWriter.doOverwrite:: retrying ', _turn);
                {$ENDIF}
                sleep(128);
                inc(_turn);

			end;
		until _done or (_turn = 17);
        {$I+}
	finally
	    CloseFile(logfile);
    end;

end;

procedure TThreadFileWriter.doAdd;
const
    NUM_OF_TRIES = 17;
var
    _file: Text;
    _done: boolean = False;
    _turn: byte = 0;
begin
    try
	    repeat

	        AssignFile(_file, fileName);
	        {$I-}
	        try
	            Append(_file);
	            _done:= (IOResult = 0);

	            if _done then
	            begin
	                Write(_file, writeString);
	                _done := (IOResult = 0);
				end;

	        {$I+}
	        finally
	            CloseFile(_file);
	        end;

	        if not _done then
	        begin
	            _done := (_turn = NUM_OF_TRIES);
	            if not _done then
	            begin
	                Inc(_turn);
	                sleep(17); {Arbitrary wait to have write access}
	                {$IFDEF linux}
	                writeln('~~~ TThreadFileWriter.doOverwrite:: retrying ', _turn);
	                {$ENDIF}

	            end;
	        end;

	    until (_done);

	except
        on E:Exception do
        begin
            writeln('ThreadWriter.doAdd() :: ', E.Message);
		end;
	end;
end;

constructor TThreadFileWriter.add(_fileName: string; _writeString: string);
begin
    Create(_fileName, _writeString);
    command := 'add';
    Start;
end;

constructor TThreadFileWriter.put(_fileName: string; _writeString: string);
begin
    Create(_fileName, _writeString);
	command := 'overwrite';
	Start;
end;

constructor TThreadFileWriter.touch(_fileName: string);
begin
    Create(_fileName, '');
    command := 'touch';
    Start;
end;
end.

