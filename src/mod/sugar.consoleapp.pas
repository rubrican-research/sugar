unit sugar.consoleapp;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;
type
{Starts a console application. Stays active until a terminate signal is received.}
{SIGTERM, SIGINT, SIGQUIT, SIGKILL, SIGABRT}

   { RbConsoleApplication }

    RbConsoleApplication = class(TObject)
    protected
        myRunning: boolean;
    public
        function isRunning(): boolean;
        procedure preRun; virtual; {prep for launch}
        procedure Run; virtual;
        procedure postRun; virtual; {wrap up}
        procedure initiateShutdown; virtual;
        procedure initiateRestart; virtual;
        function start: integer;
	end;

    function application: RbConsoleApplication;
var
    myExitCode: Longint = -1;

implementation
uses
    {$IFDEF UNIX}
    BaseUnix,
    {$ENDIF}
    sugar.utils, fpmimetypes;
var
    myApplication: RbConsoleApplication = nil;

function application: RbConsoleApplication;
begin
    if not Assigned(myApplication) then
        myApplication := RbConsoleApplication.Create;
    Result:= myApplication;
end;

{ RbConsoleApplication : COMMON}
function RbConsoleApplication.isRunning(): boolean;
begin
    Result:= myRunning;
end;

function RbConsoleApplication.start: integer;
begin
    myRunning:= true;
    Result:= 1;
    try
        preRun;
        run;
        postRun;
    except
        Result:= -1;
    end;
end;

{ RbConsoleApplication }

{$IFDEF WINDOWS}

procedure RbConsoleApplication.preRun;
begin

end;

procedure RbConsoleApplication.Run;
var
    c: string[64];
begin
    while isRunning() do begin
        readLn(c);
        case lowercase(c) of
            'quit': myRunning:=false;
		end;
	end;
end;

procedure RbConsoleApplication.postRun;
begin

end;

procedure RbConsoleApplication.initiateShutdown;
begin

end;

procedure RbConsoleApplication.initiateRestart;
begin

end;

{$ENDIF}

{ RbConsoleApplication - UNIX}
{$IFDEF UNIX}
procedure DoSig(sig: cint); cdecl;
begin
    myExitCode:= 0;
    case sig of
        SIGTERM: rbutils.Log('Shutdown Request -->');
        SIGINT:  rbutils.Log('Signal: Ctrl+C');
        SIGQUIT: rbutils.Log('Signal: Quit');
        SIGKILL: rbutils.Log('Signal: KILL');
        SIGABRT: rbutils.Log('Signal: Abort');
        SIGUSR1:
        begin
          rbutils.Log('Signal to Restart');
          myExitCode := 11;
          ExitCode   := 11;
		end;
	end;
end;

procedure RbConsoleApplication.preRun;
begin
; {nothing. override in child class}
end;

procedure RbConsoleApplication.Run;
var
    _sigerr: boolean;
begin
        _sigerr := fpSignal(SIGINT, SignalHandler(@DoSig)) = signalhandler(SIG_ERR);
        if _sigerr then
            WriteLn('Could not init signal handler for SIGINT');

        _sigerr := fpSignal(SIGTERM, SignalHandler(@DoSig)) = signalhandler(SIG_ERR);
        if _sigerr then
            WriteLn('Could not init signal handler for SIGTERM');

        _sigerr := fpSignal(SIGQUIT, SignalHandler(@DoSig)) = signalhandler(SIG_ERR);
        if _sigerr then
            WriteLn('Could not init signal handler for SIGQUIT');

        _sigerr := fpSignal(SIGKILL, SignalHandler(@DoSig)) = signalhandler(SIG_ERR);
        if _sigerr then
            WriteLn('Could not init signal handler for SIGKILL');

        _sigerr := fpSignal(SIGABRT, SignalHandler(@DoSig)) = signalhandler(SIG_ERR);
        if _sigerr then
            WriteLn('Could not init signal handler for SIGABRT');

        _sigerr := fpSignal(SIGUSR1, SignalHandler(@DoSig)) = signalhandler(SIG_ERR);
        if _sigerr then
            WriteLn('Could not init signal handler for SIGUSR1');

        FpPause;
end;

procedure RbConsoleApplication.postRun;
begin
; {nothing. override in child class}
end;


procedure RbConsoleApplication.initiateShutdown;
begin
    FpKill(FpGetpid, SIGTERM);
end;

procedure RbConsoleApplication.initiateRestart;
begin
    FpKill(FpGetpid, SIGUSR1);
end;

{$ENDIF}

finalization
    if assigned(myApplication) then
        myApplication.Free;
end.

