unit sugar.consoleapp;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, SyncObjs;

type
{Starts a console application. Stays active until a terminate signal is received.}
{SIGTERM, SIGINT, SIGQUIT, SIGKILL, SIGABRT}

   { TSugarConsoleApp }

    TSugarConsoleApp = class(TObject)
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

    function application: TSugarConsoleApp;
var
    myExitCode: Longint = -1;

implementation
uses
    {$IFDEF UNIX}
    BaseUnix, Unix;
    {$ELSE}
    windows,
    {$ENDIF}
    sugar.utils, sugar.logger,  fpmimetypes;
var
    myApplication: TSugarConsoleApp = nil;
    myStopEvent: THandle = 0;
    myTerminated : boolean = false;

function application: TSugarConsoleApp;
begin
    if not Assigned(myApplication) then
        myApplication := TSugarConsoleApp.Create;
    Result:= myApplication;
end;

{ TSugarConsoleApp : COMMON}
function TSugarConsoleApp.isRunning(): boolean;
begin
    Result:= myRunning;
end;

function TSugarConsoleApp.start: integer;
begin
    myRunning:= true;
    Result:= 1;
    try
        while not myTerminated do begin
            preRun;
            run;
            postRun;
		end;
	except
        Result:= -1;
    end;
end;

{ TSugarConsoleApp }

{$IFDEF WINDOWS}

procedure TSugarConsoleApp.preRun;
begin

end;

function consoleEventWatcher(_ctrlType: DWORD): BOOL; stdcall;
begin
    case _ctrlType of
        CTRL_C_EVENT, CTRL_BREAK_EVENT, CTRL_CLOSE_EVENT, CTRL_SHUTDOWN_EVENT:
        begin
            if myStopEvent <> 0 then
                SetEvent(myStopEvent);
            myTerminated := true;
            Result := True;  // handled
        end;
        else
            Result := False;
    end;
end;

procedure TSugarConsoleApp.Run;
var
    rc: DWORD;
begin
    // create manual-reset event, initially non-signaled
    myStopEvent := CreateEvent(nil, True, False, nil);
    if myStopEvent = 0 then
        raise Exception.Create('TSugarConsoleApp.Run:: CreateEvent failed');

    SetConsoleCtrlHandler(@consoleEventWatcher, True);
    try
        // main wait loop: block, but periodically pump queued synchronizations
        repeat
            rc := WaitForSingleObject(myStopEvent, 100); // 100ms pulse
            CheckSynchronize(0);
        until rc = WAIT_OBJECT_0;

    finally
        SetConsoleCtrlHandler(@consoleEventWatcher, False);
        if myStopEvent <> 0 then
            CloseHandle(myStopEvent);
        myStopEvent := 0;
    end;
end;

procedure TSugarConsoleApp.postRun;
begin

end;

procedure TSugarConsoleApp.initiateShutdown;
begin
    myTerminated := true;
    if myStopEvent <> 0 then
        SetEvent(myStopEvent);
end;

procedure TSugarConsoleApp.initiateRestart;
begin
    myTerminated := false;
    if myStopEvent <> 0 then
        SetEvent(myStopEvent);
end;

{$ENDIF}

{ TSugarConsoleApp - UNIX}
{$IFDEF UNIX}
procedure DoSig(sig: cint); cdecl;
begin
    myExitCode:= 0;
    case sig of
        SIGTERM: Log('Shutdown Request -->');
        SIGINT:  Log('Signal: Ctrl+C');
        SIGQUIT: Log('Signal: Quit');
        SIGKILL: Log('Signal: KILL');
        SIGABRT: Log('Signal: Abort');
        SIGUSR1:
        begin
          Log('Signal to Restart');
          myExitCode := 11;
          ExitCode   := 11;
		end;
	end;
    myTerminated := sig <> SIGUSR1; // SIGUSR1 is for restart;
    if Assigned(myStopEvent) then
        RTLEventSetEvent(myStopEvent);
end;

procedure RbConsoleApplication.preRun;
begin
; {nothing. override in child class}
end;

procedure RbConsoleApplication.Run;
var
    _sigerr: boolean;
begin
    myStopEvent := RTLEventCreate;
    try
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

        // Wait, but wake periodically to process queued synchronizations
        while RTLEventWaitFor(myStopEvent, 100) = wrTimeout do
            CheckSynchronize(0);
    finally
        RTLEventDestroy(myStopEvent);
        myStopEvent := nil;
    end;
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

