unit sugar.threadloader;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type
{ TThreadLoader }

    {A generic function that runs an object factory function in a thread
    and returns the object when it is done.

        Calling functions can opt to use a callback to be informed when the
        loader has finished. Call the data() function to access the new object.

        If you call the function before the thread has finished, you will get nil. }

    generic TThreadLoader<DataObj> = class (TThread)
    public type
        LoaderMethod = function : DataObj of object;
        LoaderProc   = function : DataObj;
        LoadedCallbackMethod = procedure(var _data: DataObj) of object;
        LoadedProgressProc = procedure (var _data: DataObj);
    protected type
        RunMethodType = (rmtMethod, rmtProc);
    protected
        myRunMethod: RunMethodType;
        myLoaderProc: LoaderProc;
        myLoadedCallbackProc: LoadedProgressProc;
        myLoaderMethod: LoaderMethod;
        myLoadedCallbackMethod: LoadedCallbackMethod;
        myData: DataObj;
        procedure Execute; override;
        procedure DoCallbackAfterLoad;
        procedure DoOnTerminate(Sender: TObject);

    public
       constructor Create(CreateSuspended: Boolean;
           const StackSize: SizeUInt= DefaultStackSize);

       constructor InitializeProc(_loader: LoaderProc; _callbackAfterLoad: LoadedProgressProc); overload;
       constructor InitializeMethod(_loader: LoaderMethod; _callbackAfterLoad: LoadedCallbackMethod); overload;

       function data: DataObj;
       function dataReady: boolean;
	end;

implementation

{ TThreadLoader }

constructor TThreadLoader.Create(CreateSuspended: Boolean;
	const StackSize: SizeUInt);
begin
    inherited;
    FreeOnTerminate:= true;
    OnTerminate:= @DoOnTerminate;
end;

constructor TThreadLoader.InitializeProc(_loader: LoaderProc;
	_callbackAfterLoad: LoadedProgressProc);
begin
    Create(True);
    myRunMethod    := rmtProc;
    myLoaderProc   := _loader;
    myLoadedCallbackProc := _callbackAfterLoad;
end;

constructor TThreadLoader.InitializeMethod(_loader: LoaderMethod;
	_callbackAfterLoad: LoadedCallbackMethod);
begin
    Create(True);
    myRunMethod      := rmtMethod;
    myLoaderMethod   := _loader;
    myLoadedCallbackMethod := _callbackAfterLoad;
end;

procedure TThreadLoader.Execute;
begin
    case myRunMethod of
        rmtMethod:
        begin
            if assigned(myLoaderMethod) then
                myData := myLoaderMethod();
		end;

		rmtProc:
        begin
            if assigned(myLoaderProc) then
                myData := myLoaderProc();
		end;
    end;
end;

procedure TThreadLoader.DoCallbackAfterLoad;
begin
    case myRunMethod of
        rmtMethod:
        if Assigned(myLoadedCallbackMethod) then
            myLoadedCallbackMethod(myData);

        rmtProc:
        if assigned(myLoadedCallbackMethod) then
            myLoadedCallbackMethod(myData);
    end;
end;

procedure TThreadLoader.DoOnTerminate(Sender: TObject);
begin
    Synchronize(@DoCallbackAfterLoad);
end;


function TThreadLoader.data: DataObj;
begin
    Result:= nil;
    if dataReady then
        Result:= myData;
end;

function TThreadLoader.dataReady: boolean;
begin
    Result:= Terminated;
end;

end.

