unit sugar.animateui;


{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, fpJSON;

type
    TControlArray = array of TControl;
    {To fine tune duration of animation according to context. The Animation Framework uses milliseconds.
    Depending on the application, you can implement a function that returns a customized se to timeToDone values based on this enum}
    NAnimationSpeed = (aspeedInstant, aspeedQuick, aspeedNormal, aspeedSlow);


    {Animation handler}
    // How to implement the animation handler.
    // The state parameter contains 3 default values that are inserted by the timer:
    //  - integers['interval']      : This is the timer interval in millizeconds
    //  - Integers['timeToDone']    : This is the time in milliseconds that the animation is allowed to take
    //  - Integers['currTime']      : This is the current elapsed time in milliseconds since the timer started. When this value is 0, you can initialize your animation state.
    // You can add more state variables when you initialize.
    ProcAnimateProc =  procedure(constref _control: TControlArray; constref _state: TJSONObject);
    ProcAnimateMeth =  procedure(constref _control: TControlArray; constref _state: TJSONObject) of object;

    // USAGE:  animate(...)
    //      constref _control: TControl: The control that should be animated.
    //      _animateProc: PAnimateProc;: the procedure that handles the animation.
    //      const _timeToDone: cardinal = 0

    {Procedure based}
    procedure animate(const _controls: TControlArray; _animateProc: ProcAnimateProc; const _timeToDone: cardinal = 0); overload;

    {Object Method based}
    procedure animate(const _controls: TControlArray; _animateMeth: ProcAnimateMeth; const _timeToDone: cardinal = 0); overload;

    {ANIMATION LIBRARIES}
    {bloom(): grow the control in height from 0 to configured height.}
    procedure bloom (constref _controls: TControlArray; constref _state: TJSONObject);


implementation
uses
    Math, sugar.logger;

type

    TAsyncFreeObj = class {copied from package saph, obj.listener to prevent dependency}
        procedure FreeObject(_obj: PtrInt);
	end;

    procedure TAsyncFreeObj.FreeObject(_obj: PtrInt);
    begin
        FreeAndNil(TObject(_obj));
        Free;// Destroy itself
    end;

{Frees the object asynchronously.
Allows a procedure to finish tasks without holding up expensive free operations}
procedure asyncFreeObject(_obj: TObject);
begin
    with TAsyncFreeObj.Create do
        Application.QueueAsyncCall(@FreeObject, PtrInt(_obj));
end;

type
    { TStopAnimation }

    TStopAnimation = class(Exception) // Raise this exception when you want to stop the animation timer
        constructor Create;  // Syntax sugar. Create with empty message;
    end;

    { TAnimateTimer }

    TAnimateTimer = class(TTimer)
    private type ERunType = (animateProc, animateMeth);
    private
        myRunType : ERunType;
        myControls: TControlArray;
        myAnimateProc: ProcAnimateProc;
        myAnimateMeth: ProcAnimateMeth;
        myTimeToDone: cardinal;         // if value is 0 then keep running till myAnimateProc raises TStopAnimation
        myCurrTime: cardinal;
        myState: TJSONObject;
    protected
        function hasMethod: boolean;
        procedure runAnimation(Sender: TObject);
        procedure stop;
        function initState: TJSONObject;
    public
        constructor Create(constref _controls: TControlArray; _animateProc: ProcAnimateProc; const _timeToDone: cardinal; _interval: cardinal = 10); reintroduce;
        constructor Create(constref _controls: TControlArray; _animateMeth: ProcAnimateMeth; const _timeToDone: cardinal; _interval: cardinal = 10); reintroduce;
        destructor Destroy; override;
        function run: boolean; // Returns true if animation was started

    end;

procedure animate(const _controls: TControlArray;
	_animateProc: ProcAnimateProc; const _timeToDone: cardinal);
begin
    TAnimateTimer.Create(_controls, _animateProc, _timeToDone);
end;


procedure animate(const _controls: TControlArray; _animateMeth: ProcAnimateMeth; const _timeToDone: cardinal);
begin
    TAnimateTimer.Create(_controls, _animateMeth, _timeToDone);
end;

procedure bloom(constref _controls: TControlArray; constref _state: TJSONObject);
const
  __defaultStep = 3;
var
    _interval   :cardinal;
    _timeToDone :cardinal;
    _currTime   :cardinal;
	_control: TControl;

begin

    _interval   := _state.integers['interval'];
    _timeToDone := _state.integers['timeToDone'];
    _currTime   := _state.integers['currTime'];

    for _control in _controls do begin
	    case _currTime of
	        0: begin {Initialization}
	            _state.Integers['height'] := _control.Height;
	            _control.height := 0;
	            if _timeToDone > 0 then begin
	                _state.Integers['step'] := max(1, round(_state.Integers['height'] / (_timeToDone div _interval)));
				end
	            else begin
	                _state.Integers['step'] := __defaultStep;
	            end;
			end;

	        else begin
	            if _control.Height < _state.integers['height'] then
	                _control.Height := min(_state.integers['height'], _control.Height + _state.Integers['step'])
	            else
	                raise TStopAnimation.Create;
			end;
		end;
	end;

end;

{ TBreakAnimation }

constructor TStopAnimation.Create;
begin
    inherited Create('');
end;

{ TAnimateTimer }

function TAnimateTimer.hasMethod: boolean;
begin
    Result := assigned(myAnimateMeth) or assigned(myAnimateProc);
end;

procedure TAnimateTimer.runAnimation(Sender: TObject);
begin
    try
        case myRunType of
        	animateProc: myAnimateProc(myControls, myState);
            animateMeth: myAnimateMeth(myControls, myState);
        end;

        inc(myCurrTime, Interval);
        myState.Integers['currTime'] := myCurrTime;

        if myTimeToDone > 0 then begin
            if myCurrTime > myTimeToDone then begin
                raise TStopAnimation.Create;
	        end;
		end;

	except
        on B:TStopAnimation do begin
            Stop;
        end;
	end;
end;

procedure TAnimateTimer.stop;
begin
    Enabled:=False;
    asyncFreeObject(self);
end;

function TAnimateTimer.initState: TJSONObject;
begin
    Result := TJSONObject.Create;
    Result.integers['interval']   := interval;
    Result.Integers['timeToDone'] := myTimeToDone;
    Result.Integers['currTime']   := myCurrTime;
    //log('initState: interval %d, timeToDone %d, currTime %d', [interval, myTimeToDone, myCurrTime]);
end;

constructor TAnimateTimer.Create(constref _controls: TControlArray;
	_animateProc: ProcAnimateProc; const _timeToDone: cardinal; _interval: cardinal);
begin
    inherited Create(Application);
    Enabled       := false;
    myRunType     := animateProc;
    myControls    := _controls;
    myAnimateProc := _animateProc;
    myAnimateMeth := nil;
    myTimetoDone  := _timeToDone;
    myCurrTime    := 0;
    Interval      := _interval;
    OnTimer       := @runAnimation;
    myState       := initState;
    run;
end;

constructor TAnimateTimer.Create(constref _controls: TControlArray;
	_animateMeth: ProcAnimateMeth; const _timeToDone: cardinal; _interval: cardinal);
begin

    inherited Create(Application);
    Enabled       := false;
    myRunType     := animateMeth;
    myControls    := _controls;
    myAnimateProc := nil;
    myAnimateMeth := _animateMeth;
    myTimetoDone  := _timeToDone;
    myCurrTime    := 0;
    Interval      := _interval;
    OnTimer       := @runAnimation;
    myState       := initState;
    run;
end;

destructor TAnimateTimer.Destroy;
begin
    myState.Free;
	inherited Destroy;
end;

function TAnimateTimer.run: boolean;
begin
    Result :=   (length(myControls) > 0)
                and assigned(myAnimateProc)
                and (Interval > 0);

    Enabled := Result;
end;


end.

