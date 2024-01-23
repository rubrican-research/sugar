unit sugar.profiler;

{$mode objfpc}{$H+}

interface

uses
	 Classes, SysUtils;
type
  { TCodeProfiler }

  TCodeProfiler = class
  private
      myStart: QWord;
      myName: string;
  public
      constructor Create(_name: string);
      function time(_desc: string): string; overload;
      function time(_desc: string; _reset: boolean): string; overload;
      function duration: QWord; overload;
      function Name: string;
      function reset: QWord;
  end;


implementation

{ TCodeProfiler }

constructor TCodeProfiler.Create(_name: string);
begin
    inherited Create;
    myName := _name;
    reset;
end;

function TCodeProfiler.time(_desc: string): string;
var
    d: QWord;
begin
    d := duration;
    Result := Format('[%s] %s = %d ms', [myName, _desc, d]);
end;

function TCodeProfiler.time(_desc: string; _reset: boolean): string;
begin
    Result := time(_desc);
    if _reset then
        reset;
end;

function TCodeProfiler.duration: QWord;
begin
    Result := GetTickCount64 - myStart;
end;

function TCodeProfiler.Name: string;
begin
    Result := myName;
end;

function TCodeProfiler.reset: QWord;
begin
    Result := duration;
    myStart := GetTickCount64;
end;

end.

