unit sugar.maps;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fgl;
type

    TStringMap    = class(specialize TFPGMap<string, string>);
    TIntStringMap = class(specialize TFPGMap<integer, string>);
    TStringIntMap = class(specialize TFPGMap<string, integer>);
    TIntMap       = class(specialize TFPGMap<integer, integer>);

{Factory functions}
    function NewStringMap    : TStringMap;
    function NewTIntStringMap: TIntStringMap;
    function NewTStringIntMap: TStringIntMap;
    function NewTIntMap      : TIntMap;

implementation

function NewStringMap: TStringMap;
begin
    Result := TStringMap.Create;
    Result.Sorted:=True;
end;

function NewTIntStringMap: TIntStringMap;
begin
    Result := TIntStringMap.Create;
    Result.Sorted:=True;
end;

function NewTStringIntMap: TStringIntMap;
begin
    Result := TStringIntMap.Create;
    Result.Sorted:=True;
end;

function NewTIntMap: TIntMap;
begin
    Result := TIntMap.Create;
    Result.Sorted:=True;
end;

end.

