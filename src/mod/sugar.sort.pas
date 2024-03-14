unit sugar.sort;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

    function sortArr(_arr: array of string): TStringArray;
    function sortList(_str: string; _delim: string=','): TStringArray;
    function sortList(_strlst: TStrings): TStrings; overload;

implementation
uses
    nsort, sugar.utils;
function sortArr(_arr: array of string): TStringArray;
var
  s: TStringList;
  a: string;
  i: integer;
begin
    s:= TStringList.Create;
    for a in _arr do s.Add(a);

    NaturalSort(s,stFloatThousand);

    SetLength(Result, s.Count);
    for i:= 0 to pred(s.Count) do Result[i]:= s[i];

    s.Free;
end;

function sortList(_str: string; _delim: string): TStringArray;
var
  sl: TStrings;
begin
    sl:= toStringList(_str, _delim);
    NaturalSort(sl, stFloatThousand);
    Result:= makeStringArray(sl);
    sl.Free;
end;

function sortList(_strlst: TStrings): TStrings;
begin
    NaturalSort(_strlst, stFloatThousand);
    Result:= _strlst;
end;

end.

