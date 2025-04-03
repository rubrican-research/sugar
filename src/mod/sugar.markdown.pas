unit sugar.markdown;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpjson;

function processMarkDown(_source: string): string;
procedure processMarkdownFor(constref _jdata: TJSONData);


implementation
uses
    MarkdownProcessor;

function processMarkDown(_source: string): string;
var
  mdProcessor: TMarkdownProcessor;
begin
    try
        mdProcessor:= TMarkdownProcessor.CreateDialect(mdDaringFireball);
        mdProcessor.AllowUnsafe:= True;
        Result:= mdProcessor.process(_source);
	finally
	    mdProcessor.Free;
    end;
end;

procedure processMarkdownFor(constref _jdata: TJSONData);
begin
    if assigned(_jdata) then
      _jdata.AsString:= processMarkDown(_jdata.AsString);
end;

end.

