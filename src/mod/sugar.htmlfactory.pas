unit sugar.htmlfactory;

{$mode objfpc}{$H+}

{General object factory for Rubrican}

interface

uses
    Classes, SysUtils, sugar.htmlbuilder, sugar.utils, sugar.textfiler;

const
 ASSETS_BASE = DirectorySeparator + 'assets';
 ASSETS_CSS  = DirectorySeparator + 'css';
 ASSETS_JS   = DirectorySeparator + 'js';
 ASSETS_IMAGES = DirectorySeparator + 'images';
 ASSETS_FONTS  = DirectorySeparator + 'fonts';
 ASSETS_FILES  = DirectorySeparator + 'files';
 ASSETS_MEDIA  = DirectorySeparator + 'media';

 {Used by the dynamic page generator}
 ASSETS_WIDGET_BASE =   DirectorySeparator + 'rb-widgets';
 ASSETS_WIDGET_CSS =    ASSETS_WIDGET_BASE + DirectorySeparator + 'css';
 ASSETS_WIDGET_JS =     ASSETS_WIDGET_BASE + DirectorySeparator + 'js';
 ASSETS_WIDGET_PAGES  = ASSETS_WIDGET_BASE + DirectorySeparator + 'html';


procedure Init(_basePath: string);
function basePath: string;
function getAssetsPath (const _subPath: string): string;

{Factory functions}
function newStyle: THtmlStyle;
function newStyleSheet: THtmlStyleSheet;
function newJavaScriptFile: TJavaScripts;
function newTextFiler (_path: string  = ASSETS_BASE): TTextFiler;
function newWidgetTextFiler: TTextFiler;

implementation

var
  myBasePath: string;

function newTextFiler (_path: string = ASSETS_BASE): TTextFiler;
begin
  Result := TTextFiler.Create;
  Result.rootDir:= appendPath([basePath,_path]);
  Result.autorefresh:=true;
end;

function newWidgetTextFiler: TTextFiler;
begin
   Result := newTextFiler(ASSETS_WIDGET_BASE);
end;

function basePath: string;
begin
    Result:= myBasePath;
    if myBasePath.isEmpty then
        Result:= ExpandFileName(''); {get current directory}
        //trip('RbHtmlPackage.objectFactory.basePath(): Path is not defined. You have to set the path before using any file-write functions');

end;

function getAssetsPath(const _subPath: string): string;
begin
     Result := appendPath([basePath, ASSETS_BASE, _subPath]);
end;


procedure Init(_basePath: string);
begin
  myBasePath := _basePath;
end;

 function newStyle: THtmlStyle;
begin
  Result:= THtmlStyle.Create;
  Result.filePath := getAssetsPath(ASSETS_WIDGET_CSS);
  Result.webPath  := ASSETS_WIDGET_CSS;
end;

function newStyleSheet: THtmlStyleSheet;
begin
  Result := THtmlStyleSheet.Create;
  Result.filePath := getAssetsPath(ASSETS_WIDGET_CSS);
  Result.webPath  := ASSETS_WIDGET_CSS;
end;

function newJavaScriptFile: TJavaScripts;
begin
  Result := TJavaScripts.Create;
  Result.filePath:= AppendPath([basePath, ASSETS_WIDGET_JS ]);
end;

end.
