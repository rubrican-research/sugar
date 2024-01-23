unit sugar.webpage;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, sugar.collections, sugar.htmlbuilder, sugar.templater;


type

    { RbWebPage }

    {TODO: Create a streamlined specify JS Scripts for events. Create js file
    in assets folder. Do js packaging before serving the page}

    { Page is forced to refresh if a call to setElementText or getElement is
      made after the page has been rendered to html.

      If no call is made then the page is returned from cache.

    }

    RbWebPage = class
    private
        myName: string;
        myWebRoot: string;
        function getElement(_collection: THtmlCollection; _id: string): THtmlElement;
        function getElementText(_collection: THtmlCollection; _id: string): string;
        procedure setElementText(_collection: THtmlCollection; _id: string;
            _text: string);
        function getfields(_id: string): string;
        procedure setfields(_id: string; const _text: string);
        function getheader(id: string): string;
        procedure setheader(id: string; const _text: string);
        function getpageFooter(id: string): string;
        procedure setpageFooter(id: string; const _text: string);
        function getBodyElement(id: string): THtmlElement;
        function getHeadElement(id: string): THtmlElement;
        procedure setName(const _Name: string);

        function getFieldList(_collection: THtmlCollection;
            _prefix: string = DEFAULT_FIELD_PREFIX): string;

        function getRootDir: string;
        procedure setRootDir(const _RootDir: string);
        function getFilePath: string;
        function getName: string;
        procedure setWebRoot(const _WebRoot: string);
        function getWebPath: string;
		function getHeaderElement(id: string): THtmlElement;
		function getFooterElement(id: string): THtmlElement;
		function getPageBody(id: string): string;
		procedure setPageBody(id: string; const _pageBody: string);
		function getPageElement(id: string): THtmlElement;

    protected
        myRootDir: string;
        myHtmlDoc: THtmlDoc;
        myPageCache: string;
        myPreviewCache: string;
        myParent: RbWebPage;
        myShouldCache: boolean;

        function isPageCached: boolean;
        function isPreviewCached: boolean;
    public
        {sets the path from where page sections can be loaded from files}
        property RootDir: string read getRootDir write setRootDir;
        property WebRoot: string read myWebRoot write setWebRoot;
        property Name: string read getName write setName;
        property FilePath: string read getFilePath;
        property WebPath: string read getWebPath;

        {any field in Header, Body or Footer that starts with rbf-}
        property field[id: string]: string read getfields write setfields;

        {Access to elements by id. Replace text by assigning}
        property pageBody[id: string]: string read getPageBody write setPageBody;
        property pageHeader[id: string]: string read getheader write setheader;
        property pageFooter[id: string]: string read getpageFooter write setpageFooter;

        {Access to element Objects. }
        property element[id: string]: THtmlElement read getPageElement;
        property bodyElement[id: string]: THtmlElement read getBodyElement;
        property headerElement [id: string]: THtmlElement read getHeaderElement;
        property footerElement [id: string]: THtmlElement read getFooterElement;

        constructor Create; virtual;
        destructor Destroy; override;

        function cacheOn: RbWebPage;
        function cacheOff: RbWebPage;

        function css(_css: THtmlStyleSheet): RbWebPage; overload;
        function css: THtmlStyleSheet; overload;
        function cssCopy(_css: THtmlStyleSheet): RbWebPage;

        function linkCSSFile(const _cssPath: string): RbWebPage;
        {links to an existing JS File}
        function linkJSFile(const _jsName: string; const _createNew: boolean=false): RbWebPage;
        {links to a JS file. If not found, it creates it}
        function linkNewJSFile(const _jsName: string): RbWebPage;

        {includes the code in a script tag}
        function includeJSScript(const _jscode: string; _id: string = ''; _name: string = ''): RbWebPage;

        function parent(_parent: RbWebPage): RbWebPage;
        function parent: RbWebPage;

        function document: THtmlDoc; overload;
        function document(_template: THtmlDoc): RbWebPage; overload;

        function htmlHead: THtmlHead;
        function pageBodyObj: THtmlBody;
        function pageHeaderObj: THtmlCollection;
        function pageFooterObj: THtmlCollection;

        {Scripts}
        function preScript(const _script: string): THtmlScript;
        function postScript(const _script: string): THtmlScript;

        function preview: string; deprecated 'Preview function is not necessary anymore';
        function page: string; virtual;
        function showFields(_prefix: string = DEFAULT_FIELD_PREFIX): string;

        {empties the body part of the doc}
        procedure clearBody;

        procedure refresh(var msg); message 'Refresh Cache';
        function saveto(_path: string): boolean;

        {Makes a deep copy of the source}
        function copyFrom(_source: RbWebPage): RbWebPage; virtual;
        function clone: RbWebPage; virtual;

        {Create an return a parsed HTML page}
        function getCachedHtml: THtmlTemplater;
    end;

   {Defines an association between a page name and the builder function
     that returns and instance of RbWebpage}
    generic ViewDef<BuilderProc> = record
        Name: string;
        builder: BuilderProc;
    end;

function scriptObj(_jsPath: string=''): THtmlScript;

implementation

uses
    sugar.utils, sugar.htmlfactory, sugar.logger;

function scriptObj(_jsPath: string): THtmlScript;
begin
    Result := THTMLScript.Create;
    if not _jsPath.IsEmpty then
       Result.setSrc(_jsPath);
end;

{ RbWebPage }

function RbWebPage.getElement(_collection: THtmlCollection; _id: string): THtmlElement;
begin
    Result := _collection.get(_id);
    //if not Assigned(Result) then
    //begin
    //    raise Exception.Create(myName + ' ~ Template does not contain element:: ' + _id);
    //end;
end;

function RbWebPage.getFieldList(_collection: THtmlCollection; _prefix: string): string;
const
    F = '_page.fields[''%s''] := '''';</br>' + sLineBreak;
var
    i: integer;
    _field: string;
    _ids: TStrings;
begin
    Result := '';
    _ids := _collection.getIDs(_prefix);

    for i := 0 to _ids.Count - 1 do
         Result := Result + Format(F, [_ids[i]]);

    _ids.Free;
end;

function RbWebPage.getRootDir: string;
begin
    Result := myRootDir;
end;

procedure RbWebPage.setRootDir(const _RootDir: string);
begin
    myRootDir := _RootDir;
    myHtmlDoc.Scripts.filePath := appendPath([myRootDir, Name]);
end;

function RbWebPage.getFilePath: string;
begin
    Result := myHtmlDoc.Scripts.filePath;
end;

function RbWebPage.getName: string;
begin
    if myName.IsEmpty then
    begin
        Log('RbWebpage: Name has not been defined.');
        myName:= ClassName;
	end;
	Result := myName;
end;

procedure RbWebPage.setWebRoot(const _WebRoot: string);
begin
    myWebRoot := _WebRoot;
    myHtmlDoc.Scripts.webPath := appendPath([WebRoot, Name]);
end;

function RbWebPage.getWebPath: string;
begin
    Result := myHtmlDoc.Scripts.webPath;
end;

function RbWebPage.getFooterElement(id: string): THtmlElement;
begin
    Result := getElement(myHtmlDoc.Body.footer, id);
end;

function RbWebPage.getPageBody(id: string): string;
begin
    Result := getElementText(myHtmlDoc.Body, id);
end;

procedure RbWebPage.setPageBody(id: string; const _pageBody: string);
begin
    setElementText(myHtmlDoc.Body, id, _pageBody);
end;

function RbWebPage.getPageElement(id: string): THtmlElement;
begin
     Result := getElement(pageBodyObj,id);

    if not assigned(Result) then
        Result:= getElement(pageHeaderObj, id);

     if not assigned(Result) then
        Result:= getElement(pageFooterObj, id);

     if not assigned(Result) then
        log('getPageElement(): Element "%s" not found', [id]);
end;

function RbWebPage.getHeaderElement(id: string): THtmlElement;
begin
    Result := getElement(myHtmlDoc.Body.header, id);
end;

function RbWebPage.getElementText(_collection: THtmlCollection; _id: string): string;
var
    tmpElement: THtmlElement;
    err: string = '';
begin
    Result := '';
    tmpElement := _collection.get(_id);
    if Assigned(tmpElement) then
        Result := tmpElement.Text
    else
    begin
        err := 'RbWebPage.getElementText():: id not found:: ' + myName + '->' + _id;
        err := err + sLineBreak + sLineBreak;
        err := err + 'USE THESE FIELDS' + sLineBreak;
        err := err + getFieldList(_collection);
        raise Exception.Create(err);
    end;

end;

procedure RbWebPage.setElementText(_collection: THtmlCollection;
    _id: string; _text: string);
var
    r: integer = 0;
    tmpElement: THtmlElement;
    err: string = '';
begin
    tmpElement := _collection.get(_id);

    if Assigned(tmpElement) then
    begin
        tmpElement.Text := _text;
        refresh(r);
    end
    else
    begin
        err := 'RbWebPage.setElementText():: id not found:: ' + myName + '->' + _id;
        err := err + sLineBreak + 'Available fields:' +
            sLineBreak + getFieldList(_collection);
        log(err);
        // raise Exception.Create(err);
    end;
end;

function RbWebPage.getfields(_id: string): string;
var
    el : THtmlElement;
begin
    Result:= '';
    el := pageBodyObj.get(_id);

    if not Assigned(el) then
          el := pageHeaderObj.get(_id);

    if not Assigned(el) then
           el:= pageFooterObj.get(_id);

    if assigned(el) then
        Result:= el.Text
     else
          log('getfields(): Not found :' + _id);
end;

procedure RbWebPage.setfields(_id: string; const _text: string);
var
    el : THtmlElement;
begin
     el := pageBodyObj.get(_id);

     if not Assigned(el) then
          el := pageHeaderObj.get(_id);

     if not Assigned(el) then
           el:= pageFooterObj.get(_id);

     if assigned(el) then
        el.Text := _text

     else
          log('setfields(): Not found :' + _id);
end;

function RbWebPage.getheader(id: string): string;
begin
    Result := getElementText(myHtmlDoc.Body.header, id);
end;

procedure RbWebPage.setheader(id: string; const _text: string);
begin
    setElementText(myHtmlDoc.Body.header, id, _text);
end;

function RbWebPage.getpageFooter(id: string): string;
begin
    Result := getElementText(myHtmlDoc.body.footer, id);
end;

procedure RbWebPage.setpageFooter(id: string; const _text: string);
begin
    setElementText(myHtmlDoc.body.footer, id, _text);
end;

function RbWebPage.getBodyElement(id: string): THtmlElement;
begin
    Result := getElement(myHtmlDoc.Body, id);
end;

function RbWebPage.getHeadElement(id: string): THtmlElement;
begin
    Result := getElement(myHtmlDoc.Body.Header, id);
end;

procedure RbWebPage.setName(const _Name: string);
begin
    myName := _Name;
    myHtmlDoc.Name:= myName;
end;


function RbWebPage.isPageCached: boolean;
begin
    if myShouldCache then
        Result := (myPageCache.Length > 0)
    else
        Result := False;
end;

function RbWebPage.isPreviewCached: boolean;
begin
    if myShouldCache then
        Result := (myPreviewCache.Length > 0)
    else
        Result := False;
end;


constructor RbWebPage.Create;
begin
    myHtmlDoc := THtmlDoc.Create;
    myHtmlDoc.Language := 'en';
    myPageCache := '';
    myPreviewCache := '';
    myShouldCache := True; // sets the cache function. Default - cache is on
end;

destructor RbWebPage.Destroy;
begin
    FreeAndNil(myHtmlDoc);
    inherited Destroy;
end;

function RbWebPage.cacheOn: RbWebPage;
begin
    myShouldCache := True;
    Result := Self;
end;

function RbWebPage.cacheOff: RbWebPage;
begin
    myShouldCache := False;
    Result := Self;
end;

function RbWebPage.css(_css: THtmlStyleSheet): RbWebPage;
begin
    myHtmlDoc.Styles := _css;
    Result := self;
end;

function RbWebPage.cssCopy(_css: THtmlStyleSheet): RbWebPage;
begin
    myHtmlDoc.Styles.import(_css, True);
    Result := Self;
end;

function RbWebPage.linkCSSFile(const _cssPath: string): RbWebPage;
begin
    raise Exception.Create('UseCSS not implemented');
end;

{Creates a new JSFile and then includes it}
function RbWebPage.linkNewJSFile(const _jsName: string): RbWebPage;
begin
    Result := linkJSFile(_jsName, true);
end;

function RbWebPage.includeJSScript(const _jscode: string; _id: string;
	_name: string): RbWebPage;
begin
    with document.Body.ending.add(scriptObj()) do
    begin
        if not _id.isEmpty then
            setId(_id);
        Name:= _name;
        Text:=_jscode;
	end;
	Result := self;
end;

{Includes an existing JSFile }
function RbWebPage.linkJSFile(const _jsName: string; const _createNew: boolean
	): RbWebPage;
var
    _script: TJavaScript;
begin
    if not Assigned(myHtmlDoc.Scripts.find(_jsName)) then
    begin
	    _script := myHtmlDoc.Scripts.get(_jsName);
        if _createNew then
	        _script.touch; {creates a new file if not exists}
	    document.Body.ending.add(scriptObj(_script.url));
	end;
	Result := self;
end;

function RbWebPage.css: THtmlStyleSheet;
begin
    Result := myHtmlDoc.Styles;
end;

function RbWebPage.parent(_parent: RbWebPage): RbWebPage;
begin
    if myParent <> _parent then
        myParent := _parent;
    Result := Self;
end;

function RbWebPage.parent: RbWebPage;
begin
    Result := myParent;
end;

function RbWebPage.document: THtmlDoc;
begin
    Result := myHtmlDoc;
end;

function RbWebPage.document(_template: THtmlDoc): RbWebPage;
begin
    if Assigned(myHtmlDoc) then
        FreeAndNil(myHtmlDoc);

    myHtmlDoc := _template;
    Result := Self;
end;


function RbWebPage.htmlHead: THtmlHead;
begin
     Result := myHtmlDoc.Head;
end;

function RbWebPage.pageBodyObj: THtmlBody;
begin
     Result := myHtmlDoc.Body;
end;

function RbWebPage.pageHeaderObj: THtmlCollection;
begin
     Result := pageBodyObj.header;
end;

function RbWebPage.pageFooterObj: THtmlCollection;
begin
     Result := pageBodyObj.footer;
end;

function RbWebPage.preScript(const _script: string): THtmlScript;
begin
    Result := THTMLScript.Create;
    Result.Text := _script;
    myHtmlDoc.Head.add(Result);
end;

function RbWebPage.postScript(const _script: string): THtmlScript;
begin
    Result := THTMLScript.Create;
    Result.Text := _script;
    pageBodyObj.Ending.Add(Result);
end;

function RbWebPage.page: string;
begin
    {check the cache. If the cache hasn't changed then return pre-built page}
    if not isPageCached then
    begin
        {first generate the javascript file}

        {link to the generated javascrit file}

        {render html}
        myPageCache := document.html;
    end;
    Result := myPageCache;
end;

function RbWebPage.showFields(_prefix: string): string;
begin
    Result := getFieldList(document.Body);
end;

procedure RbWebPage.clearBody;
begin
    myPageCache := '';
    myPreviewCache := '';
    document.Body.Clear;
end;

procedure RbWebPage.refresh(var msg);
begin
    {Re-render the page when page() or preview() is called}
    if not myPageCache.isEmpty then
        myPageCache := '';

    if not myPreviewCache.isEmpty then
        myPreviewCache := '';
end;

function RbWebPage.saveto(_path: string): boolean;
var
    f: TStringList;
begin
    f := TStringList.Create;
    try
        f.Text := page;
        f.SaveToFile(_path);
    finally
        FreeAndNil(f);
    end;
    Result := True;
end;

function RbWebPage.copyFrom(_source: RbWebPage): RbWebPage;
begin
    self.myName := _source.myName;
    self.myHtmlDoc := _source.myHtmlDoc.clone;
    self.myParent := _source.myParent;
    self.myShouldCache := _source.myShouldCache;
    Result := Self;
end;

function RbWebPage.clone: RbWebPage;
begin
    Result := RbWebPage.Create;
    Result.copyFrom(self);
end;

function RbWebPage.getCachedHtml: THtmlTemplater;
begin
     Result := THtmlTemplater.Create;
     Result.template := page;
end;

function RbWebPage.preview: string;
begin
    if not isPreviewCached then
    begin
        myPreviewCache := myHtmlDoc.html;
    end;
    Result := myPreviewCache;
end;

end.
