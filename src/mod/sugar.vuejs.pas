unit sugar.vuejs;

{Wrapper for Vuejs. Version 2.x}

{$mode objfpc}{$H+}
{$macro ON}
{$DEFINE _:=+}

interface

uses
    Classes, SysUtils, sugar.collections, fgl, fpjson, jsonparser, sugar.htmlbuilder, sugar.jsonbuilder;

const
  {Object member names of the payload JSON Object. See rb.min.js -> serialize}
  __payload      = 'payload';
  __payload_name = 'name';
  __payload_data = 'data';      {object name in the serialized JSON payload}


type
	{ RbVuePropertyData }

    RbVuePropertyData = class
    protected
        myJSONObject: TJSONObject;
        function defaultRenderer(_name: string): string;

    public
        property json: TJSONObject read myJSONObject;
        constructor Create;
        destructor Destroy; override;
        function render: string; virtual; abstract;
        function loadFromFile(_fileName: string): boolean; virtual;
        function propertyName: string; virtual; abstract;

        {Imports data from passed parameter.
        New values are added. Existing values... not sure yet??}
        function import(_sourceJSON: TJSONObject; _freeObject: boolean = false) : RbVuePropertyData;

        function setData(_data: TJSONObject): RbVuePropertyData; virtual;
        {syntax sugar to add JSON values}
        function Add(const AName: TJSONStringType; AValue: TJSONData): RbVuePropertyData; overload; virtual;
        function Add(const AName: TJSONStringType; AValue: Boolean): RbVuePropertyData; overload; virtual;
        function Add(const AName: TJSONStringType; AValue: TJSONFloat): RbVuePropertyData; overload; virtual;
        function Add(const AName, AValue: TJSONStringType): RbVuePropertyData; overload; virtual;
        function Add(const AName : String; AValue: TJSONUnicodeStringType): RbVuePropertyData; overload; virtual;
        function Add(const AName: TJSONStringType; Avalue: Integer): RbVuePropertyData; overload; virtual;
        function Add(const AName: TJSONStringType; Avalue: Int64): RbVuePropertyData; overload; virtual;
        function Add(const AName: TJSONStringType; Avalue: QWord): RbVuePropertyData; overload; virtual;
        function Add(const AName: TJSONStringType): RbVuePropertyData; overload; virtual;
        function Add(const AName: TJSONStringType; AValue : TJSONArray): RbVuePropertyData; overload; virtual;
        function Code(const AName: TJSONStringType; AValue : string): RbVuePropertyData; virtual; {Adds an unquoted value member}
    end;

    RbVueApp = class;
    RbVueAppList = class;
    RbVueComponent = class;
    RbVueLifecycleMethods = class;

    RbVueData = class;
    RbVueMethodList = class;
    RbVueMethods = class;
    RbVueProps = class;
    RbVueSlots = class;
    RbVueScopedSlots = class;
    RbVueRefs = class;
    RbVueComputed = class;
    RbVueDirectives = class;
    RbVueWatch=class;
    RbVueFilters=class;
    RbVueModel=class;
    RbVueMounted = class;
    RbVueCreated = class;
    RbVueUpdated = class;
    RbVueDestroyed = class;

    { RbVueBase }

const
    VUEAPP_PROP_COUNT = 15;

type
    RbVueBase = class
    protected
    const
        INDENT = '    ';
    protected
        myProperties: array[0..VUEAPP_PROP_COUNT-1] of RbVuePropertyData; {Total 12}
        myName: string;
        mychildren: RbVueAppList;
        myroot:         RbVueBase;

        mymethods:      RbVueMethods;
        mymodel:        RbVueModel;
        mydata:         RbVueData;
        myprops:        RbVueProps;
        mycomputed:     RbVueComputed;
        myslots:        RbVueSlots;
        myscopedSlots:  RbVueScopedSlots;
        myrefs:         RbVueRefs;
        mydirectives:   RbVueDirectives;
        mywatch:        RbVueWatch;
        myfilters:      RbVueFilters;
        mymounted:      RbVueMounted;
        myCreated:      RbVueCreated;
        myUpdated:      RbVueUpdated;
        myDestroyed:    RbVueDestroyed;

    protected
        function renderProperties: string;
    public
        function root: RbVueBase; overload;
        function setRoot(_root: RbVueBase): RbVueBase; overload;

        {Vuew Object name}
        function Name(_Name: string): RbVueBase;
        function Name: string; overload;

        function data: RbVueData;

        {set this object as data object. Free the previous object}
        function setData(_obj: TJSONObject): RbVueBase;

        function methods: RbVueMethods;
        function props: RbVueProps;
        function computed: RbVueComputed;
        function children: RbVueAppList;
        function directives: RbVueDirectives;
        function slots: RbVueSlots;
        function scopedSlots: RbVueScopedSlots;
        function refs: RbVueRefs;
        function watch: RbVueWatch;
        function filters: RbVueFilters;
        function mounted: RbVueMounted;
        function created: RbVueCreated;
        function updated: RbVueUpdated;
        function destroyed: RbVueDestroyed;

        constructor Create; virtual;
        destructor Destroy; override;

        function code: string; virtual; abstract;
        function asJSONObjectMember: string; virtual;
        function asJSONObject: TJSONObject; virtual;
    class function CDNLinks: THtmlElementArray;
    end;

    { RbVueApp }

    {This class requires functions defined in rb.min.js}
    {See end of file for the required JS code}
    RbVueApp = class(RbVueBase)
	    private type RbVapList = specialize TFPGObjectList<RbVueApp>;
    protected
        myElement: string;
        myNestedVaps: RbVapList;
		function renderObj: string; // Returns a JSON Object;
        function render(_varFormatting: string): string;
        function fnameSeralizer: string;
        function funcSerializeThis: string; virtual;
        procedure setSerializeMethod;
        function funcSaveMethodBody (_url: string; _onSuccess: string; _onError: string): string;
        function funcDeleteMethodBody(_url: string; _payload: string; _onSuccess: string; _onError: string): string;
        function funcCancelMethodBody(_url: string; _payload: string; _onSuccess: string; _onError: string): string;
        function funcRestfulMethodBody(_url: string; _method: string; _payload: string; _onSuccess: string; _onError: string): string;
    public
        function element(const _el: string): RbVueBase; overload;
        function element: string; overload;
        function code: string; override;
        function asJSONObjectMember: string; override;
        function importData(_json: TJSONObject): RbVueApp;

        {Generates a JS method that serializes the data in this Instance.
            You can have multiple save methods with different names.
            onSuccess is code for a JS function to be called after the server returns success
            onError is code for a JS function to be called after the server returns success}
        function saveMethod(_name: string; _url: string; _onSuccess: string='' ; _onError: string=''): RbVueApp;

        {Generates a JS method that sends the "_payload" parameter to the server with a DELETE method
            You can have multiple save methods with different names.
            onSuccess is code for a JS function to be called after the server returns success
            onError is code for a JS function to be called after the server returns success}

        function deleteMethod(_name: string; _url: string; _payload: string; _onSuccess: string='' ; _onError: string=''): RbVueApp;

        { Generates a JS method that POSTs the url.
          The action taken is to cancel the current edit function and release locks}
        function cancelMethod(_name: string; _url: string; _payload: string; _onSuccessURL: string='' ; _onError: string=''): RbVueApp;

        {Generates a general JS Method that calls a url with the specified method }
        function restfulCallMethod(_name: string; _method: string; _url: string; _payload: string; _onSuccess: string='' ; _onError: string=''): RbVueApp;

        constructor Create; override;
        constructor New(_name: string; _element: string); virtual;
        destructor Destroy; override;

        {Nesting Vue Apps}
        procedure add(_name: string; _vap: RbVueApp); overload;
    end;

    { RbVueComponent }

    RbVueComponent = class(RbVueBase)
    protected
        myTemplate: THtmlCollection;
    public
        constructor Create; override; overload;
        constructor Create(_name: string); overload;

        function getTemplate: THtmlCollection;
        function code: string; override;
    public
        property template: THtmlCollection read getTemplate write mytemplate;
    end;

	{ RbVueData }

    RbVueData = class(RbVuePropertyData)
	    function render: string; override;
        public const name = 'data';
	    function propertyName: string; override;
    end;

    { RbVueMethodList }

    RbVueMethodList = class(RbVuePropertyData)
		function render: string; override;
	private
		function getmethod(_name: string): string;
		procedure setmethod(_name: string; const _method: string);
    public
        property method[_name: string]: string read getmethod write setmethod; default;
        function add(_name: string; _code: string): RbVueMethodList;
    end;

    RbVueMethods = class(RbVueMethodList)
    public const name = 'methods';
	    function propertyName: string; override;

	end;


	{ RbVueProps }

    RbVueProps = class(RbVuePropertyData)
		function render: string; override;
        public const name = 'props';
	    function propertyName: string; override;
    end;

    RbVueAppListBase = specialize GenericHashObjectList<RbVueApp>;
    RbVueAppList = class(RbVueAppListBase)

    end;

	{ RbVueSlots }

    RbVueSlots = class(RbVuePropertyData)
		function render: string; override;
    public const name = 'slots';
	function propertyName: string; override;
    end;

	{ RbVueScopedSlots }

    RbVueScopedSlots = class(RbVuePropertyData)
		function render: string; override;
    public const name = 'scopedslots';
	function propertyName: string; override;
    end;

	{ RbVueRefs }

    RbVueRefs = class(RbVuePropertyData)
		function render: string; override;
    public const name = 'refs';
	function propertyName: string; override;
    end;

	{ RbVueComputed }

    RbVueComputed = class(RbVueMethodList)
		function render: string; override;
        public const name = 'computed';
	    function propertyName: string; override;
    end;

	{ RbVueDirectives }

    RbVueDirectives = class(RbVuePropertyData)
		function render: string; override;
    public const name = 'directives';
	function propertyName: string; override;
    end;

	{ RbVueWatch }

    RbVueWatch = class(RbVuePropertyData)
		function render: string; override;
    public const name = 'watch';
	function propertyName: string; override;
    end;

	{ RbVueFilters }

    RbVueFilters = class(RbVuePropertyData)
		function render: string; override;
        public const name = 'filters';
	    function propertyName: string; override;
    end;

	{ RbVueModel }

    RbVueModel = class(RbVuePropertyData)
		function render: string; override;
        public const name = 'model';
	    function propertyName: string; override;
    end;

	{ RbVueLifecycleMethods }


    RbVueLifecycleMethods = class(RbVuePropertyData)
    private
        property json;
		function getCode: string;
		procedure setCode(const _code: string);
    protected
		function render: string; override;
        function getName: string; virtual; abstract;
    public
        property name: string read getName;
        property code: string read getCode write setCode;
        function loadFromFile(_fileName: string): boolean; override;
		function propertyName: string; override;
    end;

	{ RbVueMounted }

    RbVueMounted = class(RbVueLifecycleMethods)
        private const myName = 'mounted';
		function getName: string; override;
    end;

	{ RbVueCreated }

    RbVueCreated =class(RbVueLifecycleMethods)
        private const myName = 'created';
	    function getName: string; override;
	end;

	{ RbVueUpdated }
    RbVueUpdated =class(RbVueLifecycleMethods)
        private const myName = 'updated';
	    function getName: string; override;
	end;

	{ RbVueDestroyed }

    RbVueDestroyed =class(RbVueLifecycleMethods)
        private const myName = 'destroyed';
	    function getName: string; override;
	end;

{General purpose Vue Apps}
{ RB VUE DYNA PANEL - USAGE
This is a general purpose add / delete ui panel that can be customized to use a specific JSON Object.
Things to remember.
_listName = is the name of the array that holds the objects in the browser. Remember that this name
must match the v-for definition "object in "listName"
_objectName is used to denote a specific object in the list. This name is used as the parameter
name for function to remove the panel from the browser.
REMEMBER -
NEW OBJECT FUNCTION NAME: the add button must use the function name of the format -
"newXXX" where XXX is exactly the value in _objectName.
ADD OBJECT FUNCTION NAME: the add button must use the function name of the format -
"addXXX" where XXX is exactly the value in _objectName.
REMOVE OBJECT FUNCTION NAME: is of the format
"rmXXX" where XXX is exactly the value in _objectName
REPOSITION OBJECT FUNCTION NAME: is of the format
"reposXXX" where XXX is exactly the value in _objectName
_refName is the name that will be used to scroll the panel into view, which is implemented in the
addObject() function.
REMEMBER = You have to specify the vRef() for an input value to match this name exactly.
!!uidx!!
FINALLY. The v-bind:key value must be of the format "_objectName".uidx - this is very important.
}

type
	{ RbVueDynaPanel }

    RbVueDynaPanel = class (RbVueApp)
    protected
        isBuilt: boolean;
        listData: TJSONArray;
        defaultDynaModel: TJSONObject;
        autoFreeDefaultModel: boolean;
        autoFreeList: boolean;

        procedure build;

    public const indexName = 'uidx';

    public
        listName: string;
        objectName: string;
        refName: string;

        {These functions generate JS Code to add, push, remove and reposition objects.
        They call JS functions that are in rb.min.js}
        function funcNewObject(_object: TJSONObject): string; overload;
        function funcNewObject: string; overload;

        function funcPushNewObject(_listName:string; _fnNameNewObject: string; _refName: string): string; overload;
        function funcPushNewObject: string; overload;

        function funcRemoveObject(_listName: string; _objectName: string): string; overload;
        function funcRemoveObject: string;overload;

        function funcRepositionObject(_listName: string; _objectName: string; _refName: string): string; overload;
        function funcRepositionObject: string; overload;


        {default function names}
        function fnameNewObject(_objectName: string): string; overload;
        function fnameNewObject: string; overload;

        function fnameAddObject(_objectName: string): string; overload;
        function fnameAddObject: string; overload;

        function fnameRemoveObject(_objectName: string): string; overload;
        function fnameRemoveObject: string; overload;

		function fnameReposition(_objectName: string): string; overload;
		function fnameReposition: string; overload;

        function fnameSeralizer: string;

        {defines the JSON Object that this dynaPanel is aware of and uses to store in the list}
        function setDefaultDynaModel(_model: TJSONObject; _autoFree: boolean = true): RbVueDynaPanel;
        procedure clearDefaultDynaModel;
        {use this to define the model directly only after you have called setDefaultDynaModel first}
        function dynaModel: TJSONObject; overload;
        function dynaModel(_listName: string): TJSONObject; overload;
        function dynaModel(_objectName: string; _listName: string): TJSONObject; overload;

        {Preload the list array with the supplied array}
        function setListData(_dataArr: TJSONArray; _autoFree: boolean = true): RbVueDynaPanel; overload;
        function setListData(_listName: string; _dataArr: TJSONArray; _autoFree: boolean = true): RbVueDynaPanel; overload;
        function getListData: TJSONArray;

        function code: string; override;
        function asJSONObjectMember: string; override;
        function asJSONObject: TJSONObject; override;
        function asVueApp: RbVueApp;

        constructor New(_name: string; _element: string); override;
        destructor Destroy; override;
	end;

    RbVueMultiPanelListBase = specialize GenericHashObjectList<RbVueDynaPanel>;

	{ RbVueMultiDynaPanel }

    {contains multiple dynaPanels sharing one data object}
    RbVueMultiDynaPanel = class(RbVueMultiPanelListBase)
    protected
         myIsInitialized: boolean;
         procedure init; virtual; {override this to initialize this object}

    public
        name: string;
        element: string;
        function panel(_name: string): RbVueDynaPanel;

        {generates a set of vueApp instances, one for each panel}
        function code_separate: string;

        {generates a JS Object that contains each panel as member vueApp instances}
        function asJSVariable: string;

        {This generates a vue AppObject that contains the elements of each panel merged into the main VueApp}
        function vueAppObj: RbVueApp;

        {Generates a JS function that returns a single js object which contains references to all the
        panel variables in the browswer. This object is serialized to JSON when you call the asJSON() method.
        The resulting string can be send to the server}
        function serializer: string;

        {import() loads a vueApp object:: IMPORTS A JSON THAT WAS GENERATED THROUGH THE serializer JS function}
        function import(_panelData: TJSONArray; _freeData: boolean=true
			): RbVueMultiDynaPanel;

        constructor New(const _name: string; const _element: string); virtual;
    end;

{Functions to generate form submit, delete JS methods}
function JSFormSubmit(_url: string; _payloadBody: string; _method: string = 'POST'): string;
function JSFormDelete(_url: string; _redirectTo:string): string;



implementation

uses
    sugar.utils, sugar.jshelpers, sugar.jsonlib, sugar.logger;

function JSFormSubmit(_url: string; _payloadBody: string; _method: string
	): string;
begin
    Result:= 'JSFormSubmit not implemented';
end;

function JSFormDelete(_url: string; _redirectTo: string): string;
begin
    Result:= 'JSFormDelete not implemented';
end;

{ RbVueMethods }

function RbVueMethods.propertyName: string;
begin
    Result:= name;
end;


{ RbVueDestroyed }

function RbVueDestroyed.getName: string;
begin
    Result:= myName;
end;

{ RbVueUpdated }

function RbVueUpdated.getName: string;
begin
    Result:= myName;
end;

{ RbVueCreated }

function RbVueCreated.getName: string;
begin
    Result:= myName;
end;

{ RbVueMounted }

function RbVueMounted.getName: string;
begin
    Result:= myName;
end;

{ RbVueMultiDynaPanel }

function RbVueMultiDynaPanel.panel(_name: string): RbVueDynaPanel;
begin
    result:= get(_name);
    if result.Name.isEmpty then Result.Name(_name);
end;

function RbVueMultiDynaPanel.code_separate: string;
var
    i: integer;
begin
    if not myIsInitialized then Init();
    Result:= '';
    for i := 0 to pred(Count) do
        Result:= Result + items[i].code;
end;


function RbVueMultiDynaPanel.asJSVariable: string;
const
  vap_template = 'var %s = (function(){return{%s}})();';
var
    i: integer;
    _code: string = '';
    addComma: boolean = false;
begin
    if not myIsInitialized then Init();
    for i := 0 to pred(Count) do
    begin
        if addComma then
            _code := _code + ','
        else
            addComma := true;
        _code := _code + Items[i].asJSONObjectMember;
    end;
    Result:= Format(vap_template, [name, _code]);
end;

{This generates a vue AppObject that contains the elements of each panel merged into the main VueApp}
function RbVueMultiDynaPanel.vueAppObj: RbVueApp;
var
    dynaPanel: RbVueDynaPanel;
    i: integer;
    d: integer;
    myVProp: RbVuePropertyData;
    _listVProp: RbVuePropertyData;
begin
    if not myIsInitialized then Init();
    Result:= RbVueApp.New(name, element);
    for i := 0 to pred(Count) do
    begin
        dynaPanel:= Items[i];
        dynaPanel.build;
        for d := 0 to pred(VUEAPP_PROP_COUNT) do
        begin
            {this works because the array has identical sequence of props}
            // log('importing: ' +dynaPanel.myProperties[d].ClassName);
            Result.myProperties[d].import(dynaPanel.myProperties[d].json);
		end;
	end;
    {---------------------------------------------------------------------------------------------------------}
    {## THIS IS IMPORTANT  ##}
    {---------------------------------------------------------------------------------------------------------}
    {When you import vueApp objects, the default serialize method gets overwritten
    by the serialize function of the last imported object. You HAVE to set it to serialize this
    APP object correctly. It is just too tedious right now to ignore the serialize function during import:
    Stanley - 14 Oct 2020}
    Result.setSerializeMethod;

end;


{---------------------------------------------------------------------------------------------------------}
{## SECTION:: SERIALIZING a MultiDynaPanel in the browser ##}
{---------------------------------------------------------------------------------------------------------}
(*This function generates JavaScript code that serializes the MultiDynaPanel.
USAGE::
*******
*   This function outputs a string that is valid Javascript code which instantiates an object.
*   The contents of the panels array are actual variables that have been previously instantiated
*   when this Multipanel's code was executed in the browser.
*
*   EXAMPLE:
*       var subjectMultiPanel_list = {
*           name: "subjectMultiPanel",
*           el: "rbf_contents",
*           panels: [panelMain, panelStandards, panelObjectives, panelSkills, panelDispositions],
*           asJSON: function () {
*               // This contains code that loops through each item in this.panels and
*               // appends the serialized JSON data to the output
*               // When you click the submit button call this function, assign it to a string variable
*               // and send contents of the variable as the payload to the server.
*           }
*       }
*)
function RbVueMultiDynaPanel.serializer: string;
const
  var_template = 'var %s_list = %s;';
var
    i: integer;
    _vars: string = '';
    _bAddComma: boolean = false;
    _jObj: TJSONObject;
begin
    if not myIsInitialized then Init();
    {Create a comma separated list of panel names, which are actually js vars in the browser}
	for i:=0 to pred(Count) do
	begin
	    if _bAddComma then
	        _vars:= _vars + ','
	    else
	        _bAddComma := true;
	    _vars:= _vars + Items[i].Name;
	end;

	_vars:= '['+ _vars + ']';
	_jObj:= TJSONObject.Create;
	with _jObj do
	begin
	    add('name', name);
	    add('el',   element);
	    add('panels', TJSONUnquotedString.Create(_vars)); {To create an array of data variables you make sure that there are no quotes}
	    add('asJSON',TJSONUnquotedString.Create(
	        'function() {' +
	        '   var result="[";' +
	        '   this.panels.forEach(function (value, index, array) {' +
	        '       if (index>0) {result+= ","}' +
	        '       result += value.serialize();' +
	        '   });' +
	        '   result = result + "]";' +
	        '   return result;' +
	        '}'));
	end;

	Result:= Format(var_template, [name, _jObj.AsJSON]);
	_jObj.Free;

end;

function RbVueMultiDynaPanel.import(_panelData: TJSONArray; _freeData: boolean
	): RbVueMultiDynaPanel;
var
    _jobj: TJSONObject;
	i: Integer;
begin
    Result:= self;
    if assigned(_panelData) then
    begin
        // Log('loading panel data');
        for i:= 0 to pred(_panelData.Count) do
        begin
            if _panelData.Items[i].JSONType = jtObject then
            begin
                _jobj:= _panelData.Items[i] as TJSONObject;
                // log('IMPORTING PANEL:: ' + _jobj.Strings['panel']);
                try
                    panel(_jobj.Strings['panel']).importData(_jobj.Objects['data']);
				except
                    on E:Exception do
                        log('RbVueNestedDynaPanel.import: >> ' + _jobj.Strings['panel'] +  ' || ' + E.Message);
				end;
			end;
		end;
        if _freeData then
            _panelData.Free; {because import makes a clone}
	end;
    // log('done importing panel data');
end;

procedure RbVueMultiDynaPanel.init;
begin
    myIsInitialized:= true;
end;

constructor RbVueMultiDynaPanel.New(const _name: string; const _element: string
	);
begin
    Create();
    name:= _name;
    element:= _element;
    myIsInitialized:= false;
end;

{ RbVueModel }

function RbVueModel.render: string;
begin
    Result:= defaultRenderer(name);
end;

function RbVueModel.propertyName: string;
begin
    Result:= name;
end;


{ RbVueDynaPanel }

function RbVueDynaPanel.funcPushNewObject(_listName: string;
	_fnNameNewObject: string; _refName: string): string;
begin
    {increment uidx. push a new object onto the list. On nextTick() scroll into view.}
    Result:= Format(
    'this.$dynaPanelDef.objectAdd(' _
        'this,' _           {context of "this}
        'this.%s,' _        {array of objects}
        'this.%s(),' _      {new object}
        '{refname: "%s"}' _ {refname to get the element}
    ')', [_listName, _fnNameNewObject, _refName]);
    Result:= jsFunc('evnt', Result);
end;

function RbVueDynaPanel.funcPushNewObject: string;
begin
    Result:= funcPushNewObject(listName, fnameNewObject, refName);
end;

function RbVueDynaPanel.funcRemoveObject(_listName: string; _objectName: string
	): string;
begin
    Result:= jsFunc(_objectName,
    Format('this.$dynaPanelDef.objectRemove(this.%s, %s)', [_listName, _objectName]));
end;

function RbVueDynaPanel.funcRemoveObject: string;
begin
    Result:= funcRemoveObject(listName, objectName);
end;

function RbVueDynaPanel.funcRepositionObject(_listName: string;
	_objectName: string; _refName: string): string;
begin
    Result:= jsFunc(_objectName + ', delta',
    Format('this.$dynaPanelDef.reposition(' _
    'this, ' _          {context}
    'this.%s, ' _       {listName}
    '%s, ' _            {objectName}
    'delta, ' _         {1 for down, -1 for up}
    '{refname: "%s"});',{refname to reposition}
    [_listName, _objectName, _refName]));
end;

function RbVueDynaPanel.funcRepositionObject: string;
begin
    Result:= funcRepositionObject(listName, objectName, refName);
end;

//function RbVueDynaPanel.funcSerializeThis: string;
//begin
//    Result:= jsFunc('','' +
//    '   return this.$dynaPanelDef.serialize(' +
//    '       "' + Name +'",' +
//    '       this.$data' +
//    '   );');
//end;

procedure RbVueDynaPanel.build;
begin
    if isBuilt then exit;

    {-- VALIDATIONS --}
    if listName.isEmpty then trip('RbVueDynaPanel: listName is empty');
    if objectName.isEmpty then trip('RbVueDynaPanel: objectName is empty');
    if (myNestedVaps.Count = 0) and (not Assigned(defaultDynaModel)) then trip('RbVueDynaPanel: defaultDynaModel is not assigned');

    {setData - adding the list}
    if not assigned(data.json.find(listName)) then
    begin
        if Assigned(listData) then
        begin
            if autoFreeList then
                data.json.Add(listName, listData)
            else
                data.json.Add(listName, listData.Clone);
        end
        else
            {- if there is not member with the list name, create an array default.}
            data.json.Add(listName, TJSONArray.Create());
	end;

    {- all good here - }

    {METHODS --}
    {newObject}
    methods.method[fnameNewObject]:=funcNewObject;
    {addObject}
    methods.method[fnameAddObject]:=funcPushNewObject;
    {rmObject}
    methods.method[fnameRemoveObject]:=funcRemoveObject;
    {reposObject}
    methods.method[fnameReposition]:=funcRepositionObject;
    {serialize}
    setSerializeMethod;

    isBuilt:= true;
end;

function RbVueDynaPanel.fnameNewObject(_objectName: string): string;
begin
    Result := 'new' + _objectName;
end;

function RbVueDynaPanel.fnameNewObject: string;
begin
    Result:= fnameNewObject(objectName);
end;

function RbVueDynaPanel.fnameAddObject(_objectName: string): string;
begin
    Result:= 'add' + _objectName;
end;

function RbVueDynaPanel.fnameAddObject: string;
begin
    Result:= fnameAddObject(objectName);
end;

function RbVueDynaPanel.fnameRemoveObject(_objectName: string): string;
begin
    Result:= 'rm' + _objectName;
end;

function RbVueDynaPanel.fnameRemoveObject: string;
begin
    Result:= fnameRemoveObject(objectName);
end;

function RbVueDynaPanel.fnameReposition(_objectName: string): string;
begin
    Result:= 'repos' + _objectName;
end;

function RbVueDynaPanel.fnameReposition: string;
begin
    Result:= fnameReposition(objectName);
end;

function RbVueDynaPanel.fnameSeralizer: string;
begin
    result:= 'serialize';
end;

function RbVueDynaPanel.funcNewObject(_object: TJSONObject): string;
begin
    {Allows you to add default values into the data}
    if Assigned(_object) then
	    Result:= jsFunc('', Format('return (%s);', [_object.AsJSON]))
    else
        Result:= jsFunc('', 'return ({});')
end;

function RbVueDynaPanel.funcNewObject: string;
begin
    Result:= funcNewObject(dynaModel);
end;

function RbVueDynaPanel.dynaModel: TJSONObject;
begin
    Result:= defaultDynaModel;
end;

function RbVueDynaPanel.dynaModel(_listName: string): TJSONObject;
begin
    listName:= _listName;
    setDefaultDynaModel(TJSONObject.Create);
    Result:= defaultDynaModel;
    if isBuilt then isBuilt:= false;
end;

function RbVueDynaPanel.dynaModel(_objectName: string; _listName: string
	): TJSONObject;
begin
    objectName:= _objectName;
    Result:= dynaModel(_listName);
end;

function RbVueDynaPanel.setDefaultDynaModel(_model: TJSONObject; _autoFree: boolean
	): RbVueDynaPanel;
begin
    clearDefaultDynaModel;
    defaultDynaModel:= _model;
    autoFreeDefaultModel:= _autoFree;
    if isBuilt then isBuilt:= false;
	Result:= self;
end;

procedure RbVueDynaPanel.clearDefaultDynaModel;
begin
    if autoFreeDefaultModel then
    begin
        if Assigned(defaultDynaModel) then
            defaultDynaModel.Free;
	end;
    defaultDynaModel:= nil;
    if isBuilt then isBuilt:= false;
end;

function RbVueDynaPanel.setListData(_dataArr: TJSONArray; _autoFree: boolean
	): RbVueDynaPanel;
begin

    listData:=_dataArr;
    autoFreeList:= _autoFree;
    Result:= self;

    {Autofreeing listData is not needed in the destructor because
    it is passed over to the data object, which is freed when VueApp is destroyed.
    The decision to clone or not is made in build().
    If autoFree is true, then listData is not cloned.}
end;

function RbVueDynaPanel.setListData(_listName: string; _dataArr: TJSONArray;
	_autoFree: boolean): RbVueDynaPanel;
begin
    listName:= _listName;
    Result:= setListData(_dataArr, _autoFree)
end;

function RbVueDynaPanel.getListData: TJSONArray;
begin
    Result:= listData;
end;

function RbVueDynaPanel.code: string;
begin
    build;
	Result:=inherited code;
end;

function RbVueDynaPanel.asJSONObjectMember: string;
begin
    build;
	Result:=inherited asJSONObjectMember;
end;

function RbVueDynaPanel.asJSONObject: TJSONObject;
begin
    Result:= nil;
end;

function RbVueDynaPanel.asVueApp: RbVueApp;
begin
    build;
    Result:= self;
end;

constructor RbVueDynaPanel.New(_name: string; _element: string);
begin
	inherited New(_name, _element);
    isBuilt:= false;
    data.json.Add(indexName, -1); {Default Index field}
    autoFreeDefaultModel:= true;
end;

destructor RbVueDynaPanel.Destroy;
begin
    if autoFreeDefaultModel then
        defaultDynaModel.Free;

    {Autofreeing listData is not needed here because it is passed over to
    the data object, which is freed when VueApp is destroyed. The decision to
    clone or not is made in build(). If autoFree is true, then listData is not cloned.}

    {--- if autoFreeList then
        listData.Free; ----}

	inherited Destroy;
end;


{ RbVueLifecycleMethods }

function RbVueLifecycleMethods.getCode: string;
begin
    Result:= json.Strings['code'];
end;

procedure RbVueLifecycleMethods.setCode(const _code: string);
begin
    json.Strings['code'] := _code;
end;

function RbVueLifecycleMethods.render: string;
begin
    Result:= name + ':' + code;
end;

function RbVueLifecycleMethods.loadFromFile(_fileName: string): boolean;
begin
   code:= getFileContent(_fileName, true);
   Result:= true;
end;

function RbVueLifecycleMethods.propertyName: string;
begin
    Result:= getName;
end;

{ RbVueFilters }

function RbVueFilters.render: string;
begin
    Result:= defaultRenderer(name);
end;

function RbVueFilters.propertyName: string;
begin
    Result:= name;
end;

{ RbVueWatch }

function RbVueWatch.render: string;
begin
    Result:= defaultRenderer(name);
end;

function RbVueWatch.propertyName: string;
begin
    Result:= name;
end;

{ RbVueDirectives }

function RbVueDirectives.render: string;
begin
    Result:= defaultRenderer(name);
end;

function RbVueDirectives.propertyName: string;
begin
    Result:= name;
end;

{ RbVueComputed }

function RbVueComputed.render: string;
begin
    Result:= defaultRenderer(name);
end;

function RbVueComputed.propertyName: string;
begin
    Result:= name;
end;

{ RbVueRefs }

function RbVueRefs.render: string;
begin
    Result:= defaultRenderer(name);
end;

function RbVueRefs.propertyName: string;
begin
    Result:= name;
end;

{ RbVueScopedSlots }

function RbVueScopedSlots.render: string;
begin
    Result:= defaultRenderer(name);
end;

function RbVueScopedSlots.propertyName: string;
begin
    Result:= name;
end;

{ RbVueSlots }

function RbVueSlots.render: string;
begin
    Result:= defaultRenderer(name);
end;

function RbVueSlots.propertyName: string;
begin
    Result:= name;
end;

{ RbVueData }

function RbVueData.render: string;
begin
    Result:= defaultRenderer(name);

    Result:= jsFunc(name, '',
             Format('return (%s);',[json.AsJSON])
             , jsFuncObject);
end;

function RbVueData.propertyName: string;
begin
    Result:= name;
end;


{ RbVueProps }

function RbVueProps.render: string;
begin
    Result:= defaultRenderer(name);
end;

function RbVueProps.propertyName: string;
begin
    Result:= name;
end;

{ RbVueMethodList }

function RbVueMethodList.render: string;
var
  x : TJSONEnum;
  bAddComma: boolean = false;
begin
    Result:= '';
    for x in json do
    begin
        if bAddComma then
            Result:= Result + ','
        else
            bAddComma := true;

        Result := Result + Format('%s: %s', [x.Key, x.Value.AsString]); {This gets rid of the quotes around the json string so the function is defined within the object}
	end;
    Result:= Format('%s: {%s}',[propertyName, Result]);
end;

function RbVueMethodList.getmethod(_name: string): string;
begin
    Result:= '';
    if assigned(json.Find(_name)) then
        Result:= json.Strings[_name];
end;

procedure RbVueMethodList.setmethod(_name: string; const _method: string);
begin
    if assigned(json.Find(_name)) then
        json.Strings[_name]:= _method
    else
        json.Add(_name, TJSONUnquotedString.Create(_method));
end;


function RbVueMethodList.add(_name: string; _code: string): RbVueMethodList;
begin
    if not assigned(json.Find(_name)) then
        json.Add(_name, TJSONUnquotedString.Create(_code));
    Result:= self;
end;

{ RbVuePropertyData }

function RbVuePropertyData.defaultRenderer(_name: string): string;
begin
    Result:= _name + ':' + json.AsJSON;
end;

constructor RbVuePropertyData.Create;
begin
    inherited Create;
    myJSONObject:= TJSONObject.Create;
    myJSONObject.UnquotedMemberNames:=True;
end;

destructor RbVuePropertyData.Destroy;
begin
    myJSONObject.Free;
	inherited Destroy;
end;

function RbVuePropertyData.loadFromFile(_fileName: string): boolean;
var
    tmp: TJSONData;
begin
    Result:= false;
    try
        Log('RbVueBase.loadDataFromFile(): Trying to load from ' + _fileName);
        tmp:= loadJSON(_fileName);
        if Assigned(tmp) then
        begin
            if tmp is TJSONObject then
            begin
                myJSONObject.Free;
                myJSONObject := TJSONObject(tmp);
                Result:= True;
                Log('RbVueBase.loadDataFromFile(): Data loaded!');
			end
            else
            begin
                tmp.Free;
                Log('RbVueBase.loadDataFromFile(): File does not contain JSON Object!');
			end;
		end;
	except
       on E:Exception do
       begin
            log('RbVueBase.loadDataFromFile(): ' + E.Message);
	   end;
	end;

end;

function RbVuePropertyData.import(_sourceJSON: TJSONObject; _freeObject: boolean
	): RbVuePropertyData;
var
    i: integer;
begin
    Result:= self;
    if Assigned(_sourceJSON) then
        copyJSONObject(_sourceJSON, json);

    if _freeObject then
        _sourceJSON.Free;
end;

function RbVuePropertyData.setData(_data: TJSONObject): RbVuePropertyData;
begin
    if assigned(myJSONObject) then myJSONObject.Free;
    myJSONObject:= _data;
    Result:= self;
end;


function RbVuePropertyData.Add(const AName: TJSONStringType; AValue: TJSONData
	): RbVuePropertyData;
begin
    Result:= self;
    json.add(AName, AValue);
end;

function RbVuePropertyData.Add(const AName: TJSONStringType; AValue: Boolean
	): RbVuePropertyData;
begin
    Result:= self;
    json.add(AName, AValue);
end;

function RbVuePropertyData.Add(const AName: TJSONStringType; AValue: TJSONFloat
	): RbVuePropertyData;
begin
    Result:= self;
    json.add(AName, AValue);
end;

function RbVuePropertyData.Add(const AName, AValue: TJSONStringType): RbVuePropertyData;
begin
    Result:= self;
    json.add(AName, AValue);
end;

function RbVuePropertyData.Add(const AName: String; AValue: TJSONUnicodeStringType
	): RbVuePropertyData;
begin
    Result:= self;
    json.add(AName, AValue);
end;

function RbVuePropertyData.Add(const AName: TJSONStringType; Avalue: Integer
	): RbVuePropertyData;
begin
    Result:= self;
    json.add(AName, AValue);
end;

function RbVuePropertyData.Add(const AName: TJSONStringType; Avalue: Int64): RbVuePropertyData;
begin
    Result:= self;
    json.add(AName, AValue);
end;

function RbVuePropertyData.Add(const AName: TJSONStringType; Avalue: QWord): RbVuePropertyData;
begin
    Result:= self;
    json.add(AName, AValue);
end;

function RbVuePropertyData.Add(const AName: TJSONStringType): RbVuePropertyData;
begin
    Result:= self;
    json.add(AName);
end;

function RbVuePropertyData.Add(const AName: TJSONStringType; AValue: TJSONArray
	): RbVuePropertyData;
begin
    Result:= self;
    json.add(AName, AValue);
end;

function RbVuePropertyData.Code(const AName: TJSONStringType; AValue: string
	): RbVuePropertyData;
begin
    Result:= self;
    json.add(AName, TJSONUnquotedString.Create(AValue));
end;


{ RbVueComponent }

constructor RbVueComponent.Create;
begin
    inherited Create;
    Name('component');
end;

constructor RbVueComponent.Create(_name: string);
begin
    Create;
    name(_name);
end;

function RbVueComponent.getTemplate: THtmlCollection;
begin
    if not Assigned(myTemplate) then
        myTemplate := THtmlTemplate.Create;

    Result := myTemplate;
end;

function RbVueComponent.code: string;
var
    _script: TStringList;
    _properties: string = '';
    _comma: string = '';
begin
    _script := TStringList.Create;
    with _script do
    begin
        _properties:= renderProperties;
        if not _properties.IsEmpty then _comma := ',';

        add(Format('Vue.component(''%s'', {', [Name]));
        add(Format('template: ''%s''%s', [template.html, _comma]));
        add(_properties);
        add('});');

    end;
    Result := _script.Text;
    _script.Free;
end;

{ RbVueApp }

function RbVueApp.renderObj: string;
var
    _script: TStringList;
    _properties: string = '';
    _comma: string = '';
begin
    _script := TStringList.Create;
    try

	    with _script do
	    begin
			add('{');

		    _properties:= renderProperties;

		    {element. add comma if properties exist}
		    if not _properties.isEmpty then _comma:= ',';
		    if not element.IsEmpty then
		    	add(Format('el: ''#%s''%s', [element, _comma]));

		    {add properties}
		    add(_properties);
		    add('}');
	    end;
	    Result:= _script.Text;

	finally
      _script.Free;
	end;
end;

function RbVueApp.render(_varFormatting: string): string;
begin
    if myName.isEmpty then myName:= '_' + genRandomKey(8);
    Result:= Format(_varFormatting, [Name, renderObj()]);
end;

function RbVueApp.fnameSeralizer: string;
begin
    Result:= 'serialize';
end;

function RbVueApp.funcSerializeThis: string;
begin
    {DEFINITION OF PAYLOAD JSON}
    {rb.min.js -> Vue.prototype.$dynaPanelDef.serialize()}
    Result:= jsFunc('','' +
    '   return this.$dynaPanelDef.serialize(' + '"' + Name + '",' +
    '       this.$data' +
    '   );');
end;

procedure RbVueApp.setSerializeMethod;
begin
    if Length(methods.method[fnameSeralizer]) = 0 then
        methods.method[fnameSeralizer]:= funcSerializeThis;
end;



function RbVueApp.funcSaveMethodBody(_url: string; _onSuccess: string; _onError: string): string;

{GENERATE SOMETHING LIKE THIS}
{============================}
(*
        saveGrade: function(e) {
            let payload = {
                method: "POST",
                body: this.serialize()
            };
            doFetch("/curriculum/grade/save?grade_code=G001", payload, function(e) {
                goBack();
            });
            return false;
        }
*)

var
    _fbody: string;
    _payloadJSON: TJSONObject;
begin
    {DEFINITION OF PAYLOAD JSON}
    {rb.min.js -> Vue.prototype.$dynaPanelDef.serialize()}

    setSerializeMethod;
    _payloadJSON:= TJSONObject.Create;

    {Generate the PAYLOAD code (js) }
    with _payloadJSON do
    begin
        add('method', 'POST');
        add('body',TJSONUnquotedString.Create('this.serialize()'));
	end;
	_fbody:= 'let payload = ' + _payloadJSON.AsJSON + ';';

    {*** doFetch() is defined in rb.min.js ***}
	//* Do fetch takes maximum 4 parameters
    //*  a) url: The url that the fetch request should hit
	//*  b) payload: the object that will be sent to the url
    //*  c) [optional] code that should execute onSuccess
	//*  d) [optional] code that should execute onFail */

    _fbody:= _fbody + Format('doFetch("%s", payload%s%s);',
                               [_url,

                               {If the onSuccess function is defined, append it to the parameter list}
                               iif(_onSuccess.isEmpty,
                                    {true}  '',                 {no success function}
                                    {false} ', ' + _onSuccess), {append comma, add success function}

                               {If the onError function is defined, append it to the parameter list}
                               iif(_onError.isEmpty,
                               {true}  '',                 {no success function}
                               {false} ', ' + _onError)    {append comma, add success function}

                               ]);

    _fbody:= _fbody + 'return false;';

    Result:=jsFunc(
    {parameters}'e',
    {body} _fbody);

    _payloadJSON.Free;
end;

function RbVueApp.funcDeleteMethodBody(_url: string; _payload: string;
	_onSuccess: string; _onError: string): string;

(* GENERATE SOMETHING LIKE THIS
-------------------------------
		deleteGrade: function(e) {
		    let payload = {
		        method: "DELETE",
		        body: JSON.stringify({
		            grade_code: this.grade_code
		        })
		    };
            if window.confirm("PLEASE CONFIRM.\nAre you sure you want to delete?") {
		        doFetch("/curriculum/grade/delete?grade_code=G001", payload, function(e) {
		            goTo('/curriculum/grade')
		        });
            }
		    return false;
        }
*)

var
    _fbody: string;
    _payloadJSON: TJSONObject;
begin
    _payloadJSON:= TJSONObject.Create;

    {Generate the PAYLOAD code (js) }
    with _payloadJSON do
    begin
        add('method', 'DELETE');
        if _payload.isEmpty then
            add('body',TJSONUnquotedString.Create('JSON.stringify({})'))
        else
            add('body',TJSONUnquotedString.Create('JSON.stringify(' + _payload + ')'));
	end;

    _fbody:= 'if (window.confirm("PLEASE CONFIRM\nAre you sure you want to delete?")){' + sLinebreak +
              '       let payload = ' + _payloadJSON.AsJSON + ';';

    {*** doFetch() is defined in rb.min.js ***}
	//* Do fetch takes maximum 4 parameters
    //*  a) url: The url that the fetch request should hit
	//*  b) payload: the object that will be sent to the url
    //*  c) [optional] code that should execute onSuccess
	//*  d) [optional] code that should execute onFail */
    _fbody:= _fbody + Format('doFetch("%s", payload%s%s);',
                               [_url,

                               {If the onSuccess function is defined, append it to the parameter list}
                               iif(_onSuccess.isEmpty,
                                    {true}  '',                 {no success function}
                                    {false} ', ' + _onSuccess), {append comma, add success function}

                               {If the onError function is defined, append it to the parameter list}
                               iif(_onError.isEmpty,
                               {true}  '',                 {no success function}
                               {false} ', ' + _onError) {append comma, add success function}
                               ]);



    _fbody:= _fbody + '}'; {end brace for If window.confirm()}
    _fbody:= _fbody + 'return false;';

    Result:=jsFunc(
    {parameters}'e',
    {body} _fbody);

    _payloadJSON.Free;
end;

function RbVueApp.funcCancelMethodBody(_url: string; _payload: string;
	_onSuccess: string; _onError: string): string;
var
    _fbody: string;
    _payloadJSON: TJSONObject;
begin
    _payloadJSON:= TJSONObject.Create;

    {Generate the PAYLOAD code (js) }
    with _payloadJSON do
    begin
        add('method', 'POST');
        if _payload.isEmpty then
            add('body',TJSONUnquotedString.Create('JSON.stringify({})'))
        else
            add('body',TJSONUnquotedString.Create('JSON.stringify(' + _payload + ')'));
	end;
	_fbody:= 'let payload = ' + _payloadJSON.AsJSON + ';';

    {*** doFetch() is defined in rb.min.js ***}
	//* Do fetch takes maximum 4 parameters
    //*  a) url: The url that the fetch request should hit
	//*  b) payload: the object that will be sent to the url
    //*  c) [optional] code that should execute onSuccess
	//*  d) [optional] code that should execute onFail */
    _fbody:= _fbody + Format('doFetch("%s", payload%s%s);',
                               [_url,

                               {If the onSuccess function is defined, append it to the parameter list}
                               iif(_onSuccess.isEmpty,
                                    {true}  '',                 {no success function}
                                    {false} ', ' + _onSuccess), {append comma, add success function}

                               {If the onError function is defined, append it to the parameter list}
                               iif(_onError.isEmpty,
                               {true}  '',                 {no success function}
                               {false} ', ' + _onError) {append comma, add success function}
                               ]);

    _fbody:= _fbody + 'return false;';

    Result:=jsFunc(
    {parameters}'e',
    {body} _fbody);

    _payloadJSON.Free;
end;

function RbVueApp.funcRestfulMethodBody(_url: string; _method: string;
	_payload: string; _onSuccess: string; _onError: string): string;
begin

end;

function RbVueApp.element(const _el: string): RbVueBase;
begin
    myElement := _el;
    Result := self;
end;

function RbVueApp.element: string;
begin
    Result := myElement;
end;

function RbVueApp.code: string;
begin

    // var vap_knowledge;
    // window.addEventListener('load', function (e) {
    //     vap_knowledge = new Vue({
    //                            el: "#rbf_contents",
    //                            methods: {}
    //							}
    //						)
    //			}
    //		);

	// PARAM 0 = variable name
    // PARAM 1 = Vue JSON Object
    Result:= render('var %0:s;' + sLinebreak
     + 'window.addEventListener(''load'', function (e) {' + sLinebreak
     	+ '%0:s = new Vue (%1:s);' + sLinebreak
     	+ '}' + sLinebreak
     + ');');

    // Need to render this as a global variable with a function executed at window load.
    // Stanley 16 Dec 2023


end;

function RbVueApp.asJSONObjectMember: string;
begin
    Result:= render('%s: new Vue (%s)');
end;

function RbVueApp.importData(_json: TJSONObject): RbVueApp;
begin
    data.import(_json);
    Result:= self;
end;

function RbVueApp.saveMethod(_name: string; _url: string; _onSuccess: string;
	_onError: string): RbVueApp;
begin
    methods.method[_name]:= funcSaveMethodBody(_url, _onSuccess, _onError);
    Result:= self;
end;

function RbVueApp.deleteMethod(_name: string; _url: string; _payload: string;
	_onSuccess: string; _onError: string): RbVueApp;
begin
    methods.method[_name]:= funcDeleteMethodBody(_url, _payload, _onSuccess, _onError);
    Result:= self;
end;

function RbVueApp.cancelMethod(_name: string; _url: string; _payload: string;
	_onSuccessURL: string; _onError: string): RbVueApp;
begin
    methods.method[_name]:= funcCancelMethodBody(
                                                    _url,
                                                    _payload,
                                                    jsFunc('e', Format('goTo(''%s'')', [_onSuccessURL])),
                                                    _onError
                                                );
    Result:= self;
end;

function RbVueApp.restfulCallMethod(_name: string; _method: string;
	_url: string; _payload: string; _onSuccess: string; _onError: string
	): RbVueApp;
begin
    methods.method[_name]:= funcRestfulMethodBody(_method, _url, _payload, _onSuccess, _onError);
    Result:= self;
end;

constructor RbVueApp.Create;
begin
    inherited Create;
    myNestedVaps:= RbVapList.Create;
end;

constructor RbVueApp.New(_name: string; _element: string);
begin
    Create;
    Name(_name);
    if not _element.isEmpty then
        myElement := _element;
end;

destructor RbVueApp.Destroy;
begin
    myNestedVaps.Free;
	inherited Destroy;
end;

procedure RbVueApp.add(_name: string; _vap: RbVueApp);
begin
    _vap.Name(_name);
    myNestedVaps.Add(_vap);
end;


{ RbVueBase }

function RbVueBase.Name(_Name: string): RbVueBase;
begin
    myName := _Name;
    Result := Self;
end;

function RbVueBase.Name: string;
begin
    Result := myName;
end;

function RbVueBase.data: RbVueData;
begin
    Result := myData;
end;

function RbVueBase.setData(_obj: TJSONObject): RbVueBase;
begin
    myData.setData(_obj);
    Result:= Self;
end;

function RbVueBase.methods: RbVueMethods;
begin
    Result := mymethods;
end;

function RbVueBase.props: RbVueProps;
begin
    Result := myprops;
end;

function RbVueBase.computed: RbVueComputed;
begin
    Result:= mycomputed;
end;

function RbVueBase.renderProperties: string;
var
    _property: RbVuePropertyData;
    bAddComma: boolean = false;
begin
    Result:= '';
    for _property in myProperties do
    begin
        if _property.json.Count>0 then
        begin
            if bAddComma then
                Result:= Result + ','
            else
                bAddComma:= true;
            Result:= Result + _property.render;
		end;
	end;
end;

function RbVueBase.root: RbVueBase;
begin
    Result := myroot;
end;

function RbVueBase.setRoot(_root: RbVueBase): RbVueBase;
begin
    Result := self;
    myroot := _root;
end;

function RbVueBase.children: RbVueAppList;
begin
    Result := mychildren;
end;

function RbVueBase.directives: RbVueDirectives;
begin
    Result:= mydirectives;
end;

function RbVueBase.slots: RbVueSlots;
begin
    Result := myslots;
end;

function RbVueBase.scopedSlots: RbVueScopedSlots;
begin
    Result := myscopedSlots;
end;

function RbVueBase.refs: RbVueRefs;
begin
    Result := myrefs;
end;

function RbVueBase.watch: RbVueWatch;
begin
    Result:= mywatch;
end;

function RbVueBase.filters: RbVueFilters;
begin
    Result:= myfilters;
end;

function RbVueBase.mounted: RbVueMounted;
begin
    Result:= mymounted;
end;

function RbVueBase.created: RbVueCreated;
begin
    result:= myCreated;
end;

function RbVueBase.updated: RbVueUpdated;
begin
    Result:= myUpdated;
end;

function RbVueBase.destroyed: RbVueDestroyed;
begin
    Result:= myDestroyed;
end;


constructor RbVueBase.Create;
var
    i: integer = -1;
begin
    inherited;
    myName      := '';
    mychildren  := RbVueAppList.Create;
    myroot      := self; {only a pointer}

    mymethods   := RbVueMethods.Create;
    mymodel     := RbVueModel.Create;
    mydata      := RbVueData.Create;
    myprops     := RbVueProps.Create;
    mycomputed  := RbVueComputed.Create;
    myslots     := RbVueSlots.Create;
    myscopedSlots := RbVueScopedSlots.Create;
    myrefs      := RbVueRefs.Create;
    mydirectives:= RbVueDirectives.Create;
    mywatch     := RbVueWatch.Create;
    myfilters   := RbVueFilters.Create;
    mymounted   := RbVueMounted.Create;
    myCreated   := RbVueCreated.Create;
    myUpdated   := RbVueUpdated.Create;
    myDestroyed := RbVueDestroyed.Create;

    {To simplify rendering}
	myProperties[plus1(i)]:= mymethods;
	myProperties[plus1(i)]:= mymodel;
	myProperties[plus1(i)]:= mydata;
	myProperties[plus1(i)]:= myprops;
	myProperties[plus1(i)]:= mycomputed;
	myProperties[plus1(i)]:= myslots;
	myProperties[plus1(i)]:= myscopedSlots;
	myProperties[plus1(i)]:= myrefs;
	myProperties[plus1(i)]:= mydirectives;
	myProperties[plus1(i)]:= mywatch;
	myProperties[plus1(i)]:= myfilters;
   	myProperties[plus1(i)]:= mymounted;
   	myProperties[plus1(i)]:= myCreated;
   	myProperties[plus1(i)]:= myUpdated;
   	myProperties[plus1(i)]:= myDestroyed;
    {TOTAL 15}
end;

destructor RbVueBase.Destroy;
var
    _property: RbVuePropertyData;
begin
    mychildren.Free;

    for _property in myProperties do
        _property.Free;

    inherited Destroy;
end;

function RbVueBase.asJSONObjectMember: string;
begin
    Result:= 'AsJSONObjectMember () not implemented';
end;

function RbVueBase.asJSONObject: TJSONObject;
var
	_property: RbVuePropertyData;
begin
    Result:= TJSONObject.Create;
    for _property in myProperties do
    begin
        if _property.json.count> 0 then
            Result.add(_property.propertyName, _property.json);
	end;
end;

class function RbVueBase.CDNLinks: THtmlElementArray;
begin
    SetLength(Result, 2);

    {VueJS}
    Result[0] := THTMLScript.Create;
    with Result[0] as THTMLScript do setSrc('https://cdn.jsdelivr.net/npm/vue/dist/vue.js').setAttrFlag('defer');
    //with Result[0] as THTMLScript do setSrc('https://unpkg.com/vue');
    // with Result[0] as THTMLScript do setSrc('https://cdnjs.cloudflare.com/ajax/libs/vue/2.6.10/vue.min.js');

    {Mousetrap js}
    Result[1] := THTMLScript.Create;
    with Result[1] as THTMLScript do setSrc('https://cdnjs.cloudflare.com/ajax/libs/mousetrap/1.6.5/mousetrap.min.js').setAttrFlag('defer');

 //   {Vue Select js}
 //   Result[2] := THTMLScript.Create;
 //   with Result[2] as THTMLScript do setSrc('https://unpkg.com/vue-select@latest');
 //
 //   {Vue Select js}
 //   Result[3] := THtmlLink.Create;
 //   with Result[3] as THtmlLink do
 //   begin
 //       href:= 'https://unpkg.com/vue-select@latest/dist/vue-select.css';
 //       rel:= 'stylesheet';
	//end;
end;

end.

(*
### rb.min.js ####
******************
/* VARIABLES */
var id = 0;
/* Queue functions to run after the document as loaded */
var onLoadFuncs = [];
function rbAddOnLoadFunc(_loader){
    onLoadFuncs.push(_loader);
};
function rbRunOnLoaders(e) {
    onLoadFuncs.forEach((_loader) => _loader(e));
};
/* RESIZE */
function rbDocResize(){
/*
JS - Media Match Function
Use JavaScript's matchMedia Method to detect the type of device you are using, and invoke/activate functions based on those criteria.
Reference Links:
http://davidwalsh.name/orientation-change
http://davidwalsh.name/device-state-detection-css-media-queries-javascript
http://www.slideshare.net/DavidKnight5/howto-match-media-25008199
http://caniuse.com/matchmedia
*/
    var design;
        if (window.matchMedia('only screen and (max-width: 640px)').matches) {
            design = 'mobile';
            document.body.style="ui mobile screen";
        } else if (window.matchMedia('only screen and (max-width: 1024px)').matches) {
            design = 'tablet';
            document.body.style="ui tablet screen";
        } else if (window.matchMedia('screen').matches) {
            design = 'desktop';
            document.body.style="ui screen";
        } else if (window.matchMedia('handheld').matches) {
            design = 'handheld mobile';
            document.body.style="ui mobile screen";
        }
        console.log('screen size is ' + design);
}
function rbPreventCtrl_S() {
	$(document).bind('keydown', function(e){
		if (e.ctrlKey && (e.which == 83)) {
			e.preventDefault();
			return false;
		} else {
			return true;
		}
	});
};
function initSUIComponents(object){
	$(".ui.dropdown").dropdown();
};
/* Inside a Vue object use jQuery instead of $ */
Vue.prototype.$dynaPanelDef = (function() {
    return {
		scrollTo: function (el){
            if (!(el===undefined)) {
				el.scrollIntoView();
				el = el.querySelector('.autofocus');
                el.focus();
            } else {
                console.log("Undefined element");
            }
		},
        objectAdd: function(self, array, object, ref) {
            array.push(object);
			/* This inserts a property into the object uidx which serves as the id */
            object.uidx = array.length;
            self.$nextTick(() => {
				initSUIComponents(self);
                this.scrollTo(self.$refs[ref.refname][object.uidx - 1]);
            });
        },
        listReindex: (array) => array.forEach((object, index) => {
            object.uidx = index + 1;
        }),
        objectRemove: function(array, object) {
            array.splice(array.indexOf(object), 1);
            this.listReindex(array);
        },
        /*## REPOSITION ##*/
        reposition: function(self, array, object, delta, ref) {
            let index = array.indexOf(object);
            if (delta > 1) {delta = 1;}
            else if (delta < -1) {delta = -1;};
            let newIndex = index + delta;
            /* Return if already at the top or bottom.*/
            if (!(newIndex < 0 || newIndex == array.length)) {
				/*Sort the indixes (fixed)*/
				let indexes = [index, newIndex].sort((a, b) => a - b);
				/*Replace from lowest index, two elements, reverting the order*/
				array.splice(indexes[0], 2, array[indexes[1]], array[indexes[0]]);
				this.listReindex(array);
			}
			this.scrollTo(self.$refs[ref.refname][object.uidx - 1]);
        },
        makeID: function(prefix){
			if (!prefix)
				prefix = "id";
			return [prefix, id++].join("_");
		},
		serialize: function(panelName, myData){
            let result = {
			    payload: "RbVueApp",
				name: panelName,
				data: myData
            };
			return JSON.stringify(result);
		}
	}
})();
function goTo(url){
	path = window.location.origin + url;
	window.location.href = path;
};
function showThumbs(_thumbs, _class, status, _message){
    _thumbs = _thumbs + " outline icon";
	if (_message === undefined) { _message = "";}
	_message =`<i class="${_thumbs }"></i>  ${status} </br> ${_message}`;
    $('body').toast({
        class: _class,
        compact:false,
        displayTime: 1500,
        position: "bottom right",
        message: _message
    });
};
function goBack() {
  window.location=document.referrer;
}
/* FORM POSTING*/
function doFetch(_url,  _payload, _onSuccess, _onError){
    /* Check for form validation before sending */
	if (!(_url === undefined) || (url==="")) {
		fetch(_url, _payload)
	        .then((response) => {
			       /* console.log(response); */
				    if (!response.ok) {
	                    throw (Error (response.statusText));
					}
	                return response.json();
	         })
	        .then(function (data){
					if (data.status==='OK'){
					    showThumbs('thumbs up', 'black', data.status, data.message);
						if (typeof(_onSuccess)==='function') _onSuccess(data);
					}else{
                        showThumbs('thumbs up', 'orange',data.status, data.message);
						if (typeof(_onError)==='function') _onError(error);
					}
	        })
	        .catch( function (error) {
                showThumbs('thumbs down', 'error', 'ERR', error);
                if (typeof(_onError)==='function') _onError(error);
			});
    }
	return false;
};
/* Inside a Vue object use jQuery instead of $ */
function initJQueryinVue(){
	Vue.prototype.jQuery = jQuery;
	Vue.prototype.document = document;
	Vue.prototype.window = window;
};
/* INITIALIZATION */
/* Initializing functions that must run inside OnLoad() */
rbAddOnLoadFunc(initJQueryinVue);
rbAddOnLoadFunc(rbDocResize);
/* Event Listeners */
document.onresize=rbDocResize; /* Document resizer */
window.addEventListener('load', rbRunOnLoaders); /* Run OnLoad functions */
*)
