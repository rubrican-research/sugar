unit sugar.htmlbuilder.truthtable;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, contnrs, sugar.collections, sugar.htmlbuilder, fpjson;

type

  RbTruthTable = class;

  { RbTruthTableCell }

  RbTruthTableCell = class
  private
      myOwner: TObject;
      myRowName: string;
      myColName: string;
	  function getRowName: string;virtual;
	  procedure setRowName(const _rowName: string);virtual;
	  function getColName: string;virtual;
	  procedure setColName(const _colName: string);virtual;
	  function getValue: TObject; virtual;
	  procedure setValue(const _value: TObject);virtual;

  public
    property rowName: string read getRowName write setRowName;
    property colName: string read getColName write setColName;
    property value: TObject read getValue write setValue;
    function owner: TObject; virtual;
    constructor Create; virtual;
    constructor Create (constref _owner: TObject); virtual;
  end;

  { GTruthTableCell }

  generic GTruthTableCell<DataClass: TObject; OwnerClass: RbTruthTable> = class(RbTruthTableCell)
  private
      myValue: DataClass;
	  function getValue: DataClass; reintroduce;
	  procedure setValue(const _value: DataClass); reintroduce;
  public
    property value: DataClass read getValue write setValue;
    function owner: OwnerClass; reintroduce;
    constructor Create; override;
    constructor Create (constref _owner: OwnerClass); reintroduce;
    destructor Destroy; override;
  end;

  RbTruthTable = class
        function rowCount: integer; virtual;
  end;

  generic GTruthTable <DataClass:TObject> = class(RbTruthTable)
  private type

        PTruthTableCell = specialize GTruthTableCell<DataClass, RbTruthTable>;

        PColumnListBase = specialize GenericHashObjectList <PTruthTableCell>;

		{ PColumnList }

        PColumnList = class (PColumnListBase)
        private
            procedure OnTruthTableCellCreate(_cell: TObject);
            function createTruthTableCell: PTruthTableCell;
        public
            rowName: string;
            owner: RbTruthTable;
            constructor Create(FreeObjects: boolean=True); reintroduce;
            function add(const AName: shortstring; AObject: PTruthTableCell): integer; override;
            function colNames: TStringArray;
        end;

        PRowListBase = specialize GenericHashObjectList <PColumnList>;

		{ PRowList }

        PRowList = class(PRowListBase)
            owner: RbTruthTable;
            function get(const s: shortstring): PColumnList; reintroduce;
        end;

    function fillStringMap(constref _map: TStringMap; const _names: string; const _captions: string;
	  const _delimiter: char): TStringMap;
	function addToStringMap(constref _map: TStringMap; const _name: string;
		_caption: string): TStringMap;

  private
      myID: string;
      myName: string;
	  myrestrictColumns: boolean;
	  myrestrictRows: boolean;
	  procedure setRestrictColumns(const _restrictColumns: boolean);
      procedure setRestrictRows(const _restrictRows: boolean);
	  function getName: string;
	  procedure setName(const _Name: string);
	  function getID: string;
  protected
       myRows: PRowList;
       myRowNames, myColNames: TStringMap ; {used as index of names}
       myRowDescriptions, myColDescriptions: TStringMap;
       myShowDescriptions: boolean;
  public
    Description: string;
    property ID: string read getID;
    property Name: string read getName write setName;
    {Restrict properties make sure that rows and cols cannot be added on the fly. Only what was defined}
    property restrictColumns: boolean read myrestrictColumns write setRestrictColumns;
    property restrictRows: boolean read myrestrictRows write setRestrictRows;
    property showDescriptions: boolean read myShowDescriptions write myShowDescriptions;

    constructor Create;
    destructor Destroy; override;
    function get(const _row: string; const _col: string): DataClass; overload;
    function get(const _row: integer; const _col: integer): DataClass; overload;

    function rowCount: integer; override;
    function row(const _i: integer): PColumnList;
    function row(const _row: string): PColumnList;

    function rowNames: TStringArray;
    function rowCaption(const _name: string): string; overload;
    function rowCaption(const _i: integer): string; overload;

    function rowDescription(const _name: string): string; overload;
    function rowDescription(const _name: string; _value: string): string; overload;

    function rowDescription(const _i: integer): string; overload;
    function rowDescription(const _i: integer; _value: string): string; overload;

    function colCount: integer;
    function colCount(_rowName: string): integer; overload;
    function colCount(_rowIndex: integer): integer; overload;

    function colNames: TStringArray;
    function colCaption(const _name: string): string; overload;
    function colCaption(const _i: integer): string; overload;

    function colDescription(const _name: string): string; overload;
    function colDescription(const _name: string; _value: string): string; overload;

    function colDescription(const _i: integer): string; overload;
    function colDescription(const _i: integer; _value: string): string; overload;

    {Sets fixed rows and columns to use}
    procedure fixRows(const _names: string; const _captions: string = ''; const _delimiter: char = ',');
    procedure addRow(const _name: string; const _caption: string = ''; _description: string = '');

    procedure fixColumns(const _names: string; const _captions: string= ''; const _delimiter: char = ',');
    procedure addCol(const _name: string; const _caption: string = ''; _description: string = '');

    function asHtmlObj: THtmlCollection; virtual;
    function asJSONObj: TJSONObject; virtual;

    {Interfacing API}

    {implement how the data is to be rendered as a string. This is used in the asHtmlObj}
    function getStr(const _row: string; const _col: string): string; virtual; abstract;
    {implements how the data is rendered as a JSON object. This is used in the asJSON() }
    function getJSON(const _row: string; const _col: string): TJSONObject; virtual; abstract;
    {implements how data is rendered as an HTML string}
    function getHtml(const _row: string; const _col: string): THtmlCollection; virtual; abstract;

    // function getModel: RbModel;
  end;

  RbStringTruthTableBase = specialize GTruthTable<TStringObject>;

  { RbStringTruthTable }

  RbStringTruthTable = class (RbStringTruthTableBase)
  public
      {implement how the data is to be rendered as a string. This is used in the asHtmlObj}
	  function getStr(const _row: string; const _col: string): string; override;
	  function getJSON(const _row: string; const _col: string): TJSONObject; override;
	  function getHtml(const _row: string; const _col: string): THtmlCollection; override;

  end;


implementation
uses
    sugar.utils;
{ RbTruthTable }

function RbTruthTable.rowCount: integer;
begin
    Result:= -1;
end;

{ GTruthTable.PColumnList }

procedure GTruthTable.PColumnList.OnTruthTableCellCreate(_cell: TObject);
begin

end;

function GTruthTable.PColumnList.createTruthTableCell: PTruthTableCell;
begin
    Result:= PTruthTableCell.Create(owner);
end;

constructor GTruthTable.PColumnList.Create(FreeObjects: boolean);
begin
    inherited;
    onCreateObj:= @OnTruthTableCellCreate;
    objConstructor:= @createTruthTableCell;
end;

function GTruthTable.PColumnList.add(const AName: shortstring;
	AObject: PTruthTableCell): integer;
begin
    AObject.colName:= AName;
    AObject.rowName:= rowName;
	Result:=inherited add(AName, AObject);
end;

function GTruthTable.PColumnList.colNames: TStringArray;
begin
    Result:= toStringArray(getNames());
end;

{ GTruthTable.PRowList }

function GTruthTable.PRowList.get(const s: shortstring): PColumnList;
begin
    Result:= find(s);
    if not Assigned(Result) then
    begin
        Result:= PColumnList.Create();
        Result.owner:= owner;
        Result.rowName:= s;
        add(s, Result);
	end;
end;

{ GTruthTable }

constructor GTruthTable.Create;
begin
    inherited;
    myRows:= PRowList.Create;
    myRows.owner:= Self;
    myRowNames:= TStringMap.Create;
    myRowDescriptions:= TStringMap.Create;
    myColNames:= TStringMap.Create;
    myColDescriptions:= TStringMap.Create;
    restrictRows:= False;
    restrictColumns:= False;
    myShowDescriptions:= True;
end;

destructor GTruthTable.Destroy;
begin
    myRowNames.Free;
    myRowDescriptions.Free;
    myColNames.Free;
    myColDescriptions.Free;
    myRows.Free;
	inherited Destroy;
end;

function GTruthTable.get(const _row: string; const _col: string): DataClass;
var
    p: Pointer;
begin
    if not restrictRows then
    begin
        if myRowNames.valueOf(_row).isEmpty then
            myRowNames.put(_row, _col);
	end
	else
    begin
        if myRowNames.valueOf(_row).isEmpty then
            trip('GTruthTable is restricted. Row not declared: ' + _row);
    end;


    if not restrictColumns then
    begin
        if myColNames.valueOf(_col).isEmpty then
            myColNames.put(_col, _col);
    end
    else
    begin
        if myColNames.valueOf(_col).isEmpty then
            trip('GTruthTable is restricted. Column not declared: ' + _col);
	end;

	Result:= myRows.get(_row).get(_col).Value;
end;

function GTruthTable.get(const _row: integer; const _col: integer): DataClass;
begin
    Result:= get(myRowNames.NameOfIndex(_row), myColNames.NameOfIndex(_col));
end;

function GTruthTable.rowCount: integer;
begin
    Result:= myRows.Count;
end;

function GTruthTable.row(const _i: integer): PColumnList;
begin
    Result:= myRows.Items[_i];
end;

function GTruthTable.row(const _row: string): PColumnList;
begin
    Result:= myRows.get(_row);
end;


procedure GTruthTable.setRestrictColumns(const _restrictColumns: boolean);
begin
	if myrestrictColumns=_restrictColumns then Exit;
	myrestrictColumns:=_restrictColumns;
end;

procedure GTruthTable.setRestrictRows(const _restrictRows: boolean);
begin
	if myrestrictRows=_restrictRows then Exit;
	myrestrictRows:=_restrictRows;
end;

function GTruthTable.getName: string;
begin
    if myName.isEmpty then
        Result:= 'TT' + genRandomKey(7)
    else
        Result:= myName
end;

procedure GTruthTable.setName(const _Name: string);
begin
    if myName = _Name then exit;
    myName:= _Name;
end;

function GTruthTable.getID: string;
begin
    if myID.IsEmpty then
        myID:= 'i'+genRandomKey(6);

    Result:= myID;
end;

function GTruthTable.rowNames: TStringArray;
begin
    Result:= myRowNames.keys;
end;

function GTruthTable.rowCaption(const _name: string): string;
begin
    Result:= myRowNames.valueOf(_name);
end;

function GTruthTable.rowCaption(const _i: integer): string;
begin
    Result:= rowCaption(myRowNames.NameOfIndex(_i));
end;

function GTruthTable.rowDescription(const _name: string): string;
begin
    Result:= myRowDescriptions.valueOf(_name);
end;

function GTruthTable.rowDescription(const _name: string; _value: string
	): string;
begin
    myRowDescriptions.put(_name, _value);
    Result:= _value;
end;

function GTruthTable.rowDescription(const _i: integer): string;
begin
    Result:= rowDescription(myRowDescriptions.NameOfIndex(_i));
end;

function GTruthTable.rowDescription(const _i: integer; _value: string): string;
begin
    Result:= rowDescription(myRowDescriptions.NameOfIndex(_i), _value);
end;

function GTruthTable.colCount: integer;
begin
    Result:= myColNames.Count;
end;

function GTruthTable.colCount(_rowName: string): integer;
begin
    Result:= Row(_rowName).Count;
end;

function GTruthTable.colCount(_rowIndex: integer): integer;
begin
  Result:= Row(_rowIndex).Count;
end;

function GTruthTable.colNames: TStringArray;
begin
    Result:= myColNames.keys;
end;

function GTruthTable.colCaption(const _name: string): string;
begin
    Result:= myColNames.valueOf(_name);
end;

function GTruthTable.colCaption(const _i: integer): string;
begin
    Result:= colCaption(myColNames.NameOfIndex(_i));
end;

function GTruthTable.colDescription(const _name: string): string;
begin
    Result:= myColDescriptions.valueOf(_name);
end;

function GTruthTable.colDescription(const _name: string; _value: string
	): string;
begin
    myColDescriptions.put(_name, _value);
    Result:= _value;
end;

function GTruthTable.colDescription(const _i: integer): string;
begin
    Result:= colDescription(myColDescriptions.NameOfIndex(_i));
end;

function GTruthTable.colDescription(const _i: integer; _value: string): string;
begin
    Result:= colDescription(myColDescriptions.NameOfIndex(_i), _value);
end;

function GTruthTable.addToStringMap(constref _map: TStringMap; const _name: string; _caption: string): TStringMap;
begin
    Result:= _map;
    if _caption.isEmpty then _caption:= _name;
    _map.add(_name, _caption);
end;

function GTruthTable.fillStringMap(constref _map: TStringMap;
	const _names: string; const _captions: string; const _delimiter: char
	): TStringMap;
var
    i: integer;
    _count, _captCount: integer;
    _arNames, _arCaptions: TStringArray;
    _caption: string = '';
begin
    {Clear the map}
    _map.Free;
    Result:= TStringMap.Create;

    _arNames:= toStringArray(_names, _delimiter);
    _arCaptions:= toStringArray(_captions, _delimiter);
    _count:= Length(_arNames);
    _captCount:= Length(_arCaptions);

    for i:= 0 to pred(_count) do
    begin
        if i < _captCount then
            _caption:= iif((_arCaptions[i].Length > 0), _arCaptions[i], _arNames[i])
        else
            _caption:= _arNames[i];

        _map.Add(_arNames[i],_caption);
	end;
end;

procedure GTruthTable.fixRows(const _names: string; const _captions: string;
	const _delimiter: char);
var
    i: integer;
begin
    if not restrictRows then restrictRows:= true;
    myRowNames:= fillStringMap(myRowNames, _names, _captions, _delimiter);
end;

procedure GTruthTable.addRow(const _name: string; const _caption: string;
	_description: string);
begin
    myRowNames:= addToStringMap(myRowNames, _name, _caption);
    rowDescription(_name, _description);
end;

procedure GTruthTable.fixColumns(const _names: string; const _captions: string;
	const _delimiter: char);
begin
    if not restrictColumns then restrictColumns:= true;
    myColNames:= fillStringMap(myColNames, _names, _captions, _delimiter);

end;

procedure GTruthTable.addCol(const _name: string; const _caption: string;
	_description: string);
begin
    myColNames:= addToStringMap(myColNames, _name, _caption);
    colDescription(_name, _description);
end;

function GTruthTable.asHtmlObj: THtmlCollection;
var
  _tbl: THtmlTable;
  _colName: string;
  _rowName: string;
  _tblRow: THtmlTableRow;
begin
  _tbl := THtmlTable.Create;
  _tbl.AddClass('ui compact celled table');

  with _tbl.tableheader.newRow do
  begin
      newCol('');
      for _colName in colNames do
      begin
          newCol(colCaption(_colName)).addClass('ui center aligned');

	  end;

  end;

  _colName:= '';
  for _rowName in rowNames do
  begin
      _tblRow:= _tbl.newRow;

      with _tblRow.newCol do
      begin
          style.border_style('solid none').border_width('1px');
          strong(rowCaption(_rowName));
          if myShowDescriptions then
          begin
              span_(rowDescription(_rowName)).addClass('ui small text');
          end
          else
            setTitle(rowDescription(_rowName));
	  end;

	  for _colName in colNames do
      begin
          with _tblRow.newCol() do
          begin
            addClass('ui center aligned');
            add(getHtml(_rowName,_colName));
            style.border_style('solid none').border_width('1px');
		  end;
	  end;
  end;
  Result:= _tbl;
end;

function GTruthTable.asJSONObj: TJSONObject;
var
  _row, _col: string;
  _obj: TJSONObject;
begin
    Result:= TJSONObject.Create;
    for _row in rowNames do
    begin
        _obj:= TJSONObject.Create;
        for _col in colNames do
        begin
            _obj.Add(_col, getJSON(_row, _col));
		end;
        Result.Add(_row, _obj);
	end;
end;

{ GTruthTableCell }

function GTruthTableCell.getValue: DataClass;
begin
    if not Assigned(myValue) then
        myValue:= DataClass.Create;

    Result:= myValue;
end;

procedure GTruthTableCell.setValue(const _value: DataClass);
begin
    // Don't do anything here.
end;

function GTruthTableCell.owner: OwnerClass;
begin
    Result:= inherited owner as OwnerClass;
end;

constructor GTruthTableCell.Create;
begin
	inherited Create;
end;

constructor GTruthTableCell.Create(constref _owner: OwnerClass);
begin
    inherited Create(_owner);
end;

destructor GTruthTableCell.Destroy;
begin
    myValue.Free;
	inherited Destroy;
end;

{ RbTruthTableCell }

function RbTruthTableCell.getColName: string;
begin
    Result:= myColName;
end;

function RbTruthTableCell.getRowName: string;
begin
    Result:= myRowName;
end;

function RbTruthTableCell.getValue: TObject;
begin
    Result:= nil;
end;

procedure RbTruthTableCell.setColName(const _colName: string);
begin
    if myColName = _colName then exit;
    myColName:= _colName;
end;

procedure RbTruthTableCell.setRowName(const _rowName: string);
begin
    if myRowName = _rowName then exit;
    myRowName:= _rowName;
end;

procedure RbTruthTableCell.setValue(const _value: TObject);
begin
    // Don't do anything
end;

function RbTruthTableCell.owner: TObject;
begin
    Result:= myOwner;
end;

constructor RbTruthTableCell.Create;
begin
    inherited;
end;

constructor RbTruthTableCell.Create(constref _owner: TObject);
begin
    Create;
    myOwner:= _owner;
end;

function RbStringTruthTable.getStr(const _row: string; const _col: string): string;
begin
    Result:= get(_row,_col).Value;
end;

function RbStringTruthTable.getJSON(const _row: string; const _col: string
	): TJSONObject;
begin
    Result:= TJSONObject.Create;
    Result.add('value', get(_row,_col).Value);
end;

function RbStringTruthTable.getHtml(const _row: string; const _col: string
	): THtmlCollection;
begin
    Result:= THtmlDiv.Create;
    if (Length(get(_row,_col).Value)>0) then
        Result.input_();
        Result.span_(getStr(_row,_col));
end;

end.


