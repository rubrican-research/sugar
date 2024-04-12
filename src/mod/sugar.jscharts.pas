unit sugar.jscharts;
{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpJson, sugar.jshelpers, sugar.utils, sugar.collections, sugar.htmlbuilder, sugar.jsonlib;

type

  { TJSChartData }

  { TJSChartDataColumn }
  TJSChartDataColumns = class;

  TJSChartDataColumn = class
  private
	  myrole: string;
	  procedure setrole(const _role: string);
  protected
      myid: string;
      myOwner: TJSChartDataColumns;
	  myname: string;
	  mydataType: JSPrimitiveDataType;
      myIsFloat: boolean;
	  procedure setid(const _id: string);
	  procedure setname(const _name: string);
	  procedure setdataType(const _dataType: JSPrimitiveDataType); virtual;
  public
    property id: string read myid write setid;
    property name: string read myname write setname;
    property dataType: JSPrimitiveDataType read mydataType write setdataType;
    property role: string read myrole write setrole;
    property owner: TJSChartDataColumns read myOwner;
  end;

  TJSChartDataColumnBase = specialize GenericHashObjectList <TJSChartDataColumn>;

  { TJSChartDataColumns }

  TJSChartDataColumns = class(TJSChartDataColumnBase)
    function add(const AName: shortstring; AObject: TJSChartDataColumn): integer; reintroduce;
    function add(AObject: TJSChartDataColumn): TJSChartDataColumn; overload;
  end;


  TJSChartData = class
  protected
    myColumns: TJSChartDataColumns;
    myData: TJSONArray;
  public
    name: string;
    caption: string;
    legend: string;
    isDate: boolean;
    function data: TJSONArray;
    function columns: TJSChartDataColumns;
    function addColumn(_name: string; _dataType: JSPrimitiveDataType; _role: string = ''): TJSChartDataColumn; virtual; overload;
    function addColumn(_id: string; _name: string; _dataType: JSPrimitiveDataType; _role: string = ''): TJSChartDataColumn; virtual; overload;
    function columCount: integer;

    function newRow:  TDynaJSONObject;
    function rowCount: integer;

    constructor Create; virtual;
    destructor Destroy; override;

  end;

  { TJSChartSeries }

  TJSChartDataListBase = specialize GenericHashObjectList<TJSChartData>;

  { TJSChartDataList }

  TJSChartDataList = class(TJSChartDataListBase)
    function add(const AName: shortstring; AObject: TJSChartData): integer; reintroduce;
  end;

  {Add more type here when needed}
  TJSChartType = (jschartUndefined, jschartLine, jschartBar, jschartColumn, jschartArea, jschartScatter, jschartHistogram, jschartPie, jschartTimeline);

  TJSChart = class
  protected
    mySeries: TJSChartDataList;
    myName: string;
    myElement: string;
    myChartType: TJSChartType;
  protected
	  function getName: string; virtual;
	  procedure setName(const _Name: string); virtual;
	  function getElement: string; virtual;
	  procedure setElement(const _Element: string); virtual;
	  function getChartType: TJSChartType; virtual;
	  procedure setChartType(const _ChartType: TJSChartType); virtual;
  public
    property Name: string read getName write setName;
    property Element: string read getElement write setElement;
    property ChartType: TJSChartType read getChartType write setChartType;

    class function CDNLinks: THtmlElementArray;virtual;
    function code: string;virtual;
    {get data series }
    function series(_name: string):   TJSChartData; virtual; overload;
    function series(_index: integer): TJSChartData; virtual; overload;
    function seriesCount: integer;

    constructor Create;
    destructor Destroy; override;
  end;


implementation
{ TJSChartDataList }

function TJSChartDataList.add(const AName: shortstring; AObject: TJSChartData): integer;
begin
    Result:= inherited add(AName, AObject);
    if AObject.name <> AName then AObject.name:= AName;
end;

{ TJSChartDataList }

{ TJSChartDataColumns }

function TJSChartDataColumns.add(const AName: shortstring;
	AObject: TJSChartDataColumn): integer;
begin
    Result:= inherited add(AName, AObject);
    AObject.myOwner:= self;
    if AObject.name <> AName then AObject.name:= AName;
end;

function TJSChartDataColumns.add(AObject: TJSChartDataColumn
	): TJSChartDataColumn;
begin
    Result:= AObject;
    add(AObject.name, AObject);
end;

{ TJSChartDataColumn }

procedure TJSChartDataColumn.setdataType(const _dataType: JSPrimitiveDataType);
begin
	if mydataType=_dataType then Exit;
	mydataType:=_dataType;
end;

procedure TJSChartDataColumn.setrole(const _role: string);
begin
	if myrole=_role then Exit;
	myrole:=_role;
end;

procedure TJSChartDataColumn.setid(const _id: string);
begin
	if myid=_id then Exit;
	myid:=_id;
end;

procedure TJSChartDataColumn.setname(const _name: string);
begin
	if myname=_name then Exit;
	myname:=_name;
end;

{ TJSChart }

function TJSChart.getChartType: TJSChartType;
begin
    Result:= myChartType;
end;

function TJSChart.getElement: string;
begin
    Result:= myElement;
    if Result.isEmpty then
        Result:= 'chart_container'; {default name}
end;

function TJSChart.getName: string;
begin
    Result:= myName;
    if Result.isEmpty then
        Result:= 'chart_' + GetTickCount64.ToString; {default name}
end;

procedure TJSChart.setChartType(const _ChartType: TJSChartType);
begin
    myChartType:= _ChartType;
end;

procedure TJSChart.setElement(const _Element: string);
begin
    myElement:= _Element;
end;

procedure TJSChart.setName(const _Name: string);
begin
    myName:= _Name;
end;

class function TJSChart.CDNLinks: THtmlElementArray;
begin
    SetLength(Result, 0);
end;

function TJSChart.code: string;
begin
    Result:= '';
end;

function TJSChart.series(_name: string): TJSChartData;
begin
    Result:= mySeries.get(_name);
end;

function TJSChart.series(_index: integer): TJSChartData;
begin
    Result:= mySeries.Items[_index];
end;

function TJSChart.seriesCount: integer;
begin
    Result:= mySeries.Count;
end;

constructor TJSChart.Create;
begin
    inherited;
    mySeries:=TJSChartDataList.Create;
end;

destructor TJSChart.Destroy;
begin
    mySeries.Free;
	inherited Destroy;
end;

{ TJSChartData }

function TJSChartData.data: TJSONArray;
begin
    Result:= myData;
end;

function TJSChartData.columns: TJSChartDataColumns;
begin
    Result:= myColumns;
end;

function TJSChartData.addColumn(_name: string; _dataType: JSPrimitiveDataType;
	_role: string): TJSChartDataColumn;
begin
    Result:= addColumn('id'+_name, _name, _dataType,_role);
end;

function TJSChartData.addColumn(_id: string; _name: string;
	_dataType: JSPrimitiveDataType; _role: string): TJSChartDataColumn;
begin
    with myColumns.get(_name) do
    begin
        id      := _id;
        name    := _name;
        datatype:= _dataType;
        role    := _role;
    end;
end;


function TJSChartData.columCount: integer;
begin
    Result:= myColumns.Count;
end;

function TJSChartData.newRow: TDynaJSONObject;
begin
    Result:= TDynaJSONObject.Create;
    myData.Add(Result);
end;

function TJSChartData.rowCount: integer;
begin
    Result:= myData.Count;
end;

constructor TJSChartData.Create;
begin
    inherited;
    myData:= TJSONArray.Create;
    myColumns:= TJSChartDataColumns.Create;
end;

destructor TJSChartData.Destroy;
begin
    myColumns.Free;
    myData.Free;
    inherited Destroy;
end;




end.

