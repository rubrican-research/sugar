unit sugar.jsGoogleChart;
{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, fpJson, sugar.jshelpers, sugar.utils, sugar.collections, sugar.htmlbuilder, sugar.jscharts,
   sugar.jsonlib;

type
  { TJSGoogleChart }
  { Don't use multiple series. }
  TJSGoogleChart = class(TJSChart)
  private
	  myautoHeight: boolean;
	  myautoWidth: boolean;
	  procedure setautoHeight(const _autoHeight: boolean);
	  procedure setautoWidth(const _autoWidth: boolean);
	  procedure setautoDimensions(const _autoDimensions: boolean);
	  function getAutoDimensions: boolean;
  protected
    myOptions: TDynaJSONObject;
    {This is the API name for the chart we are trying to generate. See setChartType()}
    myChartObjectName: string;
    myVarChart: string;
    myVarOptions: string;
    myVarData: string;
    function drawFunctionName: string;
    procedure setChartType(const _ChartType: TJSChartType); override;
  public
    class function CDNLinks: THtmlElementArray; override;
    function options: TDynaJSONObject;
    function code: string; override;

    class function packageLoaderJS: string; virtual;
    function jsCodeDrawFunction: string; virtual;
    function jsCodeOnLoadFunction: string; virtual;
    function jsCodeDataVariable: string; virtual;
    function jsCodeOptionsVariable: string; virtual;
    function jsCodeChartVariable: string; virtual;
    function jsNewDate(_dt: TDateTime): string; virtual;
    function jsNewDateTime(_dt: TDateTime): string; virtual;
    function jsCodeTimeOfDay(_dt:TDateTime): TJSONArray; virtual;

    constructor Create; virtual;
    destructor Destroy; override;
  public
    {These properties generate a JS function that sets the height and width of the chart to
    the height and width of the parent node of the element. See jsCodeOptionsVariable()}
    property autoHeight: boolean read myautoHeight write setautoHeight;
    property autoWidth: boolean read myautoWidth write setautoWidth;
    property autoDimensions: boolean read getAutoDimensions write setautoDimensions;
  end;

  { TGoogleLineChart }

  TGoogleLineChart = class(TJSGoogleChart)
    constructor Create; override;
  end;

  { TGoogleBarChart }

  TGoogleBarChart = class(TJSGoogleChart)
    constructor Create; override;
    function jsCodeOptionsVariable: string; override;
    class function packageLoaderJS: string; override;
  end;

  { TGoogleColumnChart }

  TGoogleColumnChart = class(TGoogleBarChart)
        constructor Create; override;
  end;

  { TGoogleTimelineChart }
  {https://developers.google.com/chart/interactive/docs/gallery/timeline?hl=en#overview
    To use Timelines, every series that you add must have these colums
        1) Row Name             : string
        2) Bar Label [optional] : string
        3) Start                : DateTime
        4) End                  : DateTime

    Don't use multiple series.

    ---------------
    Example (code):
    ---------------
            _timeLineChart:= TGoogleTimelineChart.Create;
            with _timeLineChart do
            begin
                Element := 'timeline';
                Name    := 'OperationsGraph';
                with series('A') do
                begin
                    addColumn('Op',     jstypeString);
                    addColumn('Name',   jstypeString);
                    addColumn('From',   jstypeDateTime);
                    addColumn('To',     jstypeDateTime);
                    with newRow do
                    begin
                        add('Op', 'P');
                        add('Name', 'Pumping');
                        add('From', date('2021-03-01T13:05:2'));
                        add('To',   date('2021-03-01T13:10:0'));
    				end;
                end;
            end;
  }
  TGoogleTimelineChart = class(TJSGoogleChart)
    class function packageLoaderJS: string; override;
    constructor Create; override;
  end;

  { TGooglePieChart }

  { TGooglePieTextStyle }

  TGooglePieTextStyle = class
    color: string;
    fontName: string;
    fontSize: string;
    function JSONObj: TDynaJSONObject;
  end;

  { TGooglePieChartSlice }

  TGooglePieChartSlice = class
  private
    myTextStyle: TGooglePieTextStyle;

  public
    sliceIndex: word;
    color: string;
    offset: string;
  public
    constructor Create;
    destructor Destroy; override;

    function JSONObj: TDynaJSONObject;
  end;

  TGooglePieChartSlicesListBase = specialize GenericHashObjectList<TGooglePieChartSlice>;

  { TGooglePieChartSlices }

  TGooglePieChartSlices = class(TGooglePieChartSlicesListBase)
    function JSONObj: TDynaJSONObject;
    function asJSON: string;
  end;

  TGooglePieChart = class(TJSGoogleChart)
  public
    type

  protected
    const __IS3D = 'is3D';

  private
      mySlices : TGooglePieChartSlices;
	  function Get_3D: boolean;
	  procedure Set_3D(const _value: boolean);
      function jsCodeOptionsVariable: string; override;

  public
    class function packageLoaderJS: string; override;
    constructor Create; override;
    destructor Destroy; override;

  public
    property _3D: boolean read Get_3D write Set_3D;
    function slices: TGooglePieChartSlices;
  end;

implementation


{ TGooglePieChartSlices }

function TGooglePieChartSlices.JSONObj: TDynaJSONObject;
var
	i: Integer;
	_slice: TGooglePieChartSlice;

begin
    Result:= TDynaJSONObject.Create;
    for i := 0 to pred(Count) do
    begin
        _slice := Items[i];
        Result.obj('slices').add(Names[i], _slice.JSONObj);
	end;
end;

function TGooglePieChartSlices.asJSON: string;
var
	_jsonObj: TDynaJSONObject;
begin
    _jsonObj := JSONObj();
    Result:= _jsonObj.AsJSON;
    _jsonObj.Free;
end;

{ TGooglePieChartSlice }

constructor TGooglePieChartSlice.Create;
begin
    inherited;
    myTextStyle:= TGooglePieTextStyle.Create;
end;

destructor TGooglePieChartSlice.Destroy;
begin
    myTextStyle.Free;
    inherited;
end;

function TGooglePieChartSlice.JSONObj: TDynaJSONObject;
var
	_textStyleObj: TDynaJSONObject;
begin
    Result := TDynaJSONObject.CreateJS;
    with Result do
    begin
        if not color.IsEmpty then
            add('color', color);

        if not offset.IsEmpty then
            add('offset', offset);

        _textStyleObj := myTextStyle.JSONObj;

        if _textStyleObj.Count > 0 then
            add('textStyle',_textStyleObj)
        else
            _textStyleObj.Free;

	end;
end;

{ TGooglePieTextStyle }

function TGooglePieTextStyle.JSONObj: TDynaJSONObject;
begin

    Result := TDynaJSONObject.CreateJS;
    with Result do
    begin
        if not color.isEmpty then
            add('color', color);

        if not fontName.isEmpty then
            add('fontName', fontName);

        if not fontSize.isEmpty then
            add('fontSize', fontSize);
	end;

end;


{ TGooglePieChart }

function TGooglePieChart.Get_3D: boolean;
begin
    Result:= options.Booleans[__IS3D];
end;

procedure TGooglePieChart.Set_3D(const _value: boolean);
begin
    options.Booleans[__IS3D] := _value;
end;

function TGooglePieChart.jsCodeOptionsVariable: string;
var
	i: Integer;
	_slice: TGooglePieChartSlice;
	_slicesObj: TDynaJSONObject;
begin

    if myOptions.IndexOfName('slices') >= 0 then
        myOptions.Delete('slices');

    _slicesObj := TDynaJSONObject.CreateJS;

    myOptions.Add('slices', _slicesObj);

    for i := 0 to pred(slices.Count) do
    begin
        _slice := slices.Items[i];
        _slicesObj.Add(slices.Names[i], _slice.JSONObj);;
	end;

	Result:=inherited jsCodeOptionsVariable;
end;

class function TGooglePieChart.packageLoaderJS: string;
begin
	Result:=inherited packageLoaderJS;
end;

constructor TGooglePieChart.Create;
begin
	inherited Create;
    ChartType:= jschartPie;
    options.Add(__IS3D, False);
    mySlices := TGooglePieChartSlices.Create();
end;

destructor TGooglePieChart.Destroy;
begin
    mySlices.Free;
	inherited Destroy;
end;

function TGooglePieChart.slices: TGooglePieChartSlices;
begin
    Result:= mySlices;
end;

{ TGoogleColumnChart }

constructor TGoogleColumnChart.Create;
begin
	inherited Create;
    ChartType:= jschartColumn;
end;

{ TGoogleBarChart }

constructor TGoogleBarChart.Create;
begin
	inherited Create;
    ChartType:= jschartBar;
end;

function TGoogleBarChart.jsCodeOptionsVariable: string;
begin
	Result:=inherited jsCodeOptionsVariable;
end;

class function TGoogleBarChart.packageLoaderJS: string;
begin
	    Result:= 'google.charts.load(''current'',{''packages'':[''corechart'', ''bar'']});';
end;

{ TGoogleTimelineChart }

class function TGoogleTimelineChart.packageLoaderJS: string;
begin
    Result:= 'google.charts.load(''current'',{''packages'':[''timeline'']});';
end;

constructor TGoogleTimelineChart.Create;
begin
	inherited Create;
    ChartType:= jschartTimeline;
end;


{ TGoogleLineChart }

constructor TGoogleLineChart.Create;
begin
	inherited Create;
    ChartType:= jschartLine;
end;

{ TJSGoogleChart }

procedure TJSGoogleChart.setautoDimensions(const _autoDimensions: boolean);
begin
    autoHeight:= _autoDimensions;
    autoWidth := _autoDimensions;
end;

function TJSGoogleChart.getAutoDimensions: boolean;
begin
    Result:= autoHeight and autoWidth;
end;

procedure TJSGoogleChart.setautoHeight(const _autoHeight: boolean);
begin
	if myautoHeight=_autoHeight then Exit;
	myautoHeight:=_autoHeight;
end;

procedure TJSGoogleChart.setautoWidth(const _autoWidth: boolean);
begin
	if myautoWidth=_autoWidth then Exit;
	myautoWidth:=_autoWidth;
end;

function TJSGoogleChart.drawFunctionName: string;
var
	i: Integer;
	_name: string;
begin
    _name := 'F';
    if Name.IsEmpty then
        _name := _name + genRandomKey(4)
    else for i := 1 to Name.Length do
    begin
        if Name[i] in ['A'..'Z', 'a'..'z'] then
            _name:= Result + Name[i];
	end;
	Result:= Format('draw_%s',[_name]);
end;

procedure TJSGoogleChart.setChartType(const _ChartType: TJSChartType);
begin
	inherited setChartType(_ChartType);
    case _ChartType of
        jschartUndefined: ;
        jschartLine:        myChartObjectName:= 'LineChart';
        jschartBar:         myChartObjectName:= 'BarChart';
        jschartColumn:      myChartObjectName:= 'ColumnChart';
        jschartArea:        myChartObjectName:= 'AreaChart';
        jschartScatter:     myChartObjectName:= 'ScatterChart';
        jschartHistogram:   myChartObjectName:= 'Histogram';
        jschartPie:         myChartObjectName:= 'PieChart';
        jschartTimeline:    myChartObjectName:= 'Timeline';
    end;

    if myChartObjectName.isEmpty then
        myChartObjectName := 'LineChart';
end;

class function TJSGoogleChart.CDNLinks: THtmlElementArray;
begin
    SetLength(Result, 1);
    Result[0]:= THTMLScript.Create;
    with Result[0] as THtmlScript do
        setSrc('https://www.gstatic.com/charts/loader.js');

    //Result[1]:= THTMLScript.Create;
    //with Result[1] as THtmlScript do
    //    setSrc('https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.27.0/locale/af.min.js');
end;

function TJSGoogleChart.options: TDynaJSONObject;
begin
    Result:= myOptions;
end;

function TJSGoogleChart.code: string;
begin
    Result:= jsCodeOnLoadFunction + jsCodeDrawFunction;
end;

class function TJSGoogleChart.packageLoaderJS: string;
begin
    Result:= 'google.charts.load(''current'',{''packages'':[''corechart'']});';
end;

function TJSGoogleChart.jsCodeDrawFunction: string;
begin
    Result:= jsFunc(drawFunctionName,'',

    {Function body}
    jsCodeDataVariable +
    jsCodeOptionsVariable +
    jsCodeChartVariable +
    Format('%s.draw(%s,%s);', [myVarChart, myVarData, myVarOptions]),

    {function signature type}
    jsfuncNamed
    );
end;

function TJSGoogleChart.jsCodeOnLoadFunction: string;
begin
    Result:= Format('google.charts.setOnLoadCallback(%s);', [drawFunctionName]);

end;

function TJSGoogleChart.jsCodeDataVariable: string;
var
  _d: TDynaJSONObject;
  i, j, k : integer;
  _chartData: TJSChartData;
  _row: TJSONObject;
  _column: TJSChartDataColumn;
  _dt: TDateTime;

  function getTypeName(_t: JSPrimitiveDataType) : string;
  begin
      case _t of
          jstypeBoolean: Result:= 'boolean';
          jstypeNumber, jstypeBigInt, jstypeFloat:  Result:= 'number' ;
          jstypeDate: Result:= 'date';
          jstypeTime: Result:= 'timeofday';
          jstypeDateTime: Result:= 'datetime';
          jstypeTimeOfDay: Result:= 'timeofday';
          else Result:= 'string';
      end;
  end;

begin
(*
   var data = new google.visualization.DataTable(
   {
     cols: [{id: 'task', label: 'Employee Name', type: 'string'},
            {id: 'startDate', label: 'Start Date', type: 'date'}],
     rows: [{c:[{v: 'Mike'}, {v: new Date(2008, 1, 28), f:'February 28, 2008'}]},
            {c:[{v: 'Bob'}, {v: new Date(2007, 5, 1)}]},
            {c:[{v: 'Alice'}, {v: new Date(2006, 7, 16)}]},
            {c:[{v: 'Frank'}, {v: new Date(2007, 11, 28)}]},
            {c:[{v: 'Floyd'}, {v: new Date(2005, 3, 13)}]},
            {c:[{v: 'Fritz'}, {v: new Date(2011, 6, 1)}]}
           ]
   }
)
*)

    _d:= TDynaJSONObject.CreateJS;

    {loop through series}
    for i:= 0 to pred(seriesCount) do
    begin
        _chartData := series(i);
        {loop through columns}
        for j:= 0 to pred(_chartData.columCount) do
        begin
            _column := _chartData.columns.Items[j];
		    with _d.addObjToArray('cols') do
		    begin
                {id}
                if _column.id.IsEmpty then
		            add('id',   Format('col%d%d',[i, j]))
                else
                    add('id',_column.id);
                {label}
		        add('label',_column.name);
                {type}
		        add('type', getTypeName(_column.dataType));
                {role}
                if not _column.role.IsEmpty then
                    add('role', _column.role);
			end;
		end;

        {loop through rows}
        for j:= 0 to pred(_chartData.rowCount) do
        begin

            _row:= _chartData.data.Items[j] as TJSONObject;

            {Add a row to array "rows" as object "c"}
	        with _d.addObjToArray('rows') do
	        begin
	            {loop through the columns}
	            for k:= 0 to pred(_chartData.columCount) do
	            begin
	                _column := _chartData.columns.Items[k];

	                case _column.datatype of
	                    jstypeBoolean: addObjToArray('c').add('v', _row.Booleans[_column.name]);
	                    jstypeFloat:   addObjToArray('c').add('v', _row.Floats[_column.name]);
	                    jstypeNumber:  addObjToArray('c').add('v', _row.Integers[_column.name]);
	                    jstypeBigInt:  addObjToArray('c').add('v', _row.Int64s[_column.name]);

	                    jstypeDate: begin
	                        _dt:= FloatToDateTime(_row.Floats[_column.name]);
	                        addObjToArray('c').addJSCode('v', jsNewDate(_dt));
						end;

						jstypeDateTime: begin
	                        _dt:= FloatToDateTime(_row.Floats[_column.name]);
	                        addObjToArray('c').addJSCode('v', jsNewDateTime(_dt));
						end;

	                    jstypeTime:begin
	                        _dt:= FloatToDateTime(_row.Floats[_column.name]);
	                        addObjToArray('c').add('v', jsCodeTimeOfDay(_dt));
						end;

	                    jstypeTimeOfDay:begin
	                        _dt:= FloatToDateTime(_row.Floats[_column.name]);
	                        addObjToArray('c').add('v', jsCodeTimeOfDay(_dt));
						end;

	                    else
	                      {consider the data as a string value}
	                      addObjToArray('c').add('v', _row.Strings[_column.name]);
					end;
			    end;
			end;
		end;
	end;

    Result:= Format('var %s = new google.visualization.DataTable(%s);', [myVarData, _d.AsJSON]);

    _d.Free;
end;

function TJSGoogleChart.jsCodeOptionsVariable: string;
begin
    if autoHeight then
        myOptions.putJS('height', jsFunc('',format('return document.getElementById(''%s'').parentElement.offsetHeight',[element]), {immediate} true));

    if autoWidth then
        myOptions.putJS('width', jsFunc('',format('return document.getElementById(''%s'').parentElement.offsetWidth',[element]), {immediate} true));


    Result:= Format('var %s = %s;', [myVarOptions, myOptions.AsJSON]);
end;

function TJSGoogleChart.jsCodeChartVariable: string;
begin
    Result:= Format('var %s = new google.visualization.%s(document.getElementById(''%s''));', [myVarChart, myChartObjectName, element]);
end;

function TJSGoogleChart.jsNewDate(_dt: TDateTime): string;
var
  y, m, d: word;
begin
    DecodeDate(_dt, y,m,d);
    Result:= format('new Date(%d, %d, %d)', [y, m, d]);
end;

function TJSGoogleChart.jsNewDateTime(_dt: TDateTime): string;
var
  y, m, d, h, n, s, ms: word;
begin
    DecodeDate(_dt, y,m,d);
    DecodeTime (_dt, h, n, s, ms);
    Result:= format('new Date(%d, %d, %d, %d, %d, %d, %d)', [y, m, d, h, n, s, ms]);
end;

function TJSGoogleChart.jsCodeTimeOfDay(_dt: TDateTime): TJSONArray;
var
  h, n, s, ms: word;
begin
    DecodeTime (_dt, h, n, s, ms);
    Result:= TJSONArray.Create([h, n, s, ms]);
end;

constructor TJSGoogleChart.Create;
begin
    inherited;
    myOptions   := TDynaJSONObject.CreateJS;
    myVarChart  := 'chart';
    myVarOptions:= 'options';
    myVarData   := 'data';
    autoDimensions:= false;
end;

destructor TJSGoogleChart.Destroy;
begin
    myOptions.Free;
	inherited Destroy;
end;
end.

