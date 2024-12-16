unit sugar.uihelper;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Controls, ComCtrls, ExtCtrls, StdCtrls, Graphics, Grids;

type

    TUIState  = (uiDefault, uiHighlight, uiWarning, uiError, uiHover);

	{ TOnHover }

    TOnHover = class
        procedure OnMouseEnter(Sender: TObject);
        procedure OnMouseLeave(Sender: TObject);
    end;

    // Sets the font color depending on the UIState
    procedure uiState(_c: TControl; _s: TUIState; _hint: string = '');
    procedure uiState(_arrc: array of TControl; _s: TUIState; _hint: string = '');

    function onHover: TOnHover;
    procedure setHover(_lbl: TLabel);
    procedure uiShake(_c: TControl);

    procedure populateFromArray(_sl: TStrings;_ar: array of string);

    // extracts a real value from the edit box. if it is not a real value, it returns the default value
    function realVal(constref _c: TWinControl; const _default: real = 0.0): real;
    function intVal (constref _c: TWinControl; const _default: integer = 0): integer;

    // Converts and sets values to the edit control
    procedure setVal(constref _c: TWinControl; const _value: integer); overload;
    procedure setVal(constref _c: TWinControl; const _value: double); overload;
    procedure setVal(constref _c: TWinControl; const _value: double; _format: TFormatSettings); overload;
    function  numAsStr(const _val: integer): string; overload;
    function  numAsStr(const _val: double; _precision: word = 2): string; overload;
    function  numAsStr(const _val: double; _format: TFormatSettings): string; overload;


    // Visual indication of the control when it enters;
    procedure onControlFocus(Sender:TObject);
    procedure onControlExit(Sender: TObject);

    // Only permits valid float value input
    procedure KeyDownFloatValues(Sender: TObject; var Key: Word; Shift: TShiftState);

    function gridSort(constref sg: TStringGrid; _cols: array of integer; _order: TSortOrder = soAscending): TStringGrid;
    procedure resizeGridCols(constref _grid: TStringGrid; _arrWidths: array of byte);  overload; // resizes the grid according to the percentages in the array

    function getCurrentWord(_memo: TMemo): string;

implementation
uses
    LCLType, LCLIntf, Forms, sugar.utils, sugar.collections, sugar.sort, Math, sugar.logger;
var
  myOnHover: TOnHover;

function onHover: TOnHover;
begin
    Result:= myOnHover;
end;

procedure setHover(_lbl: TLabel);
begin
    _lbl.OnMouseEnter:= @onHover.OnMouseEnter;
    _lbl.OnMouseLeave:= @onHover.OnMouseLeave;
end;

procedure uiShake(_c: TControl);
const
  WAIT = 200;

var
  i: integer;
begin
    for i:= 0 to 14 do
    begin
        _c.Top := _c.Top + 3;
        Sleep(WAIT);
        Application.ProcessMessages;
        _c.Top := _c.Top - 3;
        Application.ProcessMessages;
    end;
    _c.Visible:= true;

end;

{ TOnHover }

procedure TOnHover.OnMouseEnter(Sender: TObject);
var
	_lbl: TLabel;
begin
    if Sender is TLabel then
    begin
        _lbl := Sender as TLabel;
        _lbl.Font.Style := _lbl.Font.Style + [fsUnderline];
        _lbl.Font.Color := clHighlight;
        _lbl.Cursor     := crHandPoint;
	end;
end;

procedure TOnHover.OnMouseLeave(Sender: TObject);
var
	_lbl: TLabel;
begin
    if Sender is TLabel then
    begin
        _lbl := Sender as TLabel;
        _lbl.Font.Style := _lbl.Font.Style - [fsUnderline];
        _lbl.Font.Color := clDefault;
        _lbl.Cursor     := crDefault;
	end;
end;

procedure uiState(_c: TControl; _s: TUIState; _hint: string);
begin

    _c.Font.Style := _c.Font.Style - [fsBold];

    case _s of
    	uiDefault:   begin
            _c.Font.Color   := clDefault;
            if _c is TLabel then
                _c.color        := clNone
            else
                _c.color        := clDefault;
		end;

		uiHighlight: begin
            _c.Font.Color:= clHighlightText;

            if _c is TLabel then
                _c.color        := clNone
            else
                _c.color := clHighlight;
        end;

        uiWarning:   begin
            _c.Font.Color:= clPurple;
            if _c is TLabel then
                _c.color        := clNone
            else
                _c.color := clInfoBk;
		end;

		uiError:    begin
            _c.Font.Color := clRed;
            _c.Font.Style := _c.Font.Style + [fsBold];
            if _c is TLabel then
                _c.color        := clNone
            else
                _c.color := clInfoBk;
		end;
	end;
    _c.Hint:= _hint;
    _c.ShowHint:= not _hint.IsEmpty;
end;

procedure uiState(_arrc: array of TControl; _s: TUIState; _hint: string);
var
	_c: TControl;
begin
    for _c in _arrc do begin
        uiState(_c, _s, _hint);
	end;
end;

procedure populateFromArray(_sl: TStrings;
	_ar: array of string);
var
	_s: String;
begin
    for _s in _ar do
        _sl.Add(_s);
end;

function realVal(constref _c: TWinControl; const _default: real): real;
var
    _val: string = '';
begin
    if (_c is TEdit) then
        _val := TEdit(_c).Text
    else if (_c is TComboBox) then
        _val := TComboBox(_c).Text;

    if not _val.isEmpty then
        try
            Result := StrToFloat(_val);
	    except
            Result:= _default;
	    end
    else
        Result:= _default;
end;

function intVal(constref _c: TWinControl; const _default: integer): integer;
var
    _val: string = '';
begin
    if (_c is TEdit) then
        _val := TEdit(_c).Text
    else if (_c is TComboBox) then
        _val := TComboBox(_c).Text;

    if not _val.isEmpty then
        try
            Result := StrToInt(_val);
	    except
            Result:= _default;
	    end
    else
        Result:= _default;

end;

procedure setVal(constref _c: TWinControl; const _value: integer);
begin
    if (_c is TEdit) then
        TEdit(_c).Text :=  numAsStr(_value)
    else if (_c is TComboBox) then
        TComboBox(_c).Text :=  numAsStr(_value)
    else
        trip(Format('setVal: %s not supported', [_c.ClassName]));

end;

procedure setVal(constref _c: TWinControl; const _value: double);
begin
    if (_c is TEdit) then
        TEdit(_c).Text :=  numAsStr(_value)
    else if (_c is TComboBox) then
        TComboBox(_c).Text :=  numAsStr(_value)
    else
        trip(Format('setVal: %s not supported', [_c.ClassName]));
end;

procedure setVal(constref _c: TWinControl; const _value: double;
	_format: TFormatSettings);
begin
    if (_c is TEdit) then
        TEdit(_c).Text :=  numAsStr(_value)
    else if (_c is TComboBox) then
        TComboBox(_c).Text :=  numAsStr(_value, _format)
    else
        trip(Format('setVal: %s not supported', [_c.ClassName]));
end;

function numAsStr(const _val: integer): string;
begin
    try
        Result:= _val.ToString;
	except
        Result := 'NAN';
	end;
end;

function numAsStr(const _val: double; _precision: word): string;
begin
    try
        Result:= format('%.*f', [_precision, _val]);
	except
        Result := 'NAN';
	end;
end;

function numAsStr(const _val: double; _format: TFormatSettings): string;
begin
    try
        Result:= _val.ToString(_format);
	except
        Result := 'NAN';
	end;
end;


procedure onControlFocus(Sender: TObject);
begin
    if Sender is TFrame then
        TFrame(Sender).Color:= cl3DLight;
end;

procedure onControlExit(Sender: TObject);
begin
    if Sender is TFrame then
        TFrame(Sender).Color:= clDefault;
end;

procedure KeyDownFloatValues(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if key in [
            VK_RETURN,
            VK_0..VK_9,
            VK_NUMPAD0..VK_NUMPAD9,
            VK_SUBTRACT,
            VK_OEM_PERIOD,
            VK_OEM_COMMA,
            VK_BACK,
            VK_DELETE,
            VK_LEFT,
            VK_RIGHT,
            VK_END,
            VK_HOME,
            VK_TAB
            ] then

            // Do nothing

    else
        Key := 0;

end;

type

    TListOfTStringsBase = specialize GenericHashObjectList<TStrings>;
    TListOfTStrings = class (TListOfTStringsBase)
    end;



function gridSort(constref sg: TStringGrid; _cols: array of integer; _order: TSortOrder = soAscending): TStringGrid;
const
  TERM_DELIM = '|';
  DELIM = '•';
var
    _index  : TStringIndexMap;
    _rows   : TListOfTStrings;
	_r, _col: Integer;
    _sorted : TStringArray;
	_term, s: String;
    _delta  : integer;

begin
    Result := sg;

    if sg.FixedRows = sg.RowCount then exit; // No rows to sort.

    _index  := TStringIndexMap.Create();
    _rows   := TListOfTStrings.Create(true);

    try

        for _r := sg.FixedRows to pred(sg.RowCount) do
        begin
            _term := '';

            for _col in _cols do begin
                if not InRange(_col, 0, pred(sg.ColCount)) then continue;
                if not _term.isEmpty then _term := _term + TERM_DELIM;
                _term := _term + sg.Cells[_col, _r];
			end;
            // Append row number to the
			_term := Format('%s' + TERM_DELIM + '%d',[_term, _r]);

            _index.idx[_term] := _r;
            _rows.add(_term, clone(sg.rows[_r]));

		end;

        _sorted := sortList(_index.getNames(DELIM), DELIM);


        case _order of
            soAscending:  begin
                _r := sg.FixedRows;
                _delta := 1;
            end;
            soDescending: begin
                _r := pred(sg.RowCount);
                _delta := -1;
            end;
        end;

        sg.BeginUpdate;
        for s in _sorted do
        begin
            sg.Rows[_r].AddStrings(_rows.get(s), true); // This adds the objects as well
            _r := _r + _delta; {increments or decrements depending on sorting order}
		end;
        sg.EndUpdate;

	finally
        _index.Free;
        _rows.Free;
	end;
end;


procedure resizeGridCols(constref _grid: TStringGrid; _arrWidths: array of byte
	);
const
  __GRID_BUFFER = 4;

var
    _colCount: byte;
	_col: TGridColumn;
	_width, _i: Integer;
begin
    _colCount := Min(Length(_arrWidths), _grid.ColCount); // To loop over only the colums that are available.
    _width := _grid.Width - __GRID_BUFFER - GetSystemMetrics(SM_CXVSCROLL); // to prevent the scroll bar from showing;
    for _i := 0 to pred(_colCount) do begin
        _col := _grid.Columns.Items[_i];
        _col.Width:= trunc(_width * (_arrWidths[_i]/100));
	end;
end;

function getCurrentWord(_memo: TMemo): string;
var
	_line, _col: LongInt;
    _startPos, _endPos: sizeInt;
begin
    _line  := _memo.CaretPos.Y;
    _col   := succ(_memo.CaretPos.X);
    Result := strBetween(_memo.Lines.Strings[_line], _col, __whitespace, __whitespace, _startPos, _endPos);
end;


initialization
    myOnHover := TOnHover.Create;

finalization
    myOnHover.Free;
end.

