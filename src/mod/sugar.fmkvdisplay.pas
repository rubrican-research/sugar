unit sugar.fmkvdisplay;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, fgl, Graphics, StdCtrls;

type

	{ TFMKVDisplay }

	{ TKVLabel }

    TKVLabel = class
	private
		function getKey: string;
		function getValue: string;
		procedure setKey(const _value: string);
		procedure setValue(const _value: string);
	public
        keylabel: TLabel;
        valLabel: TLabel;
        property key: string read getKey write setKey;
        property value: string read getValue write setValue;
        function clone: TKVLabel;
	end;

    TKVMapBase = specialize TFPGMapObject<string, TKVLabel>;

	{ TKVMap }

    TKVMap = class(TKVMapBase)
	private
		myKeyWidth: integer;
		myLPadding: integer;
		myRPadding: integer;
		myValProportion: word;
		mywidth: integer;
		procedure setHPadding(const _value: integer);
		procedure SetKeyWidth(const _value: integer);
		procedure setRPadding(const _value: integer);
        procedure SetValProportion(const _value: word);
        procedure Setwidth(const _value: integer);

	public
        constructor Create(AFreeObjects: Boolean);
        procedure rename(const _oldKey: string; const _newKey: string);
        procedure changeKey(_index: integer; const _newKey: string);
        procedure refreshProportion;
    public
        property width: integer read mywidth write Setwidth;
        property KeyWidth: integer read myKeyWidth write SetKeyWidth;
        property ValProportion: word read myValProportion write SetValProportion;
        property LPadding : integer read myLPadding write setHPadding; // Left padding
        property RPadding : integer read myRPadding write setRPadding; // Right padding
	end;


    TFMKVDisplay = class(TFrame)
		lblKey: TLabel;
		lblValue: TLabel;
		procedure FrameResize(Sender: TObject);

    private
		myKeyWidth: integer;
		myLPadding: integer;
        myKVMap: TKVMap;
		myKeyFont: TFont;
		myRPadding: integer;
		myValFont: TFont;
		myValProportion: word;
		function Getkeys(_index: integer): string;
        function Getvalue(_key: string): string;
		function Getvalues(_index: integer): string;
		procedure setLPadding(const _value: integer);
		procedure SetKeyFont(const _value: TFont);
		procedure Setkeys(_index: integer; const _value: string);
		procedure SetKeyWidth(const _value: integer);
		procedure setRPadding(const _value: integer);
		procedure SetValFont(const _value: TFont);
		procedure SetValProportion(const _value: word);
		procedure Setvalue(_key: string; const _value: string);
		procedure Setvalues(_index: integer; const _value: string);

        function geKVLabel(const _key: string): TKVLabel;                     // Only returns KV if it exists

    public
        constructor Create(TheOwner: TComponent); override;
        destructor Destroy; override;

    public
        property LPadding : integer read myLPadding write setLPadding; // Left padding
        property RPadding : integer read myRPadding write setRPadding; // Right padding
        property KeyWidth : integer read myKeyWidth write SetKeyWidth;
        property ValProportion: word read myValProportion write SetValProportion; // Proportion of key to value in display.
        property KeyFont: TFont read myKeyFont write SetKeyFont;
        property ValFont: TFont read myValFont write SetValFont;
        property value[_key: string]: string read Getvalue write Setvalue;
        property keys[_index: integer]: string read Getkeys write Setkeys;
        property values[_index: integer]: string read Getvalues write Setvalues;

        function keyLabel(_key: string): TLabel;
        function keyLabel(_index: integer): TLabel;
        function valLabel(_key: string): TLabel;
        function valLabel(_index: integer): TLabel;

        function kv(const _key: string; const _val: string): TFMKVDisplay; // Creates a KV if not found


        function delete(_key: string): boolean; overload;
        function delete(_index: integer): boolean; overload;
        function keyExists(_key: string): boolean;
        function keyIndex(_key: string): integer;

        function newKVMap(const _key: string=''; const _value: string = ''): TKVLabel;
        function count: integer;

        procedure refreshProportion;
        procedure clear;

    end;

function clone(constref _src: TLabel): TLabel;
function assignFont(constref _src: TFont; constref _dest: TFont): TFont; // Destination

implementation
uses
    Math;

function clone(constref _src: TLabel): TLabel;
begin
    Result:= TLabel.Create(_src.Owner);
    with Result do begin
        align           := _src.align;
        Alignment       := _src.Alignment;
        Anchors         := _src.Anchors;
        Autosize        := _src.Autosize;
        with BorderSpacing do begin
            Around              :=  _src.BorderSpacing.Around;
            Bottom              :=  _src.BorderSpacing.Bottom;
            CellAlignHorizontal :=  _src.BorderSpacing.CellAlignHorizontal;
            CellAlignVertical   :=  _src.BorderSpacing.CellAlignVertical;
            InnerBorder         :=  _src.BorderSpacing.InnerBorder;
            Left                :=  _src.BorderSpacing.Left;
            Right               :=  _src.BorderSpacing.Right;
            Top                 :=  _src.BorderSpacing.Top;
		end;
        Caption         := _src.Caption;
        Color           := _src.Color;

        with Constraints do begin
            MaxHeight           := _src.Constraints.MaxHeight ;
            MaxWidth            := _src.Constraints.MaxWidth  ;
            MinHeight           := _src.Constraints.MinHeight ;
            MaxHeight           := _src.Constraints.MaxHeight ;
		end;

        Cursor          := _src.Cursor;
        DragCursor      := _src.DragCursor;
        DragKind        := _src.DragKind;
        DragMode        := _src.DragMode;
        Enabled         := _src.Enabled;
        FocusControl    := _src.FocusControl;

        assignFont(_src.Font, Font);

        Height  := _src.Height;
        HelpContext := _src.HelpContext;
        HelpKeyword := _src.HelpKeyword;
        HelpType    := _src.HelpType;
        Hint        := _src.Hint;
        Layout      := _src.Layout;
        Left        := _src.Left;
        OptimalFill := _src.OptimalFill;
        ParentBidiMode  := _src.ParentBiDiMode;
        ParentColor := _src.ParentColor;
        ParentShowHint := _src.ParentShowHint;
        PopupMenu   := _src.PopupMenu;
        ShowAccelChar:= _src.ShowAccelChar;
        ShowHint    := _src.ShowHint;
        Tag         := _src.Tag;
        Top         := _src.Top;
        Transparent := _src.Transparent;
        Visible     := _src.Visible;
        Width       := _src.Width;
        WordWrap    := _src.WordWrap;
	end;
end;

function assignFont(constref _src: TFont; constref _dest: TFont): TFont;
begin
    Result:= _dest;
    with _dest do begin
        CharSet		:= _src.CharSet;
        Color		:= _src.Color;
        Height		:= _src.Height;
        Name		:= _src.Name;
        Orientation	:= _src.Orientation;
        Pitch		:= _src.Pitch;
        Quality		:= _src.Quality;
        Size		:= _src.Size;
        Style		:= _src.Style;
	end;
end;

{$R *.lfm}

{ TKVLabel }

function TKVLabel.getKey: string;
begin
    Result:= keylabel.Caption;
end;

function TKVLabel.getValue: string;
begin
    Result:=valLabel.Caption;
end;

procedure TKVLabel.setKey(const _value: string);
begin
    keyLabel.Caption := _value;
end;

procedure TKVLabel.setValue(const _value: string);
begin
    valLabel.Caption := _value;
end;

function TKVLabel.clone: TKVLabel;
begin
    Result := TKVLabel.Create;
    Result.keylabel := keylabel;
    Result.valLabel := valLabel;
end;

{ TKVMap }

procedure TKVMap.Setwidth(const _value: integer);
begin
	if mywidth=_value then Exit;
	mywidth:=_value;
    refreshProportion;
end;

procedure TKVMap.SetValProportion(const _value: word);
begin
	if myValProportion=_value then Exit;
	myValProportion:=_value;
    refreshProportion;
end;

procedure TKVMap.setHPadding(const _value: integer);
begin
	if myLPadding=_value then Exit;
	myLPadding:=_value;
end;

procedure TKVMap.SetKeyWidth(const _value: integer);
begin
	if myKeyWidth=_value then Exit;
	myKeyWidth:=_value;
    refreshProportion;
end;

procedure TKVMap.setRPadding(const _value: integer);
begin
	if myRPadding=_value then Exit;
	myRPadding:=_value;
end;


constructor TKVMap.Create(AFreeObjects: Boolean);
begin
	inherited Create(AFreeObjects);
    Sorted:=True;
end;

procedure TKVMap.rename(const _oldKey: string; const _newKey: string);
var
  _i: integer;
begin
    if find(_oldKey, _i) then
    begin
        changeKey(_i, _newKey);
	end;
end;

procedure TKVMap.changeKey(_index: integer; const _newKey: string);
begin
    if InRange(_index, 0, pred(count)) then begin
        Sorted := False;
        Keys[_index] := _newKey;
        Data[_index].Key := _newKey;
        Sorted := True;
    end;

end;

procedure TKVMap.refreshProportion;
var
	_valWidth, _keyWidth: integer;
begin
    _valWidth := round (width * (ValProportion/100)) - (Round(RPadding/2) + RPadding * 2);
    if Count > 0 then begin
        with Data[0] do begin
	        if (Width>0) and (ValProportion>0) then begin
	            keylabel.Constraints.MinWidth := KeyWidth;
	            keylabel.Constraints.MaxWidth := KeyWidth;
	            valLabel.Constraints.MinWidth := _valWidth;
			end
			else
	        begin
	            keylabel.Constraints.MinWidth := 0;
	            keylabel.Constraints.MaxWidth := 0;
	            valLabel.Constraints.MinWidth := 0;
			end;
		end;
	end;
end;

{ TFMKVDisplay }

function TFMKVDisplay.Getvalue(_key: string): string;
var
  _i: integer;
begin
    if myKVMap.Find(_key, _i) then
        Result:= myKVMap.Data[_i].value
    else
        Result:= '';
end;

procedure TFMKVDisplay.FrameResize(Sender: TObject);
begin
    myKVMap.width:= Width;
end;

function TFMKVDisplay.Getkeys(_index: integer): string;
begin
    if InRange(_index, 0, pred(count)) then
        Result:= myKVMap.Keys[_index];
end;

function TFMKVDisplay.Getvalues(_index: integer): string;
begin
    if InRange(_index, 0, pred(count)) then
        Result:= myKVMap.Data[_index].value;
end;

procedure TFMKVDisplay.setLPadding(const _value: integer);
begin
	if myLPadding=_value then Exit;
	myLPadding:=_value;
    myKVMap.LPadding:=_value;
end;

procedure TFMKVDisplay.SetKeyFont(const _value: TFont);
begin
	if myKeyFont=_value then Exit;
	myKeyFont:=_value;
end;

procedure TFMKVDisplay.Setkeys(_index: integer; const _value: string);
begin
    myKVMap.changeKey(_index, _value);
end;

procedure TFMKVDisplay.SetKeyWidth(const _value: integer);
begin
	if myKeyWidth=_value then Exit;
	myKeyWidth:=_value;
    myKVMap.KeyWidth:=myKeyWidth;

    lblKey.Constraints.MinWidth:= _value;
    lblKey.Constraints.MaxWidth:= _value;

end;

procedure TFMKVDisplay.setRPadding(const _value: integer);
begin
	if myRPadding=_value then Exit;
	myRPadding:=_value;
    myKVMap.RPadding:=_value;
end;

procedure TFMKVDisplay.SetValFont(const _value: TFont);
begin
	if myValFont=_value then Exit;
	myValFont:=_value;
end;

procedure TFMKVDisplay.SetValProportion(const _value: word);
begin
	if myValProportion=_value then Exit;
	myValProportion:=_value;
    myKVMap.ValProportion:= myValProportion;
end;

procedure TFMKVDisplay.Setvalue(_key: string; const _value: string);
var
  _i : integer;
begin
    if myKVMap.Find(_key, _i) then
        myKVMap.KeyData[_key].value:=_value;
end;

procedure TFMKVDisplay.Setvalues(_index: integer; const _value: string);
begin
    if InRange(_index, 0, pred(count)) then
        myKVMap.Data[_index].value := _value;
end;

constructor TFMKVDisplay.Create(TheOwner: TComponent);
var
    _keyWidth, _valWidth: integer;
begin
	inherited Create(TheOwner);
    Name := '';
    myKVMap := TKVMap.Create(true);
    lblKey.Visible  := False;
    lblValue.Visible:= false;

    if lblKey.BorderSpacing.Around = 0 then begin
        LPadding := lblKey.BorderSpacing.Left;
        RPadding := lblKey.BorderSpacing.Right;
    end  else begin
        LPadding := lblKey.BorderSpacing.Around;
        RPadding := lblKey.BorderSpacing.Around;
	end;

    KeyWidth  := lblKey.Width   + LPadding + round(RPadding/2);
    _valWidth := lblValue.Width + round(RPadding/2) + RPadding;

    ValProportion   :=  round(_valWidth/Width * 100); { % }
end;


destructor TFMKVDisplay.Destroy;
begin
    myKVMap.Destroy;
	inherited Destroy;
end;

function TFMKVDisplay.keyLabel(_key: string): TLabel;
var
  _i : integer;
begin
    Result := nil;
    if myKVMap.Find(_key, _i) then
        Result:= myKVMap.KeyData[_key].keylabel;
end;

function TFMKVDisplay.keyLabel(_index: integer): TLabel;
begin
    Result := nil;
    if InRange(_index, 0, pred(count)) then
        Result:= myKVMap.Data[_index].keylabel;
end;


function TFMKVDisplay.valLabel(_key: string): TLabel;
var
  _i : integer;
begin
    Result := nil;
    if myKVMap.Find(_key, _i) then
        Result:= myKVMap.KeyData[_key].keylabel;
end;

function TFMKVDisplay.valLabel(_index: integer): TLabel;
begin
    Result := nil;
    if InRange(_index, 0, pred(count)) then
        Result:= myKVMap.Data[_index].keylabel;
end;


function TFMKVDisplay.kv(const _key: string; const _val: string): TFMKVDisplay;
var
  _i : integer;
  _isNew: boolean;
begin
    if not keyExists(_key) then begin
        {Add a new KV}
        myKVMap.Add(_key, newKVMap(_key, _val));
	end
	else
    begin
        geKVLabel(_key).value := _val;; // We are sure it exists here.
	end;
    Result := self;
end;

function TFMKVDisplay.geKVLabel(const _key: string): TKVLabel;
var
  _i : integer;

begin
    if myKVMap.Find(_key, _i) then
        Result := myKVMap.Data[_i]
    else
        Result := nil;
end;

function TFMKVDisplay.delete(_key: string): boolean;
begin
    Result := delete(keyIndex(_key));
end;

function TFMKVDisplay.delete(_index: integer): boolean;
begin
    Result := false;
    if inRange(_index, 0, pred(Count)) then begin
        removeControl(myKVMap.Data[_index].keylabel);
        removeControl(myKVMap.Data[_index].valLabel);
        myKVMap.Delete(_index);
        Result := true;
	end;
end;

function TFMKVDisplay.keyExists(_key: string): boolean;
var
  _i : integer;
begin
    Result:= myKVMap.Find(_key, _i);
end;

function TFMKVDisplay.keyIndex(_key: string): integer;
var
  _i: integer;
begin
    if myKVMap.Find(_key, _i) then
        Result := _i
    else
        Result := -1;
end;

function TFMKVDisplay.newKVMap(const _key: string; const _value: string
	): TKVLabel;
begin

    Result := TKVLabel .Create;
    Result.keylabel := clone(lblKey);
    Result.valLabel := clone(lblValue);

    InsertControl(Result.keylabel);
    InsertControl(Result.valLabel);

    Result.keylabel.Visible := true;
    Result.valLabel.Visible := true;
    Result.Key   := _key;
    Result.value := _value;
end;

function TFMKVDisplay.count: integer;
begin
    Result:= myKVMap.Count;
end;

procedure TFMKVDisplay.refreshProportion;
begin
    myKVMap.refreshProportion;
end;

procedure TFMKVDisplay.clear;
begin
    while count>0 do delete(0);
end;

end.

