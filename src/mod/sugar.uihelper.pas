unit sugar.uihelper;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Controls, ExtCtrls, StdCtrls, Graphics;
type

    TUIState  = (uiDefault, uiHighlight, uiWarning, uiError);
	{ TOnHover }

    TOnHover = class
        procedure OnMouseEnter(Sender: TObject);
        procedure OnMouseLeave(Sender: TObject);
    end;

    // Sets the font color depending on the UIState
    procedure uiState(_c: TControl; _s: TUIState; _hint: string = '');

    function onHover: TOnHover;
    procedure setHover(_lbl: TLabel);
    procedure uiShake(_c: TControl);

    procedure populateFromArray(_sl: TStrings;_ar: array of string);

    // extracts a real value from the edit box. if it is not a real value, it returns the default value
    function realVal(constref _edt: TEdit; const _default: real = 0.0): real;

    // Visual indication of the control when it enters;
    procedure onControlFocus(Sender:TObject);
    procedure onControlExit(Sender: TObject);


implementation
uses
    Forms;
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
  WAIT = 400;

var
  i: integer;
begin
    for i:= 0 to 14 do
    begin
        _c.Top := _c.Top + 3;
        Sleep(WAIT);
        Application.ProcessMessages;
        _c.Top := _c.Top - 3;
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
        _lbl.Font.Style:= _lbl.Font.Style + [fsUnderline];
        _lbl.Font.Color:= clHighlight;
        _lbl.Cursor := crHandPoint;
	end;
end;

procedure TOnHover.OnMouseLeave(Sender: TObject);
var
	_lbl: TLabel;
begin
    if Sender is TLabel then
    begin
        _lbl := Sender as TLabel;
        _lbl.Font.Style:= _lbl.Font.Style - [fsUnderline];
        _lbl.Font.Color:= clDefault;
        _lbl.Cursor := crDefault;
	end;
end;

procedure uiState(_c: TControl; _s: TUIState; _hint: string);
begin
    case _s of
    	uiDefault:   _c.Font.Color:= clDefault;
        uiHighlight: _c.Font.Color:= clHighlight;
        uiWarning:   _c.Font.Color:= clPurple;
        uiError:     _c.Font.Color := clRed;
    end;

    _c.Hint:= _hint;
    _c.ShowHint:= not _hint.IsEmpty;
end;

procedure populateFromArray(_sl: TStrings;
	_ar: array of string);
var
	_s: String;
begin
    for _s in _ar do
        _sl.Add(_s);
end;

function realVal(constref _edt: TEdit; const _default: real): real;
begin
    try
        Result := StrToFloat(_edt.Text);
	except
        Result:= _default;
	end;
end;

procedure onControlFocus(Sender: TObject);
begin
    if Sender is TFrame then
        TFrame(Sender).Color:= clHighlight;
end;

procedure onControlExit(Sender: TObject);
begin
    if Sender is TFrame then
        TFrame(Sender).Color:= clDefault;
end;


initialization
    myOnHover := TOnHover.Create;

finalization
    myOnHover.Free;
end.

