unit sugar.csshelper;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

const
{right padded with space for concatenate}
    left = 'left ';
    center = 'center ';
    right = 'right ';
    top = 'top ';
    bottom = 'bottom ';
    farthest_side = 'farthest-side ';
    closest_side  = 'closest-side ';
    initial = 'initial ';
    inherit = 'inherit ';
    unset = 'unset';
    evenodd = 'evenodd ';
    nonzero = 'nonzero ';



function addTerminator(_val: string) : string;

{CSS Function / Value builders}
function inset  (const _lengths: string=''; const _round: string = ''): string;
function circle (const _radius: string=''; const _at: string = ''): string;
function ellipse(const _radius1: string; const _radius2: string; const _at: string = ''): string;
{
polygon()

    polygon( [<fill-rule>,]? [<shape-arg> <shape-arg>]# )

    <fill-rule> represents the filling rule used to determine the interior of the polygon. Possible values are nonzero and evenodd. Default value when omitted is nonzero.

    Each pair argument in the list represents xi and yi - the x and y axis coordinates of the ith vertex of the polygon.

}
function polygon(const _points: string): string;
function path   (const _path: string): string;
(*
linear-gradient(
  [ <angle> | to <side-or-corner> ,]? <color-stop-list> )
  \---------------------------------/ \----------------------------/
    Definition of the gradient line        List of color stops

where <side-or-corner> = [ left | right ] || [ top | bottom ]
  and <color-stop-list> = [ <linear-color-stop> [, <color-hint>? ]? ]#, <linear-color-stop>
  and <linear-color-stop> = <color> [ <color-stop-length> ]?
  and <color-stop-length> = [ <percentage> | <length> ]{1,2}
  and <color-hint> = [ <percentage> | <length> ]
*)
function linearGradient(const _def: string): string;
function radialGradient(const _def: string): string;
function conicGradient(const _def: string): string;
function repeatingLinearGradient(const _def: string): string;
function repeatingRadialGradient(const _def: string): string;
function rgb(const _red, _green, _blue: word): string;
function rgba(const _red, _green, _blue: word;_alpha : string ='100%'): string;

{
Functional notation: hsl[a](H, S, L[, A])
    H (hue) is an <angle> of the color circle given in degs, rads, grads, or turns in CSS Color Module Level 4. When written as a unitless <number>, it is interpreted as degrees, as specified in CSS Color Module Level 3. By definition, red=0deg=360deg, with the other colors spread around the circle, so green=120deg, blue=240deg, etc. As an <angle>, it implicitly wraps around such that -120deg=240deg, 480deg=120deg, -1turn=1turn, etc.
    S (saturation) and L (lightness) are percentages. 100% saturation is completely saturated, while 0% is completely unsaturated (gray). 100% lightness is white, 0% lightness is black, and 50% lightness is “normal.”
    A (alpha) can be a <number> between 0 and 1, or a <percentage>, where the number 1 corresponds to 100% (full opacity).
}
function hsl(const _h, _s, _l : string): string; overload;
function hsla(const _h, _s, _l: string; _a: string = '100%'): string; overload;

{CSS UNITS}

{Relative to the font-size of the element
(2em means 2 times the size of the current font)}
function em(const _val: real): string;
{Relative to font-size of the root element}
function rem(const _val: real): string;
{Relative to the parent element}
function precent(const _val: real): string;
{pixels (1px = 1/96th of 1in) }
function px(const _val: real): string;
{centimeters}
function cm(const _val: real): string;
{millimeters}
function mm(const _val: real): string;
{inches}
function inch(const _val: real): string;
{points (1pt = 1/72 of 1in) }
function pt(const _val: real): string;
{picas (1pc = 12 pt) }
function pc(const _val: real): string;
{Relative to 1% of the height of the viewport}
function vh(const _val: real): string;
{Relative to 1% of the width of the viewport}
function vw(const _val: real): string;
{Relative to 1% of viewport's* larger dimension}
function vmax(const _val: real): string;
{Relative to 1% of viewport's* smaller dimension}
function vmin(const _val: real): string;

function deg(_deg: real): string;
function rad(_rad: real): string;
function turn(_turn: real): string;

function ms(_milliseconds:word): string;
function secs(_ms:real): string;

implementation

//uses
//    sugar.htmlbuilder;

{Adds a semicolon at the end if not already there}
function addTerminator(_val: string) : string;
const
  semicolon = ';';
var
    iLast : integer;
begin
    iLast:= _val.Length;
    _val := Trim(_val);
    if _val[iLast] <> semicolon then
      _val := _val + semicolon;
    Result := _val;
end;

function getUnit(_val : real; _unit: string) : string;
begin
    Result := FormatFloat('#.##', _val) + _unit;
end;

function inset(const _lengths: string; const _round: string): string;
begin
	  Result := 'inset(';

    Result := Result + Trim(_lengths);
    if _round.Length > 0 then
        Result := Result + ' round ' + _round;

    Result := Result + ');';
end;

function circle(const _radius: string; const _at: string): string;
begin
	  Result := 'circle(';

    Result := Result + Trim(_radius);
    if _at.Length > 0 then
        Result := Result + ' at ' + _at;

    Result := Result + ');';
end;

function ellipse(const _radius1: string; const _radius2: string;
		const _at: string): string;
begin
	  Result := 'ellipse(';

    Result := Result + Trim(_radius1) + ' ' + Trim(_radius1);
    if _at.Length > 0 then
        Result := Result + ' at ' + _at;

    Result := Result + ');';

end;

function polygon(const _points: string): string;
begin
	  Result := 'polygon(' + _points + ')';

end;

function path(const _path: string): string;
begin
	  Result := 'path('+  _path + ')';

end;

function linearGradient(const _def: string): string;
begin
  Result := 'linear-gradient(' + _def + ');';
end;

function radialGradient(const _def: string): string;
begin
    Result := 'radial-gradient(' + _def + ');';
end;

function conicGradient(const _def: string): string;
begin
    Result := 'conic-gradient(' + _def + ');';
end;

function repeatingLinearGradient(const _def: string): string;
begin
    Result := 'repeating-linear-gradient(' + _def + ');';
end;

function repeatingRadialGradient(const _def: string): string;
begin
    Result := 'repeating-radial-gradient(' + _def + ');';
end;

function rgb(const _red, _green, _blue: word): string;
begin
  Result := Format('rgb(%d,%d,%d) ',[_red, _green, _blue]);
end;

function rgba(const _red, _green, _blue: word; _alpha: string): string;
begin
    Result := Format('rgba(%d,%d,%d,%s) ',[_red, _green, _blue, _alpha]);
end;

function hsl(const _h, _s, _l: string): string;
begin
    Result := Format('hsl(%s, %s %s) ', [_h, _s, _l])
end;


function hsla(const _h, _s, _l: string; _a: string): string;
begin
    Result := Format('hsla(%s, %s %s, %s) ', [_h, _s, _l, _a])
end;


function em(const _val: real): string;
begin
    Result := getUnit(_val,'em ');
end;

function rem(const _val: real): string;
begin
    Result := getUnit(_val,'rem ');
end;


function precent(const _val: real): string;
begin
    Result := getUnit(_val,'% ');
end;

function px(const _val: real): string;
begin
    Result := getUnit(_val,'px ');
end;

function cm(const _val: real): string;
begin
    Result := getUnit(_val,'cm ');
end;

function mm(const _val: real): string;
begin
    Result := getUnit(_val,'mm ');
end;

function inch(const _val: real): string;
begin
    Result := getUnit(_val,'in ');
end;

function pt(const _val: real): string;
begin
    Result := getUnit(_val,'pt ');
end;

function pc(const _val: real): string;
begin
    Result := getUnit(_val,'pc ');
end;

function vh(const _val: real): string;
begin
    Result := getUnit(_val,'vh ');
end;

function vw(const _val: real): string;
begin
    Result := getUnit(_val,'vw ');
end;

function vmax(const _val: real): string;
begin
    Result := getUnit(_val,'vmax ');
end;

function vmin(const _val: real): string;
begin
    Result := getUnit(_val,'vmin ');
end;

function deg(_deg: real): string;
begin
  if (_deg < -360) or (_deg > 360) then
      raise Exception.Create(
      'deg(): ' + sLinebreak +
      'Value must be between -360 and +360. Parameter given is: '
       + FloatToStr(_deg));

  Result := getUnit(_deg, 'deg ');
end;

function rad(_rad: real): string;
begin
  Result := getUnit(_rad, 'rad ');
end;

function turn(_turn: real): string;
begin
  Result := getUnit(_turn, 'turn ');
end;

function ms(_milliseconds: word): string;
begin
    Result := getUnit(_milliseconds, 'ms');
end;

function secs(_ms: real): string;
begin
  Result := ms(trunc(_ms * 1000));
end;

end.
