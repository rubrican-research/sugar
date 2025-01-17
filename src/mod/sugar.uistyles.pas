unit sugar.uistyles;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Graphics, controls, StdCtrls, ComCtrls,ExtCtrls, sugar.collections;
type

	{ TUIStyle }

    TUIStyle = class
        name  : string;
        align : TAlign;
        alignment: TAlignment;
        autoSize: boolean;
        color: TColor;
        Font: TFont;
        BorderSpacing: TControlBorderSpacing;
        BorderStyle : TControlBorderStyle;
        BorderWidth: TBorderWidth;

        textStyle: TTextStyle; // Not used yet

        function assign(constref _source: TUIStyle): TUIStyle;
        function import(constref _source: TControl): TUIStyle; // stores the style from the source.
        function apply(constref _c: TControl): TUIStyle;
        function apply(constref _arrc: array of TControl): TUIStyle;
        function clone: TUIStyle;

        constructor Create;
        destructor Destroy; override;

	end;

	{ TUIStyles }

    TUIStyles = class(specialize GenericHashObjectList<TUIStyle>)
        function get(const s: shortstring): TUIStyle; override;
        function add(const AName: shortstring; AObject: TUIStyle): integer;
			reintroduce;
	end;

    function styles: TUIStyles;
implementation

var
    myUIStyles: TUIStyles;

function styles: TUIStyles;
begin
    Result := myUIStyles;
end;

function toLeftRight(alignment: TAlignment): TLeftRight;
begin
    case alignment of
    	taLeftJustify:  Result := taLeftJustify;
        taRightJustify: Result := taRightJustify;
        taCenter:       Result := taLeftJustify;
    end;
end;

procedure setAlignment(_c: TControl; alignment: TAlignment);
begin
    if (_c is TLabel) then
        TLabel(_c).Alignment := alignment
    else if (_c is TCustomStaticText) then
        TCustomStaticText(_c).Alignment := alignment
    else if (_c is TCustomCheckBox) then
        TCustomCheckBox(_c).Alignment := toLeftRight(alignment);

end;

procedure setBorderStyle(_c: TControl; borderStyle: TBorderStyle);
begin
    if _c is TCustomControl then
        TCustomControl(_c).BorderStyle:=borderStyle;
end;

procedure setBorderWidth(_c: TControl; borderWidth: TBorderWidth);
begin
    if _c is TWinControl then
        TWinControl(_c).BorderWidth := BorderWidth;
end;

function getAlignment(_c: TControl): TAlignment;
begin
    Result := taLeftJustify; // default
    if (_c is TLabel) then
        Result := TLabel(_c).Alignment
    else if (_c is TCustomStaticText) then
        Result := TCustomStaticText(_c).Alignment
    else if (_c is TCustomCheckBox) then
        Result := TCustomCheckBox(_c).Alignment;
end;

function getBorderStyle(_c: TControl): TBorderStyle;
begin
    if _c is TCustomControl then
        Result := TCustomControl(_c).BorderStyle
    else
        Result := bsNone;
end;

function getBorderWidth(_c: TControl): TBorderWidth;
begin
    if _c is TWinControl then
        Result := TWinControl(_c).BorderWidth
    else
        Result := 0;
end;

function defaultTextStyle: TTextStyle; forward;

function getTextStyle(_c: TControl): TTextStyle;
begin
    Result := defaultTextStyle;
end;

function defaultTextStyle: TTextStyle;
begin
    with result do begin
        Alignment := taLeftJustify;     // TextRect Only: horizontal alignment
        Layout    := tlTop;             // TextRect Only: vertical alignment
        SingleLine:= true;              // If WordBreak is false then process #13, #10 as
                                        // standard chars and perform no Line breaking.
        Clipping  := true;              // TextRect Only: Clip Text to passed Rectangle
        ExpandTabs:= true;              // Replace #9 by apropriate amount of spaces (default is usually 8)
        ShowPrefix:= false;             // TextRect Only: Process first single '&' per
                                        // line as an underscore and draw '&&' as '&'

        Wordbreak := false;             // TextRect Only: If line of text is too long
                                        //    too fit between left and right boundaries
                                        //    try to break into multiple lines between
                                        //    words
                                        //    See also EndEllipsis.

        Opaque    := false;             // TextRect: Fills background with current Brush
                                        // TextOut : Fills background with current
                                        //            foreground color

        SystemFont := False;            // Use the system font instead of Canvas Font
        RightToLeft:= false;            //For RightToLeft text reading (Text Direction)

        EndEllipsis:= true;             // TextRect Only: If line of text is too long
                                        //    to fit between left and right boundaries
                                        //    truncates the text and adds "..."
                                        //    If Wordbreak is set as well, Workbreak will
                                        //    dominate.
	end;
end;

{ TUIStyle }

function TUIStyle.assign(constref _source: TUIStyle): TUIStyle;
begin
    Result := Self;
    Result.name         := _source.name;
    Result.align        := _source.align;
    Result.alignment    := _source.alignment;
    Result.autoSize     := _source.autoSize;
    Result.color        := _source.color;
    Result.Font.Assign(_source.Font);
    Result.BorderSpacing.assign(_source.BorderSpacing);
    Result.BorderStyle  := _source.BorderStyle;
    Result.BorderWidth  := _source.BorderWidth;

    Result.textStyle    := _source.textStyle;
end;

function TUIStyle.import(constref _source: TControl): TUIStyle;
begin
    align        := _source.align;
    alignment    := getAlignment(_source);
    autoSize     := _source.autoSize;
    color        := _source.color;
    Font.Assign(_source.Font);
    BorderSpacing.assign(_source.BorderSpacing);
    BorderStyle  := getBorderStyle(_source);
    BorderWidth  := getBorderWidth(_source);
    textStyle    := getTextStyle(_source);

end;

function TUIStyle.apply(constref _c: TControl): TUIStyle;
begin
    Result := self;
    _c.Align:= align;
    setAlignment(_c, alignment);
    _c.AutoSize := autoSize;
    _c.Color:= color;
    _c.Font.Assign(Font);
    _c.BorderSpacing.Assign(BorderSpacing);
    setBorderStyle(_c, BorderStyle);
    setBorderWidth(_c, BorderWidth);
    if _c is TWinControl then begin
        TWinControl(_c).BorderWidth := BorderWidth;
	end;
end;

function TUIStyle.apply(constref _arrc: array of TControl): TUIStyle;
var
	_c: TControl;
begin
    for _c in _arrc do
        apply(_c);
    Result := self;
end;

function TUIStyle.clone: TUIStyle;
begin
    Result := TUIStyle.Create;
    Result.name        := name;
    Result.align       := align;
    Result.alignment   := alignment;
    Result.autoSize    := autoSize;
    Result.color       := color;
    Result.Font.Assign(Font);
    Result.BorderSpacing.assign(BorderSpacing);
    Result.BorderStyle := BorderStyle;
    Result.BorderWidth := BorderWidth;
end;

constructor TUIStyle.Create;
begin
    inherited Create;
    Font := TFont.Create;
    BorderSpacing := TControlBorderSpacing.Create(nil);
    TextStyle     := defaultTextStyle();
end;

destructor TUIStyle.Destroy;
begin
    Font.Free;
    BorderSpacing.Free;
	inherited Destroy;
end;

{ TUIStyles }

function TUIStyles.get(const s: shortstring): TUIStyle;
begin
	Result:=inherited get(s);
    {Force the style to have the same name as the key}
    if Result.name <> s then
        Result.name:= s;
end;

function TUIStyles.add(const AName: shortstring; AObject: TUIStyle): integer;
begin
    if AObject.name <> AName then AObject.name := AName;
	Result:=inherited add(AName, AObject);
end;

initialization
    myUIStyles.Create();

finalization
    myUIStyles.Free;

end.

