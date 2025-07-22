unit frame.TagControl;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Graphics, Menus,
	fgl;

const
    BORDER_SPACING_AROUND = 7;
    TAG_PREFIX = '#';

type

    { TTagControl }

    TTagControl = class(TFrame)
        edtTag: TEdit;
        closeLabel: TLabel;
		mnuDelete: TMenuItem;
		mnuCopy: TMenuItem;
        Panel: TPanel;
		pMenu: TPopupMenu;
		tagLabel: TLabel;
        procedure edtTagEditingDone(Sender: TObject);
        procedure edtTagKeyPress(Sender: TObject; var Key: char);
		procedure mnuCopyClick(Sender: TObject);
		procedure MouseEnter(Sender: TObject);
		procedure MouseLeave(Sender: TObject);
        procedure tagLabelDblClick(Sender: TObject);
    private
		myborderColorDefault: TColor;
		myborderColorHovered: TColor;
		myborderColorSelected: TColor;
        myEditable: boolean;
        myOnClose: TNotifyEvent;
        myOnCloseQuery: TCloseQueryEvent;
		myonEnter: TNotifyEvent;
		myonExit: TNotifyEvent;
		myonSelect: TNotifyEvent;
        myOnTagTextChange: TNotifyEvent;
		myonUnselect: TNotifyEvent;
		myreadOnly: boolean;
		myselected: boolean;
        mytagPrefix: string;
        myHoverCount: integer;

        function getBackgroundColor: TColor;
        function getBorderColor: TColor;
		function getCanUnselect: boolean;
        function getEditable: boolean;
        function getIconColor: TColor;
        function getText: string;
        function getTextColor: TColor;
        procedure setBackgroundColor(const _value: TColor);
        procedure setBorderColor(const _value: TColor);
		procedure SetborderColorDefault(const _value: TColor);
		procedure SetborderColorHovered(const _value: TColor);
		procedure SetborderColorSelected(const _value: TColor);
        procedure setEditable(const _value: boolean);
        procedure setIconColor(const _value: TColor);
        procedure setOnClose(const _value: TNotifyEvent);
        procedure setOnCloseQuery(const _value: TCloseQueryEvent);
		procedure SetonEnter(const _value: TNotifyEvent);
		procedure SetonExit(const _value: TNotifyEvent);
		procedure SetonSelect(const _value: TNotifyEvent);
        procedure SetOnTagTextChange(const _value: TNotifyEvent);
		procedure SetonUnselect(const _value: TNotifyEvent);
		procedure SetreadOnly(const _value: boolean);
		procedure Setselected(const _value: boolean);
        procedure SettagPrefix(const _value: string);
        procedure setText(const _value: string);
        procedure settextColor(const _value: TColor);
    protected
        procedure DoOnCloseClick(Sender: TObject);
        procedure init;
        procedure incHoverCount;
        procedure decHoverCount;

    protected
        property editable: boolean read getEditable write setEditable; // Toggle between edit and display

    public
        constructor Create(TheOwner: TComponent); override;
        destructor Destroy; override;

        property tagPrefix: string read mytagPrefix write SettagPrefix;
        property Text: string read getText write setText;
        property textColor: TColor read getTextColor write setTextColor;
        property borderColor: TColor read getBorderColor write setBorderColor;
        property borderColorDefault: TColor read myborderColorDefault write SetborderColorDefault;
        property borderColorSelected: TColor read myborderColorSelected write SetborderColorSelected;
        property borderColorHovered: TColor read myborderColorHovered write SetborderColorHovered;

        property iconColor: TColor read getIconColor write setIconColor;
        property backgroundColor: TColor read getBackgroundColor
            write setBackgroundColor;

        property onClose: TNotifyEvent read myOnClose write setOnClose;
        property OnCloseQuery: TCloseQueryEvent
            read myOnCloseQuery write setOnCloseQuery;

        property OnTagTextChange: TNotifyEvent
            read myOnTagTextChange write SetOnTagTextChange;

        property readOnly: boolean read myreadOnly write SetreadOnly; // Permission to edit

        property selected: boolean read myselected write Setselected;
        property canUnselect: boolean read getCanUnselect;

        property onSelect: TNotifyEvent read myonSelect write SetonSelect;       // Calls when this is selected
        property onUnselect: TNotifyEvent read myonUnselect write SetonUnselect; // Called when this is unselected

        property onEnter: TNotifyEvent read myonEnter write SetonEnter;
        property onExit: TNotifyEvent read myonExit write SetonExit ;

    end;

    TTagControlClass = class of TTagControl;

    TTagControlList = class(specialize TFPGMapObject<string, TTagControl>)

    end;

    function ensureTagPrefix(const _prefix, _w: string): string;

implementation

{$R *.lfm}

uses
    LCLType, sugar.utils, sugar.uihelper, Math, clipbrd;

function ensureTagPrefix(const _prefix, _w: string): string;
begin
    Result := _w;
    if not (_prefix.isEmpty)  then
        if not _w.StartsWith(_prefix) then
            Result := _prefix + _w;
end;

{ TTagControl }

function TTagControl.getText: string;
begin
    case editable of
    	True    : Result := edtTag.Text ;
        False   : Result := tagLabel.Caption;
    end;
end;

function TTagControl.getTextColor: TColor;
begin
    Result := tagLabel.Font.Color;
end;

procedure TTagControl.setBackgroundColor(const _value: TColor);
begin
    color := _value;
end;

function TTagControl.getBorderColor: TColor;
begin
    Result := Panel.BevelColor;
end;

function TTagControl.getCanUnselect: boolean;
begin
    Result := not myEditable;
end;

function TTagControl.getEditable: boolean;
begin
    Result := myEditable;
end;

procedure TTagControl.tagLabelDblClick(Sender: TObject);
begin
    if not readOnly then // Don't allow edit without permission
        editable := True;
end;

procedure TTagControl.edtTagEditingDone(Sender: TObject);
begin
    if editable then
        editable := False;
end;

procedure TTagControl.edtTagKeyPress(Sender: TObject; var Key: char);
begin
    if key in __WHITESPACE_CHARS then
        key := #0;
end;

procedure TTagControl.mnuCopyClick(Sender: TObject);
begin
    Clipboard.AsText := text;
end;

procedure TTagControl.MouseEnter(Sender: TObject);
begin
    incHoverCount;
end;

procedure TTagControl.MouseLeave(Sender: TObject);
begin
    decHoverCount;
end;

function TTagControl.getBackgroundColor: TColor;
begin
    Result := color;
end;

function TTagControl.getIconColor: TColor;
begin
    Result := closeLabel.Font.Color;
end;

procedure TTagControl.setBorderColor(const _value: TColor);
begin
    Panel.BevelColor   := _value;
end;

procedure TTagControl.SetborderColorDefault(const _value: TColor);
begin
	if myborderColorDefault=_value then Exit;
	myborderColorDefault:=_value;
end;

procedure TTagControl.SetborderColorHovered(const _value: TColor);
begin
	if myborderColorHovered=_value then Exit;
	myborderColorHovered:=_value;
end;

procedure TTagControl.SetborderColorSelected(const _value: TColor);
begin
	if myborderColorSelected=_value then Exit;
	myborderColorSelected:=_value;
end;

procedure TTagControl.setEditable(const _value: boolean);
begin

    myEditable := _value;
    case myEditable of

        True: begin
            edtTag.Text := tagLabel.Caption;
            edtTag.Width := tagLabel.Width;
            tagLabel.Visible := False;
            edtTag.Visible := True;
            edtTag.SetFocus;
        end;

        False: begin
            tagLabel.Cation := ensureTagPrefix(tagPrefix, edtTag.Text);
            tagLabel.Visible := True;
            edtTag.Visible := False;
        end;
    end;

end;

procedure TTagControl.setIconColor(const _value: TColor);
begin
    closeLabel.Font.Color := _value;
end;


procedure TTagControl.setOnClose(const _value: TNotifyEvent);
begin
    if myonClose = _value then Exit;
    myonClose := _value;
    mnuDelete.OnClick := _value;
end;

procedure TTagControl.setOnCloseQuery(const _value: TCloseQueryEvent);
begin
    if myOnCloseQuery = _value then Exit;
    myOnCloseQuery := _value;
end;

procedure TTagControl.SetonEnter(const _value: TNotifyEvent);
begin
	if myonEnter=_value then Exit;
	myonEnter:=_value;
end;

procedure TTagControl.SetonExit(const _value: TNotifyEvent);
begin
	if myonExit=_value then Exit;
	myonExit:=_value;
end;

procedure TTagControl.SetonSelect(const _value: TNotifyEvent);
begin
	if myonSelect=_value then Exit;
	myonSelect:=_value;
end;

procedure TTagControl.SetOnTagTextChange(const _value: TNotifyEvent);
begin
    if myOnTagTextChange = _value then Exit;
    myOnTagTextChange := _value;
end;

procedure TTagControl.SetonUnselect(const _value: TNotifyEvent);
begin
	if myonUnselect=_value then Exit;
	myonUnselect:=_value;
end;

procedure TTagControl.SetreadOnly(const _value: boolean);
begin
	if myreadOnly=_value then Exit;
	myreadOnly:=_value;

    if myReadOnly and editable then
        editable := false;
end;

procedure TTagControl.Setselected(const _value: boolean);
begin

    case _value of
    	True    : begin
            borderColor := borderColorSelected;
            mySelected := true;
		end;

        False   : begin
            if canUnselect then begin
                borderColor := borderColorDefault;
                mySelected := false;
			end else begin
                mySelected := true;
                raise Exception.Create('TTagControl:: canUnselect() returned false so this TagControl cannot be unselected at this time.');
			end;
		end;
    end;
end;

procedure TTagControl.SettagPrefix(const _value: string);
begin
    if mytagPrefix = _value then Exit;
    mytagPrefix := _value;
end;

procedure TTagControl.setText(const _value: string);
begin
    tagLabel.Caption := _value;
    if assigned(OnTagTextChange) then
        OnTagTextChange(Self);
end;

procedure TTagControl.settextColor(const _value: TColor);
begin
    tagLabel.Font.Color := _value;
end;

procedure TTagControl.DoOnCloseClick(Sender: TObject);
var
    _canClose: boolean = True;
begin

    if assigned(myOnCloseQuery) then
        myOnCloseQuery(Self, _canClose);

    if assigned(myOnClose) then
    begin
        if _canClose then
        begin
            myOnClose(Self);
        end;
    end;
end;

procedure TTagControl.init;
begin

    with tagLabel do
    begin
        Align := alClient;
        //layout := tlCenter;
        BorderSpacing.Around := BORDER_SPACING_AROUND;
        AutoSize := True;
        // WordWrap := True;
    end;

    setHover(closeLabel);

    with closeLabel do
    begin
        Align := alRight;
        layout := tlTop;
        BorderSpacing.Around := BORDER_SPACING_AROUND;
        Caption := '🗙';
        OnClick := @DoOnCloseClick;
    end;

    with edtTag do
    begin
        Align := alClient;
        BorderSpacing.Around := BORDER_SPACING_AROUND;
        BorderStyle := bsNone;
    end;

    textColor       := clDefault;
    borderColor     := clDefault;
    iconColor       := clDefault;
    backgroundColor := clDefault;
    borderColorSelected := clBlue;
    borderColorHovered  := clHighlight;
    borderColorDefault  := clDefault;

    myHoverCount    := 0;
end;

procedure TTagControl.incHoverCount;
begin
    inc(myHoverCount);
    if myHoverCount = 1 then
        if assigned(onEnter) then onEnter(Self);

    if myHoverCount > 0 then
        borderColor:= borderColorHovered;
end;

procedure TTagControl.decHoverCount;
begin
    myHoverCount :=  max(0, pred(myHoverCount));
    if myHoverCount = 0 then begin
        case selected of
        	True    : borderColor := borderColorSelected;
            False   : borderColor := borderColorDefault;
        end;
        if assigned(onExit) then onExit(Self);
	end;
end;

constructor TTagControl.Create(TheOwner: TComponent);
begin
    inherited Create(TheOwner);
    //AutoSize := True;

    Name := '';
    AutoSize := True;
    tagPrefix := TAG_PREFIX;

    Panel.BorderStyle := bsNone;
    Panel.BevelColor := clSkyBlue;
    Panel.BorderWidth := 1;

    BorderSpacing.Right := 3;
    BorderSpacing.Bottom := 3;

    init;
end;

destructor TTagControl.Destroy;
begin
    inherited Destroy;
end;


end.
