unit frame.TagControl;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Graphics, fgl;

const
    BORDER_SPACING_AROUND = 7;

type

	{ TTagControl }

    TTagControl = class(TFrame)
		edtTag: TEdit;
		tagLabel: TLabel;
		closeLabel: TLabel;
		Panel: TPanel;
  procedure edtTagEditingDone(Sender: TObject);
  procedure tagLabelDblClick(Sender: TObject);
    private
        myEditiable : boolean;
        myOnClose: TNotifyEvent;
		myOnCloseQuery: TCloseQueryEvent;
		myOnTagTextChange: TNotifyEvent;

        function getBackgroundColor: TColor;
        function getBorderColor: TColor;
		function getEditState: boolean;
		function getIconColor: TColor;
        function getText: string;
		function getTextColor: TColor;
		procedure setBackgroundColor(const _value: TColor);
		procedure setBorderColor(const _value: TColor);
		procedure setEditState(const _value: boolean);
		procedure setIconColor(const _value: TColor);
		procedure setOnClose(const _value: TNotifyEvent);
		procedure setOnCloseQuery(const _value: TCloseQueryEvent);
		procedure SetOnTagTextChange(const _value: TNotifyEvent);
		procedure setText(const _value: string);
		procedure settextColor(const _value: TColor);
    protected
        procedure DoOnCloseClick(Sender: TObject);
        procedure init;

    public
        constructor Create(TheOwner: TComponent); override;
        destructor Destroy; override;

        property text: string read getText write setText;
        property textColor: TColor read getTextColor write setTextColor;
        property borderColor: TColor read getBorderColor write setBorderColor;
        property iconColor: TColor read getIconColor write setIconColor;
        property backgroundColor: TColor read getBackgroundColor write setBackgroundColor;


        property onClose: TNotifyEvent read myOnClose write setOnClose;
        property OnCloseQuery: TCloseQueryEvent read myOnCloseQuery write setOnCloseQuery;
        property editable : boolean read getEditState write setEditState;

        property OnTagTextChange : TNotifyEvent read myOnTagTextChange write SetOnTagTextChange;



	end;

    TTagControlClass = class of TTagControl;

    TTagControlList = class(specialize TFPGMapObject<string,  TTagControl>)

    end;


implementation

{$R *.lfm}

uses
     LCLType, sugar.utils, sugar.uihelper;

{ TTagControl }

function TTagControl.getText: string;
begin
    Result := tagLabel.Caption;
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

function TTagControl.getEditState: boolean;
begin
    Result := myEditiable;
end;

procedure TTagControl.tagLabelDblClick(Sender: TObject);
begin
    editable := true;
end;

procedure TTagControl.edtTagEditingDone(Sender: TObject);
begin
    if editable then
        editable := false;
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
    Panel.BevelColor := _value;
end;

procedure TTagControl.setEditState(const _value: boolean);
begin

    myEditiable := _value;
    case myEditiable of
    	True:   begin
            edtTag.Text      := text;
            edtTag.Width     := tagLabel.Width;
            tagLabel.Visible := false;
            edtTag.Visible   := true;
            edtTag.SetFocus;
		end;

        False:  begin
            text := edtTag.Text;
            edtTag.Visible   := false;
            tagLabel.Visible := true;
		end;
    end;;
end;

procedure TTagControl.setIconColor(const _value: TColor);
begin
    closeLabel.Font.Color := _value;
end;


procedure TTagControl.setOnClose(const _value: TNotifyEvent);
begin
	if myonClose=_value then Exit;
	myonClose:=_value;
end;

procedure TTagControl.setOnCloseQuery(const _value: TCloseQueryEvent);
begin
	if myOnCloseQuery=_value then Exit;
	myOnCloseQuery:=_value;
end;

procedure TTagControl.SetOnTagTextChange(const _value: TNotifyEvent);
begin
	if myOnTagTextChange=_value then Exit;
	myOnTagTextChange:=_value;
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
    _canClose: boolean = true;
begin
    if assigned(myOnCloseQuery) then
        myOnCloseQuery(Self, _canClose);

    if assigned(myOnClose) then begin
        if _canClose then begin
            myOnClose(Self);
        end;
	end;
end;

procedure TTagControl.init;
begin

    with tagLabel do begin
        Align  := alClient;
        layout := tlCenter;
        BorderSpacing.Around := BORDER_SPACING_AROUND;
        AutoSize := True;
        // WordWrap := True;
	end;

    setHover(closeLabel);
    with closeLabel do begin
        Align  := alRight;
        layout := tlTop;
        BorderSpacing.Around := BORDER_SPACING_AROUND;
        Caption := '🗙';
        OnClick := @DoOnCloseClick;
	end;

    with edtTag do begin
        Align := alClient;
        BorderSpacing.Around := BORDER_SPACING_AROUND;
        BorderStyle := bsNone;
    end;

    textColor   := clDefault;
    borderColor := clDefault;
    iconColor   := clDefault;
    backgroundColor := clDefault;
end;

constructor TTagControl.Create(TheOwner: TComponent);
begin
	inherited Create(TheOwner);
    //AutoSize := True;
    Name        := '';
    AutoSize    := True;

    Panel.BorderStyle := bsNone;
    Panel.BevelColor  := clSkyBlue;
    Panel.BorderWidth := 1;

    BorderSpacing.Right := 3;
    BorderSpacing.Bottom:= 3;

    init;
end;

destructor TTagControl.Destroy;
begin
	inherited Destroy;
end;


end.

