unit sugar.toast;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
    TToastSize = (toastSmall, toastMedium, toastLarge);


	{ TFormToast }

    TFormToast = class(TForm)
		fadeTimer: TTimer;
		motionTimer: TTimer;
		procedure fadeTimerTimer(Sender: TObject);
		procedure FormClick(Sender: TObject);
		procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
		procedure motionTimerTimer(Sender: TObject);
    private
		myfade: boolean;
		myfadeTime: word;
		mytext: string;
		mywidthRatio: currency;
		procedure setfade(const _value: boolean);
		procedure setfadeTime(const _value: word);
		procedure settext(const _value: string);
		procedure setwidthRatio(const _value: currency);

    public
        property widthRatio: currency read mywidthRatio write setwidthRatio;
        property fade: boolean read myfade write setfade;
        property fadeTime: word read myfadeTime write setfadeTime;
        property text: string read mytext write settext;
    public
        procedure toast(const _msg: string; _fadeTime: word = 1000);
        procedure toast(_frameClass: TCustomFrameClass; _fadeTime: word = 1000);
    end;

    procedure toast(const _msg: string; _fadeTime: word = 500; _size: TToastSize = toastSmall);
    procedure toast(_frameClass: TCustomFrameClass; _fadeTime: word = 500; _size: TToastSize = toastSmall);

    function setToastPos(_t: TFormToast; _size: TToastSize): TFormToast;



implementation

{$R *.lfm}

uses
    Math;


procedure toast(const _msg: string; _fadeTime: word; _size: TToastSize);
var
	_t: TFormToast;
	_curr: TForm;
begin
    _curr := Screen.ActiveForm;
    _t := TFormToast.Create(Application);
    setToastPos(_t, toastSmall).toast(_msg, _fadeTime);
    _curr.SetFocus;
end;

procedure toast(_frameClass: TCustomFrameClass; _fadeTime: word;
	_size: TToastSize);
var
	_t: TFormToast;
	_curr: TForm;
begin
    _curr := Screen.ActiveForm;
    _t := TFormToast.Create(Application);
    setToastPos(_t, toastSmall).toast(_frameClass, _fadeTime);
    _curr.SetFocus;
end;


function setToastPos(_t: TFormToast; _size: TToastSize): TFormToast;
var
	_f: TForm;
begin
    _f := Screen.ActiveForm;
    Result := _t;
    _t.left := Screen.Width  - _t.Width ;
    _t.top  := Screen.height - _t.Height - 60;
end;

{ TFormToast }

procedure TFormToast.fadeTimerTimer(Sender: TObject);
begin
    motionTimer.Enabled := true;
    fadeTimer.Enabled:=False;
end;

procedure TFormToast.FormClick(Sender: TObject);
begin
    Close;
end;

procedure TFormToast.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    CloseAction := caFree;
end;

procedure TFormToast.motionTimerTimer(Sender: TObject);
begin
    AlphaBlendValue := Max(0, AlphaBlendValue - 30);
    if AlphaBlendValue = 0 then close;
end;

procedure TFormToast.setfade(const _value: boolean);
begin
	if myfade=_value then Exit;
	myfade:=_value;
end;

procedure TFormToast.setfadeTime(const _value: word);
begin
	if myfadeTime=_value then Exit;
	myfadeTime:=_value;
end;

procedure TFormToast.settext(const _value: string);
begin
	if mytext=_value then Exit;
	mytext:=_value;
end;

procedure TFormToast.setwidthRatio(const _value: currency);
begin
	if mywidthRatio=_value then Exit;
	mywidthRatio:=_value;
end;

procedure TFormToast.toast(const _msg: string; _fadeTime: word);
var
	_pnl: TPanel;
    _lbl: TLabel;
begin
    _pnl := TPanel.Create(Self);
    _pnl.Align  :=alClient;
    _pnl.color  :=clInfoBk;
    _pnl.OnClick := @FormClick;

    _lbl := TLabel.Create(Self);
    with _lbl do begin
        align := alClient;
        BorderSpacing.Around := 14;
        Caption := _msg;
        Layout  := tlCenter;
        WordWrap :=True;
        OnClick := _pnl.OnClick;
	end;

    _pnl.InsertControl(_lbl);
    InsertControl(_pnl);

    fadeTimer.Enabled:= _fadeTime > 0;
    Visible := True;
end;

procedure TFormToast.toast(_frameClass: TCustomFrameClass; _fadeTime: word);
var
    _f: TCustomFrame;
begin
    _f := _frameClass.Create(Self);
    _f.align := alClient;
    _f.OnClick := @FormClick;
    insertControl(_f);
    fadeTimer.Enabled:= _fadeTime > 0;;
    Visible := True;
end;

end.

