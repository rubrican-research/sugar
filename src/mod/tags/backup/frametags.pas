unit frameTags;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, StdCtrls, fgl;

const
    __around = 7;

type



	{ TTagControl }

    TTagControl = class(TPanel)
    private
		mybgColor: TColor;
		mycolor: TColor;
		myOnClose: TNotifyEvent;
		myOnCloseQuery: TCloseQueryEvent;
		mytextColor: TColor;
        tagLabel: TLabel;
        closeLabel: TLabel;
		function getText: string;
		procedure setOnClose(const _value: TNotifyEvent);
		procedure setOnCloseQuery(const _value: TCloseQueryEvent);
		procedure setText(const _value: string);
		procedure settextColor(const _value: TColor);
    protected
        procedure DoOnCloseClick(Sender: TObject);
        procedure init;
    public
        constructor Create(TheOwner: TComponent); override;
        destructor Destroy; override;

        property text: string read getText write setText;
        property textColor: TColor read mytextColor write settextColor;
        property onClose: TNotifyEvent read myOnClose write setOnClose;
        property OnCloseQuery: TCloseQueryEvent read myOnCloseQuery write setOnCloseQuery;

	end;

    TTagControlClass = class of TTagControl;

    TTagControlList = class(specialize TFPGMapObject<string,  TTagControl>)

	end;

	{ TTagsEditor }

    TTagsEditor = class(TFrame)
		Edit1: TEdit;
		pnlTags: TFlowPanel;
		procedure Edit1KeyDown(Sender: TObject; var Key: Word;
			Shift: TShiftState);
    private
        tagControlClass : TTagControlClass;
        tags: TTagControlList;
        function newTag: TTagControl;
        procedure onCloseTag(Sender: TObject);
    public
        procedure addTags(_tags: TStringArray);
        procedure addTags(_tags: string);
    public
        constructor Create(TheOwner: TComponent); override;
        destructor Destroy; override;
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

procedure TTagControl.setText(const _value: string);
begin
    tagLabel.Caption := _value;
end;

procedure TTagControl.settextColor(const _value: TColor);
begin
	if mytextColor=_value then Exit;
	mytextColor:=_value;
    tagLabel.Font.Color:= _value;
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
    tagLabel := TLabel.Create(Self);

    with tagLabel do begin
        Align  := alClient;
        layout := tlCenter;
        BorderSpacing.Around := __around;
        AutoSize := True;
        WordWrap := True;
	end;
    InsertControl(tagLabel);

	closeLabel := TLabel.Create(Self);
    setHover(closeLabel);
    with closeLabel do begin
        Align  := alRight;
        layout := tlTop;
        BorderSpacing.Around := __around;
        Caption := '🗙';
        OnClick := @DoOnCloseClick;
	end;
    insertControl(closeLabel);

end;

constructor TTagControl.Create(TheOwner: TComponent);
begin
	inherited Create(TheOwner);
    //AutoSize := True;
    Name := '';
    Caption := '';
    AutoSize := True;
    BorderStyle:= bsSingle;
    BevelColor:=clSkyBlue;
    BevelOuter:= bvNone;
    BevelInner:= bvNone;
    BorderWidth:=0;
    BorderSpacing.Right := 3;
    BorderSpacing.Bottom:= 3;
    init;
end;

destructor TTagControl.Destroy;
begin
	inherited Destroy;
end;

{ TTagsEditor }

procedure TTagsEditor.Edit1KeyDown(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
    if Key = VK_RETURN then addTags(Edit1.Text);
end;

function TTagsEditor.newTag: TTagControl;
begin
    if assigned (tagControlClass) then
        Result := tagControlClass.Create(Self)
    else
        Result := TTagControl.Create(Self);

    Result.onClose:= @onCloseTag;
end;

procedure TTagsEditor.onCloseTag(Sender: TObject);
begin
    TTagControl(Sender).Visible := false;
end;

procedure TTagsEditor.addTags(_tags: TStringArray);
var
	_word: String;
	_tagControl: TTagControl;
	_i: Integer;
begin
    for _word in _tags do begin
        _i := tags.indexOf(_word);
        if _i = -1 then begin
	        _tagControl := newTag;
	        _tagControl.text:= _word;
            _tagControl.textColor := clSkyBlue;
	        tags[_word] := _tagControl;
	        pnlTags.InsertControl(_tagControl);
		end
        else
            tags.Data[_i].Visible := True;
	end;

end;

procedure TTagsEditor.addTags(_tags: string);
begin
    addTags(wordArray(_tags));
end;



constructor TTagsEditor.Create(TheOwner: TComponent);
begin
	inherited Create(TheOwner);
    tagControlClass:= TTagControl;
    tags := TTagControlList.Create(True);
end;

destructor TTagsEditor.Destroy;
begin
    tags.Free;
    inherited Destroy;
end;

end.

