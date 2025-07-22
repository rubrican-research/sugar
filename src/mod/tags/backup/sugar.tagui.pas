unit sugar.tagUI;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, StdCtrls, Buttons,
    frame.TagControl;

type
    { TTagsEditor }

    TTagsEditor = class(TFrame)
		btnAddTags: TSpeedButton;
		btnClear: TSpeedButton;
		btnCopy: TSpeedButton;
		btnEdit: TSpeedButton;
		edtTagText: TComboBox;
		idleTimer: TIdleTimer;
        Label1: TLabel;
		pnlTools: TPanel;
        pblCaption: TPanel;
        pnlAdd: TPanel;
        pnlTags: TFlowPanel;
        procedure btnAddTagsClick(Sender: TObject);
		procedure btnClearClick(Sender: TObject);
        procedure btnCopyClick(Sender: TObject);
        procedure btnEditClick(Sender: TObject);
        procedure edtTagTextEditingDone(Sender: TObject);
		procedure idleTimerTimer(Sender: TObject);
        procedure ListBox1Exit(Sender: TObject);
		procedure OnToolActive(Sender: TObject);
    private
		myalwaysAddNewTags: boolean;
		myautoHideTools: boolean;
        mybackgroundColor: TColor;
        myborderColor: TColor;
        myenableAddTags: boolean;
		myHoveredTagControl: TTagControl;
        myiconColor: TColor;
		mylookupStringList: TStringList;
        myreadOnly: boolean;
		mySelectFrom: TStringArray;
		mytagPrefix: string;
        mytextColor: TColor;
        tagControlClass: TTagControlClass;
        myTags: TTagControlList;
        mySelectedTagControl: TTagControl;

        function getCaption: TCaption;
        function getCaptionFont: TFont;
        function Gettag(_text: string): TTagControl;
        function newTag: TTagControl;
        procedure onCloseTag(Sender: TObject);
		procedure SetalwaysAddNewTags(const _value: boolean);
		procedure SetautoHideTools(const _value: boolean);
        procedure SetbackgroundColor(const _value: TColor);
        procedure SetborderColor(const _value: TColor);
        procedure setCaption(const _value: TCaption);
        procedure setCaptionFont(const _value: TFont);
        procedure SetenableAddTags(const _value: boolean);
        procedure SeticonColor(const _value: TColor);

        procedure SetreadOnly(const _value: boolean);
		procedure SettagPrefix(const _value: string);
        procedure SettextColor(const _value: TColor);

        procedure showTagList;

        procedure scheduleDelete(_tagControl: TTagControl);
        procedure asyncDelete(_tagControlPtr: PtrInt);

        // Handler for tag text changes. This needs to rename the index;
        procedure OnTagTextChange(Sender: TObject);
        procedure OnTagSelect(Sender: TObject);
        procedure OnTagUnSelect(Sender: TObject);
        procedure OnTagEnter(Sender: TObject);
        procedure OnTagExit(Sender: TObject);


    public
        procedure addTags(_tags: TStringArray);
        procedure addTags(_tags: string); //Each tag is space limited.
        procedure deleteTag(_tag: string);
        procedure deleteTag(_tagControl: TTagControl);
        function tags: TStringArray;
        procedure clearTagSelection;
    public
        constructor Create(TheOwner: TComponent); override;
        destructor Destroy; override;
    public
        property Caption: TCaption read getCaption write setCaption;
        property CaptionFont: TFont read getCaptionFont write setCaptionFont;
        property tag[_text: string]: TTagControl read Gettag;
        property selectedTag: TTagControl read mySelectedTagControl;
        property hoveredTag: TTagControl read myHoveredTagControl;

        property textColor: TColor read mytextColor write SettextColor;
        property borderColor: TColor read myborderColor write SetborderColor;
        property iconColor: TColor read myiconColor write SeticonColor;
        property backgroundColor: TColor read mybackgroundColor write SetbackgroundColor;

        property ReadOnly: boolean read myreadOnly write SetreadOnly;
        property enableAddTags: boolean read myenableAddTags write SetenableAddTags;

        property tagPrefix: string read mytagPrefix write SettagPrefix;
        property autoHideTools: boolean read myautoHideTools write SetautoHideTools;

        // Determines the behaviour when a list of tags is given to select from.
        // TRUE: then the user can enter a value that is not in
        // the selection list and the tag will be added.
        // FALSE: then the user cannot enter any values. The user must
        // select from the list that was given.
        property alwaysAddNewTags: boolean read myalwaysAddNewTags write SetalwaysAddNewTags;

        function selectFrom(_values: TStringArray): TTagsEditor;
    end;



implementation

{$R *.lfm}

uses
    LCLType, sugar.utils, Clipbrd, Dialogs;


    { TTagsEditor }

procedure TTagsEditor.ListBox1Exit(Sender: TObject);
begin
    TControl(Sender).Visible := False;
end;

procedure TTagsEditor.OnToolActive(Sender: TObject);
begin
    if autoHideTools then begin
        pnlTools.Visible      := True;
        idleTimer.AutoEnabled := true;
	end;
end;

procedure TTagsEditor.edtTagTextEditingDone(Sender: TObject);
begin
    if edtTagText.Text <> '' then
    begin
        addTags(edtTagText.Text);
        edtTagText.Text := '';
    end;
end;

procedure TTagsEditor.idleTimerTimer(Sender: TObject);
begin
    idleTimer.AutoEnabled := False;
    idleTimer.Enabled     := false;
    if autoHideTools then
        pnlTools.Visible := false;
end;

procedure TTagsEditor.btnEditClick(Sender: TObject);
begin
    ReadOnly := not ReadOnly;
end;

procedure TTagsEditor.btnAddTagsClick(Sender: TObject);
begin
    enableAddTags := not enableAddTags;
end;



procedure TTagsEditor.btnClearClick(Sender: TObject);
var
	i: Integer;
begin
    if MessageDlg('Cofirm', 'Do you want to delete all tags? You cannot undo this.', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        for i := 0 to pred(myTags.Count) do begin
            scheduleDelete(myTags.Data[i]);
	    end;
end;

procedure TTagsEditor.btnCopyClick(Sender: TObject);
begin
    Clipboard.AsText := getDelimitedString(tags);
end;

function TTagsEditor.newTag: TTagControl;
begin
    if assigned(tagControlClass) then
        Result := tagControlClass.Create(Self)
    else
        Result := TTagControl.Create(Self);

    Result.textColor          := textColor;
    Result.borderColor        := borderColor;
    Result.borderColorDefault := borderColor;
    Result.iconColor          := iconColor;
    Result.backgroundColor    := backgroundColor;
    Result.readOnly           := ReadOnly;

    Result.onClose          := @onCloseTag;
    Result.OnTagTextChange  := @OnTagTextChange;
    Result.onSelect         := @OnTagSelect;
    Result.onUnselect       := @OnTagUnselect;
    Result.OnEnter          := @OnTagEnter;
    Result.OnExit           := @OnTagExit;

end;

function TTagsEditor.Gettag(_text: string): TTagControl;
begin
    Result := myTags.KeyData[_text];
end;

function TTagsEditor.getCaption: TCaption;
begin
    Result := Label1.Caption;
end;

function TTagsEditor.getCaptionFont: TFont;
begin
    Result := Label1.Font;
end;

procedure TTagsEditor.onCloseTag(Sender: TObject);
begin
    if not ReadOnly then begin
        TTagControl(Sender).Visible := False;
        scheduleDelete(TTagControl(Sender));
	end;
end;

procedure TTagsEditor.SetalwaysAddNewTags(const _value: boolean);
begin
	myalwaysAddNewTags:=_value;
    case myalwaysAddNewTags of
    	True    : begin
            edtTagText.Style := csDropDown;
		end;

        False   : begin
            edtTagText.Style := csDropDownList;
		end;
    end;;
end;

procedure TTagsEditor.SetautoHideTools(const _value: boolean);
begin
    myautoHideTools:=_value;
    case myautoHideTools of
    	True    : begin
            pnlTools.Visible := false;
		end;

        False   : begin
            pnlTools.Visible := true;
		end;
    end;;

end;

procedure TTagsEditor.SetbackgroundColor(const _value: TColor);
begin
    if mybackgroundColor = _value then Exit;
    mybackgroundColor := _value;
end;

procedure TTagsEditor.SetborderColor(const _value: TColor);
begin
    if myborderColor = _value then Exit;
    myborderColor := _value;
end;

procedure TTagsEditor.setCaption(const _value: TCaption);
begin
    Label1.Caption := _value;
end;

procedure TTagsEditor.setCaptionFont(const _value: TFont);
begin
    Label1.Font.Assign(_value);
end;

procedure TTagsEditor.SetenableAddTags(const _value: boolean);
begin
    myenableAddTags := _value;
    case myEnableAddTags of
        True: begin
            pnlAdd.Visible := True;
            btnAddTags.Caption := '-';
        end;

        False: begin
            pnlAdd.Visible := False;
            btnAddTags.Caption := '+';
        end;
    end;
end;

procedure TTagsEditor.SeticonColor(const _value: TColor);
begin
    if myiconColor = _value then Exit;
    myiconColor := _value;
end;


procedure TTagsEditor.SetreadOnly(const _value: boolean);
var
	i: Integer;
begin
    myreadOnly := _value;
    case myReadOnly of
        True: begin
            btnEdit.Caption := '✎' ; // Show that you can edit
        end;

        False: begin
            btnEdit.Caption := '✐ '; // Show that you can savet
        end;
    end;

    for i := 0 to pred(myTags.Count) do begin
        myTags.Data[i].readonly := myreadOnly;
	end;
end;

procedure TTagsEditor.SettagPrefix(const _value: string);
begin
	if mytagPrefix=_value then Exit;
	mytagPrefix:=_value;
end;


procedure TTagsEditor.SettextColor(const _value: TColor);
begin
    if mytextColor = _value then Exit;
    mytextColor := _value;
end;

procedure TTagsEditor.showTagList;
var
    i: integer;
begin
    edtTagText.Text := '';
    for i := 0 to pred(myTags.Count) do
    begin
        if edtTagText.Text <> '' then
            edtTagText.Text := edtTagText.Text + ' ';
        edtTagText.Text := edtTagText.Text + myTags.Data[i].Text;
    end;
end;

procedure TTagsEditor.scheduleDelete(_tagControl: TTagControl);
begin
    Application.QueueAsyncCall(@asyncDelete, PtrInt(_tagControl));
end;

procedure TTagsEditor.asyncDelete(_tagControlPtr: PtrInt);
var
    _tagControl: TTagControl;
begin
    _tagControl := TTagControl(_tagControlPtr);
    myTags.Delete(myTags.IndexOfData(_tagControl));
    //showTagList;
end;

procedure TTagsEditor.OnTagTextChange(Sender: TObject);
var
    _tagControl: TTagControl;
    i: integer;
begin
    if Sender is TTagControl then
        _tagControl := TTagControl(Sender)
    else
        exit;

    i := myTags.IndexOfData(_tagControl);

    if i > -1 then
    begin
        myTags.Keys[i] := _tagControl.Text;
    end;

end;

procedure TTagsEditor.OnTagSelect(Sender: TObject);
begin
    exit;
    if assigned(mySelectedTagControl) then begin
        if mySelectedTagControl.canUnselect then begin
            mySelectedTagControl.selected := false;

            mySelectedTagControl := TTagControl(Sender);
            mySelectedTagControl.Selected := true;
		end;
	end
    else begin
        mySelectedTagControl := TTagControl(Sender);
        mySelectedTagControl.Selected := true;
    end;
end;

procedure TTagsEditor.OnTagUnSelect(Sender: TObject);
begin

end;

procedure TTagsEditor.OnTagEnter(Sender: TObject);
begin
    if assigned(myHoveredTagControl) then begin
        if myHoveredTagControl.canUnselect then begin
            //myHoveredTagControl.Hovered := false;

            myHoveredTagControl := TTagControl(Sender);
            //myHoveredTagControl.Hovered := true;
		end;
	end
    else begin
        myHoveredTagControl := TTagControl(Sender);
        //myHoveredTagControl.Hovered := true;
    end;

end;

procedure TTagsEditor.OnTagExit(Sender: TObject);
begin

end;

procedure TTagsEditor.addTags(_tags: TStringArray);
var
    _word: string;
    _tagText : string;
    _tagControl: TTagControl;
    _i: integer;
begin
    pnlTags.DisableAlign;
    pnlTags.DisableAutoSizing;
    try
	    for _word in _tags do
	    begin
	        _tagText := ensureTagPrefix(tagPrefix, _word);
	        _i := myTags.indexOf(_tagText);
	        if _i = -1 then
	        begin
	            _tagControl := newTag;
	            _tagControl.Text := _tagText;
	            myTags[_tagText] := _tagControl;
	            pnlTags.InsertControl(_tagControl);
	        end;
	    end;

	finally
    	pnlTags.EnableAlign;
        pnlTags.EnableAutoSizing;
	end;
end;

procedure TTagsEditor.addTags(_tags: string);
begin
    addTags(wordArray(_tags));
end;

procedure TTagsEditor.deleteTag(_tag: string);
begin
    scheduleDelete(tag[_tag]);
end;

procedure TTagsEditor.deleteTag(_tagControl: TTagControl);
begin
    scheduleDelete(_tagControl);
end;

function TTagsEditor.tags: TStringArray;
var
    i: integer;
begin
    Result := [];
    SetLength(Result, myTags.Count);
    for i := 0 to pred(myTags.Count) do
    begin
        Result[i] := myTags.Keys[i];
    end;
end;

procedure TTagsEditor.clearTagSelection;
begin

end;

constructor TTagsEditor.Create(TheOwner: TComponent);
begin
    inherited Create(TheOwner);

    tagControlClass := TTagControl;
    myTags := TTagControlList.Create(True);
    mySelectedTagControl := nil;

    mylookupStringList := TStringList.Create;
    mylookupStringList.Sorted := true;

    textColor   := clDefault;
    borderColor := clSkyBlue;
    iconColor   := clSilver;
    backgroundColor := clDefault;

    tagPrefix     := TAG_PREFIX;
    enableAddTags := False;
    ReadOnly      := False;
    AutoHideTools := False;
    alwaysAddNewTags := True;

end;

destructor TTagsEditor.Destroy;
begin
    myTags.Free;
    mylookupStringList.Free;
    inherited Destroy;
end;

function TTagsEditor.selectFrom(_values: TStringArray): TTagsEditor;
var
	s: String;
begin
    Result := self;
    mySelectFrom := _values;
    myLookupStringList.Clear;
    edtTagText.Items.Clear;
    for s in mySelectFrom do begin
        mylookupStringList.Add(s);
        edtTagText.Items.Add(s);
	end;
    alwaysAddNewTags := edtTagText.Items.Count = 0;
end;

end.
