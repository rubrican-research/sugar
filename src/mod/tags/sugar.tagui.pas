unit sugar.tagUI;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, StdCtrls,  frame.TagControl;

type
	{ TTagsEditor }

    TTagsEditor = class(TFrame)
		Edit1: TEdit;
		ListBox1: TListBox;
		pnlTags: TFlowPanel;
		procedure Edit1KeyDown(Sender: TObject; var Key: Word;
			Shift: TShiftState);
		procedure ListBox1Exit(Sender: TObject);
    private
		mybackgroundColor: TColor;
		myborderColor: TColor;
		myiconColor: TColor;
		mytextColor: TColor;
        tagControlClass : TTagControlClass;
        myTags: TTagControlList;
		function Gettag(_text: string): TTagControl;
        function newTag: TTagControl;
        procedure onCloseTag(Sender: TObject);
		procedure SetbackgroundColor(const _value: TColor);
		procedure SetborderColor(const _value: TColor);
		procedure SeticonColor(const _value: TColor);
		procedure SettextColor(const _value: TColor);
        procedure showSelection;
        procedure showTagList;

        procedure markForDelete(_tagControl: TTagControl);
        procedure asyncDelete(_tagControlPtr: PtrInt);

        // Handler for tag text changes. This needs to rename the index;
        procedure OnTagTextChange(Sender: TObject);
    public
        procedure addTags(_tags: TStringArray);
        procedure addTags(_tags: string);
        function tags: TStringArray;
    public
        constructor Create(TheOwner: TComponent); override;
        destructor Destroy; override;
    public
        property tag[_text: string] : TTagControl read Gettag;
        property textColor      : TColor read mytextColor write SettextColor;
        property borderColor    : TColor read myborderColor write SetborderColor;
        property iconColor      : TColor read myiconColor write SeticonColor;
        property backgroundColor: TColor read mybackgroundColor write SetbackgroundColor;
    end;

implementation

{$R *.lfm}

uses
   LCLType, sugar.utils, sugar.uihelper;

{ TTagsEditor }

procedure TTagsEditor.Edit1KeyDown(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
    if Key = VK_RETURN then
        addTags(Edit1.Text);
    // showSelection;
end;

procedure TTagsEditor.ListBox1Exit(Sender: TObject);
begin
    TControl(Sender).Visible:=False;
end;

function TTagsEditor.newTag: TTagControl;
begin
    if assigned (tagControlClass) then
        Result := tagControlClass.Create(Self)
    else
        Result := TTagControl.Create(Self);
    Result.OnTagTextChange  := @OnTagTextChange;
    Result.textColor        := textColor;
    Result.borderColor      := borderColor;
    Result.iconColor        := iconColor;
    Result.backgroundColor  := backgroundColor;

    Result.onClose:= @onCloseTag;
end;

function TTagsEditor.Gettag(_text: string): TTagControl;
begin
    Result := myTags.KeyData[_text];
end;

procedure TTagsEditor.onCloseTag(Sender: TObject);
begin
    TTagControl(Sender).Visible := false;
    markForDelete(TTagControl(Sender));
end;

procedure TTagsEditor.SetbackgroundColor(const _value: TColor);
begin
	if mybackgroundColor=_value then Exit;
	mybackgroundColor:=_value;
end;

procedure TTagsEditor.SetborderColor(const _value: TColor);
begin
	if myborderColor=_value then Exit;
	myborderColor:=_value;
end;

procedure TTagsEditor.SeticonColor(const _value: TColor);
begin
	if myiconColor=_value then Exit;
	myiconColor:=_value;
end;


procedure TTagsEditor.SettextColor(const _value: TColor);
begin
	if mytextColor=_value then Exit;
	mytextColor:=_value;
end;

procedure TTagsEditor.showSelection;
var
	_word: String;
begin
    positionBelow(Edit1, ListBox1);
    ListBox1.Items.Clear;
    for _word in tags do begin
        ListBox1.Items.Add(_word);
	end;
    ListBox1.Visible := True;
end;

procedure TTagsEditor.showTagList;
var
	i: Integer;
begin
    Edit1.Text := '';
    for i := 0 to pred(myTags.Count) do begin
        if Edit1.Text <> '' then
           Edit1.Text := Edit1.Text + ' ';
        Edit1.Text := Edit1.Text + myTags.Data[i].text;
    end;
end;

procedure TTagsEditor.markForDelete(_tagControl: TTagControl);
begin
    Application.QueueAsyncCall(@asyncDelete, PtrInt(_tagControl));
end;

procedure TTagsEditor.asyncDelete(_tagControlPtr: PtrInt);
var
	_tagControl: TTagControl;
begin
    _tagControl := TTagControl(_tagControlPtr);
    myTags.Delete(myTags.IndexOfData(_tagControl));
    showTagList;
end;

procedure TTagsEditor.OnTagTextChange(Sender: TObject);
var
	_tagControl: TTagControl;
	i: Integer;
begin
    if Sender is TTagControl then
        _tagControl := TTagControl(Sender)
    else
        exit;

    i := myTags.IndexOfData(_tagControl);

    if i > -1 then begin
        myTags.Keys[i] := _tagControl.text;
        showTagList;
	end;

    //else
    //    raise Exception.Create('TTagsEditor.OnTagTextChange:: Unable to find TagControl for ' + _tagControl.Text);


end;

procedure TTagsEditor.addTags(_tags: TStringArray);
var
	_word: String;
	_tagControl: TTagControl;
	_i: Integer;
begin
    for _word in _tags do begin
        _i := myTags.indexOf(_word);
        if _i = -1 then begin
	        _tagControl := newTag;
	        _tagControl.text:= _word;
	        myTags[_word] := _tagControl;
	        pnlTags.InsertControl(_tagControl);
		end
        else
            myTags.Data[_i].Visible := True;
	end;
end;

procedure TTagsEditor.addTags(_tags: string);
begin
    addTags(wordArray(_tags));
end;

function TTagsEditor.tags: TStringArray;
var
	i: Integer;
begin
    Result := [];
    SetLength(Result, myTags.Count);
    for i := 0 to pred(myTags.Count) do begin
        Result[i] := myTags.Keys[i];
	end;
end;



constructor TTagsEditor.Create(TheOwner: TComponent);
begin
	inherited Create(TheOwner);

    textColor        := clDefault;
    borderColor      := clSkyBlue;
    iconColor        := clSilver;
    backgroundColor  := clDefault;

    tagControlClass:= TTagControl;
    myTags := TTagControlList.Create(True);
end;

destructor TTagsEditor.Destroy;
begin
    myTags.Free;
    inherited Destroy;
end;

end.

