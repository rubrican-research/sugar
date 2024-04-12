unit bulma;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, sugar.htmlbuilder, webui;

type

    { TBulmaCSS }

    TBulmaCSS = class(TInterfacedObject, IWebUI)
    protected
        myUIBase: TWebUIBase;

    public
        {instance}
        function uiBase: TWebUIBase;
        function CDNLinks: THtmlElementArray;
        {style classes}
        function uiMenuItemLink: string;
        function uiMenuItemDisabled: string;
        function uiMenuItemActive: string;
        function uiMenuItem: string;
        function uiMenuContainer: string;
        function uiMenu: string;
        function uiFlex: string;
        function uiContainer: string;

        {span blocks}
        function uiContent: string;
        function uiExtraContent: string;
        function uiHeaderText: string;
        function uiSubHeaderText: string;
        function uiMetaText: string;
        function uiDescriptionText: string;

        {Colours}
        function uiPrimaryColour: string;
        function uiSecondaryColour: string;
        function uiForeColour1: string;
        function uiForeColour2: string;
        function uiForeColour3: string;
        function uiForeColour4: string;
        function uiForeColour5: string;
        function uiForeColour6: string;
        function uiBackColour1: string;
        function uiBackColour2: string;
        function uiBackColour3: string;
        function uiBackColour4: string;
        function uiBackColour5: string;
        function uiBackColour6: string;

        {Sizes}
        function uiVeryLarge: string;
        function uiLarge: string;
        function uiMedium: string;
        function uiSmall:string;
        function uiVerySmall: string;

        {States}
        function uiEnabled(_yes: boolean = true): string;
        function uiVisible(_yes: boolean = true): string;
        function uiReadOnly(_yes: boolean = true): string;

        {Controls - edit}
        function selection(const _name: string; _choices: TStringArray): THtmlSelect;
        function timeInput(_id: string; _name: string; _placeholder: string = '';
            _caption: string = ''; _value: string = ''): THtmlInput;
        function textInput(_id: string; _name: string; _placeholder: string = '';
            _caption: string = ''; _value: string = ''): THtmlEditBox;
        function phoneInput(_id: string; _name: string; _placeholder: string = '';
            _caption: string = ''; _value: string = ''): THtmlInput;
        function passwordInput(_id: string; _name: string;
            _placeholder: string = ''; _caption: string = '';
            _value: string = ''): THtmlInput;
        function monthInput(_id: string; _name: string; _placeholder: string = '';
            _caption: string = ''; _value: string = ''): THtmlInput;
        function numberInput(_id: string; _name: string; _placeholder: string = '';
            _caption: string = ''; _value: string = ''): THtmlInput;
        function textbox(const _name: string; _placeholder: string = ''): THtmlTextArea;
        function htmlLabel(_text: string): THtmlLabel;
        function fileInput(_id: string; _name: string; _placeholder: string = '';
            _caption: string = ''; _value: string = ''): THtmlInput;
        function emailInput(_id: string; _name: string; _placeholder: string = '';
            _caption: string = ''; _value: string = ''): THtmlInput;
        function datetimeInput(_id: string; _name: string;
            _placeholder: string = ''; _caption: string = '';
            _value: string = ''): THtmlInput;
        function dateInput(_id: string; _name: string; _placeholder: string = '';
            _caption: string = ''; _value: string = ''): THtmlInput;
        function weekInput(_id: string; _name: string; _placeholder: string = '';
            _caption: string = ''; _value: string = ''): THtmlInput;


        {Controls - Buttons}
        function button(const _name: string; _caption: string): THtmlButton;
        function primaryButton(const _name: string; _caption: string): THtmlButton;
        function submitButton(const _name: string; _caption: string): THtmlButton;
        function secondaryButton(const _name: string; _caption: string): THtmlButton;
        function resetButton(const _name: string; _caption: string): THtmlButton;

        {Controls - Choices}
        function radioButton(const _name: string; const _caption: string;
            const _value: string = ''): THtmlRadioButton;

        function checkBox(const _name: string; const _caption: string): THtmlCheckBox;

        {Containers}
        function container: THtmlDiv;
        function segmentGroup: THtmlDiv;
        function segment: THtmlDiv;
        function card: THtmlDiv;
        function item: THtmlDiv;

        {Multimedia}
        function imageContainer: THtmlDiv;
        function image: THtmlImg;
        {Form}
        function newForm(const _id: string; const _name: string = '';
            const _action: string = ''): THtmlForm;
        function menuToggleDropDown: THtmlCollection;
        function menuItem(_caption: string; _url: string; _name: string;
            _id: string): THtmlAnchor;
        function menuDropDown: THtmlCollection;
        function menu: THtmlCollection;

        constructor Create;
        destructor Destroy; override;
        function uiFieldContainer: string;
        function uiField: string;
        function newInput(_id: string; _name: string; _placeholder: string;
            _caption: string; _value: string): THtmlInput;
        function icon(_name: string; _attr: string = ''): string;
        function emoji(_name: string; _attr: string = ''): string;
        function AssetLinks: THtmlElementArray;
        function uiDividerHidden: string;
        function uiDivider: string;
        function uiThreeFields: string;
        function uiTwoFields: string;
        function uiSixFields: string;
        function uiFourFields: string;
        function uiFiveFields: string;
        function uiRow: string;
        function uiGrid(const _numColumns: byte = 1): string;
        function uiFields(const _numFields: byte): string;
        function uiColumns(const _numColumns: byte = 1): string;
        function uiDropdown: string;
        function urlLink(const _url: string; _caption: string): THtmlAnchor;
		function itemGroup: THtmlDiv;
		function cardGroup: THtmlDiv;
		function span(_text: string; _class: string=''): THtmlSpan;
		function text(_text: string; _class: string=''): string;
		function newField(_width: byte=0; const _id: string=''): THtmlDiv;
		function uiVerticalAlign(_valign: UIVerticalAlignment): string;
		function uiSegment(_segmentType: UISegmentType): string;
		function uiRequired(_yes: boolean=true): string;
		function uiHorizontalAlign(_halign: UIHorizontalAlignment): string;
		function uiFloated(_float: UIFloatType=uiFloatedNone): string;
		function textButton(const _name: string; _caption: string): THtmlButton;
		function segment(_segmentType: UISegmentType=uiSegmentBasicBox
			): THtmlDiv;
		function outlineButton(const _name: string; _caption: string
			): THtmlButton;
		function gridRow(_colWidths: array of word): THtmlDiv;
		function gridColumn(_width: word=0; _float: UIFloatType=uiFloatedNone
			): THtmlDiv;
		function grid: THtmlDiv; overload;
		function grid(_cols: word; _gridType: string;
			_valign: UIVerticalAlignment): THtmlDiv; overload;
		function newFieldContainer(const _numElements: byte=1; const _id: string
			=''): THtmlDiv;
		function inlineFields: THtmlDiv;
		function table: THtmlTable;
		function uiContainerFluid: string;
		function containerFluid: THtmlDiv;
    end;


implementation

uses
    assets;

function TBulmaCSS.container: THtmlDiv;
begin
    Result := uiBase.container;
    Result.setClass(uiContainer);
end;

function TBulmaCSS.segmentGroup: THtmlDiv;
begin

end;

function TBulmaCSS.segment: THtmlDiv;
begin

end;

function TBulmaCSS.card: THtmlDiv;
begin

end;

function TBulmaCSS.item: THtmlDiv;
begin

end;

function TBulmaCSS.imageContainer: THtmlDiv;
begin

end;

function TBulmaCSS.image: THtmlImg;
begin

end;

function TBulmaCSS.menu: THtmlCollection;
begin

end;

constructor TBulmaCSS.Create;
begin
    inherited;
    myUIBase := TWebUIBase.Create;
    ;
end;

destructor TBulmaCSS.Destroy;
begin
    FreeAndNil(myUIBase);
    inherited Destroy;
end;

function TBulmaCSS.menuDropDown: THtmlCollection;
begin

end;

function TBulmaCSS.menuToggleDropDown: THtmlCollection;
begin

end;

function TBulmaCSS.menuItem(_caption: string; _url: string; _name: string;
    _id: string): THtmlAnchor;
begin
    Result := THtmlAnchor.Create;
    with Result as THtmlAnchor do
    begin
        href := _url;
        Text := _caption;
        tagName := _name;
        tagID := _id;
        target := target_self;
        setClass('navbar-item');
    end;
end;

function TBulmaCSS.htmlLabel(_text: string): THtmlLabel;
begin
    Result := uiBase.htmlLabel(_text);
    Result.setClass('label');
end;

function TBulmaCSS.textInput(_id: string; _name: string; _placeholder: string;
    _caption: string; _value: string): THtmlEditBox;
begin
    Result := uiBase.textInput(_id, _name, _placeholder, _caption, _value);
    Result.setClass('input');
end;

function TBulmaCSS.newInput(_id: string; _name: string; _placeholder: string;
    _caption: string; _value: string): THtmlInput;
begin
    Result := uiBase.newInput(_id, _name, _placeholder, _caption, _value);
    Result.setClass('input');
end;

function TBulmaCSS.emoji(_name: string; _attr: string): string;
begin
    Result := Format('<i class="%s" %s> </i>', [_name, _attr]);
end;

function TBulmaCSS.AssetLinks: THtmlElementArray;
begin
    Result := nil;
end;

function TBulmaCSS.uiDivider: string;
begin
    Result := '';
end;

function TBulmaCSS.uiThreeFields: string;
begin
    Result := '';
end;

function TBulmaCSS.uiFiveFields: string;
begin
    Result := '';
end;

function TBulmaCSS.newFieldContainer(const _numElements: byte;
    const _id: string): THtmlDiv;
begin
    Result := THtmlDiv.Create;
    Result.setID(_id);
end;

function TBulmaCSS.inlineFields: THtmlDiv;
begin

end;

function TBulmaCSS.table: THtmlTable;
begin
    Result:= uiBase.table;
end;

function TBulmaCSS.containerFluid: THtmlDiv;
begin
    Result:= uiBase.containerFluid;
end;

function TBulmaCSS.uiContainerFluid: string;
begin
    Result:= uiBase.uiContainerFluid;
end;

function TBulmaCSS.uiDropdown: string;
begin
    Result := '';
end;

function TBulmaCSS.urlLink(const _url: string; _caption: string): THtmlAnchor;
begin
    Result := uiBase.urlLink(_url, _caption);
end;

function TBulmaCSS.cardGroup: THtmlDiv;
begin
    Result:= THtmlDiv.Create;
end;

function TBulmaCSS.span(_text: string; _class: string): THtmlSpan;
begin
    Result:= uiBase.span(_text,_class);
end;

function TBulmaCSS.text(_text: string; _class: string): string;
begin
    Result:= uiBase.text(_text,_class);
end;

function TBulmaCSS.newField(_width: byte; const _id: string): THtmlDiv;
begin
    Result:= uiBase.newField(_width, _id);
    Result.setClass('control');
end;

function TBulmaCSS.grid(_cols: word; _gridType: string;
	_valign: UIVerticalAlignment): THtmlDiv;
begin

end;

function TBulmaCSS.grid: THtmlDiv;
begin

end;

function TBulmaCSS.gridColumn(_width: word; _float: UIFloatType): THtmlDiv;
begin

end;

function TBulmaCSS.gridRow(_colWidths: array of word): THtmlDiv;
begin

end;

function TBulmaCSS.outlineButton(const _name: string; _caption: string
	): THtmlButton;
begin

end;

function TBulmaCSS.segment(_segmentType: UISegmentType): THtmlDiv;
begin

end;

function TBulmaCSS.textButton(const _name: string; _caption: string
	): THtmlButton;
begin

end;

function TBulmaCSS.uiFloated(_float: UIFloatType): string;
begin

end;

function TBulmaCSS.uiHorizontalAlign(_halign: UIHorizontalAlignment): string;
begin

end;

function TBulmaCSS.uiRequired(_yes: boolean): string;
begin

end;

function TBulmaCSS.uiSegment(_segmentType: UISegmentType): string;
begin

end;

function TBulmaCSS.uiVerticalAlign(_valign: UIVerticalAlignment): string;
begin

end;

function TBulmaCSS.itemGroup: THtmlDiv;
begin
    Result:= THtmlDiv.Create;
end;

function TBulmaCSS.uiColumns(const _numColumns: byte): string;
begin
    Result := '';
end;

function TBulmaCSS.uiFields(const _numFields: byte): string;
begin
    Result := '';
end;

function TBulmaCSS.uiGrid(const _numColumns: byte): string;
begin
    Result := '';
end;

function TBulmaCSS.uiRow: string;
begin
    Result := '';
end;

function TBulmaCSS.uiFourFields: string;
begin
    Result := '';
end;

function TBulmaCSS.uiSixFields: string;
begin
    Result := '';
end;

function TBulmaCSS.uiTwoFields: string;
begin
    Result := '';
end;

function TBulmaCSS.uiDividerHidden: string;
begin
    Result := '';
end;

function TBulmaCSS.icon(_name: string; _attr: string): string;
begin
    Result := '';
end;

function TBulmaCSS.uiField: string;
begin
    Result := 'field';
end;

function TBulmaCSS.uiFieldContainer: string;
begin
    Result := 'field is-grouped';
end;

function TBulmaCSS.numberInput(_id: string; _name: string; _placeholder: string;
    _caption: string; _value: string): THtmlInput;
begin
    Result := uiBase.numberInput(_id, _name, _placeholder, _caption, _value);
    Result.setClass('input');
end;

function TBulmaCSS.dateInput(_id: string; _name: string; _placeholder: string;
    _caption: string; _value: string): THtmlInput;
begin
    Result := uiBase.dateInput(_id, _name, _placeholder, _caption, _value);
    Result.setClass('input');
end;

function TBulmaCSS.passwordInput(_id: string; _name: string;
    _placeholder: string; _caption: string; _value: string): THtmlInput;
begin
    Result := uiBase.passwordInput(_id, _name, _placeholder, _caption, _value);
    Result.setClass('input');
end;

function TBulmaCSS.emailInput(_id: string; _name: string; _placeholder: string;
    _caption: string; _value: string): THtmlInput;
begin
    Result := uiBase.emailInput(_id, _name, _placeholder, _caption, _value);
    Result.setClass('input');
end;

function TBulmaCSS.timeInput(_id: string; _name: string; _placeholder: string;
    _caption: string; _value: string): THtmlInput;
begin
    Result := uiBase.timeInput(_id, _name, _placeholder, _caption, _value);
    Result.setClass('input');
end;

function TBulmaCSS.datetimeInput(_id: string; _name: string;
    _placeholder: string; _caption: string; _value: string): THtmlInput;
begin
    Result := uiBase.datetimeInput(_id, _name, _placeholder, _caption, _value);
    Result.setClass('input');
end;

function TBulmaCSS.phoneInput(_id: string; _name: string; _placeholder: string;
    _caption: string; _value: string): THtmlInput;
begin
    Result := uiBase.phoneInput(_id, _name, _placeholder, _caption, _value);
    Result.setClass('input');
end;

function TBulmaCSS.fileInput(_id: string; _name: string; _placeholder: string;
    _caption: string; _value: string): THtmlInput;
begin
    Result := uiBase.fileInput(_id, _name, _placeholder, _caption, _value);
    Result.setClass('input');
end;

function TBulmaCSS.monthInput(_id: string; _name: string; _placeholder: string;
    _caption: string; _value: string): THtmlInput;
begin
    Result := uiBase.monthInput(_id, _name, _placeholder, _caption, _value);
    Result.setClass('input');
end;

function TBulmaCSS.weekInput(_id: string; _name: string; _placeholder: string;
    _caption: string; _value: string): THtmlInput;
begin
    Result := uiBase.weekInput(_id, _name, _placeholder, _caption, _value);
    Result.setClass('input');
end;

function TBulmaCSS.radioButton(const _name: string; const _caption: string;
    const _value: string): THtmlRadioButton;
begin
    Result := uiBase.radioButton(_name, _caption, _value);
    Result.labelObj.setClass('radio');
end;

function TBulmaCSS.checkBox(const _name: string;
    const _caption: string): THtmlCheckBox;
begin
    Result := uiBase.checkBox(_name, _caption);
    Result.labelObj.setClass('checkbox');
end;

function TBulmaCSS.selection(const _name: string; _choices: TStringArray): THtmlSelect;
begin
    Result := uiBase.selection(_name, _choices);
end;

function TBulmaCSS.textbox(const _name: string; _placeholder: string): THtmlTextArea;
begin
    Result := uiBase.textbox(_name, _placeholder);
    Result.setClass('textarea');
end;

function TBulmaCSS.button(const _name: string; _caption: string): THtmlButton;
begin
    Result := uiBase.button(_name, _caption);
    Result.setClass('button');
end;

function TBulmaCSS.submitButton(const _name: string; _caption: string): THtmlButton;
begin
    Result := uiBase.submitButton(_name, _caption);
    Result.setClass('button is-primary');
end;

function TBulmaCSS.primaryButton(const _name: string; _caption: string): THtmlButton;
begin
    Result := uiBase.primaryButton(_name, _caption);
    Result.setClass('button is-primary');
end;

function TBulmaCSS.secondaryButton(const _name: string; _caption: string): THtmlButton;
begin
    Result := uiBase.secondaryButton(_name, _caption);
    Result.setClass('button is-info');
end;

function TBulmaCSS.resetButton(const _name: string; _caption: string): THtmlButton;
begin
    Result := uiBase.resetButton(_name, _caption);
    Result.setClass('button is-warning');
end;

function TBulmaCSS.uiContainer: string;
begin
    Result := 'container';
end;

function TBulmaCSS.uiContent: string;
begin
    Result:= '';
end;

function TBulmaCSS.uiExtraContent: string;
begin
    Result:= '';
end;

function TBulmaCSS.uiHeaderText: string;
begin
    Result:= '';
end;

function TBulmaCSS.uiSubHeaderText: string;
begin
    Result:= '';
end;

function TBulmaCSS.uiMetaText: string;
begin
    Result:= '';
end;

function TBulmaCSS.uiDescriptionText: string;
begin
    Result:= '';
end;

function TBulmaCSS.uiPrimaryColour: string;
begin
    Result:= '';
end;

function TBulmaCSS.uiSecondaryColour: string;
begin
    Result:= '';
end;

function TBulmaCSS.uiForeColour1: string;
begin
    Result:= '';
end;

function TBulmaCSS.uiForeColour2: string;
begin
    Result:= '';
end;

function TBulmaCSS.uiForeColour3: string;
begin
    Result:= '';
end;

function TBulmaCSS.uiForeColour4: string;
begin
    Result:= '';
end;

function TBulmaCSS.uiForeColour5: string;
begin
    Result:= '';
end;

function TBulmaCSS.uiForeColour6: string;
begin

end;

function TBulmaCSS.uiBackColour1: string;
begin
    Result:= '';
end;

function TBulmaCSS.uiBackColour2: string;
begin
    Result:= '';
end;

function TBulmaCSS.uiBackColour3: string;
begin
    Result:= '';
end;

function TBulmaCSS.uiBackColour4: string;
begin
    Result:= '';
end;

function TBulmaCSS.uiBackColour5: string;
begin
    Result:= '';
end;

function TBulmaCSS.uiBackColour6: string;
begin
    Result:= '';
end;

function TBulmaCSS.uiVeryLarge: string;
begin
    Result:= '';
end;

function TBulmaCSS.uiLarge: string;
begin
    Result:= '';
end;

function TBulmaCSS.uiMedium: string;
begin
    Result:= '';
end;

function TBulmaCSS.uiSmall: string;
begin
    Result:= '';
end;

function TBulmaCSS.uiVerySmall: string;
begin
    Result:= '';
end;

function TBulmaCSS.uiEnabled(_yes: boolean): string;
begin
    Result:= '';
end;

function TBulmaCSS.uiVisible(_yes: boolean): string;
begin
    Result:= '';
end;

function TBulmaCSS.uiReadOnly(_yes: boolean): string;
begin
    Result:= '';
end;

function TBulmaCSS.uiFlex: string;
begin
    Result := 'is-flex';
end;

function TBulmaCSS.uiMenuContainer: string;
begin
    Result := 'menu';
end;

function TBulmaCSS.uiMenu: string;
begin
    Result := 'menu';
end;

function TBulmaCSS.uiMenuItem: string;
begin
    Result := 'menu';
end;

function TBulmaCSS.uiMenuItemLink: string;
begin
    Result := 'menu link';
end;

function TBulmaCSS.uiMenuItemActive: string;
begin
    Result := 'is-active';
end;

function TBulmaCSS.uiMenuItemDisabled: string;
begin
    Result := 'is-disabled';
end;

function TBulmaCSS.uiBase: TWebUIBase;
begin
    Result := myUIBase;
end;

function TBulmaCSS.CDNLinks: THtmlElementArray;
begin
    SetLength(Result, 3);
    Result[0] := BulmaCSS;
    Result[1] := JQueryJS;
    Result[2] := FontAwesome5JS;
end;

function TBulmaCSS.newForm(const _id: string; const _name: string;
    const _action: string): THtmlForm;
begin
    Result := uiBase.newForm(_id, _name, _action);
    Result.setClass('ui form piled segment');
end;

end.
