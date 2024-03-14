unit sugar.htmlbuilder.vue;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
    Classes, SysUtils, sugar.htmlbuilder;
type

	{ THtmlBuilderVue }

    THtmlBuilderVue = type helper for THtmlElement
    public
        function install(constref _container: THtmlCollection): THtmlCollection;
        {VueJS bindings - refactor out to new Library}
        function vbind(_name: string; _value: string): THtmlElement;
        function vmodel(_field: string): THtmlElement;
        function vmodelNumber(_field: string): THtmlElement;

    public
        type vOnModifier = (vOnNone, vOnPrevent, vOnStop, vOnCapture, vOnSelf, vOnOnce, vOnPassive);
    public
        function vOn(_event: string; _params: string; _modifier: vOnModifier = vOnNone): THtmlElement;
        function vText(_text: string): THtmlElement;
        function vHtml(_html: string): THtmlElement;
        function vfor(_loop: string): THtmlElement;
        function vIf(_ifCondition: string): THtmlElement;
        function vShow(_condition: string): THtmlElement;
        function vCloak(_active: boolean = true): THtmlElement;
        function vRef(_refName: string): THtmlElement;
        function vKey(_keyField: string): THtmlElement;
	end;

implementation

{ THtmlBuilderVue }

function THtmlBuilderVue.install(constref _container: THtmlCollection
	): THtmlCollection;
begin
    Result:= _container;
    Result.script
        .setSrc('https://unpkg.com/vue@3/dist/vue.global.js')
        .setDefer;
end;

function THtmlBuilderVue.vbind(_name: string; _value: string): THtmlElement;
begin
  Result := Self;
  setAttr('v-bind:' + _name, _value);
end;

function THtmlBuilderVue.vmodel(_field: string): THtmlElement;
begin
  Result := Self;
  setAttr('v-model', _field);
end;

function THtmlBuilderVue.vmodelNumber(_field: string): THtmlElement;
begin
  Result := Self;
  setAttr('v-model.number', _field);

end;

function THtmlBuilderVue.vOn(_event: string; _params: string;
	_modifier: vOnModifier): THtmlElement;
var
    _attr: string = 'v-on:%s';
begin
    Result := Self;
    case _modifier of
        vOnNone: ;
        vOnPrevent: _attr += '.prevent';
        vOnStop:    _attr += '.stop';
        vOnCapture: _attr += '.capture';
        vOnSelf:    _attr += '.self';
        vOnOnce:    _attr += '.once';
        vOnPassive: _attr += '.passive';
    end;

    setAttr(Format(_attr, [_event]), _params);
end;

function THtmlBuilderVue.vText(_text: string): THtmlElement;
begin
    Result := Self;
    setAttr('v-text', _text);
end;

function THtmlBuilderVue.vHtml(_html: string): THtmlElement;
begin
    Result := Self;
    setAttr('v-html', _html);
end;

function THtmlBuilderVue.vfor(_loop: string): THtmlElement;
begin
    Result := Self;
    setAttr('v-for', _loop);
end;

function THtmlBuilderVue.vIf(_ifCondition: string): THtmlElement;
begin
    Result := Self;
    setAttr('v-if', _ifCondition);
end;

function THtmlBuilderVue.vShow(_condition: string): THtmlElement;
begin
    Result := Self;
    setAttr('v-show', _condition);
end;

function THtmlBuilderVue.vCloak(_active: boolean): THtmlElement;
begin
    Result := Self;
    case _active of
        True:  setAttrFlag('v-cloak');
        False: rmAttrFlag('v-cloak');
    end;
end;

function THtmlBuilderVue.vRef(_refName: string): THtmlElement;
begin
    Result := Self;
    setAttr('ref', _refName);
end;

function THtmlBuilderVue.vKey(_keyField: string): THtmlElement;
begin
    Result := Self;
    vbind('key', _keyField);
end;

end.

