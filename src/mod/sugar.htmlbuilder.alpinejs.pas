unit sugar.htmlbuilder.alpinejs;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}
interface

uses
    Classes, SysUtils, sugar.htmlbuilder, fpjson;

type

    TAlpineJSOnModifier = (
        xonNone,
        xonPrevent,     // .prevent is the equivalent of calling .preventDefault() inside a listener on the browser event object.
        xonStop,        // Similar to .prevent, .stop is the equivalent of calling .stopPropagation() inside a listener on the browser event object.
        xonOutside,     // .outside is a convenience helper for listening for a click outside of the element it is attached to. Here's a simple dropdown component example to demonstrate:
        xonWindow,      // When the .window modifier is present, Alpine will register the event listener on the root window object on the page instead of the element itself.
        xonDocument,    // .document works similarly to .window only it registers listeners on the document global, instead of the window global.
        xonOnce,        // By adding .once to a listener, you are ensuring that the handler is only called ONCE.
        xonDebounce,    // Sometimes it is useful to "debounce" an event handler so that it only is called after a certain period of inactivity (250 milliseconds by default). If you wish to lengthen or shorten the debounce time, you can do so by trailing a duration after the .debounce modifier like so: <input @input.debounce.500ms="fetchResults">
        xonThrottle,    // .throttle is similar to .debounce except it will release a handler call every 250 milliseconds instead of deferring it indefinitely. <div @scroll.window.throttle.750ms="handleScroll">...</div>
        xonSelf,        // By adding .self to an event listener, you are ensuring that the event originated on the element it is declared on, and not from a child element.
        xonCamel,       // Sometimes you may want to listen for camelCased events such as customEvent in our example. Because camelCasing inside HTML attributes is not supported, adding the .camel modifier is necessary for Alpine to camelCase the event name internally. By adding .camel in the above example, Alpine is now listening for customEvent instead of custom-event.
        xonDot,         // Similar to the .camelCase modifier there may be situations where you want to listen for events that have dots in their name (like custom.event). Since dots within the event name are reserved by Alpine you need to write them with dashes and add the .dot modifier. In the code example above custom-event.dot will correspond to the event name custom.event.
        xonPassive,     // Browsers optimize scrolling on pages to be fast and smooth even when JavaScript is being executed on the page. However, improperly implemented touch and wheel listeners can block this optimization and cause poor site performance. If you are listening for touch events, it's important to add .passive to your listeners to not block scroll performance.
        xonCapture      // Add this modifier if you want to execute this listener in the event's capturing phase, e.g. before the event bubbles from the target element up the DOM.
    );

const
    __AlpineJSOnModifier: array[TAlpineJSOnModifier] of string = (
            '',
            '.prevent',
            '.stop',
            '.outside',
            '.window',
            '.document',
            '.once',
            '.debounce',
            '.throttle',
            '.self',
            '.camel',
            '.dot',
            '.passive',
            '.capture'
    );

type
    TAlpineJSKey = (
        keyShift,
        keyEnter,
        keySpace,
        keyCtrl,
        keyCmd,
        keyMeta,
        keyAlt,
        keyUp,
        keyDown,
        keyLeft,
        keyRight,
        keyEscape,
        keyTab,
        keyCapslock,
        keyEqual,
        keyPeriod,
        keySlash
    );

    TAlpineJSKeys = set of TAlpineJSKey;
const
    __AlpineJSKey: array[TAlpineJSKey] of string = (
            '.shift',
            '.enter',
            '.space',
            '.ctrl',
            '.cmd',
            '.meta',
            '.alt',
            '.up',
            '.down',
            '.left',
            '.right',
            '.escape',
            '.tab',
            '.caps-lock',
            '.equal',
            '.period',
            '.slash'
    );

type
    TAlpineJSTransitionModifier = (
        transNone,
        transDuration,
        transDelay,
        transOpacity,
        transScale
    );

    TAlpineJSTransitionDirective = (
        trdNone,
        trdEnter,
        trdEnterStart,
        trdEnterEnd,
        trdLeave,
        trdLeaveStart,
        trdLeaveEnd
    );
const
    __AlpineJSTransitionModifier: array[TAlpineJSTransitionModifier] of string = (
        '',
        '.duration',
        '.delay',
        '.opacity',
        '.scale'
    );

    __AlpineJSTransitionDirective: array[TAlpineJSTransitionDirective] of string = (
        '',
        ':enter',
        ':enter-start',
        ':enter-end',
        ':leave',
        ':leave-start',
        ':leave-end'
    );




	{ THtmlBuilderAlpineJS }
type
    THtmlBuilderAlpineJS = type helper for THtmlElement
    public
        function cdn(constref _container: THtmlCollection): THtmlCollection;

        function xData(const _v: string) : THtmlElement; overload;
        function xData(constref _json: TJSONObject; const _autoFree: boolean = false) : THtmlElement; overload;

        function xBind(const _name: string; const _value: string) : THtmlElement;

        function xOn(const _event: string;
                     const _handler: string;
                     const _modifier: TAlpineJSOnModifier = xonNone;
                     const _qualifier: string = '') : THtmlElement; overload;

        function xOn(const _event: THtmlDOMEvent;
                     const _handler: string;
                     const _modifier: TAlpineJSOnModifier = xonNone;
                     const _qualifier: string = '') : THtmlElement; overload;


        function xKeyup(const _keys: TAlpineJSKeys; const _handler: string): THtmlElement;
        function xKeyDown(const _keys: TAlpineJSKeys; const _handler: string): THtmlElement;

        function xText(const _v: string) : THtmlElement;
        function xHtml(const _v: string) : THtmlElement;
        function xModel(const _v: string) : THtmlElement;
        function xModelable(const _v: string) : THtmlElement; //x-modelable allows you to expose any Alpine property as the target of the x-model directive.

        function xInit(const _v: string):THtmlElement;
        function xShow(const _v: string) : THtmlElement;

        // Assigns a class to a transition directive
        function xTransitionClass(
                     const _directive: TAlpineJSTransitionDirective;
                     const _class: string ='';
                     const _modifier: TAlpineJSTransitionModifier = transNone
        ) : THtmlElement;

        // Assigns a class to a transition directive
        function xTransitionModifier(
                     const _directive: TAlpineJSTransitionDirective;
                     const _modifier: TAlpineJSTransitionModifier;
                     const _modifierValue: string = ''
        ) : THtmlElement;
        // Construct the transition manually
        function xTransition(_directive: string; _handler: string=''): THtmlElement;

        function xFor(const _v: string; _key: string = '') : THtmlElement; overload; // This only works on THtmlTemplate

        function xIf(const _v: string):THtmlElement;

        function xEffect(const _v: string):THtmlElement;
        function xRef(const _v: string):THtmlElement;
        function xCloak(const _active:boolean = true):THtmlElement;
        function xIgnore(const _active: boolean = true): THtmlElement;

        function xID(const _v: string): THtmlElement;
        function xteleport(const _v: string): THtmlElement;

	end;

implementation

{ THtmlBuilderAlpineJS }

function THtmlBuilderAlpineJS.cdn(constref _container: THtmlCollection
	): THtmlCollection;
begin
    Result:= _container;
    with Result do begin
        script
            .setSrc('https://cdn.jsdelivr.net/npm/alpinejs@3.13.5/dist/cdn.min.js')
            .setDefer;
	end;
end;

function THtmlBuilderAlpineJS.xData(const _v: string): THtmlElement;
begin
    Result:= self;
    Result.setAttr('data', _v);
end;

function THtmlBuilderAlpineJS.xData(constref _json: TJSONObject;
	const _autoFree: boolean): THtmlElement;
var
  _myjson: TJSONObject;
begin
    Result:= self;
    if assigned(_json) then begin
	    if _autoFree then
	        _myjson := _json
	    else
	        _myjson := _json.Clone as TJSONObject;
	    Result:= xData(_myjson.FormatJSON(AsCompactJSON));
	    _myjson.Free;
	end;
end;

function THtmlBuilderAlpineJS.xBind(const _name: string; const _value: string
	): THtmlElement;
begin
    Result := Self;
    setAttr('x-bind:' + _name, _value);
end;


function THtmlBuilderAlpineJS.xOn(const _event: string; const _handler: string;
	const _modifier: TAlpineJSOnModifier; const _qualifier: string): THtmlElement;
var
    _attr: string = 'x-on:%s';
begin
    Result := Self;
    // append the modifier
    _attr += __AlpineJSOnModifier[_modifier];

    // append the qualifier for the appropriate modifiers
    case _modifier of
        xonPrevent: ;
        xonStop: ;
        xonOutside: ;
        xonWindow: ;
        xonDocument: ;
        xonOnce: ;
        xonDebounce: _attr += _qualifier; // usually to add the time in ms. example "x-on:input.debounce.500ms"
        xonThrottle: _attr += _qualifier;
        xonSelf: ;
        xonCamel: ;
        xonDot: ;
        xonPassive: ;
        xonCapture: ;
    end;

    setAttr(Format(_attr, [_event]), _handler);
end;

function THtmlBuilderAlpineJS.xOn(const _event: THtmlDOMEvent;
	const _handler: string; const _modifier: TAlpineJSOnModifier;
	const _qualifier: string): THtmlElement;
begin
    result:= xOn(__DOMEvents[_event], _handler, _modifier, _qualifier);
end;

function THtmlBuilderAlpineJS.xKeyup(const _keys: TAlpineJSKeys;
	const _handler: string): THtmlElement;
var
    _attr: string = 'x-on:keyup';
	_k: TAlpineJSKey;
begin
    Result := Self;
    for _k in _keys do begin
        _attr += __AlpineJSKey[_k];
	end;
    setAttr(_attr, _handler);
end;

function THtmlBuilderAlpineJS.xKeyDown(const _keys: TAlpineJSKeys;
	const _handler: string): THtmlElement;
var
    _attr: string = 'x-on:keydown';
	_k: TAlpineJSKey;
begin
    Result := Self;
    for _k in _keys do begin
        _attr += __AlpineJSKey[_k];
	end;
    setAttr(_attr, _handler);
end;

function THtmlBuilderAlpineJS.xText(const _v: string): THtmlElement;
begin
    Result := Self;
    setAttr('x-text', _v);
end;

function THtmlBuilderAlpineJS.xHtml(const _v: string): THtmlElement;
begin
    Result := Self;
    setAttr('x-html', _v);

end;

function THtmlBuilderAlpineJS.xModel(const _v: string): THtmlElement;
begin
    Result := Self;
    setAttr('x-model', _v);
end;

function THtmlBuilderAlpineJS.xModelable(const _v: string): THtmlElement;
begin
    Result := Self;
    setAttr('x-modelable', _v);
end;

function THtmlBuilderAlpineJS.xInit(const _v: string): THtmlElement;
begin
    Result := Self;
    setAttr('x-init', _v);
end;

function THtmlBuilderAlpineJS.xShow(const _v: string): THtmlElement;
begin
    Result := Self;
    setAttr('x-show', _v);
end;

function THtmlBuilderAlpineJS.xTransitionClass(
	const _directive: TAlpineJSTransitionDirective; const _class: string;
	const _modifier: TAlpineJSTransitionModifier): THtmlElement;
var
    _attr: string = 'x-transition';
begin
    Result := Self;
    if _directive > trdNone then begin
        _attr += __AlpineJSTransitionDirective[_directive];
	end;

    if _modifier > transNone then begin
        _attr += __AlpineJSTransitionModifier[_modifier];
	end;

    setAttr(_attr, _class);
end;

function THtmlBuilderAlpineJS.xTransitionModifier(
	const _directive: TAlpineJSTransitionDirective;
	const _modifier: TAlpineJSTransitionModifier; const _modifierValue: string
	): THtmlElement;
var
    _attr: string = 'x-transition';
begin
    Result := Self;
    if _directive > trdNone then begin
        _attr += __AlpineJSTransitionDirective[_directive];
	end;

    if _modifier > transNone then begin
        _attr += __AlpineJSTransitionModifier[_modifier];
        if not _modifierValue.isEmpty then begin
            if not _modifierValue.StartsWith('.') then
                _attr += '.';
            _attr += _modifierValue;
		end;
	end;
    setAttrFlag(_attr);
end;


function THtmlBuilderAlpineJS.xTransition(_directive: string; _handler: string
	): THtmlElement;
var
    _attr: string = 'x-transition';
begin
    Result:= Self;
    if not _directive.IsEmpty then
        _attr += _directive;

    if _handler.isEmpty then
        setAttrFlag(_attr)
    else
        setAttr(_attr, _handler);
end;

function THtmlBuilderAlpineJS.xFor(const _v: string; _key: string
	): THtmlElement;
begin
    Result := Self;
    if Self.ClassType =  THtmlTemplate then begin
        setAttr('x-for', _v);
        if not _key.isEmpty then
            setAttr(':key', _key);
	end
    else
        raise Exception.Create('AlpineJS requires "x-for" to be used on THTMLTemplate. It will not work on ' + Self.ClassName + '.');
end;

function THtmlBuilderAlpineJS.xIf(const _v: string): THtmlElement;
begin
    Result := Self;
    if Self.ClassType = THtmlTemplate then begin
        setAttr('x-if', _v);
	end
    else
        raise Exception.Create('AlpineJS requires "x-if" to be used on THTMLTemplate. It will not work on ' + Self.ClassName + '.');
end;

function THtmlBuilderAlpineJS.xEffect(const _v: string): THtmlElement;
begin
    Result := Self;
    setAttr('x-effect', _v);
end;

function THtmlBuilderAlpineJS.xRef(const _v: string): THtmlElement;
begin
    Result := Self;
    setAttr('x-ref', _v);
end;

function THtmlBuilderAlpineJS.xCloak(const _active: boolean): THtmlElement;
begin
    Result := Self;
    case _active of
        True:  setAttrFlag('x-cloak');
        False: rmAttrFlag('x-cloak');
    end;
end;

function THtmlBuilderAlpineJS.xIgnore(const _active: boolean): THtmlElement;
const
    __xignore = 'x-ignore';
begin
    Result := Self;
    case _active of
        True:  setAttrFlag(__xignore);
        False: rmAttrFlag (__xignore);
    end;
end;

function THtmlBuilderAlpineJS.xID(const _v: string): THtmlElement;
begin
    Result := Self;
    setAttr('x-id', _v);
end;

function THtmlBuilderAlpineJS.xteleport(const _v: string): THtmlElement;
begin
    Result := Self;
    setAttr('x-teleport', _v);
end;

end.

