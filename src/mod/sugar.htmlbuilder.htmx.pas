unit sugar.htmlbuilder.htmx;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
    Classes, SysUtils, sugar.htmlbuilder, fpjson;

type
    THtmxAttribute = (
        hx_get,
        hx_post,
        hx_on,
        hx_push_url,
        hx_select,
        hx_select_oob,
        hx_swap,
        hx_swap_oob,
        hx_target,
        hx_trigger,
        hx_vals,
        //-------
        hx_boost,
        hx_confirm,
        hx_delete,
        hx_disable,
        hx_disable_elt,
        hx_disinherit,
        hx_encoding,
        hx_ext,
        hx_headers,
        hx_history,
        hx_history_elt,
        hx_include,
        hx_indicator,
        hx_params,
        hx_patch,
        hx_preserve,
        hx_prompt,
        hx_put,
        hx_replace_url,
        hx_request,
        hx_sse,
        hx_sync,
        hx_validate,
        hx_vars, // deprecated
        hx_ws    // moved
    );

    THtmxAttributeDef = record
        attr: string;
        desc: string;
	end;

const
    HTMXAttr: array[THtmxAttribute] of THtmxAttributeDef = (
	    (attr:'hx-get';         desc:'issues a GET to the specified URL'),
	    (attr: 'hx-post';       desc: 'issues a POST to the specified URL'),
	    (attr: 'hx-on*';        desc: 'handle events with inline scripts on elements'),
	    (attr: 'hx-push-url';   desc: 'push a URL into the browser location bar to create history'),
	    (attr: 'hx-select';     desc: 'select content to swap in from a response'),
	    (attr: 'hx-select-oob'; desc: 'select content to swap in from a response, somewhere other than the target (out of band)'),
	    (attr: 'hx-swap';       desc: 'controls how content will swap in (outerHTML, beforeend, afterend, …)'),
	    (attr: 'hx-swap-oob';   desc: 'mark element to swap in from a response (out of band)'),
	    (attr: 'hx-target';     desc: 'specifies the target element to be swapped'),
	    (attr: 'hx-trigger';    desc: 'specifies the event that triggers the request'),
	    (attr: 'hx-vals';       desc: 'add values to submit with the request (JSON format)'),

        (attr: 'hx-boost';      desc: 'add progressive enhancement for links and forms'),
        (attr: 'hx-confirm';    desc: 'shows a confirm() dialog before issuing a request'),
        (attr: 'hx-delete';     desc: 'issues a DELETE to the specified URL'),
        (attr: 'hx-disable';    desc: 'disables htmx processing for the given node and any children nodes'),
        (attr: 'hx-disabled-elt'; desc: 'adds the disabled attribute to the specified elements while a request is in flight'),
        (attr: 'hx-disinherit'; desc: 'control and disable automatic attribute inheritance for child nodes'),
        (attr: 'hx-encoding';   desc: 'changes the request encoding type'),
        (attr: 'hx-ext';        desc: 'extensions to use for this element'),
        (attr: 'hx-headers';    desc: 'adds to the headers that will be submitted with the request'),
        (attr: 'hx-history';    desc: 'prevent sensitive data being saved to the history cache'),
        (attr: 'hx-history-elt'; desc: 'the element to snapshot and restore during history navigation'),
        (attr: 'hx-include';    desc: 'include additional data in requests'),
        (attr: 'hx-indicator';  desc: 'the element to put the htmx-request class on during the request'),
        (attr: 'hx-params';     desc: 'filters the parameters that will be submitted with a request'),
        (attr: 'hx-patch';      desc: 'issues a PATCH to the specified URL'),
        (attr: 'hx-preserve';   desc: 'specifies elements to keep unchanged between requests'),
        (attr: 'hx-prompt';     desc: 'shows a prompt() before submitting a request'),
        (attr: 'hx-put';        desc: 'issues a PUT to the specified URL'),
        (attr: 'hx-replace-url'; desc: 'replace the URL in the browser location bar'),
        (attr: 'hx-request';    desc: 'configures various aspects of the request'),
        (attr: 'hx-sse';        desc: 'has been moved to an extension. Documentation for older versions'),
        (attr: 'hx-sync';       desc: 'control how requests made by different elements are synchronized'),
        (attr: 'hx-validate';   desc: 'force elements to validate themselves before a request'),
        (attr: 'hx-vars';       desc: 'adds values dynamically to the parameters to submit with the request (deprecated, please use hx-vals)'),
        (attr: 'hx-ws';         desc: 'has been moved to an extension. Documentation for older versions')
    );
type
    THtmxEvent = (
		htmxAbort,
		htmxAfterOnLoad,
		htmxAfterProcessNode,
		htmxAfterRequest,
		htmxAfterSettle,
		htmxAfterSwap,
		htmxbeforeCleanupElement,
		htmxbeforeOnLoad,
		htmxBeforeProcessNode,
		htmxBeforeRequest,
		htmxBeforeSwap,
		htmxBeforeSend,
		htmxConfigRequest,
		htmxConfirm,
		htmxHistoryCacheError,
		htmxHistoryCacheMiss,
		htmxHistoryCacheMissError,
		htmxHistoryCacheMissLoad,
		htmxHistoryRestore,
		htmxBeforeHistorySave,
		htmxLoad,
		htmxNoSSESourceError,
		htmxOnLoadError,
		htmxOobAfterSwap,
		htmxOobBeforeSwap,
		htmxOobErrorNoTarget,
		htmxPrompt,
		htmxPushedIntoHistory,
		htmxResponseError,
		htmxSendError,
		htmxSseError,
		htmxSseOpen,
		htmxSwapError,
		htmxTargetError,
		htmxTimeout,
		htmxValidationValidate,
		htmxValidationFailed,
		htmxValidationHalted,
		htmxXHRAbort,
		htmxXHRLoadEnd,
		htmxXHRLoadStart,
		htmxXHRProgress
    );

const
    HTMXEvents: array[THtmxEvent] of THtmxAttributeDef = (
	    (attr:'htmx:abort';             desc:'send this event to an element to abort a request'),
	    (attr:'htmx:afterOnLoad';       desc:'triggered after an AJAX request has completed processing a successful response'),
	    (attr:'htmx:afterProcessNode';  desc:'triggered after htmx has initialized a node'),
	    (attr:'htmx:afterRequest';      desc:'triggered after an AJAX request has completed'),
	    (attr:'htmx:afterSettle';       desc:'triggered after the DOM has settled'),
	    (attr:'htmx:afterSwap';         desc:'triggered after new content has been swapped in'),
	    (attr:'htmx:beforeCleanupElement';desc:'triggered before htmx disables an element or removes it from the DOM'),
	    (attr:'htmx:beforeOnLoad';      desc:'triggered before any response processing occurs'),
	    (attr:'htmx:beforeProcessNode'; desc:'triggered before htmx initializes a node'),
	    (attr:'htmx:beforeRequest';     desc:'triggered before an AJAX request is made'),
	    (attr:'htmx:beforeSwap';        desc:'triggered before a swap is done, allows you to configure the swap'),
	    (attr:'htmx:beforeSend';        desc:'triggered just before an ajax request is sent'),
	    (attr:'htmx:configRequest';     desc:'triggered before the request, allows you to customize parameters, headers'),
	    (attr:'htmx:confirm';           desc:'triggered after a trigger occurs on an element, allows you to cancel (or delay) issuing the AJAX request'),
	    (attr:'htmx:historyCacheError'; desc:'triggered on an error during cache writing'),
	    (attr:'htmx:historyCacheMiss';  desc:'triggered on a cache miss in the history subsystem'),
	    (attr:'htmx:historyCacheMissError';desc:'triggered on a unsuccessful remote retrieval'),
	    (attr:'htmx:historyCacheMissLoad';desc:'triggered on a successful remote retrieval'),
	    (attr:'htmx:historyRestore';    desc:'triggered when htmx handles a history restoration action'),
	    (attr:'htmx:beforeHistorySave'; desc:'triggered before content is saved to the history cache'),
	    (attr:'htmx:load';              desc:'triggered when new content is added to the DOM'),
	    (attr:'htmx:noSSESourceError';  desc:'triggered when an element refers to a SSE event in its trigger, but no parent SSE source has been defined'),
	    (attr:'htmx:onLoadError';       desc:'triggered when an exception occurs during the onLoad handling in htmx'),
	    (attr:'htmx:oobAfterSwap';      desc:'triggered after an out of band element as been swapped in'),
	    (attr:'htmx:oobBeforeSwap';     desc:'triggered before an out of band element swap is done, allows you to configure the swap'),
	    (attr:'htmx:oobErrorNoTarget';  desc:'triggered when an out of band element does not have a matching ID in the current DOM'),
	    (attr:'htmx:prompt';            desc:'triggered after a prompt is shown'),
	    (attr:'htmx:pushedIntoHistory'; desc:'triggered after an url is pushed into history'),
	    (attr:'htmx:responseError';     desc:'triggered when an HTTP response error (non-200 or 300 response code) occurs'),
	    (attr:'htmx:sendError';         desc:'triggered when a network error prevents an HTTP request from happening'),
	    (attr:'htmx:sseError';          desc:'triggered when an error occurs with a SSE source'),
	    (attr:'htmx:sseOpen';           desc:'triggered when a SSE source is opened'),
	    (attr:'htmx:swapError';         desc:'triggered when an error occurs during the swap phase'),
	    (attr:'htmx:targetError';       desc:'triggered when an invalid target is specified'),
	    (attr:'htmx:timeout';           desc:'triggered when a request timeout occurs'),
	    (attr:'htmx:validation:validate';desc:'triggered before an element is validated'),
	    (attr:'htmx:validation:failed'; desc:'triggered when an element fails validation'),
	    (attr:'htmx:validation:halted'; desc:'triggered when a request is halted due to validation errors'),
	    (attr:'htmx:xhr:abort';         desc:'triggered when an ajax request aborts'),
	    (attr:'htmx:xhr:loadend';       desc:'triggered when an ajax request ends'),
	    (attr:'htmx:xhr:loadstart';     desc:'triggered when an ajax request starts'),
	    (attr:'htmx:xhr:progress';      desc:'triggered periodically during an ajax request that supports progress events')
    );

type
    THtmxSwap = (
        hxsInnerHTML,
        hxsOuterHTML,
        hxsBeforeBegin,
        hxsAfterBegin,
        hxsBeforeEnd,
        hxsAfterEnd,
        hxsDelete,
        hxsNone
    );
const
    HTMXSwap: array[THtmxSwap] of THtmxAttributeDef = (
        (attr: 'innerHTML';     desc: 'Replace the inner html of the target element'),
        (attr: 'outerHTML';     desc: 'Replace the entire target element with the response'),
        (attr: 'beforebegin';   desc: 'Insert the response before the target element'),
        (attr: 'afterbegin';    desc: 'nsert the response before the first child of the target element'),
        (attr: 'beforeend';     desc: 'Insert the response after the last child of the target element'),
        (attr: 'afterend';      desc: 'Insert the response after the target element'),
        (attr: 'delete';        desc: 'Deletes the target element regardless of the response'),
        (attr: 'none';          desc: 'Does not append content from response (out of band items will still be processed).')
    );

type
    THTMXSwapModifier = (
        hxsmNone,
        hxsmSwap,
        hxsmSettle,
        hxsmIgnoreTitle,
        hxsmScrollTop,
        hxsmScrollBottom,
        hxsmShowTop,
        hxsmShowBottom,
        hxsmFocusScroll


    );
const
    __HTMXSwapModifier: array[THTMXSwapModifier] of THtmxAttributeDef = (
        (attr: '';              desc: ''),
        (attr: 'innerHTML';     desc: 'Replace the inner html of the target element'),
	    (attr: 'outerHTML';     desc: 'Replace the entire target element with the response'),
	    (attr: 'beforebegin';   desc: 'Insert the response before the target element'),
	    (attr: 'afterbegin';    desc: 'Insert the response before the first child of the target element'),
	    (attr: 'beforeend';     desc: 'Insert the response after the last child of the target element'),
	    (attr: 'afterend';      desc: 'Insert the response after the target element'),
	    (attr: 'delete';        desc: 'Deletes the target element regardless of the response'),
	    (attr: 'none';          desc: 'Does not append content from response (out of band items will still be processed).')
    );

	{ THtmlBuilderHtmx }
type
    THtmlBuilderHtmx = type helper for THtmlElement
    public
        function cdn(constref _container: THtmlCollection): THtmlCollection;
        function hxGet(_v:string): THtmlElement;
        function hxPost(_v:string): THtmlElement;

        function hxOn(_event: string; _v:string): THtmlElement; overload;
        function hxOn(_event: THtmlDOMEvent; _v:string): THtmlElement; overload;
        function hxOn(_event: THtmxEvent; _v:string): THtmlElement; overload;

        function hxPushUrl(_state: boolean): THtmlElement; overload;
        function hxPushUrl(_url: string): THtmlElement; overload;

        function hxSelect(_v:string): THtmlElement;
        function hxSelectOob(_v:string): THtmlElement;

        function hxSwap(_s: THtmxSwap;                    // swap value
                        _m: THTMXSwapModifier = hxsmNone; // modifiers
                        _add: string  = ''                // additional attributes
                        ): THtmlElement;

        function hxSwapOob(_v: string): THtmlElement;

        {
        The hx-target attribute allows you to target a different element for swapping than the one issuing the AJAX request. The value of this attribute can be:

        A CSS query selector of the element to target.
        this which indicates that the element that the hx-target attribute is on is the target.
        closest <CSS selector> which will find the closest ancestor element or itself, that matches the given CSS selector (e.g. closest tr will target the closest table row to the element).
        find <CSS selector> which will find the first child descendant element that matches the given CSS selector.
        next which resolves to element.nextElementSibling
        next <CSS selector> which will scan the DOM forward for the first element that matches the given CSS selector. (e.g. next .error will target the closest following sibling element with error class)
        previous which resolves to element.previousElementSibling
        previous <CSS selector> which will scan the DOM backwards for the first element that matches the given CSS selector. (e.g previous .error will target the closest previous sibling with error class)        }
        function hxTarget(_v: string ): THtmlElement; overload;

        type
            THTMXTarget = (
                xThis,
                xClosest,
                xFind,
                xNext,
                xPrevious
            );
        const
            __HTMXTarget: array[THTMXTarget] of string = (
                'this',
                'closest',
                'find',
                'next',
                'previous'
            );
        function hxTarget(_target: THTMXTarget; _selector: string=''): THtmlElement; overload;

        { SEE: https://htmx.org/attributes/hx-trigger/
        This has far too many options. It is easier to construct the value text in code as needed.
        Parameterizing would be counterproductive}
        function hxTrigger(_v: string): THtmlElement; overload;

        function hxVals(_v: string): THtmlElement; overload;
        function hxVals(_j: TJSONObject; _eval: boolean = false): THtmlElement; overload;

        //-------
        function hxBoost: THtmlElement; overload;
        function hxConfirm(_v: string): THtmlElement; overload;
        function hxDelete(_v: string): THtmlElement; overload;
        function hxDisable(_v: string): THtmlElement; overload;
        function hxDisable_elt(_v: string): THtmlElement; overload;
        function hxDisinherit(_v: string): THtmlElement; overload;
        function hxEncoding(_v: string): THtmlElement; overload;
        function hxExt(_v: string): THtmlElement; overload;
        function hxHeaders(_v: string): THtmlElement; overload;
        function hxHistory(_v: string): THtmlElement; overload;
        function hxHistoryElt(_v: string): THtmlElement; overload;
        function hxInclude(_v: string): THtmlElement; overload;
        function hxIndicator(_v: string): THtmlElement; overload;
        function hxParams(_v: string): THtmlElement; overload;
        function hxPatch(_v: string): THtmlElement; overload;
        function hxPreserve(_v: string): THtmlElement; overload;
        function hxPrompt(_v: string): THtmlElement; overload;
        function hxPut(_v: string): THtmlElement; overload;
        function hxReplaceUrl(_v: string): THtmlElement; overload;
        function hxRequest(_v: string): THtmlElement; overload;
        function hxSse(_v: string): THtmlElement; overload;
        function hxSync(_v: string): THtmlElement; overload;
        function hxValidate(_v: string): THtmlElement; overload;
        function hxVars(_v: string): THtmlElement; overload; deprecated;
        function hxWs(_v: string): THtmlElement; overload;   // moved
	end;

implementation

{ THtmlBuilderHtmx }

function THtmlBuilderHtmx.cdn(constref _container: THtmlCollection
	): THtmlCollection;
begin
  //<script src="https://unpkg.com/htmx.org@1.9.10"
  Result:= _container;
  with Result do begin
      script
        .setSrc ('https://unpkg.com/htmx.org@1.9.10')
        .setAttr('integrity', 'sha384-D1Kt99CQMDuVetoL1lrYwg5t+9QdHe7NLX/SoJYkXDFfX37iInKRy5xLSi8nO7UC')
        .setAttr('crossorigin', 'anyonymous')
        .setDefer;
  end;
end;

function THtmlBuilderHtmx.hxGet(_v: string): THtmlElement;
begin
    Result:= self;
    setAttr('hx-get', _v);
end;

function THtmlBuilderHtmx.hxPost(_v: string): THtmlElement;
begin
    Result:= self;
    setAttr('hx-post', _v);
end;

function THtmlBuilderHtmx.hxOn(_event: string; _v: string): THtmlElement;
begin
    Result:= self;
    SetAttr('hx-on:' + _event, _v);
end;

function THtmlBuilderHtmx.hxOn(_event: THtmlDOMEvent; _v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxOn(_event: THtmxEvent; _v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxPushUrl(_state: boolean): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxPushUrl(_url: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxSelect(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxSelectOob(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxSwap(_s: THtmxSwap; _m: THTMXSwapModifier;
	_add: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxSwapOob(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxTarget(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxTarget(_target: THTMXTarget; _selector: string
	): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxTrigger(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxVals(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxVals(_j: TJSONObject; _eval: boolean): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxBoost: THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxConfirm(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxDelete(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxDisable(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxDisable_elt(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxDisinherit(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxEncoding(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxExt(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxHeaders(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxHistory(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxHistoryElt(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxInclude(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxIndicator(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxParams(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxPatch(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxPreserve(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxPrompt(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxPut(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxReplaceUrl(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxRequest(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxSse(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxSync(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxValidate(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxVars(_v: string): THtmlElement;
begin

end;

function THtmlBuilderHtmx.hxWs(_v: string): THtmlElement;
begin

end;

end.

