unit sugar.htmlbuilder;

{This unit is built to use VueJS and UIKit.
It is not general enough. Refactoring is necessary to use other Frameworks}

{$mode objfpc}{$H+}
{$modeSwitch typehelpers}

interface

uses
    Classes, SysUtils, contnrs, sugar.collections, sugar.textfiler;

const
    HTML_DOC_HEADER = '<!DOCTYPE HTML>';
    br = '</br>';
    DEFAULT_ELEMENT_ID_PREFIX = 'E';
    {use this prefix when creating template fields in a view}
    DEFAULT_FIELD_PREFIX = '_';

    TEMPLATE_PREFIX = '{{'; TEMPLATE_POSTFIX = '}}';

    JSProp = '%s: %s';
    DEFAULT_INDENT = '    ';

type
    THtmlGlobalAttributes = (
        attr_accesskey,
        attr_autocapitalize,
        attr_autofocus,
        attr_class,
        attr_contenteditable,
        attr_contextmenu,       // Non-standardDeprecated,
        attr_data,              // data-xxx
        attr_dir,
        attr_draggable,
        attr_enterkeyhint,
        attr_exportparts,
        attr_hidden,
        attr_id,
        attr_inert,
        attr_inputmode,
        attr_is,
        attr_itemid,
        attr_itemprop,
        attr_itemref,
        attr_itemscope,
        attr_itemtype,
        attr_lang,
        attr_nonce,
        attr_part,
        attr_popover,
        attr_slot,
        attr_spellcheck,
        attr_style,
        attr_tabindex,
        attr_title,
        attr_translate,
        attr_virtualkeyboardpolicy
    );

    THtmlDOMEvent = (
      // Document Events
        dom_afterscriptexecute,  //    Non-standard
        dom_beforescriptexecute, //    Non-standard
        dom_copy,
        dom_cut,
        dom_DOMContentLoaded,
        dom_fullscreenchange,
        dom_fullscreenerror,
        dom_paste,
        dom_pointerlockchange,
        dom_pointerlockerror,
        dom_prerenderingchange,  // Experimental
        dom_readystatechange,
        dom_scroll,
        dom_scrollend,
        dom_securitypolicyviolation,
        dom_selectionchange,
        dom_visibilitychange,

        // Window Events
        dom_afterprint,
        dom_appinstalled,
        dom_beforeinstallprompt,
        dom_beforeprint,
        dom_beforeunload,
        dom_blur,
        //copy,
        //cut,
        dom_devicemotion,
        dom_deviceorientation,
        dom_deviceorientationabsolute,
        dom_error,
        dom_focus,
        dom_gamepadconnected,
        dom_gamepaddisconnected,
        dom_hashchange,
        dom_languagechange,
        dom_load,
        dom_message,
        dom_messageerror,
        dom_offline,
        dom_online,
        dom_orientationchange, // Deprecated
        dom_pagehide,
        dom_pageshow,
        // paste,
        dom_popstate,
        dom_rejectionhandled,
        dom_resize,
        dom_storage,
        dom_unhandledrejection,
        dom_unload, // Deprecated
        dom_vrdisplayactivate,      // Non-standardDeprecated
        dom_vrdisplayconnect,       // Non-standardDeprecated
        dom_vrdisplaydeactivate,    // Non-standardDeprecated
        dom_vrdisplaydisconnect,    // Non-standardDeprecated
        dom_vrdisplaypresentchange, // Non-standardDeprecated

        // Element Events
        //afterscriptexecute, //    Non-standard
        dom_animationcancel,
        dom_animationend,
        dom_animationiteration,
        dom_animationstart,
        dom_auxclick,
        dom_beforeinput,
        dom_beforematch, //    Experimental
        //beforescriptexecute, //    Non-standard
        dom_beforexrselect, //    Experimental
        //blur,
        dom_click,
        dom_compositionend,
        dom_compositionstart,
        dom_compositionupdate,
        dom_contentvisibilityautostatechange, // Experimental
        dom_contextmenu,
        //copy,
        //cut,
        dom_dblclick,
        dom_DOMActivate, // Deprecated
        dom_DOMMouseScroll, // Non-standardDeprecated
        //focus,
        dom_focusin,
        dom_focusout,
        //fullscreenchange,
        //fullscreenerror,
        dom_gesturechange,  // Non-standard
        dom_gestureend, // Non-standard
        dom_gesturestart, //    Non-standard
        dom_gotpointercapture,
        dom_input,
        dom_keydown,
        dom_keypress, // Deprecated
        dom_keyup, //
        dom_lostpointercapture,
        dom_mousedown,
        dom_mouseenter,
        dom_mouseleave,
        dom_mousemove,
        dom_mouseout,
        dom_mouseover,
        dom_mouseup,
        dom_mousewheel, // Non-standardDeprecated
        dom_MozMousePixelScroll, // Non-standardDeprecated
        //paste,
        dom_pointercancel,
        dom_pointerdown,
        dom_pointerenter,
        dom_pointerleave,
        dom_pointermove,
        dom_pointerout,
        dom_pointerover,
        dom_pointerrawupdate, // Experimental
        dom_pointerup,
        //scroll,
        //scrollend,
        //securitypolicyviolation,
        dom_touchcancel,
        dom_touchend,
        dom_touchmove,
        dom_touchstart,
        dom_transitioncancel,
        dom_transitionend,
        dom_transitionrun,
        dom_transitionstart,
        dom_webkitmouseforcechanged, //   Non-standard
        dom_webkitmouseforcedown, // Non-standard
        dom_webkitmouseforceup, // Non-standard
        dom_webkitmouseforcewillbegin, // Non-standard
        dom_wheel,

        // Form Events
        dom_formdata,
        dom_reset,
        dom_submit,

        // Details Element
        dom_toggle
    );
const
    __DOMEvents: array[THtmlDOMEvent] of string = (
    // Document Events
      'afterscriptexecute',   //    Non-standard
      'beforescriptexecute',  //    Non-standard
      'copy',
      'cut',
      'DOMContentLoaded',
      'fullscreenchange',
      'fullscreenerror',
      'paste',
      'pointerlockchange',
      'pointerlockerror',
      'prerenderingchange',   // Experimental
      'readystatechange',
      'scroll',
      'scrollend',
      'securitypolicyviolation',
      'selectionchange',
      'visibilitychange',

      // Window Events
      'afterprint',
      'appinstalled',
      'beforeinstallprompt',
      'beforeprint',
      'beforeunload',
      'blur',
      //copy',
      //cut',
      'devicemotion',
      'deviceorientation',
      'deviceorientationabsolute',
      'error',
      'focus',
      'gamepadconnected',
      'gamepaddisconnected',
      'hashchange',
      'languagechange',
      'load',
      'message',
      'messageerror',
      'offline',
      'online',
      'orientationchange',  // Deprecated
      'pagehide',
      'pageshow',
      // paste',
      'popstate',
      'rejectionhandled',
      'resize',
      'storage',
      'unhandledrejection',
      'unload',  // Deprecated
      'vrdisplayactivate',       // Non-standardDeprecated
      'vrdisplayconnect',        // Non-standardDeprecated
      'vrdisplaydeactivate',     // Non-standardDeprecated
      'vrdisplaydisconnect',     // Non-standardDeprecated
      'vrdisplaypresentchange',  // Non-standardDeprecated

      // Element Events
      //afterscriptexecute',  //    Non-standard
      'animationcancel',
      'animationend',
      'animationiteration',
      'animationstart',
      'auxclick',
      'beforeinput',
      'beforematch',  //    Experimental
      //beforescriptexecute',  //    Non-standard
      'beforexrselect',  //    Experimental
      //blur',
      'click',
      'compositionend',
      'compositionstart',
      'compositionupdate',
      'contentvisibilityautostatechange',  // Experimental
      'contextmenu',
      //copy',
      //cut',
      'dblclick',
      'DOMActivate',  // Deprecated
      'DOMMouseScroll',  // Non-standardDeprecated
      //focus',
      'focusin',
      'focusout',
      //fullscreenchange',
      //fullscreenerror',
      'gesturechange',   // Non-standard
      'gestureend',  // Non-standard
      'gesturestart',  //    Non-standard
      'gotpointercapture',
      'input',
      'keydown',
      'keypress',  // Deprecated
      'keyup',  //
      'lostpointercapture',
      'mousedown',
      'mouseenter',
      'mouseleave',
      'mousemove',
      'mouseout',
      'mouseover',
      'mouseup',
      'mousewheel',  // Non-standardDeprecated
      'MozMousePixelScroll',  // Non-standardDeprecated
      //paste',
      'pointercancel',
      'pointerdown',
      'pointerenter',
      'pointerleave',
      'pointermove',
      'pointerout',
      'pointerover',
      'pointerrawupdate',  // Experimental
      'pointerup',
      //scroll',
      //scrollend',
      //securitypolicyviolation',
      'touchcancel',
      'touchend',
      'touchmove',
      'touchstart',
      'transitioncancel',
      'transitionend',
      'transitionrun',
      'transitionstart',
      'webkitmouseforcechanged',  //   Non-standard
      'webkitmouseforcedown',  // Non-standard
      'webkitmouseforceup',  // Non-standard
      'webkitmouseforcewillbegin',  // Non-standard
      'wheel',

      // Form Events
      'formdata',
      'reset',
      'submit',
      'toggle'
    );

type

    THtmlMediaEvents = (
        mevUnknown
    );

	{ THtmlGlobalAttributesHelper }

    THtmlGlobalAttributesHelper = type helper for THtmlGlobalAttributes
        function asString: string;
        function asString(_v: string): string; // appends this text to the attribute (needed for _data)
    end;



    THtmlInputType = (inputButton, inputCheckbox, inputColor, inputDate,
        inputDatetime_local, inputEmail, inputFile, inputHidden, inputImage,
        inputMonth, inputNumber, inputPassword, inputRadio, inputRange,
        inputResetbtn, inputSearch, inputSubmit, inputTel, inputText,
        inputTime, inputUrl, inputWeek);

    { THtmlInputTypeHelper }

    THtmlInputTypeHelper = type helper for THtmlInputType
        function toString: string;
        function fromString(_str: string): THtmlInputtype;
    end;

    // for the anchor tag <a>
    THtmlAReferrerPolicy = (no_referrer, no_referrer_when_downgrade, origin, origin_when_cross_origin, unsafe_url);

    { THtmlAReferrerPolicyHelper }

    THtmlAReferrerPolicyHelper = type helper for THtmlAReferrerPolicy
        function toString: string;
        function fromString(_str: string): THtmlAReferrerPolicy;
    end;


    THtmlARel = (alternate, author, bookmark, _external, help, license, Next,
        nofollow, noreferrer, noopener, prev, rel_search, tag);

    { THtmlARelHelper }

    THtmlARelHelper = type helper for THtmlARel
        function toString: string;
        function fromString(_str: string): THtmlARel;
    end;

    THtmlATarget = (target_blank, target_parent, target_self, target_top, target_frame);

    { THtmlATargetHelper }

    THtmlATargetHelper = type helper for THtmlATarget
        function toString: string;
        function fromString(_str: string): THtmlATarget;
    end;

    THtmlFormEncType = (form_urlencoded, form__multipart_formdata, form_text_plain);

    { THtmlFormEncTypeHelper }

    THtmlFormEncTypeHelper = type helper for THtmlFormEncType
        function toString: string;
        procedure fromString(_str: string);
    end;

    THtmlFormMethod = (formPost, formGet);

    { THtmlFormMethodHelper }

    THtmlFormMethodHelper = type helper for THtmlFormMethod
        function toString: string;
        procedure fromString(_str: string);
    end;
{ 	TODO -ostanley -cFeature : TJavaScript class should be integrated into THMLDoc such that on
	load up it contains the code that is actually in on the disk.
    It writes to the disc only if the file is not available.
    This way, it serves the latest version of the code to the page
 }

    { TJavaScript }
    TJavaScript = class
    private
        myuseFileLoader: boolean;
		myCodeDefinition: string;
        procedure setuseFileLoader(const _useFileLoader: boolean);
		procedure setCodeDefinition(const _muster: string);
    protected
        myCode: TStringList;
        myName: string;
        myFileLoader: TTextFiler;
    public
        newObj: boolean; {Flag to check if this was just created by the JavaScriots List}
        property useFileLoader: boolean read myuseFileLoader write setuseFileLoader;
        constructor Create;
        destructor Destroy; override;

        function Name(_name: string): TJavaScript; overload;
        function Name: string;
        function url: string;
        function code: string; virtual;
        procedure Clear;

        function add(_line: string): TJavaScript;
        procedure touch; // creates an empty file if not exists;
        procedure load;
        procedure save;

        function RootDir(_RootDir: string): TJavaScript; overload;
        function RootDir: string; overload;
        function WebRoot(_WebRoot: string): TJavaScript; overload;
        function WebRoot: string; overload;

        function copyFrom(_source: TJavaScript): TJavaScript; virtual;
        function clone: TJavaScript; virtual;

        { defines how the final code is exported.
          This is used as the format string to contain the code)}
        property codeDefinition: string read myCodeDefinition write setCodeDefinition;
    end;

    TJavaScriptsBase = specialize GenericHashObjectList<TJavaScript>;
    { TJavaScript }

    { TJavaScripts }

    TJavaScripts = class(TJavaScriptsBase)
    public
        filePath: string;
        webPath: string;
        scriptName: string;
        useFileLoader: boolean;
        constructor Create(FreeObjects: boolean = True); reintroduce;
        function get(const s: shortstring): TJavaScript; reintroduce;
        function code: string;
        function save(_fileName: string = ''): boolean;
        function copyFrom(_source: TJavaScripts): TJavaScripts; virtual;
        function clone: TJavaScripts; virtual;
        function asProps: string;
        function asObject: string;
        function asClass: string;
        {Call this function to define all the scripts that need to be loaded from the disc
         - if the file is not found, it is created
         - if the file is found, it is loaded }
        function load(_scriptName: string): TJavaScript;
    end;

    TCSSStyleBase = class;
    THtmlStyle = class;
    THtmlStyleListBase = specialize GenericHashObjectList<THtmlStyle>;

    { THtmlStyleList}
    THtmlStyleList = class(THtmlStyleListBase)
	private
		myfilePath: string;
		myindent: string;
		mywebPath: string;
		mydocumentName: string;
		procedure setfilePath(const _filePath: string);
		procedure Setindent(const _value: string);

		procedure setwebPath(const _webPath: string);
		procedure setdocumentName(const _documentName: string);
	public
        property indent: string read myIndent write Setindent;
        property filePath: string read myfilePath write setfilePath;
        property webPath: string read mywebPath write setwebPath;
        property documentName: string read mydocumentName write setdocumentName;
        function get(const s: shortstring): THtmlStyle; reintroduce;
        function styleDef: string;
        function save(_filename: string): boolean;
    end;

	{ TCSSRule }

    TCSSRule = class
    protected
        myRule: TCSSStyleBase;
    public
        Name: string;
        constructor Create;
        destructor Destroy; override;
        function styleDef: string; virtual;
	end;

    TCSSRulesBase = specialize GenericHashObjectList<TCSSRule>;

	{ TCSSRules }

    TCSSRules = class(TCSSRulesBase)
        function ruleDef: string;
	end;

	{ TCSSFontFace }

    TCSSFontFace = class (TCSSRule)
    private
        mySourceFile: string;
		function getSrc: string;
		procedure setSrc(const _src: string);
		function getFontFamily: string;
		procedure setFontFamily(const _fontFamily: string);
		function getfontStretch: string;
		procedure setfontStretch(const _fontStretch: string);
		function getfontStyle: string;
		procedure setfontStyle(const _fontStyle: string);
		function getfontWeight: string;
		procedure setfontWeight(const _fontWeight: string);
		function getunicodeRange: string;
		procedure setunicodeRange(const _unicodeRange: string);
    public
        property src: string read getSrc write setSrc;
        property fontFamily: string read getFontFamily write setFontFamily;
        property fontStretch: string read getfontStretch write setfontStretch;
        property fontStyle: string read getfontStyle write setfontStyle;
        property fontWeight: string read getfontWeight write setfontWeight;
        property unicodeRange: string read getunicodeRange write setunicodeRange;
        function isEmbedded: boolean;

        {call with empty string to clear embedding}
        function embedFont(_fileName: string = ''): TCSSFontFace;
        function styleDef: string; override;

        constructor Create;
	end;

    TCSSPage = class (TCSSRule)

	end;

    { THtmlStyleSelector }
    THtmlStyleSelector = class
    private
        myCurrentSelector: string;
        {combinators for the current selector}
        myAdjacent: string;
		myindent: string;
        mySibling: string;
        myChild: string;
        myDecendent: string;
        myColumn: string;
        myFilePath: string;
        myWebPath: string;

       {
          we need a style list because of each selector
          can have multiple pseudo classes and elements and each needs
          a dedicated style.
       }
        myStyles: THtmlStyleList;
		myRules: TCSSRules;

        mydocumentName: string;
		myisCached: boolean;

        function getStyle(const _pseudo: string = ''): THtmlStyle;
        procedure onCreateStyleObject(_style: TObject);
		procedure setDocumentName(const _documentName: string);
		function getFilePath: string;
		procedure setFilePath(const _filePath: string); virtual;
		function getWebPath: string;
		procedure Setindent(const _value: string);
		procedure setWebPath(const _webPath: string); virtual;
		procedure setisCached(const _isCached: boolean);

    public
        property indent: string read myindent write Setindent;
        property filePath: string read getFilePath write setFilePath;
        property webPath: string read getWebPath write setWebPath;
        property documentName: string read mydocumentName write setDocumentName;
        property isCached: boolean read myisCached write setisCached;

        constructor Create;
        destructor Destroy; override;

        function save(_filename: string): boolean;

        function copyFrom(_source: THtmlStyleSelector): THtmlStyleSelector; virtual;
        function clone: THtmlStyleSelector; virtual;
    public
     { SELECTOR Definition

      Adjacent sibling combinator A + B
          Specifies that the elements selected by both A and B have
          the same parent and that the element selected by B immediately
          follows the element selected by A horizontally.

      General sibling combinator A ~ B
          Specifies that the elements selected by both A and B share
          the same parent and that the element selected by A comes before
          —but not necessarily immediately before—the element selected by B.

      Child combinator A > B
          Specifies that the element selected by B is the direct child of
          the element selected by A.

      Descendant combinator A B
          Specifies that the element selected by B is a descendant of
          the element selected by A, but is not necessarily a direct child.

      Column combinator A || B
          Specifies that the element selected by B is located within the
          table column specified by A. Elements which span multiple columns
          are considered to be a member of all of those columns.
      }

        function select(const _selector: string): THtmlStyleSelector;

        function adjacent(const _selector: string): THtmlStyle;
        function sibling(const _selector: string): THtmlStyle;
        function child(const _selector: string): THtmlStyle;
        function decendent(const _selector: string): THtmlStyle;
        function column(const _selector: string): THtmlStyle;

        function style: THtmlStyle;
        function styleList: THtmlStyleList;
        function import(_css: THtmlStyleSelector; _overwriteExisting: boolean = False): THtmlStyleSelector;

        // CSS psuedo classes
        function active: THtmlStyle;
        function Checked: THtmlStyle;
        function default_: THtmlStyle;
        function defined: THtmlStyle;
        function disabled: THtmlStyle;
        function empty: THtmlStyle;
        function Enabled: THtmlStyle;
        function First: THtmlStyle;
        function first_child: THtmlStyle;
        function first_of_type: THtmlStyle;
        function focus: THtmlStyle;
        function focus_visible: THtmlStyle;
        function focus_within: THtmlStyle;
        function fullscreen: THtmlStyle;
        function hover: THtmlStyle;
        function in_range: THtmlStyle;
        function indeterminate: THtmlStyle;
        function invalid: THtmlStyle;
        function lang(_lang: string): THtmlStyle;
        function last_child: THtmlStyle;
        function last_of_type: THtmlStyle;
        function left: THtmlStyle;
        function link: THtmlStyle;
        function not_(_selector: string): THtmlStyle;
        function nth_child(_n: integer): THtmlStyle;
        function nth_col(_n: integer): THtmlStyle;
        function nth_last_child(_n: integer): THtmlStyle;
        function nth_last_of_type(_n: integer): THtmlStyle;
        function nth_of_type(_n: integer): THtmlStyle;
        function only_child: THtmlStyle;
        function only_of_type: THtmlStyle;
        function optional: THtmlStyle;
        function out_of_range: THtmlStyle;
        function placeholder_shown: THtmlStyle;
        function read_only: THtmlStyle;
        function read_write: THtmlStyle;
        function required: THtmlStyle;
        function right: THtmlStyle;
        function root: THtmlStyle;
        function scope: THtmlStyle;
        function target: THtmlStyle;
        function valid: THtmlStyle;
        function visited: THtmlStyle;

        // psuedo elements
        function after: THtmlStyle;
        function before: THtmlStyle;
        function cue: THtmlStyle;
        function cue_region: THtmlStyle;
        function first_letter: THtmlStyle;
        function first_line: THtmlStyle;
        function grammar_error: THtmlStyle;
        function marker: THtmlStyle;
        function part(): THtmlStyle;
        function placeholder: THtmlStyle;
        function selection: THtmlStyle;
        function slotted(_selector: string): THtmlStyle;
        function spelling_error: THtmlStyle;

        {renders the CSS within style tags to embed in the head section}
        function cssInternal: string;

        {renders all the style elements}
        function styledef: string;
        function styleCount: integer;
        function ruleCount: integer;

        {@ rules}
        function fontFace(_name: string): TCSSFontFace;

    end;


    { THtmlStyleSheet }
    THtmlStyleSheet = class(THtmlStyleSelector)
    private
        myFiler: TTextFiler;
    	procedure setfilePath(const _filePath: string); override;
		procedure setwebPath(const _webPath: string); override;
    public
        constructor Create;
        destructor Destroy; override;
        function uri: string; {get the url to include into a page}
    end;

    THtmlStyleSheetLink = class;

    { RgbColor }

    RgbColor = object
        red: word;
        green: word;
        blue: word;
        alpha: word;
        procedure let(_red, _green, _blue: word; _alpha: word = 100);
        procedure let(_rbg: string);
        function rgba: string;
    end;

    { HslColor }

    HslColor = object
        hue: real;
        sat: real; {saturation}
        lum: real; {luminescence}
        alpha: word;
        procedure let(_hue: real; _sat: real; _lum: real; _alpha: word); overload;
        procedure let(_hsla: string); overload;
        function hsla: string;
    end;

    CSSBoxes = (margin_box, border_box, padding_box, content_box,
        fill_box, stroke_box, view_box);

    { CSSBoxesHelper }

    CSSBoxesHelper = type helper for CSSBoxes
        function AsString: string;
        function fromString(_val: string): CSSBoxes;
    end;

	{ TCSSStyleBase }

    TCSSStyleBase = class (TStringList)
    protected
        myselector: string;
        function setStyle(_prop: string; _value: string): integer;
        function getStyle(_prop: string): string;
        function extractStyleText(_css_snippet: string): string; virtual;
        function formatCSSInternal(_styleText: string): string; virtual;
    public
        indent : string;
        constructor Create;
        destructor Destroy; override;
        function getStyleText: string; virtual;
	end;

    { THtmlStyle }


    THtmlStyle = class(TCSSStyleBase)
    private
        myfileLoader: TTextFiler;
		myfilePath: string;
		mywebPath: string;
		mydocumentName: string;
		myisCached: boolean;
        //function setStyle(_prop: string; _value: string): integer;
        //function getStyle(_prop: string): string;
        //function formatCSSInternal(_styleText: string): string;
		procedure setfilePath(const _filePath: string);
		procedure setwebPath(const _webPath: string);
		function getFileLoader: TTextFiler;
		procedure setdocumentName(const _documentName: string);
		procedure setisCached(const _isCached: boolean);

    public
        constructor Create;
        destructor Destroy; override;
        function copyFrom(_source: THtmlStyle): THtmlStyle; virtual;
        function clone: THtmlStyle; virtual;
        function getStyleText: string; override;
        function extractStyleText(_css_snippet: string): string; override;

    public
        property filePath: string read myfilePath write setfilePath;
        property documentName: string read mydocumentName write setdocumentName;
        property webPath: string read mywebPath write setwebPath;
        property fileLoader: TTextFiler read getFileLoader;
        property isCached: boolean read myisCached write setisCached;

    public
        function undefined: boolean; {Returns true if the style has not yet been defined}
        function selector(_selector: string): THtmlStyle; overload;
        function selector: string; overload;

        {Apply this CSS text for this selector}
        function use(_source: THtmlStyle): THtmlStyle;

        function cssInline: string;
        function cssInternal: string;

        {POSITIONS }
        function left(_left: string): THtmlStyle; overload;
        function left: string; overload;
        function top(_top: string): THtmlStyle; overload;
        function top: string; overload;
        function right(_right: string): THtmlStyle; overload;
        function right: string; overload;
        function bottom(_bottom: string): THtmlStyle; overload;
        function bottom: string; overload;

        {ANIMATION - setters}
        function animation(_animation: string): THtmlStyle; overload;
        function animation_delay(_animation_delay: string): THtmlStyle; overload;
        function animation_direction(_animation_direction: string): THtmlStyle; overload;
        function animation_duration(_animation_duration: string): THtmlStyle; overload;
        function animation_fill_mode(_animation_fill_mode: string): THtmlStyle; overload;
        function animation_iteration_count(_animation_iteration_count: string):
            THtmlStyle; overload;
        function animation_name(_animation_name: string): THtmlStyle; overload;
        function animation_play_state(_animation_play_state: string): THtmlStyle;
            overload;
        function animation_timing_function(_animation_timing_function: string):
            THtmlStyle; overload;
        {ANIMATION - getters}
        function animation: string; overload;
        function animation_delay: string; overload;
        function animation_direction: string; overload;
        function animation_duration: string; overload;
        function animation_fill_mode: string; overload;
        function animation_iteration_count: string; overload;
        function animation_name: string; overload;
        function animation_play_state: string; overload;
        function animation_timing_function: string; overload;
        {BORDER - setters}
        function border(_border: string): THtmlStyle; overload;
        function border_block(_border_block: string): THtmlStyle; overload;
        function border_block_color(_border_block_color: string): THtmlStyle; overload;
        function border_block_end(_border_block_end: string): THtmlStyle; overload;
        function border_block_end_color(_border_block_end_color: string): THtmlStyle;
            overload;
        function border_block_end_style(_border_block_end_style: string): THtmlStyle;
            overload;
        function border_block_end_width(_border_block_end_width: string): THtmlStyle;
            overload;
        function border_block_start(_border_block_start: string): THtmlStyle; overload;
        function border_block_start_color(_border_block_start_color: string): THtmlStyle;
            overload;
        function border_block_start_style(_border_block_start_style: string): THtmlStyle;
            overload;
        function border_block_start_width(_border_block_start_width: string): THtmlStyle;
            overload;
        function border_block_style(_border_block_style: string): THtmlStyle; overload;
        function border_block_width(_border_block_width: string): THtmlStyle; overload;
        function border_bottom(_border_bottom: string): THtmlStyle; overload;
        function border_bottom_color(_border_bottom_color: string): THtmlStyle; overload;
        function border_bottom_left_radius(_border_bottom_left_radius: string):
            THtmlStyle; overload;
        function border_bottom_right_radius(_border_bottom_right_radius: string):
            THtmlStyle; overload;
        function border_bottom_style(_border_bottom_style: string): THtmlStyle; overload;
        function border_bottom_width(_border_bottom_width: string): THtmlStyle; overload;
        function border_collapse(_border_collapse: string): THtmlStyle; overload;
        function border_color(_border_color: string): THtmlStyle; overload;
        function border_end_end_radius(_border_end_end_radius: string): THtmlStyle;
            overload;
        function border_end_start_radius(_border_end_start_radius: string): THtmlStyle;
            overload;
        function border_image(_border_image: string): THtmlStyle; overload;
        function border_image_outset(_border_image_outset: string): THtmlStyle; overload;
        function border_image_repeat(_border_image_repeat: string): THtmlStyle; overload;
        function border_image_slice(_border_image_slice: string): THtmlStyle; overload;
        function border_image_source(_border_image_source: string): THtmlStyle; overload;
        function border_image_width(_border_image_width: string): THtmlStyle; overload;
        function border_inline(_border_inline: string): THtmlStyle; overload;
        function border_inline_color(_border_inline_color: string): THtmlStyle; overload;
        function border_inline_end(_border_inline_end: string): THtmlStyle; overload;
        function border_inline_end_color(_border_inline_end_color: string): THtmlStyle;
            overload;
        function border_inline_end_style(_border_inline_end_style: string): THtmlStyle;
            overload;
        function border_inline_end_width(_border_inline_end_width: string): THtmlStyle;
            overload;
        function border_inline_start(_border_inline_start: string): THtmlStyle; overload;
        function border_inline_start_color(_border_inline_start_color: string):
            THtmlStyle; overload;
        function border_inline_start_style(_border_inline_start_style: string):
            THtmlStyle; overload;
        function border_inline_start_width(_border_inline_start_width: string):
            THtmlStyle; overload;
        function border_inline_style(_border_inline_style: string): THtmlStyle; overload;
        function border_inline_width(_border_inline_width: string): THtmlStyle; overload;
        function border_left(_border_left: string): THtmlStyle; overload;
        function border_left_color(_border_left_color: string): THtmlStyle; overload;
        function border_left_style(_border_left_style: string): THtmlStyle; overload;
        function border_left_width(_border_left_width: string): THtmlStyle; overload;
        function border_radius(_border_radius: string): THtmlStyle; overload;
        function border_right(_border_right: string): THtmlStyle; overload;
        function border_right_color(_border_right_color: string): THtmlStyle; overload;
        function border_right_style(_border_right_style: string): THtmlStyle; overload;
        function border_right_width(_border_right_width: string): THtmlStyle; overload;
        function border_spacing(_border_spacing: string): THtmlStyle; overload;
        function border_start_end_radius(_border_start_end_radius: string): THtmlStyle;
            overload;
        function border_start_start_radius(_border_start_start_radius: string):
            THtmlStyle; overload;
        function border_style(_border_style: string): THtmlStyle; overload;
        function border_top(_border_top: string): THtmlStyle; overload;
        function border_top_color(_border_top_color: string): THtmlStyle; overload;
        function border_top_left_radius(_border_top_left_radius: string): THtmlStyle;
            overload;
        function border_top_right_radius(_border_top_right_radius: string): THtmlStyle;
            overload;
        function border_top_style(_border_top_style: string): THtmlStyle; overload;
        function border_top_width(_border_top_width: string): THtmlStyle; overload;
        function border_width(_border_width: string): THtmlStyle; overload;
        {BORDER - getters}
        function border: string; overload;
        function border_block: string; overload;
        function border_block_color: string; overload;
        function border_block_end: string; overload;
        function border_block_end_color: string; overload;
        function border_block_end_style: string; overload;
        function border_block_end_width: string; overload;
        function border_block_start: string; overload;
        function border_block_start_color: string; overload;
        function border_block_start_style: string; overload;
        function border_block_start_width: string; overload;
        function border_block_style: string; overload;
        function border_block_width: string; overload;
        function border_bottom: string; overload;
        function border_bottom_color: string; overload;
        function border_bottom_left_radius: string; overload;
        function border_bottom_right_radius: string; overload;
        function border_bottom_style: string; overload;
        function border_bottom_width: string; overload;
        function border_collapse: string; overload;
        function border_color: string; overload;
        function border_end_end_radius: string; overload;
        function border_end_start_radius: string; overload;
        function border_image: string; overload;
        function border_image_outset: string; overload;
        function border_image_repeat: string; overload;
        function border_image_slice: string; overload;
        function border_image_source: string; overload;
        function border_image_width: string; overload;
        function border_inline: string; overload;
        function border_inline_color: string; overload;
        function border_inline_end: string; overload;
        function border_inline_end_color: string; overload;
        function border_inline_end_style: string; overload;
        function border_inline_end_width: string; overload;
        function border_inline_start: string; overload;
        function border_inline_start_color: string; overload;
        function border_inline_start_style: string; overload;
        function border_inline_start_width: string; overload;
        function border_inline_style: string; overload;
        function border_inline_width: string; overload;
        function border_left: string; overload;
        function border_left_color: string; overload;
        function border_left_style: string; overload;
        function border_left_width: string; overload;
        function border_radius: string; overload;
        function border_right: string; overload;
        function border_right_color: string; overload;
        function border_right_style: string; overload;
        function border_right_width: string; overload;
        function border_spacing: string; overload;
        function border_start_end_radius: string; overload;
        function border_start_start_radius: string; overload;
        function border_style: string; overload;
        function border_top: string; overload;
        function border_top_color: string; overload;
        function border_top_left_radius: string; overload;
        function border_top_right_radius: string; overload;
        function border_top_style: string; overload;
        function border_top_width: string; overload;
        function border_width: string; overload;

        {BOX-SHADOW - setter}
        function box_shadow(_box_shadow: string): THtmlStyle; overload;
        function box_shadow(_offsetX: string; _offsetY: string;
            _blurRadius: string; _spreadRadius: string;
            _colour: string): THtmlStyle; overload;
        {BOX-SHADOW - getter}
        function box_shadow: string; overload;


        {BACKGROUND - setters}
        function background(_background: string): THtmlStyle; overload;
        function background_attachment(_background_attachment: string): THtmlStyle;
            overload;
        function background_clip(_background_clip: string): THtmlStyle; overload;
        function background_color(_background_color: string): THtmlStyle; overload;
        function background_image(_background_image: string): THtmlStyle; overload;
        function background_origin(_background_origin: string): THtmlStyle; overload;
        function background_position(_background_position: string): THtmlStyle; overload;
        function background_position_x(_background_position_x: string): THtmlStyle;
            overload;
        function background_position_y(_background_position_y: string): THtmlStyle;
            overload;
        function background_repeat(_background_repeat: string): THtmlStyle; overload;
        function background_size(_background_size: string): THtmlStyle; overload;
        {BACKGROUND - getters}
        function background: string; overload;
        function background_attachment: string; overload;
        function background_clip: string; overload;
        function background_color: string; overload;
        function background_image: string; overload;
        function background_origin: string; overload;
        function background_position: string; overload;
        function background_position_x: string; overload;
        function background_position_y: string; overload;
        function background_repeat: string; overload;
        function background_size: string; overload;

        function cssgrid(_cssgrid: string): THtmlStyle; overload;
        function cssgrid: string; overload;

        function color(_color: string): THtmlStyle; overload;
        function color: string; overload;

        function float(_float: string): THtmlStyle; overload;
        function float: string; overload;

        {FLEXBOX}
        function flexbox(_flexbox: string): THtmlStyle; overload;
        function flex(_flex: string): THtmlStyle; overload;
        function flex_basis(_flex_basis: string): THtmlStyle; overload;
        function flex_direction(_flex_direction: string): THtmlStyle; overload;
        function flex_flow(_flex_flow: string): THtmlStyle; overload;
        function flex_grow(_flex_grow: string): THtmlStyle; overload;
        function flex_shrink(_flex_shrink: string): THtmlStyle; overload;
        function flex_wrap(_flex_wrap: string): THtmlStyle; overload;

        {FLEXBOX}
        function flexbox: string; overload;
        function flex: string; overload;
        function flex_basis: string; overload;
        function flex_direction: string; overload;
        function flex_flow: string; overload;
        function flex_grow: string; overload;
        function flex_shrink: string; overload;
        function flex_wrap: string; overload;

        {FONT - setters}
        function font(_font: string): THtmlStyle; overload;
        //function @font_face(_@font_face: string): THtmlStyle; overload;
        function font_family(_font_family: string): THtmlStyle; overload;
        //function font_family (@font_face)(_font_family (@font_face): string): THtmlStyle; overload;
        function font_feature_settings(_font_feature_settings: string): THtmlStyle;
            overload;
        //function font_feature_settings (@font_face)(_font_feature_settings (@font_face): string): THtmlStyle; overload;
        //function @font_feature_values(_@font_feature_values: string): THtmlStyle; overload;
        function font_kerning(_font_kerning: string): THtmlStyle; overload;
        function font_language_override(_font_language_override: string): THtmlStyle;
            overload;
        function font_optical_sizing(_font_optical_sizing: string): THtmlStyle; overload;
        function font_size(_font_size: string): THtmlStyle; overload;
        function font_size_adjust(_font_size_adjust: string): THtmlStyle; overload;
        function font_stretch(_font_stretch: string): THtmlStyle; overload;
        //function font_stretch (@font_face)(_font_stretch (@font_face): string): THtmlStyle; overload;
        function font_style(_font_style: string): THtmlStyle; overload;
        //function font_style (@font_face)(_font_style (@font_face): string): THtmlStyle; overload;
        function font_synthesis(_font_synthesis: string): THtmlStyle; overload;
        function font_variant(_font_variant: string): THtmlStyle; overload;
        //function font_variant (@font_face)(_font_variant (@font_face): string): THtmlStyle; overload;
        function font_variant_alternates(_font_variant_alternates: string): THtmlStyle;
            overload;
        function font_variant_caps(_font_variant_caps: string): THtmlStyle; overload;
        function font_variant_east_asian(_font_variant_east_asian: string): THtmlStyle;
            overload;
        function font_variant_ligatures(_font_variant_ligatures: string): THtmlStyle;
            overload;
        function font_variant_numeric(_font_variant_numeric: string): THtmlStyle;
            overload;
        function font_variant_position(_font_variant_position: string): THtmlStyle;
            overload;
        //function font_variation_settings (@font_face)(_font_variation_settings (@font_face): string): THtmlStyle; overload;
        function font_weight(_font_weight: string): THtmlStyle; overload;
        //function font_weight (@font_face)(_font_weight (@font_face): string): THtmlStyle; overload;

        {FONT - getters}
        function font: string; overload;
        //function @font_face: string;  overload;
        function font_family: string; overload;
        //function font_family (@font_face): string;  overload;
        function font_feature_settings: string; overload;
        //function font_feature_settings (@font_face): string;  overload;
        //function @font_feature_values: string;  overload;
        function font_kerning: string; overload;
        function font_language_override: string; overload;
        function font_optical_sizing: string; overload;
        function font_size: string; overload;
        function font_size_adjust: string; overload;
        function font_stretch: string; overload;
        //function font_stretch (@font_face): string;  overload;
        function font_style: string; overload;
        //function font_style (@font_face): string;  overload;
        function font_synthesis: string; overload;
        function font_variant: string; overload;
        //function font_variant (@font_face): string;  overload;
        function font_variant_alternates: string; overload;
        function font_variant_caps: string; overload;
        function font_variant_east_asian: string; overload;
        function font_variant_ligatures: string; overload;
        function font_variant_numeric: string; overload;
        function font_variant_position: string; overload;
        //function font_variation_settings (@font_face): string;  overload;
        function font_weight: string; overload;
        //function font_weight (@font_face): string;  overload;

        {FILTER - setters}
        function filter(_filter: string): THtmlStyle; overload;
        function filter_blur(_filter_blur: string): THtmlStyle; overload;
        function filter_brightness(_filter_brightness: string): THtmlStyle; overload;
        function filter_contrast(_filter_contrast: string): THtmlStyle; overload;
        function filter_dropshadow(_filter_dropshadow: string): THtmlStyle; overload;
        function filter_grayscale(_filter_grayscale: string): THtmlStyle; overload;
        function filter_hue_rotate(_filter_hue_rotate: string): THtmlStyle; overload;
        function filter_invert(_filter_invert: string): THtmlStyle; overload;
        function filter_opacity(_filter_opacity: string): THtmlStyle; overload;
        function filter_saturate(_filter_saturate: string): THtmlStyle; overload;
        function filter_sepia(_filter_sepia: string): THtmlStyle; overload;
        {FILTER - getters}
        function filter: string; overload;
        function filter_blur: string; overload;
        function filter_brightness: string; overload;
        function filter_contrast: string; overload;
        function filter_dropshadow: string; overload;
        function filter_grayscale: string; overload;
        function filter_hue_rotate: string; overload;
        function filter_invert: string; overload;
        function filter_opacity: string; overload;
        function filter_saturate: string; overload;
        function filter_sepia: string; overload;

        {PADDING - setters}
        function padding(_padding: string): THtmlStyle; overload;
        function padding_block(_padding_block: string): THtmlStyle; overload;
        function padding_block_end(_padding_block_end: string): THtmlStyle; overload;
        function padding_block_start(_padding_block_start: string): THtmlStyle; overload;
        function padding_bottom(_padding_bottom: string): THtmlStyle; overload;
        function padding_inline(_padding_inline: string): THtmlStyle; overload;
        function padding_inline_end(_padding_inline_end: string): THtmlStyle; overload;
        function padding_inline_start(_padding_inline_start: string): THtmlStyle;
            overload;
        function padding_left(_padding_left: string): THtmlStyle; overload;
        function padding_right(_padding_right: string): THtmlStyle; overload;
        function padding_top(_padding_top: string): THtmlStyle; overload;

        {PADDING - getters}
        function padding: string; overload;
        function padding_block: string; overload;
        function padding_block_end: string; overload;
        function padding_block_start: string; overload;
        function padding_bottom: string; overload;
        function padding_inline: string; overload;
        function padding_inline_end: string; overload;
        function padding_inline_start: string; overload;
        function padding_left: string; overload;
        function padding_right: string; overload;
        function padding_top: string; overload;

        {MARGIN - setters}
        function margin(_margin: string): THtmlStyle; overload;
        function margin_block(_margin_block: string): THtmlStyle; overload;
        function margin_block_end(_margin_block_end: string): THtmlStyle; overload;
        function margin_block_start(_margin_block_start: string): THtmlStyle; overload;
        function margin_bottom(_margin_bottom: string): THtmlStyle; overload;
        function margin_inline(_margin_inline: string): THtmlStyle; overload;
        function margin_inline_end(_margin_inline_end: string): THtmlStyle; overload;
        function margin_inline_start(_margin_inline_start: string): THtmlStyle; overload;
        function margin_left(_margin_left: string): THtmlStyle; overload;
        function margin_right(_margin_right: string): THtmlStyle; overload;
        function margin_top(_margin_top: string): THtmlStyle; overload;

        {MARGIN - getters}
        function margin: string; overload;
        function margin_block: string; overload;
        function margin_block_end: string; overload;
        function margin_block_start: string; overload;
        function margin_bottom: string; overload;
        function margin_inline: string; overload;
        function margin_inline_end: string; overload;
        function margin_inline_start: string; overload;
        function margin_left: string; overload;
        function margin_right: string; overload;
        function margin_top: string; overload;

        {OUTLINE - setters}
        function outline(_outline: string): THtmlStyle; overload;
        function outline_color(_outline_color: string): THtmlStyle; overload;
        function outline_offset(_outline_offset: string): THtmlStyle; overload;
        function outline_style(_outline_style: string): THtmlStyle; overload;
        function outline_width(_outline_width: string): THtmlStyle; overload;
        {OUTLINE - getters}
        function outline: string; overload;
        function outline_color: string; overload;
        function outline_offset: string; overload;
        function outline_style: string; overload;
        function outline_width: string; overload;

        {OVERFLOW - setters}
        function overflow(_overflow: string): THtmlStyle; overload;
        function overflow_wrap(_overflow_wrap: string): THtmlStyle; overload;
        function overflow_x(_overflow_x: string): THtmlStyle; overload;
        function overflow_y(_overflow_y: string): THtmlStyle; overload;
        {OVERFLOW - getters}
        function overflow: string; overload;
        function overflow_wrap: string; overload;
        function overflow_x: string; overload;
        function overflow_y: string; overload;

        {SCROLLBARS - setter}
        function scroll_behavior(_scroll_behavior: string): THtmlStyle; overload;
        function scroll_margin(_scroll_margin: string): THtmlStyle; overload;
        function scroll_margin_block(_scroll_margin_block: string): THtmlStyle; overload;
        function scroll_margin_block_end(_scroll_margin_block_end: string): THtmlStyle;
            overload;
        function scroll_margin_block_start(_scroll_margin_block_start: string):
            THtmlStyle;
            overload;
        function scroll_margin_bottom(_scroll_margin_bottom: string): THtmlStyle;
            overload;
        function scroll_margin_inline(_scroll_margin_inline: string): THtmlStyle;
            overload;
        function scroll_margin_inline_end(_scroll_margin_inline_end: string): THtmlStyle;
            overload;
        function scroll_margin_inline_start(_scroll_margin_inline_start: string):
            THtmlStyle; overload;
        function scroll_margin_left(_scroll_margin_left: string): THtmlStyle; overload;
        function scroll_margin_right(_scroll_margin_right: string): THtmlStyle; overload;
        function scroll_margin_top(_scroll_margin_top: string): THtmlStyle; overload;
        function scroll_padding(_scroll_padding: string): THtmlStyle; overload;
        function scroll_padding_block(_scroll_padding_block: string): THtmlStyle;
            overload;
        function scroll_padding_block_end(_scroll_padding_block_end: string): THtmlStyle;
            overload;
        function scroll_padding_block_start(_scroll_padding_block_start: string):
            THtmlStyle; overload;
        function scroll_padding_bottom(_scroll_padding_bottom: string): THtmlStyle;
            overload;
        function scroll_padding_inline(_scroll_padding_inline: string): THtmlStyle;
            overload;
        function scroll_padding_inline_end(_scroll_padding_inline_end: string):
            THtmlStyle;
            overload;
        function scroll_padding_inline_start(_scroll_padding_inline_start: string):
            THtmlStyle; overload;
        function scroll_padding_left(_scroll_padding_left: string): THtmlStyle; overload;
        function scroll_padding_right(_scroll_padding_right: string): THtmlStyle;
            overload;
        function scroll_padding_top(_scroll_padding_top: string): THtmlStyle; overload;
        function scroll_snap_align(_scroll_snap_align: string): THtmlStyle; overload;
        function scroll_snap_stop(_scroll_snap_stop: string): THtmlStyle; overload;
        function scroll_snap_type(_scroll_snap_type: string): THtmlStyle; overload;
        function scrollbar_color(_scrollbar_color: string): THtmlStyle; overload;
        function scrollbar_width(_scrollbar_width: string): THtmlStyle; overload;

        {SCROLLBARS - getter}
        function scroll_behavior: string; overload;
        function scroll_margin: string; overload;
        function scroll_margin_block: string; overload;
        function scroll_margin_block_end: string; overload;
        function scroll_margin_block_start: string; overload;
        function scroll_margin_bottom: string; overload;
        function scroll_margin_inline: string; overload;
        function scroll_margin_inline_end: string; overload;
        function scroll_margin_inline_start: string; overload;
        function scroll_margin_left: string; overload;
        function scroll_margin_right: string; overload;
        function scroll_margin_top: string; overload;
        function scroll_padding: string; overload;
        function scroll_padding_block: string; overload;
        function scroll_padding_block_end: string; overload;
        function scroll_padding_block_start: string; overload;
        function scroll_padding_bottom: string; overload;
        function scroll_padding_inline: string; overload;
        function scroll_padding_inline_end: string; overload;
        function scroll_padding_inline_start: string; overload;
        function scroll_padding_left: string; overload;
        function scroll_padding_right: string; overload;
        function scroll_padding_top: string; overload;
        function scroll_snap_align: string; overload;
        function scroll_snap_stop: string; overload;
        function scroll_snap_type: string; overload;
        function scrollbar_color: string; overload;
        function scrollbar_width: string; overload;

        {TEXT - setters}
        function text_align(_text_align: string): THtmlStyle; overload;
        function text_align_last(_text_align_last: string): THtmlStyle; overload;
        function text_combine_upright(_text_combine_upright: string): THtmlStyle;
            overload;
        function text_decoration(_text_decoration: string): THtmlStyle; overload;
        function text_decoration_color(_text_decoration_color: string): THtmlStyle;
            overload;
        function text_decoration_line(_text_decoration_line: string): THtmlStyle;
            overload;
        function text_decoration_style(_text_decoration_style: string): THtmlStyle;
            overload;
        function text_decoration_thickness(_text_decoration_thickness: string):
            THtmlStyle;
            overload;
        function text_emphasis(_text_emphasis: string): THtmlStyle; overload;
        function text_emphasis_color(_text_emphasis_color: string): THtmlStyle; overload;
        function text_emphasis_position(_text_emphasis_position: string): THtmlStyle;
            overload;
        function text_emphasis_style(_text_emphasis_style: string): THtmlStyle; overload;
        function text_indent(_text_indent: string): THtmlStyle; overload;
        function text_justify(_text_justify: string): THtmlStyle; overload;
        function text_orientation(_text_orientation: string): THtmlStyle; overload;
        function text_overflow(_text_overflow: string): THtmlStyle; overload;
        function text_rendering(_text_rendering: string): THtmlStyle; overload;
        function text_shadow(_text_shadow: string): THtmlStyle; overload;
        function text_transform(_text_transform: string): THtmlStyle; overload;
        function text_underline_offset(_text_underline_offset: string): THtmlStyle;
            overload;
        function text_underline_position(_text_underline_position: string): THtmlStyle;
            overload;

        {TEXT - getters}
        function text_align: string; overload;
        function text_align_last: string; overload;
        function text_combine_upright: string; overload;
        function text_decoration: string; overload;
        function text_decoration_color: string; overload;
        function text_decoration_line: string; overload;
        function text_decoration_style: string; overload;
        function text_decoration_thickness: string; overload;
        function text_emphasis: string; overload;
        function text_emphasis_color: string; overload;
        function text_emphasis_position: string; overload;
        function text_emphasis_style: string; overload;
        function text_indent: string; overload;
        function text_justify: string; overload;
        function text_orientation: string; overload;
        function text_overflow: string; overload;
        function text_rendering: string; overload;
        function text_shadow: string; overload;
        function text_transform: string; overload;
        function text_underline_offset: string; overload;
        function text_underline_position: string; overload;

        {TRANSITION - getter}
        function transition(_transition: string): THtmlStyle; overload;
        function transition_delay(_transition_delay: string): THtmlStyle; overload;
        function transition_duration(_transition_duration: string): THtmlStyle; overload;
        function transition_property(_transition_property: string): THtmlStyle; overload;
        function transition_timing_function(_transition_timing_function: string):
            THtmlStyle; overload;

        {TRANSITION - setter}
        function transition: string; overload;
        function transition_delay: string; overload;
        function transition_duration: string; overload;
        function transition_property: string; overload;
        function transition_timing_function: string; overload;

        {setters}
        function display(_display: string): THtmlStyle; overload;
        function align(_align: string): THtmlStyle; overload;
        function position(_position: string): THtmlStyle; overload;

        function Width(_width: string): THtmlStyle; overload;
        function Height(_height: string): THtmlStyle; overload;
        function min_width(_min_width: string): THtmlStyle; overload;
        function min_height(_min_height: string): THtmlStyle; overload;
        function max_width(_max_width: string): THtmlStyle; overload;
        function max_height(_max_height: string): THtmlStyle; overload;



        {getters}
        function display: string; overload;
        function align: string; overload;
        function position: string; overload;

        function Width: string; overload;
        function Height: string; overload;
        function min_width: string; overload;
        function min_height: string; overload;
        function max_width: string; overload;
        function max_height: string; overload;
    end;


    { THTMLElementBase }

    THTMLElementBase = class
    private
        class var
        myElementCount: QWord;

    protected
        FText: string;
        procedure setFText(AValue: string);
    protected
        level: QWord;
        tag: string;
        hasEndTag: boolean; // says whether the tag has an end tag or not
        function indent: string;
        function indent(const _level: QWORD): string;

        function tag_start: string; virtual;
        function tag_end: string; virtual;
        function renderFormatString: string; virtual;
        function renderFormatStringEmpty: string; virtual;
        function formatAttributes: string; virtual;
        function render(_level: QWord): string; virtual; // Returns the html

    public
        property Text: string read FText write setFText; // Gets the innerHTML
        property elementTag: string read tag;

        function getElementCount: integer;
        function append(_text: string): THtmlElementBase;
        {output functions}
        function html: string; virtual;   // returns HTML text
        function setText(_text: string): THTMLElementBase;

        // inserts whatever you pass as _code. No tag. No attributes.
        // This allows you to insert blocks of HTML in this node.
        function pre(_htmlCode: string): THTMLElementBase;
        function isPre: boolean;
        function copyFrom(_source: THtmlElementBase): THTMLElementBase; virtual;

        constructor Create; virtual;
        function noEndTag: THTMLElementBase;

    end;

    { THtmlElement }
    THtmlElement = class(THTMLElementBase)
    private
        {not used yet}
        myEvents: TStringMap;

        myStyle: THtmlStyle;
        FAttributes: TStringList;
        FParent: THtmlElement;
        function getCharSet: string;
        function getCSSClass: string;
        function getID: string;
        procedure setFCharSet(AValue: string);
        procedure setFCSSClass(AValue: string);
        procedure setFName(const __Name: string);
        function getName: string;
        procedure setFID(const __ID: string);
		function getTitleAttr: string;
		procedure setTitleAttr(const _title: string);
		function getAnchor: string;

    protected
        suppressID: boolean;
        myIndex: integer; {reference to index in the containing collection}
        function setStyle(_s: string): THtmlElement;
        function tag_start: string; override;
        function FormatAttributes: string; override;
        function render(_level: QWord): string; override;
        procedure renderStyle; virtual;
    public
        property tagName: string read getName write setFName;
        property tagID: string read getID write setFID;
        property anchor: string read getAnchor; {< Returns a on-page url to this element}
        property charset: string read getCharSet write setFCharSet;
        property attributes: TStringList read FAttributes write FAttributes;
        property parent: THtmlElement read FParent;
        property styleClass: string read getCSSClass write setFCSSClass;
        property title: string read getTitleAttr write setTitleAttr;

        function setParent(__parent: THtmlElement): THtmlElement;

        {assigns the text as is to the attributes.
        You should build it before assigning}
        function setAttr(_attributes: string = ''): THtmlElement; overload;
        function setAttr(_attributes: array of string): THtmlElement; overload;

        {key, value pair for attribute}
        function setAttr(_key: string; _value: string): THtmlElement; overload;
        function setAttr(_key: string; _value: integer): THtmlElement; overload;

        {remove an attribute}
        function rmAttr(_key: string): boolean;

        {sets a single text attribute}
        function setAttrFlag(_key: string): THtmlElement; overload;
        function setAttrFlag(_key: string; _set: boolean): THtmlElement; overload;

        {removes a flag}
        function rmAttrFlag(_key: string): THtmlElement;

        {find a flag}
        function findAttrFlag(_key: string): boolean;
        function attrIsNum(_value: string): boolean;
        function attrAsNum(_value: string): string;
        function getAttr(_name: string): string;

        {generates attributes as JSON}
        function getAttrJSON: string;
        function getAttrMithril: string;

        function style: THtmlStyle;

        {defines the class}
        function setClass(_c: string): THtmlElement; virtual; overload;
        function setClass(_c: array of string): THtmlElement; virtual; overload;

        {appends to the class}
        function hasClass(_c: string): boolean;
        function addClass(_c: string): THtmlElement;virtual;
        function removeClass(_c: string): THtmlElement;virtual;
        function toggleClass(_c: string): THtmlElement;virtual;

        function setId(_id: string): THtmlElement; virtual;
        function setField(_id: string): THtmlElement; virtual;
        function setName(_name: string): THtmlElement; virtual;
        function setTitle(_title: string): THtmlElement; virtual;


        function copyFrom(_source: THtmlElement): THtmlElement; reintroduce;
        function clone: THtmlElement; virtual;

        {event mapping}
        function events: TStringMap;
        function event(_name: string; _code: string): THtmlElement; overload;
        function event(_name: string): string; overload; //returns the event code

    public
        {data mapping}
        data: string;
        constructor Create; override; overload;
        {to create html components using custom tags}
        constructor Create(_tag: string); virtual; overload;
        {custom tags and attributes}
        constructor Create(_tag: string; _attributes: array of string); virtual; overload;
        destructor Destroy; override;

    end;
    THtmlElementClass = class of THtmlElement;

    THtmlElementArray = array of THtmlElement;

    THtmlCollection = class;
    THeading = class;
    THeading1 = class;
    THeading2 = class;
    THeading3 = class;
    THeading4 = class;
    THeading5 = class;
    THeading6 = class;
    THeading7 = class;
    TParagraph = class;
    TStrong = class;
    TEmphasis = class;
    THtmlDiv = class;
    THtmlSpan = class;
    THtmlSection = class;
    THtmlTable = class;
    THtmlUnorderedList = class;
    THtmlOrderedList = class;
    THtmlScript = class;
    THTMLMetaTag = class;
    THtmlViewPort = class;
    THtmlForm = class;
    THtmlInput = class;
    THtmlLabel = class;
    THtmlLink = class;
    THtmlAnchor = class;
    THtmlNavbar = class;
    THtmlMenu = class;
    THtmlAlert = class;
    THtmlButton = class;
    THtmlEditBox = class;
    THtmlFieldSet = class;
    THtmlLegend = class;
    THtmlCheckBox = class;
    THtmlRadioButton = class;
    THtmlTextArea = class;
    THtmlSelect = class;
    THtmlTagless = class;
    THtmlInterestingText = class;
    THtmlDialog = class;
    THtmlDetails = class;
    THtmlSummary = class;
    THtmlTemplate = class;
    THtmlFragment = class;
    THtmlImg = class;
    THtmlImageBase64 = class;


    { THtmlCollection }
    {TODO: Rewrite this class to use GenericHashObjectList}
    THtmlCollection = class(THtmlElement)
    protected
        myElementList: TFPHashObjectList;
        function getItems(index: integer): THtmlElement;
        function render(_level: QWord): string; override;
    public
        autoFree: boolean;
        property Items[index: integer]: THtmlElement read getItems;

        function Count: integer;

        constructor Create; override;
        destructor Destroy; override;

        function copyFrom(_source: THtmlCollection): THtmlCollection; reintroduce;
        function clone: THtmlCollection; reintroduce;

        // Add new HTML elements to the collection
        function add(_htmlElement: THtmlElement): THtmlElement; overload;
        function add(_htmlCollection: THtmlCollection): THtmlCollection; overload;
        // Add multiple elements
        procedure add(_htmlElements: array of THtmlElement); overload;

        // Remove an element
        function remove(_id: string): boolean;
        // returns true if successful. false if no id was found. raises exception on error

        // locate an element in the collection.
        function exists(_id: string): boolean;    // shallow search in the list of elements
        function find(_id: string): THtmlElement; // shallow search in the list of elements.
        function get(_id: string): THtmlElement;  // Searches the tree for the element.
        function getCollection(_id: string): THtmlCollection;

        // Get a list of IDs in the collection
        function getIDs(_filter: string = ''): TStrings;

        {reindex is called when you change the id of an element. Return true if successful}
        function reindex(_index: integer): boolean;

        {Short cut to insert HTML elements into the collection}
        function script(t: string = ''): THtmlScript;
        function scriptCDN(_source: string): THtmlScript;
        function link: THtmlLink;

        function h1(t: string = ''): THeading1;
        function h2(t: string = ''): THeading2;
        function h3(t: string = ''): THeading3;
        function h4(t: string = ''): THeading4;
        function h5(t: string = ''): THeading5;
        function h6(t: string = ''): THeading6;
        function h7(t: string = ''): THeading7;
        function p(t: string = ''): TParagraph; {Adds new paragraphs}
        function strong(_text: string): TStrong;
        function emphasis(_text: string): TEmphasis;

        { TODO : img tag }
        function img(src: string): THtmlImg;
        function imgBase64(_file: string): THtmlImageBase64;
        { TODO : video tag, audio tag }
        { TODO : file uploads }

        {Collections}
        function template: THtmlTemplate;
        function fragment: THtmlFragment;
        function div_(t: string = ''): THtmlDiv;
        function span_(t: string = ''): THtmlSpan;
        function section_(t: string = ''): THtmlSection;
        function table_: THtmlTable;
        function ul_: THtmlUnorderedList;
        function ol_: THtmlOrderedList;
        function form_: THtmlForm;
        function input_(_type: THtmlInputType = THtmlInputType.inputText;
            _id: string = ''; _name: string = '';
            _value: string = ''): THtmlInput;

        function fieldset: THtmlFieldSet;
        function legend: THtmlLegend;
        function editBox: THtmlEditBox;
        function button: THtmlButton;
        function checkbox: THtmlCheckBox;
        function radiobutton: THtmlRadioButton;
        function textarea: THtmlTextArea;
        function selection: THtmlSelect;
        function navbar: THtmlNavbar;
        function menu: THtmlMenu;

        {REFACTOR THESE OUT}
        function contextmenu: THtmlMenu;
        function alert: THtmlAlert;
        function login: THtmlForm;
        function confirm: THtmlForm;
        {/REFACTOR}

        function a(_text: string; _url: string): THtmlAnchor;

        function require(_file: string): THtmlTagless;
        function tagLess: THtmlTagless;
        function interesting(_text: string): THtmlInterestingText;

        function dialog: THtmlDialog;
        function details: THtmlDetails;

        procedure Clear; virtual;

        procedure reIndex(_el: THtmlElement);

        function setClass(_c: array of string): THtmlCollection; overload; reintroduce;
        function setClass(_c: string): THtmlCollection; overload; reintroduce;
        function addClass(_c: string): THtmlCollection; reintroduce;
        function removeClass(_c: string): THtmlCollection; reintroduce;
        function toggleClass(_c: string): THtmlCollection; reintroduce;

    end;
    THtmlCollectionArray = array of THtmlCollection;

        { THeading1 }
           THeading = class(THtmlCollection);

           THeading1 = class(THeading)
               constructor Create; override;
           end;

           { THeading2 }
           THeading2 = class(THeading)
               constructor Create; override;
           end;

           { THeading3 }
           THeading3 = class(THeading)
               constructor Create; override;
           end;

           { THeading4 }
           THeading4 = class(THeading)
               constructor Create; override;
           end;

           { THeading5 }

           THeading5 = class(THeading)
               constructor Create; override;
           end;

           { THeading6 }

           THeading6 = class(THeading)
               constructor Create; override;
           end;

           { THeading7 }

           THeading7 = class(THeading)
               constructor Create; override;
           end;

           { TParagraph }
           TParagraph = class(THtmlElement)
               constructor Create; override;
           end;

		   { TStrong }

           TStrong = class(THtmlElement)
                constructor Create; override;
		   end;

		   { TEmphasis }

           TEmphasis = class(THtmlElement)
                constructor Create; override;
		   end;

    { THtmlDiv }
    THtmlDiv = class(THtmlCollection)
    public
        constructor Create; override;
        destructor Destroy; override;
    end;

    THtmlDivArray = array of THtmlDiv;


    { THtmlDiv }

    { THtmlSpan }

    THtmlSpan = class(THtmlCollection)
    public
        constructor Create; override;
        destructor Destroy; override;
    end;

    { THtmlSection }

    THtmlSection = class(THtmlCollection)
    public
        constructor Create; override;
    end;

    { THtmlTableColumn }
    THtmlTableColumn = class(THtmlCollection)
    public
        constructor Create; override;
        function rowSpan(_n: word): THtmlTableColumn;
        function colSpan(_n: word): THtmlTableColumn;
        function setClass(_c: string): THtmlTableColumn; overload; reintroduce;
        function setClass(_c: array of string): THtmlTableColumn; overload; reintroduce;
        function addClass(_c: string): THtmlTableColumn; reintroduce;
        function removeClass(_c: string): THtmlTableColumn; reintroduce;
        function toggleClass(_c: string): THtmlTableColumn; reintroduce;
    end;

    { THtmlTableHeaderColumn }
    THtmlTableHeaderColumn = class(THtmlTableColumn)
    public
        constructor Create; override;
        {scope: row, col, rowgroup, colgroup, auto}
        function scope(_scope: string): THtmlTableHeaderColumn;
    end;


    THtmlTableRow = class;
    THtmlTableHeaderRow = class;

    { THtmlTableBody }
    THtmlTableBody = class(THtmlCollection)
    protected
        function render(_level: QWord): string; override;
    public
        constructor Create; override;
        function newRow: THtmlTableRow; virtual;
        function rowCount: integer; virtual;
        function row(_index: integer): THtmlTableRow; virtual;
    end;

    THtmlTableHeader = class(THtmlTableBody)
    protected
        function render(_level: QWord): string; override;
    public
        constructor Create; override;
        function newRow: THtmlTableHeaderRow; reintroduce;
        function row(_index: integer): THtmlTableHeaderRow; reintroduce;
    end;

    { THtmlTableFooter }
    THtmlTableFooter = class(THtmlTableBody)
    protected
        function render(_level: QWord): string; override;
    public
        constructor Create; override;
    end;


    { THtmlTableRow }
    THtmlTableRow = class(THtmlCollection)
    public
        constructor Create; override;
        function newColHeader(t: string = ''): THtmlTableHeaderColumn;
        function newCol(t: string = ''): THtmlTableColumn; virtual;
        function colCount: integer;
        function col(_index: integer): THtmlTableColumn; virtual;

        function setClass(_c: string): THtmlTableRow; overload; reintroduce;
        function setClass(_c: array of string): THtmlTableRow; overload; reintroduce;
        function addClass(_c: string): THtmlTableRow; reintroduce;
        function removeClass(_c: string): THtmlTableRow; reintroduce;
        function toggleClass(_c: string): THtmlTableRow; reintroduce;
    end;

    { THtmlTableHeaderRow }
    THtmlTableHeaderRow = class(THtmlTableRow)
        function newCol(t: string = ''): THtmlTableHeaderColumn; override;
        function col(_index: integer): THtmlTableHeaderColumn; override;
    end;

    { THtmlTableHeaderRow }
    THtmlTable = class(THtmlCollection)
    protected
        myHeader: THtmlTableHeader;
        myFooter: THtmlTableFooter;
        myBody: THtmlTableBody;
        function render(_level: QWord): string; override;
    public
        constructor Create; override;
        destructor Destroy; override;
        function newRow: THtmlTableRow;
        function tableheader: THtmlTableHeader;
        function tablefooter: THtmlTableFooter;
        function rowCount: integer;
        function row(_index: integer): THtmlTableRow;

        procedure Clear; override;
    end;


    { THtmlListItem }
    THtmlListItem = class(THtmlCollection)
    public
        constructor Create; override;
    end;

    THtmlList = class(THtmlCollection)
    public
        function item(t: string = ''): THtmlListItem;
    end;

    {bullets}

    { THtmlUnorderedList }

    THtmlUnorderedList = class(THtmlList)
    public
        constructor Create; override;
    end;

    {numbered list}

    { THtmlOrderedList }

    THtmlOrderedList = class(THtmlList)
    public
        constructor Create; override;
    end;

    { THtmlGrid }

    THtmlGrid = class(THtmlCollection)

        constructor Create; override;
    end;

	{ THtmlDialog }

    THtmlDialog = class(THtmlCollection)
        constructor Create; override;
    end;

	{ THtmlDetails }

    THtmlDetails = class(THtmlCollection)
        constructor Create; override;
        function summary: THtmlSummary;
    end;

	{ THtmlSummary }

    THtmlSummary = class(THtmlCollection)
        constructor Create; override;
    end;


    { THtmlHead }

    THtmlHead = class(THtmlCollection)
    public
        constructor Create; override;
        {PAGE HEADER DESCRIPTIONS}
        function base(_baseURL: string): THtmlHead;
        function charset(_charset: string = 'utf-8'): THtmlHead; overload;
        function charsetShort(_charset: string = 'utf-8'): THtmlHead; overload;
        function title(_title: string): THtmlHead;
        function author(_author: string): THtmlHead;
        function description(_descr: string): THtmlHead;
        function viewport: THtmlViewPort;
        {Components}
        function stylesheet(_css_source: string): THtmlStyleSheetLink; // with href
        function style(_text: string): THtmlHead; //embed the setClass text
        function font(_fontsource: string): THtmlHead;
        function meta(_name: string; _content: string = ''): THTMLMetaTag; overload;
        function meta: THTMLMetaTag; overload;
        {OTHERS}
        function pre(_code: string): THtmlHead; reintroduce; // put in blank pre
        function favicon(_favicon_uri: string): THtmlHead;

        function clone: THtmlHead; reintroduce;
    end;

    { THtmlBody }

    THtmlBody = class(THtmlCollection)
    private
        myHeader: THtmlCollection;
        myFooter: THtmlCollection;
        myEnding: THtmlCollection;
    protected
        function render(_level: QWord): string; override;
    public
        constructor Create; override;
        destructor Destroy; override;
        function header: THtmlCollection;
        function footer: THtmlCollection;

        function copyFrom(_source: THtmlBody): THtmlBody; reintroduce;
        function clone: THtmlBody; reintroduce;

        {This place is where you can put scripts.
        This is put at the end of the page after the footer}
        function ending: THtmlCollection;

        {clears the body. leaves header and footer intact}
        procedure Clear; override;

    end;

    { THtmlDoc }

    THtmlDoc = class(THtmlCollection)
    protected
        FHtmlHead: THtmlHead;
        FHtmlBody: THtmlBody;
        myScripts: TJavaScripts;
        myStyles: THtmlStyleSheet;
        myName: string;

        function newHead(t: string = ''): THtmlHead; virtual;
        function newBody(t: string = ''): THtmlBody; virtual;

        procedure setDocTitle(const _Title: string);
        function getDocTitle: string;
        function getLanguage: string;
        procedure setLanguage(const _language: string);
        procedure setScripts(const _Scripts: TJavaScripts);
        procedure setStyles(const _Styles: THtmlStyleSheet);
        procedure setDocName(_name: string);
    protected
        function render(_level: QWord): string; override;
    public
        property Name: string read myName write setDocName;
        property Head: THtmlHead read FHtmlHead;
        property Body: THtmlBody read FHtmlBody;
        property Title: string read getDocTitle write setDocTitle;
        property Language: string read getLanguage write setLanguage;
        property Styles: THtmlStyleSheet read myStyles write setStyles;
        property Scripts: TJavaScripts read myScripts;

        constructor Create; override;
        destructor Destroy; override;

        function copyFrom(_source: THtmlDoc): THtmlDoc; reintroduce;
        function clone: THtmlDoc; reintroduce;

    public const
        PAGE_STYLE_ID  = 'sugar-page-style';
        PAGE_SCRIPT_ID = 'sugar-page-script';

    end;

	{ THTMLStyleElement }

    THTMLStyleElement = class(THtmlElement)
        constructor Create; override;
	end;

    {THTMLScript}
    THTMLScript = class(THtmlElement)
    private
        function getAsync: boolean;
        function getDefer: boolean;
        function getScriptType: string;
        function getSrc: string;

        procedure setFScriptType(AValue: string);
        procedure setFSrc(AValue: string);
        procedure setFAsync(const _async: boolean);
        procedure setFDefer(const _defer: boolean);
    protected
        function Render(_level: QWord): string; override;
    public
        constructor Create; override;
        function setAttr(_key: string; _value: string): THtmlScript; overload;
        property src: string read getSrc write setFSrc;
        property script_type: string read getScriptType write setFScriptType;
        property async: boolean read getAsync write setFAsync;
        property defer: boolean read getDefer write setFDefer;

        function setSrc(_src: string): THtmlScript;
        function setScriptType(_script_type: string): THtmlScript;
        function setAsync: THtmlScript;
        function unsetAsync: THtmlScript;
        function setDefer: THtmlScript;
        function unsetDefer: THtmlScript;
        function LoadFile(_file: string): THtmlScript;
    end;

    { THTMLMetaTag }

    THTMLMetaTag = class(THtmlElement)
    private
        function getContent: string;
        function getHttpEquiv: string;
        procedure setFContent(AValue: string);
        procedure setFHttpEquiv(AValue: string);
    public
        constructor Create; override;

        property content: string read getContent write setFContent;
        property http_equiv: string read getHttpEquiv write setFHttpEquiv;

        function setContent(_content: string): THtmlMetaTag;
        function setHttp_equiv(_http_equiv: string): THtmlMetaTag;
    end;

    { THtmlViewPort }

    THtmlViewPort = class(THtmlMetaTag)
    private
        myWidth: integer;
        myHeight: integer;
        myInitialScale: single;
        myMinimumScale: single;
        myMaximumScale: single;
        myUserScalable: boolean;

    protected
        function Render(_level: QWord): string; override;
    public
        constructor Create; override;

      {A positive integer inputNumber, or the text device-width
      Defines the pixel width of the viewport that you want
      the web site to be rendered at.}
        function Width(_width: integer = 0): THtmlViewPort;

      {height     A positive integer, or the text device-height
      Defines the height of the viewport. Not used by any browser.}
        function Height(_height: integer = 0): THtmlViewPort;

      {initial-scale     A positive inputNumber between 0.0 and 10.0
      Defines the ratio between the device width
      (device-width in portrait mode or device-height in landscape mode)
      and the viewport size.}
        function initial_scale(_initial_scale: single = 1.0): THtmlViewPort;

      {maximum-scale     A positive inputNumber between 0.0 and 10.0
      Defines the maximum amount to zoom in. It must be greater or equal to
      the minimum-scale or the behaviour is undefined. Browser settings can
      ignore this rule and iOS10+ ignores it by default.}
        function minimum_scale(_min_scale: single = 1.0): THtmlViewPort;

      {minimum-scale     A positive inputNumber between 0.0 and 10.0
      Defines the minimum zoom level. It must be smaller or equal to the
      maximum-scale or the behaviour is undefined. Browser settings can ignore
      this rule and iOS10+ ignores it by default.}
        function maximum_scale(_max_scale: single = 1.0): THtmlViewPort;

      {user-scalable     yes or no     If set to no, the user is not able to
      zoom in the webpage. The default is yes.
      Browser settings can ignore this rule, and iOS10+ ignores it by default.}
        function user_scalable(_user_scalable: boolean = True): THtmlViewPort;


    end;

    {Forms and inputs}
    THtmlControl = class (THtmlElement) {This is the only way to get THtmlSelection to also belong to the same control base class}

    end;
    { THtmlLabel }
    THtmlLabelPosition = (labelBefore, labelAfter, labelAbove, labelBelow);
    THtmlLabel = class(THtmlControl)
    private
        function getFor: string;
        function getForm: string;
        procedure setFFor(AValue: string);
        procedure setFForm(AValue: string);
    protected
        function Render(_level: QWord): string; override;
    public
        constructor Create; override;
        property for_: string read getFor write setFFor;
        property form: string read getForm write setFForm;

        function setFor(_for: string): THtmlLabel;
        function setForm(_form: string): THtmlLabel;
    end;


    { THtmlInput }
    THtmlInputclass = class of THtmlInput;
    THtmlInput = class(THtmlControl)
    private
        FLabel: THtmlLabel;
		mylabelPosition: THtmlLabelPosition;
		myBoundLabel: boolean;
        function getAccept: string;
        function getAlign: string;
        function getAlt: string;
        function getAutoComplete: string;
        function getAutoFocus: boolean;
        function getChecked: boolean;
        function getDirName: string;
        function getDisabled: boolean;
        function getForm: string;
        function getFormAction: string;
        function getFormEncType: string;
        function getFormMethod: string;
        function getFormNoValidate: boolean;
        function getFormTarget: string;
        function getHeight: string;
        function getInputType: THtmlInputType;
        function getLabel: string;
        function getList: string;
        function getMax: string;
        function getMaxLength: string;
        function getMin: string;
        function getMultiple: boolean;
        function getPattern: string;
        function getPlaceHolder: string;
        function getReadOnly: boolean;
        function getRequired: boolean;
        function getSrc: string;
        function getStep: string;
        function getValue: string;
        function getWidth: string;
        procedure setAccept(AValue: string);
        procedure setAlign(AValue: string);
        procedure setAlt(AValue: string);
        procedure setAutoComplete(AValue: string);
        procedure setAutofocus(AValue: boolean);
        procedure setChecked(AValue: boolean);
        procedure setDirName(AValue: string);
        procedure setDisabled(AValue: boolean);
        procedure setForm(AValue: string);
        procedure setFormAction(AValue: string);
        procedure setFormEncType(AValue: string);
        procedure setFormMethod(AValue: string);
        procedure setFormNoValidate(AValue: boolean);
        procedure setFormTarget(AValue: string);
        procedure setHeight(AValue: string);
        procedure setInputType(AValue: THtmlInputType);
        procedure setLabel(AValue: string);
        procedure setList(AValue: string);
        procedure setMax(AValue: string);
        procedure setMaxLength(AValue: string);
        procedure setMin(AValue: string);
        procedure setMultiple(AValue: boolean);
        procedure setPattern(AValue: string);
        procedure setPlaceHolder(AValue: string);
        procedure setReadOnly(AValue: boolean);
        procedure setSrc(AValue: string);
        procedure setStep(AValue: string);
        procedure setValue(AValue: string);
        procedure setWidth(AValue: string);
        procedure setRequired(const _required: boolean);
        function getSize: string;
        procedure setSize(const _size: string);
		procedure setlabelPosition(const _labelPosition: THtmlLabelPosition);

    protected
        function Render(_level: QWord): string; override;
    public
        constructor Create; override;
        destructor Destroy; override;
        property labelObj: THtmlLabel read FLabel;
        property _label: string read getLabel write setLabel;
        property labelPosition: THtmlLabelPosition read mylabelPosition write setlabelPosition;
        property accept: string read getAccept write setAccept;
        property align: string read getAlign write setAlign;
        property alt: string read getAlt write setAlt;
        property autocomplete: string read getAutoComplete write setAutoComplete;
        {flag} property autofocus: boolean read getAutoFocus write setAutofocus;
        {flag} property Checked: boolean read getChecked write setChecked;
        property dirname: string read getDirName write setDirName;
        {flag} property disabled: boolean read getDisabled write setDisabled;
        property form: string read getForm write setForm;
        property form_action: string read getFormAction write setFormAction;
        property form_enc_type: string read getFormEncType write setFormEncType;
        property form_method: string read getFormMethod write setFormMethod;
        {flag} property form_no_validate: boolean
            read getFormNoValidate write setFormNoValidate;
        property form_target: string read getFormTarget write setFormTarget;
        property Height: string read getHeight write setHeight;
        property list: string read getList write setList;
        property max: string read getMax write setMax;
        property max_length: string read getMaxLength write setMaxLength;
        property min: string read getMin write setMin;
        {flag} property multiple: boolean read getMultiple write setMultiple;
        property pattern: string read getPattern write setPattern;
        property placeholder: string read getPlaceHolder write setPlaceHolder;
        {flag} property read_only: boolean read getReadOnly write setReadOnly;
        {flag} property required: boolean read getRequired write setRequired;
        property size: string read getSize write setSize;
        property src: string read getSrc write setSrc;
        property step: string read getStep write setStep;
        property input_type: THtmlInputType read getInputType write setInputType;
        property Value: string read getValue write setValue;
        property Width: string read getWidth write setWidth;
        property boundLabel: boolean read myBoundLabel write myBoundLabel;

        {chainable methods}
        function label_(_labelObj: THtmlLabel): THtmlInput;
        function setMyBoundLabel(const _boundLabel: boolean): THtmlInput;
        function copyFrom(_source: THtmlElement): THtmlElement; reintroduce;

        function setId(_id: string): THtmlElement; override;
    end;

    { THtmlForm }

    THtmlForm = class(THtmlCollection)
    private
        function getAcceptCharSet: string;
        function getAction: string;
        function getAutoComplete: string;
        function getEncType: string;
        function getMethod: string;
        function getNoValidate: string;
        function getTarget: string;
        procedure setAcceptCharSet(AValue: string);
        procedure setAction(AValue: string);
        procedure setAutoComplete(AValue: string);
        procedure setEncType(AValue: string);
        procedure setMethod(AValue: string);
        procedure setNoValidate(AValue: string);
        procedure setTarget(AValue: string);
    public
        constructor Create; override;
        property _accept_charset: string read getAcceptCharSet write setAcceptCharSet;
        property _action: string read getAction write setAction;
        property _autocomplete: string read getAutoComplete write setAutoComplete;
        property _enctype: string read getEncType write setEncType;
        property _method: string read getMethod write setMethod;
        property _novalidate: string read getNoValidate write setNoValidate;
        property _target: string read getTarget write setTarget;

        function method(_m: string): THtmlForm;
        function target(_t: string): THtmlForm;
        function action(_a: string): THtmlForm;
        function enctype(_a: string): THtmlForm;
        function novalidate(_nv: string): THtmlForm;
        function autocomplete(_ac: string): THtmlForm;
        function hidden(_name: string; _value: string): THtmlForm;
    end;

    { THtmlLink }

    THtmlLink = class(THtmlElement)
    private
        function getHRef: string;
        function getMedia: string;
        function getRel: string;
        function getType: string;
        procedure setHRef(AValue: string);
        procedure setMedia(AValue: string);
        procedure setRel(AValue: string);
        procedure setType(AValue: string);
    protected
        function tag_start: string; override;
    public
        constructor Create; override;
        property href: string read getHRef write setHRef;
        property rel: string read getRel write setRel;
        property _type: string read getType write setType;
        property media: string read getMedia write setMedia;
    end;

    { THtmlStyleSheetLink }

    THtmlStyleSheetLink = class(THtmlLink)
        constructor Create; override;
    end;

    { THtmlAnchor }

    THtmlAnchor = class(THtmlCollection)
    private
        Ftargetframe: string;
        function getCoords: string;
        function getDownload: string;
        function getHref: string;
        function getHRefLang: string;
        function getHtmlRel: THtmlARel;
        function getMedia: string;
        function getMediaType: string;
        function getPing: string;
        function getReferrerPolicy: THtmlAReferrerPolicy;
        function getRev: string;
        function getTarget: THtmlATarget;
        procedure setCoords(AValue: string);
        procedure setDownload(AValue: string);
        procedure setHref(AValue: string);
        procedure setHRefLang(AValue: string);
        procedure setHtmlRel(AValue: THtmlARel);
        procedure setMedia(AValue: string);
        procedure setMediaType(AValue: string);
        procedure setPing(AValue: string);
        procedure setReferrerPolicy(AValue: THtmlAReferrerPolicy);
        procedure setRev(AValue: string);
        procedure setTarget(AValue: THtmlATarget);
        procedure Settargetframe(AValue: string);
    public
        constructor Create; override;

        property coords: string read getCoords write setCoords;
        property download: string read getDownload write setDownload;
        property href: string read getHref write setHref;
        property hreflang: string read getHRefLang write setHRefLang;
        property media: string read getMedia write setMedia;
        property ping: string read getPing write setPing;
        property referrer_policy: THtmlAReferrerPolicy
            read getReferrerPolicy write setReferrerPolicy;
        property rel: THtmlARel read getHtmlRel write setHtmlRel;
        property rev: string read getRev write setRev;
        property target: THtmlATarget read getTarget write setTarget;
        property targetframe: string read Ftargetframe write Settargetframe;
        property media_type: string read getMediaType write setMediaType;

    end;

    { THtmlNavbar }

    THtmlNavbar = class(THtmlCollection)
        menuList: THtmlUnorderedList;
        constructor Create; override;
        function addItem(_caption: string; _link: string): THtmlAnchor;
    end;

    THtmlMenu = class(THtmlCollection)

    end;

    THtmlAlert = class(THtmlElement)

    end;

    THtmlButtonType = (btnButton, btnSubmit, btnReset);

    { THtmlButtonTypeHelper }

    THtmlButtonTypeHelper = type helper for THtmlButtonType
        function toString: string;
        procedure fromString(_str: string);
    end;

    { THtmlButton }
    THtmlButton = class(THtmlControl)
        constructor Create; override;
        function Name(_name: string): THtmlButton;
        function setID(_id: string): THtmlButton; reintroduce;
        function buttonType(_btnType: THtmlButtonType = btnButton): THtmlButton;
        function autofocus: THtmlButton;
        function disabled: THtmlButton;
        function form(_formID: string): THtmlButton; overload;
        function form(_formObj: THtmlForm): THtmlButton; overload;
        function formAction(_formAction: string): THtmlButton;
        function formenctype(_enctype: THtmlFormEncType): THtmlButton;
        function formMethod(_formMethod: THtmlFormMethod): THtmlButton;
        function formNoValidate: THtmlButton;
        function formTarget(_target: THtmlATarget): THtmlButton;
        {}
        function onClickLoad(_url: string): THtmlButton;
        function onClick(_script: string): THtmlButton;
    end;

    { THtmlEditBox }

    THtmlEditBox = class(THtmlInput)
        constructor Create; override;
    end;

    { THtmlFieldSet }

    THtmlFieldSet = class(THtmlInput)
        constructor Create; override;
    end;

    { THtmlLegend }

    THtmlLegend = class(THtmlInput)
        constructor Create; override;
    end;

    { THtmlCheckBox }

    THtmlCheckBox = class(THtmlInput)
        constructor Create; override;
    end;

    { THtmlRadioButton }

    THtmlRadioButton = class(THtmlInput)
        constructor Create; override;
    end;

    { THtmlTextArea }

    THtmlTextArea = class(THtmlControl)
    private
        setPlaceHolder: string;
        setMaxLength: integer;
        function getWrap: string;
        procedure setWrap(const _wrap: string);
        function getRows: integer;
        procedure setRows(const _rows: integer);
        function getRequired: boolean;
        procedure setRequired(const _required: boolean);
        function getReadOnly: boolean;
        procedure setReadOnly(const _read_only: boolean);
        function getCols: integer;
        procedure setCols(const _cols: integer);
        function getDisabled: boolean;
        procedure setDisabled(const _disabled: boolean);
    public
        constructor Create; override;
        property wrap: string read getWrap write setWrap; {hard, soft}
        property rows: integer read getRows write setRows;
        property required: boolean read getRequired write setRequired;
        property read_only: boolean read getReadOnly write setReadOnly;
        property placeholder: string read setPlaceHolder write setPlaceHolder;
        property maxLength: integer read setMaxLength write setMaxLength;
        property cols: integer read getCols write setCols;
        property disabled: boolean read getDisabled write setDisabled;
    end;

    { THtmlSelectOption }

    THtmlSelectOption = class(THtmlControl)
        constructor Create; override;
    private
        myValue: string;
    public
        function Value(_Value: string): THtmlSelectOption; overload;
        function Value: string; overload;
    private
        mySelected: boolean;
    public
        function Selected(_Selected: boolean): THtmlSelectOption; overload;
        function Selected: boolean; overload;
    private
        myDisabled: boolean;
    public
        function Disabled(_Disabled: boolean): THtmlSelectOption; overload;
        function Disabled: boolean; overload;
    private
        myLabel: string;
    public
        function Label_(_Label: string): THtmlSelectOption; overload;
        function Label_: string; overload;
    end;

    { THtmlSelection }

    THtmlSelect = class(THtmlCollection)
    public
        constructor Create; override;
        function addOptions(_list: array of string): THtmlSelect;
        function addOption(_text: string; _value: string;
            _selected: boolean = False): THtmlSelectOption;
        function option(_index: integer): THtmlSelectOption;
        function multipleSelect: THtmlSelect;
        function singleSelect: THtmlSelect;
        function selected(_value: string; _skipLoop: boolean = true): THtmlSelect;
    end;

    {
    THtmlTagless: An element that does not have any tag.
    It inserts text as is.

    Useful when you want to import the contents of a file / segment
    into a container
    }
    THtmlTagless = class(THtmlElement)
    protected
        myFiler: TTextFiler;
        myuseFiler: boolean;
        function getText: string;
        procedure setFText(_t: string); reintroduce;
        procedure setuseFiler(const _useFiler: boolean);
    protected
        function Render(_level: QWord): string; override;
    public
        property Text: string read getText write setFText;
        property useFiler: boolean read myuseFiler write setuseFiler;
    public
        constructor Create; override;
        destructor Destroy; override;
        function require(_file: string): THtmlTagless;
    end;

    { THtmlImg }

    THtmlImg = class(THtmlElement)
    private
        function getSrc: string; virtual;
        procedure setSrc(const _src: string); virtual;
        function getAlt: string;
        procedure setAlt(const _alt: string);
    public
        constructor Create; override;
        property src: string read getSrc write setSrc;
        property alt: string read getAlt write setAlt;
    end;

	{ THtmlImageBase64 }

    THtmlImageBase64 = class(THtmlImg)
    private
        mySourceFile: string;
        function getSrc: string; override;
        procedure setSrc(const _src: string); override;
	end;

    { THtmlInterestingText }

    THtmlInterestingText = class(THtmlElement)
        constructor Create; override;
    end;

    { THtmlEmoji }

    THtmlEmoji = class(THtmlElement)
    private
        function getEmoji: string;
        procedure setEmoji(const _emoji: string);
    public
        constructor Create; override; overload;
        constructor Create(_emoji: string); override; overload;
        property emoji: string read getEmoji write setEmoji;
        function loading: THtmlEmoji;
        function disabled: THtmlEmoji;
        function small: THtmlEmoji;
        function medium: THtmlEmoji;
        function large: THtmlEmoji;
        function big: THtmlEmoji;
        function link: THtmlEmoji;
    end;

    { THtmlTemplate }

    THtmlTemplate = class(THtmlCollection)

        constructor Create; override;

    end;

	{ THtmlFragment }
    // Generates a fragment  <> </>
    THtmlFragment = class(THtmlCollection)
        constructor Create; override;
        function renderFormatString: string; override;
    end;

procedure applyStyleClass(_element: THtmlElement; _style: THtmlStyle);
procedure applyStyleInline(_element: THtmlElement; _style: THtmlStyle);
function addStyles(_styles: array of string): string;
function selectorToClass(_selector: string): string;
function basePath: string;
procedure basePath(_path: string);

{Create custom elements with the tag}
function el(_tag: string): THtmlElement;
function el(_tag: string; _elClass: THtmlElementClass): THtmlElement;

function NewHtmlBuilderObject(_source: TObject): TObject;

function imgAsBase64(_filePath: string): string;
function imgEmbedding(_mime: string; _base64: string): string;

const
    {HTML Entities }
    htmlbreak = '</br>';
    space = '&#160;';
    lessthan = '&#60;';
    greaterthan = '&#62;';
    ampersand = '&#38;';
    doublequotes = '&#34;';
    singlequotes = '&#39;';
    copyright = '&#169;';
    registered = '&#174;';
    trademark = '&#8482;';

    {currencies}
    cent = '&#162;';
    pound = '&#163;';
    yen = '&#165;';
    euro = '&#8364;';
    rupees = '&#8377;';

    ATTR_NUMBER_PREFIX  = '~!';
    ATTR_NUMBER_POSTFIX = '!~';

implementation

uses
    strutils, fpmimetypes,
    sugar.utils, sugar.csshelper,
    sugar.htmlfactory, sugar.logger ;

var
    myBasePath: string = '';

function el(_tag: string): THtmlElement;
begin
    Result:= THtmlElement.Create(_tag);
end;

function el(_tag: string; _elClass: THtmlElementClass): THtmlElement;
begin
    Result:= _elClass.Create(_tag);
end;

function NewHtmlBuilderObject(_source: TObject): TObject;
begin
  {Creates a new object just like the source. Make sure that you check for child
  classes first before you check base classes }

    Result := nil;
    case _source.ClassName of
        {htmlbuilder classes}
        'TJavaScript'       : Result := TJavaScript.Create;
        'TJavaScripts'      : Result := TJavaScripts.Create;
        'THtmlStyleList'    : Result := THtmlStyleList.Create;
        'THtmlStyleSelector': Result := THtmlStyleSelector.Create;
        'THtmlStyleSheet'   : Result := THtmlStyleSheet.Create;
        'THtmlStyle'        : Result := THtmlStyle.Create;
        'THTMLElementBase'  : Result := THTMLElementBase.Create;
        'THtmlElement'      : Result := THtmlElement.Create;
        'THeading'          : Result := THeading.Create;
        'THeading1'         : Result := THeading1.Create;
        'THeading2'         : Result := THeading2.Create;
        'THeading3'         : Result := THeading3.Create;
        'THeading4'         : Result := THeading4.Create;
        'THeading5'         : Result := THeading5.Create;
        'THeading6'         : Result := THeading6.Create;
        'THeading7'         : Result := THeading7.Create;
        'TParagraph'        : Result := TParagraph.Create;
        'THtmlDiv'          : Result := THtmlDiv.Create;
        'THtmlSection'      : Result := THtmlSection.Create;
        'THtmlTable'        : Result := THtmlTable.Create;
        'THtmlUnorderedList': Result := THtmlUnorderedList.Create;
        'THtmlOrderedList'  : Result := THtmlOrderedList.Create;
        'THtmlScript'       : Result := THtmlScript.Create;
        'THTMLMetaTag'      : Result := THTMLMetaTag.Create;
        'THtmlViewPort'     : Result := THtmlViewPort.Create;
        'THtmlForm'         : Result := THtmlForm.Create;
        'THtmlInput'        : Result := THtmlInput.Create;
        'THtmlLabel'        : Result := THtmlLabel.Create;
        'THtmlLink'         : Result := THtmlLink.Create;
        'THtmlAnchor'       : Result := THtmlAnchor.Create;
        'THtmlNavbar'       : Result := THtmlNavbar.Create;
        'THtmlMenu'         : Result := THtmlMenu.Create;
        'THtmlAlert'        : Result := THtmlAlert.Create;
        'THtmlButton'       : Result := THtmlButton.Create;
        'THtmlEditBox'      : Result := THtmlEditBox.Create;
        'THtmlFieldSet'     : Result := THtmlFieldSet.Create;
        'THtmlLegend'       : Result := THtmlLegend.Create;
        'THtmlCheckBox'     : Result := THtmlCheckBox.Create;
        'THtmlRadioButton'  : Result := THtmlRadioButton.Create;
        'THtmlTextArea'     : Result := THtmlTextArea.Create;
        'THtmlSelect'       : Result := THtmlSelect.Create;
        'THtmlTagless'      : Result := THtmlTagless.Create;
        'THtmlHead'         : Result := THtmlHead.Create;
        'THtmlBody'         : Result := THtmlBody.Create;
        'THtmlStyleSheetLink': Result := THtmlStyleSheetLink.Create;
        'THtmlCollection'   : Result := THtmlCollection.Create;
        'THtmlITag'         : Result := THtmlInterestingText.Create;
        'THtmlDialog'       : Result := THtmlDialog.Create;
        'THtmlDetails'      : Result := THtmlDetails.Create;
        'THtmlSummary'      : Result := THtmlSummary.Create;
        'THtmlTemplate'     : Result := THtmlTemplate.Create;
        'THtmlFragment'     : Result := THtmlFragment.Create;
        'THtmlImg'          : Result := THtmlImg.Create;
        'THtmlImageBase64'  : Result := THtmlImageBase64.Create;
        else
            raise Exception.Create('NewHtmlBuilderObject: does not create ' +
                _source.ClassName);
    end;
end;

function imgAsBase64(_filePath: string): string;
begin
    Result:= '';
    if fileExists(_filePath) then
        //Result := Format('data:%s;%s,%s',[getMimeTypeFor(_filePath), 'base64', FileToBase64(_filePath)])
        Result := imgEmbedding(getMimeTypeFor(_filePath), FileToBase64(_filePath));
end;

function imgEmbedding(_mime: string; _base64: string): string;
begin
    Result := Format('data:%s;%s,%s',[_mime, 'base64', _base64]);
end;



{ TCSSRules }

function TCSSRules.ruleDef: string;
var
	i: Integer;
begin
    Result:= '';
    for i:= 0 to pred(Count) do
    begin
        Result:= Result + Items[i].styleDef;// INDENTING NEEDS TO BE DONE
	end;
end;

{ TCSSFontFace }

function TCSSFontFace.getSrc: string;
begin
    Result:= myRule.getStyle('src');
end;

procedure TCSSFontFace.setSrc(const _src: string);
begin
    myRule.setStyle('src', _src);
end;

function TCSSFontFace.getFontFamily: string;
begin
    Result:= myRule.getStyle('font-family')
end;

procedure TCSSFontFace.setFontFamily(const _fontFamily: string);
begin
    myRule.setStyle('font-family', _fontFamily);
end;

function TCSSFontFace.getfontStretch: string;
begin
    Result:= myRule.getStyle('font-stretch');
end;

function TCSSFontFace.getfontStyle: string;
begin
    Result:= myRule.getStyle('font-style');
end;

function TCSSFontFace.getfontWeight: string;
begin
    Result:= myRule.getStyle('font-weight');
end;

function TCSSFontFace.getunicodeRange: string;
begin
    Result:= myRule.getStyle('unicode-range');
end;

procedure TCSSFontFace.setfontStretch(const _fontStretch: string);
begin
    myRule.setStyle('font-stretch', _fontStretch);
end;

procedure TCSSFontFace.setfontStyle(const _fontStyle: string);
begin
    myRule.setStyle('font-style', _fontStyle);
end;

procedure TCSSFontFace.setfontWeight(const _fontWeight: string);
begin
    myRule.setStyle('font-weight', _fontWeight);
end;

procedure TCSSFontFace.setunicodeRange(const _unicodeRange: string);
begin
    myRule.setStyle('unicode-range', _unicodeRange);
end;

function TCSSFontFace.isEmbedded: boolean;
begin
    Result:= not mySourceFile.isEmpty;
end;

function TCSSFontFace.embedFont(_fileName: string): TCSSFontFace;
begin
    Result:= self;
    if FileExists(_fileName) then
    begin
        mySourceFile:= _fileName;
        setSrc(Format('url(data:%s;base64,%s)', [getMimeTypeFor(_fileName), FileToBase64(_fileName)]));
	end;
end;

function TCSSFontFace.styleDef: string;
begin
	Result:=inherited styleDef;
end;

constructor TCSSFontFace.Create;
begin
    inherited;
    name:= 'font-face';
end;

{ TCSSStyleBase }

function TCSSStyleBase.setStyle(_prop: string; _value: string): integer;
begin
    Values[_prop] := addTerminator(_value);
    Result := 1; {TODO: Return the index of the line}
end;

function TCSSStyleBase.getStyle(_prop: string): string;
begin
    Result := Values[_prop];
end;

function TCSSStyleBase.getStyleText: string;
begin
    Result:= Text;
end;

function TCSSStyleBase.extractStyleText(_css_snippet: string): string;
var
    _start, _stop: integer;
begin
{     extractStyleText() :
      just extract whatever is between the first pair of curly
      braces.
}
    try
	    Result := '';
	    if _css_snippet.IsEmpty then
	        exit;

	    _start := Pos('{', _css_snippet);
	    _stop := Pos('}', _css_snippet);
	    if _start = 0 then
	        raise Exception.Create('extractStyleText:' +
	            '{ needed to mark starting position');

	    if _stop = 0 then
	        raise Exception.Create('extractStyleText:' +
	            '} needed to mark ending position');

	    if _stop < _start then
	        raise Exception.Create('extractStyleText: } comes before {. Cannot extract ');

	    Inc(_start); // copy from next char after open curly brace

	    Result := Copy(_css_snippet, _start, _stop - _start);

	    if Result.StartsWith(sLineBreak) then
	        Result := Result.Remove(0, 1);
	except
        on E:Exception do
        begin
            log('THtmlStyle.extractStyleText(): ' + E.Message);
		end;
	end;

end;

function TCSSStyleBase.formatCSSInternal(_styleText: string): string;
var
	_formatedStyle, _s: String;
begin
    _formatedStyle := '';

    for _s in _styleText.split(sLineBreak) do begin
        if _s.IsEmpty then continue;
        if not _formatedStyle.isEmpty then
            _formatedStyle := _formatedStyle + sLineBreak;
        _formatedStyle := _formatedStyle + indent + DEFAULT_INDENT + _s;
	end;

    Result := Format('%0:s {%1:s%2:s%3:s}', [indent + mySelector,   {0}
                                             sLineBreak,            {1}
                                             _formatedStyle,        {2}
                                             sLineBreak + indent]); {3}
end;

constructor TCSSStyleBase.Create;
begin
    inherited Create;
    NameValueSeparator := ':';
end;

destructor TCSSStyleBase.Destroy;
begin
	inherited Destroy;
end;

{ TCSSRule }

constructor TCSSRule.Create;
begin
    myRule := TCSSStyleBase.Create;
end;

destructor TCSSRule.Destroy;
begin
    myRule.Free;
    inherited;
end;

function TCSSRule.styleDef: string;
begin
    Result:= Format('@%s {' + sLineBreak + '%s}'   + sLineBreak, [Name,  myRule.getStyleText]);
end;

{ THtmlImageBase64 }

function THtmlImageBase64.getSrc: string;
begin
    result:= mySourceFile;
end;

procedure THtmlImageBase64.setSrc(const _src: string);
begin
    mySourceFile := _src;
    if FileExists(mySourceFile) then
        inherited setSrc(imgAsBase64(mySourceFile))
    else
        inherited setSrc('File not found: '+ mySourceFile);
end;

{ THTMLStyleElement }

constructor THTMLStyleElement.Create;
begin
   inherited Create;
   tag:= 'style';
   setAttr('type', 'text/css');
end;

{ TEmphasis }

constructor TEmphasis.Create;
begin
	inherited Create;
    tag := 'em';
end;

{ TStrong }

constructor TStrong.Create;
begin
	inherited Create;
    tag := 'strong';
end;


{ THtmlGrid }

constructor THtmlGrid.Create;
begin
    inherited Create;
    tag := 'div';
end;

{ THtmlDialog }

constructor THtmlDialog.Create;
begin
	inherited Create;
    tag := 'dialog'
end;

{ THtmlDetails }

constructor THtmlDetails.Create;
begin
	inherited Create;
    tag := 'details';
end;

function THtmlDetails.summary: THtmlSummary;
begin
    Result:= THtmlSummary.Create;
    Add(Result);
end;

{ THtmlSummary }

constructor THtmlSummary.Create;
begin
	inherited Create;
    tag := 'summary';
end;

{ THtmlTemplate }

constructor THtmlTemplate.Create;
begin
    inherited Create;
    tag := 'template';
end;

{ THtmlFragment }

constructor THtmlFragment.Create;
begin
	inherited Create;
    tag := '';
end;

function THtmlFragment.renderFormatString: string;
begin
    Result := '<>%s</>'; // Fragment does not have id or any other attribute
end;


{ THtmlTableBody }

function THtmlTableBody.render(_level: QWord): string;
begin

    if rowCount = 0 then
        Result := ''
    else
        Result := inherited;
end;

constructor THtmlTableBody.Create;
begin
    inherited Create;
    tag := 'tbody';
end;

function THtmlTableBody.newRow: THtmlTableRow;
begin
    Result := THtmlTableRow(add(THtmlTableRow.Create));
end;

function THtmlTableBody.rowCount: integer;
begin
    Result := Count;
end;

function THtmlTableBody.row(_index: integer): THtmlTableRow;
begin
    Result := nil;
    if _index < Count then
        Result := Items[_index] as THtmlTableRow;
end;

{ THtmlTableHeaderRow }

function THtmlTableHeaderRow.newCol(t: string): THtmlTableHeaderColumn;
begin
    Result := THtmlTableHeaderColumn(add(THtmlTableHeaderColumn.Create));
    Result.Text := t;
end;

function THtmlTableHeaderRow.col(_index: integer): THtmlTableHeaderColumn;
begin
    Result := inherited col(_index) as THtmlTableHeaderColumn;
end;

{ THtmlTableFooter }

function THtmlTableFooter.render(_level: QWord): string;
begin
    if rowCount = 0 then
        Result := ''
    else
        Result := inherited;
end;

constructor THtmlTableFooter.Create;
begin
    inherited Create;
    tag := 'tfoot';
end;

{ THtmlTableHeaderColumn }
constructor THtmlTableHeaderColumn.Create;
begin
    inherited Create;
    tag := 'th';
end;

function THtmlTableHeaderColumn.scope(_scope: string): THtmlTableHeaderColumn;
begin
    setAttr('scope', _scope);
    Result := self;
end;

{ THtmlTableHeader }

function THtmlTableHeader.render(_level: QWord): string;
begin
    if rowCount = 0 then
        Result := ''
    else
        Result := inherited;
end;

constructor THtmlTableHeader.Create;
begin
    inherited Create;
    tag := 'thead';

end;

function THtmlTableHeader.newRow: THtmlTableHeaderRow;
begin
    Result := THtmlTableHeaderRow(add(THtmlTableHeaderRow.Create));
end;

function THtmlTableHeader.row(_index: integer): THtmlTableHeaderRow;
begin
    Result := inherited row(_index) as THtmlTableHeaderRow;
end;

{ THtmlSelect }

constructor THtmlSelect.Create;
begin
    inherited Create;
    tag := 'select';
end;

function THtmlSelect.addOptions(_list: array of string): THtmlSelect;
var
    _option: string;
begin
    Result := self;
    for _option in _list do
    begin
        addOption(_option, _option)
        .setId(DEFAULT_FIELD_PREFIX + ReplaceStr(_option, ' ', '_'));
    end;
end;

function THtmlSelect.addOption(_text: string; _value: string;
    _selected: boolean): THtmlSelectOption;
begin
    Result := THtmlSelectOption( add(THtmlSelectOption.Create));
    with  Result as THtmlSelectOption do
    begin
        Value(_value);
        Selected(_selected);
        Text := _text;
    end;
end;

function THtmlSelect.option(_index: integer): THtmlSelectOption;
begin
    Result := THtmlSelectOption(Items[_index]);
end;

function THtmlSelect.multipleSelect: THtmlSelect;
begin
    setAttrFlag('multiple');
    Result := self;
end;

function THtmlSelect.singleSelect: THtmlSelect;
begin
    rmAttrFlag('multiple');
    Result := self;
end;

function THtmlSelect.selected(_value: string; _skipLoop: boolean): THtmlSelect;
var
    i: integer;
    _option: THtmlSelectOption;
begin
    _option:= find(DEFAULT_FIELD_PREFIX + _value) as THtmlSelectOption;

    if (not Assigned(_option)) then
    begin
	    if not _skipLoop then {only if we should loop through all}
	    begin
	        {Loop through each value}
	        for i:= 0 to pred(Count) do
	        begin
	            _option:= Items[i] as THtmlSelectOption;
	            if _option.Value = _value then
	            begin
	                {found. Set the option as selected}
	                _option.Selected(true);
	                break;
				end;
			end;
		end
	end
	else
        {Find worked so, just set it as selected}
        _option.Selected(true);

    Result:= self;
end;

{ THtmlSelectOption }

constructor THtmlSelectOption.Create;
begin
    inherited Create;
    tag := 'option';
end;

function THtmlSelectOption.Value(_Value: string): THtmlSelectOption;
begin
    setAttr('value', _Value);
    Result := self;
end;

function THtmlSelectOption.Value: string;
begin
    Result := getAttr('value');
end;

function THtmlSelectOption.Selected(_Selected: boolean): THtmlSelectOption;
begin
    if _selected then
        setAttrFlag('selected')
    else
        rmAttrFlag('selected');

    Result := self;
end;

function THtmlSelectOption.Selected: boolean;
begin
    Result := findAttrFlag('selected');
end;

function THtmlSelectOption.Disabled(_Disabled: boolean): THtmlSelectOption;
begin
    if _disabled then
        setAttrFlag('disabled')
    else
        rmAttrFlag('disabled');
    Result := self;
end;

function THtmlSelectOption.Disabled: boolean;
begin
    Result := findAttrFlag('disabled');
end;

function THtmlSelectOption.Label_(_Label: string): THtmlSelectOption;
begin
    Result := self;
    setAttr('label', _label);
end;

function THtmlSelectOption.Label_: string;
begin
    Result := getAttr('label');
end;

{ THtmlInterestingText }

constructor THtmlInterestingText.Create;
begin
    inherited Create;
    tag := 'i';
end;


{ THtmlRadioButton }

constructor THtmlRadioButton.Create;
begin
    inherited Create;
    input_type := inputRadio;
end;

{ THtmlCheckBox }

constructor THtmlCheckBox.Create;
begin
    inherited Create;
    input_type := inputCheckbox;
    labelPosition:= labelAfter;
end;

{ THtmlLegend }

constructor THtmlLegend.Create;
begin
    inherited Create;
    tag := 'legend';
end;

{ THtmlFieldSet }

constructor THtmlFieldSet.Create;
begin
    inherited Create;
    tag := 'fieldset';
end;

{ THtmlEditBox }

constructor THtmlEditBox.Create;
begin
    inherited Create;
    input_type := inputText;
end;

{ THtmlTextArea }

function THtmlTextArea.getCols: integer;
begin
    Result := 0;
    if not getAttr('cols').isEmpty then
        Result:= attrAsNum('cols').ToInteger;
end;

function THtmlTextArea.getDisabled: boolean;
begin
    Result:= findAttrFlag('disabled')
end;

function THtmlTextArea.getReadOnly: boolean;
begin
    Result:= findAttrFlag('readonly');
end;

function THtmlTextArea.getRequired: boolean;
begin
    Result:= findAttrFlag('required');
end;

function THtmlTextArea.getRows: integer;
begin
    Result := 0;
    if not getAttr('rows').isEmpty then
        Result:= attrAsNum('rows').ToInteger;

end;

function THtmlTextArea.getWrap: string;
begin
    Result:= getAttr('wrap');
end;

procedure THtmlTextArea.setCols(const _cols: integer);
begin
    setAttr('cols', _cols );
end;

procedure THtmlTextArea.setDisabled(const _disabled: boolean);
begin
    setAttrFlag('disabled', _disabled);
end;

procedure THtmlTextArea.setReadOnly(const _read_only: boolean);
begin
    setAttrFlag('readonly', _read_only);
end;

procedure THtmlTextArea.setRequired(const _required: boolean);
begin
    setAttrFlag('required', _required);
end;

procedure THtmlTextArea.setRows(const _rows: integer);
begin
    setAttr('rows',_rows);
end;

procedure THtmlTextArea.setWrap(const _wrap: string);
begin
    setAttr('wrap', _wrap);
end;

constructor THtmlTextArea.Create;
begin
    inherited Create;
    tag := 'textarea';
end;


{ THtmlEmoji }

function THtmlEmoji.getEmoji: string;
begin
    Result := FAttributes.Values['data-emoji'].Replace(':', '');
end;

procedure THtmlEmoji.setEmoji(const _emoji: string);
begin
    setAttr('data-emoji', ':' + _emoji + ':');
end;

constructor THtmlEmoji.Create;
begin
    inherited Create;
    tag := 'em';
end;

constructor THtmlEmoji.Create(_emoji: string);
begin
    Create; {Call the actual constructor}
    emoji := _emoji;
end;

function THtmlEmoji.loading: THtmlEmoji;
begin
    addClass('loading');
    Result := Self;
end;

function THtmlEmoji.disabled: THtmlEmoji;
begin
    addClass('disabled');
    Result := Self;
end;

function THtmlEmoji.small: THtmlEmoji;
begin
    addClass('small');
    Result := Self;
end;

function THtmlEmoji.medium: THtmlEmoji;
begin
    addClass('medium');
    Result := Self;
end;

function THtmlEmoji.large: THtmlEmoji;
begin
    addClass('large');
    Result := Self;
end;

function THtmlEmoji.big: THtmlEmoji;
begin
    addClass('big');
    Result := Self;
end;

function THtmlEmoji.link: THtmlEmoji;
begin
    addClass('link');
    Result := Self;
end;

{ THtmlSpan }

constructor THtmlSpan.Create;
begin
    inherited Create;
    tag := 'span';
end;

destructor THtmlSpan.Destroy;
begin
    inherited Destroy;
end;

{ THeading7 }

constructor THeading7.Create;
begin
    inherited Create;
    tag := 'h7';
end;

{ THeading6 }

constructor THeading6.Create;
begin
    inherited Create;
    tag := 'h6';
end;

{ THeading5 }

constructor THeading5.Create;
begin
    inherited Create;
    tag := 'h5';
end;

{ THtmlImg }

function THtmlImg.getAlt: string;
begin
    Result := FAttributes.Values['alt'];
end;

function THtmlImg.getSrc: string;
begin
    Result := FAttributes.Values['src'];
end;

procedure THtmlImg.setAlt(const _alt: string);
begin
    setAttr('alt', _alt);
end;

procedure THtmlImg.setSrc(const _src: string);
begin
    setAttr('src', _src);
end;

constructor THtmlImg.Create;
begin
    inherited Create;
    tag := 'img';
    alt:=' ';
    hasEndTag := False;
end;

{ THtmlTagless }

function THtmlTagless.getText: string;
begin
    if useFiler then
        FText := myFiler.content;
    Result := FText;
end;

procedure THtmlTagless.setFText(_t: string);
begin
    if useFiler then
    begin
        myFiler.content := _t;
    end;
    FText := _t;
end;

procedure THtmlTagless.setuseFiler(const _useFiler: boolean);
begin
    if myuseFiler = _useFiler then
        Exit;

    myuseFiler := _useFiler;

    if _useFiler then
    begin
        if myFiler.rootDir.IsEmpty then
            myFiler.rootDir := myBasePath;

        if myFiler.fileName.IsEmpty then
            myFiler.fileName := sanitizeFileName(tagID, '.html');
    end;
end;

function THtmlTagless.Render(_level: QWord): string;
begin
    Result := inherited;
end;

constructor THtmlTagless.Create;
begin
    inherited;
    tag := '';
    myFiler := TTextFiler.Create;
    myFiler.fileName := '';
end;

destructor THtmlTagless.Destroy;
begin
    FreeAndNil(myFiler);
    inherited Destroy;
end;

{
  require():
  You need to make sure the file is present under the rootDir if you
  wish to use the content synch functionality.

  If you need to import some other file, then simply load that
  and assign it to the Text property. Content sync to file will not
  be activated.
}

function THtmlTagless.require(_file: string): THtmlTagless;
begin
    myFiler.rootDir := BasePath;
    myFiler.fileName := _file;
    useFiler := True;
    Result := Self;
end;


const
    NO_INDEX = -101;

{ THtmlStyleSheet }

procedure THtmlStyleSheet.setfilePath(const _filePath: string);
begin
    inherited setFilePath(_filePath);
    myFiler.rootDir:= myfilePath;
end;

procedure THtmlStyleSheet.setwebPath(const _webPath: string);
begin
	inherited setWebPath(_webPath);
    myFiler.webRoot:= _webPath;
end;



constructor THtmlStyleSheet.Create;
begin
    inherited;
    myFiler := newTextFiler();
end;

destructor THtmlStyleSheet.Destroy;
begin
    myFiler.Destroy;
    inherited Destroy;
end;

function THtmlStyleSheet.uri: string;
begin
    Result := myFiler.url;
end;


{ TJavaScripts }

constructor TJavaScripts.Create(FreeObjects: boolean);
begin
    inherited;
    {default paths. You can change it after the object is created}
    filePath := getAssetsPath(ASSETS_JS);
    webPath := ASSETS_JS;
end;

function TJavaScripts.get(const s: shortstring): TJavaScript;
begin
    Result := inherited get(s);
    {Set Fileloader path}
    if Result.newObj then
    begin
        Result.RootDir(filePath);
        Result.WebRoot(webPath);
        Result.useFileLoader := useFileLoader;
        Result.newObj := False;
    end;
    {set JavaScript name}
    if Result.Name.IsEmpty then
        Result.Name(s);
end;

function TJavaScripts.code: string;
var
    _i: integer;
    _js: TJavaScript;
begin
    Result := '';
    for _i := 0 to Count - 1 do
    begin
        _js := items[_i];
        Result := Result + _js.code + sLineBreak;
    end;
end;

function TJavaScripts.save(_fileName: string): boolean;
begin
    Result := True;

    if _fileName.isEmpty then
        _fileName := scriptName;

    with newTextFiler() do
    begin
        rootDir := filePath;
        fileName := _fileName;
        content := code;
        Free;
    end;
end;

function TJavaScripts.copyFrom(_source: TJavaScripts): TJavaScripts;
var
    i: integer;
    _name: string;
    _obj: TJavaScript;
begin
    filePath := _source.filePath;
    for i := 0 to _source.Count - 1 do
    begin
        _name := Names[i];
        _obj := Items[i];
        Add(_name, _obj.clone);
    end;
    Result := self;
end;

function TJavaScripts.clone: TJavaScripts;
begin
    Result := NewHtmlBuilderObject(Self) as TJavaScripts;
    Result.copyFrom(self);
end;

function TJavaScripts.asProps: string;
var
    _i: integer;
    _code: string = '';
begin
    {Add other methods}
    for _i := 0 to Count - 1 do
    begin
        if _i > 0 then
            _code := _code + ',' + sLineBreak;

        _code := _code + Format(JSProp, [Names[_i], Items[_i].code]);
    end;
    Result := _code;
end;

function TJavaScripts.asObject: string;
begin
    Result := '{' + self.asProps + '}';
end;

function TJavaScripts.asClass: string;
begin

end;

function TJavaScripts.load(_scriptName: string): TJavaScript;
begin
    Result:= get(_scriptName);
    Result.useFileLoader:= True;
    Result.touch;
end;

{ TJavaScript }

procedure TJavaScript.setuseFileLoader(const _useFileLoader: boolean);
begin
    if myuseFileLoader = _useFileLoader then
        Exit;
    myuseFileLoader := _useFileLoader;
end;

procedure TJavaScript.setCodeDefinition(const _muster: string);
begin
	if myCodeDefinition=_muster then Exit;
	myCodeDefinition:=_muster;
end;

constructor TJavaScript.Create;
begin
    myCode := TStringList.Create;
    myFileLoader := newTextFiler();
    newObj := True;
end;

destructor TJavaScript.Destroy;
begin
    FreeAndNil(myFileLoader);
    FreeAndNil(myCode);
    inherited Destroy;
end;

function TJavaScript.Name(_name: string): TJavaScript;
begin
    if CompareStr(myName, _name) <> 0 then
    begin
        myName := _name;
        myFileLoader.fileName := sanitizeFileName(_name, '.js');
    end;
    Result := self;
end;

function TJavaScript.Name: string;
begin
    Result := myName;
end;

procedure TJavaScript.load;
var
    _content: string;
begin
    if useFileLoader and Assigned(myFileLoader) then
        myCode.Text := myFileLoader.Content;
end;

function TJavaScript.code: string;
begin
    load;
    if codeDefinition.isEmpty then
        codeDefinition:= '%s';
    Result := Format(codeDefinition, [myCode.Text]);
end;

procedure TJavaScript.Clear;
begin
    myCode.Clear;
end;

function TJavaScript.add(_line: string): TJavaScript;
begin
    myCode.Add(_line);
    Result := self;
end;

procedure TJavaScript.touch;
begin
    if Assigned(myFileLoader) then
        myFileLoader.touch;
end;

procedure TJavaScript.save;
begin
    myFileLoader.content := myCode.Text;
end;

function TJavaScript.RootDir(_RootDir: string): TJavaScript;
begin
    myFileLoader.rootDir := _RootDir;
    Result := self;
end;

function TJavaScript.RootDir: string;
begin
    Result := myFileLoader.rootDir;
end;

function TJavaScript.WebRoot(_WebRoot: string): TJavaScript;
begin
    myFileLoader.webRoot := _WebRoot;
    Result := Self;
end;

function TJavaScript.WebRoot: string;
begin
    Result := myFileLoader.webRoot;
end;

function TJavaScript.copyFrom(_source: TJavaScript): TJavaScript;
begin
    myCode.Text := _source.code;
    myName := _source.Name;
    Result := self;
end;

function TJavaScript.clone: TJavaScript;
begin
    Result := NewHtmlBuilderObject(Self) as TJavaScript;
    Result.copyFrom(self);
end;

function TJavaScript.url: string;
begin
    Result := myFileLoader.url;
end;


{ THtmlStyleListHelper }

procedure THtmlStyleList.setfilePath(const _filePath: string);
var
    i: integer;
begin
	if myfilePath=_filePath then Exit;
	myfilePath:=_filePath;
    for i:= 0 to pred(Count) do
        Items[i].filePath:= myFilePath;
end;

procedure THtmlStyleList.Setindent(const _value: string);
begin
	if myindent=_value then Exit;
	myindent:=_value;
end;

procedure THtmlStyleList.setwebPath(const _webPath: string);
var
	i: Integer;
begin
	if mywebPath=_webPath then Exit;
	mywebPath:=_webPath;

    for i:= 0 to pred(Count) do
        Items[i].webPath:= mywebPath;
end;

procedure THtmlStyleList.setdocumentName(const _documentName: string);
var
    i: integer;
begin
	if mydocumentName=_documentName then Exit;
	mydocumentName:=_documentName;

    for i:= 0 to Pred(Count) do
    begin
        Items[i].documentName:= mydocumentName;
	end;
end;

function THtmlStyleList.get(const s: shortstring): THtmlStyle;
begin
    Result := inherited get(s);
    if Result.selector <> s then
        Result.selector(s);

    if Result.webPath <> webPath then
        Result.webPath:= webPath;

    if Result.filePath<> filePath then
        Result.filePath:= filePath;
end;

function THtmlStyleList.styleDef: string;
var
    _style: THtmlStyle;
    i: integer;
begin
    Result := '';
    for i := 0 to Count - 1 do
    begin
        _style := Items[i];
        _style.indent := indent + DEFAULT_INDENT;
        Result := Result + _style.cssInternal + sLineBreak;
    end;
end;

function THtmlStyleList.save(_filename: string): boolean;
begin
    Result := True;
    try
        with newTextFiler() do
        begin
            fileName := _filename;
            content := styledef;
            Free;
        end;
    except
        Result := False;
    end;
end;

{ RgbColor }

procedure RgbColor.let(_red, _green, _blue: word; _alpha: word);
begin
    self.red := _red;
    self.green := _green;
    self.blue := _blue;
    self.alpha := _alpha;
end;

procedure RgbColor.let(_rbg: string);
begin
    raise Exception.Create('Not implemented');
end;

function RgbColor.rgba: string;
begin
    Result := sugar.csshelper.rgba(red, green, blue, FormatFloat('#.##', alpha) + '%');
end;

{ HslColor }

procedure HslColor.let(_hue: real; _sat: real; _lum: real; _alpha: word);
begin
    hue := _hue;
    sat := _sat;
    lum := _lum;
    alpha := _alpha;
end;

procedure HslColor.let(_hsla: string);
begin
    raise Exception.Create('Not implemented');
end;

function HslColor.hsla: string;
var
    _h, _s, _l, _a: string;
begin
    _h := deg(hue);
    _s := FormatFloat('#.##', sat) + '%';
    _l := FormatFloat('#.##', lum) + '%';
    _a := FormatFloat('#.##', alpha) + '%';
    Result := sugar.csshelper.hsla(_h, _s, _l, _a);
end;

{ THtmlNavbar }

constructor THtmlNavbar.Create;
begin
    inherited Create;
    tag := 'nav';
    menuList := THtmlUnorderedList(add(THtmlUnorderedList.Create));
end;

function THtmlNavbar.addItem(_caption: string; _link: string): THtmlAnchor;
begin
    Result := THtmlAnchor(menuList.item.a(_caption, _link));
end;

{ THtmlARelHelper }

function THtmlARelHelper.toString: string;
begin
    case self of
        alternate: Result := 'alternate';
        author: Result := 'author';
        bookmark: Result := 'bookmark';
        _external: Result := 'external';
        help: Result := 'help';
        license: Result := 'license';
        Next: Result := 'next';
        nofollow: Result := 'nofollow';
        noreferrer: Result := 'noreferrer';
        noopener: Result := 'noopener';
        prev: Result := 'prev';
        rel_search: Result := 'rel-search';
        tag: Result := 'tag';
    end;
end;

function THtmlARelHelper.fromString(_str: string): THtmlARel;
begin
    case LowerCase(_str) of
        'alternate': self := alternate;
        'author': self := author;
        'bookmark': self := bookmark;
        'external': self := _external;
        'help': self := help;
        'license': self := license;
        'next': self := Next;
        'nofollow': self := nofollow;
        'noreferrer': self := noreferrer;
        'noopener': self := noopener;
        'prev': self := prev;
        'rel-search': self := rel_search;
        'tag': self := tag;
    end;
    Result := self;
end;

{ THtmlAReferrerPolicyHelper }

function THtmlAReferrerPolicyHelper.toString: string;
begin
    case self of
        no_referrer: Result := 'no-referrer';
        no_referrer_when_downgrade: Result := 'no-referrer-when_downgrade';
        origin: Result := 'origin';
        origin_when_cross_origin: Result := 'origin-when-cross-origin';
        unsafe_url: Result := 'unsafe-url';
    end;
end;

function THtmlAReferrerPolicyHelper.fromString(_str: string): THtmlAReferrerPolicy;
begin
    case LowerCase(_str) of
        'no-referrer': self := no_referrer;
        'no-referrer-when-downgrade': self := no_referrer_when_downgrade;
        'origin': self := origin;
        'origin-when-cross-origin': self := origin_when_cross_origin;
        'unsafe-url': self := unsafe_url;
    end;
end;

{ THtmlGlobalAttributesHelper }

function THtmlGlobalAttributesHelper.asString: string;
begin
    Result := asString('');
end;

function THtmlGlobalAttributesHelper.asString(_v: string): string;
begin
    case self of
    	attr_accesskey:         Result := 'accesskey';
        attr_autocapitalize:    Result := 'autocapitalize';
        attr_autofocus:         Result := 'autofocums';
        attr_class:             Result := 'class';
        attr_contenteditable:   Result := 'contenteditable';
        attr_contextmenu:       Result := 'contextmenu';
        attr_data:              Result := 'data-' + _v;   // append the value
        attr_dir:               Result := 'dir';
        attr_draggable:         Result := 'draggable';
        attr_enterkeyhint:      Result := 'enterkeyhint';
        attr_exportparts:       Result := 'exportparts';
        attr_hidden:            Result := 'hidden';
        attr_id:                Result := 'id';
        attr_inert:             Result := 'inert';
        attr_inputmode:         Result := 'inputmode';
        attr_is:                Result := 'is';
        attr_itemid:            Result := 'itemid';
        attr_itemprop:          Result := 'itemprop';
        attr_itemref:           Result := 'itemref';
        attr_itemscope:         Result := 'itemscope';
        attr_itemtype:          Result := 'itemtype';
        attr_lang:              Result := 'lang';
        attr_nonce:             Result := 'nonce';
        attr_part:              Result := 'part';
        attr_popover:           Result := 'popover';
        attr_slot:              Result := 'slot';
        attr_spellcheck:        Result := 'spellcheck';
        attr_style:             Result := 'style';
        attr_tabindex:          Result := 'tabindex';
        attr_title:             Result := 'title';
        attr_translate:         Result := 'translate';
        attr_virtualkeyboardpolicy: Result := 'virutalkeyboardpolicy';
    end;

end;

{ THtmlInputTypeHelper }

function THtmlInputTypeHelper.toString: string;
begin
    case self of
        inputButton:     Result := 'button';
        inputCheckbox:  Result := 'checkbox';
        inputColor:     Result := 'color';
        inputDate:      Result := 'date';
        inputDatetime_local: Result := 'datetime-local';
        inputEmail:     Result := 'email';
        inputFile:      Result := 'file';
        inputHidden:    Result := 'hidden';
        inputImage:     Result := 'image';
        inputMonth:     Result := 'month';
        inputNumber:    Result := 'number';
        inputPassword:  Result := 'password';
        inputRadio:     Result := 'radio';
        inputRange:     Result := 'range';
        inputResetbtn:  Result := 'reset';
        inputSearch:    Result := 'search';
        inputSubmit:    Result := 'submit';
        inputTel:       Result := 'tel';
        inputText:      Result := 'text';
        inputTime:      Result := 'time';
        inputUrl:       Result := 'url';
        inputWeek:      Result := 'week';
    end;
end;

function THtmlInputTypeHelper.fromString(_str: string): THtmlInputtype;
begin
    case LowerCase(_str) of
        'button': self := inputButton;
        'checkbox': self := inputCheckbox;
        'color': self := inputColor;
        'date': self := inputDate;
        'datetime-local': self := inputDatetime_local;
        'email': self := inputEmail;
        'file': self := inputFile;
        'hidden': self := inputHidden;
        'image': self := inputImage;
        'month': self := inputMonth;
        'number': self := inputNumber;
        'password': self := inputPassword;
        'radio': self := inputRadio;
        'range': self := inputRange;
        'reset': self := inputResetbtn;
        'search': self := inputSearch;
        'submit': self := inputSubmit;
        'tel': self := inputTel;
        'text': self := inputText;
        'time': self := inputTime;
        'url': self := inputUrl;
        'week': self := inputWeek;
    end;
    Result := self;
end;

{ THtmlStyleSelector }

function THtmlStyleSelector.getStyle(const _pseudo: string): THtmlStyle;
var
    _selector: string;
begin
    _selector := Trim(myCurrentSelector + _pseudo);

    if _selector.Length = 0 then
        raise Exception.Create('THtmlStyleSelector: ' + sLinebreak +
            'CurrentSelector has not been defined so you cannot select a style');

    Result := myStyles.get(_selector);

    if Result.documentName <> mydocumentName then
        Result.documentName:= myDocumentName;

    if Result.filePath <> filePath then
        Result.filePath:= filePath;

    if Result.indent.isEmpty then
        Result.indent := indent;
end;

procedure THtmlStyleSelector.onCreateStyleObject(_style: TObject);
begin
    {set the asset path when it is being created}
    THtmlStyle(_style).filePath := filePath;
    THtmlStyle(_style).webPath  := webPath;
    THtmlStyle(_style).documentName:= documentName;
    THtmlStyle(_style).isCached := isCached;
end;

procedure THtmlStyleSelector.setDocumentName(const _documentName: string);
begin
	if mydocumentName=_documentName then Exit;
	mydocumentName:=_documentName;
    myStyles.documentName := mydocumentName;
end;

function THtmlStyleSelector.getFilePath: string;
begin
    if myFilePath.isEmpty then
        Result:= getAssetsPath('')
    else
        Result:= myFilePath;
end;

function THtmlStyleSelector.getWebPath: string;
begin
    Result:= myWebPath;
end;

procedure THtmlStyleSelector.Setindent(const _value: string);
begin
	if myindent=_value then Exit;
	myindent:=_value;
    myStyles.indent := _value;
end;

procedure THtmlStyleSelector.setFilePath(const _filePath: string);
begin
    myFilePath:= _filePath;
    myStyles.filePath:= myFilePath;
end;

procedure THtmlStyleSelector.setWebPath(const _webPath: string);
begin
    myWebPath:= _webPath;
    myStyles.webPath:= myWebPath;
end;

procedure THtmlStyleSelector.setisCached(const _isCached: boolean);
begin
	if myisCached=_isCached then Exit;
	myisCached:=_isCached;
end;

constructor THtmlStyleSelector.Create;
begin
    inherited;
    isCached:= false;
    myStyles := THtmlStyleList.Create;
    myStyles.onCreateObj := @onCreateStyleObject;
    myRules := TCSSRules.Create;
end;

destructor THtmlStyleSelector.Destroy;
var
    obj: Pointer;
begin
    {Free the lists}
    // FileLoader.Free;
    FreeAndNil(myStyles);
    FreeAndNil(myRules);
    inherited;
end;

function THtmlStyleSelector.save(_filename: string): boolean;
begin
    Result := myStyles.save(_filename);
end;

function THtmlStyleSelector.copyFrom(_source: THtmlStyleSelector): THtmlStyleSelector;
var
    i: integer;
begin
    myCurrentSelector := _source.myCurrentSelector;
    myAdjacent := _source.myAdjacent;
    mySibling := _source.mySibling;
    myChild := _source.myChild;
    myDecendent := _source.myDecendent;
    myColumn := _source.myColumn;

    {Deep copy myStyles}
    for i := 0 to _source.myStyles.Count - 1 do
    begin
        myStyles.add(_source.myStyles.Names[i], _source.myStyles.Items[i]);
    end;

    Result := self;
end;

function THtmlStyleSelector.clone: THtmlStyleSelector;
begin
    Result := NewHtmlBuilderObject(Self) as THtmlStyleSelector;
    Result.copyFrom(self);
end;

function THtmlStyleSelector.select(const _selector: string): THtmlStyleSelector;
begin
    myCurrentSelector := _selector;
    Result := self;
end;

function THtmlStyleSelector.adjacent(const _selector: string): THtmlStyle;
begin
    Result := getStyle(' + ' + _selector);
end;

function THtmlStyleSelector.sibling(const _selector: string): THtmlStyle;
begin
    Result := getStyle(' ~ ' + _selector);
end;

function THtmlStyleSelector.child(const _selector: string): THtmlStyle;
begin
    Result := getStyle(' > ' + _selector);
end;

function THtmlStyleSelector.decendent(const _selector: string): THtmlStyle;
begin
    Result := getStyle(' ' + _selector);
end;

function THtmlStyleSelector.column(const _selector: string): THtmlStyle;
begin
    Result := getStyle(' || ' + _selector);
end;

function THtmlStyleSelector.style: THtmlStyle;
begin
    Result := getStyle();
end;

function THtmlStyleSelector.styleList: THtmlStyleList;
begin
    Result := myStyles;
end;

function THtmlStyleSelector.import(_css: THtmlStyleSelector;
    _overwriteExisting: boolean): THtmlStyleSelector;
var
    i, j: integer;
    _sourceStyle: THtmlStyle;
    _newStyle: THtmlStyle;
    _prop, _value: string;
begin
    for i := 0 to _css.styleList.Count - 1 do
    begin
        _sourceStyle := _css.styleList.Items[i];

        _newStyle := styleList.get(_sourceStyle.selector);
        if (_newStyle.selector <> _sourcestyle.selector) then
            _newStyle.selector(_sourceStyle.selector);

        for j := 0 to _sourceStyle.Count - 1 do
        begin
            _prop := _sourceStyle.Names[j];
            _value := _sourceStyle.ValueFromIndex[j];

            if _overwriteExisting then
                _newStyle.Values[_prop] := _value
            else
                {append the style to existing. Do not replace}
                _newStyle.Values[_prop] :=
                    _newStyle.Values[_prop].Replace(';', ' ') + _value;
        end;
    end;
    Result := Self;
end;

function THtmlStyleSelector.active: THtmlStyle;
begin
    Result := getStyle(':active');
end;

function THtmlStyleSelector.Checked: THtmlStyle;
begin
    Result := getStyle(':checked');
end;

function THtmlStyleSelector.default_: THtmlStyle;
begin
    Result := getStyle(':default');
end;

function THtmlStyleSelector.defined: THtmlStyle;
begin
    Result := getStyle(':defined');
end;

function THtmlStyleSelector.disabled: THtmlStyle;
begin
    Result := getStyle(':disabled');
end;

function THtmlStyleSelector.empty: THtmlStyle;
begin
    Result := getStyle(':empty');
end;

function THtmlStyleSelector.Enabled: THtmlStyle;
begin
    Result := getStyle(':enabled');
end;

function THtmlStyleSelector.First: THtmlStyle;
begin
    Result := getStyle(':first');
end;

function THtmlStyleSelector.first_child: THtmlStyle;
begin
    Result := getStyle(':first-child');
end;

function THtmlStyleSelector.first_of_type: THtmlStyle;
begin
    Result := getStyle(':first-of-type');
end;

function THtmlStyleSelector.focus: THtmlStyle;
begin
    Result := getStyle(':focus');
end;

function THtmlStyleSelector.focus_visible: THtmlStyle;
begin
    Result := getStyle(':focus-visible');
end;

function THtmlStyleSelector.focus_within: THtmlStyle;
begin
    Result := getStyle(':focus-within');
end;

function THtmlStyleSelector.fullscreen: THtmlStyle;
begin
    Result := getStyle(':fullscreen');
end;

function THtmlStyleSelector.hover: THtmlStyle;
begin
    Result := getStyle(':hover');
end;

function THtmlStyleSelector.in_range: THtmlStyle;
begin
    Result := getStyle(':in-range');
end;

function THtmlStyleSelector.indeterminate: THtmlStyle;
begin
    Result := getStyle(':indeterminate');
end;

function THtmlStyleSelector.invalid: THtmlStyle;
begin
    Result := getStyle(':invalid');
end;

function THtmlStyleSelector.lang(_lang: string): THtmlStyle;
begin
    Result := getStyle(':lang(' + _lang + ')');
end;

function THtmlStyleSelector.last_child: THtmlStyle;
begin
    Result := getStyle(':last_child');
end;

function THtmlStyleSelector.last_of_type: THtmlStyle;
begin
    Result := getStyle(':last-of-type');
end;

function THtmlStyleSelector.left: THtmlStyle;
begin
    Result := getStyle(':left');
end;

function THtmlStyleSelector.link: THtmlStyle;
begin
    Result := getStyle(':link');
end;

function THtmlStyleSelector.not_(_selector: string): THtmlStyle;
begin
    Result := getStyle(':not(' + _selector + ')');
end;

function THtmlStyleSelector.nth_child(_n: integer): THtmlStyle;
begin
    Result := getStyle(':nth-child(' + _n.ToString + ')');
end;

function THtmlStyleSelector.nth_col(_n: integer): THtmlStyle;
begin
    Result := getStyle(':nth-col(' + _n.ToString + ')');
end;

function THtmlStyleSelector.nth_last_child(_n: integer): THtmlStyle;
begin
    Result := getStyle(':nth-last-child(' + _n.ToString + ')');
end;

function THtmlStyleSelector.nth_last_of_type(_n: integer): THtmlStyle;
begin
    Result := getStyle(':nth-last-of-type(' + _n.ToString + ')');
end;

function THtmlStyleSelector.nth_of_type(_n: integer): THtmlStyle;
begin
    Result := getStyle(':nth-of-type(' + _n.ToString + ')');
end;

function THtmlStyleSelector.only_child: THtmlStyle;
begin
    Result := getStyle(':only-child');
end;

function THtmlStyleSelector.only_of_type: THtmlStyle;
begin
    Result := getStyle(':only-of-type');
end;

function THtmlStyleSelector.optional: THtmlStyle;
begin
    Result := getStyle(':optional');
end;

function THtmlStyleSelector.out_of_range: THtmlStyle;
begin
    Result := getStyle(':out-of-range');
end;

function THtmlStyleSelector.placeholder_shown: THtmlStyle;
begin
    Result := getStyle(':placeholder-shown');
end;

function THtmlStyleSelector.read_only: THtmlStyle;
begin
    Result := getStyle(':read-only');
end;

function THtmlStyleSelector.read_write: THtmlStyle;
begin
    Result := getStyle(':read-write');
end;

function THtmlStyleSelector.required: THtmlStyle;
begin
    Result := getStyle(':required');
end;

function THtmlStyleSelector.right: THtmlStyle;
begin
    Result := getStyle(':right');
end;

function THtmlStyleSelector.root: THtmlStyle;
begin
    Result := getStyle(':root');
end;

function THtmlStyleSelector.scope: THtmlStyle;
begin
    Result := getStyle(':scope');
end;

function THtmlStyleSelector.target: THtmlStyle;
begin
    Result := getStyle(':target');
end;

function THtmlStyleSelector.valid: THtmlStyle;
begin
    Result := getStyle(':valid');
end;

function THtmlStyleSelector.visited: THtmlStyle;
begin
    Result := getStyle(':visited');
end;

function THtmlStyleSelector.after: THtmlStyle;
begin
    Result := getStyle('::after');
end;

function THtmlStyleSelector.before: THtmlStyle;
begin
    Result := getStyle('::before');
end;

function THtmlStyleSelector.cue: THtmlStyle;
begin
    Result := getStyle('::cue');
end;

function THtmlStyleSelector.cue_region: THtmlStyle;
begin
    Result := getStyle('::cue-region');
end;

function THtmlStyleSelector.first_letter: THtmlStyle;
begin
    Result := getStyle('::first-letter');
end;

function THtmlStyleSelector.first_line: THtmlStyle;
begin
    Result := getStyle('::first-line');
end;

function THtmlStyleSelector.grammar_error: THtmlStyle;
begin
    Result := getStyle('::grammar-error');
end;

function THtmlStyleSelector.marker: THtmlStyle;
begin
    Result := getStyle('::marker');
end;

function THtmlStyleSelector.part(): THtmlStyle;
begin
    Result := getStyle('::part');
end;

function THtmlStyleSelector.placeholder: THtmlStyle;
begin
    Result := getStyle('::placehoder');
end;

function THtmlStyleSelector.selection: THtmlStyle;
begin
    Result := getStyle('::selection');
end;

function THtmlStyleSelector.slotted(_selector: string): THtmlStyle;
begin
    Result := getStyle('::slotted');
end;

function THtmlStyleSelector.spelling_error: THtmlStyle;
begin
    Result := getStyle('::spelling-error');
end;

function THtmlStyleSelector.cssInternal: string;
begin
    Result := '<style type="text/css">' + sLineBreak + styledef + '</style>' + sLineBreak;
end;

function THtmlStyleSelector.styledef: string;
begin
    Result := myRules.ruleDef + myStyles.styleDef;
end;

function THtmlStyleSelector.styleCount: integer;
begin
    Result := myStyles.Count;
end;

function THtmlStyleSelector.ruleCount: integer;
begin
    Result:= myRules.Count;
end;

function THtmlStyleSelector.fontFace(_name: string): TCSSFontFace;
var
    _rule: TCSSRule;
begin
    Result:= nil;
    _rule:= myRules.find(_name);
    if not Assigned(_rule) then
    begin
        Result:= TCSSFontFace.Create;
        myRules.add(_name, Result);
	end
	else
    begin
        if _rule is TCSSFontFace then
            Result:= _rule as TCSSFontFace;
	end;
end;


{ CSSBoxesHelper }

function CSSBoxesHelper.AsString: string;
begin
    Result := 'not defined';
    case self of
        margin_box: Result := 'margin-box';
        border_box: Result := 'border-box';
        padding_box: Result := 'padding-box';
        content_box: Result := 'content-box';
        fill_box: Result := 'fill-box';
        stroke_box: Result := 'stroke-box';
        view_box: Result := 'view-box';
    end;
end;

function CSSBoxesHelper.fromString(_val: string): CSSBoxes;
begin
    case LowerCase(_val) of
        'margin-box': self := margin_box;
        'border-box': self := border_box;
        'padding-box': self := padding_box;
        'content-box': self := content_box;
        'fill-box': self := fill_box;
        'stroke-box': self := stroke_box;
        'view-box': self := view_box;
    end;
    Result := self;
end;

{ THtmlStyle }

constructor THtmlStyle.Create;
begin
    inherited Create;
end;

destructor THtmlStyle.Destroy;
begin
    myfileLoader.Free;
	inherited Destroy;
end;

function THtmlStyle.copyFrom(_source: THtmlStyle): THtmlStyle;
begin
    mySelector := _source.mySelector;
    Text := _source.Text;
    Result:= self;
end;

function THtmlStyle.clone: THtmlStyle;
begin
    {Duplicate the exact same class as current}
    Result := NewHtmlBuilderObject(Self) as THtmlStyle;
    Result.copyFrom(self);
end;

function THtmlStyle.undefined: boolean;
begin
    Result := (Count = 0);
end;

function THtmlStyle.selector(_selector: string): THtmlStyle;
begin
    mySelector := _selector;
    Result := Self;
end;

function THtmlStyle.selector: string;
begin
    Result := mySelector;
end;

function THtmlStyle.use(_source: THtmlStyle): THtmlStyle;
begin
    Text := Text + _source.Text;
    Result := self;
end;

//function THtmlStyle.setStyle(_prop: string; _value: string): integer;
//begin
//    Values[_prop] := addTerminator(_value);
//    Result := 1; {TODO: Return the index of the line}
//end;
//
//function THtmlStyle.getStyle(_prop: string): string;
//begin
//    Result := Values[_prop];
//end;

function THtmlStyle.extractStyleText(_css_snippet: string): string;
var
    _start, _stop: integer;
    _filename: string;
begin
{     extractStyleText() :
      just extract whatever is between the first pair of curly
      braces.
}
    try
	    Result := '';
	    if _css_snippet.IsEmpty then
	        exit;

	    _start := Pos('{', _css_snippet);
	    _stop := Pos('}', _css_snippet);
	    _filename := sanitizeFileName(mySelector, '.css');

	    if _start = 0 then
	        raise Exception.Create('extractStyleText:' +
	            '{ needed to mark starting position in the file:: ' + _filename);

	    if _stop = 0 then
	        raise Exception.Create('extractStyleText:' +
	            '} needed to mark ending position in the file ' + _filename);

	    if _stop < _start then
	        raise Exception.Create('extractStyleText: } comes before {. Cannot extract ' +
	            _filename);

	    Inc(_start); // copy from next char after open curly brace

	    Result := Copy(_css_snippet, _start, _stop - _start);

	    if Result.StartsWith(sLineBreak) then
	        Result := Result.Remove(0, 1);
	except
        on E:Exception do
        begin
            log('THtmlStyle.extractStyleText(): ' + E.Message);
		end;
	end;

end;

function THtmlStyle.getStyleText: string;
begin
{ - getStyleText() : Returns the style definition without selector

    This function also generates an individual css file that contains the
    style description for the defined selector

    extractStyleText() is defined to separate the formating of
    CSSInternal. Because the Text property contains only the style descriptions
    and not the selector, this must be added at the time of writing to the css
    file. When retreiving from the file, the selector and curly braces must
    be stripped away.

    Refactoring is required to make this more efficient.
    For alpha version, this is fine.
}
    Result := '';
    if isCached then
    begin
        FileLoader.fileName := sanitizeFileName(mySelector, '.css');
        Result := extractStyleText(FileLoader.content);

        if Result.isEmpty then
        begin
            Result := Text;
            Fileloader.Content := formatCSSInternal(Result);
        end;

        if Result.isEmpty then
            Result := Text;
	end
    else
        Result:= Text;

end;

function THtmlStyle.cssInline: string;
begin
    Result := Format('style="%s"', [getStyleText]);
end;

//function THtmlStyle.formatCSSInternal(_styleText: string): string;
//begin
//    Result := Format('%s {%s%s}', [mySelector, sLineBreak, _styleText]);
//end;

procedure THtmlStyle.setfilePath(const _filePath: string);
begin
	if myfilePath=_filePath then Exit;
	myfilePath:=_filePath;
    FileLoader.rootDir:= myfilePath;
end;

procedure THtmlStyle.setwebPath(const _webPath: string);
begin
	if mywebPath=_webPath then Exit;
	mywebPath:=_webPath;
end;

function THtmlStyle.getFileLoader: TTextFiler;
begin
    if not Assigned(myFileLoader) then
        myFileLoader := newTextFiler();
    Result:= myFileLoader;
end;

procedure THtmlStyle.setdocumentName(const _documentName: string);
begin
	if mydocumentName=_documentName then Exit;
	mydocumentName:=_documentName;
    if not (fileLoader.rootDir.EndsWith(_documentName)) then
        fileLoader.rootDir:= appendPath([filePath, myDocumentName]);
end;

procedure THtmlStyle.setisCached(const _isCached: boolean);
begin
	if myisCached=_isCached then Exit;
	myisCached:=_isCached;
end;

function THtmlStyle.cssInternal: string;
begin
    if not mySelector.IsEmpty then
        Result := formatCSSInternal(getStyleText)
    else
        log('THtmlStyle: Cannot generate css_internal because selector is undefined');
end;

function THtmlStyle.left(_left: string): THtmlStyle;
begin
    setStyle('left', _left);
    Result := Self;
end;

function THtmlStyle.left: string;
begin
    Result := getStyle('left');
end;

function THtmlStyle.top(_top: string): THtmlStyle;
begin
    setStyle('top', _top);
    Result := Self;
end;

function THtmlStyle.top: string;
begin
    Result := getStyle('top');
end;

function THtmlStyle.right(_right: string): THtmlStyle;
begin
    setStyle('right', _right);
    Result := Self;
end;

function THtmlStyle.right: string;
begin
    Result := getStyle('right');
end;

function THtmlStyle.animation(_animation: string): THtmlStyle;
begin
    setStyle('animation', _animation);
    Result := Self;
end;

function THtmlStyle.animation_delay(_animation_delay: string): THtmlStyle;
begin
    setStyle('animation-delay', _animation_delay);
    Result := Self;
end;

function THtmlStyle.animation_direction(_animation_direction: string): THtmlStyle;
begin
    setStyle('animation-direction', _animation_direction);
    Result := Self;
end;

function THtmlStyle.animation_duration(_animation_duration: string): THtmlStyle;
begin
    setStyle('animation-duration', _animation_duration);
    Result := Self;
end;

function THtmlStyle.animation_fill_mode(_animation_fill_mode: string): THtmlStyle;
begin
    setStyle('animation-fill-mode', _animation_fill_mode);
    Result := Self;
end;

function THtmlStyle.animation_iteration_count(_animation_iteration_count: string):
THtmlStyle;
begin
    setStyle('animation-iteration-count', _animation_iteration_count);
    Result := Self;
end;

function THtmlStyle.animation_name(_animation_name: string): THtmlStyle;
begin
    setStyle('animation-name', _animation_name);
    Result := Self;
end;

function THtmlStyle.animation_play_state(_animation_play_state: string): THtmlStyle;
begin
    setStyle('animation-play-state', _animation_play_state);
    Result := Self;
end;

function THtmlStyle.animation_timing_function(_animation_timing_function: string):
THtmlStyle;
begin
    setStyle('animation-timing-function', _animation_timing_function);
    Result := Self;
end;

function THtmlStyle.animation: string;
begin
    Result := getStyle('animation');
end;

function THtmlStyle.animation_delay: string;
begin
    Result := getStyle('animation-delay');
end;

function THtmlStyle.animation_direction: string;
begin
    Result := getStyle('animation-direction');
end;

function THtmlStyle.animation_duration: string;
begin
    Result := getStyle('animation-duration');
end;

function THtmlStyle.animation_fill_mode: string;
begin
    Result := getStyle('animation-fill-mode');
end;

function THtmlStyle.animation_iteration_count: string;
begin
    Result := getStyle('animation-iteration-count');
end;

function THtmlStyle.animation_name: string;
begin
    Result := getStyle('animation-name');

end;

function THtmlStyle.animation_play_state: string;
begin
    Result := getStyle('animation-play-state');
end;

function THtmlStyle.animation_timing_function: string;
begin
    Result := getStyle('animation-timing-function');
end;

function THtmlStyle.background(_background: string): THtmlStyle;
begin
    setStyle('background', _background);
    Result := Self;
end;

function THtmlStyle.background_attachment(_background_attachment: string): THtmlStyle;
begin
    setStyle('background-attachment', _background_attachment);
    Result := Self;
end;

function THtmlStyle.background_clip(_background_clip: string): THtmlStyle;
begin
    setStyle('background-clip', _background_clip);
    Result := Self;
end;

function THtmlStyle.background_color(_background_color: string): THtmlStyle;
begin
    setStyle('background-color', _background_color);
    Result := Self;
end;

function THtmlStyle.background_image(_background_image: string): THtmlStyle;
begin
    setStyle('background-image', _background_image);
    Result := Self;
end;

function THtmlStyle.background_origin(_background_origin: string): THtmlStyle;
begin
    setStyle('background-origin', _background_origin);
    Result := Self;
end;

function THtmlStyle.background_position(_background_position: string): THtmlStyle;
begin
    setStyle('background-position', _background_position);
    Result := Self;
end;

function THtmlStyle.background_position_x(_background_position_x: string): THtmlStyle;
begin
    setStyle('background-position-x', _background_position_x);
    Result := Self;
end;

function THtmlStyle.background_position_y(_background_position_y: string): THtmlStyle;
begin
    setStyle('background-position-y', _background_position_y);
    Result := Self;
end;

function THtmlStyle.background_repeat(_background_repeat: string): THtmlStyle;
begin
    setStyle('background-repeat', _background_repeat);
    Result := Self;
end;

function THtmlStyle.background_size(_background_size: string): THtmlStyle;
begin
    setStyle('background-size', _background_size);
    Result := Self;
end;

function THtmlStyle.background: string;
begin
    Result := getStyle('background');
end;

function THtmlStyle.background_attachment: string;
begin
    Result := getStyle('background-attachment');
end;

function THtmlStyle.background_clip: string;
begin
    Result := getStyle('background-clip');
end;

function THtmlStyle.background_color: string;
begin
    Result := getStyle('background-color');
end;

function THtmlStyle.background_image: string;
begin
    Result := getStyle('background-image');
end;

function THtmlStyle.background_origin: string;
begin
    Result := getStyle('background-origin');
end;

function THtmlStyle.background_position: string;
begin
    Result := getStyle('background-position');
end;

function THtmlStyle.background_position_x: string;
begin
    Result := getStyle('background-position-x');
end;

function THtmlStyle.background_position_y: string;
begin
    Result := getStyle('background-position-y');
end;

function THtmlStyle.background_repeat: string;
begin
    Result := getStyle('background-repeat');
end;

function THtmlStyle.background_size: string;
begin
    Result := getStyle('background-size');
end;

{FONT}
function THtmlStyle.font(_font: string): THtmlStyle;
begin
    setStyle('font', _font);
    Result := Self;
end;

function THtmlStyle.font_family(_font_family: string): THtmlStyle;
begin
    setStyle('font-family', _font_family);
    Result := Self;
end;

function THtmlStyle.font_feature_settings(_font_feature_settings: string): THtmlStyle;
begin
    setStyle('font-feature-settings', _font_feature_settings);
    Result := Self;
end;

function THtmlStyle.font_kerning(_font_kerning: string): THtmlStyle;
begin
    setStyle('font-kerning', _font_kerning);
    Result := Self;
end;

function THtmlStyle.font_language_override(_font_language_override: string): THtmlStyle;
begin
    setStyle('font-language-override', _font_language_override);
    Result := Self;
end;

function THtmlStyle.font_optical_sizing(_font_optical_sizing: string): THtmlStyle;
begin
    setStyle('font-optical-sizing', _font_optical_sizing);
    Result := Self;
end;

function THtmlStyle.font_size(_font_size: string): THtmlStyle;
begin
    setStyle('font-size', _font_size);
    Result := Self;
end;

function THtmlStyle.font_size_adjust(_font_size_adjust: string): THtmlStyle;
begin
    setStyle('font-size-adjust', _font_size_adjust);
    Result := Self;
end;

function THtmlStyle.font_stretch(_font_stretch: string): THtmlStyle;
begin
    setStyle('font-stretch', _font_stretch);
    Result := Self;
end;

function THtmlStyle.font_style(_font_style: string): THtmlStyle;
begin
    setStyle('font-style', _font_style);
    Result := Self;
end;

function THtmlStyle.font_synthesis(_font_synthesis: string): THtmlStyle;
begin
    setStyle('font-synthesis', _font_synthesis);
    Result := Self;
end;

function THtmlStyle.font_variant(_font_variant: string): THtmlStyle;
begin
    setStyle('font-variant', _font_variant);
    Result := Self;
end;

function THtmlStyle.font_variant_alternates(_font_variant_alternates: string):
THtmlStyle;
begin
    setStyle('font-variant-alternates', _font_variant_alternates);
    Result := Self;
end;

function THtmlStyle.font_variant_caps(_font_variant_caps: string): THtmlStyle;
begin
    setStyle('font-variant-caps', _font_variant_caps);
    Result := Self;
end;

function THtmlStyle.font_variant_east_asian(_font_variant_east_asian: string):
THtmlStyle;
begin
    setStyle('font-variant-east-asian', _font_variant_east_asian);
    Result := Self;
end;

function THtmlStyle.font_variant_ligatures(_font_variant_ligatures: string): THtmlStyle;
begin
    setStyle('font-variant-ligatures', _font_variant_ligatures);
    Result := Self;
end;

function THtmlStyle.font_variant_numeric(_font_variant_numeric: string): THtmlStyle;
begin
    setStyle('font-variant-numeric', _font_variant_numeric);
    Result := Self;
end;

function THtmlStyle.font_variant_position(_font_variant_position: string): THtmlStyle;
begin
    setStyle('font-variant-position', _font_variant_position);
    Result := Self;
end;

function THtmlStyle.font_weight(_font_weight: string): THtmlStyle;
begin
    setStyle('font-weight', _font_weight);
    Result := Self;
end;

function THtmlStyle.font: string;
begin
    Result := getStyle('font');
end;

function THtmlStyle.font_family: string;
begin
    Result := getStyle('font-family');
end;

function THtmlStyle.font_feature_settings: string;
begin
    Result := getStyle('font-feature-settings');
end;

function THtmlStyle.font_kerning: string;
begin
    Result := getStyle('font-kerning');
end;

function THtmlStyle.font_language_override: string;
begin
    Result := getStyle('font-language-override');
end;

function THtmlStyle.font_optical_sizing: string;
begin
    Result := getStyle('font-optical-sizing');
end;

function THtmlStyle.font_size: string;
begin
    Result := getStyle('font-size');
end;

function THtmlStyle.font_size_adjust: string;
begin
    Result := getStyle('font-size-adjust');
end;

function THtmlStyle.font_stretch: string;
begin
    Result := getStyle('font-stretch');
end;

function THtmlStyle.font_style: string;
begin
    Result := getStyle('font-style');
end;

function THtmlStyle.font_synthesis: string;
begin
    Result := getStyle('font-synthesis');
end;

function THtmlStyle.font_variant: string;
begin
    Result := getStyle('font-variant');
end;

function THtmlStyle.font_variant_alternates: string;
begin
    Result := getStyle('font-variant-alternates');
end;

function THtmlStyle.font_variant_caps: string;
begin
    Result := getStyle('font-variant-caps');
end;

function THtmlStyle.font_variant_east_asian: string;
begin
    Result := getStyle('font-variant-east-asian');
end;

function THtmlStyle.font_variant_ligatures: string;
begin
    Result := getStyle('font-variant-ligatures');
end;

function THtmlStyle.font_variant_numeric: string;
begin
    Result := getStyle('font-variant-numeric');
end;

function THtmlStyle.font_variant_position: string;
begin
    Result := getStyle('font-variant-position');
end;

function THtmlStyle.font_weight: string;
begin
    Result := getStyle('font-weight');
end;


function THtmlStyle.filter(_filter: string): THtmlStyle;
begin
    setStyle('filter', _filter);
    Result := Self;
end;

function THtmlStyle.filter_blur(_filter_blur: string): THtmlStyle;
begin
    setStyle('filter-blur', _filter_blur);
    Result := Self;
end;

function THtmlStyle.filter_brightness(_filter_brightness: string): THtmlStyle;
begin
    setStyle('filter-brightness ', _filter_brightness);
    Result := Self;
end;

function THtmlStyle.filter_contrast(_filter_contrast: string): THtmlStyle;
begin
    setStyle('filter-contrast', _filter_contrast);
    Result := Self;
end;

function THtmlStyle.filter_dropshadow(_filter_dropshadow: string): THtmlStyle;
begin
    setStyle('filter-dropshadow', _filter_dropshadow);
    Result := Self;
end;

function THtmlStyle.filter_grayscale(_filter_grayscale: string): THtmlStyle;
begin
    setStyle('filter-grayscale', _filter_grayscale);
    Result := Self;
end;

function THtmlStyle.filter_hue_rotate(_filter_hue_rotate: string): THtmlStyle;
begin
    setStyle('filter-hue-rotate', _filter_hue_rotate);
    Result := Self;
end;

function THtmlStyle.filter_invert(_filter_invert: string): THtmlStyle;
begin
    setStyle('filter-invert', _filter_invert);
    Result := Self;
end;

function THtmlStyle.filter_opacity(_filter_opacity: string): THtmlStyle;
begin
    setStyle('filter-opacity', _filter_opacity);
    Result := Self;
end;

function THtmlStyle.filter_saturate(_filter_saturate: string): THtmlStyle;
begin
    setStyle('filter-saturate', _filter_saturate);
    Result := Self;
end;

function THtmlStyle.filter_sepia(_filter_sepia: string): THtmlStyle;
begin
    setStyle('filter-sepia', _filter_sepia);
    Result := Self;
end;

function THtmlStyle.filter: string;
begin
    Result := getStyle('filter');
end;

function THtmlStyle.filter_blur: string;
begin
    Result := getStyle('filter-blur');
end;

function THtmlStyle.filter_brightness: string;
begin
    Result := getStyle('filter-brightness ');
end;

function THtmlStyle.filter_contrast: string;
begin
    Result := getStyle('filter-contrast');
end;

function THtmlStyle.filter_dropshadow: string;
begin
    Result := getStyle('filter-dropshadow');
end;

function THtmlStyle.filter_grayscale: string;
begin
    Result := getStyle('filter-grayscale');
end;

function THtmlStyle.filter_hue_rotate: string;
begin
    Result := getStyle('filter-hue-rotate');
end;

function THtmlStyle.filter_invert: string;
begin
    Result := getStyle('filter-invert');
end;

function THtmlStyle.filter_opacity: string;
begin
    Result := getStyle('filter-opacity');
end;

function THtmlStyle.filter_saturate: string;
begin
    Result := getStyle('filter-saturate');
end;

function THtmlStyle.filter_sepia: string;
begin
    Result := getStyle('filter-sepia');
end;

function THtmlStyle.padding(_padding: string): THtmlStyle;
begin
    setStyle('padding', _padding);
    Result := Self;
end;

function THtmlStyle.padding_block(_padding_block: string): THtmlStyle;
begin
    setStyle('padding-block', _padding_block);
    Result := Self;
end;

function THtmlStyle.padding_block_end(_padding_block_end: string): THtmlStyle;
begin
    setStyle('padding-block-end', _padding_block_end);
    Result := Self;
end;

function THtmlStyle.padding_block_start(_padding_block_start: string): THtmlStyle;
begin
    setStyle('padding-block-start', _padding_block_start);
    Result := Self;
end;

function THtmlStyle.padding_bottom(_padding_bottom: string): THtmlStyle;
begin
    setStyle('padding-bottom', _padding_bottom);
    Result := Self;
end;

function THtmlStyle.padding_inline(_padding_inline: string): THtmlStyle;
begin
    setStyle('padding-inline', _padding_inline);
    Result := Self;
end;

function THtmlStyle.padding_inline_end(_padding_inline_end: string): THtmlStyle;
begin
    setStyle('padding-inline-end', _padding_inline_end);
    Result := Self;
end;

function THtmlStyle.padding_inline_start(_padding_inline_start: string): THtmlStyle;
begin
    setStyle('padding-inline-start', _padding_inline_start);
    Result := Self;
end;

function THtmlStyle.padding_left(_padding_left: string): THtmlStyle;
begin
    setStyle('padding-left', _padding_left);
    Result := Self;
end;

function THtmlStyle.padding_right(_padding_right: string): THtmlStyle;
begin
    setStyle('padding-right', _padding_right);
    Result := Self;
end;

function THtmlStyle.padding_top(_padding_top: string): THtmlStyle;
begin
    setStyle('padding-top', _padding_top);
    Result := Self;
end;

function THtmlStyle.padding: string;
begin
    Result := getStyle('padding');
end;

function THtmlStyle.padding_block: string;
begin
    Result := getStyle('padding-block');
end;

function THtmlStyle.padding_block_end: string;
begin
    Result := getStyle('padding-block-end');
end;

function THtmlStyle.padding_block_start: string;
begin
    Result := getStyle('padding-block-start');
end;

function THtmlStyle.padding_bottom: string;
begin
    Result := getStyle('padding-bottom');
end;

function THtmlStyle.padding_inline: string;
begin
    Result := getStyle('padding-inline');
end;

function THtmlStyle.padding_inline_end: string;
begin
    Result := getStyle('padding-inline-end');
end;

function THtmlStyle.padding_inline_start: string;
begin
    Result := getStyle('padding-inline-start');
end;

function THtmlStyle.padding_left: string;
begin
    Result := getStyle('padding-left');
end;

function THtmlStyle.padding_right: string;
begin
    Result := getStyle('padding-right');
end;

function THtmlStyle.padding_top: string;
begin
    Result := getStyle('padding-top');
end;


{BORDER}
function THtmlStyle.border(_border: string): THtmlStyle;
begin
    setStyle('border', _border);
    Result := Self;
end;

function THtmlStyle.border_block(_border_block: string): THtmlStyle;
begin
    setStyle('border-block', _border_block);
    Result := Self;
end;

function THtmlStyle.border_block_color(_border_block_color: string): THtmlStyle;
begin
    setStyle('border-block-color', _border_block_color);
    Result := Self;
end;

function THtmlStyle.border_block_end(_border_block_end: string): THtmlStyle;
begin
    setStyle('border-block-end', _border_block_end);
    Result := Self;
end;

function THtmlStyle.border_block_end_color(_border_block_end_color: string): THtmlStyle;
begin
    setStyle('border-block-end-color', _border_block_end_color);
    Result := Self;
end;

function THtmlStyle.border_block_end_style(_border_block_end_style: string): THtmlStyle;
begin
    setStyle('border-block-end-style', _border_block_end_style);
    Result := Self;
end;

function THtmlStyle.border_block_end_width(_border_block_end_width: string): THtmlStyle;
begin
    setStyle('border-block-end-width', _border_block_end_width);
    Result := Self;
end;

function THtmlStyle.border_block_start(_border_block_start: string): THtmlStyle;
begin
    setStyle('border-block-start', _border_block_start);
    Result := Self;
end;

function THtmlStyle.border_block_start_color(_border_block_start_color: string):
THtmlStyle;
begin
    setStyle('border-block-start-color', _border_block_start_color);
    Result := Self;
end;

function THtmlStyle.border_block_start_style(_border_block_start_style: string):
THtmlStyle;
begin
    setStyle('border-block-start-style', _border_block_start_style);
    Result := Self;
end;

function THtmlStyle.border_block_start_width(_border_block_start_width: string):
THtmlStyle;
begin
    setStyle('border-block-start-width', _border_block_start_width);
    Result := Self;
end;

function THtmlStyle.border_block_style(_border_block_style: string): THtmlStyle;
begin
    setStyle('border-block-style', _border_block_style);
    Result := Self;
end;

function THtmlStyle.border_block_width(_border_block_width: string): THtmlStyle;
begin
    setStyle('border-block-width', _border_block_width);
    Result := Self;
end;

function THtmlStyle.border_bottom(_border_bottom: string): THtmlStyle;
begin
    setStyle('border-bottom', _border_bottom);
    Result := Self;
end;

function THtmlStyle.border_bottom_color(_border_bottom_color: string): THtmlStyle;
begin
    setStyle('border-bottom-color', _border_bottom_color);
    Result := Self;
end;

function THtmlStyle.border_bottom_left_radius(_border_bottom_left_radius: string):
THtmlStyle;
begin
    setStyle('border-bottom-left-radius', _border_bottom_left_radius);
    Result := Self;
end;

function THtmlStyle.border_bottom_right_radius(_border_bottom_right_radius: string):
THtmlStyle;
begin
    setStyle('border-bottom-right-radius', _border_bottom_right_radius);
    Result := Self;
end;

function THtmlStyle.border_bottom_style(_border_bottom_style: string): THtmlStyle;
begin
    setStyle('border-bottom-style', _border_bottom_style);
    Result := Self;
end;

function THtmlStyle.border_bottom_width(_border_bottom_width: string): THtmlStyle;
begin
    setStyle('border-bottom-width', _border_bottom_width);
    Result := Self;
end;

function THtmlStyle.border_collapse(_border_collapse: string): THtmlStyle;
begin
    setStyle('border-collapse', _border_collapse);
    Result := Self;
end;

function THtmlStyle.border_color(_border_color: string): THtmlStyle;
begin
    setStyle('border-color', _border_color);
    Result := Self;
end;

function THtmlStyle.border_end_end_radius(_border_end_end_radius: string): THtmlStyle;
begin
    setStyle('border-end-end-radius', _border_end_end_radius);
    Result := Self;
end;

function THtmlStyle.border_end_start_radius(_border_end_start_radius: string):
THtmlStyle;
begin
    setStyle('border-end-start-radius', _border_end_start_radius);
    Result := Self;
end;

function THtmlStyle.border_image(_border_image: string): THtmlStyle;
begin
    setStyle('border-image', _border_image);
    Result := Self;
end;

function THtmlStyle.border_image_outset(_border_image_outset: string): THtmlStyle;
begin
    setStyle('border-image-outset', _border_image_outset);
    Result := Self;
end;

function THtmlStyle.border_image_repeat(_border_image_repeat: string): THtmlStyle;
begin
    setStyle('border-image-repeat', _border_image_repeat);
    Result := Self;
end;

function THtmlStyle.border_image_slice(_border_image_slice: string): THtmlStyle;
begin
    setStyle('border-image-slice', _border_image_slice);
    Result := Self;
end;

function THtmlStyle.border_image_source(_border_image_source: string): THtmlStyle;
begin
    setStyle('border-image-source', _border_image_source);
    Result := Self;
end;

function THtmlStyle.border_image_width(_border_image_width: string): THtmlStyle;
begin
    setStyle('border-image-width', _border_image_width);
    Result := Self;
end;

function THtmlStyle.border_inline(_border_inline: string): THtmlStyle;
begin
    setStyle('border-inline', _border_inline);
    Result := Self;
end;

function THtmlStyle.border_inline_color(_border_inline_color: string): THtmlStyle;
begin
    setStyle('border-inline-color', _border_inline_color);
    Result := Self;
end;

function THtmlStyle.border_inline_end(_border_inline_end: string): THtmlStyle;
begin
    setStyle('border-inline-end', _border_inline_end);
    Result := Self;
end;

function THtmlStyle.border_inline_end_color(_border_inline_end_color: string):
THtmlStyle;
begin
    setStyle('border-inline-end-color', _border_inline_end_color);
    Result := Self;
end;

function THtmlStyle.border_inline_end_style(_border_inline_end_style: string):
THtmlStyle;
begin
    setStyle('border-inline-end-style', _border_inline_end_style);
    Result := Self;
end;

function THtmlStyle.border_inline_end_width(_border_inline_end_width: string):
THtmlStyle;
begin
    setStyle('border-inline-end-width', _border_inline_end_width);
    Result := Self;
end;

function THtmlStyle.border_inline_start(_border_inline_start: string): THtmlStyle;
begin
    setStyle('border-inline-start', _border_inline_start);
    Result := Self;
end;

function THtmlStyle.border_inline_start_color(_border_inline_start_color: string):
THtmlStyle;
begin
    setStyle('border-inline-start-color', _border_inline_start_color);
    Result := Self;
end;

function THtmlStyle.border_inline_start_style(_border_inline_start_style: string):
THtmlStyle;
begin
    setStyle('border-inline-start-style', _border_inline_start_style);
    Result := Self;
end;

function THtmlStyle.border_inline_start_width(_border_inline_start_width: string):
THtmlStyle;
begin
    setStyle('border-inline-start-width', _border_inline_start_width);
    Result := Self;
end;

function THtmlStyle.border_inline_style(_border_inline_style: string): THtmlStyle;
begin
    setStyle('border-inline-style', _border_inline_style);
    Result := Self;
end;

function THtmlStyle.border_inline_width(_border_inline_width: string): THtmlStyle;
begin
    setStyle('border-inline-width', _border_inline_width);
    Result := Self;
end;

function THtmlStyle.border_left(_border_left: string): THtmlStyle;
begin
    setStyle('border-left', _border_left);
    Result := Self;
end;

function THtmlStyle.border_left_color(_border_left_color: string): THtmlStyle;
begin
    setStyle('border-left-color', _border_left_color);
    Result := Self;
end;

function THtmlStyle.border_left_style(_border_left_style: string): THtmlStyle;
begin
    setStyle('border-left-style', _border_left_style);
    Result := Self;
end;

function THtmlStyle.border_left_width(_border_left_width: string): THtmlStyle;
begin
    setStyle('border-left-width', _border_left_width);
    Result := Self;
end;

function THtmlStyle.border_radius(_border_radius: string): THtmlStyle;
begin
    setStyle('border-radius', _border_radius);
    Result := Self;
end;

function THtmlStyle.border_right(_border_right: string): THtmlStyle;
begin
    setStyle('border-right', _border_right);
    Result := Self;
end;

function THtmlStyle.border_right_color(_border_right_color: string): THtmlStyle;
begin
    setStyle('border-right-color', _border_right_color);
    Result := Self;
end;

function THtmlStyle.border_right_style(_border_right_style: string): THtmlStyle;
begin
    setStyle('border-right-style', _border_right_style);
    Result := Self;
end;

function THtmlStyle.border_right_width(_border_right_width: string): THtmlStyle;
begin
    setStyle('border-right-width', _border_right_width);
    Result := Self;
end;

function THtmlStyle.border_spacing(_border_spacing: string): THtmlStyle;
begin
    setStyle('border-spacing', _border_spacing);
    Result := Self;
end;

function THtmlStyle.border_start_end_radius(_border_start_end_radius: string):
THtmlStyle;
begin
    setStyle('border-start-end-radius', _border_start_end_radius);
    Result := Self;
end;

function THtmlStyle.border_start_start_radius(_border_start_start_radius: string):
THtmlStyle;
begin
    setStyle('border-start-start-radius', _border_start_start_radius);
    Result := Self;
end;

function THtmlStyle.border_style(_border_style: string): THtmlStyle;
begin
    setStyle('border-style', _border_style);
    Result := Self;
end;

function THtmlStyle.border_top(_border_top: string): THtmlStyle;
begin
    setStyle('border-top', _border_top);
    Result := Self;
end;

function THtmlStyle.border_top_color(_border_top_color: string): THtmlStyle;
begin
    setStyle('border-top-color', _border_top_color);
    Result := Self;
end;

function THtmlStyle.border_top_left_radius(_border_top_left_radius: string): THtmlStyle;
begin
    setStyle('border-top-left-radius', _border_top_left_radius);
    Result := Self;
end;

function THtmlStyle.border_top_right_radius(_border_top_right_radius: string):
THtmlStyle;
begin
    setStyle('border-top-right-radius', _border_top_right_radius);
    Result := Self;
end;

function THtmlStyle.border_top_style(_border_top_style: string): THtmlStyle;
begin
    setStyle('border-top-style', _border_top_style);
    Result := Self;
end;

function THtmlStyle.border_top_width(_border_top_width: string): THtmlStyle;
begin
    setStyle('border-top-width', _border_top_width);
    Result := Self;
end;

function THtmlStyle.border_width(_border_width: string): THtmlStyle;
begin
    setStyle('border-width', _border_width);
    Result := Self;
end;

function THtmlStyle.border: string;
begin
    Result := getStyle('border');
end;

function THtmlStyle.border_block: string;
begin
    Result := getStyle('border-block');
end;

function THtmlStyle.border_block_color: string;
begin
    Result := getStyle('border-block-color');
end;

function THtmlStyle.border_block_end: string;
begin
    Result := getStyle('border-block-end');
end;

function THtmlStyle.border_block_end_color: string;
begin
    Result := getStyle('border-block-end-color');
end;

function THtmlStyle.border_block_end_style: string;
begin
    Result := getStyle('border-block-end-style');
end;

function THtmlStyle.border_block_end_width: string;
begin
    Result := getStyle('border-block-end-width');
end;

function THtmlStyle.border_block_start: string;
begin
    Result := getStyle('border-block-start');
end;

function THtmlStyle.border_block_start_color: string;
begin
    Result := getStyle('border-block-start-color');
end;

function THtmlStyle.border_block_start_style: string;
begin
    Result := getStyle('border-block-start-style');
end;

function THtmlStyle.border_block_start_width: string;
begin
    Result := getStyle('border-block-start-width');
end;

function THtmlStyle.border_block_style: string;
begin
    Result := getStyle('border-block-style');
end;

function THtmlStyle.border_block_width: string;
begin
    Result := getStyle('border-block-width');
end;

function THtmlStyle.border_bottom: string;
begin
    Result := getStyle('border-bottom');
end;

function THtmlStyle.border_bottom_color: string;
begin
    Result := getStyle('border-bottom-color');
end;

function THtmlStyle.border_bottom_left_radius: string;
begin
    Result := getStyle('border-bottom-left-radius');
end;

function THtmlStyle.border_bottom_right_radius: string;
begin
    Result := getStyle('border-bottom-right-radius');
end;

function THtmlStyle.border_bottom_style: string;
begin
    Result := getStyle('border-bottom-style');
end;

function THtmlStyle.border_bottom_width: string;
begin
    Result := getStyle('border-bottom-width');
end;

function THtmlStyle.border_collapse: string;
begin
    Result := getStyle('border-collapse');
end;

function THtmlStyle.border_color: string;
begin
    Result := getStyle('border-color');
end;

function THtmlStyle.border_end_end_radius: string;
begin
    Result := getStyle('border-end-end-radius');
end;

function THtmlStyle.border_end_start_radius: string;
begin
    Result := getStyle('border-end-start-radius');
end;

function THtmlStyle.border_image: string;
begin
    Result := getStyle('border-image');
end;

function THtmlStyle.border_image_outset: string;
begin
    Result := getStyle('border-image-outset');
end;

function THtmlStyle.border_image_repeat: string;
begin
    Result := getStyle('border-image-repeat');
end;

function THtmlStyle.border_image_slice: string;
begin
    Result := getStyle('border-image-slice');
end;

function THtmlStyle.border_image_source: string;
begin
    Result := getStyle('border-image-source');
end;

function THtmlStyle.border_image_width: string;
begin
    Result := getStyle('border-image-width');
end;

function THtmlStyle.border_inline: string;
begin
    Result := getStyle('border-inline');
end;

function THtmlStyle.border_inline_color: string;
begin
    Result := getStyle('border-inline-color');
end;

function THtmlStyle.border_inline_end: string;
begin
    Result := getStyle('border-inline-end');
end;

function THtmlStyle.border_inline_end_color: string;
begin
    Result := getStyle('border-inline-end-color');
end;

function THtmlStyle.border_inline_end_style: string;
begin
    Result := getStyle('border-inline-end-style');
end;

function THtmlStyle.border_inline_end_width: string;
begin
    Result := getStyle('border-inline-end-width');
end;

function THtmlStyle.border_inline_start: string;
begin
    Result := getStyle('border-inline-start');
end;

function THtmlStyle.border_inline_start_color: string;
begin
    Result := getStyle('border-inline-start-color');
end;

function THtmlStyle.border_inline_start_style: string;
begin
    Result := getStyle('border-inline-start-style');
end;

function THtmlStyle.border_inline_start_width: string;
begin
    Result := getStyle('border-inline-start-width');
end;

function THtmlStyle.border_inline_style: string;
begin
    Result := getStyle('border-inline-style');
end;

function THtmlStyle.border_inline_width: string;
begin
    Result := getStyle('border-inline-width');
end;

function THtmlStyle.border_left: string;
begin
    Result := getStyle('border-left');
end;

function THtmlStyle.border_left_color: string;
begin
    Result := getStyle('border-left-color');
end;

function THtmlStyle.border_left_style: string;
begin
    Result := getStyle('border-left-style');
end;

function THtmlStyle.border_left_width: string;
begin
    Result := getStyle('border-left-width');
end;

function THtmlStyle.border_radius: string;
begin
    Result := getStyle('border-radius');
end;

function THtmlStyle.border_right: string;
begin
    Result := getStyle('border-right');
end;

function THtmlStyle.border_right_color: string;
begin
    Result := getStyle('border-right-color');
end;

function THtmlStyle.border_right_style: string;
begin
    Result := getStyle('border-right-style');
end;

function THtmlStyle.border_right_width: string;
begin
    Result := getStyle('border-right-width');
end;

function THtmlStyle.border_spacing: string;
begin
    Result := getStyle('border-spacing');
end;

function THtmlStyle.border_start_end_radius: string;
begin
    Result := getStyle('border-start-end-radius');
end;

function THtmlStyle.border_start_start_radius: string;
begin
    Result := getStyle('border-start-start-radius');
end;

function THtmlStyle.border_style: string;
begin
    Result := getStyle('border-style');
end;

function THtmlStyle.border_top: string;
begin
    Result := getStyle('border-top');
end;

function THtmlStyle.border_top_color: string;
begin
    Result := getStyle('border-top-color');
end;

function THtmlStyle.border_top_left_radius: string;
begin
    Result := getStyle('border-top-left-radius');
end;

function THtmlStyle.border_top_right_radius: string;
begin
    Result := getStyle('border-top-right-radius');
end;

function THtmlStyle.border_top_style: string;
begin
    Result := getStyle('border-top-style');
end;

function THtmlStyle.border_top_width: string;
begin
    Result := getStyle('border-top-width');
end;

function THtmlStyle.border_width: string;
begin
    Result := getStyle('border-width');
end;

function THtmlStyle.bottom(_bottom: string): THtmlStyle;
begin
    setStyle('bottom', _bottom);
    Result := Self;
end;

function THtmlStyle.bottom: string;
begin
    Result := getStyle('bottom');
end;

function THtmlStyle.box_shadow(_box_shadow: string): THtmlStyle;
begin
    setStyle('box-shadow', _box_shadow);
    Result := self;
end;

function THtmlStyle.box_shadow(_offsetX: string; _offsetY: string;
    _blurRadius: string; _spreadRadius: string; _colour: string): THtmlStyle;
begin
    setStyle('box-shadow', Format('%s %s %s %s %s',
        [_offsetX, _offsetY, _blurRadius, _spreadRadius, _colour]));
    Result := self;
end;

function THtmlStyle.box_shadow: string;
begin
    Result := getStyle('box-shadow');
end;

{MARGIN}
function THtmlStyle.margin(_margin: string): THtmlStyle;
begin
    setStyle('margin', _margin);
    Result := Self;
end;

function THtmlStyle.margin_block(_margin_block: string): THtmlStyle;
begin
    setStyle('margin-block', _margin_block);
    Result := Self;
end;

function THtmlStyle.margin_block_end(_margin_block_end: string): THtmlStyle;
begin
    setStyle('margin-block-end', _margin_block_end);
    Result := Self;
end;

function THtmlStyle.margin_block_start(_margin_block_start: string): THtmlStyle;
begin
    setStyle('margin-block-start', _margin_block_start);
    Result := Self;
end;

function THtmlStyle.margin_bottom(_margin_bottom: string): THtmlStyle;
begin
    setStyle('margin-bottom', _margin_bottom);
    Result := Self;
end;

function THtmlStyle.margin_inline(_margin_inline: string): THtmlStyle;
begin
    setStyle('margin-inline', _margin_inline);
    Result := Self;
end;

function THtmlStyle.margin_inline_end(_margin_inline_end: string): THtmlStyle;
begin
    setStyle('margin-inline-end', _margin_inline_end);
    Result := Self;
end;

function THtmlStyle.margin_inline_start(_margin_inline_start: string): THtmlStyle;
begin
    setStyle('margin-inline-start', _margin_inline_start);
    Result := Self;
end;

function THtmlStyle.margin_left(_margin_left: string): THtmlStyle;
begin
    setStyle('margin-left', _margin_left);
    Result := Self;
end;

function THtmlStyle.margin_right(_margin_right: string): THtmlStyle;
begin
    setStyle('margin-right', _margin_right);
    Result := Self;
end;

function THtmlStyle.margin_top(_margin_top: string): THtmlStyle;
begin
    setStyle('margin-top', _margin_top);
    Result := Self;
end;

function THtmlStyle.margin: string;
begin
    Result := getStyle('margin');
end;

function THtmlStyle.margin_block: string;
begin
    Result := getStyle('margin-block');
end;

function THtmlStyle.margin_block_end: string;
begin
    Result := getStyle('margin-block-end');
end;

function THtmlStyle.margin_block_start: string;
begin
    Result := getStyle('margin-block-start');
end;

function THtmlStyle.margin_bottom: string;
begin
    Result := getStyle('margin-bottom');
end;

function THtmlStyle.margin_inline: string;
begin
    Result := getStyle('margin-inline');
end;

function THtmlStyle.margin_inline_end: string;
begin
    Result := getStyle('margin-inline-end');
end;

function THtmlStyle.margin_inline_start: string;
begin
    Result := getStyle('margin-inline-start');
end;

function THtmlStyle.margin_left: string;
begin
    Result := getStyle('margin-left');
end;

function THtmlStyle.margin_right: string;
begin
    Result := getStyle('margin-right');
end;

function THtmlStyle.margin_top: string;
begin
    Result := getStyle('margin-top');
end;

function THtmlStyle.outline(_outline: string): THtmlStyle;
begin
    setStyle('outline', _outline);
    Result := Self;
end;

function THtmlStyle.outline: string;
begin
    Result := getStyle('outline');
end;

function THtmlStyle.outline_color(_outline_color: string): THtmlStyle;
begin
    setStyle('outline-color', _outline_color);
    Result := Self;
end;

function THtmlStyle.outline_color: string;
begin
    Result := getStyle('outline-color');
end;

function THtmlStyle.outline_offset(_outline_offset: string): THtmlStyle;
begin
    setStyle('outline-offset', _outline_offset);
    Result := Self;
end;

function THtmlStyle.outline_offset: string;
begin
    Result := getStyle('outline-offset');
end;

function THtmlStyle.outline_style(_outline_style: string): THtmlStyle;
begin
    setStyle('outline-style', _outline_style);
    Result := Self;
end;

function THtmlStyle.outline_style: string;
begin
    Result := getStyle('outline-style');
end;

function THtmlStyle.outline_width(_outline_width: string): THtmlStyle;
begin
    setStyle('outline-width', _outline_width);
    Result := Self;
end;

function THtmlStyle.outline_width: string;
begin
    Result := getStyle('outline-width');
end;

function THtmlStyle.overflow(_overflow: string): THtmlStyle;
begin
    setStyle('overflow', _overflow);
    Result := Self;
end;

function THtmlStyle.overflow: string;
begin
    Result := getStyle('overflow');
end;

function THtmlStyle.overflow_wrap(_overflow_wrap: string): THtmlStyle;
begin
    setStyle('overflow-wrap', _overflow_wrap);
    Result := Self;
end;

function THtmlStyle.overflow_wrap: string;
begin
    Result := getStyle('overflow-wrap');
end;

function THtmlStyle.overflow_x(_overflow_x: string): THtmlStyle;
begin
    setStyle('overflow-x', _overflow_x);
    Result := Self;
end;

function THtmlStyle.overflow_x: string;
begin
    Result := getStyle('overflow-x');
end;

function THtmlStyle.overflow_y(_overflow_y: string): THtmlStyle;
begin
    setStyle('overflow-y', _overflow_y);
    Result := Self;
end;

function THtmlStyle.overflow_y: string;
begin
    Result := getStyle('overflow-y');
end;

{SCROLLBARS}
function THtmlStyle.scroll_behavior(_scroll_behavior: string): THtmlStyle;
begin
    setStyle('scroll-behavior', _scroll_behavior);
    Result := Self;
end;

function THtmlStyle.scroll_behavior: string;
begin
    Result := getStyle('scroll-behavior');
end;

function THtmlStyle.scroll_margin(_scroll_margin: string): THtmlStyle;
begin
    setStyle('scroll-margin', _scroll_margin);
    Result := Self;
end;

function THtmlStyle.scroll_margin: string;
begin
    Result := getStyle('scroll-margin');
end;

function THtmlStyle.scroll_margin_block(_scroll_margin_block: string): THtmlStyle;
begin
    setStyle('scroll-margin-block', _scroll_margin_block);
    Result := Self;
end;

function THtmlStyle.scroll_margin_block: string;
begin
    Result := getStyle('scroll-margin-block');
end;

function THtmlStyle.scroll_margin_block_end(_scroll_margin_block_end: string):
THtmlStyle;
begin
    setStyle('scroll-margin-block-end', _scroll_margin_block_end);
    Result := Self;
end;

function THtmlStyle.scroll_margin_block_end: string;
begin
    Result := getStyle('scroll-margin-block-end');
end;

function THtmlStyle.scroll_margin_block_start(_scroll_margin_block_start: string):
THtmlStyle;
begin
    setStyle('scroll-margin-block-start', _scroll_margin_block_start);
    Result := Self;
end;

function THtmlStyle.scroll_margin_block_start: string;
begin
    Result := getStyle('scroll-margin-block-start');
end;

function THtmlStyle.scroll_margin_bottom(_scroll_margin_bottom: string): THtmlStyle;
begin
    setStyle('scroll-margin-bottom', _scroll_margin_bottom);
    Result := Self;
end;

function THtmlStyle.scroll_margin_bottom: string;
begin
    Result := getStyle('scroll-margin-bottom');
end;

function THtmlStyle.scroll_margin_inline(_scroll_margin_inline: string): THtmlStyle;
begin
    setStyle('scroll-margin-inline', _scroll_margin_inline);
    Result := Self;
end;

function THtmlStyle.scroll_margin_inline: string;
begin
    Result := getStyle('scroll-margin-inline');
end;

function THtmlStyle.scroll_margin_inline_end(_scroll_margin_inline_end: string):
THtmlStyle;
begin
    setStyle('scroll-margin-inline-end', _scroll_margin_inline_end);
    Result := Self;
end;

function THtmlStyle.scroll_margin_inline_end: string;
begin
    Result := getStyle('scroll-margin-inline-end');
end;

function THtmlStyle.scroll_margin_inline_start(_scroll_margin_inline_start: string):
THtmlStyle;
begin
    setStyle('scroll-margin-inline-start', _scroll_margin_inline_start);
    Result := Self;
end;

function THtmlStyle.scroll_margin_inline_start: string;
begin
    Result := getStyle('scroll-margin-inline-start');
end;

function THtmlStyle.scroll_margin_left(_scroll_margin_left: string): THtmlStyle;
begin
    setStyle('scroll-margin-left', _scroll_margin_left);
    Result := Self;
end;

function THtmlStyle.scroll_margin_left: string;
begin
    Result := getStyle('scroll-margin-left');
end;

function THtmlStyle.scroll_margin_right(_scroll_margin_right: string): THtmlStyle;
begin
    setStyle('scroll-margin-right', _scroll_margin_right);
    Result := Self;
end;

function THtmlStyle.scroll_margin_right: string;
begin
    Result := getStyle('scroll-margin-right');
end;

function THtmlStyle.scroll_margin_top(_scroll_margin_top: string): THtmlStyle;
begin
    setStyle('scroll-margin-top', _scroll_margin_top);
    Result := Self;
end;

function THtmlStyle.scroll_margin_top: string;
begin
    Result := getStyle('scroll-margin-top');
end;

function THtmlStyle.scroll_padding(_scroll_padding: string): THtmlStyle;
begin
    setStyle('scroll-padding', _scroll_padding);
    Result := Self;
end;

function THtmlStyle.scroll_padding: string;
begin
    Result := getStyle('scroll-padding');
end;

function THtmlStyle.scroll_padding_block(_scroll_padding_block: string): THtmlStyle;
begin
    setStyle('scroll-padding-block', _scroll_padding_block);
    Result := Self;
end;

function THtmlStyle.scroll_padding_block: string;
begin
    Result := getStyle('scroll-padding-block');
end;

function THtmlStyle.scroll_padding_block_end(_scroll_padding_block_end: string):
THtmlStyle;
begin
    setStyle('scroll-padding-block-end', _scroll_padding_block_end);
    Result := Self;
end;

function THtmlStyle.scroll_padding_block_end: string;
begin
    Result := getStyle('scroll-padding-block-end');
end;

function THtmlStyle.scroll_padding_block_start(_scroll_padding_block_start: string):
THtmlStyle;
begin
    setStyle('scroll-padding-block-start', _scroll_padding_block_start);
    Result := Self;
end;

function THtmlStyle.scroll_padding_block_start: string;
begin
    Result := getStyle('scroll-padding-block-start');
end;

function THtmlStyle.scroll_padding_bottom(_scroll_padding_bottom: string): THtmlStyle;
begin
    setStyle('scroll-padding-bottom', _scroll_padding_bottom);
    Result := Self;
end;

function THtmlStyle.scroll_padding_bottom: string;
begin
    Result := getStyle('scroll-padding-bottom');
end;

function THtmlStyle.scroll_padding_inline(_scroll_padding_inline: string): THtmlStyle;
begin
    setStyle('scroll-padding-inline', _scroll_padding_inline);
    Result := Self;
end;

function THtmlStyle.scroll_padding_inline: string;
begin
    Result := getStyle('scroll-padding-inline');
end;

function THtmlStyle.scroll_padding_inline_end(_scroll_padding_inline_end: string):
THtmlStyle;
begin
    setStyle('scroll-padding-inline-end', _scroll_padding_inline_end);
    Result := Self;
end;

function THtmlStyle.scroll_padding_inline_end: string;
begin
    Result := getStyle('scroll-padding-inline-end');
end;

function THtmlStyle.scroll_padding_inline_start(_scroll_padding_inline_start: string):
THtmlStyle;
begin
    setStyle('scroll-padding-inline-start', _scroll_padding_inline_start);
    Result := Self;
end;

function THtmlStyle.scroll_padding_inline_start: string;
begin
    Result := getStyle('scroll-padding-inline-start');
end;

function THtmlStyle.scroll_padding_left(_scroll_padding_left: string): THtmlStyle;
begin
    setStyle('scroll-padding-left', _scroll_padding_left);
    Result := Self;
end;

function THtmlStyle.scroll_padding_left: string;
begin
    Result := getStyle('scroll-padding-left');
end;

function THtmlStyle.scroll_padding_right(_scroll_padding_right: string): THtmlStyle;
begin
    setStyle('scroll-padding-right', _scroll_padding_right);
    Result := Self;
end;

function THtmlStyle.scroll_padding_right: string;
begin
    Result := getStyle('scroll-padding-right');
end;

function THtmlStyle.scroll_padding_top(_scroll_padding_top: string): THtmlStyle;
begin
    setStyle('scroll-padding-top', _scroll_padding_top);
    Result := Self;
end;

function THtmlStyle.scroll_padding_top: string;
begin
    Result := getStyle('scroll-padding-top');
end;

function THtmlStyle.scroll_snap_align(_scroll_snap_align: string): THtmlStyle;
begin
    setStyle('scroll-snap-align', _scroll_snap_align);
    Result := Self;
end;

function THtmlStyle.scroll_snap_align: string;
begin
    Result := getStyle('scroll-snap-align');
end;

function THtmlStyle.scroll_snap_stop(_scroll_snap_stop: string): THtmlStyle;
begin
    setStyle('scroll-snap-stop', _scroll_snap_stop);
    Result := Self;
end;

function THtmlStyle.scroll_snap_stop: string;
begin
    Result := getStyle('scroll-snap-stop');
end;

function THtmlStyle.scroll_snap_type(_scroll_snap_type: string): THtmlStyle;
begin
    setStyle('scroll-snap-type', _scroll_snap_type);
    Result := Self;
end;

function THtmlStyle.scroll_snap_type: string;
begin
    Result := getStyle('scroll-snap-type');
end;

function THtmlStyle.scrollbar_color(_scrollbar_color: string): THtmlStyle;
begin
    setStyle('scrollbar-color', _scrollbar_color);
    Result := Self;
end;

function THtmlStyle.scrollbar_color: string;
begin
    Result := getStyle('scrollbar-color');
end;

function THtmlStyle.scrollbar_width(_scrollbar_width: string): THtmlStyle;
begin
    setStyle('scrollbar-width', _scrollbar_width);
    Result := Self;
end;

function THtmlStyle.scrollbar_width: string;
begin
    Result := getStyle('scrollbar-width');
end;

{TEXT - Getters and setters}
function THtmlStyle.text_align(_text_align: string): THtmlStyle;
begin
    setStyle('text-align', _text_align);
    Result := Self;
end;

function THtmlStyle.text_align_last(_text_align_last: string): THtmlStyle;
begin
    setStyle('text-align-last', _text_align_last);
    Result := Self;
end;

function THtmlStyle.text_combine_upright(_text_combine_upright: string): THtmlStyle;
begin
    setStyle('text-combine-upright', _text_combine_upright);
    Result := Self;
end;

function THtmlStyle.text_decoration(_text_decoration: string): THtmlStyle;
begin
    setStyle('text-decoration', _text_decoration);
    Result := Self;
end;

function THtmlStyle.text_decoration_color(_text_decoration_color: string): THtmlStyle;
begin
    setStyle('text-decoration-color', _text_decoration_color);
    Result := Self;
end;

function THtmlStyle.text_decoration_line(_text_decoration_line: string): THtmlStyle;
begin
    setStyle('text-decoration-line', _text_decoration_line);
    Result := Self;
end;

function THtmlStyle.text_decoration_style(_text_decoration_style: string): THtmlStyle;
begin
    setStyle('text-decoration-style', _text_decoration_style);
    Result := Self;
end;

function THtmlStyle.text_decoration_thickness(_text_decoration_thickness:
    string): THtmlStyle;
begin
    setStyle('text-decoration-thickness', _text_decoration_thickness);
    Result := Self;
end;

function THtmlStyle.text_emphasis(_text_emphasis: string): THtmlStyle;
begin
    setStyle('text-emphasis', _text_emphasis);
    Result := Self;
end;

function THtmlStyle.text_emphasis_color(_text_emphasis_color: string): THtmlStyle;
begin
    setStyle('text-emphasis-color', _text_emphasis_color);
    Result := Self;
end;

function THtmlStyle.text_emphasis_position(_text_emphasis_position: string): THtmlStyle;
begin
    setStyle('text-emphasis-position', _text_emphasis_position);
    Result := Self;
end;

function THtmlStyle.text_emphasis_style(_text_emphasis_style: string): THtmlStyle;
begin
    setStyle('text-emphasis-style', _text_emphasis_style);
    Result := Self;
end;

function THtmlStyle.text_indent(_text_indent: string): THtmlStyle;
begin
    setStyle('text-indent', _text_indent);
    Result := Self;
end;

function THtmlStyle.text_justify(_text_justify: string): THtmlStyle;
begin
    setStyle('text-justify', _text_justify);
    Result := Self;
end;

function THtmlStyle.text_orientation(_text_orientation: string): THtmlStyle;
begin
    setStyle('text-orientation', _text_orientation);
    Result := Self;
end;

function THtmlStyle.text_overflow(_text_overflow: string): THtmlStyle;
begin
    setStyle('text-overflow', _text_overflow);
    Result := Self;
end;

function THtmlStyle.text_rendering(_text_rendering: string): THtmlStyle;
begin
    setStyle('text-rendering', _text_rendering);
    Result := Self;
end;

function THtmlStyle.text_shadow(_text_shadow: string): THtmlStyle;
begin
    setStyle('text-shadow', _text_shadow);
    Result := Self;
end;

function THtmlStyle.text_transform(_text_transform: string): THtmlStyle;
begin
    setStyle('text-transform', _text_transform);
    Result := Self;
end;

function THtmlStyle.text_underline_offset(_text_underline_offset: string): THtmlStyle;
begin
    setStyle('text-underline-offset', _text_underline_offset);
    Result := Self;
end;

function THtmlStyle.text_underline_position(_text_underline_position: string): THtmlStyle;
begin
    setStyle('text-underline-position', _text_underline_position);
    Result := Self;
end;

function THtmlStyle.text_align: string;
begin
    Result := getStyle('text-align');
end;

function THtmlStyle.text_align_last: string;
begin
    Result := getStyle('text-align-last');
end;

function THtmlStyle.text_combine_upright: string;
begin
    Result := getStyle('text-combine-upright');
end;

function THtmlStyle.text_decoration: string;
begin
    Result := getStyle('text-decoration');
end;

function THtmlStyle.text_decoration_color: string;
begin
    Result := getStyle('text-decoration-color');
end;

function THtmlStyle.text_decoration_line: string;
begin
    Result := getStyle('text-decoration-line');
end;

function THtmlStyle.text_decoration_style: string;
begin
    Result := getStyle('text-decoration-style');
end;

function THtmlStyle.text_decoration_thickness: string;
begin
    Result := getStyle('text-decoration-thickness');
end;

function THtmlStyle.text_emphasis: string;
begin
    Result := getStyle('text-emphasis');
end;

function THtmlStyle.text_emphasis_color: string;
begin
    Result := getStyle('text-emphasis-color');
end;

function THtmlStyle.text_emphasis_position: string;
begin
    Result := getStyle('text-emphasis-position');
end;

function THtmlStyle.text_emphasis_style: string;
begin
    Result := getStyle('text-emphasis-style');
end;

function THtmlStyle.text_indent: string;
begin
    Result := getStyle('text-indent');
end;

function THtmlStyle.text_justify: string;
begin
    Result := getStyle('text-justify');
end;

function THtmlStyle.text_orientation: string;
begin
    Result := getStyle('text-orientation');
end;

function THtmlStyle.text_overflow: string;
begin
    Result := getStyle('text-overflow');
end;

function THtmlStyle.text_rendering: string;
begin
    Result := getStyle('text-rendering');
end;

function THtmlStyle.text_shadow: string;
begin
    Result := getStyle('text-shadow');
end;

function THtmlStyle.text_transform: string;
begin
    Result := getStyle('text-transform');
end;

function THtmlStyle.text_underline_offset: string;
begin
    Result := getStyle('text-underline-offset');
end;

function THtmlStyle.text_underline_position: string;
begin
    Result := getStyle('text-underline-position');
end;


{TRANSITION Functions}
function THtmlStyle.transition(_transition: string): THtmlStyle;
begin
    setStyle('transition', _transition);
    Result := Self;
end;

function THtmlStyle.transition_delay(_transition_delay: string): THtmlStyle;
begin
    setStyle('transition-delay', _transition_delay);
    Result := Self;
end;

function THtmlStyle.transition_duration(_transition_duration: string): THtmlStyle;
begin
    setStyle('transition-duration', _transition_duration);
    Result := Self;
end;

function THtmlStyle.transition_property(_transition_property: string): THtmlStyle;
begin
    setStyle('transition-property', _transition_property);
    Result := Self;
end;

function THtmlStyle.transition_timing_function(_transition_timing_function: string):
THtmlStyle;
begin
    setStyle('transition-timing-function', _transition_timing_function);
    Result := Self;
end;

function THtmlStyle.transition: string;
begin
    Result := getStyle('transition');
end;

function THtmlStyle.transition_delay: string;
begin
    Result := getStyle('transition-delay');
end;

function THtmlStyle.transition_duration: string;
begin
    Result := getStyle('transition-duration');
end;

function THtmlStyle.transition_property: string;
begin
    Result := getStyle('transition-property');
end;

function THtmlStyle.transition_timing_function: string;
begin
    Result := getStyle('transition-timing-function');
end;


function THtmlStyle.display(_display: string): THtmlStyle;
begin
    setStyle('display', _display);
    Result := Self;
end;

function THtmlStyle.align(_align: string): THtmlStyle;
begin
    setStyle('align', _align);
    Result := Self;
end;

function THtmlStyle.position(_position: string): THtmlStyle;
begin
    setStyle('position', _position);
    Result := Self;
end;

function THtmlStyle.Width(_width: string): THtmlStyle;
begin
    setStyle('width', _width);
    Result := Self;
end;

function THtmlStyle.Height(_height: string): THtmlStyle;
begin
    setStyle('height', _height);
    Result := Self;
end;

function THtmlStyle.min_width(_min_width: string): THtmlStyle;
begin
    setStyle('min-width', _min_width);
    Result := Self;
end;

function THtmlStyle.min_height(_min_height: string): THtmlStyle;
begin
    setStyle('min-height', _min_height);
    Result := Self;
end;

function THtmlStyle.max_width(_max_width: string): THtmlStyle;
begin
    setStyle('max-width', _max_width);
    Result := Self;
end;

function THtmlStyle.max_height(_max_height: string): THtmlStyle;
begin
    setStyle('max-height', _max_height);
    Result := Self;
end;

function THtmlStyle.float(_float: string): THtmlStyle;
begin
    setStyle('float', _float);
    Result := Self;
end;

function THtmlStyle.flexbox(_flexbox: string): THtmlStyle;
begin
    setStyle('flexbox', _flexbox);
    Result := Self;
end;

function THtmlStyle.flex(_flex: string): THtmlStyle;
begin
    setStyle('flex', _flex);
    Result := Self;
end;

function THtmlStyle.flex: string;
begin
    Result := getStyle('flex');
end;

function THtmlStyle.flex_basis(_flex_basis: string): THtmlStyle;
begin
    setStyle('flex-basis', _flex_basis);
    Result := Self;
end;

function THtmlStyle.flex_basis: string;
begin
    Result := getStyle('flex-basis');
end;

function THtmlStyle.flex_direction(_flex_direction: string): THtmlStyle;
begin
    setStyle('flex-direction', _flex_direction);
    Result := Self;
end;

function THtmlStyle.flex_direction: string;
begin
    Result := getStyle('flex-direction');
end;

function THtmlStyle.flex_flow(_flex_flow: string): THtmlStyle;
begin
    setStyle('flex-flow', _flex_flow);
    Result := Self;
end;

function THtmlStyle.flex_flow: string;
begin
    Result := getStyle('flex-flow');
end;

function THtmlStyle.flex_grow(_flex_grow: string): THtmlStyle;
begin
    setStyle('flex-grow', _flex_grow);
    Result := Self;
end;

function THtmlStyle.flex_grow: string;
begin
    Result := getStyle('flex-grow');
end;

function THtmlStyle.flex_shrink(_flex_shrink: string): THtmlStyle;
begin
    setStyle('flex-shrink', _flex_shrink);
    Result := Self;
end;

function THtmlStyle.flex_shrink: string;
begin
    Result := getStyle('flex-shrink');
end;

function THtmlStyle.flex_wrap(_flex_wrap: string): THtmlStyle;
begin
    setStyle('flex-wrap', _flex_wrap);
    Result := Self;
end;

function THtmlStyle.flex_wrap: string;
begin
    Result := getStyle('flex-wrap');
end;


function THtmlStyle.cssgrid(_cssgrid: string): THtmlStyle;
begin
    setStyle('cssgrid', _cssgrid);
    Result := Self;
end;

function THtmlStyle.display: string;
begin
    Result := getStyle('display');
end;

function THtmlStyle.align: string;
begin
    Result := getStyle('align');
end;

function THtmlStyle.position: string;
begin
    Result := getStyle('position');
end;

function THtmlStyle.Width: string;
begin
    Result := getStyle('width');
end;

function THtmlStyle.Height: string;
begin
    Result := getStyle('height');
end;

function THtmlStyle.min_width: string;
begin
    Result := getStyle('min-width');
end;

function THtmlStyle.min_height: string;
begin
    Result := getStyle('min-height');
end;

function THtmlStyle.max_width: string;
begin
    Result := getStyle('max-width');
end;

function THtmlStyle.max_height: string;
begin
    Result := getStyle('max-height');
end;

function THtmlStyle.float: string;
begin
    Result := getStyle('float');
end;

function THtmlStyle.flexbox: string;
begin
    Result := getStyle('flexbox');
end;

function THtmlStyle.cssgrid: string;
begin
    Result := getStyle('cssgrid');
end;

function THtmlStyle.color(_color: string): THtmlStyle;
begin
    setStyle('color', _color);
    Result := self;
end;

function THtmlStyle.color: string;
begin
    Result := getStyle('color');
end;

{ THtmlSection }

constructor THtmlSection.Create;
begin
    inherited Create;
    tag := 'section';
end;

{ THtmlButtonTypeHelper }

function THtmlButtonTypeHelper.toString: string;
begin
    case self of
        btnButton: Result := 'button';
        btnSubmit: Result := 'submit';
        btnReset: Result := 'reset';
    end;
end;

procedure THtmlButtonTypeHelper.fromString(_str: string);
begin
    case LowerCase(_str) of
        'button': self := btnButton;
        'submit': self := btnSubmit;
        'reset': self := btnReset;
    end;
end;

{ THtmlButton }

constructor THtmlButton.Create;
begin
    inherited Create;
    tag := 'button';
    buttonType(btnButton);
end;

function THtmlButton.Name(_name: string): THtmlButton;
begin
    Result := THtmlButton(inherited setName(_name));
end;

function THtmlButton.setID(_id: string): THtmlButton;
begin
    Result := THtmlButton(inherited setID(_id));
end;

function THtmlButton.buttonType(_btnType: THtmlButtonType): THtmlButton;
begin
    Result := self;
    setAttr('type', _btnType.toString);
end;

function THtmlButton.autofocus: THtmlButton;
begin
    Result := self;
    setAttrFlag('autofocus');
end;

function THtmlButton.disabled: THtmlButton;
begin
    Result := self;
    setAttrFlag('disabled');
end;

function THtmlButton.form(_formID: string): THtmlButton;
begin
    Result := self;
    setAttr('form', _formID);
end;

function THtmlButton.form(_formObj: THtmlForm): THtmlButton;
var
    _enctype: THtmlFormEncType = THtmlFormEncType(0);
    _formMethod: THtmlFormMethod = THtmlFormMethod(0);
    _formTarget: THtmlATarget = THtmlATarget(0);
begin

    Result := self;
    _enctype.fromString(_formObj.getEncType);
    _formMethod.fromString(_formObj.getMethod);
    _formTarget.fromString(_formObj.getTarget);
    self.form(_formObj.getID);
    self.formAction(_formObj.getAction);
    self.formenctype(_enctype);
    self.formMethod(_formMethod);
    self.formTarget(_formTarget);
end;

function THtmlButton.formAction(_formAction: string): THtmlButton;
begin
    Result := self;
    setAttr('formaction', _formAction);
end;

function THtmlButton.formenctype(_enctype: THtmlFormEncType): THtmlButton;
begin
    Result := self;
    setAttr('formenctype', _enctype.toString);
end;

function THtmlButton.formMethod(_formMethod: THtmlFormMethod): THtmlButton;
begin
    Result := self;
    setAttr('formmethod', _formMethod.toString);
end;

function THtmlButton.formNoValidate: THtmlButton;
begin
    Result := self;
    setAttrFlag('formnovalidate');
end;

function THtmlButton.formTarget(_target: THtmlATarget): THtmlButton;
begin
    Result := self;
    setAttr('formtarget', _target.toString);
end;

function THtmlButton.onClickLoad(_url: string): THtmlButton;
begin
    Result:= onClick(Format('location.href=''%s''', [_url]));
end;

function THtmlButton.onClick(_script: string): THtmlButton;
begin
    Result:= self;
    setAttr('onclick', _script);
end;

{ THtmlFormMethodHelper }

function THtmlFormMethodHelper.toString: string;
begin
    case self of
        formPost: Result := 'POST';
        formGet: Result := 'GET';
    end;
end;

procedure THtmlFormMethodHelper.fromString(_str: string);
begin
    case UpperCase(_str) of
        'POST': self := formPost;
        'GET': self := formGet;
    end;
end;

{ THtlmATargetHelper }

function THtmlATargetHelper.toString: string;
begin
    case self of
        target_blank: Result := '_blank';
        target_parent: Result := '_parent';
        target_self: Result := '_self';
        target_top: Result := '_top';
        target_frame: Result := '_frame';
    end;
end;

function THtmlATargetHelper.fromString(_str: string): THtmlATarget;
begin
    case LowerCase(_str) of
        '_blank': self := target_blank;
        '_parent': self := target_parent;
        '_self': self := target_self;
        '_top': self := target_top;
        '_frame': self := target_frame;
    end;
    Result := self;
end;

{ THtmlStyleSheetLink }

constructor THtmlStyleSheetLink.Create;
begin
    inherited Create;
    _type := 'text/css';
    rel := 'stylesheet';
end;


{ THtmlViewPort }

function THtmlViewPort.Render(_level: QWord): string;
var
    _content: string;
    swidth: string;
    sUserScalable: string;
    _formatSettings: TFormatSettings;
begin
    _formatSettings := FormatSettings;
    _formatSettings.DecimalSeparator:='.';
    {<meta setName="viewport"
      content="width=device-width,
                initial-scale=1.0,
                minimum-scale=0.5,
                maximum-scale=3.0, user-scalable=yes">}

    if myWidth = 0 then
        sWidth := 'device-width'
    else
        sWidth := myWidth.ToString;
    if myUserScalable then
        sUserScalable := 'yes'
    else
        sUserScalable := 'no';

    {full definition}
    {_content:= Format(
        'width=%s, initial-scale=%.1f, minimum-scale=%.1f, maximum-scale=%.1f, user-scalabe=%s',
        [sWidth, myInitialScale, myMinimumScale, myMaximumScale, myUserScalable]);}

    {basic definition}
    {_content:= Format(
        'width=%s, initial-scale=%.1f, user-scalable=%s',
        [sWidth, myInitialScale, sUserScalable]);}

    {minimum definition}
    _content := Format('width=%s, initial-scale=%s', [sWidth, FormatFloat('0.0', myInitialScale, _formatSettings)]);

    self.tagName := 'viewport';
    self.content := _content;

    Result := inherited;
end;

constructor THtmlViewPort.Create;
begin
    inherited Create;
    myWidth := 0;
    myHeight := 0;
    myInitialScale := 1.0;
    myMinimumScale := 0.5;
    myMaximumScale := 3.0;
    myUserScalable := True;
end;

function THtmlViewPort.Width(_width: integer): THtmlViewPort;
begin
    mywidth := _width;
    Result := Self;
end;

function THtmlViewPort.Height(_height: integer): THtmlViewPort;
begin
    myheight := _height;
    Result := Self;
end;

function THtmlViewPort.initial_scale(_initial_scale: single): THtmlViewPort;
begin
    myInitialScale := _initial_scale;
    Result := Self;
end;

function THtmlViewPort.minimum_scale(_min_scale: single): THtmlViewPort;
begin
    myMinimumScale := _min_scale;
    Result := Self;
end;

function THtmlViewPort.maximum_scale(_max_scale: single): THtmlViewPort;
begin
    myMaximumScale := _max_scale;
    Result := Self;
end;

function THtmlViewPort.user_scalable(_user_scalable: boolean): THtmlViewPort;
begin
    myUserScalable := _user_scalable;
    Result := Self;
end;

{ THtmlFormEncTypeHelper }

function THtmlFormEncTypeHelper.toString: string;
begin
    case self of
        form_urlencoded: Result := 'application/x-www-form-urlencoded';
        form__multipart_formdata: Result := 'multipart/form-data';
        form_text_plain: Result := 'text/plain';
        else
            Result := '';
    end;
end;

procedure THtmlFormEncTypeHelper.fromString(_str: string);
begin
    case LowerCase(_str) of
        'application/x-www-form-urlencoded': Self := form_urlencoded;
        'multipart/form-data': Self := form__multipart_formdata;
        'text/plain': Self := form_text_plain;
        else
            Self := form_urlencoded; {default}
    end;
end;

{ THtmlAnchor }

function THtmlAnchor.getCoords: string;
begin
    Result := FAttributes.Values['coords'];
end;

function THtmlAnchor.getDownload: string;
begin
    Result := FAttributes.Values['download'];
end;

function THtmlAnchor.getHref: string;
begin
    Result := FAttributes.Values['href'];
end;

function THtmlAnchor.getHRefLang: string;
begin
    Result := FAttributes.Values['hreflang'];
end;

function THtmlAnchor.getHtmlRel: THtmlARel;
begin
    Result := THtmlARel(0);
    Result.fromString(FAttributes.Values['rel']);  // $
end;

function THtmlAnchor.getMedia: string;
begin
    Result := FAttributes.Values['media'];
end;

function THtmlAnchor.getMediaType: string;
begin
    Result := FAttributes.Values['type'];
end;

function THtmlAnchor.getPing: string;
begin
    Result := FAttributes.Values['ping'];
end;

function THtmlAnchor.getReferrerPolicy: THtmlAReferrerPolicy;
begin
    Result := THtmlAReferrerPolicy(0);
    Result.fromString(FAttributes.Values['referrerpolicy']); // $
end;

function THtmlAnchor.getRev: string;
begin
    Result := FAttributes.Values['rev'];
end;

function THtmlAnchor.getTarget: THtmlATarget;
begin
    Result := THtmlATarget(0);
    Result.fromString(FAttributes.Values['target']); // $
end;

procedure THtmlAnchor.setCoords(AValue: string);
begin
    FAttributes.Values['coords'] := AValue;
end;

procedure THtmlAnchor.setDownload(AValue: string);
begin
    FAttributes.Values['download'] := AValue;
end;

procedure THtmlAnchor.setHref(AValue: string);
begin
    FAttributes.Values['href'] := AValue;
end;

procedure THtmlAnchor.setHRefLang(AValue: string);
begin
    FAttributes.Values['hreflang'] := AValue;
end;

procedure THtmlAnchor.setHtmlRel(AValue: THtmlARel);
begin
    FAttributes.Values['rel'] := AValue.toString;
end;

procedure THtmlAnchor.setMedia(AValue: string);
begin
    FAttributes.Values['media'] := AValue;
end;

procedure THtmlAnchor.setMediaType(AValue: string);
begin
    FAttributes.Values['type'] := AValue;
end;

procedure THtmlAnchor.setPing(AValue: string);
begin
    FAttributes.Values['ping'] := AValue;
end;

procedure THtmlAnchor.setReferrerPolicy(AValue: THtmlAReferrerPolicy);
begin
    FAttributes.Values['referrerpolicy'] := AValue.toString;
end;

procedure THtmlAnchor.setRev(AValue: string);
begin
    FAttributes.Values['rev'] := AValue;
end;

procedure THtmlAnchor.setTarget(AValue: THtmlATarget);
begin
    FAttributes.Values['target'] := AValue.toString;
end;

procedure THtmlAnchor.Settargetframe(AValue: string);
begin
    if Ftargetframe = AValue then
        Exit;
    Ftargetframe := AValue;
    FAttributes.Values['target'] := Ftargetframe;
end;

constructor THtmlAnchor.Create;
begin
    inherited Create;
    tag := 'a';
end;

{ THtmlLink }

function THtmlLink.getHRef: string;
begin
    Result := FAttributes.Values['href'];
end;

function THtmlLink.getMedia: string;
begin
    Result := FAttributes.Values['media'];
end;

function THtmlLink.getRel: string;
begin
    Result := FAttributes.Values['rel'];
end;

function THtmlLink.getType: string;
begin
    Result := FAttributes.Values['type'];
end;

procedure THtmlLink.setHRef(AValue: string);
begin
    FAttributes.Values['href'] := AValue;
end;

procedure THtmlLink.setMedia(AValue: string);
begin
    FAttributes.Values['media'] := AValue;
end;

procedure THtmlLink.setRel(AValue: string);
begin
    FAttributes.Values['rel'] := AValue;
end;

procedure THtmlLink.setType(AValue: string);
begin
    FAttributes.Values['type'] := AValue;
end;

function THtmlLink.tag_start: string;
var
    s: string;
begin
    //Result:= inherited tag_start;

    // Replace the last '>' with '/>'
    s := inherited tag_start;
    Result := copy(s, 1, Length(s) - 1) + '/>';
end;


constructor THtmlLink.Create;
begin
    inherited Create;
    suppressID:= true;
    tag := 'link';
    hasEndTag := False;
end;

{ THtmlLabel }

function THtmlLabel.getFor: string;
begin
    Result := FAttributes.Values['for'];
end;

function THtmlLabel.getForm: string;
begin
    Result := FAttributes.Values['form'];
end;

procedure THtmlLabel.setFFor(AValue: string);
begin
    FAttributes.Values['for'] := AValue;
end;

procedure THtmlLabel.setFForm(AValue: string);
begin
    FAttributes.Values['form'] := AValue;
end;

function THtmlLabel.Render(_level: QWord): string;
begin
    if for_.isEmpty then
        rmAttr('for');
    if form.isEmpty then
        rmAttr('form');
	Result:=inherited;
end;

constructor THtmlLabel.Create;
begin
    inherited Create;
    tag := 'label';
end;

function THtmlLabel.setFor(_for: string): THtmlLabel;
begin
    for_ := _for;
    Result := Self;
end;

function THtmlLabel.setForm(_form: string): THtmlLabel;
begin
    form := _form;
    Result := Self;
end;

{ THtmlInput }

function THtmlInput.getAccept: string;
begin
    Result := FAttributes.Values['accept'];
end;

function THtmlInput.getAlign: string;
begin
    Result := FAttributes.Values['align'];
end;

function THtmlInput.getAlt: string;
begin
    Result := FAttributes.Values['alt'];
end;

function THtmlInput.getAutoComplete: string;
begin
    Result := getAttr('autocomplete');
end;

function THtmlInput.getAutoFocus: boolean;
begin
    Result := findAttrFlag('autofocus');
end;

function THtmlInput.getChecked: boolean;
begin
    Result := findAttrFlag('checked');
end;

function THtmlInput.getDirName: string;
begin
    Result := FAttributes.Values['dirname'];
end;

function THtmlInput.getDisabled: boolean;
begin
    Result := findAttrFlag('disabled');
end;

function THtmlInput.getForm: string;
begin
    Result := FAttributes.Values['form'];
end;

function THtmlInput.getFormAction: string;
begin
    Result := FAttributes.Values['formaction'];
end;

function THtmlInput.getFormEncType: string;
begin
    Result := FAttributes.Values['formenctype'];
end;

function THtmlInput.getFormMethod: string;
begin
    Result := FAttributes.Values['formmethod'];
end;

function THtmlInput.getFormNoValidate: boolean;
begin
    Result := findAttrFlag('formnovalidate');
end;

function THtmlInput.getFormTarget: string;
begin
    Result := FAttributes.Values['formtarget'];
end;

function THtmlInput.getHeight: string;
begin
    Result := FAttributes.Values['height'];
end;

function THtmlInput.getInputType: THtmlInputType;
begin
    Result := THtmlInputType(0);
    Result.fromString(FAttributes.Values['type']);
end;

function THtmlInput.getLabel: string;
begin
    Result := FLabel.Text;
end;

function THtmlInput.getList: string;
begin
    Result := FAttributes.Values['list'];
end;

function THtmlInput.getMax: string;
begin
    Result := FAttributes.Values['max'];
end;

function THtmlInput.getMaxLength: string;
begin
    Result := FAttributes.Values['maxlength'];
end;

function THtmlInput.getMin: string;
begin
    Result := FAttributes.Values['min'];
end;

function THtmlInput.getMultiple: boolean;
begin
    Result := findAttrFlag('multiple');
end;

function THtmlInput.getPattern: string;
begin
    Result := FAttributes.Values['pattern'];
end;

function THtmlInput.getPlaceHolder: string;
begin
    Result := FAttributes.Values['placeholder'];
end;

function THtmlInput.getReadOnly: boolean;
begin
    Result := findAttrFlag('readonly');
end;

function THtmlInput.getRequired: boolean;
begin
    Result := findAttrFlag('required');
end;

function THtmlInput.getSrc: string;
begin
    Result := FAttributes.Values['src'];
end;

function THtmlInput.getStep: string;
begin
    Result := FAttributes.Values['step'];
end;

function THtmlInput.getValue: string;
begin
    Result := FAttributes.Values['value'];
end;

function THtmlInput.getWidth: string;
begin
    Result := FAttributes.Values['width'];
end;

procedure THtmlInput.setAccept(AValue: string);
begin
    FAttributes.Values['accept'] := AValue;
end;

procedure THtmlInput.setAlign(AValue: string);
begin
    FAttributes.Values['align'] := AValue;
end;

procedure THtmlInput.setAlt(AValue: string);
begin
    FAttributes.Values['alt'] := AValue;
end;

procedure THtmlInput.setAutoComplete(AValue: string);
begin
    FAttributes.Values['autocomplete'] := AValue;
end;

procedure THtmlInput.setAutofocus(AValue: boolean);
begin
    setAttrFlag('autofocus', AValue);
end;

procedure THtmlInput.setChecked(AValue: boolean);
begin
    setAttrFlag('checked', AValue);
end;

procedure THtmlInput.setDirName(AValue: string);
begin
    FAttributes.Values['dirname'] := AValue;
end;

procedure THtmlInput.setDisabled(AValue: boolean);
begin
    setAttrFlag('disabled', AValue);
end;

procedure THtmlInput.setForm(AValue: string);
begin
    FAttributes.Values['form'] := AValue;
end;

procedure THtmlInput.setFormAction(AValue: string);
begin
    FAttributes.Values['formaction'] := AValue;
end;

procedure THtmlInput.setFormEncType(AValue: string);
begin
    FAttributes.Values['formenctype'] := AValue;
end;

procedure THtmlInput.setFormMethod(AValue: string);
begin
    FAttributes.Values['formmethod'] := AValue;
end;

procedure THtmlInput.setFormNoValidate(AValue: boolean);
begin
    setAttrFlag('formnovalidate', AValue);
end;

procedure THtmlInput.setFormTarget(AValue: string);
begin
    FAttributes.Values['formtarget'] := AValue;
end;

procedure THtmlInput.setHeight(AValue: string);
begin
    FAttributes.Values['height'] := AValue;
end;

procedure THtmlInput.setInputType(AValue: THtmlInputType);
begin
    FAttributes.Values['type'] := AValue.toString;
end;

procedure THtmlInput.setLabel(AValue: string);
begin
    FLabel.Text := AValue;
    FLabel.for_ := Self.TagID;
end;

procedure THtmlInput.setList(AValue: string);
begin
    FAttributes.Values['list'] := AValue;
end;

procedure THtmlInput.setMax(AValue: string);
begin
    FAttributes.Values['max'] := AValue;
end;

procedure THtmlInput.setMaxLength(AValue: string);
begin
    FAttributes.Values['maxlength'] := AValue;
end;

procedure THtmlInput.setMin(AValue: string);
begin
    FAttributes.Values['min'] := AValue;
end;

procedure THtmlInput.setMultiple(AValue: boolean);
begin
    setAttrFlag('multiple', AValue);
end;

procedure THtmlInput.setPattern(AValue: string);
begin
    FAttributes.Values['pattern'] := AValue;
end;

procedure THtmlInput.setPlaceHolder(AValue: string);
begin
    FAttributes.Values['placeholder'] := AValue;
end;

procedure THtmlInput.setReadOnly(AValue: boolean);
begin
    setAttrFlag('readonly', AValue);
end;

procedure THtmlInput.setRequired(const _required: boolean);
begin
    setAttrFlag('required', _required);
end;

procedure THtmlInput.setSrc(AValue: string);
begin
    FAttributes.Values['src'] := AValue;
end;

procedure THtmlInput.setStep(AValue: string);
begin
    FAttributes.Values['step'] := AValue;
end;

procedure THtmlInput.setValue(AValue: string);
begin
    FAttributes.Values['value'] := AValue;
end;

procedure THtmlInput.setWidth(AValue: string);
begin
    FAttributes.Values['width'] := AValue;
end;

function THtmlInput.getSize: string;
begin
    Result := getAttr('size');
end;

procedure THtmlInput.setSize(const _size: string);
begin
    setAttr('size', _size);
end;

procedure THtmlInput.setlabelPosition(const _labelPosition: THtmlLabelPosition);
begin
	if mylabelPosition=_labelPosition then Exit;
	mylabelPosition:=_labelPosition;
end;

function THtmlInput.setMyBoundLabel(const _boundLabel: boolean): THtmlInput;
begin
    Result:= Self;
	if myBoundLabel=_boundLabel then Exit;
	myBoundLabel:=_boundLabel;
end;

function THtmlInput.Render(_level: QWord): string;
begin
    Result := inherited;
    if not FLabel.Text.isEmpty then
    begin
        if not myBoundLabel then
            FLabel.for_:= '';
        case mylabelPosition of
            labelBefore: Result := FLabel.Html + Result;
            labelAfter:  Result := Result + FLabel.Html;
            labelAbove: Result := FLabel.Html + Result;
            labelBelow: Result := Result + FLabel.Html;
        end;
	end;
end;

constructor THtmlInput.Create;
begin
    FLabel := THtmlLabel.Create; {so that FLabel is created when setID is called}
    inherited Create;
    tag := 'input';
    hasEndTag := False;
    myBoundLabel := true;
end;

destructor THtmlInput.Destroy;
begin
    FLabel.Destroy;
    inherited Destroy;
end;

function THtmlInput.label_(_labelObj: THtmlLabel): THtmlInput;
begin
    if Assigned(FLabel) then
        FreeAndNil(FLabel);

    FLabel := _labelObj;
    FLabel.for_ := Self.TagID;

    Result := Self;
end;

function THtmlInput.copyFrom(_source: THtmlElement): THtmlElement;
begin
    if _source is THtmlInput then
        FLabel.copyFrom(THtmlInput(_source).FLabel);
    Result := inherited copyFrom(_source);
end;

function THtmlInput.setId(_id: string): THtmlElement;
begin
	Result:=inherited setId(_id);
    FLabel.for_:= _id;
end;

{ THtmlForm }

function THtmlForm.getAcceptCharSet: string;
begin
    Result := FAttributes.Values['accept-charset'];
end;

function THtmlForm.getAction: string;
begin
    Result := FAttributes.Values['action'];
end;

function THtmlForm.getAutoComplete: string;
begin
    Result := FAttributes.Values['autocomplete'];
end;

function THtmlForm.getEncType: string;
begin
    Result := FAttributes.Values['enctype'];
end;

function THtmlForm.getMethod: string;
begin
    Result := FAttributes.Values['method'];
end;

function THtmlForm.getNoValidate: string;
begin
    Result := FAttributes.Values['novalidate'];
end;

function THtmlForm.getTarget: string;
begin
    Result := FAttributes.Values['target'];
end;

procedure THtmlForm.setAcceptCharSet(AValue: string);
begin
    FAttributes.Values['accept-charset'] := AValue;
end;

procedure THtmlForm.setAction(AValue: string);
begin
    FAttributes.Values['action'] := AValue;
end;

procedure THtmlForm.setAutoComplete(AValue: string);
begin
    FAttributes.Values['autocomplete'] := AValue;
end;

procedure THtmlForm.setEncType(AValue: string);
begin
    FAttributes.Values['enctype'] := AValue;
end;

procedure THtmlForm.setMethod(AValue: string);
begin
    FAttributes.Values['method'] := AValue;
end;

procedure THtmlForm.setNoValidate(AValue: string);
begin
    FAttributes.Values['novalidate'] := AValue;
end;

procedure THtmlForm.setTarget(AValue: string);
begin
    FAttributes.Values['target'] := AValue;
end;

constructor THtmlForm.Create;
begin
    inherited Create;
    tag := 'form';
end;

function THtmlForm.method(_m: string): THtmlForm;
begin
    _method := _m;
    Result := Self;
end;

function THtmlForm.target(_t: string): THtmlForm;
begin
    _target := _t;
    Result := Self;
end;

function THtmlForm.action(_a: string): THtmlForm;
begin
    _action := _a;
    Result := Self;
end;

function THtmlForm.enctype(_a: string): THtmlForm;
begin
    _enctype := _a;
    Result := Self;
end;

function THtmlForm.novalidate(_nv: string): THtmlForm;
begin
    _novalidate := _nv;
    Result := Self;
end;

function THtmlForm.autocomplete(_ac: string): THtmlForm;
begin
    _autocomplete := _ac;
    Result := self;
end;

function THtmlForm.hidden(_name: string; _value: string): THtmlForm;
begin
    with add(THtmlInput.Create) as THtmlInput do
    begin
        input_type:= inputHidden;
        tagID:= _name;
        setName(_name);
        Value:= _value;
	end;
end;


{ THTMLMetaTag }

function THTMLMetaTag.getContent: string;
begin
    Result := FAttributes.Values['content'];
end;

function THTMLMetaTag.getHttpEquiv: string;
begin
    Result := FAttributes.Values['http-equiv'];
end;

procedure THTMLMetaTag.setFContent(AValue: string);
begin
    FAttributes.Values['content'] := AValue;
end;

procedure THTMLMetaTag.setFHttpEquiv(AValue: string);
begin
    FAttributes.Values['http-equiv'] := AValue;
end;

constructor THTMLMetaTag.Create;
begin
    inherited Create;
    suppressID:= true;
    tag := 'meta';
    hasEndTag := False;
end;

function THTMLMetaTag.setContent(_content: string): THtmlMetaTag;
begin
    content := _content;
    Result := Self;
end;

function THTMLMetaTag.setHttp_equiv(_http_equiv: string): THtmlMetaTag;
begin
    http_equiv := _http_equiv;
    Result := Self;
end;



{ THTMLScript }

function THTMLScript.getAsync: boolean;
begin
    Result := findAttrFlag('async');
end;

function THTMLScript.getDefer: boolean;
begin
    Result := findAttrFlag('defer');
end;

function THTMLScript.getScriptType: string;
begin
    Result := FAttributes.Values['type'];
end;

function THTMLScript.getSrc: string;
begin
    Result := FAttributes.Values['src'];
end;

procedure THTMLScript.setFScriptType(AValue: string);
begin
    FAttributes.Values['type'] := AValue;
end;

procedure THTMLScript.setFSrc(AValue: string);
begin
    FAttributes.Values['src'] := AValue;
end;

procedure THTMLScript.setFAsync(const _async: boolean);
begin
    case _async of
        True: setAsync;
        False: unsetAsync;
    end;
end;

procedure THTMLScript.setFDefer(const _defer: boolean);
begin
    case _defer of
        True: setDefer;
        False: unsetDefer;
    end;
end;

function THTMLScript.Render(_level: QWord): string;
begin
    if (not src.IsEmpty and not Text.isEmpty) then
    begin
        {both src and text are filled in. We can only allow one thing}
        {comment out the text}
        Text := '/*==FIX DEFINITION== ' + Text + '*/';
    end;
    Result := inherited;
end;

constructor THTMLScript.Create;
begin
    inherited Create;
    tag := 'script';
    setAttr('type', 'application/javascript');
end;

function THTMLScript.setAttr(_key: string; _value: string): THtmlScript;
begin
    inherited setAttr(_key, _value);
    Result := Self;
end;

function THTMLScript.setSrc(_src: string): THtmlScript;
begin
    src := _src;
    Result := Self;
end;

function THTMLScript.setScriptType(_script_type: string): THtmlScript;
begin
    script_type := _script_type;
    Result := Self;
end;

function THTMLScript.setAsync: THtmlScript;
begin
    Result := Self;
    setAttrFlag('async');
end;

function THTMLScript.unsetAsync: THtmlScript;
begin
    Result := Self;
    rmAttrFlag('async');
end;

function THTMLScript.setDefer: THtmlScript;
begin
    Result := Self;
    setAttrFlag('defer');
end;

function THTMLScript.unsetDefer: THtmlScript;
begin
    Result := self;
    rmAttrFlag('defer');
end;

function THTMLScript.LoadFile(_file: string): THtmlScript;
begin
    if FileExists(_file) then
        with TStringList.Create do
        begin
            LoadFromFile(_file);
            FText := Text;
            FAttributes.Clear;
            script_type := 'text/javascript';
            Free;
        end;
    Result := Self;
end;


{ THtmlOrderedList }

constructor THtmlOrderedList.Create;
begin
    inherited Create;
    tag := 'ol';
end;

{ THtmlList }

function THtmlList.item(t: string = ''): THtmlListItem;
begin
    Result := THtmlListItem(add(THtmlListItem.Create));
    Result.Text := t;
end;

{ THtmlUnorderedList }

constructor THtmlUnorderedList.Create;
begin
    inherited Create;
    tag := 'ul';
end;

{ THtmlListItem }

constructor THtmlListItem.Create;
begin
    inherited Create;
    tag := 'li';
end;

{ THtmlTable }

function THtmlTable.render(_level: QWord): string;
var
    i: integer;
    _html, _tmp, _indent: string;
begin
    level := _level;
    _indent := indent(succ(level));
    {Table header}
    _tmp  := '';
    _html := '';

    if Assigned(myHeader) then begin
        myHeader.level := Succ(self.level);
        _html := myHeader.render(succ(level));
	end;
    if _html <> '' then
        _tmp := sLineBreak + _html;

    myBody.level := Succ(self.level);
    _html := myBody.render(succ(level));
    if _html <> '' then begin
        if _tmp <> '' then
            _tmp := _tmp + sLineBreak;
        _tmp := _tmp + sLineBreak + _html;
	end;

    {Table Footer}
    _html := '';
    if Assigned(myFooter) then begin
        myFooter.level := Succ(self.level);
        _html := myFooter.render(succ(level));
	end;
    if _html <> '' then begin
        if _tmp <> '' then
            _tmp := _tmp + sLineBreak;
        _tmp := _tmp + sLineBreak + _html;
	end;

    if Text <> '' then
        Text := _tmp + sLineBreak + _indent + Text
    else
        Text := _tmp;

    {Take over rendering}
    renderStyle;
    if Text <> '' then
        Result := Format(renderFormatString, [Text])
    else
        Result := Format(renderFormatStringEmpty, []);
end;

constructor THtmlTable.Create;
begin
    inherited Create;
    tag := 'table';
    myHeader:= THtmlTableHeader.Create;
    myHeader.Level := succ(level);

    myBody  := THtmlTableBody.Create;
    myBody.Level := succ(level);

    myFooter:= THtmlTableFooter.Create;
    myFooter.Level := succ(level);
end;

destructor THtmlTable.Destroy;
begin
    FreeAndNil(myHeader);
    FreeAndNil(myFooter);
    FreeAndNil(myBody);
    inherited Destroy;
end;

function THtmlTable.newRow: THtmlTableRow;
begin
    Result := myBody.newRow;
end;

function THtmlTable.tableheader: THtmlTableHeader;
begin
    if not Assigned(myHeader) then
        myHeader := THtmlTableHeader.Create;
    Result := myHeader;
end;

function THtmlTable.tablefooter: THtmlTableFooter;
begin
    if not Assigned(myFooter) then
        myFooter := THtmlTableFooter.Create;
    Result := myFooter;
end;

function THtmlTable.rowCount: integer;
begin
    Result := myBody.Count;
end;

function THtmlTable.row(_index: integer): THtmlTableRow;
begin
    Result := myBody.row(_index);
end;

procedure THtmlTable.Clear;
begin
    //myHeader.Clear;
    //myFooter.Clear;
    myBody.Clear;
	inherited Clear;
end;

{ THtmlTableRow }

constructor THtmlTableRow.Create;
begin
    inherited Create;
    tag := 'tr';
end;

function THtmlTableRow.newColHeader(t: string): THtmlTableHeaderColumn;
begin
    Result := THtmlTableHeaderColumn(add(THtmlTableHeaderColumn.Create));
    Result.Text := t;
end;

function THtmlTableRow.newCol(t: string = ''): THtmlTableColumn;
begin
    Result := THtmlTableColumn(add(THtmlTableColumn.Create));
    Result.Text := t;
end;

function THtmlTableRow.colCount: integer;
begin
    Result := Count;
end;

function THtmlTableRow.col(_index: integer): THtmlTableColumn;
begin
    Result := nil;
    if _index < Count then
        Result := Items[_index] as THtmlTableColumn;
end;

function THtmlTableRow.setClass(_c: string): THtmlTableRow;
begin
    inherited;
    Result:= self;
end;

function THtmlTableRow.setClass(_c: array of string): THtmlTableRow;
begin
    inherited;
    Result:= self;
end;

function THtmlTableRow.addClass(_c: string): THtmlTableRow;
begin
    inherited;
    Result:= self;
end;

function THtmlTableRow.removeClass(_c: string): THtmlTableRow;
begin
    inherited;
    Result:= self;
end;

function THtmlTableRow.toggleClass(_c: string): THtmlTableRow;
begin
    inherited;
    Result:= self;
end;

{ THtmlTableColumn }

constructor THtmlTableColumn.Create;
begin
    inherited Create;
    tag := 'td';
end;

function THtmlTableColumn.rowSpan(_n: word): THtmlTableColumn;
begin
    setAttr('rowspan', _n.ToString);
    Result := self;
end;

function THtmlTableColumn.colSpan(_n: word): THtmlTableColumn;
begin
    setAttr('colspan', _n.ToString);
    Result := self;
end;

function THtmlTableColumn.setClass(_c: string): THtmlTableColumn;
begin
    inherited; result:= self;
end;

function THtmlTableColumn.setClass(_c: array of string): THtmlTableColumn;
begin
    inherited; result:= self;
end;

function THtmlTableColumn.addClass(_c: string): THtmlTableColumn;
begin
    inherited; result:= self;
end;

function THtmlTableColumn.removeClass(_c: string): THtmlTableColumn;
begin
    inherited; result:= self;
end;

function THtmlTableColumn.toggleClass(_c: string): THtmlTableColumn;
begin
    inherited; result:= self;
end;

{ THeading4 }

constructor THeading4.Create;
begin
    inherited Create;
    tag := 'h4';
end;

{ THtmlDiv }


constructor THtmlDiv.Create;
begin
    inherited Create;
    tag := 'div';
end;

destructor THtmlDiv.Destroy;
begin
    inherited Destroy;
end;

{ THtmlDoc }
constructor THtmlDoc.Create;
begin
    inherited Create;
    suppressID:= true;

    {When you override newHead and newBody  you will be able to assign this on create}
    FHtmlHead := newHead;
    FHtmlBody := newBody;

    {Create a default element for scripts}
    Body.ending.script.setId(PAGE_SCRIPT_ID);

    {define the styles and the scripts in these }
    myScripts := TJavaScripts.Create;
    myStyles  := THtmlStyleSheet.Create;

    tag  := 'html';
    Name := 'document'; {default name}

end;

destructor THtmlDoc.Destroy;
begin
    FreeAndNil(myScripts);
    FreeAndNil(myStyles);
    inherited Destroy;
end;

function THtmlDoc.copyFrom(_source: THtmlDoc): THtmlDoc;
var
    i: integer;
    el: THtmlElement;
begin
    Result := Self;
    {SCRIPTS}
    if assigned(_source.myscripts) then
    begin
        FreeAndNil(myScripts);
        myScripts := _source.myscripts.clone;
    end;

    {STYLES}
    if Assigned(_source.myStyles) then
    begin
        FreeAndNil(myStyles);
        myStyles := _source.myStyles.clone as THtmlStyleSheet;
    end;

    {HEAD}
    FHtmlHead.copyFrom(_source.Head);

    {BODY}
    FHtmlBody.copyFrom(_source.Body);

    {ATTRIBUTES}
    FAttributes.Assign(_source.FAttributes);
end;

function THtmlDoc.clone: THtmlDoc;
begin
    Result := THtmlDoc.Create;
    Result.copyFrom(Self);
end;

function THtmlDoc.newHead(t: string): THtmlHead;
begin
    Result := THtmlHead(add(THtmlHead.Create));
    Result.Text := t;
end;

function THtmlDoc.newBody(t: string): THtmlBody;
begin
    Result := THtmlBody(add(THtmlBody.Create));
    Result.Text := t;
end;

procedure THtmlDoc.setDocTitle(const _Title: string);
begin
    Head.title(_Title);
end;

function THtmlDoc.getDocTitle: string;
var
    title_tag: THtmlElement;
begin
    Result := '';
    title_tag := Head.get('doc_title');
    if Assigned(title_tag) then
        Result := title_tag.Text;
end;

function THtmlDoc.getLanguage: string;
begin
    Result := FAttributes.Values['lang'];
end;

procedure THtmlDoc.setLanguage(const _language: string);
begin
    FAttributes.Values['lang'] := _language;
end;

procedure THtmlDoc.setScripts(const _Scripts: TJavaScripts);
begin
    if myScripts = _Scripts then
        Exit;
    myScripts := _Scripts;
end;

procedure THtmlDoc.setStyles(const _Styles: THtmlStyleSheet);
begin

    if myStyles = _Styles then
        Exit;

    if Assigned(myStyles) then
        FreeAndNil(myStyles);

    myStyles := _Styles;
end;

procedure THtmlDoc.setDocName(_name: string);
var
    _path: string;
begin
    myName:= _name;
    _path:= appendPath([getAssetsPath('pages'), myName]);

    myScripts.filePath:= _path;
    myScripts.webPath := appendPath(['/assets', _path]);

    myStyles.filePath := myScripts.filePath;
    myStyles.webPath  := myScripts.webPath;
    myStyles.documentName:= myName;
end;

function THtmlDoc.render(_level: QWord): string;
var
    _styleTag, _script: THtmlElement;
begin
 // First put the header
    Result := HTML_DOC_HEADER + sLineBreak;
    try
        {Create a default element for style}
        _styleTag := FHtmlHead.get(PAGE_STYLE_ID);
        if not assigned(_styleTag) then
        begin
            _styleTag:= FHtmlHead.add(THtmlStyleElement.Create);
            _styleTag.setID(PAGE_STYLE_ID);
		end;
        myStyles.indent := _styleTag.indent;
        _styleTag.Text  := myStyles.styledef;
    except
        on e:exception do
        begin
            writeln('THtmlDoc.render page style: ' + E.Message);
	    end;
	end;

    try
        // Page script
	    Body.ending.get(PAGE_SCRIPT_ID).Text:= myScripts.code;
	except
        on e:exception do
        begin
            writeln('THtmlDoc.render page script: ' + E.Message);
		end;
	end;

	Result := Result + inherited;
end;


{ THtmlBody }

function THtmlBody.render(_level: QWord): string;
var
    i: integer;
    _html, _tmp: string;
begin
    level := _level;
    Result := sLineBreak
              + myHeader.render(succ(_level))
              + sLineBreak;

    {Render all members}
    for i := 0 to myElementList.Count - 1 do
    begin
        _html := Items[i].render(succ(_level));
        if (_html <> '') then
            Result := Result + _html;
    end;

    {Render body text}
    if Text <> '' then
        Result := Result + sLineBreak + indent(succ(_level)) + Text;

    _tmp := myfooter.render(succ(_level));
    if _tmp <> '' then
        Result := Result + sLineBreak + _tmp;

    _tmp := myEnding.render(succ(_level));
    if _tmp <> '' then
       Result := Result + sLineBreak + _tmp;

    {take over rendering}
    Result := Format(renderFormatString, [Result]);
end;

constructor THtmlBody.Create;
begin
    inherited Create;
    suppressID:= true;
    tag:= 'body';
    myHeader := THtmlCollection.Create('header');
    myFooter := THtmlCollection.Create('footer');
    myEnding := THtmlCollection.Create(''); {render without tags. place holder for everything that needs to go at the end of the body tag}
end;

destructor THtmlBody.Destroy;
begin
    myEnding.Free;
    myFooter.Free;
    myHeader.Free;
    inherited Destroy;
end;

function THtmlBody.footer: THtmlCollection;
begin
    Result := myFooter;
end;

function THtmlBody.copyFrom(_source: THtmlBody): THtmlBody;
begin
    if Assigned(myHeader) then
        myHeader.Free;
    //writeln('cloning myHeader');
    myHeader := _source.myHeader.clone;

    if Assigned(myFooter) then
        myFooter.Free;
    //writeln('cloning myFooter');
    myFooter := _source.myFooter.clone;

    if Assigned(myEnding) then
        myEnding.Free;
    //writeln('cloning myEnding');
    myEnding := _source.myEnding.clone;

    //writeln('Copying inherited');
    inherited copyFrom(_source);
    Result := Self;
end;

function THtmlBody.clone: THtmlBody;
begin
    Result := THtmlBody.Create;
    Result.copyFrom(self);
end;

function THtmlBody.ending: THtmlCollection;
begin
    Result := myEnding;
end;

function THtmlBody.header: THtmlCollection;
begin
    Result := myHeader;
end;

procedure THtmlBody.Clear;
begin
    inherited Clear;
end;

{ THtmlHead }

constructor THtmlHead.Create;
begin
    inherited Create;
    suppressID:= true;
    tag := 'head';
    charsetShort('utf-8');
    viewPort.content:='width=device-width, initial-scale=1';
end;

function THtmlHead.meta(_name: string; _content: string): THTMLMetaTag;
begin
    Result := meta();
    Result.tagName := _name;
    Result.content := _content;
end;

function THtmlHead.meta: THTMLMetaTag;
begin
    Result := THtmlMetaTag(add(THtmlMetaTag.Create));
end;

function THtmlHead.title(_title: string): THtmlHead;
var
    title_tag: THtmlElement;
begin
    title_tag := get('doc_title');
    if not Assigned(title_tag) then
    begin
        title_tag := THtmlElement.Create;
        title_tag.tag := 'title';
        title_tag.setId('doc_title');
        add(title_tag);
    end;
    title_tag.Text := _title;
    Result := Self;
end;

function THtmlHead.author(_author: string): THtmlHead;
var
    meta_name: THTMLMetaTag;
begin
    Result := Self;
    meta('author', _author);
end;

function THtmlHead.description(_descr: string): THtmlHead;
begin
    Result := Self;
    meta('description', _descr);
end;

function THtmlHead.viewport: THtmlViewPort;
begin
    Result := THtmlViewPort(get('viewport'));
    if not Assigned(Result) then
    begin
        Result := THtmlViewPort.Create;
        Result.setId('viewport');
        add(Result);
    end;
end;

function THtmlHead.stylesheet(_css_source: string): THtmlStyleSheetLink;
begin
    Result := THtmlStyleSheetLink(add(THtmlStyleSheetLink.Create));
    with Result do
    begin
        rel := 'stylesheet';
        href := _css_source;
    end;
end;

function THtmlHead.pre(_code: string): THtmlHead;
begin
    inherited pre(_code);
    Result := Self;
end;

function THtmlHead.favicon(_favicon_uri: string): THtmlHead;
begin
    Result := self;
    with link do {adds a new link to the list}
    begin
        rel := 'icon';
        _type := 'image/png';
        href := _favicon_uri;
    end;
end;

function THtmlHead.clone: THtmlHead;
begin
    Result := inherited clone as THtmlHead;
end;

function THtmlHead.style(_text: string): THtmlHead;
var
    _style: THtmlElement;
begin
    _style := THtmlElement.Create;
    _style.tag := 'style';
    _style.Text := _text;
    add(_style);
    Result := Self;
end;

function THtmlHead.font(_fontsource: string): THtmlHead;
begin

    Result := self;
end;

function THtmlHead.base(_baseURL: string): THtmlHead;
var
    base_tag: THtmlElement;
begin
    base_tag := THtmlElement.Create('base');
    base_tag.noEndTag;
    base_tag.setAttr('href', _baseURL);
    base_tag.setAttr('target', '_blank');
    add(base_tag);
    Result := self;
end;

function THtmlHead.charset(_charset: string): THtmlHead;
begin
    {<meta http-equiv="content-type" content="text/html; charset=utf-8">}
    Result := Self;
    with meta() do begin
        http_equiv:= 'content-type';
        content := format('text/html; charset=%s',[_charset])
	end;
end;

function THtmlHead.charsetShort(_charset: string): THtmlHead;
begin
    {<meta charset="utf-8">}
    Result := Self;
    with meta() do begin
        setAttr('charset', _charset);
	end;
end;

{ TParagraph }

constructor TParagraph.Create;
begin
    inherited Create;
    tag := 'p';
end;

{ THeading3 }

constructor THeading3.Create;
begin
    inherited Create;
    tag := 'h3';
end;

{ THeading2 }

constructor THeading2.Create;
begin
    inherited Create;
    tag := 'h2';
end;

{ THeading1 }

constructor THeading1.Create;
begin
    inherited Create;
    tag := 'h1';
end;

{ THtmlCollection }

function THtmlCollection.getItems(index: integer): THtmlElement;
begin
    // Result := THtmlElement(FList.Items[index]);
    Result := THtmlElement(myElementList.Items[index]);
end;

function THtmlCollection.render(_level: QWord): string;
var
    i: integer;
    _tmp, _html: string;
begin
    level := _level;
  // Render children first
    _tmp := '';
    {Render all members}
    for i := 0 to pred(myElementList.Count) do
    begin
        Items[i].level:= succ(level);
        _html := Items[i].render(succ(level));
        if _html <> '' then
            _tmp := _tmp + sLineBreak + _html;
	end;

    // Fill the Text property with children's Html.
    // Then render text as usual
    if Text <> '' then begin
        Text := _tmp + sLineBreak + indent(succ(level)) + Text;
	end
    else
        Text := _tmp;

    {add to collection}
    renderStyle;
    if Text <> '' then
        Result:= Format(renderFormatString, [Text])
    else
        Result := Format(renderFormatStringEmpty, []);

end;

function THtmlCollection.Count: integer;
begin
    Result := myElementList.Count;
end;


function THtmlCollection.add(_htmlElement: THtmlElement): THtmlElement;
begin
    Result := _htmlElement;
    Result.setParent(Self);
    Result.myIndex := myElementList.Add(_htmlElement.getID, _htmlElement);
    Result.level   := succ(self.Level);
end;

function THtmlCollection.add(_htmlCollection: THtmlCollection): THtmlCollection;
begin
    Result:= add(THtmlElement(_htmlCollection)) as THtmlCollection;
end;

procedure THtmlCollection.add(_htmlElements: array of THtmlElement);
var
    element: THtmlElement;
begin
    for element in _htmlElements do
        add(element);
end;

function THtmlCollection.remove(_id: string): boolean;
begin

end;

function THtmlCollection.exists(_id: string): boolean;
begin
    Result := Assigned(find(_id));
end;

function THtmlCollection.find(_id: string): THtmlElement;
begin
    Result := THtmlElement(myElementList.Find(_id));
end;


function THtmlCollection.get(_id: string): THtmlElement;
var
    _index: integer = 0;
    _el: THtmlElement;
    _collection: THtmlCollection;
begin
    try
	    Result := find(_id);
	    {If not found, then search the tree}
	    if not Assigned(Result) then
	    begin
	        while (_index < Count) do
	        begin
                _el:= Items[_index];
	            if _el is THtmlCollection then
	            begin
                    if _el.tagID = _id then
                    begin
                        Result:= _el;
                        break;
					          end
                    else
                    begin
	                       _collection:= _el as THtmlCollection;
		                     Result := _collection.get(_id); {Recursive}
		                     if Assigned(Result) then begin
		                        break {found}
						             end;
					          end;
              end;
	            Inc(_index);
			end;
		end;
    except
        on E:Exception do
        begin
            writeln('THtmlCollection.get() Exception!! : ' , E.Message);
            writeln(Format('>> get(%s) in  <%s> %s', [_id, tag, tagID]));
		end;
	end;
end;

function THtmlCollection.getCollection(_id: string): THtmlCollection;
var
    e: THtmlElement;
begin
    Result:= nil;
    e:= get(_id);
    if assigned(e) then
        if e is THtmlCollection then
            Result:= e as THtmlCollection;
end;

function THtmlCollection.getIDs(_filter: string): TStrings;
var
    i, j: integer;
    el: THtmlElement;
    _idList: TStrings;
    filtered: boolean;
    skip: boolean = False;
begin
    Result := TStringList.Create;
    filtered := not _filter.IsEmpty;

    for i := 0 to Count - 1 do
    begin
        el := THtmlElement(myElementList.Items[i]);
        if filtered then
        begin
            if el.getID.StartsWith(_filter) then
                Result.Append(el.getID);
        end
        else
            Result.Append(el.getID);

        if el is THtmlCollection then
        begin
            _idList := THtmlCollection(el).getIDs(_filter); {recursive}
            for j := 0 to _idList.Count - 1 do
                Result.Append(_idList[j]);
            _idList.Free;
        end;
    end;
end;

function THtmlCollection.reindex(_index: integer): boolean;
var
    el: THtmlElement;
    _oldName: string;
begin
    Result := (_index >= 0) and (_index < Count);
    if Result then
    begin
        {if the index is valid, rename it}
        el := THtmlElement(Items[_index]);
        _oldName := myElementList.NameOfIndex(_index);
        myElementList.Rename(_oldname, el.getID);
    end;
end;

constructor THtmlCollection.Create;
begin
    inherited Create;
    {Change this to false if you want to only create a shell collection. This will be evaluated in Destroy}
    autoFree:= true;
    myElementList := TFPHashObjectList.Create();
    hasEndTag := True; // Every Html collection must have an end tag
end;

destructor THtmlCollection.Destroy;
var
    i: integer;
begin
    // myElementList.OwnsObjects := autoFree;
    FreeAndNil(myElementList);
    inherited Destroy;
end;

function THtmlCollection.h1(t: string): THeading1;
begin
    Result := THeading1(add(THeading1.Create));
    Result.Text := t;
end;

function THtmlCollection.h2(t: string): THeading2;
begin
    Result := THeading2(add(THeading2.Create));
    Result.Text := t;
end;

function THtmlCollection.h3(t: string): THeading3;
begin
    Result := THeading3(add(THeading3.Create));
    Result.Text := t;
end;

function THtmlCollection.h4(t: string): THeading4;
begin
    Result := THeading4(add(THeading4.Create));
    Result.Text := t;
end;

function THtmlCollection.h5(t: string): THeading5;
begin
    Result:= THeading5(add(THeading5.Create));
    Result.Text:= t;
end;

function THtmlCollection.h6(t: string): THeading6;
begin
    Result:= THeading6(add(THeading6.Create));
    Result.Text:= t;
end;

function THtmlCollection.h7(t: string): THeading7;
begin
    Result:= THeading7(add(THeading7.Create));
    Result.Text:= t;
end;

function THtmlCollection.p(t: string): TParagraph;
begin
    Result := TParagraph(add(TParagraph.Create));
    Result.Text := t;
end;

function THtmlCollection.script(t: string): THtmlScript;
begin
    Result := THTMLScript(add(THTMLScript.Create));
    Result.Text := t;
end;

function THtmlCollection.scriptCDN(_source: string): THtmlScript;
begin
    Result := THtmlScript(add(THtmlScript.Create));
    Result.src := _source;
    Result.script_type := 'application/javascript';
end;

function THtmlCollection.link: THtmlLink;
begin
    Result := THtmlLink(add(THtmlLink.Create));
end;

function THtmlCollection.a(_text: string; _url: string): THtmlAnchor;
begin
    Result := THtmlAnchor.Create;
    Result.Text := _text;
    Result.href := _url;
    Result.target:= target_self;
    add(Result);
end;

function THtmlCollection.require(_file: string): THtmlTagless;
begin
    Result := THtmlTagless.Create;
    Result.require(_file);
    add(Result);
end;

function THtmlCollection.tagLess: THtmlTagless;
begin
    Result := THtmlTagless.Create;
    add(Result);
end;

function THtmlCollection.interesting(_text: string): THtmlInterestingText;
begin
    Result:= THtmlInterestingText.Create;
    Result.Text:= _text;
    add(Result);
end;

function THtmlCollection.dialog: THtmlDialog;
begin
    Result:= THtmlDialog(add(THtmlDialog.Create));
end;

function THtmlCollection.details: THtmlDetails;
begin
    Result := THtmlDetails(add(THtmlDetails.Create));
end;

procedure THtmlCollection.Clear;
begin
    myElementList.Clear;
end;

procedure THtmlCollection.reIndex(_el: THtmlElement);
var
    _prevIndex: string;
    _index: integer;
begin
    _index:= myElementList.IndexOf(_el);
    if _index > -1 then
    begin
        _prevIndex:= myElementList.NameOfIndex(_index);
        myElementList.Rename(_prevIndex, _el.tagID);
	end;
end;

function THtmlCollection.setClass(_c: array of string): THtmlCollection;
begin
    inherited; result:= self;
end;

function THtmlCollection.setClass(_c: string): THtmlCollection;
begin
    inherited; result:= self;
end;

function THtmlCollection.addClass(_c: string): THtmlCollection;
begin
    inherited; result:= self;
end;

function THtmlCollection.removeClass(_c: string): THtmlCollection;
begin
    inherited; result:= self;
end;

function THtmlCollection.toggleClass(_c: string): THtmlCollection;
begin
    inherited; result:= self;
end;

function THtmlCollection.copyFrom(_source: THtmlCollection): THtmlCollection;
var
    i: integer;
    el: THtmlElement;
begin
    inherited copyFrom(_source); {from THtmlElement}
    for i := 0 to _source.Count - 1 do
    begin
        el := _source.Items[i];
        if el is THtmlCollection then
        begin
            add(THtmlCollection(el).clone);
        end
        else
        begin
            add(el.clone);
        end;
    end;
    Result := self;
end;

function THtmlCollection.clone: THtmlCollection;
begin
    {Duplicate the exact same class as current}
    Result := NewHtmlBuilderObject(Self) as THtmlCollection;
    Result.copyFrom(self);
end;

function THtmlCollection.div_(t: string): THtmlDiv;
begin
    Result := THtmlDiv(add(THtmlDiv.Create));
    Result.Text := t;
end;

function THtmlCollection.span_(t: string): THtmlSpan;
begin
    Result := THtmlSpan(add(THtmlSpan.Create));
    Result.Text := t;
end;

function THtmlCollection.section_(t: string): THtmlSection;
begin
    Result := THtmlSection(add(THtmlSection.Create));
    Result.Text := t;
end;

function THtmlCollection.table_: THtmlTable;
begin
    Result := THtmlTable(add(THtmlTable.Create));
end;

function THtmlCollection.ul_: THtmlUnorderedList;
begin
    Result := THtmlUnorderedList(add(THtmlUnorderedList.Create));
end;

function THtmlCollection.ol_: THtmlOrderedList;
begin
    Result := THtmlOrderedList(add(THtmlOrderedList.Create));
end;

function THtmlCollection.form_: THtmlForm;
begin
    Result := THtmlForm(add(THtmlForm.Create));
end;

function THtmlCollection.strong(_text: string): TStrong;
begin
    Result:= TStrong(add(TStrong.Create));
    Result.Text:= _text;
end;

function THtmlCollection.emphasis(_text: string): TEmphasis;
begin
    Result:= TEmphasis(add(TEmphasis.Create));
    Result.Text:= _text;
end;

function THtmlCollection.img(src: string): THtmlImg;
begin
    Result:= THtmlImg.Create;
    Add(Result);
    Result.src:=src;
end;

function THtmlCollection.imgBase64(_file: string): THtmlImageBase64;
begin
    Result:= THtmlImageBase64.Create;
    Add(Result);
    Result.src:=_file

end;

function THtmlCollection.template: THtmlTemplate;
begin
    Result:= THtmlTemplate(add(THtmlTemplate.Create));
end;

function THtmlCollection.fragment: THtmlFragment;
begin
    Result:= THtmlFragment(add(THtmlFragment.Create));
end;

function THtmlCollection.input_(_type: THtmlInputType = THtmlInputType.inputText;
    _id: string = ''; _name: string = ''; _value: string = ''): THtmlInput;

begin
    Result := THtmlInput.Create;

    Result.TagID := _id;
    Result.input_type := _type;
    Result.TagName := _name;
    Result.Value := _value;
    add(Result);
end;

function THtmlCollection.fieldset: THtmlFieldSet;
begin
    Result := THtmlFieldSet(add(THtmlFieldSet.Create));
end;

function THtmlCollection.legend: THtmlLegend;
begin
    Result := THtmlLegend(add(THtmlLegend.Create));
end;

function THtmlCollection.editBox: THtmlEditBox;
begin
    Result := THtmlEditBox(add(THtmlEditBox.Create));
end;

function THtmlCollection.button: THtmlButton;
begin
    Result := THtmlButton(add(THtmlButton.Create));
end;

function THtmlCollection.checkbox: THtmlCheckBox;
begin
    Result := THtmlCheckBox(add(THtmlCheckBox.Create));
end;

function THtmlCollection.radiobutton: THtmlRadioButton;
begin
    Result := THtmlRadioButton(add(THtmlRadioButton.Create));
end;

function THtmlCollection.textarea: THtmlTextArea;
begin
    Result := THtmlTextArea(add(THtmlTextArea.Create));
end;

function THtmlCollection.selection: THtmlSelect;
begin
    Result := THtmlSelect(add(THtmlSelect.Create));
end;

function THtmlCollection.navbar: THtmlNavbar;
begin
    Result := THtmlNavBar(add(THtmlNavbar.Create));
end;

function THtmlCollection.menu: THtmlMenu;
begin
    Result := THtmlMenu(add(THtmlMenu.Create));
end;

function THtmlCollection.contextmenu: THtmlMenu;
begin
    trip('THtmlCollection context menu not implemented');
end;

function THtmlCollection.alert: THtmlAlert;
begin
    trip('THtmlCollection alert is not implemented');

end;

function THtmlCollection.login: THtmlForm;
begin
    trip('THtmlCollection login form is not implemented');

end;

function THtmlCollection.confirm: THtmlForm;
begin
    trip('THtmlCollection confirm is not implemented');

end;

{ THtmlElement }

procedure THTMLElementBase.setFText(AValue: string);
begin
    FText := AValue;
end;

function THTMLElementBase.indent: string;
begin
    Result := indent(level);
end;

function THTMLElementBase.indent(const _level: QWORD): string;
var
	i: Integer;
begin
    Result := '';
    for i := 1 to _level do
        Result := Result + DEFAULT_INDENT;
end;

procedure THtmlElement.setFName(const __Name: string);
begin
    if TagName = __Name then  exit;
    FAttributes.Values['name'] := __Name;
end;

function THtmlElement.getCSSClass: string;
begin
    Result := FAttributes.Values['class'];
end;

function THtmlElement.getCharSet: string;
begin
    //Result := FAttributes.Values['charset'];
    Result := getAttr('charset');
end;

function THtmlElement.getID: string;
begin
    Result := FAttributes.Values['id'];
end;

procedure THtmlElement.setFCharSet(AValue: string);
begin
    //FAttributes.Values['charset'] := AValue;
    setAttr('charset', AValue);
end;

procedure THtmlElement.setFCSSClass(AValue: string);
begin
    FAttributes.Values['class'] := AValue;
end;

function THtmlElement.getName: string;
begin
    Result := FAttributes.Values['name'];
end;

procedure THtmlElement.setFID(const __ID: string);
begin
    if TagID = __ID then
        Exit;
    FAttributes.Values['id'] := __ID;

    if FParent is THtmlCollection then
        THtmlCollection(FParent).reIndex(self);
end;

function THtmlElement.getTitleAttr: string;
begin
    Result:= getAttr('title');
end;

procedure THtmlElement.setTitleAttr(const _title: string);
begin
    setAttr('title', _title);
end;

function THtmlElement.getAnchor: string;
begin
    Result:= '#' + tagID;
end;


function THtmlElement.tag_start: string;
var
    _attributes: string;
begin
    _attributes := FormatAttributes;

    // Spacing if there are attributes
    //if Length(_attributes) > 0 then
    //  _attributes := ' ' + _attributes;

    Result := Format('<%s%s>', [tag, _attributes]);
end;

function THtmlElement.FormatAttributes: string;
var
    i: integer;
    _attr: string;
    _value: string;

begin
    Result := '';
    for i := 0 to FAttributes.Count - 1 do
    begin
        _attr := FAttributes.Names[i];
        _value := FAttributes.ValueFromIndex[i];
        if _attr = '' then
            {including attribute flags}
            Result := Result + ' ' + _value
        else
        begin
            // Skip ID Render
            if _attr = 'id' then begin
                if suppressID then continue;
            end;

            if _value.StartsWith(ATTR_NUMBER_PREFIX) then
            begin
                {decodifying the number stored}
                _value := ReplaceStr(_value, ATTR_NUMBER_PREFIX, '');
                _value := ReplaceStr(_value, ATTR_NUMBER_POSTFIX, '');
            end
            else
            begin
                {put string values in double quotes}
                _value := '"' + _value + '"';
            end;

            Result := Result + Format(' %s=%s', [_attr, _value]);
        end;
    end;
end;

function THTMLElementBase.tag_start: string;
begin
    Result := sLineBreak + '<' + tag;
    Result := Result + FormatAttributes;

    if hasEndTag then
        Result := Result + '>'
    else
        Result := Result + '/>';

end;

function THTMLElementBase.tag_end: string;
begin
    if hasEndTag then
        Result := '</' + tag + '>'
    else
        Result := '';
end;

function THTMLElementBase.renderFormatString: string;
begin
   // Returns the format string to render the element
    if (tag <> '') then
    begin
        Result := indent +  tag_start; {---- starttag}
        if hasEndTag then
        begin
            Result := Result + '%s';                          {---- starttag <%s text> \n}
            Result := Result + sLineBreak + indent + tag_end; {---- starttag <%s text> \n ---- endtag}
		end;
        // If this does not have an end tag then there is no placeholder for text
        // Have to test this case.
	end
    else
        Result := indent + '%s';
end;

function THTMLElementBase.renderFormatStringEmpty: string;
begin
    // Returns the format string to render the element
     if (tag <> '') then
         Result := indent +  tag_start + tag_end
     else
         Result := indent + '%s';
end;

function THTMLElementBase.formatAttributes: string;
begin
    Result := '';
end;


function THTMLElementBase.render(_level: QWord): string;
    {Sanitize the text. Ensure that the % character
    is escaped in the source before  sending it to format()}
    function sanitize(_str: string): string;
    begin
        Result:= '';
        {sanitize _str here}
        if not _str.isEmpty then begin
            // infer if indent has already been applied.
            // This is a weak test... but it is good for this iteration
            if _str.startsWith(DEFAULT_INDENT) then
                Result := Result + _str
            else
                Result := Result + indent(succ(Level)) + _str;
            Result := sLineBreak + Result;
        end;
	end;
begin
    if Text <> '' then
        Result := Format(renderFormatString, [sanitize(Text)])
    else
        Result := Format(renderFormatStringEmpty, ['']);
end;



function THTMLElementBase.html: string;
begin
    Result := render(level);
end;


function THTMLElementBase.setText(_text: string): THTMLElementBase;
begin
    setFText(_text);
    Result := Self;
end;

function THTMLElementBase.pre(_htmlCode: string): THTMLElementBase;
begin
    if tag <> '' then
        tag := ''; // clear the tag if you are using this
    FText := _htmlCode;
    Result := Self;
end;

function THTMLElementBase.isPre: boolean;
begin
    Result:= tag.isEmpty;
end;

function THTMLElementBase.getElementCount: integer;
begin
    {Prevent overflow}
    if myElementCount = MaxUIntValue then
        myElementCount := 0;

    Inc(myElementCount);
    Result := myElementCount;
end;

function THTMLElementBase.append(_text: string): THtmlElementBase;
begin
    Text := Text + _text;
    Result := Self;
end;

constructor THTMLElementBase.Create;
begin
    inherited Create;
    tag := '';
    hasEndTag := True;
    level  := 0;
end;


function THTMLElementBase.noEndTag: THTMLElementBase;
begin
    hasEndTag := False;
    Result := self;
end;

function THTMLElementBase.copyFrom(_source: THtmlElementBase): THTMLElementBase;
begin
    FText := _source.Text;
    tag := _source.tag;
    hasEndTag := _source.hasEndTag;
    Result := self;
end;


function THtmlElement.render(_level: QWord): string;
begin
    renderStyle;
    Result := inherited ;
end;

procedure THtmlElement.renderStyle;
begin
    if not (myStyle.Text.isEmpty) then
        setStyle(myStyle.Text);
end;

function THtmlElement.setParent(__parent: THtmlElement): THtmlElement;
begin
    FParent := __parent;
    Result := self;
end;

function THtmlElement.setAttr(_attributes: string): THtmlElement;
begin
    FAttributes.Add(_attributes);
    Result := self;
end;

function THtmlElement.setAttr(_attributes: array of string): THtmlElement;
var
    _attr: string;
begin
    for _attr in _attributes do
        FAttributes.Add(_attr);
    Result := self;
end;

function THtmlElement.setAttr(_key: string; _value: string): THtmlElement;
begin
    FAttributes.Values[_key] := _value;
    Result := Self;
end;

function THtmlElement.setAttr(_key: string; _value: integer): THtmlElement;
begin
    Result := Self;
    FAttributes.Values[_key] := ATTR_NUMBER_PREFIX + _value.ToString + ATTR_NUMBER_POSTFIX;
end;
{Returns true if the attribute was found and removed}
function THtmlElement.rmAttr(_key: string): boolean;
var
    i: integer;
begin
	i:=FAttributes.IndexOfName(_key);
	Result:= i > -1;
	if Result then
	    FAttributes.Delete(i);
end;

function THtmlElement.findAttrFlag(_key: string): boolean;
var
    i: integer;
begin
    Result := False;
    for i := 0 to FAttributes.Count - 1 do
    begin
        Result := CompareStr(_key, FAttributes.ValueFromIndex[i]) = 0;
        if Result then
            break;
    end;
end;

function THtmlElement.attrIsNum(_value: string): boolean;
begin
    Result := _value.StartsWith(ATTR_NUMBER_PREFIX) and _value.EndsWith(ATTR_NUMBER_POSTFIX);
end;

function THtmlElement.setAttrFlag(_key: string): THtmlElement;
begin
    if not findAttrFlag(_key) then
        FAttributes.Add(_key);
    Result := self;
end;

function THtmlElement.rmAttrFlag(_key: string): THtmlElement;
begin
    Result := self;
    FAttributes.Text := StringReplace(FAttributes.Text, _key, '', [rfReplaceAll]);
end;

function THtmlElement.setAttrFlag(_key: string; _set: boolean): THtmlElement;
begin
    if _set then
        setAttrFlag(_key)
    else
        rmAttrFlag(_key);
    Result := Self;
end;

function THtmlElement.getAttr(_name: string): string;
begin
    Result := FAttributes.Values[_name];
    if attrIsNum(Result) then
        Result := attrAsNum(Result);
end;

function THtmlElement.getAttrJSON: string;
var
    i: integer;
begin
    Result := '';
    for i := 0 to FAttributes.Count - 1 do
    begin
        if not Result.isEmpty then
            Result := Result + ',';
        Result := Result + Format('%s: "%s"', [FAttributes.Names[i],
            FAttributes.ValueFromIndex[i]]);
    end;

    if not Result.isEmpty then
        Result := '{' + Result + '}';
end;

function THtmlElement.getAttrMithril: string;
var
    i: integer;
    key, val: string;
begin
    Result := '';
    for i := 0 to FAttributes.Count - 1 do
    begin
        key:=FAttributes.Names[i];
        val:= FAttributes.ValueFromIndex[i];

        if key.isEmpty then {this is a flag attribute}
            key:= val;

        Result := Result + Format('[%s=%s]', [key, val]);
    end;
end;

function THtmlElement.attrAsNum(_value: string): string;
begin
    Result := _value;
    Result.Replace(ATTR_NUMBER_PREFIX, '').Replace(ATTR_NUMBER_POSTFIX,'');
end;

function THtmlElement.style: THtmlStyle;
begin
    Result := myStyle;
end;

function THtmlElement.setClass(_c: string): THtmlElement;
begin
    setFCSSClass(_c);
    Result := Self;
end;

function THtmlElement.setClass(_c: array of string): THtmlElement;
var
    _class: string;
    _style: string;

begin
    Result := Self;
    setClass(stringFromArray(_c));
end;

function THtmlElement.hasClass(_c: string): boolean;
begin
    Result:= styleClass.Contains(_c);
end;

function THtmlElement.addClass(_c: string): THtmlElement;
var
    _clsArray: TStringArray;
    _class: string;
begin
    for _class in toStringArray(selectorToClass(_c), ' ') do
    begin
        if not hasClass(_class) then
            styleClass := styleClass + ' ' + _class;
	end;
    Result:= self;
end;

function THtmlElement.removeClass(_c: string): THtmlElement;
var
    _clsArray: TStringArray;
    _class: string;
    _tmp: string = '';
    _foundPos: integer;
    _classLen: integer;
begin
    for _class in toStringArray(selectorToClass(_c)) do
    begin
        {remove the class string}
        styleClass := StringReplace(styleClass, _class, '', [rfReplaceAll]);
        {replace two spaces with one space}
        styleClass := StringReplace(styleClass, '  ', ' ', [rfReplaceAll]);
	end;
    Result:= self;
end;

function THtmlElement.toggleClass(_c: string): THtmlElement;
begin
    if hasClass(_c) then
        removeClass(_c)
    else
        addClass(_c);
    Result:= self;
end;

function THtmlElement.setStyle(_s: string): THtmlElement;
begin
    FAttributes.Values['style'] := _s.Replace(sLineBreak, '');
    Result := Self;
end;

function THtmlElement.setId(_id: string): THtmlElement;
begin
    if _id.isEmpty then exit;
    TagID := _id;
    Result := Self;
    {if the element is part of a collection then rename the index there}
    if Assigned(FParent) then
    begin
        if myIndex <> NO_INDEX then
        begin
            if _id.Length = 0 then
                raise Exception.Create(Format('%s:: ' + sLinebreak +
                    'Cannot set id to empty string because it is a member of %s',
                    [self.ClassName, THTMLCollection(FParent).ClassName]))
            else
                if THTMLCollection(FParent).reindex(myIndex) then
                    //Log('Reindex() worked')
                else
                    Log('Reindex() not successful: ' + _id);
        end
        else
            log('Umm... myIndex is not set %s', [tagID]);
    end;
end;

function THtmlElement.setField(_id: string): THtmlElement;
begin
    Result := setId(_id);
    {add the template placeholder}
    setText(TEMPLATE_PREFIX + _id + TEMPLATE_POSTFIX);
end;

function THtmlElement.setName(_name: string): THtmlElement;
begin
    tagName := _name;
    Result := Self;
end;

function THtmlElement.setTitle(_title: string): THtmlElement;
begin
    title:= _title;
    Result:= self;
end;

constructor THtmlElement.Create;
begin
    inherited Create;
    suppressID := false;
    myIndex := NO_INDEX;
    FAttributes := TStringList.Create;
    myStyle := THtmlStyle.Create;
    setId(Format('%s%d', [DEFAULT_ELEMENT_ID_PREFIX, getElementCount]));
    myEvents := TStringMap.Create;
end;

destructor THtmlElement.Destroy;
begin
    // WriteLn('Destroying Element ', ClassName, ' ', tag, '::' , getID);
    FreeAndNil(FAttributes);
    FreeAndNil(myStyle);
    FreeAndNil(myEvents);
    inherited Destroy;
end;

constructor THtmlElement.Create(_tag: string);
begin
    Create;
    tag := _tag;
end;

constructor THtmlElement.Create(_tag: string; _attributes: array of string);
var
    _attr: string;
begin
    Create(_tag);
    for _attr in _attributes do
    begin
        FAttributes.Add(_attr);
    end;
end;

function THtmlElement.copyFrom(_source: THtmlElement): THtmlElement;
begin
    inherited copyFrom(_source); // copy THTMLElementBase;

    myStyle := _source.myStyle.clone;
    FAttributes.Assign(_source.FAttributes);
    FParent := _source.parent; {only reference. Don't clone}
    Result := self;
end;

function THtmlElement.clone: THtmlElement;
begin
    {This should also preserve the ids.
     If you need to change, you can always do
    that after you have cloned                                                 }
    Result := NewHtmlBuilderObject(Self) as THtmlElement;
    Result.copyFrom(self);
end;

function THtmlElement.events: TStringMap;
begin
    Result:= myEvents;
end;

function THtmlElement.event(_name: string; _code: string): THtmlElement;
begin
    myEvents.put(_name, _code);
end;

function THtmlElement.event(_name: string): string;
begin
    Result := myEvents.valueOf(_name);
end;

procedure applyStyleClass(_element: THtmlElement; _style: THtmlStyle);
begin
    _element.addClass(_style.selector);
end;

procedure applyStyleInline(_element: THtmlElement; _style: THtmlStyle);
begin
    _element.style.use(_style);
end;

function addStyles(_styles: array of string): string;
var
    _i, _count: integer;
    _tmpstyle: string;
begin
    Result := '';
    _count := Length(_styles);
    for _i := 0 to _count - 1 do
    begin
        _tmpstyle := selectorToClass(_styles[_i]);

        if not Result.IsEmpty then
            Result := Result + ' ';

        Result := Result + _tmpstyle;
    end;
end;

function selectorToClass(_selector: string): string;
begin
    Result := '';
    if _selector.IsEmpty then
        exit;

    if _selector[1] = '.' then
        _selector := _selector.Replace('.', '')

    else if _selector[1] in ['#', '*'] then
        raise Exception.Create('selectorToClass():' + sLinebreak +
            'You are trying to convert a style class but you sent an ID selector or wildcard: '
            + _selector);

    Result := _selector;
end;

function basePath: string;
begin
    Result := myBasePath;
end;

procedure basePath(_path: string);
begin
    myBasePath := _path;
end;

end.

