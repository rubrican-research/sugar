unit TestCase1;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testutils, testregistry, GuiTestRunner;

type

	{ TAlpineJSTests }

    TAlpineJSTests= class(TTestCase)
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure cdnTest;
        procedure xDataTest;
        procedure xDataJSONAutoFreeTest;
        procedure xDataJSONTest;
        procedure xBindTest;
        procedure xOnTest;
        procedure xOnKeyUp;
        procedure xOnKeyDown;
        procedure xTransitionDirective;
        procedure xTransitionModifier;
        procedure xTransitionFull;
        procedure xForFail;
        procedure xFor;
        procedure htmlFragment;
    end;

implementation
uses
    sugar.htmlbuilder, sugar.htmlbuilder.alpinejs, fpjson;

procedure TAlpineJSTests.cdnTest;
var
  _div: THtmlDiv;
begin
    _div := THtmlDiv.Create;
    _div.cdn(_div);
    TestRunner.MemoLog.Append(_div.html);
    _div.Free;
end;

procedure TAlpineJSTests.xDataTest;
var
  _div: THtmlDiv;
begin
    _div := THtmlDiv.Create;
    _div.xData('{a:1, b:"Hello", c:true}');
    TestRunner.MemoLog.Append(_div.html);
    _div.Free;
end;

procedure TAlpineJSTests.xDataJSONAutoFreeTest;
var
  _div: THtmlDiv;
begin
    _div := THtmlDiv.Create;
    _div.xData(TJSONObject.Create([
        'a',0,
        'b', 'Hello Me',
        'c', true,
        'd', 9.02,
        'e', TJSONArray.Create(['1', 2, 'there', 553, 'are', true])
        ]), true);
    TestRunner.MemoLog.Append(_div.html);
    _div.Free;
end;

procedure TAlpineJSTests.xDataJSONTest;
var
  _div: THtmlDiv;
  _j, _k: TJSONObject;
begin
    _div := THtmlDiv.Create;
    _j := TJSONObject.Create([
        'a',0,
        'b', 'Hello Me',
        'c', true,
        'd', 9.02,
        'e', TJSONArray.Create(['1', 2, 'there', 553, 'are', true])
        ]);
    _div.xData(_j);

    _k := TJSONObject.Create([
        'r',2310,
        't', 'This is not so great',
        'y', false,
        'u', 9934.323,
        'i', TJSONArray.Create(['a', 2312, 'there', 521553, 'are', true])
        ]);
    _div.xData(_k);

    TestRunner.MemoLog.Append(_div.html);
    _div.Free;
    _j.Free;
    _k.Free;
end;

procedure TAlpineJSTests.xBindTest;
var
  _div: THtmlDiv;
begin
    _div := THtmlDiv.Create;
    _div.xBind('h', 'this is new');
    _div.xBind('class', 'container blue');
    _div.xBind('name', 'bing_bing');
    TestRunner.MemoLog.Append(_div.html);
    _div.Free;
end;

procedure TAlpineJSTests.xOnTest;
var
  _div: THtmlDiv;
begin
    _div := THtmlDiv.Create;
    _div.xOn('click','doOnClick', xonDebounce, '.600ms');
    _div.xOn('blur','doOnBlur');
    _div.xOn(dom_dblclick, 'doDblClick', xonPrevent, 'this should not show up');
    TestRunner.MemoLog.Append(_div.html);
    _div.Free;
end;

procedure TAlpineJSTests.xOnKeyUp;
var
  _div: THtmlDiv;
begin
    _div := THtmlDiv.Create;
    _div.xKeyup([], 'No_keys');
    _div.xKeyup([keyCapslock, keyEqual], 'CapsEqual');
    _div.xKeyup([keyEnter], 'CapsEqual');
    TestRunner.MemoLog.Append(_div.html);
    _div.Free;
end;

procedure TAlpineJSTests.xOnKeyDown;
var
  _div: THtmlDiv;
begin
    _div := THtmlDiv.Create;
    _div.xKeyDown([], 'No_keys');
    _div.xKeyDown([keyShift, keyCtrl, keyEnter], 'CapsEqual');
    _div.xKeyDown([keyCtrl], 'CapsEqual');
    TestRunner.MemoLog.Append(_div.html);
    _div.Free;
end;

procedure TAlpineJSTests.xTransitionDirective;
var
  _div: THtmlDiv;
begin
    _div := THtmlDiv.Create;
    _div.xTransitionClass(trdEnter, 'enter-transition');
    _div.xTransitionClass(trdEnterEnd,'enter-transition-duration', transDuration );
    _div.xTransitionClass(trdEnterEnd, 'enter-transition-scale', transScale);
    TestRunner.MemoLog.Append(_div.html);
    _div.Free;
end;

procedure TAlpineJSTests.xTransitionModifier;
var
  _div: THtmlDiv;
begin
    _div := THtmlDiv.Create;
    _div.xTransitionModifier(trdNone,transDuration, '30ms');
    _div.xTransitionModifier(trdEnter,transScale, '90');
    _div.xTransitionModifier(trdEnterStart,transOpacity, '80');
    _div.xTransitionModifier(trdEnterEnd,transDelay, '70ms');

    TestRunner.MemoLog.Append(_div.html);
    _div.Free;
end;

procedure TAlpineJSTests.xTransitionFull;
var
  _div: THtmlDiv;
begin
    _div := THtmlDiv.Create;

    _div.xTransition('.opacity.90');
    _div.xTransition(':leave.scale', 'myNewClass');

    TestRunner.MemoLog.Append(_div.html);
    _div.Free;
end;

procedure TAlpineJSTests.xForFail;
var
  _div: THtmlDiv;
begin
    _div := THtmlDiv.Create;
    try
        try
            _div.xFor('(a, i) in arrKeys');
            TestRunner.MemoLog.Append(_div.html);
		except
            on E:Exception do
                Assert(true, E.Message);
        end;
	finally
	    _div.Free;
    end;
end;

procedure TAlpineJSTests.xFor;
var
  _div: THtmlTemplate;
begin
    _div := THtmlTemplate.Create;
    try
        try
            _div.xFor('(a, i) in arrKeys', 'index');
            TestRunner.MemoLog.Append(_div.html);
		except
            on E:Exception do
                Assert(true, E.Message);
        end;
	finally
	    _div.Free;
    end;
end;

procedure TAlpineJSTests.htmlFragment;
var
  _f: THtmlFragment;
begin
    _f := THtmlFragment.Create;
    try
        try
            // _f.xFor('(a, i) in arrKeys', 'index');
            TestRunner.MemoLog.Append(_f.html);
		except
            on E:Exception do
                Assert(true, E.Message);
        end;
	finally
	    _f.Free;
    end;
end;


procedure TAlpineJSTests.SetUp;
begin

end;

procedure TAlpineJSTests.TearDown;
begin

end;

initialization

    RegisterTest(TAlpineJSTests);
end.

