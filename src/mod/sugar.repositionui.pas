unit sugar.repositionui;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
    sugar.animateui, sugar.collections, fgl;

type

	{ TRepositioner }
    TRepositioner  = class(TPersistent, IFPObserver)
    public
        type TReposSelectState = (rssUnselected, rssSelected, rssMoving, rssResize, rssRotate);

    protected
		myEnabled: boolean;

		myselState: TReposSelectState;
        myFocused: boolean;
		myChange: DWord; // Reset when you start to move or reseize or rotate (see property setters). Inc for every change
		myrank: word;
        myPrevX: integer;
        myPrevY: integer;
        myX1: integer;
		myXOffset: integer;
        myY1: integer;
        myX2: integer;
        myY2: integer;
		myYOffset: integer;

        myOnClick     : TNotifyEvent;
        myOnMouseDown : TMouseEvent;
        myOnMouseUp   : TMouseEvent;
        myOnMouseEnter: TNotifyEvent;
        myOnMouseMove : TMouseMoveEvent;
        myOnMouseLeave: TNotifyEvent;

		function getXOffset: integer;  virtual;
		function getYOffset: integer; virtual;
        procedure incChange;
        function changed: boolean;
		procedure resetChanges; // Changes

        function getBottom: integer; virtual;
		function getFocused: boolean;virtual;
		function getHeight: integer;virtual;
		function getLeft: integer;virtual;
		function getMoving: boolean;virtual;
		function getResizing: boolean;virtual;
		function getRight: integer;virtual;
		function getRotating: boolean;virtual;
		function getSelected: boolean;virtual;
		function getTop: integer;virtual;
		function getWidth: integer;virtual;
		function getX1: integer;virtual;
		function getX2: integer;virtual;
		function getY1: integer;virtual;
		function getY2: integer;virtual;
		procedure setBottom(const _value: integer);virtual;
		procedure setFocused(const _value: boolean);virtual;
		procedure setHeight(const _value: integer);virtual;
		procedure setLeft(const _value: integer);virtual;
		procedure setMoving(const _value: boolean);virtual;
		procedure setResizing(const _value: boolean);virtual;
		procedure setRight(const _value: integer);virtual;
		procedure setRotating(const _value: boolean);virtual;
		procedure setSelected(const _value: boolean);virtual;
		procedure setTop(const _value: integer);virtual;
		procedure setWidth(const _value: integer);virtual;
		procedure setX1(const _value: integer);virtual;
		procedure setX2(const _value: integer);virtual;
		procedure SetXOffset(const _value: integer);virtual;
		procedure setY1(const _value: integer);virtual;
		procedure setY2(const _value: integer);virtual;
		procedure SetYOffset(const _value: integer);virtual;

        function canResize: boolean; virtual;
        function canRepos: boolean; virtual;

        procedure start(X, Y: integer);virtual;
        procedure resize(X, Y: integer); overload;
        procedure repos(X, Y: integer);virtual;
        procedure moveX(_dx: integer);virtual; // Change X by dx
        procedure moveY(_dy: integer);virtual; // Change Y by dy
        procedure sizeW(_dw: integer);virtual; // Change width by dw
        procedure sizeH(_dh: integer);virtual; // Change height by dh

        procedure visualizeSelState(_selState: TReposSelectState);virtual;
    public
        {EVENT HANDLERS}
        function toggleState: TRepositioner;virtual;

        procedure OnControlClick(Sender: TObject);virtual;
        procedure OnControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);virtual;
        procedure OnControlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);virtual;
        procedure OnControlMouseEnter(Sender: TObject);virtual;
        procedure OnControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);virtual;
        procedure OnControlMouseLeave(Sender: TObject);virtual;
        procedure OnControlKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);virtual;

        function controlObj: TControl; virtual;

    public
		 procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);virtual;

         property top: integer read getTop write setTop;
         property left: integer read getLeft write setLeft;
         property height: integer read getHeight write setHeight;
         property width: integer read getWidth write setWidth;
         property bottom: integer read getBottom write setBottom;
         property right: integer read getRight write setRight;
         property selected: boolean read getSelected write setSelected;
         property moving: boolean read getMoving write setMoving;
         property resizing: boolean read getResizing write setResizing;
         property rotating: boolean read getRotating write setRotating;
         property selState: TReposSelectState read myselState;

         property XOffset: integer read getXOffset write SetXOffset;
         property YOffset: integer read getYOffset write SetYOffset;
         property X1: integer read getX1 write setX1;
         property X2: integer read getX2 write setX2;
         property Y1: integer read getY1 write setY1;
         property Y2: integer read getY2 write setY2;

         property focused: boolean read getFocused write setFocused;


        // Exposed Mouse events.
        // If you want to assign additional mouse events to the control,
        // use these properties instead of directly changing the event handlers
        // on the control.

         property OnClick     : TNotifyEvent    read myOnClick       write myOnClick     ;
         property OnMouseDown : TMouseEvent     read myOnMouseDown   write myOnMouseDown ;
         property OnMouseUp   : TMouseEvent     read myOnMouseUp     write myOnMouseUp   ;
         property OnMouseEnter: TNotifyEvent    read myOnMouseEnter  write myOnMouseEnter;
         property OnMouseMove : TMouseMoveEvent read myOnMouseMove   write myOnMouseMove ;
         property OnMouseLeave: TNotifyEvent    read myOnMouseLeave  write myOnMouseLeave;

         property enabled   : boolean read myEnabled write myEnabled default true;
    public

    end;

	{ GRepositioner }

    generic GRepositioner <C: TControl> = class(TRepositioner)
    protected
        myControl: C;
    protected
        function getXOffset: integer;   override;
        function getYOffset: integer;   override;

        function getHeight: integer;    override;
        function getLeft: integer;      override;
        function getTop: integer;       override;
        function getWidth: integer;     override;

        procedure setHeight(const _value: integer);     override;
        procedure setLeft(const _value: integer);       override;
        procedure setTop(const _value: integer);        override;
        procedure setWidth(const _value: integer);      override;

        procedure visualizeSelState(_selState: TReposSelectState);  override;

        function canResize: boolean; override;
        function canRepos: boolean; override;

    public
        // Exposes the control
        property control: C read myControl;
        constructor Create  (constref _control: C);
        function initControl(constref _control: C): C; virtual;
        function controlObj: TControl; override;
	end;

    TPanelRepositioner    = class(specialize GRepositioner<TPanel>);
    TLabelRepositioner    = class(specialize GRepositioner<TLabel>);
    TEditRepositioner     = class(specialize GRepositioner<TEdit>);
    TCheckBoxRepositioner = class(specialize GRepositioner<TCheckBox>);
    TListBoxRepositioner  = class(specialize GRepositioner<TListBox>);
    TButtonRepositioner   = class(specialize GRepositioner<TButton>);
    TFrameRepositioner    = class(specialize GRepositioner<TFrame>);

    TRepositionerClass = class of TRepositioner;

	{ TRepositionerList }

    TRepositionerList = class(specialize TFPGMap<ptrInt, TRepositioner>)
    private type
			{ TWatcher }
            TWatcher = class(TPersistent, IFPObserver)
			public
                owner : TRepositionerList;
				procedure FPOObservedChanged(ASender: TObject;
					Operation: TFPObservedOperation; Data: Pointer);
            end;
    private
        watcher: TWatcher;
    public
        constructor Create;
        destructor Destroy; override;

        function add(constref _control: TControl; constref _repositioner: TRepositioner): TRepositioner; overload;
    end;
    function addRepositioner(constref _control: TControl): TRepositioner;
    function addRepositioner(constref _repositioner: TRepositioner): TRepositioner;
    function repositioner(_control: TControl): TRepositioner;

implementation

uses
    Math, sugar.logger;
var
    myRespositionerList : TRepositionerList = nil;

function addRepositioner(constref _control: TControl): TRepositioner;
begin
    if _control is TLabel then
        Result := addRepositioner(TLabelRepositioner.Create(TLabel(_control)))

    else if _control is TEdit then
        Result := addRepositioner(TEditRepositioner.Create(TEdit(_control)))

    else if _control is TPanel then
        Result := addRepositioner(TPanelRepositioner.Create(TPanel(_control)))

    else if _control is TCheckBox then
        Result := addRepositioner(TCheckBoxRepositioner.Create(TCheckBox(_control)))

    else if _control is TListBox then
        Result := addRepositioner(TListBoxRepositioner.Create(TListBox(_control)))

    else if _control is TButton then
            Result := addRepositioner(TButtonRepositioner.Create(TButton(_control)))

    else if _control is TFrame then
            Result := addRepositioner(TFrameRepositioner.Create(TFrame(_control)))
    else
        raise Exception.Create(Format('There is currently no TRepositioner implementation for %d',[_control.ClassName]));

end;

function addRepositioner(constref _repositioner: TRepositioner): TRepositioner;
begin
    //log('reposition: %s', [_repositioner.ClassName]);
    Result := myRespositionerList.add(_repositioner.controlObj, _repositioner);
end;

function repositioner(_control: TControl): TRepositioner;
var
    i: integer;
begin
    Result := nil;
    if myRespositionerList.Find(ptrInt(_control), i) then
        Result := myRespositionerList.Data[i];
end;

procedure TRepositioner.start(X, Y: integer);
begin
    //top  := Y;
    //left := X;
	resetChanges;
end;

procedure TRepositioner.incChange;
begin
    inc(myChange);
end;

function TRepositioner.getXOffset: integer;
begin
    Result := myXOffset;
end;

function TRepositioner.getYOffset: integer;
begin
    Result := myYOffset;
end;

function TRepositioner.changed: boolean;
begin
    Result := myChange > 0;
end;

procedure TRepositioner.resetChanges;
begin
    myChange:=0;
end;

function TRepositioner.getBottom: integer;
begin
    Result := top + Height;
end;

function TRepositioner.getFocused: boolean;
begin
    Result:= myFocused;
end;

function TRepositioner.getHeight: integer;
begin
    Result := -1;
end;

function TRepositioner.getLeft: integer;
begin
    Result := 1;
end;

function TRepositioner.getMoving: boolean;
begin
    Result := mySelState = rssMoving;
end;

function TRepositioner.getResizing: boolean;
begin
    Result := mySelState = rssResize;
end;

function TRepositioner.getRight: integer;
begin
    Result := left + Width;
end;

function TRepositioner.getRotating: boolean;
begin
    Result := myselState >= rssRotate;
end;

function TRepositioner.getSelected: boolean;
begin
    Result := myselState >= rssSelected;
end;

function TRepositioner.getTop: integer;
begin
    Result := 1;
end;

function TRepositioner.getWidth: integer;
begin
    Result := 1;
end;

function TRepositioner.getX1: integer;
begin
    Result := myX1;
end;

function TRepositioner.getX2: integer;
begin
    Result := myX2;
end;

function TRepositioner.getY1: integer;
begin
    Result := myY1;
end;

function TRepositioner.getY2: integer;
begin
    Result := myY2;
end;

procedure TRepositioner.setBottom(const _value: integer);
begin
    Height := Abs(_value - top);
end;

procedure TRepositioner.setFocused(const _value: boolean);
begin
    myFocused := _value;
    case myFocused of
    	True: begin
            if mySelState < rssSelected then
                mySelState := rssSelected;
          //  if (myControl is TWinControl) then begin
          //      with myControl as TWinControl do begin
          //          If CanSetFocus then SetFocus;
		        //end;
          //  end;
		end;
		False: begin
            mySelState := rssUnselected;
		end;
	end;
    visualizeSelState(mySelState);
end;

procedure TRepositioner.setHeight(const _value: integer);
begin

end;

procedure TRepositioner.setLeft(const _value: integer);
begin

end;

procedure TRepositioner.setMoving(const _value: boolean);
begin
    case _value of
        True:
        begin
        	mySelState  := rssMoving;
			resetChanges;
		end;
		False: mySelState := rssSelected;
    end;
    visualizeSelState(mySelState);
end;

procedure TRepositioner.setResizing(const _value: boolean);
begin
    case
        _value of
        True: begin
            myselState := rssResize;
            myPrevX    := 0;
            myPrevY    := 0;
            myX1       := left   - XOffset;
            myX2       := right  - XOffset;
            myY1       := top    - YOffset;
            myY2       := bottom - YOffset;
			resetChanges;
        end;
        False: myselState := rssSelected;
    end;
    visualizeSelState(mySelState);
end;

procedure TRepositioner.setRight(const _value: integer);
begin
    Width := Abs(_value - left);
end;

procedure TRepositioner.setRotating(const _value: boolean);
begin
    case _value of
        True: begin
            mySelState  := rssRotate;
			resetChanges
		end;
		False: mySelState := rssSelected;
    end;
    visualizeSelState(mySelState);
end;

procedure TRepositioner.setSelected(const _value: boolean);
begin
    case _value of
        True: begin
            if myselState < rssSelected then
                myselState := rssSelected;
        end;
        False: begin
            myselState := rssUnselected;
        end;
    end;
    visualizeSelState(mySelState);
end;

procedure TRepositioner.setTop(const _value: integer);
begin

end;

procedure TRepositioner.setWidth(const _value: integer);
begin

end;

procedure TRepositioner.setX1(const _value: integer);
begin
    myX1 := _value;
    left := myX1 + XOffset;
end;

procedure TRepositioner.setX2(const _value: integer);
begin
    myX2  := _value;
    right := myX2 + XOffset;
end;

procedure TRepositioner.SetXOffset(const _value: integer);
begin
	if myXOffset=_value then Exit;
	myXOffset:=_value;
end;

procedure TRepositioner.setY1(const _value: integer);
begin
    myY1 := _value;
    top  := myY1 + YOffset;
end;

procedure TRepositioner.setY2(const _value: integer);
begin
    myY2   := _value;
    bottom := myY2 + YOffset;
end;

procedure TRepositioner.SetYOffset(const _value: integer);
begin
	if myYOffset=_value then Exit;
	myYOffset:=_value;
end;

function TRepositioner.canResize: boolean;
begin
    Result := false;
end;

function TRepositioner.canRepos: boolean;
begin
    Result := false;
end;

procedure TRepositioner.resize(X, Y: integer);
var
    _deltaX, _deltaY: integer;
begin
    if not enabled then exit;
    if not canResize then exit;

    case myselState of
        rssResize: begin
            if myPrevX = 0 then
            begin
                myPrevX := X;
                myPrevY := Y;
            end;

            _deltaX := X - myPrevX;
            _deltaY := Y - myPrevY;

            X2      := X2 + _deltaX;
            Y2      := Y2 + _deltaY;
            myPrevX := X;
            myPrevY := Y;
            incChange;
        end;
    end;
end;

procedure TRepositioner.repos(X, Y: integer);
var
    _deltaX, _deltaY: integer;
begin
    if not enabled then exit;
    if not canRepos then exit;

    if not moving then
    begin
        moving  := True;
        myPrevX := X;
        myPrevY := Y;
        myX1 := left   - XOffset;
        myX2 := right  - XOffset;
        myY1 := top    - YOffset;
        myY2 := bottom - YOffset;
    end;

    _deltaX := X - myPrevX;
    _deltaY := Y - myPrevY;

    X1      := X1 + _deltaX;
    Y1      := Y1 + _deltaY;
    //X2      := X2 + _deltaX;
    //Y2      := Y2 + _deltaY;

    incChange;
end;

procedure TRepositioner.moveX(_dx: integer);
begin
	X1:= X1 + _dx;
    X2:= X2 + _dx;
    incChange;
end;

procedure TRepositioner.moveY(_dy: integer);
begin
	Y1:= Y1 + _dy;
    Y2:= Y2 + _dy;
    incChange;
end;

procedure TRepositioner.sizeW(_dw: integer);
begin
	X2 := X2 + _dw;
    incChange;
end;

procedure TRepositioner.sizeH(_dh: integer);
begin
	Y2 := Y2 + _dh;
    incChange;
end;

procedure TRepositioner.visualizeSelState(_selState: TReposSelectState);
begin
    // nothing here
end;

function TRepositioner.toggleState: TRepositioner;
begin
    Result:= Self;
    case selState of
        //rssUnselected: 	_ma.selected := True;
        rssSelected: 	resizing := True;
        rssMoving: 		selected := True;
        rssResize: 		selected := true;
        rssRotate: 		selected := True;
    end;
    visualizeSelState(selState);
end;


procedure TRepositioner.OnControlClick(Sender: TObject);
begin
    if assigned(myOnClick) then myOnClick(Sender);
    if not enabled then exit;
end;


procedure TRepositioner.OnControlMouseDown(Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    if assigned(myOnMouseDown) then myOnMouseDown(Sender, Button, Shift, X, Y);
    if not enabled then exit;
    // Todo

end;
procedure TRepositioner.OnControlMouseUp(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
    if assigned(myOnMouseUp) then myOnMouseUp(Sender, Button, Shift, X, Y);
    if not enabled then exit;
end;

procedure TRepositioner.OnControlMouseEnter(Sender: TObject);
begin
    if assigned(myOnMouseEnter) then myOnMouseEnter(Sender);
    if not enabled then exit;
    Focused := true;
end;

procedure TRepositioner.OnControlMouseMove(Sender: TObject; Shift: TShiftState;
	X, Y: integer);
begin
    if assigned(myOnMouseMove) then myOnMouseMove(Sender, Shift, X, Y);

    if not enabled then exit;

    case selState of
        rssUnselected: ;

        rssSelected, rssMoving:
        begin
            if (ssLeft in Shift) then
            begin
                repos(X, Y);
            end
            else if moving then moving := False;
        end;

        rssResize: begin
            if (ssLeft in Shift) then
            begin
                resize(X, Y);
            end;
        end;

        rssRotate: ;
    end;
end;

procedure TRepositioner.OnControlMouseLeave(Sender: TObject);
begin
    if assigned(myOnMouseLeave) then myOnMouseLeave(Sender);

    if not enabled then exit;

    case selState of
        rssUnselected: ;
        rssSelected: ;
        rssMoving: begin
            //moving   := False;
		end;
		rssResize: resizing := False;
        rssRotate: rotating := False;
    end;
    focused:=False;

end;

procedure TRepositioner.OnControlKeyDown(Sender: TObject; var Key: word;
	Shift: TShiftState);
begin
    if not enabled then exit;
end;

function TRepositioner.controlObj: TControl;
begin
    Result := nil;
end;

procedure TRepositioner.FPOObservedChanged(ASender: TObject;
	Operation: TFPObservedOperation; Data: Pointer);
begin

    case Operation of
    	ooChange: begin

		end;

        ooFree: begin
            Free; // Destroy this object
		end;

        ooAddItem: begin

		end;

        ooDeleteItem: begin

		end;

        ooCustom: begin

		end;
    end;
end;

{ GRepositioner }

function GRepositioner.getXOffset: integer;
begin
    if assigned(myControl.Parent) then
        myXOffset := myControl.Parent.Left
    else
        myXOffset := 0;

    Result := myXOffset;
end;

function GRepositioner.getYOffset: integer;
begin
    if assigned(myControl.Parent) then
        myYOffset := myControl.Parent.Top
    else
        myYOffset := 0;

    Result := myYOffset;
end;

function GRepositioner.getHeight: integer;
begin
	Result := myControl.Height;
end;

function GRepositioner.getLeft: integer;
begin
    Result := myControl.Left;
end;

function GRepositioner.getTop: integer;
begin
    Result := myControl.Top;
end;

function GRepositioner.getWidth: integer;
begin
	Result := myControl.width;
end;

procedure GRepositioner.setHeight(const _value: integer);
begin
	myControl.Height := _value;
end;

procedure GRepositioner.setLeft(const _value: integer);
begin
	myControl.Left := _value;
end;

procedure GRepositioner.setTop(const _value: integer);
begin
    myControl.Top := _value;
end;

procedure GRepositioner.setWidth(const _value: integer);
begin
	myControl.Width := _value;
end;

procedure GRepositioner.visualizeSelState(_selState: TReposSelectState);
begin
    case _selState of
        rssUnselected: begin
          MyControl.Color := clWhite;
        end;

        rssSelected: begin
            MyControl.Color := clWhite;
        end;

        rssMoving: begin
            MyControl.Color := clYellow;
        end;

        rssResize: begin
            MyControl.Color := clRed;
        end;

        rssRotate: begin
            MyControl.Color := clLime;
        end;
    end;
end;

function GRepositioner.canResize: boolean;
begin
	Result := control.Align = alNone;
end;

function GRepositioner.canRepos: boolean;
begin
	Result := control.Align = alNone;
end;

constructor GRepositioner.Create(constref _control: C);
begin
    //Log('Calling %s.Create()', [ClassName]);
    myControl := _control;
    myControl.FPOAttachObserver(Self);
    initControl(_control);
end;

function GRepositioner.initControl(constref _control: C): C;
begin
     Result := _control;

    // Assign previous event handlers
     myOnClick             := _control.OnClick;
     myOnMouseDown         := _control.OnMouseDown;
     myOnMouseUp           := _control.OnMouseUp;
     myOnMouseEnter        := _control.OnMouseEnter;
     myOnMouseMove         := _control.OnMouseMove;
     myOnMouseLeave        := _control.OnMouseLeave;

    // Take control of the events
     _control.OnClick      := @OnControlClick;
     _control.OnMouseDown  := @OnControlMouseDown;
     _control.OnMouseUp    := @OnControlMouseUp;
     _control.OnMouseEnter := @OnControlMouseEnter;
     _control.OnMouseMove  := @OnControlMouseMove;
     _control.OnMouseLeave := @OnControlMouseLeave;

     myX1 := _control.Left;
     myX2 := _control.Left + _control.Width;

     myY1 := _control.Top;
     myY2 := _control.Top + _control.Height;

     enabled := true;
end;

function GRepositioner.controlObj: TControl;
begin
	Result:= control;
end;

constructor TRepositionerList.Create;
begin
    inherited Create;
    watcher := TWatcher.Create;
    watcher.owner := self;
end;

destructor TRepositionerList.Destroy;
begin
    watcher.Free;
	inherited Destroy;
end;

{ TRepositionerList }
function TRepositionerList.add(constref _control: TControl; constref
	_repositioner: TRepositioner): TRepositioner;
begin
    Result := _repositioner;
    Add(ptrInt(_control), _repositioner);
    _control.FPOAttachObserver(watcher);
end;

{ TRepositionerList.TWatcher }

procedure TRepositionerList.TWatcher.FPOObservedChanged(ASender: TObject;
	Operation: TFPObservedOperation; Data: Pointer);
var
    i: integer;
	p: PtrInt;
begin
    case Operation of
    	ooChange: ;
        ooFree: begin
            p := ptrInt(ASender);
            i := owner.IndexOf(p);
            if i > -1 then begin
                owner.delete(i);
			end;
		end;
        ooAddItem: ;
        ooDeleteItem: ;
        ooCustom: ;
    end;
end;

initialization
    myRespositionerList := TRepositionerList.Create;

finalization
    myRespositionerList.Free;

end.

