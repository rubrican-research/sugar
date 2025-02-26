unit sugar.repositionui;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
    sugar.animateui, sugar.collections, fgl;

type

	{ TUIMover }
    TUIMover  = class(TPersistent, IFPObserver)
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
        function toggleState: TUIMover;virtual;

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

	{ GUIMover }

    generic GUIMover <C: TControl> = class(TUIMover)
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
        procedure repos(X, Y: integer);override;

    public
        // Exposes the control
        property control: C read myControl;
        constructor Create  (constref _control: C);
        function initControl(constref _control: C): C; virtual;
        function controlObj: TControl; override;
	end;

    TPanelMover    = class(specialize GUIMover<TPanel>);
    TLabelMover    = class(specialize GUIMover<TLabel>);
    TEditMover     = class(specialize GUIMover<TEdit>);
    TCheckBoxMover = class(specialize GUIMover<TCheckBox>);
    TListBoxMover  = class(specialize GUIMover<TListBox>);
    TButtonMover   = class(specialize GUIMover<TButton>);
    TFrameMover    = class(specialize GUIMover<TFrame>);

    TUIMoverClass = class of TUIMover;

	{ TUIMoverList }

    TUIMoverList = class(specialize TFPGMap<ptrInt, TUIMover>)
    private type
			{ TWatcher }
            TWatcher = class(TPersistent, IFPObserver)
			public
                owner : TUIMoverList;
				procedure FPOObservedChanged(ASender: TObject;
					Operation: TFPObservedOperation; Data: Pointer);
            end;
    private
        watcher: TWatcher;
    public
        constructor Create;
        destructor Destroy; override;

        function add(constref _control: TControl; constref _repositioner: TUIMover): TUIMover; overload;
    end;
    {Global list of repositioners}
    function addUIMover(constref _control: TControl): TUIMover;       // Creates a TRepositioner object for the control and adds it to the lsit.
    function addUIMover(constref _controls: TControlArray): TUIMover; // convenience. Add multiple control repositioners
    function addUIMover(constref _repositioner: TUIMover): TUIMover;  // If you have a custom repositioner, you can add that object to the list
    function getUIMover(_control: TControl): TUIMover;                // Finds and returns the repositioner object for the control. Use this when you want to change repositioner properties.

implementation

uses
    Math, sugar.logger;
var
    myRespositionerList : TUIMoverList = nil;

function addUIMover(constref _control: TControl): TUIMover;
begin
    if _control is TLabel then
        Result := addUIMover(TLabelMover.Create(TLabel(_control)))

    else if _control is TEdit then
        Result := addUIMover(TEditMover.Create(TEdit(_control)))

    else if _control is TPanel then
        Result := addUIMover(TPanelMover.Create(TPanel(_control)))

    else if _control is TCheckBox then
        Result := addUIMover(TCheckBoxMover.Create(TCheckBox(_control)))

    else if _control is TListBox then
        Result := addUIMover(TListBoxMover.Create(TListBox(_control)))

    else if _control is TButton then
            Result := addUIMover(TButtonMover.Create(TButton(_control)))

    else if _control is TFrame then
            Result := addUIMover(TFrameMover.Create(TFrame(_control)))
    else
        raise Exception.Create(Format('There is currently no TRepositioner implementation for %d',[_control.ClassName]));

end;

function addUIMover(constref _controls: TControlArray): TUIMover;
var
	c: TControl;
begin
    for c in _controls do
        addUIMover(c);
end;

function addUIMover(constref _repositioner: TUIMover): TUIMover;
begin
    //log('reposition: %s', [_repositioner.ClassName]);
    Result := myRespositionerList.add(_repositioner.controlObj, _repositioner);
end;

function getUIMover(_control: TControl): TUIMover;
var
    i: integer;
begin
    Result := nil;
    if myRespositionerList.Find(ptrInt(_control), i) then
        Result := myRespositionerList.Data[i];
end;

procedure TUIMover.start(X, Y: integer);
begin
    //top  := Y;
    //left := X;
	resetChanges;
end;

procedure TUIMover.incChange;
begin
    inc(myChange);
end;

function TUIMover.getXOffset: integer;
begin
    Result := myXOffset;
end;

function TUIMover.getYOffset: integer;
begin
    Result := myYOffset;
end;

function TUIMover.changed: boolean;
begin
    Result := myChange > 0;
end;

procedure TUIMover.resetChanges;
begin
    myChange:=0;
end;

function TUIMover.getBottom: integer;
begin
    Result := top + Height;
end;

function TUIMover.getFocused: boolean;
begin
    Result:= myFocused;
end;

function TUIMover.getHeight: integer;
begin
    Result := -1;
end;

function TUIMover.getLeft: integer;
begin
    Result := 1;
end;

function TUIMover.getMoving: boolean;
begin
    Result := mySelState = rssMoving;
end;

function TUIMover.getResizing: boolean;
begin
    Result := mySelState = rssResize;
end;

function TUIMover.getRight: integer;
begin
    Result := left + Width;
end;

function TUIMover.getRotating: boolean;
begin
    Result := myselState >= rssRotate;
end;

function TUIMover.getSelected: boolean;
begin
    Result := myselState >= rssSelected;
end;

function TUIMover.getTop: integer;
begin
    Result := 1;
end;

function TUIMover.getWidth: integer;
begin
    Result := 1;
end;

function TUIMover.getX1: integer;
begin
    Result := myX1;
end;

function TUIMover.getX2: integer;
begin
    Result := myX2;
end;

function TUIMover.getY1: integer;
begin
    Result := myY1;
end;

function TUIMover.getY2: integer;
begin
    Result := myY2;
end;

procedure TUIMover.setBottom(const _value: integer);
begin
    Height := Abs(_value - top);
end;

procedure TUIMover.setFocused(const _value: boolean);
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

procedure TUIMover.setHeight(const _value: integer);
begin

end;

procedure TUIMover.setLeft(const _value: integer);
begin

end;

procedure TUIMover.setMoving(const _value: boolean);
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

procedure TUIMover.setResizing(const _value: boolean);
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

procedure TUIMover.setRight(const _value: integer);
begin
    Width := Abs(_value - left);
end;

procedure TUIMover.setRotating(const _value: boolean);
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

procedure TUIMover.setSelected(const _value: boolean);
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

procedure TUIMover.setTop(const _value: integer);
begin

end;

procedure TUIMover.setWidth(const _value: integer);
begin

end;

procedure TUIMover.setX1(const _value: integer);
begin
    myX1 := _value;
    left := myX1 + XOffset;
end;

procedure TUIMover.setX2(const _value: integer);
begin
    myX2  := _value;
    right := myX2 + XOffset;
end;

procedure TUIMover.SetXOffset(const _value: integer);
begin
	if myXOffset=_value then Exit;
	myXOffset:=_value;
end;

procedure TUIMover.setY1(const _value: integer);
begin
    myY1 := _value;
    top  := myY1 + YOffset;
end;

procedure TUIMover.setY2(const _value: integer);
begin
    myY2   := _value;
    bottom := myY2 + YOffset;
end;

procedure TUIMover.SetYOffset(const _value: integer);
begin
	if myYOffset=_value then Exit;
	myYOffset:=_value;
end;

function TUIMover.canResize: boolean;
begin
    Result := false;
end;

function TUIMover.canRepos: boolean;
begin
    Result := false;
end;

procedure TUIMover.resize(X, Y: integer);
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

procedure TUIMover.repos(X, Y: integer);
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

procedure TUIMover.moveX(_dx: integer);
begin
	X1:= X1 + _dx;
    X2:= X2 + _dx;
    incChange;
end;

procedure TUIMover.moveY(_dy: integer);
begin
	Y1:= Y1 + _dy;
    Y2:= Y2 + _dy;
    incChange;
end;

procedure TUIMover.sizeW(_dw: integer);
begin
	X2 := X2 + _dw;
    incChange;
end;

procedure TUIMover.sizeH(_dh: integer);
begin
	Y2 := Y2 + _dh;
    incChange;
end;

procedure TUIMover.visualizeSelState(_selState: TReposSelectState);
begin
    // nothing here
end;

function TUIMover.toggleState: TUIMover;
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


procedure TUIMover.OnControlClick(Sender: TObject);
begin
    if assigned(myOnClick) then myOnClick(Sender);
    if not enabled then exit;
end;


procedure TUIMover.OnControlMouseDown(Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    if assigned(myOnMouseDown) then myOnMouseDown(Sender, Button, Shift, X, Y);
    if not enabled then exit;
    // Todo

end;
procedure TUIMover.OnControlMouseUp(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
    if assigned(myOnMouseUp) then myOnMouseUp(Sender, Button, Shift, X, Y);
    if not enabled then exit;
end;

procedure TUIMover.OnControlMouseEnter(Sender: TObject);
begin
    if assigned(myOnMouseEnter) then myOnMouseEnter(Sender);
    if not enabled then exit;
    Focused := true;
end;

procedure TUIMover.OnControlMouseMove(Sender: TObject; Shift: TShiftState;
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

procedure TUIMover.OnControlMouseLeave(Sender: TObject);
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

procedure TUIMover.OnControlKeyDown(Sender: TObject; var Key: word;
	Shift: TShiftState);
begin
    if not enabled then exit;
end;

function TUIMover.controlObj: TControl;
begin
    Result := nil;
end;

procedure TUIMover.FPOObservedChanged(ASender: TObject;
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

{ GUIMover }

function GUIMover.getXOffset: integer;
begin
    if assigned(myControl.Parent) then
        myXOffset := myControl.Parent.Left
    else
        myXOffset := 0;

    Result := myXOffset;
end;

function GUIMover.getYOffset: integer;
begin
    if assigned(myControl.Parent) then
        myYOffset := myControl.Parent.Top
    else
        myYOffset := 0;

    Result := myYOffset;
end;

function GUIMover.getHeight: integer;
begin
	Result := myControl.Height;
end;

function GUIMover.getLeft: integer;
begin
    Result := myControl.Left;
end;

function GUIMover.getTop: integer;
begin
    Result := myControl.Top;
end;

function GUIMover.getWidth: integer;
begin
	Result := myControl.width;
end;

procedure GUIMover.setHeight(const _value: integer);
begin
	myControl.Height := _value;
end;

procedure GUIMover.setLeft(const _value: integer);
begin
	myControl.Left := _value;
end;

procedure GUIMover.setTop(const _value: integer);
begin
    myControl.Top := _value;
end;

procedure GUIMover.setWidth(const _value: integer);
begin
	myControl.Width := _value;
end;

procedure GUIMover.visualizeSelState(_selState: TReposSelectState);
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

function GUIMover.canResize: boolean;
begin
	Result := control.Align = alNone;
end;

function GUIMover.canRepos: boolean;
begin
	Result := control.Align = alNone;
end;

procedure GUIMover.repos(X, Y: integer);
begin
    myControl.BringToFront;
	inherited repos(X, Y);
end;

constructor GUIMover.Create(constref _control: C);
begin
    //Log('Calling %s.Create()', [ClassName]);
    myControl := _control;
    myControl.FPOAttachObserver(Self);
    initControl(_control);
end;

function GUIMover.initControl(constref _control: C): C;
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

function GUIMover.controlObj: TControl;
begin
	Result:= control;
end;

constructor TUIMoverList.Create;
begin
    inherited Create;
    watcher := TWatcher.Create;
    watcher.owner := self;
end;

destructor TUIMoverList.Destroy;
begin
    watcher.Free;
	inherited Destroy;
end;

{ TUIMoverList }
function TUIMoverList.add(constref _control: TControl; constref
	_repositioner: TUIMover): TUIMover;
begin
    Result := _repositioner;
    Add(ptrInt(_control), _repositioner);
    _control.FPOAttachObserver(watcher);
end;

{ TUIMoverList.TWatcher }

procedure TUIMoverList.TWatcher.FPOObservedChanged(ASender: TObject;
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
    myRespositionerList := TUIMoverList.Create;

finalization
    myRespositionerList.Free;

end.

