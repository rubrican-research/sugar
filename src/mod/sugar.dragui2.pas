unit sugar.dragui2;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
    sugar.animateui, sugar.collections;

type

	{ TRepositioner }

    generic TRepositioner <C: TControl> = class(TPersistent, IFPObserver)
    //TRepositioner = class(TPersistent, IFPObserver)
    public type TReposSelectState = (rssUnselected, rssSelected, rssMoving, rssResize, rssRotate);
    protected
        myControl: C;
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

		function getXOffset: integer;
		function getYOffset: integer;
        procedure incChange;
        function changed: boolean;

		procedure resetChanges; // Changes

        function getBottom: integer;
		function getFocused: boolean;
		function getHeight: integer;
		function getLeft: integer;
		function getMoving: boolean;
		function getResizing: boolean;
		function getRight: integer;
		function getRotating: boolean;
		function getSelected: boolean;
		function getTop: integer;
		function getWidth: integer;
		function getX1: integer;
		function getX2: integer;
		function getY1: integer;
		function getY2: integer;
		procedure setBottom(const _value: integer);
		procedure setFocused(const _value: boolean);
		procedure setHeight(const _value: integer);
		procedure setLeft(const _value: integer);
		procedure setMoving(const _value: boolean);
		procedure setResizing(const _value: boolean);
		procedure setRight(const _value: integer);
		procedure setRotating(const _value: boolean);
		procedure setSelected(const _value: boolean);
		procedure setTop(const _value: integer);
		procedure setWidth(const _value: integer);
		procedure setX1(const _value: integer);
		procedure setX2(const _value: integer);
		procedure SetXOffset(const _value: integer);
		procedure setY1(const _value: integer);
		procedure setY2(const _value: integer);
		procedure SetYOffset(const _value: integer);
        procedure start(X, Y: integer);
        procedure resize(X, Y: integer); overload;
        procedure repos(X, Y: integer);
        procedure moveX(_dx: integer); // Change X by dx
        procedure moveY(_dy: integer); // Change Y by dy
        procedure sizeW(_dw: integer); // Change width by dw
        procedure sizeH(_dh: integer); // Change height by dh
        procedure visualizeSelState(_selState: TReposSelectState);
    public
        {EVENT HANDLERS}
        function toggleState: TRepositioner;
        procedure OnControlClick(Sender: TObject);
        procedure OnControlDblClick(Sender: TObject);
        procedure OnControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure OnControlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure OnControlMouseEnter(Sender: TObject);
        procedure OnControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
        procedure OnControlMouseLeave(Sender: TObject);
        procedure OnControlKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);

    public
		procedure FPOObservedChanged(ASender: TObject;
			Operation: TFPObservedOperation; Data: Pointer);
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

    public
        constructor Create(constref _control: C);
        //constructor Create(constref _control: TPanel);
        function initControl(constref _control: C): C;
        //function initControl(constref _control: TPanel): TPanel;

	end;

    TLayoutManager = class
        container: TWinControl;
	end;

    TPanelRepositioner = class(specialize TRepositioner<TPanel>);
    TLabelRepositioner = class(specialize TRepositioner<TLabel>);
    TEditRepositioner  = class(specialize TRepositioner<TEdit>);
    TCheckBoxRepositioner = class(specialize TRepositioner<TCheckBox>);

    function insertControl(_container: TWinControl; _control: TWinControl) : TWinControl;


implementation

uses
    Math;

function insertControl(_container: TWinControl; _control: TWinControl
	): TWinControl;
begin

end;


procedure TRepositioner.start(X, Y: integer);
begin
    top  := Y;
    left := X;
	resetChanges;
end;

procedure TRepositioner.incChange;
begin
    inc(myChange);
end;

function TRepositioner.getXOffset: integer;
begin
    if assigned(myControl.Parent) then
        myXOffset := myControl.Parent.Left
    else
        myXOffset := 0;

    Result := myXOffset;
    //Result := 0;
end;

function TRepositioner.getYOffset: integer;
begin
    if assigned(myControl.Parent) then
        myYOffset := myControl.Parent.Top
    else
        myYOffset := 0;

    Result := myYOffset;
    //Result := 0;
end;

function TRepositioner.changed: boolean;
begin
    Result := myChange>0;
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
    Result := myControl.Height;
end;

function TRepositioner.getLeft: integer;
begin
    Result := myControl.left;
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
    Result := myControl.Top;
end;

function TRepositioner.getWidth: integer;
begin
    Result := myControl.width;
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
    myControl.Height := _value;
end;

procedure TRepositioner.setLeft(const _value: integer);
begin
    myControl.Left := _value
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
    myControl.Top := _value;
end;

procedure TRepositioner.setWidth(const _value: integer);
begin
    myControl.Width := _value;
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

procedure TRepositioner.resize(X, Y: integer);
var
    _deltaX, _deltaY: integer;
begin
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
            inc(myChange);
        end;
   // 	else
   // 	begin
   //     	X2 := X;
   //     	Y2 := Y;
			//inc(myChange);
   // 	end;
    end;
end;

procedure TRepositioner.repos(X, Y: integer);
var
    _deltaX, _deltaY: integer;
begin
    if not moving then
    begin
        moving  := True;
        myPrevX := X;
        myPrevY := Y;
    end;

    _deltaX := X - myPrevX;
    _deltaY := Y - myPrevY;

    X1      := X1 + _deltaX;
    Y1      := Y1 + _deltaY;
    X2      := X2 + _deltaX;
    Y2      := Y2 + _deltaY;
    Inc(myChange);
end;

procedure TRepositioner.moveX(_dx: integer);
begin
	X1:= X1 + _dx;
    X2:= X2 + _dx;
    inc(myChange);
end;

procedure TRepositioner.moveY(_dy: integer);
begin
	Y1:= Y1 + _dy;
    Y2:= Y2 + _dy;
    inc(myChange);
end;

procedure TRepositioner.sizeW(_dw: integer);
begin
	X2 := X2 + _dw;
    inc(myChange);
end;

procedure TRepositioner.sizeH(_dh: integer);
begin
	Y2 := Y2 + _dh;
    inc(myChange);
end;

procedure TRepositioner.visualizeSelState(_selState: TReposSelectState);
begin
    case _selState of
        rssUnselected: begin
          MyControl.Color := clWhite;
        end;

        rssSelected, rssMoving: begin
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

end;

procedure TRepositioner.OnControlDblClick(Sender: TObject);
begin
    toggleState;
end;

procedure TRepositioner.OnControlMouseDown(Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    // Todo

end;
procedure TRepositioner.OnControlMouseUp(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TRepositioner.OnControlMouseEnter(Sender: TObject);
begin
    Focused := true;
end;

procedure TRepositioner.OnControlMouseMove(Sender: TObject; Shift: TShiftState;
	X, Y: integer);
begin
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
    case selState of
        rssUnselected: ;
        rssSelected: ;
        rssMoving: moving   := False;
        rssResize: resizing := False;
        rssRotate: rotating := False;
    end;
    focused:=False;
end;

procedure TRepositioner.OnControlKeyDown(Sender: TObject; var Key: word;
	Shift: TShiftState);
begin

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

constructor TRepositioner.Create(constref _control: C);
//constructor TRepositioner.Create(constref _control: TPanel);
begin
    inherited Create;
    myControl := _control;
    myControl.FPOAttachObserver(Self);
    initControl(_control);
end;

function TRepositioner.initControl(constref _control: C): C;
//function TRepositioner.initControl(constref _control: TPanel): TPanel;
begin
     Result := _control;
     _control.OnClick      := @OnControlClick;
     //_control.OnDblClick   := @OnControlDblClick;
     _control.OnMouseDown  := @OnControlMouseDown;
     _control.OnMouseUp    := @OnControlMouseUp;
     _control.OnMouseEnter := @OnControlMouseEnter;
     _control.OnMouseMove  := @OnControlMouseMove;
     _control.OnMouseLeave := @OnControlMouseLeave;

     myX1 := _control.Left;
     myX2 := _control.Left + _control.Width;
     myY1 := _control.Top;
     myY2 := _control.Top + _control.Height;

end;

end.

