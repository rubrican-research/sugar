unit sugar.dragui;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, sugar.uihelper, sugar.animateui;

type
    TDragDirection = (dragDirNone, dragDirLeft, dragDirRight, dragDirUp, dragDirDown);
    TDraggingDirection = set of TDragDirection;
    function _(_d: TDragDirection): string; overload;
type
	{ TDragObjectMover }

    TDragObjectMover = class
    public const NOVAL = MaxLongint;
    protected
        myDraggingDirection : TDraggingDirection;
        myX, myY: integer;
        mydX, mydY: Integer;
        prevdX, prevdY: Integer;
        function setDirection(const _dir: TDragDirection): TDragObjectMover;

    public
        procedure reset;
        function move(Sender, Source: TObject; X, Y: Integer;
			State: TDragState) : TDragObjectMover; // for chaining
    public
        function movedLeft: boolean;
        function movedRight: boolean;
        function movedHorizontal: boolean;
        procedure clearHorizontalMovement;

        function movedUp: boolean;
        function movedDown: boolean;
        function movedVertical: boolean;
        procedure clearVerticalMovement;
    public
        property dragDirection: TDraggingDirection read myDraggingDirection;
        property dX: integer read myDX;
        property dY: integer read myDY;
	end;


    TDragAcceptProc = function (Sender, Source: TObject; X, Y: Integer; State: TDragState): boolean of object; {This is similar to the TDragOverEvent}

    EDragRepositionUIBreak = class(Exception); // Raise this error object inside the objects drag event handlers if you want to break the default handling TDragRepositionUI;

	{ TDragRepositionUI }
    {DESIGN NOTES:
            AUTOMATIC FREEING
            You shouldn't free an object of this class.
            It is freed when the container is freed because
            it implements IFPObserver

            OBJECT EVENT HANDLERS CALLED
            If Child objects have drag event handlers assigned,
            they WILL be called before TDragRepositionUI event handlers are called.
            To prevent the functionality of TDragREpositionUI from running,
            you can simply raise EDragRepositionUIBreak in the object's event handler
    }
    generic TDragRepositionUI <ChildControl: TControl; ParentControl: TWinControl> = class(TPersistent, IFPObserver)
    private
		myShowAnimation: boolean;
		myenabled: boolean;
    private
		myanimationSpeed: NAnimationSpeed;
		mycolorDefault: TColor;
		mycolorDragNotAllowed: TColor;
		mycolorDragOver: TColor;
		mycolorDragStart: TColor;
		mycolorHover: TColor;
		mycolorSelected: TColor;
		mycontainer: ParentControl;
		myform: TForm;
 type

		{ TLocalDragState }

        TLocalDragState = class(TPersistent)
        public
            control : ChildControl;
            i       : integer; // current index
            pos     : TPoint;
            constructor Create;
        end;

	    function getDragAcceptProc: TDragAcceptProc;
		procedure setanimationSpeed(const _value: NAnimationSpeed);
		procedure setcolorDefault(const _value: TColor);
		procedure setcolorDragNotAllowed(const _value: TColor);
		procedure setcolorDragOver(const _value: TColor);
		procedure setcolorDragStart(const _value: TColor);
		procedure setcolorHover(const _value: TColor);
		procedure setcolorSelected(const _value: TColor);
		procedure setcontainer(const _value: ParentControl);
		procedure setform(const _value: TForm);
		procedure setShowAnimation(const _value: boolean);
	    procedure setDragAcceptProc(const _value: TDragAcceptProc);
		procedure setenabled(const _value: boolean);

    protected
        myDragHandles    : TControlArray;
        myPrevOnStartDrag: TStartDragEvent;
        myPrevOnDragOver : TDragOverEvent;
        myPrevOnDragDrop : TDragDropEvent;
        myPrevOnEndDrag  : TEndDragEvent;

        myDragAcceptProc: TDragAcceptProc;
        myDragState     : TLocalDragState;
        myMouseWiz        : TMouseWizard;
        myGlobalMouseWiz  : TGlobalMouseWizard;
        myDragObjectMover : TDragObjectMover;

		function getChild(_index: cardinal): ChildControl;
        function animationDuration: cardinal; virtual;
        function defaultDragAcceptProc(Sender, Source: TObject; X, Y: Integer; State: TDragState): boolean;

   public
        constructor Create(constref _container: ParentControl; const _childOffset: cardinal);
        destructor Destroy; override;
		procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);

    public
        property form:                TForm read myform write setform;
        property container:           ParentControl read mycontainer write setcontainer;
        property colorDefault:        TColor read mycolorDefault write setcolorDefault;
        property colorHover:          TColor read mycolorHover write setcolorHover;
        property colorDragStart:      TColor read mycolorDragStart write setcolorDragStart;
        property colorDragOver:       TColor read mycolorDragOver write setcolorDragOver;
        property colorDragNotAllowed: TColor read mycolorDragNotAllowed write setcolorDragNotAllowed;
        property colorSelected:       TColor read mycolorSelected write setcolorSelected;
        property animationSpeed:      NAnimationSpeed read myanimationSpeed write setanimationSpeed;


        procedure dragStart(Sender: TObject; var DragObject: TDragObject);
        procedure dragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
        procedure dragDrop(Sender, Source: TObject; X, Y: Integer); {works for both parent and draghandle}
		procedure dragEnd(Sender, Target: TObject; X, Y: Integer);


        procedure dragHandleStart(Sender: TObject; var DragObject: TDragObject);
        procedure dragHandleOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
        procedure dragHandleDrop(Sender, Source: TObject; X, Y: Integer); {works for both parent and draghandle}
		procedure dragHandleEnd(Sender, Target: TObject; X, Y: Integer);


        function insertChild: ChildControl;
        function init: integer;
        function initChild(constref _child: ChildControl): ChildControl;

        procedure addDragHandle(constref _lbl: TLabel);
        procedure addDragHandle(constref _pnl: TPanel);
        procedure addDragHandle(constref _img: TImage);
        procedure addDragHandle(constref _shape: TShape);
        procedure initDragHandles(constref _child: TControl);
    public
         property child[_index: cardinal]: ChildControl read getChild;
         property dragAcceptProc: TDragAcceptProc read getDragAcceptProc write setDragAcceptProc;
         property enabled: boolean read myenabled write setenabled;
         property showAnimation: boolean read myShowAnimation write setShowAnimation;

	end;

    TPanelDragRepositionUI  = class(specialize TDragRepositionUI<TPanel, TWinControl>);
    TScrollboxDragRepositionUI  = class(specialize TDragRepositionUI<TScrollBox, TWinControl>);
    TButtonDragRepositionUI = class(specialize TDragRepositionUI<TButton, TWinControl>);
    TFrameDragRepositionUI  = class(specialize TDragRepositionUI<TFrame, TWinControl>);

var
    dragUIColorDefault  :       TColor = clDefault;
    dragUIColorSelected :       TColor = cl3DLight;
    dragUIColorDragOver :       TColor = cl3DHiLight;
    dragUIColorHover    :       TColor = clCream;
    dragUIColorDragStart:       TColor = cl3DShadow;
    dragUIColorDragNotAllowed:  TColor = clMaroon;




implementation
uses
    LCLType, Math, sugar.logger;

function _(_d: TDragDirection): string;
begin
    case _d of
    	dragDirNone: Result := '';
        dragDirLeft: Result := 'Left';
        dragDirRight:Result := 'Right';
        dragDirUp:   Result := 'Up';
        dragDirDown: Result := 'Down';
    end;
end;

{ TDragObjectMover }

function TDragObjectMover.setDirection(const _dir: TDragDirection
	): TDragObjectMover;
begin
    Result := self;
    case _dir of
    	dragDirNone: myDraggingDirection := [];

        dragDirLeft:  begin
            Exclude(myDraggingDirection, dragDirRight);
            Include(myDraggingDirection, dragDirLeft);
		end;

        dragDirRight: begin
            Exclude(myDraggingDirection, dragDirLeft);
            Include(myDraggingDirection, dragDirRight);
		end;

        dragDirUp: begin
            Exclude(myDraggingDirection, dragDirDown);
            Include(myDraggingDirection, dragDirUp);
		end;

        dragDirDown: begin
            Exclude(myDraggingDirection, dragDirUp);
            Include(myDraggingDirection, dragDirDown);
		end;
    end;
end;

function TDragObjectMover.move(Sender, Source: TObject; X, Y: Integer;
	State: TDragState): TDragObjectMover;
const __Threshold = 20;
var
    newDX, newDY : integer;
begin
    Result := Self;

    if myX = NOVAL then
        newdX := 0
    else
        newdX := X - myX;

    if myY = NOVAL then
        newdY := 0
    else
        newdY := Y - myY;

    if newdX > 0 then      begin
        setDirection(dragDirRight);
        newdX := min(__Threshold, newdX)
	end
	else if newdX < 0 then begin
        setDirection(dragDirLeft);
        newdx := max(-1 * __Threshold, newdX);
	end
    else
        clearHorizontalMovement;

    if newdY > 0 then begin
        setDirection(dragDirDown);
        newdY := min(__Threshold, newdY)
	end
	else if newdY < 0 then begin
        setDirection(dragDirUp);
        newdY := max(-1 * __Threshold, newdY);
	end
    else
        clearVerticalMovement;

    if abs(newDX) = __Threshold then newDX := mydX;
    if abs(newDY) = __Threshold then newDY := mydY;

    myX := X;
    myY := Y;

    prevdX := mydX;
    prevdY := mydY;

    mydX   := newDX;
    mydY   := newDY;
end;

function TDragObjectMover.movedLeft: boolean;
begin
    Result := dragDirLeft in myDraggingDirection;
end;

function TDragObjectMover.movedRight: boolean;
begin
    Result := dragDirRight in myDraggingDirection;
end;

function TDragObjectMover.movedHorizontal: boolean;
begin
    Result := movedLeft or movedRight;
end;

procedure TDragObjectMover.clearHorizontalMovement;
begin
    myDraggingDirection :=  myDraggingDirection - [dragDirLeft,dragDirRight];
end;

function TDragObjectMover.movedUp: boolean;
begin
    Result := dragDirUp in myDraggingDirection;
end;

function TDragObjectMover.movedDown: boolean;
begin
    Result := dragDirDown in myDraggingDirection;
end;

function TDragObjectMover.movedVertical: boolean;
begin
    Result := movedUp or movedDown;
end;

procedure TDragObjectMover.clearVerticalMovement;
begin
    myDraggingDirection :=  myDraggingDirection - [dragDirUp,dragDirDown];
end;

procedure TDragObjectMover.reset;
begin
    myX     := NOVAL;
    myY     := NOVAL;
    mydX      := NOVAL;
    mydY      := NOVAL;
    prevdX  := NOVAL;
    prevdy  := NOVAL;
    myDraggingDirection:= [];
end;



{ TDragRepositionUI }


function TDragRepositionUI.initChild(constref _child: ChildControl): ChildControl;
begin
    Result := _child;
    myPrevOnStartDrag:= Result.OnStartDrag;
    myPrevOnDragOver := Result.OnDragOver;
    myPrevOnDragDrop := Result.OnDragDrop;
    myPrevOnEndDrag  := Result.OnEndDrag;

    Result.Align := alTop;

    Result.DragMode   := dmAutomatic;
    Result.OnStartDrag:= @dragStart;
    Result.OnDragOver := @dragOver;
    Result.OnDragDrop := @dragDrop;
    Result.OnEndDrag  := @dragEnd;

    initDragHandles(_child);

end;

procedure TDragRepositionUI.addDragHandle(constref _lbl: TLabel);
begin
    if not Assigned(_lbl.Parent) then exit;
    if not isParent(container, _lbl) then exit;

    {Take control of the drag handle component. Override any prev behaviours}
    _lbl.DragMode   := dmAutomatic;
    _lbl.OnStartDrag:= @dragHandleStart;
    _lbl.OnDragOver := @dragHandleOver;
    _lbl.OnDragDrop := @dragHandleDrop;
    _lbl.OnEndDrag  := @dragHandleEnd;

end;

procedure TDragRepositionUI.addDragHandle(constref _pnl: TPanel);
begin
    if not Assigned(_pnl.Parent) then exit;
    if not isParent(container, _pnl) then exit;


    {Take control of the drag handle component. Override any prev behaviours}
    _pnl.DragMode   := dmAutomatic;
    _pnl.OnStartDrag:= @dragHandleStart;
    _pnl.OnDragOver := @dragHandleOver;
    _pnl.OnDragDrop := @dragHandleDrop;
    _pnl.OnEndDrag  := @dragHandleEnd;

end;

procedure TDragRepositionUI.addDragHandle(constref _img: TImage);
begin
    if not Assigned(_img.Parent) then exit;
    if not isParent(container, _img) then exit;

    {Take control of the drag handle component. Override any prev behaviours}
    _img.DragMode   := dmAutomatic;
    _img.OnStartDrag:= @dragHandleStart;
    _img.OnDragOver := @dragHandleOver;
    _img.OnDragDrop := @dragHandleDrop;
    _img.OnEndDrag  := @dragHandleEnd;

end;


procedure TDragRepositionUI.addDragHandle(constref _shape: TShape);
begin
    if not Assigned(_shape.Parent) then exit;
    if not isParent(container, _shape) then exit;

    {Take control of the drag handle component. Override any prev behaviours}
    _shape.DragMode   := dmAutomatic;
    _shape.OnStartDrag:= @dragHandleStart;
    _shape.OnDragOver := @dragHandleOver;
    _shape.OnDragDrop := @dragHandleDrop;
    _shape.OnEndDrag  := @dragHandleEnd;
end;

procedure TDragRepositionUI.initDragHandles(constref _child: TControl);
var
	_control: TControl;
	i: Integer;
begin
    if _child is TWinControl then begin
	    for  i := 0 to pred(TWinControl(_child).ControlCount) do begin
	        _control := TWinControl(_child).Controls[i];
	        case _control.ClassName of // Using this instead of IF to enhance readability
				'TLabel':   addDragHandle(TLabel(_control));
				'TPanel':   addDragHandle(TPanel(_control));
				'TImage':   addDragHandle(TImage(_control));
				'TShape':   addDragHandle(TShape(_control));
	        end;
	        initDragHandles(_control);
		end;
    end;
end;


function TDragRepositionUI.animationDuration: cardinal;
begin
    // Result in milliseconds
    case animationSpeed of
    	aspeedInstant: Result := 30;
        aspeedQuick:   Result := 100;
        aspeedNormal:  Result := 160;
        aspeedSlow:    Result := 800;
    end;
end;

function TDragRepositionUI.defaultDragAcceptProc(Sender, Source: TObject; X,
	Y: Integer; State: TDragState): boolean;
begin
    //log('Deciding to accept drop');
    Result := (source  <> sender) and isParent(container, TControl(Sender)) and isParent(container, TControl(Source));
    //case Result of
    //	True:  log('Yes. Please drop');
    //    False: log('Na. Dont drop') ;
    //end;


end;

constructor TDragRepositionUI.Create(constref _container: ParentControl;
	const _childOffset: cardinal);

begin
    inherited Create;
    myDragObjectMover  := TDragObjectMover.Create;
    myGlobalMouseWiz   := TGlobalMouseWizard.Create;

    myMouseWiz         := TMouseWizard.Create;
    myMouseWiz.enabled :=True;

    animationSpeed     := aspeedNormal;
    container          := _container;
    container.FPOAttachObserver(Self);

    Self.colorSelected := dragUIColorSelected;
    Self.colorDefault  := dragUIColorDefault;
    Self.colorDragOver := dragUIColorDragOver;

    Enabled            := true;
    ShowAnimation      := True;

    init;
end;

destructor TDragRepositionUI.Destroy;
begin
    myDragObjectMover.Free;
    myGlobalMouseWiz.Free;
    myMouseWiz.Free;
    myDragState.Free; // In case it wasn't freed
    inherited Destroy;
end;

procedure TDragRepositionUI.FPOObservedChanged(ASender: TObject;
	Operation: TFPObservedOperation; Data: Pointer);
begin
    case Operation of
    	ooChange: ;
        ooFree: begin
            // log('TDragRepositionUI.FPOObservedChanged -- ooFree');
            Free; // Destroy this object
		end;
        ooAddItem: begin
            //log('TDragRepositionUI.FPOObservedChanged -- ooAddItem');
            if TObject(Data) is ChildControl then begin
                initChild(ChildControl(Data));
			end;
		end;
        ooDeleteItem: ;
        ooCustom: ;
    end;
end;


function TDragRepositionUI.getDragAcceptProc: TDragAcceptProc;
begin
    if not assigned(myDragAcceptProc) then
        Result := @defaultDragAcceptProc
    else
        Result := myDragAcceptProc;

end;

procedure TDragRepositionUI.setanimationSpeed(const _value: NAnimationSpeed);
begin
	if myanimationSpeed=_value then Exit;
	myanimationSpeed:=_value;
end;

procedure TDragRepositionUI.setcolorDefault(const _value: TColor);
begin
	if mycolorDefault=_value then Exit;
	mycolorDefault:=_value;
end;

procedure TDragRepositionUI.setcolorDragNotAllowed(const _value: TColor);
begin
	if mycolorDragNotAllowed=_value then Exit;
	mycolorDragNotAllowed:=_value;
end;

procedure TDragRepositionUI.setcolorDragOver(const _value: TColor);
begin
	if mycolorDragOver=_value then Exit;
	mycolorDragOver:=_value;
end;

procedure TDragRepositionUI.setcolorDragStart(const _value: TColor);
begin
	if mycolorDragStart=_value then Exit;
	mycolorDragStart:=_value;
end;

procedure TDragRepositionUI.setcolorHover(const _value: TColor);
begin
	if mycolorHover=_value then Exit;
	mycolorHover:=_value;
end;

procedure TDragRepositionUI.setcolorSelected(const _value: TColor);
begin
	if mycolorSelected=_value then Exit;
	mycolorSelected:=_value;
end;

procedure TDragRepositionUI.setcontainer(const _value: ParentControl);
begin
	if mycontainer=_value then Exit;
	mycontainer:=_value;
end;

procedure TDragRepositionUI.setform(const _value: TForm);
begin
	if myform=_value then Exit;
	myform:=_value;
end;

procedure TDragRepositionUI.setShowAnimation(const _value: boolean);
begin
	if myShowAnimation=_value then Exit;
	myShowAnimation:=_value;
end;

procedure TDragRepositionUI.setDragAcceptProc(const _value: TDragAcceptProc);
begin
    myDragAcceptProc := _value;
end;

procedure TDragRepositionUI.setenabled(const _value: boolean);
begin
	if myenabled=_value then Exit;
	myenabled:=_value;
end;

function TDragRepositionUI.getChild(_index: cardinal): ChildControl;
begin
    if InRange(_index, 0, pred(container.ControlCount)) then
        Result := ChildControl(container.Controls[_index])
    else
        Result := nil;
end;

procedure TDragRepositionUI.dragStart(Sender: TObject; var DragObject: TDragObject);
var
	_parent: TWinControl;
    _source: ChildControl;
    h, w, l, t, _dy: Integer;
begin
     //log('~~~~~~~~~~~TDragRepositionUI.dragStart~~~~~~~~~~~~~~~');
    try
        myDragObjectMover.reset;
	    if assigned(myPrevOnStartDrag) then myPrevOnStartDrag(Sender, DragObject);
        if not enabled then exit;

        if Sender is ChildControl then begin
	        {Have to bring the control to the front}
            _source := Sender as ChildControl;
	        _parent := _source.Parent;

	        myDragState := TLocalDragState.Create;
	        with myDragState do begin
	            control := ChildControl(Sender);
	            i       := _parent.GetControlIndex(TWinControl(Sender));
	            pos     := Point(control.Left, control.Top);
			end;
            _source.color := colorSelected;
		end;
	except
        on E: EDragRepositionUIBreak do begin
            ; // Do nothing
		end;

        On E: Exception do raise E;
	end;

end;

procedure TDragRepositionUI.dragOver(Sender, Source: TObject; X, Y: Integer;
	State: TDragState; var Accept: Boolean);
var
    i: integer = 0;
	_source: ChildControl;
	h, w, l, _dy, _min, _max, _x, _y, _threshold: Integer;
    sb : TScrollingWinControl;

    _diff, _scrollRate, _range1, _range2, _range3: integer;

    function getScrollRate(_delta: integer): integer;
    begin
        if inRange(_delta, 0, _range1) then begin
            Result := 1;
		end
		else if inRange(_delta, _range1 + 1, _range2) then begin
            Result := 3;
		end
		else if inRange(_delta, _range2 + 1, _range3) then begin
            Result := 7;
		end
        else
            Result := 10;
	end;

begin
    Accept := false;
    //log('');
    //log('TDragRepositionUI.dragOver --->>');
    try
	    if assigned(myPrevOnDragOver) then begin
            // i:=0;
            //log('%d) TDragRepositionUI.dragOver', [i]);
            myPrevOnDragOver(Sender, Source, X, Y, State, Accept);
		end;

		if not enabled then exit;

        //i:=1;
        //log('%d) TDragRepositionUI.dragOver', [i]);
        //myDragObjectMover.move(Sender, Source,X, Y, State);

        //i:=2;
        //log('%d) TDragRepositionUI.dragOver', [i]);

	    if Source is ChildControl then begin
            //i:=3;
            //log('%d) TDragRepositionUI.dragOver', [i]);
	        _source := Source as ChildControl;

            //myGlobalMouseWiz.Move(_source, X, Y);
            //log('before: (left, top) = (%d, %d)', [_source.left, _source.top]);
            //_source.left := _source.left + myDragObjectMover.dX;
            //_source.Top  := _source.Top + myDragObjectMover.dy;
            //log('after: (left, top) = (%d, %d)', [_source.left, _source.top]);
	    end;

	    if sender <> source then begin
            //i:=4;
            //log('%d) TDragRepositionUI.dragOver', [i]);
	        if Sender is ChildControl then begin
                //i:=5;
                //log('%d) TDragRepositionUI.dragOver', [i]);
	            with Sender as ChildControl do begin

		            case State of
		    	        dsDragEnter: begin
	                        color := colorDragOver;
                            //log (' ====================== ENTER ==========================' );
				        end;
				        dsDragLeave: begin
	                        color := colorDefault;
                            //log (' ====================== LEAVE ==========================' );
                            //log ('' );
	                    end;
					end;
		        end;

                if ChildControl(Sender).Parent is TScrollingWinControl then begin
                    sb := TScrollingWinControl(ChildControl(Sender).Parent);

                    _threshold := round(sb.Height/3);

                    _min := round(sb.VertScrollBar.Position  + _threshold);
                    _max := round(sb.VertScrollBar.Position  + sb.height - _threshold);

                    _x   := ChildControl(Sender).Left + X;
                    _y   := ChildControl(Sender).Top  + Y;

                    _range1 := round(_threshold * 0.50);
                    _range2 := round(_threshold * 0.75);
                    _range3 := round(_threshold * 0.90);

                    if _y <= _min then begin
                            // Scroll up
                        _diff   := _min - _y;
                        _scrollRate := getScrollRate(_diff);
                        sb.VertScrollBar.Position := sb.VertScrollBar.Position - _scrollRate;
                        //log('UP:: diff %d, scrollRate: %d', [_diff, _scrollRate]);

					end
                    else if _y >= _max then begin
                        // Scroll down;
                        _diff := _y - _max;
                        _scrollRate := getScrollRate(_diff);
                        //log('UP:: diff %d, scrollRate: %d', [_diff, _scrollRate]);
                        sb.VertScrollBar.Position := sb.VertScrollBar.Position + _scrollRate;
					end;
				end;
			end
            else
                ;//Log('Sender is not ChildControl');

	        if not Accept then begin
                i:=6;
	            Accept := dragAcceptProc(Sender, Source, X, Y, State);
                //case Accept of
                //	True:   Log('Drop Accepted');
                //    False:  Log('Drop Denied');
                //end;
			end;
		end
        else
            ; //Log('Sender = Source');

	except
        on E: EDragRepositionUIBreak do begin
            ; // Do nothing
		end;

        On E: Exception do raise E;
	end;
    //log('============= TDragRepositionUI.dragOver ===============');
end;

procedure TDragRepositionUI.dragDrop(Sender, Source: TObject; X, Y: Integer);
var
    _sender, _source: ChildControl;
    _sourceParent, _senderParent: TWinControl;
    _idxSender, _idxSource: integer;
    _senderTop, _sourceTop, _okState, i: integer;
begin
    //log('TDragRepositionUI.dragDrop');
    //log('Sender is %s, Sender.parent is %s;; Source is %s and Source.Parent is %s', [Sender.ClassName, TControl(Sender).Parent.ClassName, Source.ClassName, TControl(Source).Parent.ClassName]);
    try
        //log('.. check 1');
        if assigned(myPrevOnDragDrop) then myPrevOnDragDrop(Sender, Source, X, Y);
        //log('.. check 2');
        if not enabled then exit;
        //log('.. check 3');
	    if Sender =  Source     then exit;
        //log('.. check 4');
        if not assigned(Source) then exit;
        //log('.. check 5');
        if not assigned(Sender) then exit;

        _senderParent := TControl(Sender).Parent;
        _sourceParent := TControl(Source).Parent;

        _okState := 0;
        i := 0;
        while (container <>_senderParent) and (_senderParent <> form) and (i < 20 )do begin
            //log ('  ** check sender: %s (parent %s)',[TControl(sender).ClassName, _senderParent.ClassName]);
            Sender        := _senderParent;
            _senderParent := _senderParent.Parent;
            inc(i);
    	end;
        if i = 20 then begin
            //log('Sheesh!!');
            exit;
    	end;

        if _senderParent = container then
            inc(_okState);

        i := 0;
        while (container <>_sourceParent) and (_sourceParent <> form) and (i < 20) do begin
            //log ('  ** check source: %s (parent %s)',[TControl(source).ClassName, _sourceParent.ClassName]);
            Source        := _sourceParent;
            _sourceParent := _sourceParent.Parent;
            inc(i);
    	end;

        if i = 20 then begin
            //log('Bheesh!!');
            exit;
    	end;

        if _sourceParent = container then
            inc(_okState);

        //Log('.. check 6');
        if _okState <> 2 then
            exit;

	    if source is ChildControl then begin
	        _source := source as ChildControl;
    	    if _source.Parent is ParentControl then
    	        _sourceParent := _source.Parent as ParentControl;
        end;

	    if sender is ChildControl then begin
	        _sender := sender as ChildControl;
    		if _sender.Parent is ParentControl then
    	        _senderParent := _sender.Parent as ParentControl;
		end;

	    if not (
	                assigned(_source)
	                and assigned(_sourceParent)
	                and assigned(_sender)
	                and assigned(_senderParent)
	            )
	    then exit;

        //log('.. check 7');
        if _senderParent = _sourceParent then begin
            //log ('.... parents are identical');
	        _idxSource  := _senderParent.GetControlIndex(_source);
	        _idxSender  := _senderParent.GetControlIndex(_sender);

	        //log(' == BEFORE  idxSource %d, idxSender %d', [_idxSource, _idxSender]);

	        {Hack to reorder the child}
	        _sourceParent.RemoveControl(_source);
	        _senderParent.InsertControl(_source, _idxSender);

	        //_idxSource  := _senderParent.GetControlIndex(_source);
	        //_idxSender  := _senderParent.GetControlIndex(_sender);
	        //log(' == AFTER idxSource %d, idxSender %d', [_idxSource, _idxSender]);
            _sourceTop    := _source.Top;
            _senderTop    := _sender.Top;

	        _source.Align := alNone;
            _source.Top   := Max(0, _senderTop - 5);

            _sender.Align := alNone;
            _sender.Top   := Max(0, _senderTop + 5);

            _source.Align := alTop;
            _sender.Align := alTop;

            if showAnimation then
	            animate([_source], @bloom, animationDuration);
		end
        else begin
            //log ('.... parents NOT the same');
            //log('..... cannot drop');
		end;

	    FreeAndNil(myDragState);

	except
        on E: EDragRepositionUIBreak do begin
            ; // Do nothing
		end;

        On E: Exception do raise E;
	end;
end;

procedure TDragRepositionUI.dragEnd(Sender, Target: TObject; X, Y: Integer);
var
    _i, t: integer;
    _sender: ChildControl;
    _parent: ParentControl;
	_target: TWinControl;
begin
    //log('~~~~~~~~~~~~~~TDragRepositionUI.dragEnd~~~~~~~~~~~~~~');
    try
        if assigned(myPrevOnEndDrag) then myPrevOnEndDrag(Sender, Target, X, Y);
        if not enabled then exit;
        _sender := ChildControl(sender);
        _target := TWinControl(Target);

        if assigned(myDragState) then begin
            // This means that mouse capture was lost
            // and end drag was called without dragDrop.
	        _parent := ParentControl(_sender.parent);
	        _parent.removeControl(_sender);

            // Return the object to its original place
	        if myDragState.i >= pred(_parent.ControlCount) then
	            _parent.InsertControl(_sender)
	        else begin
	            _parent.InsertControl(_sender, max(0, myDragState.i));
			end;

            if assigned(_target) then begin
	            t := _target.top;
	            _target.align := alNone;
	            _target.top := t + 1;

                _sender.align := alNone;
                _sender.Top := max(0, myDragState.pos.Y - 5);

                _sender.align := alTop;
                _target.align := alTop;

            end;
            FreeAndNil(myDragState);
	    end;

        _sender.color := colorDefault;


	except
        on E: EDragRepositionUIBreak do begin
            ; // Do nothing
		end;

        On E: Exception do raise E;
	end;
end;

procedure TDragRepositionUI.dragHandleStart(Sender: TObject;
	var DragObject: TDragObject);
var
    _senderParent, _sourceParent: TWinControl;
    _okState: integer = 0; // 0 = false;
    i : integer;
begin
    //log('');
    //log('----------------------------------------------------------------------------------------------------------');
    //log('TDragRepositionUI.dragHandleOver:: 00) Sender is %s, Sender.parent is %s;; Source is %s and Source.Parent is %s', [Sender.ClassName, TControl(Sender).Parent.ClassName, Source.ClassName, TControl(Source).Parent.ClassName]);
    //18:07:02:506:: dragOver:: 0) Sender is TShape, Sender.parent is TPanel and Source.Parent is TPanel

    _senderParent := TControl(Sender).Parent;
    if (_senderParent = container) then begin
        dragStart(Sender, DragObject);
        exit;
	end;

    _okState := 0;
    i := 0;
    while (container <> _senderParent) and (_senderParent <> form) and (i < 20 )do begin
        //log ('  ** check sender: %s (parent %s)',[TControl(sender).ClassName, _senderParent.ClassName]);
        Sender        := _senderParent;
        _senderParent := _senderParent.Parent;
        inc(i);
	end;

    if i = 20 then begin
        //log('Sheesh!!');
        exit;
	end;

    if _senderParent = container then
        inc(_okState);

    if _okState = 1 then begin
        dragStart(Sender, DragObject);
	end;
end;

procedure TDragRepositionUI.dragHandleOver(Sender, Source: TObject; X,
	Y: Integer; State: TDragState; var Accept: Boolean);
var
    _senderParent, _sourceParent: TWinControl;
    _okState: integer = 0; // 0 = false;
    i : integer;
begin
    //log('');
    //log('----------------------------------------------------------------------------------------------------------');
    //log('TDragRepositionUI.dragHandleOver:: 00) Sender is %s, Sender.parent is %s;; Source is %s and Source.Parent is %s', [Sender.ClassName, TControl(Sender).Parent.ClassName, Source.ClassName, TControl(Source).Parent.ClassName]);
    //18:07:02:506:: dragOver:: 0) Sender is TShape, Sender.parent is TPanel and Source.Parent is TPanel

    _senderParent := TControl(Sender).Parent;
    _sourceParent := TControl(Source).Parent;

    if (_senderParent = _sourceParent) and (_sourceParent = container) then begin
        dragOver(Sender, Source, X, Y, State, Accept);
        //case Accept of
        //	True:   log('TDragRepositionUI.dragHandleOver:: 1) True');
        //    False:  log('TDragRepositionUI.dragHandleOver:: 1) False. Sender is %s and Source.Parent is %s', [Sender.ClassName, TControl(Source).Parent.ClassName]);
        //end;
        exit;
	end;

    _okState := 0;
    i := 0;
    while (container <>_senderParent) and (_senderParent <> form) and (i < 20 )do begin
        //log ('  ** check sender: %s (parent %s)',[TControl(sender).ClassName, _senderParent.ClassName]);
        Sender        := _senderParent;
        _senderParent := _senderParent.Parent;
        inc(i);
	end;
    if i = 20 then begin
        //log('Sheesh!!');
        exit;
	end;

    if _senderParent = container then
        inc(_okState);

    i := 0;
    while (container <>_sourceParent) and (_sourceParent <> form) and (i < 20) do begin
        //log ('  ** check source: %s (parent %s)',[TControl(source).ClassName, _sourceParent.ClassName]);
        Source        := _sourceParent;
        _sourceParent := _sourceParent.Parent;
        inc(i);
	end;

    if i = 20 then begin
        //log('Bheesh!!');
        exit;
	end;

    if _sourceParent = container then
        inc(_okState);

    if _okState = 2 then begin
        //log('TDragRepositionUI.dragHandleOver:: 01) Sender is %s, Sender.parent is %s and Source is %s Source.Parent is %s', [Sender.ClassName, TControl(Sender).Parent.ClassName, TControl(Source).ClassName, TControl(Source).Parent.ClassName]);
        // 19:15:06:342:: dragOver:: 01) Sender is TPanel, Sender.parent is TScrollBox and Source.Parent is TScrollBox
        dragOver(Sender, Source, X, Y, State, Accept);
        //case Accept of
        //	True:  log('TDragRepositionUI.dragHandleOver:: 2) True');
        //    False: log('TDragRepositionUI.dragHandleOver:: 2) False Sender.Parent is %s and Source.Parent is %s', [TControl(Sender).Parent.ClassName, TControl(Source).Parent.ClassName]);
        //end;
	end else
        ; //log('TDragRepositionUI.dragHandleOver:: 02) MISMATCH:: Sender is %s, Sender.parent is %s and Source.Parent is %s', [Sender.ClassName, TControl(Sender).Parent.ClassName, TControl(Source).Parent.ClassName]);

    //log('----------------------------------------------------------------------------------------------------------');
    //log('');
end;

procedure TDragRepositionUI.dragHandleDrop(Sender, Source: TObject; X,
	Y: Integer);
begin
    if Sender is ChildControl then
        dragDrop(Sender, TControl(Source).Parent, X, Y)

    else if assigned(TControl(Sender).Parent) then
        dragDrop(TControl(Sender).Parent, TControl(Source).Parent, X, Y)
end;

procedure TDragRepositionUI.dragHandleEnd(Sender, Target: TObject; X, Y: Integer
	);
begin
    if not assigned(Target) then exit;

    if Target is ChildControl then
        dragEnd(TControl(Sender).Parent, Target, X, Y)
    else
        dragEnd(TControl(Sender).Parent, TControl(Target).Parent, X, Y);

end;


function TDragRepositionUI.insertChild: ChildControl;
begin
    Result := initChild(ChildControl.Create(container));
end;

function TDragRepositionUI.init: integer;
var
	i: Integer;
	_control: TControl;
begin
    Result := 0;
    for i := 0 to pred(container.controlCount) do begin
        _control := container.controls[i];
        if _control is ChildControl then begin
            initChild(ChildControl(_control));
            Inc(Result);
		end;
	end;
end;

{ TDragRepositionUI.TLocalDragState }

constructor TDragRepositionUI.TLocalDragState.Create;
begin
    inherited Create;
    FPOAttachObserver(Application);
end;

end.

