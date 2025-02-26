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
    private type
        TLocalDragState = class
            control : ChildControl;
            i       : integer; // current index
            pos     : TPoint;
        end;

	    function getDragAcceptProc: TDragAcceptProc;
		procedure setShowAnimation(const _value: boolean);
	    procedure setDragAcceptProc(const _value: TDragAcceptProc);
		procedure setenabled(const _value: boolean);

    protected

        myPrevOnStartDrag: TStartDragEvent;
        myPrevOnDragOver : TDragOverEvent;
        myPrevOnDragDrop : TDragDropEvent;
        myPrevOnEndDrag  : TEndDragEvent;

        myDragAcceptProc: TDragAcceptProc;
        myDragState: TLocalDragState;
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
        form: TForm;
        container: ParentControl;
        colorDefault: TColor;
        colorSelected: TColor;
        colorDragOver: TColor;
        animationSpeed: NAnimationSpeed;

        procedure dragStart(Sender: TObject; var DragObject: TDragObject);
        procedure dragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
        procedure dragDrop(Sender, Source: TObject; X, Y: Integer); {works for both parent and draghandle}
		procedure dragEnd(Sender, Target: TObject; X, Y: Integer);

        function insertChild: ChildControl;
        function init: integer;
        function initChild(constref _child: ChildControl): ChildControl;
    public
         property child[_index: cardinal]: ChildControl read getChild;
         property dragAcceptProc: TDragAcceptProc read getDragAcceptProc write setDragAcceptProc;
         property enabled: boolean read myenabled write setenabled;
         property showAnimation: boolean read myShowAnimation write setShowAnimation;

	end;

	{ TDragHandle }

    TDragHandle = class(TPanel)
        constructor Create(TheOwner: TComponent); override;

    private type
        TLocalDragState = class
            control : TWinControl;
            i       : integer; // current index
            pos     : TPoint;
        end;

	private
		myanimationSpeed: NAnimationSpeed;
		mycolorDefault: TColor;
		mycolorDragNotAllowed: TColor;
		mycolorDragOver: TColor;
		mycolorDragStart: TColor;
		mycolorHover: TColor;
		mycolorSelected: TColor;
		mycontainer: TWinControl;
		myForm: TForm;
        myContainerHOffset  : cardinal;
        myContainerVOffset  : cardinal;
        myDragState: TLocalDragState;

		function getContainer: TWinControl;
        procedure setanimationSpeed(const _value: NAnimationSpeed);
		procedure setcolorDefault(const _value: TColor);
        procedure setcolorDragNotAllowed(const _value: TColor);
		procedure setcolorDragOver(const _value: TColor);
		procedure setcolorDragStart(const _value: TColor);
        procedure setcolorHover(const _value: TColor);
		procedure setcolorSelected(const _value: TColor);
		procedure setcontainer(const _value: TWinControl);

    protected
        procedure SetParent(NewParent: TWinControl); override;
        procedure dragStart(Sender: TObject; var DragObject: TDragObject);
        procedure uiDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); {Works for both parent and draghandle}
        procedure uiDragDrop(Sender, Source: TObject; X, Y: Integer);
        procedure parentDragDrop(Sender, Source: TObject; X, Y: Integer);
        procedure dragEnd(Sender, Target: TObject; X, Y: Integer);
        procedure parentDragEnd(Sender, Target: TObject; X, Y: Integer);

        procedure MouseEnter(Sender: TObject);
        procedure MouseLeave(Sender: TObject);


    protected
 	   function mPoint: TPoint;
 	   function mX: integer;
 	   function mY: integer;

 	   {These calculate the horizontal and vertical offset of the container }
 	   function calcHOffset: integer;
 	   function calcVOffset: integer;

    published
        property form:                TForm read myForm;
        property container:           TWinControl read getContainer write setcontainer;
        property colorDefault:        TColor read mycolorDefault write setcolorDefault;
        property colorHover:          TColor read mycolorHover write setcolorHover;
        property colorDragStart:      TColor read mycolorDragStart write setcolorDragStart;
        property colorDragOver:       TColor read mycolorDragOver write setcolorDragOver;
        property colorDragNotAllowed: TColor read mycolorDragNotAllowed write setcolorDragNotAllowed;
        property colorSelected:       TColor read mycolorSelected write setcolorSelected;
        property animationSpeed:      NAnimationSpeed read myanimationSpeed write setanimationSpeed;

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
	_sender, _source: TControl;
    newDX, newDY : integer;
begin
    Result := Self;
    _sender := TControl(Sender);
    _source := TControl(Source);

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
    Result       := _child;

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
    Result := (source  <> sender) and (source is ChildControl);
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
        myDragObjectMover.move(Sender, Source,X, Y, State);

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

                    _range1 := round(_threshold * 0.5);
                    _range2 := round(_threshold * 0.8);
                    _range3 := round(_threshold * 0.95);

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
			end;

	        if not Accept then begin
                i:=6;
	            Accept := dragAcceptProc(Sender, Source, X, Y, State);
			end;
		end;
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
    _sourceParent, _senderParent: ParentControl;
    _idxSender, _idxSource: integer;
begin
    //log('TDragRepositionUI.dragDrop');
    try
        if assigned(myPrevOnDragDrop) then myPrevOnDragDrop(Sender, Source, X, Y);
        if not enabled then exit;

	    if Sender =  Source then exit;

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

	    if _senderParent = _sourceParent then begin

	        _idxSource  := _senderParent.GetControlIndex(_source);
	        _idxSender  := _senderParent.GetControlIndex(_sender);

	        //log(' == BEFORE  idxSource %d, idxSender %d', [_idxSource, _idxSender]);

	        {Hack to reorder the child}
	        _sourceParent.RemoveControl(_source);
	        _senderParent.InsertControl(_source, _idxSender);

	        //_idxSource  := _senderParent.GetControlIndex(_source);
	        //_idxSender  := _senderParent.GetControlIndex(_sender);
	        //log(' == AFTER idxSource %d, idxSender %d', [_idxSource, _idxSender]);

	        _source.Align := alNone;
            _source.Top   := Max(0, _sender.Top - 5);
            _source.Align := alTop;

            if showAnimation then
	            animate([_source], @bloom, animationDuration);
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

{ TDragHandle }

constructor TDragHandle.Create(TheOwner: TComponent);
begin
	inherited Create(TheOwner);

    myContainer := nil;

    Align      := alRight;
    width      := 20;
    caption    := '↕';
    BevelOuter := bvNone;

    DragMode   := dmAutomatic;
    OnStartDrag:= @dragStart;
    OnDragOver := @uiDragOver;
    OnDragDrop := @uiDragDrop;
    OnEndDrag  := @dragEnd;

    OnMouseEnter := @MouseEnter;
    OnMouseLeave := @MouseLeave;

    colorDefault        :=  dragUIColorDefault;
    colorHover          :=  dragUIColorHover;
    colorDragStart      :=  dragUIColorDragStart;
    colorDragOver       :=  dragUIColorDragOver;
    colorDragNotAllowed :=  dragUIColorDragNotAllowed;
    colorSelected       :=  dragUIColorSelected;

    animationSpeed      :=  aspeedQuick;

end;

procedure TDragHandle.setcolorHover(const _value: TColor);
begin
	//if mycolorHover=_value then Exit;
	mycolorHover:=_value;
end;

procedure TDragHandle.setcolorSelected(const _value: TColor);
begin
	if mycolorSelected=_value then Exit;
	mycolorSelected:=_value;
end;

procedure TDragHandle.setcontainer(const _value: TWinControl);
begin
	if mycontainer=_value then Exit;
	mycontainer:=_value;
end;

procedure TDragHandle.SetParent(NewParent: TWinControl);
begin

    if assigned(Parent) then begin
	    if parent is TPanel then begin
	        TPanel(parent).OnDragOver := nil;
	        TPanel(parent).OnDragDrop := nil;
	    end
	    else if parent is TFlowPanel then begin
	        TFlowPanel(parent).OnDragOver := nil;
	        TFlowPanel(parent).OnDragDrop := nil;
	    end
	    else if parent is TScrollBox then begin
	        TScrollBox(parent).OnDragOver := nil;
	        TScrollBox(parent).OnDragDrop := nil;
	    end;
	end;

    if Assigned(NewParent) then begin
        inherited SetParent(NewParent);

	    if parent is TPanel then begin
	        TPanel(parent).OnDragOver := @uiDragOver;
	        TPanel(parent).OnDragDrop := @parentDragDrop;
	    end
	    else if parent is TFlowPanel then begin
	        TFlowPanel(parent).OnDragOver := @uiDragOver;
	        TFlowPanel(parent).OnDragDrop := @parentDragDrop;
	    end
	    else if parent is TScrollBox then begin
	        TScrollBox(parent).OnDragOver := @uiDragOver;
	        TScrollBox(parent).OnDragDrop := @parentDragDrop;
	    end
	        else
	            raise Exception.Create('TDragHandle:: Parent must be TPanel, TFlowPanel or TScrollBox');

	    myContainer  := Parent.Parent;
        colorDefault :=  Parent.Color;
        myContainerHOffset  := calcHOffset;
        myContainerVOffset  := calcVOffset;
    end;
end;


const
    __dragPosOffest = 10;

procedure TDragHandle.dragStart(Sender: TObject; var DragObject: TDragObject);
var
    h, w, l, _dy: Integer;
begin

    //log('TDragHandle.dragStart');

    {Have to bring the control to the front}
    myDragState := TLocalDragState.Create;
    with myDragState do begin
        control := Parent;
        i       := container.GetControlIndex(Parent);
        pos     := Point(Parent.Left, Parent.Top);
	end;

    container.RemoveControl(Parent);
    container.InsertControl(Parent);

    myContainerHOffset    := calcHOffset;
    myContainerVOffset    := calcVOffset;

    Parent.color := colorDragStart;
    if Parent.Align <> alNone then begin
		h := Parent.height;
		w := Parent.width;
		l := Parent.left;
		Parent.Align := alNone;
		Parent.height := h;
		Parent.width  := w;
		Parent.left   := l + __dragPosOffest;
   	end;
     //Parent.Top  := mY - myContainerVOffset + __dragPosOffest;  {spacing below the cursor};
    EndDrag(False);
    Parent.BeginDrag(False, -1);
end;

procedure TDragHandle.uiDragOver(Sender, Source: TObject; X, Y: Integer;
	State: TDragState; var Accept: Boolean);
var
	_source: TWinControl;
	h, w, l, _dy: Integer;
begin
    //log('TDragHandle.uiDragOver: Sender is %s, Source is %s', [Sender.ClassName, Source.ClassName]);

    if sender <> source then begin
        if Sender is TWinControl then begin
            with Sender as TWinControl do begin
	            case State of
	    	        dsDragEnter: begin
                        color := colorDragOver;
			        end;
			        dsDragLeave: begin
                        color := colorDefault;
                    end;
				end;
	        end;
		end;
        Accept := TWinControl(Sender).Parent = TWinControl(Source).Parent;
	end;

    if Sender is TDragHandle then begin
        _source := TDragHandle(Sender).Parent;
        //log('TDragHandle.uiDragOver: source has been set to the parent');
	end
	else begin
        _source := TWinControl(Sender);
        //log('TDragHandle.uiDragOver: source has is the panel ');
	end;

    _source.color := colorDragOver;
    if _source.Align <> alNone then begin
        h := _source.height;
        w := _source.width;
        l := _source.left;
        _source.Align := alNone;
        _source.height := h;
        _source.width  := w;
        _source.left   := l + __dragPosOffest;
    end;

    _source.Top  := mY - myContainerVOffset + __dragPosOffest;  {spacing below the cursor};

end;

procedure TDragHandle.uiDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
    if Parent.Dragging then
        Parent.DragDrop(Source, X, Y);
end;

procedure TDragHandle.parentDragDrop(Sender, Source: TObject; X, Y: Integer);
var
    _sender, _source: TWinControl;
    _sourceParent, _senderParent: TWinControl;
    _idxSender, _idxSource: integer;
begin

    //log('TDragHandle.parentDragDrop');

    {Check if drag and drop is happening on the same control}
    if Sender =  Source then exit;

    {Extract pointers and establish context}
    if Sender is TDragHandle then
        Sender := TDragHandle(Sender).Parent;

    if Source is TDragHandle then
        Source := TDragHandle(Source).Parent;

    if source is TWinControl then
        _source := source as TWinControl;
    if _source.Parent is TWinControl then
        _sourceParent := _source.Parent as TWinControl;

    if sender is TWinControl then
        _sender := sender as TWinControl;
    if _sender.Parent is TWinControl then
        _senderParent := _sender.Parent as TWinControl;

    {Check if all pointers are assigned. Don't continue otherwise}
    if not (
                assigned(_source)
                and assigned(_sourceParent)
                and assigned(_sender)
                and assigned(_senderParent)
            )
    then begin
        //log('exiting: all pointers are not assigned');
        exit;

	end;

    {Check if the context of this procedure call is container->panels. Don't continue otherwise}
    if not (_sourceParent = _senderParent) then begin
        //log('exiting: source parent is not sender parent');
        exit;
    end;
    if (container = _sourceParent) then begin
        //log('exiting: source parent is not contianer');
        exit;
	end;

    if _senderParent = _sourceParent then begin
        _idxSource  := _senderParent.GetControlIndex(_source);
        _idxSender  := _senderParent.GetControlIndex(_sender);

        //log(' == BEFORE  idxSource %d, idxSender %d', [_idxSource, _idxSender]);

        {Hack to reorder the child}
        _sourceParent.RemoveControl(_source);
        _senderParent.InsertControl(_source, _idxSender);

        _idxSource  := _senderParent.GetControlIndex(_source);
        _idxSender  := _senderParent.GetControlIndex(_sender);
        //log(' == AFTER idxSource %d, idxSender %d', [_idxSource, _idxSender]);

        _source.Top   := Max(0, _sender.Top -5);
        _sender.Top   := Max(_senderParent.Height, _sender.top + 5);
        _source.Align := alTop;

        animate([_source], @bloom, animationDuration(animationSpeed));
	end;
    FreeAndNil(myDragState);
end;


procedure TDragHandle.dragEnd(Sender, Target: TObject; X, Y: Integer);
begin
    //log('TDragHandle.dragEnd');
    if Sender is TDragHandle then begin
        TDragHandle(Sender).Parent.EndDrag(True);
	end;
end;

procedure TDragHandle.parentDragEnd(Sender, Target: TObject; X, Y: Integer);
var
    _i: integer;
    _sender: TWinControl;
    _parent: TWinControl;
begin
    //log('TDragHandle.parentDragEnd');

    if assigned(myDragState) then begin
        _sender := TWinControl(sender);
        _parent := TWinControl(_sender.parent);

        _parent.removeControl(_sender);

        if myDragState.i >= _parent.ControlCount then
            _parent.InsertControl(TWinControl(Sender))
        else begin
            _parent.InsertControl(_sender, max(0, myDragState.i));
            _sender.Top := max(0, myDragState.pos.Y -1);
		end;

		FreeAndNil(myDragState);
    end;

    with TWinControl(Sender) do begin
        color := colorDefault;
        align := alTop;
	end;

end;

procedure TDragHandle.MouseEnter(Sender: TObject);
begin
    color := colorHover;
end;

procedure TDragHandle.MouseLeave(Sender: TObject);
begin
    color := colorDefault;
end;

function TDragHandle.mPoint: TPoint;
begin
    Result := Mouse.CursorPos;
end;

function TDragHandle.mX: integer;
begin
    Result := Mouse.CursorPos.X;
end;

function TDragHandle.mY: integer;
begin
    Result := Mouse.CursorPos.Y;
end;

function TDragHandle.calcHOffset: integer;
var
    _parent: TControl;
    _continue : boolean =  true;
begin
    Result  := Parent.Left - (Parent.BorderSpacing.Around + Parent.BorderSpacing.Left);
    _parent := Parent.Parent;
    while _continue do begin
        _continue := assigned(_parent);
        if _continue then begin
            _continue := (_parent is TForm);
            if _continue then begin
                // any additional adjustments
                if not assigned(myForm) then begin
                    myForm := TForm(_parent);
                    _continue := false;
				end;
			end;
            Result := Result + _parent.Left - (_parent.BorderSpacing.Around + _parent.BorderSpacing.Left);
            _parent := _parent.Parent;
		end;
	end;
end;

function TDragHandle.calcVOffset: integer;
var
    _parent: TControl;
    _continue: boolean =  true;
begin
    Result  := Parent.Top - (Parent.BorderSpacing.Around + Parent.BorderSpacing.Top);
    _parent := Parent.Parent;

    while _continue do begin
        _continue := assigned(_parent);
        if _continue then begin
            _continue := (_parent is TForm);
            if _continue then begin
                if not assigned(myForm) then begin
                    myForm := TForm(_parent);
                    _continue := false;
				end;
				Result := Result +  windowBarHeight(TForm(_parent));
			end;
            Result := Result + _parent.Top - (_parent.BorderSpacing.Around + _parent.BorderSpacing.Top);
            _parent := _parent.Parent;
		end;
	end;
end;


procedure TDragHandle.setcolorDragNotAllowed(const _value: TColor);
begin
	if mycolorDragNotAllowed=_value then Exit;
	mycolorDragNotAllowed:=_value;
end;

procedure TDragHandle.setanimationSpeed(const _value: NAnimationSpeed);
begin
	if myanimationSpeed=_value then Exit;
	myanimationSpeed:=_value;
end;

function TDragHandle.getContainer: TWinControl;
begin
    if assigned(mycontainer) then begin
        Result := myContainer;
    end
    else if Assigned(Parent) then begin
        myContainer := Parent.Parent;
        Result := myContainer;
	end;

end;

procedure TDragHandle.setcolorDefault(const _value: TColor);
begin
	if mycolorDefault=_value then Exit;
	mycolorDefault:=_value;
end;

procedure TDragHandle.setcolorDragOver(const _value: TColor);
begin
	if mycolorDragOver=_value then Exit;
	mycolorDragOver:=_value;
end;

procedure TDragHandle.setcolorDragStart(const _value: TColor);
begin
	if mycolorDragStart=_value then Exit;
	mycolorDragStart:=_value;
end;


end.

