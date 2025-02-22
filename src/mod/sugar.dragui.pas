unit sugar.dragui;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, sugar.animateui ;
type

	{ TDragRepositionUI }
    {NOTE: You shouldn't free an object of this class. It is is freed when the container is freed}
    generic TDragRepositionUI <ChildControl: TControl; ParentControl: TWinControl> = class(TPersistent, IFPObserver)
    private type
        TLocalDragState = class
            control : ChildControl;
            i       : integer; // current index
            pos     : TPoint;
        end;

    protected
        prevMousePoint : TPoint;
        containerHOffset  : cardinal;
        containerVOffset  : cardinal;
        myDragState: TLocalDragState;
        function deltaX: integer;
		function deltaY: integer;
		function getChild(_index: cardinal): ChildControl;
        procedure initMouseDelta;
        function animationDuration: cardinal; virtual;
   public
	   function mPoint: TPoint;
	   function mX: integer;
	   function mY: integer;

	   {These calculate the horizontal and vertical offset of the container }
	   function calcHOffset(constref _container: ParentControl): integer;
	   function calcVOffset(constref _container: ParentControl): integer;

   public
        constructor Create(constref _container: ParentControl; const _childOffset: cardinal);
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
    LCLType, Math, sugar.uihelper, sugar.logger;


{Global mouse position finders }

{ TDragRepositionUI }
function TDragRepositionUI.mPoint: TPoint;
begin
    Result := Controls.Mouse.CursorPos;
end;

function TDragRepositionUI.mX: integer;
begin
    Result := Controls.Mouse.CursorPos.X;
end;

function TDragRepositionUI.mY: integer;
begin
    Result := Controls.Mouse.CursorPos.Y;
end;

function TDragRepositionUI.initChild(constref _child: ChildControl): ChildControl;
begin
    Result       := _child;
    Result.Align := alTop;

    Result.DragMode   := dmAutomatic;
    Result.OnStartDrag:= @dragStart;
    Result.OnDragOver := @dragOver;
    Result.OnDragDrop := @dragDrop;
    Result.OnEndDrag  := @dragEnd;

end;

function TDragRepositionUI.calcHOffset(constref _container: ParentControl
	): integer;
var
    _parent: TControl;
    _continue : boolean =  true;
begin
    Result  := _container.Left - (_container.BorderSpacing.Around + _container.BorderSpacing.Left);
    _parent := _container.Parent;
    while _continue do begin
        _continue := assigned(_parent);
        if _continue then begin
            _continue := (_parent is TForm);
            if _continue then begin
                // any additional adjustments
                if not assigned(form) then begin
                    form := TForm(_parent);
                    _continue := false;
				end;
			end;
            Result := Result - (_parent.Left + _parent.BorderSpacing.Around + _parent.BorderSpacing.Left);
            _parent := _parent.Parent;
		end;
	end;
end;

function TDragRepositionUI.calcVOffset(constref _container: ParentControl
	): integer;
var
    _parent: TControl;
    _continue: boolean =  true;
begin
    Result  := _container.Top - (_container.BorderSpacing.Around + _container.BorderSpacing.Top);
    _parent := _container.Parent;

    while _continue do begin
        _continue := assigned(_parent);
        if _continue then begin
            _continue := (_parent is TForm);
            if _continue then begin
                if not assigned(form) then begin
                    form := TForm(_parent);
                    _continue := false;
				end;
				Result := Result +  windowBarHeight(TForm(_parent));
			end;
            Result := Result + _parent.Top - (_parent.BorderSpacing.Around + _parent.BorderSpacing.Top);
            _parent := _parent.Parent;
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

constructor TDragRepositionUI.Create(constref _container: ParentControl;
	const _childOffset: cardinal);
var
	_control: TControl;
	i: Integer;
begin
    inherited Create;
    animationSpeed     := aspeedNormal;
    container          := _container;
    container.FPOAttachObserver(Self);

    Self.colorSelected := dragUIColorSelected;
    Self.colorDefault  := dragUIColorDefault;
    Self.colorDragOver := dragUIColorDragOver;
    init;

    containerHOffset    := calcHOffset(container);
    containerVOffset    := calcVOffset(container);

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

procedure TDragRepositionUI.initMouseDelta;
begin
    prevMousePoint := mPoint; // stores the current mouse posioint
end;

function TDragRepositionUI.deltaX: integer;
var
    _currPoint : TPoint;
begin
    _currPoint := mPoint;
    Result := _currPoint.x - prevMousePoint.X; // determines whether it moved left or right
    prevMousePoint:= _currPoint;
end;

function TDragRepositionUI.deltaY: integer;
var
    _currPoint : TPoint;
begin
    _currPoint := mPoint;
    Result := _currPoint.Y - prevMousePoint.Y; // determines if it moved up or down
    prevMousePoint:= _currPoint;
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
    h, w, l, _dy: Integer;
begin
    log('TDragRepositionUI.dragStart');
    if Sender is ChildControl then begin
        {Have to bring the control to the front}
        _parent := childControl(Sender).Parent;
        myDragState := TLocalDragState.Create;
        with myDragState do begin
            control := ChildControl(Sender);
            i       := _parent.GetControlIndex(TWinControl(Sender));
            pos     := Point(control.Left, control.Top);
		end;
        _parent.RemoveControl(TWinControl(Sender));
        _parent.InsertControl(TWinControl(Sender));

        initMouseDelta;
        containerHOffset    := calcHOffset(container);
        containerVOffset    := calcVOffset(container);

        _source       := Sender as ChildControl;
        _source.color := colorSelected;
        if _source.Align <> alNone then begin
			h := _source.height;
			w := _source.width;
			l := _source.left;
			_source.Align := alNone;
			_source.height := h;
			_source.width  := w;
			_source.left   := l + 10;
       	end;
        _source.Top  := mY - containerVOffset + 10;  {spacing below the cursor};
	end;
end;

procedure TDragRepositionUI.dragOver(Sender, Source: TObject; X, Y: Integer;
	State: TDragState; var Accept: Boolean);
var
	_source: ChildControl;
	h, w, l, _dy: Integer;
begin
    if Source is ChildControl then begin
        _source       := source as ChildControl;
        _source.color := colorSelected;
        if _source.Align <> alNone then begin
            h := _source.height;
            w := _source.width;
            l := _source.left;
            _source.Align := alNone;
            _source.height := h;
            _source.width  := w;
            _source.left   := l + 10;
		end;
        _source.Top  := mY - containerVOffset + 10;  {spacing below the cursor};

	end;

    if sender <> source then begin
        if Sender is ChildControl then begin
            with Sender as ChildControl do begin
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
        Accept := (source is ChildControl);
	end

end;

procedure TDragRepositionUI.dragDrop(Sender, Source: TObject; X, Y: Integer);
var
    _sender, _source: ChildControl;
    _sourceParent, _senderParent: ParentControl;
    _idxSender, _idxSource: integer;
begin
    log('TDragRepositionUI.dragDrop');
    if Sender =  Source then exit;

    if source is ChildControl then
        _source := source as ChildControl;
    if _source.Parent is ParentControl then
        _sourceParent := _source.Parent as ParentControl;

    if sender is ChildControl then
        _sender := sender as ChildControl;
    if _sender.Parent is ParentControl then
        _senderParent := _sender.Parent as ParentControl;

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

        log(' == BEFORE  idxSource %d, idxSender %d', [_idxSource, _idxSender]);

        {Hack to reorder the child}
        _sourceParent.RemoveControl(_source);
        _senderParent.InsertControl(_source, _idxSender);

        _idxSource  := _senderParent.GetControlIndex(_source);
        _idxSender  := _senderParent.GetControlIndex(_sender);
        log(' == AFTER idxSource %d, idxSender %d', [_idxSource, _idxSender]);

        _source.Top   := Max(0, _sender.Top -5);
        _sender.Top   := Max(_senderParent.Height, _sender.top + 5);
        _source.Align := alTop;

        animate([_source], @bloom, animationDuration);
	end;
    FreeAndNil(myDragState);
end;

procedure TDragRepositionUI.dragEnd(Sender, Target: TObject; X, Y: Integer);
var
    _i: integer;
    _sender: ChildControl;
    _parent: ParentControl;
begin
    log('TDragRepositionUI.dragEnd');

    if assigned(myDragState) then begin
        _sender := ChildControl(sender);
        _parent := ParentControl(_sender.parent);
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
        color := clDefault;
        align := alTop;
	end;

    initMouseDelta;

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

    log('TDragHandle.dragStart');

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
    log('TDragHandle.uiDragOver: Sender is %s, Source is %s', [Sender.ClassName, Source.ClassName]);

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
        log('TDragHandle.uiDragOver: source has been set to the parent');
	end
	else begin
        _source := TWinControl(Sender);
        log('TDragHandle.uiDragOver: source has is the panel ');
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

    log('TDragHandle.parentDragDrop');

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
        log('exiting: all pointers are not assigned');
        exit;

	end;

    {Check if the context of this procedure call is container->panels. Don't continue otherwise}
    if not (_sourceParent = _senderParent) then begin
        log('exiting: source parent is not sender parent');
        exit;
    end;
    if (container = _sourceParent) then begin
        log('exiting: source parent is not contianer');
        exit;
	end;

    if _senderParent = _sourceParent then begin
        _idxSource  := _senderParent.GetControlIndex(_source);
        _idxSender  := _senderParent.GetControlIndex(_sender);

        log(' == BEFORE  idxSource %d, idxSender %d', [_idxSource, _idxSender]);

        {Hack to reorder the child}
        _sourceParent.RemoveControl(_source);
        _senderParent.InsertControl(_source, _idxSender);

        _idxSource  := _senderParent.GetControlIndex(_source);
        _idxSender  := _senderParent.GetControlIndex(_sender);
        log(' == AFTER idxSource %d, idxSender %d', [_idxSource, _idxSender]);

        _source.Top   := Max(0, _sender.Top -5);
        _sender.Top   := Max(_senderParent.Height, _sender.top + 5);
        _source.Align := alTop;

        animate([_source], @bloom, animationDuration(animationSpeed));
	end;
    FreeAndNil(myDragState);
end;


procedure TDragHandle.dragEnd(Sender, Target: TObject; X, Y: Integer);
begin
    log('TDragHandle.dragEnd');
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
    log('TDragHandle.parentDragEnd');

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

