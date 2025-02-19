unit sugar.dragui;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, sugar.animateui ;
type

	{ TDragRepositionUI }

    generic TDragRepositionUI <ChildControl: TControl; ParentControl: TWinControl> = class(TPersistent, IFPObserver)
    private
        prevMousePoint : TPoint;
        containerHOffset  : cardinal;
        containerVOffset  : cardinal;
        function deltaX: integer;
		function deltaY: integer;
		function getChild(_index: cardinal): ChildControl;
        procedure initMouseDelta;
        function mPoint: TPoint;
		function mX: integer;
		function mY: integer;

        {These calculate the }
        function calcHOffset(constref _container: ParentControl): integer;
        function calcVOffset(constref _container: ParentControl): integer;

        function animationDuration: cardinal;

   public
        constructor Create(constref _container: ParentControl; const _childOffset: cardinal);
		procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);

   public
        form: TForm;
        container: ParentControl;
        colorSelected: TColor;
        colorDefault: TColor;
        colorDragOver: TColor;
        animationSpeed: NAnimationSpeed;

        procedure dragDrop(Sender, Source: TObject; X, Y: Integer);
		procedure dragEnd(Sender, Target: TObject; X, Y: Integer);
        procedure dragOver(Sender, Source: TObject; X, Y: Integer;
			State: TDragState; var Accept: Boolean);
		procedure dragStart(Sender: TObject; var DragObject: TDragObject);

        function insertChild: ChildControl;
        function init: integer;
        function initChild(constref _child: ChildControl): ChildControl;
    public
         property child[_index: cardinal]: ChildControl read getChild;

	end;

    TPanelDragRepositionUI  = class(specialize TDragRepositionUI<TPanel, TWinControl>);
    TButtonDragRepositionUI = class(specialize TDragRepositionUI<TButton, TWinControl>);
    TFrameDragRepositionUI  = class(specialize TDragRepositionUI<TFrame, TWinControl>);

var
    colorSelected :TColor = cl3DLight;
    colorDefault  :TColor = clDefault;
    colorDragOver :TColor = cl3DHiLight;

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
    Result := _child;
    Result.Align := alTop;
    Result.OnStartDrag:= @dragStart;
    Result.OnEndDrag  := @dragEnd;

    Result.DragMode   := dmAutomatic;
    Result.OnDragDrop := @dragDrop;
    Result.OnDragOver := @dragOver;
end;

function TDragRepositionUI.calcHOffset(constref _container: ParentControl
	): integer;
var
    _parent: TControl;
    _continue : boolean =  true;
begin
    Result  := _container.Left + _container.BorderSpacing.Around + _container.BorderSpacing.Left;
    _parent := _container.Parent;
    while _continue do begin
        _continue := assigned(_parent);
        if _continue then begin
            _continue := (_parent is TForm);
            if _continue then begin
                // any additional adjustments
                if not assigned(form) then form := TForm(_parent);
                _continue := false;
            end;
            Result := Result + _parent.Left + _parent.BorderSpacing.Around + _parent.BorderSpacing.Left;
            _parent := _parent.Parent;
		end;
	end;
    //log('calcHOffset = %d', [Result]);
end;

function TDragRepositionUI.calcVOffset(constref _container: ParentControl
	): integer;
var
    _parent: TControl;
    _continue: boolean =  true;
begin
    Result  := _container.Top + _container.BorderSpacing.Around + _container.BorderSpacing.Top;
    _parent := _container.Parent;
    while _continue do begin
        _continue := assigned(_parent);
        if _continue then begin
            _continue := (_parent is TForm);
            if _continue then begin
                if not assigned(form) then form := TForm(_parent);
                Result := Result +  windowBarHeight(TForm(_parent));
                _continue := false;
			end;
            Result := Result + _parent.Top + _parent.BorderSpacing.Around + _parent.BorderSpacing.Top;
            _parent := _parent.Parent;
		end;
	end;
    //log('calcVOffset = %d', [Result]);
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

    Self.colorSelected := sugar.dragui.colorSelected;
    Self.colorDefault  := sugar.dragui.colorDefault;
    Self.colorDragOver := sugar.dragui.colorDragOver;
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

procedure TDragRepositionUI.dragDrop(Sender, Source: TObject; X, Y: Integer);
var
    _sender, _source: ChildControl;
    _sourceParent, _senderParent: ParentControl;
    _idxSender, _idxSource: integer;
begin

    if Sender <> Source then begin

        if source is ChildControl then
            _source := source as ChildControl;
        if _source.Parent is ParentControl then
            _sourceParent := _source.Parent as ParentControl;

        if sender is ChildControl then
            _sender := sender as ChildControl;

        if _sender.Parent is ParentControl then
            _senderParent := _sender.Parent as ParentControl;

        if _senderParent = _sourceParent then begin
            _idxSource  := _senderParent.GetControlIndex(_source);
            _idxSender  := _senderParent.GetControlIndex(_sender);

            {Hack to reorder the child}
            _sourceParent.RemoveControl(_source);
            _senderParent.InsertControl(_source, _idxSender);

            _source.Align:= alTop;
            _source.Top := Max(0, _sender.Top -1);
            animate([_source], @bloom, animationDuration);
		end;
	end;
end;

procedure TDragRepositionUI.dragEnd(Sender, Target: TObject; X, Y: Integer);
begin
    with TWinControl(Sender) do begin
        color := clDefault;
        align := alTop;
	end;
    initMouseDelta;
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
            _source.left   := l;
		end;
        _source.Top  := mY - containerVOffset + 15 {spacing below the cursor};
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
procedure TDragRepositionUI.dragStart(Sender: TObject; var DragObject: TDragObject);
var
	_parent: TWinControl;
begin
    if Sender is ChildControl then begin
        {Have to bring the control to the front}
        _parent := childControl(Sender).Parent;
        _parent.RemoveControl(TWinControl(Sender));
        _parent.InsertControl(TWinControl(Sender));
        initMouseDelta;
        containerHOffset    := calcHOffset(container);
        containerVOffset    := calcVOffset(container);
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

end.

