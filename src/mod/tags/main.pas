unit main;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

	{ TForm1 }

    TForm1 = class(TForm)
		procedure FormCreate(Sender: TObject);
    private

    public

    end;

var
    Form1: TForm1;

implementation

{$R *.lfm}
uses
    sugar.tagUI, sugar.logger;
{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
	te: TTagsEditor;
	i: Integer;
begin
    for i := 0 to 3 do begin
	    te := TTagsEditor.Create(Self);
        te.Name := '';
	    te.Align:= alTop;
        te.BorderSpacing.Around:=10;
	    InsertControl(te);
	end;
end;

initialization
    startLog();

end.

