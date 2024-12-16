unit main;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, RTTICtrls;

type

	{ TForm1 }

    TForm1 = class(TForm)
		Button1: TButton;
		Button2: TButton;
		Memo1: TMemo;
		MultiPropertyLink1: TMultiPropertyLink;
		procedure Button1Click(Sender: TObject);
		procedure Button2Click(Sender: TObject);
    private

    public

    end;

var
    Form1: TForm1;

implementation

{$R *.lfm}

uses
    sugar.htmlbuilder, rtti_test;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
    _h: THtmlDoc;
begin
    _h := THtmlDoc.Create;
    memo1.Text:= _h.html;
    _h.Free;

end;

procedure TForm1.Button2Click(Sender: TObject);
begin
    Form2.ShowModal
end;

end.

