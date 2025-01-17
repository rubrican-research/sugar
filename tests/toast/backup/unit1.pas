unit Unit1;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

	{ TForm1 }

    TForm1 = class(TForm)
		Button1: TButton;
		Button2: TButton;
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
    sugar.toast;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
    toast('Welcome');
end;

procedure TForm1.Button2Click(Sender: TObject);
var
	pnl: TPanel;
begin
    pnl := TPanel.Create(Application);
    pnl.Visible := true
end;

end.

