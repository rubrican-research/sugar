unit memocurrentword;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

	{ TForm1 }

    TForm1 = class(TForm)
		Label1: TLabel;
		Memo1: TMemo;
		procedure Memo1Click(Sender: TObject);
		procedure Memo1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
			);
    private

    public
        procedure showCurrentWord;
    end;

var
    Form1: TForm1;

implementation

{$R *.lfm}

uses
    sugar.uihelper, sugar.logger;

{ TForm1 }

procedure TForm1.Memo1Click(Sender: TObject);
begin
    showCurrentWord;
end;

procedure TForm1.Memo1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    showCurrentWord;
end;

procedure TForm1.showCurrentWord;
begin
    label1.Caption := getCurrentWord(Memo1);
end;

initialization
    startLog();


end.

