unit main;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, sugar.htmlbuilder;

type

	{ TForm1 }

    TForm1 = class(TForm)
		Button1: TButton;
		Button10: TButton;
		Button11: TButton;
		Button12: TButton;
		Button2: TButton;
		Button3: TButton;
		Button4: TButton;
		Button5: TButton;
		Button6: TButton;
		Button7: TButton;
		Button8: TButton;
		Button9: TButton;
		Label1: TLabel;
		Memo1: TMemo;
		Panel1: TPanel;
		procedure Button1Click(Sender: TObject);
    private

    public

    end;

var
    Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
    with THtmlDiv.Create do
    begin
        with div_ do begin
            h1('Hello');
            p('Something is brewing in the distance');
		end;
        with div_ do begin
            with div_.ol_ do begin
                item('one');
                item('two');
                item('three');
                item('four');
			end;
            with div_ do begin
                with div_ do begin
                    h2('Nice');
                    p('so be it');
                    p('something is going to be great here');
                end;
            end;
		end;
        script(
            'let x = 0;'__
            'let y=10;'__
            'if (x==y){ console.log("Oh bother")}'
        );
        memo1.lines.add(html);
        Free;
	end;

end;

end.

