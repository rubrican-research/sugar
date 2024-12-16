unit rtti_test;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, RTTICtrls;

type

	{ TForm2 }

	{ TRecord }

    TRecord = class(TPersistent)
	private
		Fnames: TStringList;
		procedure Setnames(AValue: TStringList);
    published
        property names: TStringList read Fnames write Setnames;
	end;

    TForm2 = class(TForm)
		TIComboBox1: TTIComboBox;
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
    private
        myR : TRecord;
    public

    end;

var
    Form2: TForm2;

implementation

{$R *.lfm}

{ TRecord }

procedure TRecord.Setnames(AValue: TStringList);
begin
	if Fnames=AValue then Exit;
	Fnames:=AValue;
end;

{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin
    myR := TRecord.Create;
    TIComboBox1.Link.TIObject := myR;
    TIComboBox1.Link.TIPropertyName:='names';
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
    myR.Free;
end;

end.

