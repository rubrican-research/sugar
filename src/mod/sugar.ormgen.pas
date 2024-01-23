unit sugar.ormgen;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, sugar.ddlmodel, sugar.pascalcodemodel;

function genUnitFromSchema(schema: DSchema; unt: RbUnit): RbUnit;

// helper functions
function properCase(s: string): string;

implementation

function properCase(s: string): string;
begin
    Result := LowerCase(s);
    Result[1] := uppercase(string(Result[1]))[1];
end;

function genUnitFromSchema(schema: DSchema; unt: RbUnit): RbUnit;
const
  TBL_CLASS_TEMPLATE = 'O%s';
var
    tbl: DTable;
    i, j: integer;
    cls: RbClass;
    fld: DField;
    helperUnit : RbUnit;
    fieldEnums: RbEnum;
    enum_value : string;
    enum_toString, enum_fromString : TStringList;
    enumHelper : RbTypeHelper;
    proj : RbPascalProject;

begin
    enum_toString := TStringList.Create;
    enum_fromString := TStringList.Create;

    {Create a unit to hold the Field Enums and Type helpers}
    proj := RbPascalProject(unt.Owner);
    helperUnit := proj.unit_(unt.Name + 'Helper');

    {Use the helper unit}
    unt.intf_uses(helperUnit.Name);

    {loop through each table in the schema}
    for i := 0 to schema.Count - 1 do
    begin
        tbl := schema.Items[i];

        {Define the classname of the ORM object}
        cls := unt.Classes.get(Format(TBL_CLASS_TEMPLATE, [ProperCase(tbl.Name)]));
        fieldEnums := helperUnit.intf_enum.get(Format('en%sFields',[cls.Name]));
        {Type Helper}
        enumHelper := RbTypeHelper.Create;
        enumHelper
            .HelperFor(fieldEnums.Name) {helper for}
            .Name(fieldEnums.Name+'Helper'); {name}

        {Add the Type Helper to the helper unit}
        helperUnit.Classes.add(enumHelper);

        {Loop through columns in the current table}
        for j := 0 to tbl.Count - 1 do
        begin
            fld := tbl.Items[j];
            cls.crwProp(fld.Name, fld.DataType, fld.Comment);
            enum_value := Format('%s_%s',[LowerCase(tbl.Name), fld.Name]);
            fieldEnums.add(enum_value);
            enum_toString.Add(Format('%s: Result := ''%s'';',[enum_value, fld.Name]));
            enum_fromString.Add(Format('''%s'': Self := %s;',[fld.Name, enum_value]));
        end;

        {toString function}
        enumHelper.publ.procs.get('toString')
            .ProcType(procFunction)
            .return_type('string')
            .code(format('case Self of  %s ' + sLinebreak +
                              'end;',[enum_toString.Text]));
        {fromString function}
        enumHelper.publ.procs.get('fromString')
            .ProcType(procProcedure)
            .params('_val: string')
            .code(format('case _val of  %s ' + sLinebreak +
                  'end;',[enum_fromString.Text]));

        {Reset the string lists}
        enum_fromString.Clear;
        enum_toString.Clear;
    end;
    enum_fromString.Free;
    enum_ToString.Free;
    Result := unt;
end;


end.
