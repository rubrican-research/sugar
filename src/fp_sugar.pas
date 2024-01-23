{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fp_sugar;

{$warn 5023 off : no warning about unused units}
interface

uses
     sugar.httphelper, sugar.utils, sugar.uihelper, sugar.collections, 
     sugar.logger, sugar.profiler, sugar.htmlbuilder, sugar.csshelper, 
     sugar.htmlfactory, sugar.textfiler, sugar.threadwriter, 
     sugar.sqlitehelper, sugar.jsonlib, sugar.ddldatatypes, 
     sugar.pascalcodemodel, sugar.ormgen, sugar.ddlmodel, sugar.querybuilder, 
     sugar.templater, sugar.modelbase, sugar.jsondb, sugar.webpage, 
     sugar.vuejs, sugar.jshelpers, sugar.jsonbuilder, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fp_sugar', @Register);
end.
