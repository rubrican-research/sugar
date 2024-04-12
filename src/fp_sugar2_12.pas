{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fp_sugar2_12;

{$warn 5023 off : no warning about unused units}
interface

uses
    nsort, sugar.collections, sugar.consoleapp, sugar.csshelper, 
    sugar.ddldatatypes, sugar.ddlmodel, sugar.htmlbuilder.alpinejs, 
    sugar.htmlbuilder.htmx, sugar.htmlbuilder, sugar.htmlbuilder.vue, 
    sugar.htmlfactory, sugar.httphelper, sugar.jshelpers, sugar.jsonbuilder, 
    sugar.jsondb, sugar.jsonlib, sugar.logger, sugar.markdown, 
    sugar.modelbase, sugar.ormgen, sugar.pascalcodemodel, sugar.profiler, 
    sugar.querybuilder, sugar.sort, sugar.sqlitehelper, sugar.templater, 
    sugar.textfiler, sugar.threadwriter, sugar.uihelper, sugar.utils, 
    sugar.vuejs, sugar.webpage, sugar.htmlpage, assets, bulma, semanticui, 
    webui, sugar.jsGoogleChart, sugar.jscharts, sugar.threadloader, 
    LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fp_sugar2_12', @Register);
end.
