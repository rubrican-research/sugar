{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fp_sugar;

{$warn 5023 off : no warning about unused units}
interface

uses
    sugar.utils, sugar.uihelper, sugar.collections, sugar.logger, 
    sugar.profiler, sugar.csshelper, sugar.textfiler, sugar.threadwriter, 
    sugar.sqlitehelper, sugar.modelbase, sugar.ddldatatypes, 
    sugar.pascalcodemodel, sugar.ormgen, sugar.ddlmodel, sugar.querybuilder, 
    sugar.jsondb, sugar.httphelper, sugar.htmlbuilder, sugar.htmlbuilder.vue, 
    sugar.htmlbuilder.alpinejs, sugar.templater, sugar.htmlfactory, 
    sugar.jshelpers, sugar.jsonlib, sugar.webpage, sugar.vuejs, 
    sugar.jsonbuilder, sugar.consoleapp, sugar.sort, nsort, sugar.markdown, 
    sugar.htmlbuilder.htmx, sugar.threadloader, sugar.jsGoogleChart, 
    sugar.jscharts, sugar.htmlpage, assets, bulma, semanticui, webui, 
    sugar.fmkvdisplay, sugar.markdownhelper, sugar.tagUI, sugar.toast, 
    sugar.uistyles, sugar.animateui, sugar.dragui, sugar.register, 
    sugar.repositionui, git.api, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fp_sugar', @Register);
end.
