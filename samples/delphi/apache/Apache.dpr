library Apache;

{$R *.res}

(*
 httpd.conf entries:
 LoadModule apache_horse_module modules/Apache.dll
 <Location /apache_horse>
    SetHandler apache_horse_module-handle
 </Location>
 To use the feature:
 http://localhost/apache_horse/ping
 These entries assume that the output directory for this project is the apache/modules directory.
 httpd.conf entries should be different if the project is changed in these ways:
   1. The TApacheModuleData variable name is changed.
   2. The project is renamed.
   3. The output directory is not the apache/modules directory.
   4. The dynamic library extension depends on a platform. Use .dll on Windows and .so on Linux.
*)

// Declare exported variable so that Apache can access this module.

uses Horse, Web.HTTPD24Impl;

var
  ApacheModuleData: TApacheModuleData;

exports
  ApacheModuleData name 'apache_horse_module';

begin
  // Need to set "HORSE_APACHE" compilation directive

  THorse.DefaultModule := @ApacheModuleData;
  THorse.HandlerName := 'apache_horse_module-handle';

  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('pong');
    end);

  THorse.Listen;
end.
