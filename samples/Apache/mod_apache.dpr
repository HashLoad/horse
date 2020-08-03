library mod_apache;

uses
  System.SysUtils,
  Horse.Apache;

{$R *.res}

(*
 httpd.conf entries:

 LoadModule apache_module modules/mod_apache.dll

 <Location /apache_horse>
    SetHandler mod_apache-handler
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
exports
  GModuleData name 'apache_module';

var
  App: THorse;

begin
  App := THorse.Create;

  App.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('pong');
    end);

  App.Start;
end.
