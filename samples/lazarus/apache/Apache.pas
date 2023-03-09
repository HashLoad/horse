Library Apache;

{$DEFINE Apache2_4}
{$DEFINE FPCAPACHE_2_4}
{$MODE DELPHI}{$H+}

Uses
{$ifdef unix}
  cthreads,
{$endif}
  Horse, httpd24, fpApache24, custapache24;

const
  ModuleName = 'apache_horse_module';

var
  ApacheModuleData : module; {$ifdef unix} public name ModuleName;{$endif unix}

exports ApacheModuleData name ModuleName;

procedure GetPing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('pong');
end;

begin
  // Need to set "HORSE_APACHE" compilation directive

  THorse.Get('/ping', GetPing);

  THorse.DefaultModule := @ApacheModuleData;
  THorse.HandlerName := 'apache_horse_module-handle';
  THorse.ModuleName := ModuleName;

  THorse.Listen;
end.

