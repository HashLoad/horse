{$IF DEFINED(HORSE_APACHE) OR DEFINED(HORSE_ISAPI)}
library HostedProviderCheck;
{$ELSE}
program HostedProviderCheck;
{$ENDIF}

{$IFDEF FPC}{$MODE DELPHI}{$H+}{$ENDIF}
{$IFDEF FPC}{$MODESWITCH CVAR}{$ENDIF}
{$IFNDEF FPC}
{$IFNDEF HORSE_APACHE}{$IFNDEF HORSE_ISAPI}{$APPTYPE CONSOLE}{$ENDIF}{$ENDIF}
{$ENDIF}

uses
{$IF DEFINED(FPC) AND DEFINED(UNIX)}
  cthreads,
{$ENDIF}
  Horse, Horse.Commons
{$IFDEF HORSE_APACHE}
{$IFDEF FPC}
  , httpd24, fpApache24, custapache24
{$ELSE}
  , Web.HTTPD24Impl
{$ENDIF}
{$ENDIF}
  ;

{$IFDEF HORSE_APACHE}
var
{$IFDEF FPC}
  ApacheModuleData: module; public name 'horse_hosted_module';
{$ELSE}
  ApacheModuleData: TApacheModuleData;
{$ENDIF}
exports ApacheModuleData name 'horse_hosted_module';
{$ENDIF}

procedure Ping(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.AddHeader('X-Horse-Hosted', 'ok');
  Res.Send('pong');
end;

procedure Query(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send(Req.Query['v']);
end;

procedure Body(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.ContentType('application/json; charset=utf-8').Send(Req.Body);
end;

procedure Form(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send(Req.ContentFields['v']);
end;

begin
{$IFDEF HORSE_RADIX_ROUTER}
  THorse.UseRadixRouter;
{$ENDIF}
{$IFDEF HORSE_APACHE}
  THorse.DefaultModule := @ApacheModuleData;
  THorse.HandlerName := 'horse-hosted-handler';
{$IFDEF FPC}
  THorse.ModuleName := 'horse_hosted_module';
{$ENDIF}
{$ENDIF}
  THorse.Get('/ping', Ping);
  THorse.Get('/query', Query);
  THorse.Post('/body', Body);
  THorse.Put('/form', Form);
{$IFDEF HORSE_FCGI}
  // The container publishes no ports; use the provider's supported default bind.
{$IFDEF HORSE_RADIX_ROUTER}
  THorse.Listen(19282, '0.0.0.0');
{$ELSE}
  THorse.Listen(19281, '0.0.0.0');
{$ENDIF}
{$ELSE}
  THorse.Listen;
{$ENDIF}
end.
