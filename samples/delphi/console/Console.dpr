program Console;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Horse,
  System.Classes,
  System.SysUtils,
  {$IFDEF LINUX}
  Posix.Signal,    // Linux signal constants
  {$ENDIF}
  Horse.Provider.ISAPI in '..\..\..\src\Horse.Provider.ISAPI.pas',
  Horse.Provider.VCL in '..\..\..\src\Horse.Provider.VCL.pas',
  Horse.Request in '..\..\..\src\Horse.Request.pas',
  Horse.Response in '..\..\..\src\Horse.Response.pas',
  Horse.Rtti.Helper in '..\..\..\src\Horse.Rtti.Helper.pas',
  Horse.Rtti in '..\..\..\src\Horse.Rtti.pas',
  Horse.Session in '..\..\..\src\Horse.Session.pas',
  Horse.WebModule in '..\..\..\src\Horse.WebModule.pas',
  ThirdParty.Posix.Syslog in '..\..\..\src\ThirdParty.Posix.Syslog.pas',
  Web.WebConst in '..\..\..\src\Web.WebConst.pas',
  Horse.Callback in '..\..\..\src\Horse.Callback.pas',
  Horse.Commons in '..\..\..\src\Horse.Commons.pas',
  Horse.Constants in '..\..\..\src\Horse.Constants.pas',
  Horse.Core.Files in '..\..\..\src\Horse.Core.Files.pas',
  Horse.Core.Group.Contract in '..\..\..\src\Horse.Core.Group.Contract.pas',
  Horse.Core.Group in '..\..\..\src\Horse.Core.Group.pas',
  Horse.Core.Param.Config in '..\..\..\src\Horse.Core.Param.Config.pas',
  Horse.Core.Param.Field.Brackets in '..\..\..\src\Horse.Core.Param.Field.Brackets.pas',
  Horse.Core.Param.Field in '..\..\..\src\Horse.Core.Param.Field.pas',
  Horse.Core.Param.Header in '..\..\..\src\Horse.Core.Param.Header.pas',
  Horse.Core.Param in '..\..\..\src\Horse.Core.Param.pas',
  Horse.Core in '..\..\..\src\Horse.Core.pas',
  Horse.Core.Route.Contract in '..\..\..\src\Horse.Core.Route.Contract.pas',
  Horse.Core.Route in '..\..\..\src\Horse.Core.Route.pas',
  Horse.Core.RouterTree.NextCaller in '..\..\..\src\Horse.Core.RouterTree.NextCaller.pas',
  Horse.Core.RouterTree in '..\..\..\src\Horse.Core.RouterTree.pas',
  Horse.EnvironmentVariables in '..\..\..\src\Horse.EnvironmentVariables.pas',
  Horse.Exception.Interrupted in '..\..\..\src\Horse.Exception.Interrupted.pas',
  Horse.Exception in '..\..\..\src\Horse.Exception.pas',
  Horse.Mime in '..\..\..\src\Horse.Mime.pas',
  Horse.Provider.Config in '..\..\..\src\Horse.Provider.Config.pas',
  Horse.Provider.Console in '..\..\..\src\Horse.Provider.Console.pas',
  Horse.Provider.Daemon in '..\..\..\src\Horse.Provider.Daemon.pas';

var
  Config: THorseCrossSocketConfig;
  {$IFDEF LINUX}
  GShutdown: Boolean = False;
  {$ENDIF}

{$IFDEF LINUX}
// Signal handler — called on SIGTERM or SIGINT
procedure HandleSignal(Sig: Integer); cdecl;
begin
  GShutdown := True;
end;
{$ENDIF}


begin
  {$IFDEF MSWINDOWS}
  IsConsole := False;
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  {$IFDEF LINUX}
  // Install signal handlers
  signal(SIGTERM, HandleSignal);
  signal(SIGINT,  HandleSignal);
  {$ENDIF}


  // ── Routes ────────────────────────────────────────────────────────
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.ContentType('text/plain; charset=utf-8');
      Res.Send('pong');
    end);

  THorse.Get('/health',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('{"status":"ok"}');
    end);

  THorse.Post('/echo',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.ContentType(Req.ContentType);
      Res.Send(Req.Body<TStream>);
    end);

  // ── Config ────────────────────────────────────────────────────────
  Config                := THorseCrossSocketConfig.Default;
  Config.IoThreads      := 0;   // 0 = CrossSocket picks CPU-count threads
  Config.MaxBodySize    := 8 * 1024 * 1024;   // 8 MB
  Config.MaxHeaderSize  := 8192;
  Config.KeepAliveTimeout := 30;
  Config.ReadTimeout    := 20;

  // ── Start ─────────────────────────────────────────────────────────
  // ListenWithConfig is non-blocking: Start() launches epoll/IO threads
  // then returns.  The main thread MUST block or the process exits.
  THorse.ListenWithConfig(8080, Config);
  WriteLn('Server started on :8080  (press Ctrl+C to stop)');

  // ── Keep-alive loop ───────────────────────────────────────────────
  // ReadLn blocks the main thread.  In production replace with a
  // signal handler (see Phase 8).  In Docker, stdin is /dev/null so
  // ReadLn returns immediately — use the signal-handler pattern.

  {$IFDEF MSWINDOWS}
  ReadLn;
  THorse.StopListen;
  {$ENDIF}

  {$IFDEF LINUX}
  // Keep-alive loop — wakes every 100ms to check GShutdown
  while not GShutdown do
    Sleep(100);

  WriteLn('Shutdown signal received — draining...');
  THorse.StopListen;
  WriteLn('Stopped.');
  {$ENDIF}


end.

