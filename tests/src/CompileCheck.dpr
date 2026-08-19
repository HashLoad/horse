program CompileCheck;
{$APPTYPE CONSOLE}

{$IFNDEF FPC}
  {$I HorseTestDefines.inc}
{$ENDIF}

uses
  {$IFDEF FPC}
  Generics.Defaults,
  Horse.Core.Param.Header,
  {$ENDIF}
  {$IFDEF HORSE_PROVIDER_IOCP}
  Horse.Provider.IOCP,
  {$ENDIF}
  {$IFDEF HORSE_PROVIDER_HTTPSYS}
  Horse.Provider.HttpSys,
  {$ENDIF}
  {$IFDEF HORSE_PROVIDER_APACHE}
  Horse.Provider.Apache,
  {$ENDIF}
  {$IFDEF HORSE_PROVIDER_CGI}
  Horse.Provider.CGI,
  {$ENDIF}
  {$IFDEF HORSE_PROVIDER_ISAPI}
  Horse.Provider.ISAPI,
  {$ENDIF}
  {$IFDEF HORSE_PROVIDER_DAEMON}
  Horse.Provider.Daemon,
  {$ENDIF}
  {$IFDEF HORSE_PROVIDER_VCL}
  Horse.Provider.VCL,
  {$ENDIF}
  {$IFDEF HORSE_PROVIDER_LCL}
  Horse.Provider.FPC.LCL,
  {$ENDIF}
  Horse;

{$IFDEF FPC}
var
  LHeaderComparer: IEqualityComparer<string>;
{$ENDIF}

begin
  // Apenas uma chamada estática simples para forçar a compilação de todo o grafo de units
  THorse.GetActivePort;
  {$IFDEF FPC}
  { Compile-time regression check for the platform-dependent specialization of
    IEqualityComparer<string>, plus a minimal behavior check when executed. }
  LHeaderComparer := THorseHeaderComparer.Create;
  if (not LHeaderComparer.Equals('Content-Type', 'content-type')) or
     (LHeaderComparer.GetHashCode('Content-Type') <>
      LHeaderComparer.GetHashCode('content-type')) then
    Halt(1);
  {$ENDIF}
end.
