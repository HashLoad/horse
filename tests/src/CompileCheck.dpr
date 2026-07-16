program CompileCheck;
{$APPTYPE CONSOLE}

{$IFNDEF FPC}
  {$I HorseTestDefines.inc}
{$ENDIF}

uses
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

begin
  // Apenas uma chamada estática simples para forçar a compilação de todo o grafo de units
  THorse.GetActivePort;
end.
