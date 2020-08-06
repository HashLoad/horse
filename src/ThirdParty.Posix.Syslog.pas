{ ****************************************************************************** }
{ }
{ Linux Daemon with Delphi }
{ }
{ Author: Paolo Rossi (2017) }
{ }
{ http://www.paolorossi.net }
{ http://blog.paolorossi.net }
{ https://github.com/paolo-rossi }
{ }
{ ****************************************************************************** }
unit ThirdParty.Posix.Syslog;

interface

{$IFDEF POSIX}

uses
  System.SysUtils,
  Posix.Base;

const // openlog() option
  LOG_PID = $01;
  LOG_CONS = $02;
  LOG_ODELAY = $04;
  LOG_NDELAY = $08;
  LOG_NOWAIT = $10;
  LOG_PERROR = $20;

const // openlog() facility
  LOG_KERN = 0 shl 3;
  LOG_USER = 1 shl 3;
  LOG_MAIL = 2 shl 3;
  LOG_DAEMON = 3 shl 3;
  LOG_AUTH = 4 shl 3;
  LOG_SYSLOG = 5 shl 3;
  LOG_LPR = 6 shl 3;
  LOG_NEWS = 7 shl 3;
  LOG_UUCP = 8 shl 3;
  LOG_CRON = 9 shl 3;
  LOG_AUTHPRIV = 10 shl 3;
  LOG_FTP = 11 shl 3;
  LOG_LOCAL0 = 16 shl 3;
  LOG_LOCAL1 = 17 shl 3;
  LOG_LOCAL2 = 18 shl 3;
  LOG_LOCAL3 = 19 shl 3;
  LOG_LOCAL4 = 20 shl 3;
  LOG_LOCAL5 = 21 shl 3;
  LOG_LOCAL6 = 22 shl 3;
  LOG_LOCAL7 = 23 shl 3;
  LOG_NFACILITIES = 24;
  LOG_FACMASK = $03F8;
  INTERNAL_NOPRI = $10;
  INTERNAL_MARK = LOG_NFACILITIES shl 3;

const // setlogmask() level
  LOG_EMERG = 0;
  LOG_ALERT = 1;
  LOG_CRIT = 2;
  LOG_ERR = 3;
  LOG_WARNING = 4;
  LOG_NOTICE = 5;
  LOG_INFO = 6;
  LOG_DEBUG = 7;
  LOG_PRIMASK = $07;

procedure closelog; cdecl;
  external libc name _PU + 'closelog';

procedure openlog(ident: MarshaledAString; option: LongInt; facility: LongInt); cdecl;
  external libc name _PU + 'openlog';

function setlogmask(mask: LongInt): LongInt; cdecl;
  external libc name _PU + 'setlogmask';

procedure _syslog(priority: LongInt; _format: MarshaledAString; args: array of const); cdecl;
  external libc name _PU + 'syslog';

// procedure openlog2(ident: MarshaledAString; option: LongInt; facility: LongInt); cdecl;

procedure Syslog(APriority: LongInt; const AFormat: string); overload;

procedure Syslog(APriority: LongInt; const AFormat: string; AArgs: array of const); overload;

{$ENDIF}

implementation

{$IFDEF POSIX}

procedure Syslog(APriority: LongInt; const AFormat: string);
var
  LMarshaller: TMarshaller;
  str: MarshaledAString;
begin
  str := LMarshaller.AsAnsi(AFormat, CP_UTF8).ToPointer;
  _syslog(APriority, str, []);
end;

procedure Syslog(APriority: LongInt; const AFormat: string; AArgs: array of const);
begin
  Syslog(APriority, Format(AFormat, AArgs));
end;

{$ENDIF}

end.
