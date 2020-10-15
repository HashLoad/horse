unit Horse.Proc;

{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

{$IF NOT DEFINED(FPC)}
uses
  System.SysUtils;
{$ENDIF}

type
  TNextProc = {$IF DEFINED(FPC)} procedure of object {$ELSE} System.SysUtils.TProc {$ENDIF};
  TProc = {$IF DEFINED(FPC)} procedure {$ELSE} System.SysUtils.TProc {$ENDIF};
{$IF DEFINED(FPC)}
  TProc<T> = procedure(Arg1: T);
{$ENDIF}

implementation

end.
