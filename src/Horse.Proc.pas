unit Horse.Proc;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

{$I Horse.FunctionReferences.inc}

interface

{$IF NOT DEFINED(FPC)}
uses
  System.SysUtils;
{$ENDIF}

type
{$IF DEFINED(FPC)}
  {$IF DEFINED(HORSE_FPC_FUNCTIONREFERENCES)}
  TProc = reference to procedure;
  TNextProc = TProc;
  TProc<T> = reference to procedure(Arg1: T);
  {$ELSE}
  TNextProc = procedure of object;
  TProc = procedure;
  TProc<T> = procedure(Arg1: T);
  {$ENDIF}
{$ELSE}
  TNextProc = System.SysUtils.TProc;
  TProc = System.SysUtils.TProc;
{$ENDIF}

implementation

end.
