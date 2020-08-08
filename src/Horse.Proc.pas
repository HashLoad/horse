unit Horse.Proc;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

{$IF DEFINED(FPC)}

type

  TProc = procedure;
  TProc<T> = procedure (Arg1: T);
  TProc<T1,T2> =  procedure (Arg1: T1; Arg2: T2);
  TProc<T1,T2,T3> =  procedure (Arg1: T1; Arg2: T2; Arg3: T3);
  TProc<T1,T2,T3,T4> = procedure (Arg1: T1; Arg2: T2; Arg3: T3; Arg4: T4);

{$ENDIF}

implementation

end.

