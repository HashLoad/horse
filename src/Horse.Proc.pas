unit Horse.Proc;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

{$IF DEFINED(FPC)}

type

  TNextProc = procedure of object;
  TProc = procedure;
  TProc<T> = procedure (Arg1: T);

{$ENDIF}

implementation

end.

