unit Horse.Proc;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
  {$IF DEFINED(HORSE_FPC_FUNCTIONREFERENCES)}
    {$MODESWITCH FUNCTIONREFERENCES+}
  {$ENDIF}
{$ENDIF}

interface

{$IF NOT DEFINED(FPC)}
uses
  System.SysUtils;
{$ENDIF}

type
  TNextProc = {$IF DEFINED(FPC)}procedure of object{$ELSE}System.SysUtils.TProc{$ENDIF};
  TProc = {$IF DEFINED(FPC)}{$IF DEFINED(HORSE_FPC_FUNCTIONREFERENCES)}reference to {$ENDIF}procedure{$ELSE}System.SysUtils.TProc{$ENDIF};

  {$IF DEFINED(FPC)}
    TProc<T> = {$IF DEFINED(HORSE_FPC_FUNCTIONREFERENCES)}reference to {$ENDIF}procedure(Arg1: T);
  {$ENDIF}

implementation

end.
