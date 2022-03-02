unit Horse.Exception.Interrupted;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils;
{$ELSE}
  System.SysUtils;
{$ENDIF}

type
  EHorseCallbackInterrupted = class(Exception)
    constructor Create; reintroduce;
  end;

implementation

constructor EHorseCallbackInterrupted.Create;
begin
  inherited Create(EmptyStr);
end;

end.
