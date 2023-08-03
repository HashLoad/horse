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
    constructor Create; reintroduce; overload;
    constructor Create(AMessage: string); reintroduce; overload;
  end;

implementation

constructor EHorseCallbackInterrupted.Create;
begin
  inherited Create(EmptyStr);
end;

constructor EHorseCallbackInterrupted.Create(AMessage: string);
begin
  inherited Create(AMessage);
end;

end.
