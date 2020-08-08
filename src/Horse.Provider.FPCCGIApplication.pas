unit Horse.Provider.FPCCGIApplication;
{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

{$IF DEFINED(FPC)}


uses

  SysUtils, Classes, fpCGI,
  Horse.Provider.Abstract, Horse.Constants, Horse.Proc;

type

  THorseProvider<T: class> = class(THorseProviderAbstract<T>)
  private
    class var FCGIApplication: TCGIApplication;
    class function GetDefaultCGIApplication: TCGIApplication;
    class function CGIApplicationIsNil: Boolean;
    class function GetDefaultPort: Integer; static;
    class function GetDefaultHost: string; static;
    class procedure InternalListen; virtual;
  public
    constructor Create; reintroduce; overload;
    class procedure Listen; overload; override;
    class procedure Listen(ACallback: TProc<T>); reintroduce; overload; static;
    class procedure Start; deprecated 'Use Listen instead';
    class destructor UnInitialize;
  end;

var
  ShowCleanUpErrors: Boolean = False;

{$ENDIF}

implementation

{$IF DEFINED(FPC)}


uses

  Horse.WebModule;

{ THorseProvider<T> }

class function THorseProvider<T>.GetDefaultCGIApplication: TCGIApplication;
begin
  if CGIApplicationIsNil then
    FCGIApplication := Application;
  Result := FCGIApplication;
end;

class function THorseProvider<T>.CGIApplicationIsNil: Boolean;
begin
  Result := FCGIApplication = nil;
end;

constructor THorseProvider<T>.Create;
begin
  inherited Create;
end;

class function THorseProvider<T>.GetDefaultHost: string;
begin
  Result := DEFAULT_HOST;
end;

class function THorseProvider<T>.GetDefaultPort: Integer;
begin
  Result := DEFAULT_PORT;
end;

class procedure THorseProvider<T>.InternalListen;
var
  LCGIApplication: TCGIApplication;
begin
  inherited;
  LCGIApplication := GetDefaultCGIApplication;
  LCGIApplication.LegacyRouting := True;
  LCGIApplication.Initialize;
  DoOnListen;
  LCGIApplication.Run;
end;

class procedure THorseProvider<T>.Start;
begin
  Listen;
end;

class procedure THorseProvider<T>.Listen;
begin
  InternalListen;;
end;

class procedure THorseProvider<T>.Listen(ACallback: TProc<T>);
begin
  SetOnListen(ACallback);
  InternalListen;
end;

class destructor THorseProvider<T>.UnInitialize;
begin

end;

{$ENDIF}

end.
