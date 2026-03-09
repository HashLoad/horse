unit Horse.Provider.Abstract;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils,
  Horse.Proc,
{$ELSE}
  System.SysUtils,
{$ENDIF}
  Horse.Core,
{ ===========================================================================
  PATCH-ABS-1 — added unit Horse.Provider.Config
  Reason: THorseProviderAbstract needs to declare ListenWithConfig, whose
  parameter type THorseCrossSocketConfig is defined in Horse.Provider.Config.
  Placing the record in a separate unit avoids a circular dependency between
  Horse.Provider.Abstract and Horse.Provider.CrossSocket.Server.
  =========================================================================== }
  Horse.Provider.Config;
{ =========================================================================== }

type
  THorseProviderAbstract = class(THorseCore)
  private
    class var FOnListen: TProc;
    class var FOnStopListen: TProc;
    class function GetOnStopListen: TProc; static;
  protected
    class function GetOnListen: TProc; static;
    class procedure SetOnListen(const AValue: TProc); static;
    class procedure SetOnStopListen(const AValue: TProc); static;
    class procedure DoOnListen;
    class procedure DoOnStopListen;
  public
    class property OnListen: TProc read GetOnListen write SetOnListen;
    class property OnStopListen: TProc read GetOnStopListen write SetOnStopListen;
    class procedure Listen; virtual; abstract;
    class procedure StopListen; virtual;
{ ===========================================================================
  PATCH-ABS-2 — added ListenWithConfig virtual class method
  Reason: THorseProviderCrossSocket overrides this to receive the full
  THorseCrossSocketConfig (TLS settings, timeouts, size limits, etc.).
  Default implementation delegates to Listen(APort) so all existing
  providers — Indy, VCL, CGI, Apache, Daemon — compile and run unchanged.
  =========================================================================== }
    class procedure ListenWithConfig(const APort: Integer;
      const AConfig: THorseCrossSocketConfig); virtual;
{ =========================================================================== }
  end;

implementation

class procedure THorseProviderAbstract.DoOnListen;
begin
  if Assigned(FOnListen) then
    FOnListen();
end;

class procedure THorseProviderAbstract.DoOnStopListen;
begin
  if Assigned(FOnStopListen) then
    FOnStopListen();
end;

class function THorseProviderAbstract.GetOnListen: TProc;
begin
  Result := FOnListen;
end;

class function THorseProviderAbstract.GetOnStopListen: TProc;
begin
  Result := FOnStopListen;
end;

class procedure THorseProviderAbstract.SetOnListen(const AValue: TProc);
begin
  FOnListen := AValue;
end;

class procedure THorseProviderAbstract.SetOnStopListen(const AValue: TProc);
begin
  FOnStopListen := AValue;
end;

class procedure THorseProviderAbstract.StopListen;
begin
  raise Exception.Create('StopListen not implemented');
end;

{ ===========================================================================
  PATCH-ABS-2 — implementation of ListenWithConfig
  =========================================================================== }
class procedure THorseProviderAbstract.ListenWithConfig(const APort: Integer;
  const AConfig: THorseCrossSocketConfig);
begin
  Listen;
end;
{ =========================================================================== }

end.
