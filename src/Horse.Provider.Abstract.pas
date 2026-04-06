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
  =========================================================================== }
  Horse.Provider.Config,
{ =========================================================================== }
  Horse.Request,
  Horse.Response;

type
  THorseProviderAbstract = class(THorseCore)
  private
    class var FOnListen: TProc;
    class var FOnStopListen: TProc;
    // NOTE: FPort is intentionally NOT declared here.
    // Each concrete provider owns its own FPort class var so there is no
    // ambiguity between the Console provider's FPort and the CrossSocket
    // provider's FPort. Sharing a single FPort in the abstract base caused
    // silent port-not-changing bugs when both providers were compiled.
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
  =========================================================================== }
    class procedure ListenWithConfig(const APort: Integer;
      const AConfig: THorseCrossSocketConfig); virtual;
{ =========================================================================== }
{ ===========================================================================
  PATCH-ABS-3 — Execute class method
  Runs the Horse middleware/route pipeline for a given request+response pair.
  Providers that bypass TWebRequest (CrossSocket, raw socket, etc.) call this
  after populating THorseRequest via the request bridge.
  Implementation: THorseCore.Routes.Execute(Req, Res, nil)
  =========================================================================== }
    class procedure Execute(
      const ARequest:  THorseRequest;
      const AResponse: THorseResponse
    ); virtual;
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
  Default fallback for providers that do not use THorseCrossSocketConfig.
  Sets nothing (no shared FPort here) and delegates to the no-arg Listen.
  AConfig is intentionally ignored — non-CrossSocket providers have no use
  for it. CrossSocket overrides this completely.
  =========================================================================== }
class procedure THorseProviderAbstract.ListenWithConfig(const APort: Integer;
  const AConfig: THorseCrossSocketConfig);
begin
  Listen;
end;
{ =========================================================================== }

{ ===========================================================================
  PATCH-ABS-3 — Execute: runs the Horse middleware+route pipeline.
  THorseRouterTree.Execute(Req, Res, Next) walks all registered middleware and
  the matching route handler.  Passing nil for Next means the pipeline ends
  naturally when all handlers have run (EHorseCallbackInterrupted is raised
  internally by Horse when a middleware calls Next with no further handlers —
  this is normal and is caught by the provider's exception handler).
  =========================================================================== }
class procedure THorseProviderAbstract.Execute(
  const ARequest:  THorseRequest;
  const AResponse: THorseResponse
);
begin
  Routes.Execute(ARequest, AResponse);
end;
{ =========================================================================== }

end.
