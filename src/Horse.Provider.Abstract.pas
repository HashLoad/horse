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
{ ===========================================================================
  PATCH-ABS-4 — MaxConnections raised to abstract base
  Each Indy-based concrete provider (Console, VCL, Daemon, Apache) already
  declares its own FMaxConnections class var and MaxConnections property that
  shadows this one — their existing behaviour is completely unchanged.
  For THorseProviderCrossSocket (which has no MaxConnections of its own), the
  inheritance chain resolves here: the value is stored but never forwarded to
  the CrossSocket transport layer. CrossSocket connection limits are configured
  via THorseCrossSocketConfig.MaxConnections instead. Raising the property
  here preserves source compatibility for projects that call
  THorse.MaxConnections := N and then switch to HORSE_CROSSSOCKET.
  =========================================================================== }
    class var FMaxConnections: Integer;
    class function GetMaxConnections: Integer; static;
    class procedure SetMaxConnections(const AValue: Integer); static;
{ =========================================================================== }
  protected
    class function GetOnListen: TProc; static;
    class procedure SetOnListen(const AValue: TProc); static;
    class procedure SetOnStopListen(const AValue: TProc); static;
    class procedure DoOnListen;
    class procedure DoOnStopListen;
  public
    class property OnListen: TProc read GetOnListen write SetOnListen;
    class property OnStopListen: TProc read GetOnStopListen write SetOnStopListen;
{ PATCH-ABS-4 }
    class property MaxConnections: Integer read GetMaxConnections write SetMaxConnections;
{ end PATCH-ABS-4 }
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
  PATCH-ABS-4 — MaxConnections getter/setter
  On Indy providers, each concrete THorseProvider declares its own
  FMaxConnections class var and static getter/setter that shadow these — so
  the Indy path never touches this code.  On CrossSocket, this is the only
  MaxConnections implementation; the stored value is accepted (compiles
  silently) but not forwarded to the transport layer.  Connection limits for
  CrossSocket are set via THorseCrossSocketConfig.MaxConnections.
  =========================================================================== }
class function THorseProviderAbstract.GetMaxConnections: Integer;
begin
  Result := FMaxConnections;
end;

class procedure THorseProviderAbstract.SetMaxConnections(const AValue: Integer);
begin
  FMaxConnections := AValue;
end;
{ =========================================================================== }

{ ===========================================================================
  PATCH-ABS-2 — implementation of ListenWithConfig
  Contract: every concrete provider MUST override this method.
  The abstract base has no FPort and no way to forward APort to Listen
  polymorphically — calling the no-arg Listen here would silently ignore
  the caller-supplied port (the exact bug this patch was written to fix).
  Raising here converts "silent wrong port" into an immediate compile-time-
  detectable oversight for any future provider that forgets to override.
  All existing patched providers (Console, Daemon, VCL, FPC.*) already
  override this and call SetPort(APort) before their own Listen.
  =========================================================================== }
class procedure THorseProviderAbstract.ListenWithConfig(const APort: Integer;
  const AConfig: THorseCrossSocketConfig);
begin
  raise Exception.CreateFmt(
    '%s must override ListenWithConfig — the base implementation cannot ' +
    'forward port %d to Listen. Override ListenWithConfig in the concrete ' +
    'provider and call SetPort(APort) before Listen.',
    [ClassName, APort]);
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
