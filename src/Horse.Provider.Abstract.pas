unit Horse.Provider.Abstract;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils,
  Horse.Proc,
  Generics.Collections,
{$ELSE}
  System.SysUtils,
  System.Generics.Collections,
{$ENDIF}
  Horse.Core,
  Horse.Core.Base,
{ ===========================================================================
  PATCH-ABS-1 — added unit Horse.Provider.Config
  =========================================================================== }
  Horse.Provider.Config,
{ =========================================================================== }
  Horse.Request,
  Horse.Response,
  Horse.Instance;

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
    class var FReadTimeout: Integer;
    class function GetReadTimeout: Integer; static;
    class procedure SetReadTimeout(const AValue: Integer); static;
{ =========================================================================== }
    class var FOnBeforeListen: TList<THorseServerLifecycleProc>;
    class var FOnAfterListen: TList<THorseServerLifecycleProc>;
    class var FOnBeforeStop: TList<THorseServerLifecycleProc>;
    class var FOnAfterStop: TList<THorseServerLifecycleProc>;

    class var FOnBeforeListenMethod: TList<THorseServerLifecycleMethod>;
    class var FOnAfterListenMethod: TList<THorseServerLifecycleMethod>;
    class var FOnBeforeStopMethod: TList<THorseServerLifecycleMethod>;
    class var FOnAfterStopMethod: TList<THorseServerLifecycleMethod>;
  protected
    class function GetOnListen: TProc; static;
    class procedure SetOnListen(const AValue: TProc); static;
    class procedure SetOnStopListen(const AValue: TProc); static;
    class procedure DoOnListen;
    class procedure DoOnStopListen;
  public
    class function GetActivePort: Integer; virtual;

    class function AddOnBeforeListen(const ACallback: THorseServerLifecycleProc): TClass; overload;
    class function AddOnAfterListen(const ACallback: THorseServerLifecycleProc): TClass; overload;
    class function AddOnBeforeStop(const ACallback: THorseServerLifecycleProc): TClass; overload;
    class function AddOnAfterStop(const ACallback: THorseServerLifecycleProc): TClass; overload;

    class function AddOnBeforeListen(const ACallback: THorseServerLifecycleMethod): TClass; overload;
    class function AddOnAfterListen(const ACallback: THorseServerLifecycleMethod): TClass; overload;
    class function AddOnBeforeStop(const ACallback: THorseServerLifecycleMethod): TClass; overload;
    class function AddOnAfterStop(const ACallback: THorseServerLifecycleMethod): TClass; overload;

    class procedure TriggerGlobalBeforeListen; static;
    class procedure TriggerGlobalAfterListen; static;
    class procedure TriggerGlobalBeforeStop; static;
    class procedure TriggerGlobalAfterStop; static;

    class procedure TriggerBeforeListen; virtual;
    class procedure TriggerAfterListen; virtual;
    class procedure TriggerBeforeStop; virtual;
    class procedure TriggerAfterStop; virtual;
    class property OnListen: TProc read GetOnListen write SetOnListen;
    class property OnStopListen: TProc read GetOnStopListen write SetOnStopListen;
{ PATCH-ABS-4 }
    class property MaxConnections: Integer read GetMaxConnections write SetMaxConnections;
    class property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
{ end PATCH-ABS-4 }
    class procedure Listen; virtual; abstract;
    class procedure StopListen; virtual;
    class procedure StopListenGraceful(const ATimeoutMS: Integer = 5000); virtual;
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
    class constructor Create;
    class destructor Destroy;
  end;

implementation

class constructor THorseProviderAbstract.Create;
begin
  FOnBeforeListen := TList<THorseServerLifecycleProc>.Create;
  FOnAfterListen := TList<THorseServerLifecycleProc>.Create;
  FOnBeforeStop := TList<THorseServerLifecycleProc>.Create;
  FOnAfterStop := TList<THorseServerLifecycleProc>.Create;

  FOnBeforeListenMethod := TList<THorseServerLifecycleMethod>.Create;
  FOnAfterListenMethod := TList<THorseServerLifecycleMethod>.Create;
  FOnBeforeStopMethod := TList<THorseServerLifecycleMethod>.Create;
  FOnAfterStopMethod := TList<THorseServerLifecycleMethod>.Create;
end;

class destructor THorseProviderAbstract.Destroy;
begin
  FOnBeforeListen.Free;
  FOnAfterListen.Free;
  FOnBeforeStop.Free;
  FOnAfterStop.Free;

  FOnBeforeListenMethod.Free;
  FOnAfterListenMethod.Free;
  FOnBeforeStopMethod.Free;
  FOnAfterStopMethod.Free;
end;

class function THorseProviderAbstract.GetActivePort: Integer;
begin
  Result := 0;
end;

class function THorseProviderAbstract.AddOnBeforeListen(const ACallback: THorseServerLifecycleProc): TClass;
begin
  Result := Self;
  FOnBeforeListen.Add(ACallback);
end;

class function THorseProviderAbstract.AddOnAfterListen(const ACallback: THorseServerLifecycleProc): TClass;
begin
  Result := Self;
  FOnAfterListen.Add(ACallback);
end;

class function THorseProviderAbstract.AddOnBeforeStop(const ACallback: THorseServerLifecycleProc): TClass;
begin
  Result := Self;
  FOnBeforeStop.Add(ACallback);
end;

class function THorseProviderAbstract.AddOnAfterStop(const ACallback: THorseServerLifecycleProc): TClass;
begin
  Result := Self;
  FOnAfterStop.Add(ACallback);
end;

class function THorseProviderAbstract.AddOnBeforeListen(const ACallback: THorseServerLifecycleMethod): TClass;
begin
  Result := Self;
  FOnBeforeListenMethod.Add(ACallback);
end;

class function THorseProviderAbstract.AddOnAfterListen(const ACallback: THorseServerLifecycleMethod): TClass;
begin
  Result := Self;
  FOnAfterListenMethod.Add(ACallback);
end;

class function THorseProviderAbstract.AddOnBeforeStop(const ACallback: THorseServerLifecycleMethod): TClass;
begin
  Result := Self;
  FOnBeforeStopMethod.Add(ACallback);
end;

class function THorseProviderAbstract.AddOnAfterStop(const ACallback: THorseServerLifecycleMethod): TClass;
begin
  Result := Self;
  FOnAfterStopMethod.Add(ACallback);
end;

class procedure THorseProviderAbstract.TriggerGlobalBeforeListen;
var
  LCallback: THorseServerLifecycleProc;
  LMethod: THorseServerLifecycleMethod;
begin
  for LCallback in FOnBeforeListen do
    LCallback(nil);
  for LMethod in FOnBeforeListenMethod do
    LMethod(nil);
end;

class procedure THorseProviderAbstract.TriggerGlobalAfterListen;
var
  LCallback: THorseServerLifecycleProc;
  LMethod: THorseServerLifecycleMethod;
begin
  for LCallback in FOnAfterListen do
    LCallback(nil);
  for LMethod in FOnAfterListenMethod do
    LMethod(nil);
end;

class procedure THorseProviderAbstract.TriggerGlobalBeforeStop;
var
  LCallback: THorseServerLifecycleProc;
  LMethod: THorseServerLifecycleMethod;
begin
  for LCallback in FOnBeforeStop do
    LCallback(nil);
  for LMethod in FOnBeforeStopMethod do
    LMethod(nil);
end;

class procedure THorseProviderAbstract.TriggerGlobalAfterStop;
var
  LCallback: THorseServerLifecycleProc;
  LMethod: THorseServerLifecycleMethod;
begin
  for LCallback in FOnAfterStop do
    LCallback(nil);
  for LMethod in FOnAfterStopMethod do
    LMethod(nil);
end;

class procedure THorseProviderAbstract.TriggerBeforeListen;
var
  LInstance: THorseCoreBase;
begin
  LInstance := GetHorseInstanceByPort(GetActivePort);
  if (LInstance <> nil) and (LInstance is THorseInstance) then
    THorseInstance(LInstance).TriggerBeforeListen
  else
    TriggerGlobalBeforeListen;
end;

class procedure THorseProviderAbstract.TriggerAfterListen;
var
  LInstance: THorseCoreBase;
begin
  LInstance := GetHorseInstanceByPort(GetActivePort);
  if (LInstance <> nil) and (LInstance is THorseInstance) then
    THorseInstance(LInstance).TriggerAfterListen
  else
    TriggerGlobalAfterListen;
end;

class procedure THorseProviderAbstract.TriggerBeforeStop;
var
  LInstance: THorseCoreBase;
begin
  LInstance := GetHorseInstanceByPort(GetActivePort);
  if (LInstance <> nil) and (LInstance is THorseInstance) then
    THorseInstance(LInstance).TriggerBeforeStop
  else
    TriggerGlobalBeforeStop;
end;

class procedure THorseProviderAbstract.TriggerAfterStop;
var
  LInstance: THorseCoreBase;
begin
  LInstance := GetHorseInstanceByPort(GetActivePort);
  if (LInstance <> nil) and (LInstance is THorseInstance) then
    THorseInstance(LInstance).TriggerAfterStop
  else
    TriggerGlobalAfterStop;
end;

class procedure THorseProviderAbstract.DoOnListen;
begin
  TriggerAfterListen;
  if Assigned(FOnListen) then
    FOnListen();
end;

class procedure THorseProviderAbstract.DoOnStopListen;
begin
  if Assigned(FOnStopListen) then
    FOnStopListen();
  TriggerAfterStop;
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
  TriggerBeforeStop;
  raise Exception.Create('StopListen not implemented');
end;

class procedure THorseProviderAbstract.StopListenGraceful(const ATimeoutMS: Integer);
begin
  THorseCore.SetIsShuttingDown(True);
  try
    StopListen;
  finally
    THorseCore.SetIsShuttingDown(False);
  end;
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

class function THorseProviderAbstract.GetReadTimeout: Integer;
begin
  Result := FReadTimeout;
end;

class procedure THorseProviderAbstract.SetReadTimeout(const AValue: Integer);
begin
  FReadTimeout := AValue;
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
var
  LInstance: THorseCoreBase;
  LPort: Integer;
begin
  LPort := 9000;
  if Assigned(ARequest) and (ARequest.RawWebRequest <> nil) then
    LPort := ARequest.RawWebRequest.ServerPort;

  LInstance := GetHorseInstanceByPort(LPort);
  if LInstance <> nil then
  begin
    LInstance.DoIncrementActiveRequests;
    try
      LInstance.GetRoutes.Execute(ARequest, AResponse);
    finally
      LInstance.DoDecrementActiveRequests;
    end;
  end
  else
  begin
    THorseCore.IncrementActiveRequests;
    try
      THorseCore.Routes.Execute(ARequest, AResponse);
    finally
      THorseCore.DecrementActiveRequests;
    end;
  end;
end;
{ =========================================================================== }

end.
