unit Horse.WebModule;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  Classes,
  httpdefs,
  fpHTTP,
  fpWeb,
{$ELSE}
  System.Classes,
  Web.HTTPApp,
{$ENDIF}
  Horse.Core,
  Horse.Core.Base;

type
{$IF DEFINED(FPC)}
  { FPC-WEBMODULE-1 — HandleRequest is overridden directly (instead of
    DoOnRequest) to bypass TCustomFPWebModule.HandleRequest entirely. Horse
    uses neither fpWeb sessions nor fpWeb actions, and FPC trunk's
    TCustomFPWebModule.HandleRequest gained CSRF protection that reads
    Session.Variables for every write request (POST/PUT/DELETE/PATCH) BEFORE
    it calls DoOnRequest: TSessionHTTPModule.GetSession then raises 'Default
    session not available outside handlerequest' (module error 500) because
    FSessionRequest is never assigned on this dispatch path. Overriding
    DoOnRequest (upstream's approach) does NOT avoid it — the parent
    HandleRequest runs the CSRF check first. Dispatching straight into the
    Horse pipeline from an overridden HandleRequest avoids the whole
    session/CSRF/action machinery. (Upstream-PR candidate.) }
  THorseWebModule = class(TFPWebModule)
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
{$ELSE}
  THorseWebModule = class(TWebModule)
{$ENDIF}
    procedure HandlerAction(const Sender: TObject; const Request: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF}; const Response: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF}; var Handled: Boolean);
  private
    FHorse: THorseCore;
    class var FInstance: THorseWebModule;
  public
    property Horse: THorseCore read FHorse write FHorse;
    constructor Create(AOwner: TComponent); override;
    class function GetInstance: THorseWebModule;
  end;

var
{$IF DEFINED(FPC)}
  HorseWebModule: THorseWebModule;
{$ELSE}
  WebModuleClass: TComponentClass = THorseWebModule;
{$ENDIF}

implementation

uses

{$IF DEFINED(FPC)}
  SysUtils,
{$ELSE}
  System.SysUtils,
{$ENDIF}        
  Horse.Request,
  Horse.Response,
  Horse.Exception.Interrupted,
  Horse.Core.WebSocket
  {$IF NOT DEFINED(FPC)}
  , IdHTTPWebBrokerBridge,
  Horse.Provider.Indy.WebSocket
  {$ENDIF};

{$IF DEFINED(FPC)}
  {$R Horse.WebModule.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

class function THorseWebModule.GetInstance: THorseWebModule;
begin
  Result := FInstance;
end;

constructor THorseWebModule.Create(AOwner: TComponent);
begin
  {$IF DEFINED(FPC)}
  inherited CreateNew(AOwner, 0);
  {$ELSE}
  inherited;
  {$ENDIF}
  FHorse := THorseCore.GetInstance;
  FInstance := Self;
end;

{$IF DEFINED(FPC)}
procedure THorseWebModule.HandleRequest(ARequest: TRequest; AResponse: TResponse);
var
  LHandled: Boolean;
begin
  { FPC-WEBMODULE-1 — deliberately NOT calling inherited: see the class
    declaration comment. HandlerAction runs the full Horse pipeline and
    populates AResponse; fpWeb sends it after this returns. }
  LHandled := True;
  HandlerAction(Self, ARequest, AResponse, LHandled);

  { A handler that only sets headers (e.g. a HEAD route calling AddHeader
    without Send) leaves the response with neither a body nor a
    Content-Length header. An explicit Content-Length: 0 keeps every
    complete response deterministically framed for keep-alive clients. }
  if (AResponse.ContentStream = nil) and (AResponse.Contents.Count = 0)
    and (not AResponse.HeadersSent) then
    AResponse.ContentLength := 0;
end;
{$ENDIF}

procedure THorseWebModule.HandlerAction(const Sender: TObject; const Request: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF};
  const Response: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF}; var Handled: Boolean);
var
  LRequest: THorseRequest;
  LResponse: THorseResponse;
  LCore: THorseCoreBase;
  LPort: Integer;
begin
  Handled := True;
  LRequest := THorseRequest.Create(Request);
  LResponse := THorseResponse.Create(Response);

  {$IF NOT DEFINED(FPC)}
  if Request is TIdHTTPAppRequest then
  begin
    LRequest.Services.Add(THorseWebSocketUpgrader,
      TIndyWebSocketUpgrader.Create(
        TIdHTTPAppRequest(Request).GetThread,
        Request,
        Response
      ), True);
  end;
  {$ENDIF}

  try
    try
      LPort := Request.ServerPort;
      LCore := GetHorseInstanceByPort(LPort);
      if LCore <> nil then
      begin
        LCore.DoIncrementActiveRequests;
        try
          LCore.GetRoutes.Execute(LRequest, LResponse);
        finally
          LCore.DoDecrementActiveRequests;
        end;
      end
      else
      begin
        THorseCore.IncrementActiveRequests;
        try
          THorseCore.Routes.Execute(LRequest, LResponse);
        finally
          THorseCore.DecrementActiveRequests;
        end;
      end;
    except
      on E: Exception do
      begin
        if not E.InheritsFrom(EHorseCallbackInterrupted) then
          raise;
      end;
    end;
  finally
    if LRequest.Body<TObject> = LResponse.Content then
      LResponse.Content(nil);
    LRequest.Free;
    LResponse.Free;
  end;
end;

{$IF DEFINED(FPC)}
initialization
  RegisterHTTPModule(THorseWebModule);
{$ENDIF}

end.
