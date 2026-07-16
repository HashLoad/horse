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
  THorseWebModule = class(TFPWebModule)
    procedure DoOnRequest(ARequest: TRequest; AResponse: TResponse; var AHandled: Boolean); override;
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
procedure THorseWebModule.DoOnRequest(ARequest: {$IF DEFINED(FPC)}TRequest{$ELSE}  TWebRequest {$ENDIF}; AResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}  TWebResponse {$ENDIF}; var AHandled: Boolean);
begin
  HandlerAction(Self, ARequest, AResponse, AHandled);
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
