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
  Horse.Core;

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
  Horse.Exception.Interrupted;

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
begin
  Handled := True;
  LRequest := THorseRequest.Create(Request);
  LResponse := THorseResponse.Create(Response);
  try
    try
      FHorse.Routes.Execute(LRequest, LResponse)
    except
      on E: Exception do
        if not E.InheritsFrom(EHorseCallbackInterrupted) then
          raise;
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
