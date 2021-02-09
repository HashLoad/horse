unit Horse.WebModule;
{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}
interface

uses
{$IF DEFINED(FPC)}
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb,
{$ELSE}
  System.SysUtils, System.Classes, Web.HTTPApp,
{$ENDIF}
  Horse.Core, Horse.Commons;

type

{$IF DEFINED(FPC)}
  THorseWebModule = class(TFPWebModule)
      procedure DoOnRequest(ARequest: TRequest; AResponse: TResponse; var AHandled: Boolean); override;
{$ELSE}
  THorseWebModule = class(TWebModule)
{$ENDIF}
    procedure HandlerAction(Sender: TObject; Request: {$IF DEFINED(FPC)}TRequest{$ELSE}  TWebRequest {$ENDIF}; Response: {$IF DEFINED(FPC)}TResponse{$ELSE}  TWebResponse {$ENDIF}; var Handled: Boolean);
  private
    FHorse: THorseCore;
  public
    property Horse: THorseCore read FHorse write FHorse;
    constructor Create(AOwner: TComponent); override;
  end;

var
  {$IF DEFINED(FPC)}
    HorseWebModule: THorseWebModule;
  {$ELSE}
    WebModuleClass: TComponentClass = THorseWebModule;
  {$ENDIF}


implementation

uses Horse.HTTP, Horse.Exception;

{%CLASSGROUP 'System.Classes.TPersistent'}
{$IF DEFINED(FPC)}
{$R Horse.WebModule.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}


constructor THorseWebModule.Create(AOwner: TComponent);
begin
  inherited;
  FHorse := THorseCore.GetInstance;
end;

{$IF DEFINED(FPC)}
procedure THorseWebModule.DoOnRequest(ARequest: {$IF DEFINED(FPC)}TRequest{$ELSE}  TWebRequest {$ENDIF}; AResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}  TWebResponse {$ENDIF}; var AHandled: Boolean);
begin
  HandlerAction(Self, ARequest, AResponse, AHandled);
end;
{$ENDIF}

procedure THorseWebModule.HandlerAction(Sender: TObject; Request: {$IF DEFINED(FPC)}TRequest{$ELSE}  TWebRequest {$ENDIF};
  Response: {$IF DEFINED(FPC)}TResponse{$ELSE}  TWebResponse {$ENDIF}; var Handled: Boolean);
var
  LRequest: THorseRequest;
  LResponse: THorseResponse;
begin
  Handled := true;
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
