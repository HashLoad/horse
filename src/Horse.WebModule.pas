unit Horse.WebModule;

interface

uses System.SysUtils, System.Classes, Web.HTTPApp, Horse.Core, Horse.Commons;

type
  THorseWebModule = class(TWebModule)
    procedure HandlerAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    FHorse: THorseCore;
  public
    property Horse: THorseCore read FHorse write FHorse;
    constructor Create(AOwner: TComponent); override;
  end;

var
  WebModuleClass: TComponentClass = THorseWebModule;

implementation

uses Horse.HTTP, Horse.Exception, Web.WebConst;

{%CLASSGROUP 'System.Classes.TPersistent'}
{$R *.dfm}

constructor THorseWebModule.Create(AOwner: TComponent);
begin
  inherited;
  FHorse := THorseCore.GetInstance;
end;

procedure THorseWebModule.HandlerAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var
  LRequest: THorseRequest;
  LResponse: THorseResponse;
begin
  LRequest := THorseRequest.Create(Request);
  LResponse := THorseResponse.Create(Response);
  try
    try
      if not FHorse.Routes.Execute(LRequest, LResponse) then
      begin
        Response.Content := 'Not Found';
        Response.StatusCode := THTTPStatus.NotFound.ToInteger;
      end;
    except
      on E: Exception do
        if not E.InheritsFrom(EHorseCallbackInterrupted) then
          raise;
    end;
  finally
    if LRequest.Body<TObject> = THorseHackResponse(LResponse).GetContent then
      THorseHackResponse(LResponse).SetContent(nil);
    LRequest.Free;
    LResponse.Free;
  end;
end;

end.
