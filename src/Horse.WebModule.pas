unit Horse.WebModule;

interface

uses System.SysUtils, System.IOUtils, System.Classes, Web.HTTPApp, Horse.Core, System.RegularExpressions;

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

uses Horse.HTTP;

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
    Response.Content := 'Not Found';
    Response.StatusCode := 404;
    try
      FHorse.Routes.Execute(LRequest, LResponse);
    except
      on E: Exception do
        if not E.InheritsFrom(EHorseCallbackInterrupted) then
          raise;
    end;
  finally
    LRequest.Free;
    LResponse.Free;
  end;
end;

end.
