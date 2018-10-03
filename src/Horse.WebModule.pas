unit Horse.WebModule;

interface

uses System.SysUtils, System.IOUtils, System.Classes, Web.HTTPApp, Horse,
  System.RegularExpressions;

type
  THorseResultRegex = record
    Path: string;
    Sucess: Boolean;
    constructor Create(APath: string; ASucess: Boolean);
  end;

  THorseQueueMiddlewares = class
  private
    FMiddlewares: THorseMiddlewares;
    FRequest: THorseRequest;
    FResponse: THorseResponse;
  public
    procedure Next;
    constructor Create(AMiddlewares: THorseMiddlewares; ARequest: THorseRequest; AResponse: THorseResponse);
  end;

  THorseWebModule = class(TWebModule)
    procedure HandlerAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    FHorse: THorse;
  public
    property Horse: THorse read FHorse write FHorse;
    constructor Create(AOwner: TComponent); override;
  end;

var
  WebModuleClass: TComponentClass = THorseWebModule;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}
{$R *.dfm}

constructor THorseWebModule.Create(AOwner: TComponent);
begin
  inherited;
  FHorse := THorse.GetInstance;
end;

procedure THorseWebModule.HandlerAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse;
  var Handled: Boolean);
var
  LRequest: THorseRequest;
  LResponse: THorseResponse;
begin
  LRequest := THorseRequest.Create(Request);
  LResponse := THorseResponse.Create(Response);

  if FHorse.Routes.CanExecute(LRequest) then
  begin
    FHorse.Routes.Execute(LRequest, LResponse);
  end
  else
  begin
    Response.Content := 'Not Found';
    Response.StatusCode := 404;
  end;
end;

{ THorseResultRegex }

constructor THorseResultRegex.Create(APath: string; ASucess: Boolean);
begin
  Path := APath;
  Sucess := ASucess;
end;

{ THorseQueueMidleware }

constructor THorseQueueMiddlewares.Create(AMiddlewares: THorseMiddlewares; ARequest: THorseRequest;
  AResponse: THorseResponse);
begin
  FMiddlewares := AMiddlewares;
  FRequest := ARequest;
  FResponse := AResponse;
end;

procedure THorseQueueMiddlewares.Next;
begin
  if FMiddlewares.Count > 0 then
    FMiddlewares.Dequeue.Execute(FRequest, FResponse, Next);
end;

end.
