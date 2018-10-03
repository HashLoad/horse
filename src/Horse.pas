unit Horse;

interface

uses
  System.SysUtils, System.Generics.Collections, System.Types, CGIApp,
  IPPeerServer, IPPeerAPI, IdHTTPWebBrokerBridge, Web.HTTPApp, Web.WebReq,
  Web.WebBroker, Horse.HTTP, System.Hash, Horse.Router;

type
  THorseParams = Horse.HTTP.THorseParams;

  THorseRequest = Horse.HTTP.THorseRequest;

  THorseHackRequest = Horse.HTTP.THorseHackRequest;

  THorseResponse = Horse.HTTP.THorseResponse;

  THorseHackResponse = Horse.HTTP.THorseHackResponse;

  THorseCallback = Horse.Router.THorseCallback;


  THorse = class
  private
    FPort: Integer;
    FRoutes: THorseRouterTree;
    function IsDev: Boolean;
    procedure StartDev;
    procedure StartProd;
    procedure Initialize;
    procedure RegisterRoute(AHTTPType: TMethodType; APath: string; ACallback: THorseCallback);
    class var FInstance: THorse;
  public
    destructor Destroy; override;
    constructor Create(APort: Integer); overload;
    constructor Create; overload;
    property Port: Integer read FPort write FPort;
    property Routes: THorseRouterTree read FRoutes write FRoutes;
    procedure Use(APath: string; ACallback: THorseCallback); overload;
    procedure Use(ACallback: THorseCallback); overload;
    procedure Get(APath: string; ACallback: THorseCallback);
    procedure Put(APath: string; ACallback: THorseCallback);
    procedure Post(APath: string; ACallback: THorseCallback);
    procedure Delete(APath: string; ACallback: THorseCallback);
    procedure Start;
    class function GetInstance: THorse;
  end;

implementation

{ THorse }

uses Horse.Constants, Horse.WebModule, System.IOUtils;

constructor THorse.Create(APort: Integer);
begin
  FPort := APort;
  Initialize;
end;

constructor THorse.Create;
begin
  FPort := DEFAULT_PORT;
  Initialize;
end;

destructor THorse.Destroy;
begin
  FRoutes.Free;
  inherited;
end;

procedure THorse.Delete(APath: string; ACallback: THorseCallback);
begin
  RegisterRoute(mtDelete, APath, ACallback);
end;

procedure THorse.Initialize;
begin
  FInstance := Self;
  FRoutes := THorseRouterTree.Create;
end;

procedure THorse.Get(APath: string; ACallback: THorseCallback);
begin
  RegisterRoute(mtGet, APath, ACallback);
end;

class function THorse.GetInstance: THorse;
begin
  Result := FInstance;
end;

function THorse.IsDev: Boolean;
var
  LHorseDev: string;
begin
  LHorseDev := GetEnvironmentVariable(HORSE_ENV);
  Result := LHorseDev.IsEmpty or (LowerCase(LHorseDev) = ENV_D) or (LowerCase(LHorseDev) = ENV_DEV) or
    (LowerCase(LHorseDev) = ENV_DEVELOPMENT);
end;

procedure THorse.Post(APath: string; ACallback: THorseCallback);
begin
  RegisterRoute(mtPost, APath, ACallback);
end;

procedure THorse.Put(APath: string; ACallback: THorseCallback);
begin
  RegisterRoute(mtPut, APath, ACallback);
end;

procedure THorse.RegisterRoute(AHTTPType: TMethodType; APath: string; ACallback: THorseCallback);
begin
  if not APath.StartsWith('/') then
    APath := '/' + APath;

  if APath.EndsWith('/') then
    APath := APath.Remove(High(APath) - 1, 1);

  FRoutes.RegisterRoute(AHTTPType, APath, ACallback);
end;

procedure THorse.Start;
begin
  if IsDev then
    StartDev
  else
    StartProd;
end;

procedure THorse.StartDev;
var
  LHTTPWebBroker: TIdHTTPWebBrokerBridge;
begin
  WebRequestHandler.WebModuleClass := WebModuleClass;
  LHTTPWebBroker := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LHTTPWebBroker.DefaultPort := FPort;
    Writeln(Format(START_RUNNING, [FPort]));
    while True do
      LHTTPWebBroker.Active := True;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end
end;

procedure THorse.StartProd;
begin
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  Application.Run;
end;

procedure THorse.Use(ACallback: THorseCallback);
begin
  FRoutes.RegisterMiddleware('/', ACallback);
end;

procedure THorse.Use(APath: string; ACallback: THorseCallback);
begin
  FRoutes.RegisterMiddleware(APath, ACallback);
end;

end.
