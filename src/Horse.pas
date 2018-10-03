unit Horse;

interface

uses
  System.SysUtils, System.Generics.Collections, System.Types, CGIApp,
  IPPeerServer, IPPeerAPI, IdHTTPWebBrokerBridge, Web.HTTPApp, Web.WebReq,
  Web.WebBroker, Horse.HTTP, System.Hash, IdContext;

type
  THorseList = Horse.HTTP.THorseList;

  THorseRequest = Horse.HTTP.THorseRequest;

  THorseHackRequest = Horse.HTTP.THorseHackRequest;

  THorseResponse = Horse.HTTP.THorseResponse;

  THorseHackResponse = Horse.HTTP.THorseHackResponse;

  THorseCallback = reference to procedure(ARequest: THorseRequest;
    AResponse: THorseResponse; ANext: TProc);

  THorseMiddleware = record
    MethodType: TMethodType;
    Callback: THorseCallback;
    procedure Execute(ARequest: THorseRequest; AResponse: THorseResponse;
      ANext: TProc);
    constructor Create(AMethodType: TMethodType; ACallback: THorseCallback);
  end;

  THorseMiddlewares = TQueue<THorseMiddleware>;

  THorseRoutes = TDictionary<string, THorseMiddlewares>;

  THorse = class
  private
    FPort: Integer;
    FRoutes: THorseRoutes;
    procedure OnAuthentication(AContext: TIdContext;
      const AAuthType, AAuthData: String; var VUsername, VPassword: String;
      var VHandled: Boolean);
    function IsDev: Boolean;
    procedure StartDev;
    procedure StartProd;
    procedure Initialize;
    procedure RegisterRoute(AHTTPType: TMethodType; APath: string;
      ACallback: THorseCallback);
    class var FInstance: THorse;
  public
    destructor Destroy; override;
    constructor Create(APort: Integer); overload;
    constructor Create; overload;
    property Port: Integer read FPort write FPort;
    property Routes: THorseRoutes read FRoutes write FRoutes;
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
  FRoutes := THorseRoutes.Create;
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
  Result := LHorseDev.IsEmpty or (LowerCase(LHorseDev) = ENV_D) or
    (LowerCase(LHorseDev) = ENV_DEV) or
    (LowerCase(LHorseDev) = ENV_DEVELOPMENT);
end;

procedure THorse.OnAuthentication(AContext: TIdContext;
  const AAuthType, AAuthData: String; var VUsername, VPassword: String;
  var VHandled: Boolean);
begin
  VHandled := True;
end;

procedure THorse.Post(APath: string; ACallback: THorseCallback);
begin
  RegisterRoute(mtPost, APath, ACallback);
end;

procedure THorse.Put(APath: string; ACallback: THorseCallback);
begin
  RegisterRoute(mtPut, APath, ACallback);
end;

procedure THorse.RegisterRoute(AHTTPType: TMethodType; APath: string;
  ACallback: THorseCallback);
var
  LMiddlewares: THorseMiddlewares;
  LMiddleware: THorseMiddleware;
begin
  if not APath.StartsWith('/') then
    APath := '/' + APath;

  if APath.EndsWith('/') then
    APath := APath.Remove(High(APath) - 1, 1);

  if not FRoutes.TryGetValue(APath, LMiddlewares) then
  begin
    LMiddlewares := THorseMiddlewares.Create;
    FRoutes.Add(APath, LMiddlewares);
  end;

  LMiddleware := THorseMiddleware.Create(AHTTPType, ACallback);
  LMiddlewares.Enqueue(LMiddleware);
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
    LHTTPWebBroker.OnParseAuthentication := OnAuthentication;
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
  RegisterRoute(mtAny, EmptyStr, ACallback);
end;

procedure THorse.Use(APath: string; ACallback: THorseCallback);
begin
  RegisterRoute(mtAny, APath, ACallback);
end;

{ THorseMiddleware }

constructor THorseMiddleware.Create(AMethodType: TMethodType;
  ACallback: THorseCallback);
begin
  MethodType := AMethodType;
  Callback := ACallback;
end;

procedure THorseMiddleware.Execute(ARequest: THorseRequest;
  AResponse: THorseResponse; ANext: TProc);
var
  LCalledNext: Boolean;
begin
  LCalledNext := False;
  Callback(ARequest, AResponse,
    procedure
    begin
      LCalledNext := True;
      ANext;
    end);
  if not LCalledNext then
    ANext;
end;

end.
