unit Horse;

interface

uses
  System.SysUtils, System.Generics.Collections, System.Types, CGIApp,
  IPPeerServer, IPPeerAPI, IdHTTPWebBrokerBridge, Web.HTTPApp, Web.WebReq,
  Web.WebBroker, Horse.HTTP, System.Hash, Horse.Router, IdContext;

type
  EHorseCallbackInterrupted = Horse.HTTP.EHorseCallbackInterrupted;

  THorseList = Horse.HTTP.THorseList;

  THorseRequest = Horse.HTTP.THorseRequest;

  THorseHackRequest = Horse.HTTP.THorseHackRequest;

  THorseResponse = Horse.HTTP.THorseResponse;

  THorseHackResponse = Horse.HTTP.THorseHackResponse;

  THorseCallback = Horse.Router.THorseCallback;

  THorse = class
  private
    FPort: Integer;
    FRoutes: THorseRouterTree;
    procedure OnAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String;
      var VUsername, VPassword: String; var VHandled: Boolean);
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

uses Horse.Constants, Horse.WebModule, System.IOUtils, IdSchedulerOfThreadPool;

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
  FRoutes.free;
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

procedure THorse.OnAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String;
  var VUsername, VPassword: String; var VHandled: Boolean);
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

procedure THorse.RegisterRoute(AHTTPType: TMethodType; APath: string; ACallback: THorseCallback);
begin
  if APath.EndsWith('/') then
    APath := APath.Remove(High(APath) - 1, 1);

  if not APath.StartsWith('/') then
    APath := '/' + APath;

  FRoutes.RegisterRoute(AHTTPType, APath, ACallback);
end;

procedure THorse.Start;
var
  LHTTPWebBroker: TIdHTTPWebBrokerBridge;
  LAttach: string;
begin
  WebRequestHandler.WebModuleClass := WebModuleClass;
  LHTTPWebBroker := TIdHTTPWebBrokerBridge.Create(nil);
  try
    try

      LHTTPWebBroker.OnParseAuthentication := OnAuthentication;

      LHTTPWebBroker.DefaultPort := FPort;
      Writeln(Format(START_RUNNING, [FPort]));
      LHTTPWebBroker.Active := True;
      LHTTPWebBroker.StartListening;
      Write('Press return to stop ...');

      Read(LAttach);

      LHTTPWebBroker.Active := False;
      LHTTPWebBroker.Bindings.Clear;
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;
  finally
    LHTTPWebBroker.free;
  end;
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
