unit Horse.API;

interface

uses Horse.HTTP, System.SysUtils, Horse.Router, Web.HTTPApp;

type
  EHorseCallbackInterrupted = Horse.HTTP.EHorseCallbackInterrupted;
  TProc = System.SysUtils.TProc;
  THorseList = Horse.HTTP.THorseList;
  THorseRequest = Horse.HTTP.THorseRequest;
  THorseHackRequest = Horse.HTTP.THorseHackRequest;
  THorseResponse = Horse.HTTP.THorseResponse;
  THorseHackResponse = Horse.HTTP.THorseHackResponse;
  THorseCallback = Horse.Router.THorseCallback;

  THorseAPI = class
  private
    FRoutes: THorseRouterTree;
    procedure Initialize;
    procedure RegisterRoute(AHTTPType: TMethodType; APath: string; ACallback: THorseCallback);
    class var FInstance: THorseAPI;
  public
    destructor Destroy; override;
    constructor Create; overload;
    property Routes: THorseRouterTree read FRoutes write FRoutes;
    procedure Use(APath: string; ACallback: THorseCallback); overload;
    procedure Use(ACallback: THorseCallback); overload;
    procedure Use(APath: string; ACallbacks: array of THorseCallback); overload;
    procedure Use(ACallbacks: array of THorseCallback); overload;

    procedure Get(APath: string; ACallback: THorseCallback); overload;
    procedure Get(APath: string; ACallbacks: array of THorseCallback); overload;
    procedure Put(APath: string; ACallback: THorseCallback); overload;
    procedure Put(APath: string; ACallbacks: array of THorseCallback); overload;
    procedure Post(APath: string; ACallback: THorseCallback); overload;
    procedure Post(APath: string; ACallbacks: array of THorseCallback); overload;
    procedure Delete(APath: string; ACallback: THorseCallback); overload;
    procedure Delete(APath: string; ACallbacks: array of THorseCallback); overload;

    procedure Start; virtual; abstract;
    class function GetInstance: THorseAPI;
  end;

implementation

{ THorse }

constructor THorseAPI.Create;
begin
  Initialize;
end;

destructor THorseAPI.Destroy;
begin
  FRoutes.free;
  inherited;
end;

procedure THorseAPI.Delete(APath: string; ACallbacks: array of THorseCallback);
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
  begin
    Delete(APath, LCallback);
  end;
end;

procedure THorseAPI.Delete(APath: string; ACallback: THorseCallback);
begin
  RegisterRoute(mtDelete, APath, ACallback);
end;

procedure THorseAPI.Initialize;
begin
  FInstance := Self;
  FRoutes := THorseRouterTree.Create;
end;

procedure THorseAPI.Get(APath: string; ACallback: THorseCallback);
begin
  RegisterRoute(mtGet, APath, ACallback);
end;

procedure THorseAPI.Get(APath: string; ACallbacks: array of THorseCallback);
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
  begin
    Get(APath, LCallback);
  end;
end;

class function THorseAPI.GetInstance: THorseAPI;
begin
  Result := FInstance;
end;

procedure THorseAPI.Post(APath: string; ACallback: THorseCallback);
begin
  RegisterRoute(mtPost, APath, ACallback);
end;

procedure THorseAPI.Put(APath: string; ACallback: THorseCallback);
begin
  RegisterRoute(mtPut, APath, ACallback);
end;

procedure THorseAPI.RegisterRoute(AHTTPType: TMethodType; APath: string; ACallback: THorseCallback);
begin
  if APath.EndsWith('/') then
    APath := APath.Remove(High(APath) - 1, 1);
  if not APath.StartsWith('/') then
    APath := '/' + APath;
  FRoutes.RegisterRoute(AHTTPType, APath, ACallback);
end;

procedure THorseAPI.Use(ACallbacks: array of THorseCallback);
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    Use(LCallback);
end;

procedure THorseAPI.Use(APath: string; ACallbacks: array of THorseCallback);
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    Use(APath, LCallback);
end;

procedure THorseAPI.Use(ACallback: THorseCallback);
begin
  FRoutes.RegisterMiddleware('/', ACallback);
end;

procedure THorseAPI.Use(APath: string; ACallback: THorseCallback);
begin
  FRoutes.RegisterMiddleware(APath, ACallback);
end;

procedure THorseAPI.Post(APath: string; ACallbacks: array of THorseCallback);
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    Post(APath, LCallback);
end;

procedure THorseAPI.Put(APath: string; ACallbacks: array of THorseCallback);
var
  LCallback: THorseCallback;
begin
  for LCallback in ACallbacks do
    Put(APath, LCallback);
end;

end.
