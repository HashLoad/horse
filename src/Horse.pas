unit Horse;

interface

uses Horse.Core, IdContext, Horse.HTTP, System.SysUtils, Horse.Router, Horse.Exception;

type
  EHorseException = Horse.Exception.EHorseException;
  EHorseCallbackInterrupted = Horse.Exception.EHorseCallbackInterrupted;
  TProc = System.SysUtils.TProc;
  THorseList = Horse.HTTP.THorseList;
  THorseRequest = Horse.HTTP.THorseRequest;
  THorseHackRequest = Horse.HTTP.THorseHackRequest;
  THorseResponse = Horse.HTTP.THorseResponse;
  THorseHackResponse = Horse.HTTP.THorseHackResponse;
  THorseCallback = Horse.Router.THorseCallback;

  THorse = class(THorseCore)
  private
    FAddr: string;
    FPort: Integer;
    FMaxConnections: Integer;
    FListenQueue: Integer;
    FHTTPWebBroker: THorseHTTPWebBroker;
    class var FInstance: THorse;
    procedure OnAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String;
      var VHandled: Boolean);
  public
    constructor Create; overload;
    constructor Create(APort: Integer); overload;
    constructor Create(AAddr: string; APort: Integer); overload;
    destructor Destroy; override;
    property ListenQueue: Integer read FListenQueue write FListenQueue;
    property MaxConnections: Integer read FMaxConnections write FMaxConnections;
    property Port: Integer read FPort write FPort;
    procedure Start; override;
    procedure Stop; override;
    procedure Initialize;
    class function GetInstance: THorse;
    class destructor UnInitialize;
  end;

implementation

{ THorse }

uses Horse.Constants, Horse.WebModule, Web.WebReq, IdCustomTCPServer;

constructor THorse.Create(APort: Integer);
begin
  Create(DEFAULT_ADDR, APort);
end;

constructor THorse.Create(AAddr: string; APort: Integer);
begin
  inherited Create;
  FAddr := AAddr;
  FPort := APort;
  FHTTPWebBroker := THorseHTTPWebBroker.Create(nil);
  FHTTPWebBroker.OnParseAuthentication := OnAuthentication;
  FListenQueue := IdListenQueueDefault;
  MaxConnections := 0;
  Initialize;
end;

destructor THorse.Destroy;
begin
  if Assigned(FHTTPWebBroker) then
    FHTTPWebBroker.Free;
  inherited;
end;

constructor THorse.Create;
begin
  Create(DEFAULT_ADDR, DEFAULT_PORT);
end;

class function THorse.GetInstance: THorse;
begin
  Result := FInstance;
end;

procedure THorse.OnAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String;
  var VHandled: Boolean);
begin
  VHandled := True;
end;

procedure THorse.stop;
begin
   inherited;
   if Assigned(FHTTPWebBroker) then
   begin
      FHTTPWebBroker.StopListening;
      FHTTPWebBroker.Active := false;
   end;
end;

procedure THorse.Start;
var
  LAttach: string;
begin
  inherited;
  WebRequestHandler.WebModuleClass := WebModuleClass;
  try
    WebRequestHandler.MaxConnections := 0;
    FHTTPWebBroker.MaxConnections := FMaxConnections;
    FHTTPWebBroker.ListenQueue := FListenQueue;
    FHTTPWebBroker.Bindings.Clear;
    FHTTPWebBroker.Bindings.Add;
    FHTTPWebBroker.Bindings.Items[0].IP:= FAddr;
    FHTTPWebBroker.Bindings.Items[0].Port:= FPort;
    FHTTPWebBroker.DefaultPort := FPort;
    FHTTPWebBroker.Active := True;
    FHTTPWebBroker.StartListening;

    if IsConsole then
    begin
      Writeln(Format(START_RUNNING, [FAddr, FPort]));
      Write('Press return to stop ...');
      Read(LAttach);
    end;
  except
    on E: Exception do
    begin
      if IsConsole then
      begin
        Writeln(E.ClassName, ': ', E.Message);
        Read(LAttach);
      end
      else
        raise E;
    end;
  end;
end;

procedure THorse.Initialize;
begin
  FInstance := Self;
end;

class destructor THorse.UnInitialize;
begin
  if Assigned(FInstance) then
     FInstance.Free;
end;

end.
