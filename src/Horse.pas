unit Horse;

interface

uses IdHTTPWebBrokerBridge, Horse.Core, IdContext, Horse.HTTP, System.SysUtils, Horse.Router, Horse.Exception;

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
    FPort: Integer;
    FMaxConnections: Integer;
    FListenQueue: Integer;
    FHTTPWebBroker: TIdHTTPWebBrokerBridge;
    class var FInstance: THorse;
    procedure OnAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String;
      var VHandled: Boolean);
  public
    constructor Create; overload;
    constructor Create(APort: Integer); overload;
    destructor Destroy; override;
    property ListenQueue: Integer read FListenQueue write FListenQueue;
    property MaxConnections: Integer read FMaxConnections write FMaxConnections;
    property Port: Integer read FPort write FPort;
    procedure Start;
    procedure Initialize;
    class function GetInstance: THorse;
    class destructor UnInitialize;
  end;

implementation

{ THorse }

uses Horse.Constants, Horse.WebModule, Web.WebReq, IdCustomTCPServer;

constructor THorse.Create(APort: Integer);
begin
  inherited Create;
  FPort := APort;
  FHTTPWebBroker := TIdHTTPWebBrokerBridge.Create(nil);
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
  Create(DEFAULT_PORT);
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

procedure THorse.Start;
var
  LAttach: string;
begin
  inherited;
  WebRequestHandler.WebModuleClass := WebModuleClass;
  try
    if FMaxConnections > 0 then
      WebRequestHandler.MaxConnections := FMaxConnections;
    FHTTPWebBroker.ListenQueue := FListenQueue;
    FHTTPWebBroker.DefaultPort := FPort;
    FHTTPWebBroker.Active := True;
    FHTTPWebBroker.StartListening;

    if IsConsole then
    begin
      Writeln(Format(START_RUNNING, [FPort]));
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
