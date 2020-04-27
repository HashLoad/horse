unit Horse;

interface

uses IdHTTPWebBrokerBridge, Horse.Core, IdContext, Horse.HTTP, System.SysUtils, Horse.Router;

type
  EHorseCallbackInterrupted = Horse.HTTP.EHorseCallbackInterrupted;
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
    procedure OnAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String;
      var VHandled: Boolean);
  protected
    procedure Initialize; override;
  public
    constructor Create; overload;
    constructor Create(APort: Integer); overload;
    destructor Destroy; override;
    property ListenQueue: Integer read FListenQueue write FListenQueue;
    property MaxConnections: Integer read FMaxConnections write FMaxConnections;
    property Port: Integer read FPort write FPort;
    procedure Start; override;
  end;

implementation

{ THorse }

uses Horse.Constants, Horse.WebModule, Web.WebReq, IdCustomTCPServer;

constructor THorse.Create(APort: Integer);
begin
  inherited Create;
  FPort := APort;
end;

destructor THorse.Destroy;
begin
  if Assigned(FHTTPWebBroker) then
    FHTTPWebBroker.Free;
  inherited;
end;

procedure THorse.Initialize;
begin
  inherited;
  FHTTPWebBroker := TIdHTTPWebBrokerBridge.Create(nil);
  FHTTPWebBroker.OnParseAuthentication := OnAuthentication;
  FListenQueue := IdListenQueueDefault;
  MaxConnections := 0;
end;

constructor THorse.Create;
begin
  inherited Create;
  FPort := DEFAULT_PORT;
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
      FHTTPWebBroker.MaxConnections := FMaxConnections;
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

end.
