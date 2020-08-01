unit Horse.Provider.Console;

interface

{$IF DEFINED(HORSE_CONSOLE)}


uses
  Horse.Provider.Abstract, Horse.Constants, IdHTTPWebBrokerBridge, IdContext;

type

  THorseProvider = class(THorseProviderAbstract)
  private
    class var FPort: Integer;
    class var FMaxConnections: Integer;
    class var FListenQueue: Integer;
    class var FIdHTTPWebBrokerBridge: TIdHTTPWebBrokerBridge;
    class function GetDefaultHTTPWebBroker: TIdHTTPWebBrokerBridge;
    class function HTTPWebBrokerIsNil: Boolean;
    class procedure OnAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
    class procedure SetListenQueue(const Value: Integer); static;
    class procedure SetMaxConnections(const Value: Integer); static;
    class procedure SetPort(const Value: Integer); static;
    class function GetListenQueue: Integer; static;
    class function GetMaxConnections: Integer; static;
    class function GetPort: Integer; static;
    class function GetDefaultPort: Integer; static;
    class procedure InternalListen; static;
    class procedure InternalStopListen; static;
  public
    class property Port: Integer read GetPort write SetPort;
    class property MaxConnections: Integer read GetMaxConnections write SetMaxConnections;
    class property ListenQueue: Integer read GetListenQueue write SetListenQueue;
    class procedure Listen; overload; override;
    class procedure StopListen; override;
    class procedure Listen(APort: Integer); reintroduce; overload; static;
    class destructor UnInitialize;
  end;
{$ENDIF}

implementation

{$IF DEFINED(HORSE_CONSOLE)}


uses
  System.SysUtils, Web.WebReq, Horse.WebModule,
  IdCustomTCPServer;

{ THorseProvider }

class function THorseProvider.GetDefaultHTTPWebBroker: TIdHTTPWebBrokerBridge;
begin
  if HTTPWebBrokerIsNil then
  begin
    FIdHTTPWebBrokerBridge := TIdHTTPWebBrokerBridge.Create(nil);
    FIdHTTPWebBrokerBridge.OnParseAuthentication := OnAuthentication;
  end;
  Result := FIdHTTPWebBrokerBridge;
end;

class function THorseProvider.HTTPWebBrokerIsNil: Boolean;
begin
  Result := FIdHTTPWebBrokerBridge = nil;
end;

class function THorseProvider.GetDefaultPort: Integer;
begin
  Result := DEFAULT_PORT;
end;

class function THorseProvider.GetListenQueue: Integer;
begin
  Result := FListenQueue;
end;

class function THorseProvider.GetMaxConnections: Integer;
begin
  Result := FMaxConnections;
end;

class function THorseProvider.GetPort: Integer;
begin
  Result := FPort;
end;

class procedure THorseProvider.InternalListen;
var
  LAttach: string;
begin
  inherited;
  if FPort <= 0 then
    FPort := GetDefaultPort;
  WebRequestHandler.WebModuleClass := WebModuleClass;
  try
    if FMaxConnections > 0 then
    begin
      WebRequestHandler.MaxConnections := FMaxConnections;
      GetDefaultHTTPWebBroker.MaxConnections := FMaxConnections;
    end;
    if FListenQueue = 0 then
      FListenQueue := IdListenQueueDefault;
    GetDefaultHTTPWebBroker.ListenQueue := FListenQueue;
    GetDefaultHTTPWebBroker.DefaultPort := FPort;
    GetDefaultHTTPWebBroker.Active := True;
    GetDefaultHTTPWebBroker.StartListening;

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

class procedure THorseProvider.InternalStopListen;
begin
  if not HTTPWebBrokerIsNil then
    GetDefaultHTTPWebBroker.StopListening
  else
    raise Exception.Create('Horse not listen');
end;

class procedure THorseProvider.StopListen;
begin
  InternalStopListen;
end;

class procedure THorseProvider.Listen;
begin
  InternalListen;;
end;

class procedure THorseProvider.Listen(APort: Integer);
begin
  SetPort(APort);
  InternalListen;;
end;

class procedure THorseProvider.OnAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
begin
  VHandled := True;
end;

class procedure THorseProvider.SetListenQueue(const Value: Integer);
begin
  FListenQueue := Value;
end;

class procedure THorseProvider.SetMaxConnections(const Value: Integer);
begin
  FMaxConnections := Value;
end;

class procedure THorseProvider.SetPort(const Value: Integer);
begin
  FPort := Value;
end;

class destructor THorseProvider.UnInitialize;
begin
  FreeAndNil(FIdHTTPWebBrokerBridge);
end;
{$ENDIF}

end.
