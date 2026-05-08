unit Horse.Provider.FPC.Daemon;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

{$IF DEFINED(HORSE_DAEMON) AND DEFINED(FPC)}

uses
  SysUtils,
  Classes,
  httpdefs,
  fpHTTP,
  fphttpserver,
  Horse.Request,
  Horse.Response,
  Horse.Core,
  Horse.Provider.Abstract,
  Horse.Constants,
  Horse.Proc,
  Horse.Commons;

type
  THTTPServerThread = class(TThread)
  private
    FHost: string;
    FPort: Integer;
    FListenQueue: Word;
    FServer: TFPHTTPServer;
    FHorse: THorseCore;
    procedure OnRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
  protected
    procedure Execute; override;
  public
    constructor Create;
    property Server: TFPHTTPServer read FServer;
    property Port: Integer read FPort write FPort;
    property Host: string read FHost write FHost;
    property ListenQueue: Word read FListenQueue write FListenQueue;
  end;

  THTTPServerShutdownThread = class(TThread)
  private
    FServerThread: THTTPServerThread;
  protected
    procedure Execute; override;
  public
    constructor Create(AServerThread: THTTPServerThread);
  end;

  THorseProvider = class(THorseProviderAbstract)
  private
    class var FPort: Integer;
    class var FHost: string;
    class var FRunning: Boolean;
    class var FListenQueue: Integer;
    class var FHTTPServerThread: THTTPServerThread;
    class function HTTPServerThreadIsNil: Boolean;
    class procedure SetListenQueue(const AValue: Integer); static;
    class procedure SetPort(const AValue: Integer); static;
    class procedure SetHost(const AValue: string); static;
    class function GetListenQueue: Integer; static;
    class function GetPort: Integer; static;
    class function GetDefaultPort: Integer; static;
    class function GetDefaultHost: string; static;
    class function GetHost: string; static;
    class procedure InternalListen; virtual;
    class procedure InternalStopListen; virtual;
  public
    class property Host: string read GetHost write SetHost;
    class property Port: Integer read GetPort write SetPort;
    class property ListenQueue: Integer read GetListenQueue write SetListenQueue;
    class procedure StopListen; override;
    class procedure Listen; overload; override;
    class procedure Listen(const APort: Integer; const AHost: string = '0.0.0.0';
      const ACallbackListen: TProc = nil; const ACallbackStopListen: TProc = nil); reintroduce; overload; static;
    class procedure Listen(const APort: Integer; const ACallbackListen: TProc;
      const ACallbackStopListen: TProc = nil); reintroduce; overload; static;
    class procedure Listen(const AHost: string; const ACallbackListen: TProc = nil;
      const ACallbackStopListen: TProc = nil); reintroduce; overload; static;
    class procedure Listen(const ACallbackListen: TProc;
      const ACallbackStopListen: TProc = nil); reintroduce; overload; static;
    class destructor UnInitialize;
    class function IsRunning: Boolean;
  end;

{$ENDIF}

implementation

{$IF DEFINED(HORSE_DAEMON) AND DEFINED(FPC)}

uses
  Horse.WebModule, Horse.Exception.Interrupted;

{ THTTPServerThread }

constructor THTTPServerThread.Create;
begin
  inherited Create(True);

  FreeOnTerminate := False;

  FHorse := THorseCore.GetInstance;
end;

procedure THTTPServerThread.Execute;
begin
  FServer := TFPHTTPServer.Create(nil);

  try
    FServer.HostName := FHost;
    FServer.Port := FPort;
    FServer.Threaded := True;
    FServer.QueueSize := FListenQueue;
    FServer.OnRequest := OnRequest;

    FServer.Active := True;

    while not Terminated do
      Sleep(100);

  finally
    FreeAndNil(FServer);
  end;
end;

procedure THTTPServerThread.OnRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  LRequest: THorseRequest;
  LResponse: THorseResponse;
begin
  LRequest := THorseRequest.Create(ARequest);
  LResponse := THorseResponse.Create(AResponse);

  try
    try
      if not FHorse.Routes.Execute(LRequest, LResponse) then
      begin
        AResponse.Content := 'Not Found';
        AResponse.Code := THTTPStatus.NotFound.ToInteger;
      end;
    except
      on E: Exception do
        if not E.InheritsFrom(EHorseCallbackInterrupted) then
          raise;
    end;
  finally
    if LRequest.Body<TObject> = LResponse.Content then
      LResponse.Content(nil);

    LRequest.Free;
    LResponse.Free;
  end;
end;

{ THTTPServerShutdownThread }

constructor THTTPServerShutdownThread.Create(AServerThread: THTTPServerThread);
begin
  inherited Create(False);

  FreeOnTerminate := True;

  FServerThread := AServerThread;
end;

procedure THTTPServerShutdownThread.Execute;
begin
  if Assigned(FServerThread) then
  begin
    FServerThread.Terminate;

    if Assigned(FServerThread.Server) then
    begin
      try
        FServerThread.Server.Active := False;
      except
      end;
    end;

    FServerThread.WaitFor;
    FreeAndNil(FServerThread);
  end;
end;

{ THorseProvider }

class function THorseProvider.HTTPServerThreadIsNil: Boolean;
begin
  Result := FHTTPServerThread = nil;
end;

class function THorseProvider.IsRunning: Boolean;
begin
  Result := FRunning;
end;

class procedure THorseProvider.StopListen;
begin
  InternalStopListen;
end;

class function THorseProvider.GetDefaultHost: string;
begin
  Result := DEFAULT_HOST;
end;

class function THorseProvider.GetDefaultPort: Integer;
begin
  Result := DEFAULT_PORT;
end;

class function THorseProvider.GetHost: string;
begin
  Result := FHost;
end;

class function THorseProvider.GetListenQueue: Integer;
begin
  Result := FListenQueue;
end;

class function THorseProvider.GetPort: Integer;
begin
  Result := FPort;
end;

class procedure THorseProvider.InternalListen;
begin
  inherited;
  if FRunning then
    Exit;

  if FPort <= 0 then
    FPort := GetDefaultPort;

  if FHost.IsEmpty then
    FHost := GetDefaultHost;

  if FListenQueue = 0 then
    FListenQueue := 15;

  FHTTPServerThread := THTTPServerThread.Create;
  FHTTPServerThread.Port := FPort;
  FHTTPServerThread.Host := FHost;
  FHTTPServerThread.ListenQueue := FListenQueue;

  FHTTPServerThread.Start;

  FRunning := True;

  DoOnListen;
end;

class procedure THorseProvider.InternalStopListen;
begin
  if HTTPServerThreadIsNil then
    Exit;
  FRunning := False;
  DoOnStopListen;
  THTTPServerShutdownThread.Create(FHTTPServerThread);
  FHTTPServerThread := nil;
end;

class procedure THorseProvider.Listen;
begin
  InternalListen;
end;

class procedure THorseProvider.Listen(const APort: Integer; const AHost: string;
  const ACallbackListen, ACallbackStopListen: TProc);
begin
  SetPort(APort);
  SetHost(AHost);

  SetOnListen(ACallbackListen);
  SetOnStopListen(ACallbackStopListen);

  InternalListen;
end;

class procedure THorseProvider.Listen(const AHost: string; const ACallbackListen,
  ACallbackStopListen: TProc);
begin
  Listen(FPort, AHost, ACallbackListen, ACallbackStopListen);
end;

class procedure THorseProvider.Listen(const ACallbackListen, ACallbackStopListen: TProc);
begin
  Listen(FPort, FHost, ACallbackListen, ACallbackStopListen);
end;

class procedure THorseProvider.Listen(const APort: Integer; const ACallbackListen,
  ACallbackStopListen: TProc);
begin
  Listen(APort, FHost, ACallbackListen, ACallbackStopListen);
end;

class procedure THorseProvider.SetHost(const AValue: string);
begin
  FHost := AValue;
end;

class procedure THorseProvider.SetListenQueue(const AValue: Integer);
begin
  FListenQueue := AValue;
end;

class procedure THorseProvider.SetPort(const AValue: Integer);
begin
  FPort := AValue;
end;

class destructor THorseProvider.UnInitialize;
begin

end;

{$ENDIF}

end.
