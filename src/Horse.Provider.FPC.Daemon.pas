unit Horse.Provider.FPC.Daemon;

{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

{$IF DEFINED(HORSE_DAEMON)}

uses
  SysUtils, Classes, httpdefs, fpHTTP, fphttpserver, Horse.HTTP, HOrse.Core,
  Horse.Provider.Abstract, Horse.Constants, Horse.Proc, Horse.Commons,
  Horse.Exception;

type

  { THTTPServerThread }

  THTTPServerThread = class(TThread)
  private
    FStarServer : Boolean;
    FHost: String;
    FPort: Integer;
    FListenQueue: Word;
    FServer: TFPHTTPServer;
    FHorse : THorseCore;

  public
    constructor Create(CreateSuspended: Boolean;
                       const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    procedure StartServer;
    procedure StopServer;
    property Port: Integer read FPort write FPort;
    property Host: String read FHost write FHost;
    property ListenQueue: Word read FListenQueue write FListenQueue;
    procedure Execute; override;
    procedure OnRequest(Sender: TObject;
                        var ARequest: TFPHTTPConnectionRequest;
                        var AResponse : TFPHTTPConnectionResponse);
  end;

  { THorseProvider }

  THorseProvider<T: class> = class(THorseProviderAbstract<T>)
  private
    class var FPort: Integer;
    class var FHost: string;
    class var FRunning : Boolean;
    class var FListenQueue: Integer;
    class var FHTTPServerThread: THTTPServerThread;
    class function GetDefaultHTTPServerThread: THTTPServerThread;
    class function HTTPServerThreadIsNil: Boolean;
    class procedure SetListenQueue(const Value: Integer); static;
    class procedure SetPort(const Value: Integer); static;
    class procedure SetHost(const Value: string); static;
    class function GetListenQueue: Integer; static;
    class function GetPort: Integer; static;
    class function GetDefaultPort: Integer; static;
    class function GetDefaultHost: string; static;
    class function GetHost: string; static;
    class procedure InternalListen; virtual;
    class procedure InternalStopListen; virtual;
  public
    constructor Create; reintroduce; overload;
    constructor Create(APort: Integer); reintroduce; overload; deprecated 'Use Port method to set port';
    class property Host: string read GetHost write SetHost;
    class property Port: Integer read GetPort write SetPort;
    class property ListenQueue: Integer read GetListenQueue write SetListenQueue;
    class procedure StopListen; override;
    class procedure Listen; overload; override;
    class procedure Listen(APort: Integer; const AHost: string = '0.0.0.0'; ACallback: TProc<T> = nil); reintroduce; overload; static;
    class procedure Listen(APort: Integer; ACallback: TProc<T>); reintroduce; overload; static;
    class procedure Listen(AHost: string; const ACallback: TProc<T> = nil); reintroduce; overload; static;
    class procedure Listen(ACallback: TProc<T>); reintroduce; overload; static;
    class procedure Start; deprecated 'Use Listen instead';
    class destructor UnInitialize;
    class function IsRunning: Boolean;
  end;

{$ENDIF}

implementation

{$IF DEFINED(HORSE_DAEMON) AND DEFINED(FPC)}

uses
  Horse.WebModule;

{ THorseProvider<T> }

class function THorseProvider<T>.GetDefaultHTTPServerThread: THTTPServerThread;
begin
  if HTTPServerThreadIsNil then
    FHTTPServerThread := THTTPServerThread.Create(True);
  Result := FHTTPServerThread;
end;

class function THorseProvider<T>.IsRunning: Boolean;
begin
  Result := FRunning;
end;

class function THorseProvider<T>.HTTPServerThreadIsNil: Boolean;
begin
  Result := FHTTPServerThread = nil;
end;

constructor THorseProvider<T>.Create(APort: Integer);
begin
  inherited Create;
  SetPort(APort);
end;

class procedure THorseProvider<T>.StopListen;
begin
  InternalStopListen;
end;

constructor THorseProvider<T>.Create;
begin
  inherited Create;
end;

class function THorseProvider<T>.GetDefaultHost: string;
begin
  Result := DEFAULT_HOST;
end;

class function THorseProvider<T>.GetDefaultPort: Integer;
begin
  Result := DEFAULT_PORT;
end;

class function THorseProvider<T>.GetHost: string;
begin
  Result := FHost;
end;

class function THorseProvider<T>.GetListenQueue: Integer;
begin
  Result := FListenQueue;
end;

class function THorseProvider<T>.GetPort: Integer;
begin
  Result := FPort;
end;

class procedure THorseProvider<T>.InternalListen;
var
  LHTTPServerThread: THTTPServerThread;
begin
  inherited;
  if FPort <= 0 then
    FPort := GetDefaultPort;
  if FHost.IsEmpty then
    FHost := GetDefaultHost;
  if FListenQueue = 0 then
    FListenQueue := 15;
  LHTTPServerThread := GetDefaultHTTPServerThread;
  LHTTPServerThread.Port := FPort;
  LHTTPServerThread.Host := FHost;
  LHTTPServerThread.ListenQueue := FListenQueue;
  LHTTPServerThread.StartServer;
  FRunning := True;
  DoOnListen;
end;

class procedure THorseProvider<T>.Start;
begin
  Listen;
end;

class procedure THorseProvider<T>.Listen;
begin
  InternalListen;
end;

class procedure THorseProvider<T>.Listen(APort: Integer; const AHost: string; ACallback: TProc<T>);
begin
  SetPort(APort);
  SetHost(AHost);
  SetOnListen(ACallback);
  InternalListen;
end;

class procedure THorseProvider<T>.Listen(AHost: string; const ACallback: TProc<T>);
begin
  Listen(FPort, AHost, ACallback);
end;

class procedure THorseProvider<T>.Listen(ACallback: TProc<T>);
begin
  Listen(FPort, FHost, ACallback);
end;

class procedure THorseProvider<T>.Listen(APort: Integer; ACallback: TProc<T>);
begin
  Listen(APort, FHost, ACallback);
end;

class procedure THorseProvider<T>.SetHost(const Value: string);
begin
  FHost := Value;
end;

class procedure THorseProvider<T>.SetListenQueue(const Value: Integer);
begin
  FListenQueue := Value;
end;

class procedure THorseProvider<T>.SetPort(const Value: Integer);
begin
  FPort := Value;
end;

class destructor THorseProvider<T>.UnInitialize;
begin
  FreeAndNil(FHTTPServerThread);
end;

class procedure THorseProvider<T>.InternalStopListen;
begin
  if not HTTPServerThreadIsNil then
  begin
    GetDefaultHTTPServerThread.StopServer;
    FRunning := False;
    DoOnListen;
  end
  else
    raise Exception.Create('Horse not listen');
end;

{ THTTPServerThread }

procedure THTTPServerThread.OnRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  LRequest: THorseHackRequest;
  LResponse: THorseHackResponse;
begin
  LRequest := THorseHackRequest.Create(ARequest);
  LResponse := THorseHackResponse.Create(AResponse);
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
    if LRequest.Body<TObject> = THorseHackResponse(LResponse).GetContent then
      THorseHackResponse(LResponse).SetContent(nil);
    LRequest.Free;
    LResponse.Free;
  end;
end;

constructor THTTPServerThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt = DefaultStackSize);
begin
  inherited Create(CreateSuspended, StackSize);
  FreeOnTerminate := True;
  FStarServer := False;
  FServer := TFPHttpServer.Create(Nil);
  FServer.OnRequest := OnRequest;
  //
  FHorse := THorseCore.GetInstance;
end;

destructor THTTPServerThread.Destroy;
begin
  if Assigned(FServer) then
    FServer.Active := False;
  FreeAndNil(FServer);
  inherited Destroy;
end;

procedure THTTPServerThread.StartServer;
begin
  Start;
  FStarServer := True;
end;

procedure THTTPServerThread.StopServer;
begin
  FStarServer := False;
  FServer.Active := FStarServer;
end;

procedure THTTPServerThread.Execute;
begin
  while not Terminated do
  begin
    if FStarServer then
    begin
      FServer.HostName := FHost;
      FServer.Port := FPort;
      FServer.Threaded:= True;
      FServer.QueueSize := FListenQueue;
      FServer.Active := True;
    end;
  end;
end;

{$ENDIF}

end.
