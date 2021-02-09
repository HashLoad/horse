unit Horse.Provider.FPC.FastCGI;

{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

{$IF DEFINED(FPC) AND DEFINED(HORSE_FCGI)}

uses
  SysUtils, Classes, fpFCGI, httpdefs, fpHTTP,
  Horse.Provider.Abstract, Horse.Constants, Horse.Proc;

type

  { THorseProvider }

  THorseProvider<T: class> = class(THorseProviderAbstract<T>)
  private
    class var FPort: Integer;
    class var FHost: string;
    class var FRunning: Boolean;
    class var FFastCGIApplication: TFCGIApplication;
    class function GetDefaultFastCGIApplication: TFCGIApplication;
    class function FastCGIApplicationIsNil: Boolean;
    class procedure SetPort(const Value: Integer); static;
    class procedure SetHost(const Value: string); static;
    class function GetPort: Integer; static;
    class function GetDefaultPort: Integer; static;
    class function GetDefaultHost: string; static;
    class function GetHost: string; static;
    class procedure InternalListen; virtual;
    class procedure DoGetModule(Sender: TObject; ARequest: TRequest; var ModuleClass: TCustomHTTPModuleClass);
  public
    constructor Create; reintroduce; overload;
    constructor Create(APort: Integer); reintroduce; overload; deprecated 'Use Port method to set port';
    class property Host: string read GetHost write SetHost;
    class property Port: Integer read GetPort write SetPort;
    class procedure Listen; overload; override;
    class procedure Listen(APort: Integer; const AHost: string = '0.0.0.0'; ACallback: TProc<T> = nil); reintroduce; overload; static;
    class procedure Listen(APort: Integer; ACallback: TProc<T>); reintroduce; overload; static;
    class procedure Listen(AHost: string; const ACallback: TProc<T> = nil); reintroduce; overload; static;
    class procedure Listen(ACallback: TProc<T>); reintroduce; overload; static;
    class procedure Start; deprecated 'Use Listen instead';
    class destructor UnInitialize;
  end;

{$ENDIF}

implementation

{$IF DEFINED(FPC) AND DEFINED(HORSE_FCGI)}

uses
  Horse.WebModule;

{ THorseProvider<T> }

class function THorseProvider<T>.GetDefaultFastCGIApplication: TFCGIApplication;
begin
  if FastCGIApplicationIsNil then
    FFastCGIApplication := Application;
  Result := FFastCGIApplication;
end;

class function THorseProvider<T>.FastCGIApplicationIsNil: Boolean;
begin
  Result := FFastCGIApplication = nil;
end;

constructor THorseProvider<T>.Create(APort: Integer);
begin
  inherited Create;
  SetPort(APort);
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
  Result := -1;
end;

class function THorseProvider<T>.GetHost: string;
begin
  Result := FHost;
end;

class function THorseProvider<T>.GetPort: Integer;
begin
  Result := FPort;
end;

class procedure THorseProvider<T>.InternalListen;
var
  LFastCGIApplication: TFCGIApplication;
begin
  inherited;
  if FHost.IsEmpty then
    FHost := GetDefaultHost;
  LFastCGIApplication := GetDefaultFastCGIApplication;
  LFastCGIApplication.AllowDefaultModule := True;
  LFastCGIApplication.OnGetModule := DoGetModule;
  if FPort > 0 then
    LFastCGIApplication.Port := FPort;
  LFastCGIApplication.LegacyRouting := True;
  LFastCGIApplication.Address := FHost;
  LFastCGIApplication.Initialize;
  FRunning := True;
  DoOnListen;
  LFastCGIApplication.Run;
end;

class procedure THorseProvider<T>.DoGetModule(Sender: TObject; ARequest: TRequest; var ModuleClass: TCustomHTTPModuleClass);
begin
  ModuleClass := THorseWebModule;
end;

class procedure THorseProvider<T>.Start;
begin
  Listen;
end;

class procedure THorseProvider<T>.Listen;
begin
  InternalListen;;
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

class procedure THorseProvider<T>.SetPort(const Value: Integer);
begin
  FPort := Value;
end;

class destructor THorseProvider<T>.UnInitialize;
begin

end;

{$ENDIF}

end.
