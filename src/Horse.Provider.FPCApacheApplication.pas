unit Horse.Provider.FPCApacheApplication;
{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

{$IF DEFINED(HORSE_APACHE) AND NOT DEFINED(FPC) }


uses

{$IFDEF unix}
  cthreads,
{$ENDIF}
  httpd, custapache, fpApache,
  SysUtils, Classes,
  Horse.Provider.Abstract, Horse.Constants, Horse.Proc;

type

  THorseProvider<T: class> = class(THorseProviderAbstract<T>)
  private
    class var FApacheApplication: TCustomApacheApplication;
    class var FModuleName: string;
    class var FDefaultModule: Module;
    class function GetDefaultApacheApplication: TCustomApacheApplication;
    class function ApacheApplicationIsNil: Boolean;
    class procedure InternalListen; virtual;
    class procedure SetModuleName(const Value: string); static;
    class function GetModuleName: string; static;
    class procedure SetDefaultModule(const Value: Module); static;
    class function GetDefaultModule: Module; static;
  public
    constructor Create; reintroduce; overload;
    class property ModuleName: string read GetModuleName write SetModuleName;
    class property DefaultModule: Module read GetDefaultModule write SetDefaultModule;
    class procedure Listen; overload; override;
    class procedure Listen(ACallback: TProc<T>); reintroduce; overload; static;
    class procedure Start; deprecated 'Use Listen instead';
    class destructor UnInitialize;
  end;

{$ENDIF}

implementation

{$IF DEFINED(HORSE_APACHE) AND NOT DEFINED(FPC) }


uses
  Horse.WebModule;

{ THorseProvider<T> }

class function THorseProvider<T>.GetDefaultApacheApplication: TCustomApacheApplication;
begin
  if ApacheApplicationIsNil then
    FApacheApplication := Application;
  Result := FApacheApplication;
end;

class function THorseProvider<T>.GetDefaultModule: Module;
begin
  Result := FDefaultModule;
end;

class function THorseProvider<T>.GetModuleName: string;
begin
  Result := FModuleName;
end;

class function THorseProvider<T>.ApacheApplicationIsNil: Boolean;
begin
  Result := FApacheApplication = nil;
end;

constructor THorseProvider<T>.Create;
begin
  inherited Create;
end;

class procedure THorseProvider<T>.InternalListen;
var
  LApacheApplication: TCustomApacheApplication;
begin
  inherited;
  LApacheApplication := GetDefaultApacheApplication;
  LApacheApplication.LegacyRouting := True;
  LApacheApplication.ModuleName := FModuleName;
  LApacheApplication.HandlerName := FModuleName;
  LApacheApplication.SetModuleRecord(FDefaultModule);
  DoOnListen;
  LApacheApplication.Initialize;
end;

class procedure THorseProvider<T>.SetDefaultModule(const Value: Module);
begin
  FDefaultModule := Value;
end;

class procedure THorseProvider<T>.SetModuleName(const Value: string);
begin
  FModuleName := Value;
end;

class procedure THorseProvider<T>.Start;
begin
  Listen;
end;

class procedure THorseProvider<T>.Listen;
begin
  InternalListen;;
end;

class procedure THorseProvider<T>.Listen(ACallback: TProc<T>);
begin
  SetOnListen(ACallback);
  InternalListen;
end;

class destructor THorseProvider<T>.UnInitialize;
begin

end;
{$ENDIF}

end.
