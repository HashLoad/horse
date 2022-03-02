unit Horse.Provider.FPC.CGI;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

{$IF DEFINED(HORSE_CGI) AND DEFINED(FPC)}
uses SysUtils, Classes, fpCGI, fphttp, httpdefs, Horse.Provider.Abstract, Horse.Proc;

type
  THorseProvider<T: class> = class(THorseProviderAbstract<T>)
  private
    class var FCGIApplication: TCGIApplication;
    class function GetDefaultCGIApplication: TCGIApplication;
    class function CGIApplicationIsNil: Boolean;
    class procedure InternalListen; virtual;
    class procedure DoGetModule(Sender: TObject; ARequest: TRequest; var ModuleClass: TCustomHTTPModuleClass);
  public
    constructor Create; reintroduce; overload;
    class procedure Listen; overload; override;
    class procedure Listen(const ACallback: TProc<T>); reintroduce; overload; static;
  end;

var
  ShowCleanUpErrors: Boolean = False;
{$ENDIF}

implementation

{$IF DEFINED(HORSE_CGI) AND DEFINED(FPC)}
uses Horse.WebModule;

class function THorseProvider<T>.GetDefaultCGIApplication: TCGIApplication;
begin
  if CGIApplicationIsNil then
    FCGIApplication := Application;
  Result := FCGIApplication;
end;

class function THorseProvider<T>.CGIApplicationIsNil: Boolean;
begin
  Result := FCGIApplication = nil;
end;

constructor THorseProvider<T>.Create;
begin
  inherited Create;
end;

class procedure THorseProvider<T>.InternalListen;
var
  LCGIApplication: TCGIApplication;
begin
  inherited;
  LCGIApplication := GetDefaultCGIApplication;
  LCGIApplication.AllowDefaultModule := True;
  LCGIApplication.OnGetModule := DoGetModule;
  LCGIApplication.LegacyRouting := True;
  LCGIApplication.Initialize;
  DoOnListen;
  LCGIApplication.Run;
end;

class procedure THorseProvider<T>.DoGetModule(Sender: TObject; ARequest: TRequest; var ModuleClass: TCustomHTTPModuleClass);
begin
  ModuleClass := THorseWebModule;
end;

class procedure THorseProvider<T>.Listen;
begin
  InternalListen;;
end;

class procedure THorseProvider<T>.Listen(const ACallback: TProc<T>);
begin
  SetOnListen(ACallback);
  InternalListen;
end;
{$ENDIF}

end.
