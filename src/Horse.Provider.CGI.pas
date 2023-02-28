unit Horse.Provider.CGI;

interface

{$IF DEFINED(HORSE_CGI) AND NOT DEFINED(FPC)}
uses
  Horse.Provider.Abstract,
  System.SysUtils;

type
  THorseProvider = class(THorseProviderAbstract)
  private
    class procedure InternalListen; static;
  public
    class procedure Listen; overload; override;
    class procedure Listen(const ACallback: TProc); reintroduce; overload; static;
  end;
{$ENDIF}

implementation

{$IF DEFINED(HORSE_CGI) AND NOT DEFINED(FPC)}
uses
  Web.WebBroker,
  Web.CGIApp,
  Horse.WebModule;

class procedure THorseProvider.InternalListen;
begin
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  DoOnListen;
  Application.Run;
end;

class procedure THorseProvider.Listen;
begin
  inherited;
  InternalListen;
end;

class procedure THorseProvider.Listen(const ACallback: TProc);
begin
  inherited;
  SetOnListen(ACallback);
  InternalListen;
end;
{$ENDIF}

end.
