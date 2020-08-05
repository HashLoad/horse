unit Horse.Provider.CGI;

interface
{$IF DEFINED(HORSE_CGI)}
uses
  Horse.Provider.Abstract;

type

  THorseProvider = class(THorseProviderAbstract)
  private
    class procedure InternalListen; static;
  public
    class procedure Listen; overload; override;
  end;
{$ENDIF}

implementation

{$IF DEFINED(HORSE_CGI)}
uses Web.WebBroker, Web.CGIApp, Horse.WebModule;

{ THorseProvider }

class procedure THorseProvider.InternalListen;
begin
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  Application.Run;
end;

class procedure THorseProvider.Listen;
begin
  inherited;
  InternalListen;
end;
{$ENDIF}

end.
