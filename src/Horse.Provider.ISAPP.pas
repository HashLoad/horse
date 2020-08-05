unit Horse.Provider.ISAPP;

interface
{$IF DEFINED(HORSE_ISAPP)}
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

{$IF DEFINED(HORSE_ISAPP)}
uses
  Web.WebBroker, System.Win.ComObj, Winapi.ActiveX, Horse.WebModule;

{ THorseProvider }

class procedure THorseProvider.InternalListen;
begin
  CoInitFlags := COINIT_MULTITHREADED;
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
