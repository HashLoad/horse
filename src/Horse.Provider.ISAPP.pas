unit Horse.Provider.ISAPP;

interface

{$IF DEFINED(HORSE_ISAPP)}

uses
  Horse.Provider.Abstract, System.SysUtils;

type

  THorseProvider = class(THorseProviderAbstract)
  private
    class procedure InternalListen; static;
  public
    class procedure Start; deprecated 'Use Listen instead';
    class procedure Listen; overload; override;
    class procedure Listen(ACallback: TProc<TObject>); reintroduce; overload; static;
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

class procedure THorseProvider.Listen(ACallback: TProc<TObject>);
begin
  inherited;
  SetOnListen(ACallback);
  InternalListen;
end;

class procedure THorseProvider.Start;
begin
  Listen;
end;

{$ENDIF}

end.
