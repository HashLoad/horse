unit Horse.Provider.CGI;

interface

{$IF DEFINED(HORSE_CGI) AND NOT DEFINED(FPC) }

uses
  Horse.Provider.Abstract, System.SysUtils;

type

  THorseProvider<T: class> = class(THorseProviderAbstract<T>)
  private
    class procedure InternalListen; static;
  public
    class procedure Start; deprecated 'Use Listen instead';
    class procedure Listen; overload; override;
    class procedure Listen(ACallback: TProc<T>); reintroduce; overload; static;
  end;
{$ENDIF}

implementation

{$IF DEFINED(HORSE_CGI) AND NOT DEFINED(FPC)}

uses Web.WebBroker, Web.CGIApp, Horse.WebModule;

{ THorseProvider<T> }

class procedure THorseProvider<T>.InternalListen;
begin
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  DoOnListen;
  Application.Run;
end;

class procedure THorseProvider<T>.Listen;
begin
  inherited;
  InternalListen;
end;

class procedure THorseProvider<T>.Listen(ACallback: TProc<T>);
begin
  inherited;
  SetOnListen(ACallback);
  InternalListen;
end;

class procedure THorseProvider<T>.Start;
begin
  Listen;
end;


{$ENDIF}

end.
