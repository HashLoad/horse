unit Horse.Provider.IOUring;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  Horse.Provider.Epoll;

type
  THorseProviderIOUring = class(THorseProviderEpoll)
  end;

implementation

end.
