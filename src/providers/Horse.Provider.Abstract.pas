unit Horse.Provider.Abstract;

interface

uses
  System.SysUtils, Horse.Core;

type

  THorseProviderAbstract = class(THorseCore)
  public
    class procedure Listen; virtual; abstract;
    class procedure StopListen; virtual;
  end;

implementation

{ THorseProviderAbstract }

class procedure THorseProviderAbstract.StopListen;
begin
  raise Exception.Create('StopListen not implemented');
end;

end.
