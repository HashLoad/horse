unit Horse.Provider.Abstract;

interface

uses
  System.SysUtils, Horse.Core;

type

  THorseProviderAbstract = class(THorseCore)
  private
    class var FOnListen: TProc<TObject>;
  protected
    class procedure SetOnListen(const Value: TProc<TObject>); static;
    class function GetOnListen: TProc<TObject>; static;
    class procedure DoOnListen;
  public
    class procedure Listen; virtual; abstract;
    class procedure StopListen; virtual;
    class property OnListen: TProc<TObject> read GetOnListen write SetOnListen;
  end;

implementation

uses
  Horse.Constants;

{ THorseProviderAbstract }

class procedure THorseProviderAbstract.DoOnListen;
begin
  if Assigned(FOnListen) then
    FOnListen(GetInstance);
end;

class function THorseProviderAbstract.GetOnListen: TProc<TObject>;
begin
  Result := FOnListen;
end;

class procedure THorseProviderAbstract.SetOnListen(const Value: TProc<TObject>);
begin
  FOnListen := Value;
end;

class procedure THorseProviderAbstract.StopListen;
begin
  raise Exception.Create('StopListen not implemented');
end;

end.
