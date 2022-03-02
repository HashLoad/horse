unit Horse.Provider.Abstract;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils, Horse.Proc,
{$ELSE}
  System.SysUtils,
{$ENDIF}
  Horse.Core;

type
  THorseProviderAbstract<T: class{$IF DEFINED(FPC)}, constructor{$ENDIF}> = class(THorseCore)
  private
    class var FOnListen: TProc<T>;
    class var FOnStopListen: TProc<T>;
    class function GetOnStopListen: TProc<T>; static;
  protected
    class function GetOnListen: TProc<T>; static;
    class procedure SetOnListen(const AValue: TProc<T>); static;
    class procedure SetOnStopListen(const AValue: TProc<T>); static;
    class procedure DoOnListen;
    class procedure DoOnStopListen;
  public
    class property OnListen: TProc<T> read GetOnListen write SetOnListen;
    class property OnStopListen: TProc<T> read GetOnStopListen write SetOnStopListen;
    class procedure Listen; virtual; abstract;
    class procedure StopListen; virtual;
  end;

implementation

class procedure THorseProviderAbstract<T>.DoOnListen;
begin
  if Assigned(FOnListen) then
    FOnListen({$IF DEFINED(FPC)}T(GetInstance){$ELSE}GetInstance{$ENDIF});
end;

class procedure THorseProviderAbstract<T>.DoOnStopListen;
begin
  if Assigned(FOnStopListen) then
    FOnStopListen({$IF DEFINED(FPC)}T(GetInstance){$ELSE}GetInstance{$ENDIF});
end;

class function THorseProviderAbstract<T>.GetOnListen: TProc<T>;
begin
  Result := FOnListen;
end;

class function THorseProviderAbstract<T>.GetOnStopListen: TProc<T>;
begin
  Result := FOnStopListen;
end;

class procedure THorseProviderAbstract<T>.SetOnListen(const AValue: TProc<T>);
begin
  FOnListen := AValue;
end;

class procedure THorseProviderAbstract<T>.SetOnStopListen(const AValue: TProc<T>);
begin
  FOnStopListen := AValue;
end;

class procedure THorseProviderAbstract<T>.StopListen;
begin
  raise Exception.Create('StopListen not implemented');
end;

end.
