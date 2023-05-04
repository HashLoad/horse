unit Horse.Provider.Abstract;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils,
  Horse.Proc,
{$ELSE}
  System.SysUtils,
{$ENDIF}
  Horse.Core;

type
  THorseProviderAbstract = class(THorseCore)
  private
    class var FOnListen: TProc;
    class var FOnStopListen: TProc;
    class function GetOnStopListen: TProc; static;
  protected
    class function GetOnListen: TProc; static;
    class procedure SetOnListen(const AValue: TProc); static;
    class procedure SetOnStopListen(const AValue: TProc); static;
    class procedure DoOnListen;
    class procedure DoOnStopListen;
  public
    class property OnListen: TProc read GetOnListen write SetOnListen;
    class property OnStopListen: TProc read GetOnStopListen write SetOnStopListen;
    class procedure Listen; virtual; abstract;
    class procedure StopListen; virtual;
  end;

implementation

class procedure THorseProviderAbstract.DoOnListen;
begin
  if Assigned(FOnListen) then
    FOnListen();
end;

class procedure THorseProviderAbstract.DoOnStopListen;
begin
  if Assigned(FOnStopListen) then
    FOnStopListen();
end;

class function THorseProviderAbstract.GetOnListen: TProc;
begin
  Result := FOnListen;
end;

class function THorseProviderAbstract.GetOnStopListen: TProc;
begin
  Result := FOnStopListen;
end;

class procedure THorseProviderAbstract.SetOnListen(const AValue: TProc);
begin
  FOnListen := AValue;
end;

class procedure THorseProviderAbstract.SetOnStopListen(const AValue: TProc);
begin
  FOnStopListen := AValue;
end;

class procedure THorseProviderAbstract.StopListen;
begin
  raise Exception.Create('StopListen not implemented');
end;

end.
