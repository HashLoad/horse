unit Horse.Core.Factory;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  Horse.Core.Group.Contract,
  Horse.Core.Route.Contract,
  Horse.Core;

function CreateHorseCoreGroup: IHorseCoreGroup<THorseCore>;
function CreateHorseCoreRoute(const APath: string): IHorseCoreRoute<THorseCore>;

implementation

uses
  Horse.Core.Group,
  Horse.Core.Route;

function CreateHorseCoreGroup: IHorseCoreGroup<THorseCore>;
begin
  Result := THorseCoreGroup<THorseCore>.Create;
end;

function CreateHorseCoreRoute(const APath: string): IHorseCoreRoute<THorseCore>;
begin
  Result := THorseCoreRoute<THorseCore>.Create(APath);
end;

end.
