program RouterLifetimeCheck;

{$MODE DELPHI}{$H+}

uses
  Horse,
  Horse.Core,
  Horse.Core.RouterTree,
  Horse.Core.Router.Contract,
  Horse.Instance,
  Horse.Callback,
  Horse.Request,
  Horse.Response;

procedure Ping(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('pong');
end;

var
  LInstance: THorseInstance;
  LHeldRouter: IHorseRouter;
begin
  THorse.Get('/api/users/:id', THorseCallbackRequestResponse(@Ping));
  THorse.UseRadixRouter;
  THorse.Post('/api/items', THorseCallbackRequestResponse(@Ping));
  THorse.Routes := THorseRouterTree.Create;
  THorse.Get('/api/health', THorseCallbackRequestResponse(@Ping));

  LInstance := THorseInstance.Create;
  try
    LInstance.Get('/instance/ping', THorseCallbackRequestResponse(@Ping));
    LHeldRouter := LInstance.GetRoutes;
  finally
    LInstance.Free;
  end;
  if LHeldRouter.GetPrefix <> '' then
    Halt(1);
  LHeldRouter := nil;
end.
