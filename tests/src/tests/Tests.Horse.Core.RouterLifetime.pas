unit Tests.Horse.Core.RouterLifetime;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  DUnitX.TestFramework,
  Horse.Core,
  Horse.Core.RouterTree,
  Horse.Core.Router.Radix,
  Horse.Core.Router.Contract,
  Horse.Instance,
  Horse.Commons;

type
  TTrackedRouterTree = class(THorseRouterTree)
  public
    destructor Destroy; override;
  end;

  TTrackedRadixRouter = class(THorseRadixRouter)
  public
    destructor Destroy; override;
  end;

  [TestFixture]
  TTestRouterLifetime = class
  public
    [Test]
    procedure TreeFreedWithLastInterface;

    [Test]
    procedure RadixFreedWithLastInterface;

    [Test]
    procedure ReplacingGlobalRouterFreesPrevious;

    [Test]
    procedure InstanceRouterSurvivesWhileInterfaceIsHeld;
  end;

implementation

var
  GTreeDestroyCount: Integer;
  GRadixDestroyCount: Integer;

destructor TTrackedRouterTree.Destroy;
begin
  Inc(GTreeDestroyCount);
  inherited;
end;

destructor TTrackedRadixRouter.Destroy;
begin
  Inc(GRadixDestroyCount);
  inherited;
end;

procedure TTestRouterLifetime.TreeFreedWithLastInterface;
var
  LRouter: IHorseRouter;
begin
  GTreeDestroyCount := 0;
  LRouter := TTrackedRouterTree.Create;
  LRouter.RegisterRoute(mtGet, '/api/users/:id', nil);
  LRouter := nil;
  Assert.AreEqual(1, GTreeDestroyCount);
end;

procedure TTestRouterLifetime.RadixFreedWithLastInterface;
var
  LRouter: IHorseRouter;
begin
  GRadixDestroyCount := 0;
  LRouter := TTrackedRadixRouter.Create;
  LRouter.RegisterRoute(mtGet, '/api/users/:id', nil);
  LRouter := nil;
  Assert.AreEqual(1, GRadixDestroyCount);
end;

procedure TTestRouterLifetime.ReplacingGlobalRouterFreesPrevious;
var
  LPrevious: IHorseRouter;
begin
  LPrevious := THorseCore.Routes;
  GTreeDestroyCount := 0;
  GRadixDestroyCount := 0;
  try
    THorseCore.Routes := TTrackedRouterTree.Create;
    THorseCore.Routes := TTrackedRadixRouter.Create;
    Assert.AreEqual(1, GTreeDestroyCount);
  finally
    THorseCore.Routes := LPrevious;
  end;
  Assert.AreEqual(1, GRadixDestroyCount);
end;

procedure TTestRouterLifetime.InstanceRouterSurvivesWhileInterfaceIsHeld;
var
  LInstance: THorseInstance;
  LRouter: IHorseRouter;
begin
  LInstance := THorseInstance.Create;
  try
    LRouter := LInstance.GetRoutes;
  finally
    LInstance.Free;
  end;
  Assert.AreEqual('', LRouter.GetPrefix);
  LRouter.RegisterRoute(mtGet, '/held-after-instance', nil);
  LRouter := nil;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestRouterLifetime);

end.
