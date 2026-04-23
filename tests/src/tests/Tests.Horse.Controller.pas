unit Tests.Horse.Controller;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  DUnitX.TestFramework,
  Horse,
  {$IF DEFINED(FPC)}
  SysUtils,
  fpHTTP
  {$ELSE}
  System.SysUtils,
  Web.HTTPApp
  {$ENDIF};

type
  TMyController = class(THorseController)
  public
    class var Executed: Boolean;
    procedure Get; override;
  end;

  TMyRequestMock = class(THorseRequest)
  public
    function MethodType: TMethodType; override;
  end;

  [TestFixture]
  TTestHorseController = class
  public
    [Setup]
    procedure Setup;
    [Test]
    procedure TestHandle;
  end;

implementation

{ TMyController }

procedure TMyController.Get;
begin
  Executed := True;
end;

{ TMyRequestMock }

function TMyRequestMock.MethodType: TMethodType;
begin
  Result := mtGet;
end;

{ TTestHorseController }

procedure TTestHorseController.Setup;
begin
  TMyController.Executed := False;
end;

procedure TTestHorseController.TestHandle;
var
  LReq: TMyRequestMock;
begin
  LReq := TMyRequestMock.Create(nil);
  try
    THorseController<TMyController>.Handle(LReq, nil, nil);
    Assert.IsTrue(TMyController.Executed);
  finally
    LReq.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestHorseController);

end.
