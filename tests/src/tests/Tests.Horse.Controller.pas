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
  {$M+}
  TMyController = class(THorseController)
  public
    class var ExecutedGet: Boolean;
    class var ExecutedListUsers: Boolean;
    class var ExecutedGetUserById: Boolean;
    
    procedure Get; override;
  published
    procedure ListUsers;
    procedure GetUserById;
  end;
  {$M-}

  TMyRequestMock = class(THorseRequest)
  private
    FMethodType: TMethodType;
    FPathInfo: string;
  public
    function MethodType: TMethodType; override;
    function PathInfo: string; override;
    constructor Create(AMethod: TMethodType; const APathInfo: string);
  end;

  [TestFixture]
  TTestHorseController = class
  public
    [Setup]
    procedure Setup;
    
    [Test]
    procedure TestFallbackToExecute;
    
    [Test]
    procedure TestMappedMethodListUsers;
    
    [Test]
    procedure TestMappedMethodGetUserById;
  end;

implementation

{ TMyController }

procedure TMyController.Get;
begin
  ExecutedGet := True;
end;

procedure TMyController.ListUsers;
begin
  ExecutedListUsers := True;
end;

procedure TMyController.GetUserById;
begin
  ExecutedGetUserById := True;
end;

{ TMyRequestMock }

constructor TMyRequestMock.Create(AMethod: TMethodType; const APathInfo: string);
begin
  inherited Create(nil);
  FMethodType := AMethod;
  FPathInfo := APathInfo;
end;

function TMyRequestMock.MethodType: TMethodType;
begin
  Result := FMethodType;
end;

function TMyRequestMock.PathInfo: string;
begin
  Result := FPathInfo;
end;

{ TTestHorseController }

procedure TTestHorseController.Setup;
begin
  TMyController.ExecutedGet := False;
  TMyController.ExecutedListUsers := False;
  TMyController.ExecutedGetUserById := False;
  
  // Register routes mapping for testing
  THorseController<TMyController>.Map('/users', mtGet, 'ListUsers');
  THorseController<TMyController>.Map('/users/:id', mtGet, 'GetUserById');
end;

procedure TTestHorseController.TestFallbackToExecute;
var
  LReq: TMyRequestMock;
begin
  // A path that is not mapped but falls back to standard execution
  LReq := TMyRequestMock.Create(mtGet, '/unmapped/path');
  try
    THorseController<TMyController>.Handle(LReq, nil, nil);
    Assert.IsTrue(TMyController.ExecutedGet);
    Assert.IsFalse(TMyController.ExecutedListUsers);
  finally
    LReq.Free;
  end;
end;

procedure TTestHorseController.TestMappedMethodListUsers;
var
  LReq: TMyRequestMock;
begin
  LReq := TMyRequestMock.Create(mtGet, '/users');
  try
    THorseController<TMyController>.Handle(LReq, nil, nil);
    Assert.IsTrue(TMyController.ExecutedListUsers);
    Assert.IsFalse(TMyController.ExecutedGet);
  finally
    LReq.Free;
  end;
end;

procedure TTestHorseController.TestMappedMethodGetUserById;
var
  LReq: TMyRequestMock;
begin
  LReq := TMyRequestMock.Create(mtGet, '/users/123');
  try
    THorseController<TMyController>.Handle(LReq, nil, nil);
    Assert.IsTrue(TMyController.ExecutedGetUserById);
    Assert.IsFalse(TMyController.ExecutedListUsers);
  finally
    LReq.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestHorseController);

end.
