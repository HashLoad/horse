unit Tests.Horse.Core.Cookie;

interface

uses
  DUnitX.TestFramework, System.SysUtils, Horse.Commons, Horse.Exception,
  Horse.Core.Cookie;

type
  // THorseCookie refuses invalid input by raising EHorseException, and a raise
  // inside a route reaches the client as EHorseException.ToJSON with
  // EHorseException.Status. These tests pin that every refusal carries its
  // reason in Message (what logs and `on E: Exception` handlers read) AND in
  // Error (the "error" field of the JSON reply), with status 500.
  //
  // Before this fix the unit raised in two ways that each lost something:
  // EHorseException.Create(text) stores text as the title only, leaving
  // Message and Error empty; the inherited Exception.CreateFmt sets Message
  // but skips EHorseException.Create, so Error stays empty and Status is 0,
  // which is not a THTTPStatus value.
  [TestFixture]
  TTestHorseCoreCookie = class
  private
    procedure AssertRefusal(const AAction: TProc; const AExpectedText: string);
  public
    [Test]
    procedure ValidCookieIsAccepted;
    [Test]
    procedure InvalidNameCarriesItsReason;
    [Test]
    procedure InvalidValueInConstructorCarriesItsReason;
    [Test]
    procedure InvalidValueInSetterCarriesItsReason;
    [Test]
    procedure MissingNameAtToHeaderValueCarriesItsReason;
  end;

implementation

procedure TTestHorseCoreCookie.AssertRefusal(const AAction: TProc;
  const AExpectedText: string);
var
  LRaised: Boolean;
  LMessage, LError: string;
  LStatus: THTTPStatus;
begin
  LRaised := False;
  LStatus := THTTPStatus.OK;
  try
    AAction();
  except
    on E: EHorseException do
    begin
      LRaised := True;
      LMessage := E.Message;
      LError := E.Error;
      LStatus := E.Status;
    end;
  end;
  Assert.IsTrue(LRaised, 'expected EHorseException for: ' + AExpectedText);
  Assert.IsTrue(Pos(AExpectedText, LMessage) > 0,
    'Message must carry the reason, got [' + LMessage + ']');
  Assert.AreEqual(LMessage, LError,
    'Error (the JSON "error" field) must carry the same reason');
  Assert.IsTrue(LStatus = THTTPStatus.InternalServerError,
    Format('Status must be 500, got %d', [Ord(LStatus)]));
end;

procedure TTestHorseCoreCookie.ValidCookieIsAccepted;
var
  LCookie: THorseCookie;
begin
  // Control: the refusals below are not just "everything raises".
  LCookie := THorseCookie.Create('session', 'abc123');
  try
    Assert.AreEqual('session=abc123', LCookie.ToHeaderValue);
  finally
    LCookie.Free;
  end;
end;

procedure TTestHorseCoreCookie.InvalidNameCarriesItsReason;
begin
  AssertRefusal(
    procedure
    begin
      THorseCookie.Create('bad name', 'v').Free;
    end,
    'Invalid character in cookie name');
end;

procedure TTestHorseCoreCookie.InvalidValueInConstructorCarriesItsReason;
begin
  AssertRefusal(
    procedure
    begin
      THorseCookie.Create('ok', 'a;b').Free;
    end,
    'Invalid character in cookie value');
end;

procedure TTestHorseCoreCookie.InvalidValueInSetterCarriesItsReason;
begin
  AssertRefusal(
    procedure
    var
      LCookie: THorseCookie;
    begin
      LCookie := THorseCookie.Create('ok');
      try
        LCookie.Path('/a' + #13#10 + 'Set-Cookie: x=y');
      finally
        LCookie.Free;
      end;
    end,
    'Invalid character in cookie Path');
end;

procedure TTestHorseCoreCookie.MissingNameAtToHeaderValueCarriesItsReason;
begin
  AssertRefusal(
    procedure
    var
      LCookie: THorseCookie;
    begin
      LCookie := THorseCookie.Create;
      try
        LCookie.ToHeaderValue;
      finally
        LCookie.Free;
      end;
    end,
    'Cookie name must be set before ToHeaderValue');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestHorseCoreCookie);

end.
