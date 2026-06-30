unit Tests.AssertHelper;

interface

uses
  DUnitX.TestFramework, System.SysUtils;

type
  TAssertHelper = class helper for Assert
  public
    class procedure WillRaiseWithMessage(const AMethod: TProc; const AExceptionClass: TClass; const AExpectedMessage: string; const AMessage: string = '');
    class procedure WillNotRaiseWithMessage(const AMethod: TProc; const AExceptionClass: TClass; const AExpectedMessage: string; const AMessage: string = '');
  end;

implementation

{ TAssertHelper }

class procedure TAssertHelper.WillRaiseWithMessage(const AMethod: TProc; const AExceptionClass: TClass;
  const AExpectedMessage: string; const AMessage: string);
var
  LFailed: Boolean;
begin
  LFailed := True;
  try
    AMethod();
    LFailed := False;
  except
    on E: Exception do
    begin
      if (AExceptionClass <> nil) and (not E.InheritsFrom(AExceptionClass)) then
        Assert.Fail(Format('Expected exception of type %s, but got %s: %s', [AExceptionClass.ClassName, E.ClassName, E.Message]), ReturnAddress)
      else if (AExpectedMessage <> '') and (not E.Message.Contains(AExpectedMessage)) then
        Assert.Fail(Format('Expected exception message to contain "%s", but got "%s"', [AExpectedMessage, E.Message]), ReturnAddress)
      else
        Assert.IsTrue(True);
    end;
  end;
  if not LFailed then
    Assert.Fail(Format('Expected exception of type %s was not raised.', [AExceptionClass.ClassName]), ReturnAddress);
end;

class procedure TAssertHelper.WillNotRaiseWithMessage(const AMethod: TProc; const AExceptionClass: TClass;
  const AExpectedMessage: string; const AMessage: string);
var
  LRaised: Boolean;
begin
  LRaised := False;
  try
    AMethod();
  except
    on E: Exception do
    begin
      if (AExceptionClass <> nil) and E.InheritsFrom(AExceptionClass) then
      begin
        if (AExpectedMessage = '') or E.Message.Contains(AExpectedMessage) then
        begin
          LRaised := True;
          Assert.Fail(Format('Exception of type %s was raised: %s', [E.ClassName, E.Message]), ReturnAddress);
        end;
      end;
    end;
  end;
  if not LRaised then
    Assert.IsTrue(True);
end;

end.
