program HeaderHashCheck;

{$MODE DELPHI}{$H+}
{$IFOPT R-}{$FATAL Range checking must be enabled}{$ENDIF}
{$IFOPT Q-}{$FATAL Overflow checking must be enabled}{$ENDIF}

uses
  SysUtils, Generics.Defaults, HTTPDefs,
  Horse.Core.Param, Horse.Core.Param.Header;

type
  THashCase = record
    Name: string;
    Expected: DWord;
  end;
  TTestProc = procedure;

const
  // Independent DJB2 modulo-2^32 vectors, including values above MaxInt.
  Cases: array[0..6] of THashCase = (
    (Name: ''; Expected: 5381),
    (Name: 'Host'; Expected: 2090335843),
    (Name: 'Content-Type'; Expected: 4200957359),
    (Name: 'Authorization'; Expected: 1595898102),
    (Name: 'X-Custom-Header'; Expected: 4148212891),
    (Name: 'Accept-Encoding'; Expected: 1116246921),
    (Name: 'X-Trace-Id'; Expected: 3544274515)
  );

var
  Checks, Groups, Failures: Integer;

procedure Check(Condition: Boolean; const MessageText: string);
begin
  Inc(Checks);
  if not Condition then
    raise Exception.Create(MessageText);
end;

procedure RunTest(const Name: string; TestProc: TTestProc);
begin
  Inc(Groups);
  try
    TestProc;
    Writeln('PASS ', Name);
  except
    on E: Exception do
    begin
      Inc(Failures);
      Writeln('FAIL ', Name, ': ', E.ClassName, ': ', E.Message);
    end;
  end;
end;

procedure TestHashVectors;
var
  Comparer: IEqualityComparer<string>;
  I: Integer;
begin
  Comparer := THorseHeaderComparer.Create;
  for I := Low(Cases) to High(Cases) do
  begin
    Check(Comparer.GetHashCode(Cases[I].Name) = Cases[I].Expected,
      'Unexpected hash for ' + Cases[I].Name);
    Check(Comparer.GetHashCode(UpperCase(Cases[I].Name)) = Cases[I].Expected,
      'Uppercase hash mismatch for ' + Cases[I].Name);
    Check(Comparer.GetHashCode(LowerCase(Cases[I].Name)) = Cases[I].Expected,
      'Lowercase hash mismatch for ' + Cases[I].Name);
    Check(Comparer.Equals(Cases[I].Name, UpperCase(Cases[I].Name)),
      'Comparer equality mismatch for ' + Cases[I].Name);
  end;
end;

procedure TestLongHeader;
var
  Comparer: IEqualityComparer<string>;
  Name: string;
begin
  Comparer := THorseHeaderComparer.Create;
  Name := StringOfChar('X', 4096);
  Check(Comparer.GetHashCode(Name) = 2600047877, 'Long header hash changed');
  Check(Comparer.GetHashCode(LowerCase(Name)) = 2600047877,
    'Long lowercase header hash changed');
end;

procedure TestDictionaryOperations;
var
  Comparer: IEqualityComparer<string>;
  Headers: THorseList;
  I: Integer;
  Value: string;
begin
  Comparer := THorseHeaderComparer.Create;
  Headers := THorseList.Create(Comparer);
  try
    for I := Low(Cases) to High(Cases) do
      Headers.AddOrSetValue(Cases[I].Name, IntToStr(I));
    Check(Headers.Count = Length(Cases), 'Header insertion count');
    for I := Low(Cases) to High(Cases) do
    begin
      Check(Headers.TryGetValue(LowerCase(Cases[I].Name), Value),
        'Lowercase header lookup');
      Check(Value = IntToStr(I), 'Incorrect header value');
      Headers.AddOrSetValue(UpperCase(Cases[I].Name), 'updated');
      Check(Headers[Cases[I].Name] = 'updated', 'Case-insensitive update');
    end;
    Check(Headers.Count = Length(Cases), 'Updates must not duplicate keys');
    for I := Low(Cases) to High(Cases) do
    begin
      Headers.Remove(LowerCase(Cases[I].Name));
      Check(not Headers.ContainsKey(UpperCase(Cases[I].Name)), 'Header removal');
    end;
    Check(Headers.Count = 0, 'Dictionary must be empty after removal');
  finally
    Headers.Free;
  end;
end;

procedure TestRequestHeaderExtraction;
var
  Request: TRequest;
  Headers: THorseList;
begin
  Request := TRequest.Create;
  try
    Request.ContentType := 'application/json';
    Request.Host := 'localhost';
    Headers := THorseCoreParamHeader.GetHeaders(Request);
    try
      Check(Headers['content-type'] = 'application/json', 'Content-Type extraction');
      Check(Headers['HOST'] = 'localhost', 'Host extraction');
    finally
      Headers.Free;
    end;
  finally
    Request.Free;
  end;
end;

begin
  RunTest('known hashes and ASCII case equivalence', @TestHashVectors);
  RunTest('4096-character header', @TestLongHeader);
  RunTest('header dictionary insert/lookup/update/remove', @TestDictionaryOperations);
  RunTest('request header extraction', @TestRequestHeaderExtraction);
  Writeln('Groups: ', Groups, '; checks: ', Checks, '; failures: ', Failures);
  if Failures <> 0 then
    Halt(1);
end.
