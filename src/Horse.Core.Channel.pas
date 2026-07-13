unit Horse.Core.Channel;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.SyncObjs;

type
  THorseChannel = class
  public
    procedure Close; virtual; abstract;
    function IsClosed: Boolean; virtual; abstract;
    function ReadObject(out AValue: TObject; ATimeoutMS: Cardinal = INFINITE): Boolean; virtual; abstract;
    procedure WriteObject(const AValue: TObject); virtual; abstract;
    function Count: Integer; virtual; abstract;
  end;

  THorseChannel<T: class> = class(THorseChannel)
  private
    FQueue: TQueue<T>;
    FLock: TCriticalSection;
    FEvent: TSimpleEvent;
    FClosed: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Write(const AValue: T);
    function Read(out AValue: T; ATimeoutMS: Cardinal = INFINITE): Boolean;
    
    procedure Close; override;
    function IsClosed: Boolean; override;
    function ReadObject(out AValue: TObject; ATimeoutMS: Cardinal = INFINITE): Boolean; override;
    procedure WriteObject(const AValue: TObject); override;
    function Count: Integer; override;
  end;

implementation

{ THorseChannel<T> }

constructor THorseChannel<T>.Create;
begin
  inherited Create;
  FQueue := TQueue<T>.Create;
  FLock := TCriticalSection.Create;
  FEvent := TSimpleEvent.Create(nil, False, False, '');
  FClosed := False;
end;

destructor THorseChannel<T>.Destroy;
var
  LItem: T;
begin
  FLock.Enter;
  try
    while FQueue.Count > 0 do
    begin
      LItem := FQueue.Dequeue;
      if Assigned(LItem) then
        LItem.Free;
    end;
    FreeAndNil(FQueue);
  finally
    FLock.Leave;
    FreeAndNil(FLock);
  end;
  FreeAndNil(FEvent);
  inherited Destroy;
end;

procedure THorseChannel<T>.Write(const AValue: T);
begin
  FLock.Enter;
  try
    if FClosed then
      raise EInvalidOpException.Create('Cannot write to a closed channel');
    FQueue.Enqueue(AValue);
    FEvent.SetEvent;
  finally
    FLock.Leave;
  end;
end;

function THorseChannel<T>.Read(out AValue: T; ATimeoutMS: Cardinal = INFINITE): Boolean;
var
  LWaitResult: TWaitResult;
begin
  Result := False;
  while True do
  begin
    FLock.Enter;
    try
      if FQueue.Count > 0 then
      begin
        AValue := FQueue.Dequeue;
        Result := True;
        Exit;
      end;
      if FClosed then
      begin
        Exit;
      end;
    finally
      FLock.Leave;
    end;

    LWaitResult := FEvent.WaitFor(ATimeoutMS);
    if LWaitResult = wrTimeout then
      Exit;
  end;
end;

procedure THorseChannel<T>.Close;
begin
  FLock.Enter;
  try
    FClosed := True;
    FEvent.SetEvent;
  finally
    FLock.Leave;
  end;
end;

function THorseChannel<T>.IsClosed: Boolean;
begin
  Result := FClosed;
end;

function THorseChannel<T>.ReadObject(out AValue: TObject; ATimeoutMS: Cardinal = INFINITE): Boolean;
var
  LValue: T;
begin
  Result := Read(LValue, ATimeoutMS);
  if Result then
    AValue := LValue
  else
    AValue := nil;
end;

procedure THorseChannel<T>.WriteObject(const AValue: TObject);
begin
  Write(T(AValue));
end;

function THorseChannel<T>.Count: Integer;
begin
  FLock.Enter;
  try
    Result := FQueue.Count;
  finally
    FLock.Leave;
  end;
end;

end.
