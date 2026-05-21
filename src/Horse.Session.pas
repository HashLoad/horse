unit Horse.Session;

{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  Generics.Collections;
{$ELSE}
  System.Generics.Collections;
{$ENDIF}

type
  TSession = class
  end;

  TSessionClass = class of TSession;

  THorseSessions = class
  private
    FSessions: TObjectDictionary<TSessionClass, TSession>;
    function GetSession(const ASessionClass: TSessionClass): TSession; overload;
    function GetObject(const ASessionClass: TSessionClass): TObject; overload;
  public
    function TryGetSession<T: class>(out ASession: T): Boolean;
    function Contains(const ASessionClass: TSessionClass): Boolean;
    function SetSession(const ASessionClass: TSessionClass; const AInstance: TSession): THorseSessions;
{ PATCH-SES-1 — Clear: wipe all stored sessions in-place for pool reuse.
  TObjectDictionary([doOwnsValues]).Clear frees every TSession before removing
  it, so callers do not need to free sessions manually.  The dictionary object
  itself is kept alive so the next request reuses it without any allocation. }
    procedure Clear;
{ end PATCH-SES-1 }
    property Session[const ASessionClass: TSessionClass]: TSession read GetSession;
    property &Object[const ASessionClass: TSessionClass]: TObject read GetObject;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
{$IF DEFINED(FPC)}
  SysUtils;
{$ELSE}
  System.SysUtils;
{$ENDIF}

constructor THorseSessions.Create;
begin
  FSessions := TObjectDictionary<TSessionClass, TSession>.Create([doOwnsValues]);
end;

destructor THorseSessions.Destroy;
begin
  FSessions.Free;
  inherited Destroy;
end;

{ PATCH-SES-1 }
procedure THorseSessions.Clear;
begin
  FSessions.Clear;
end;
{ end PATCH-SES-1 }

function THorseSessions.GetObject(const ASessionClass: TSessionClass): TObject;
begin
  Result := FSessions.Items[ASessionClass];
end;

function THorseSessions.GetSession(const ASessionClass: TSessionClass): TSession;
begin
  Result := FSessions.Items[ASessionClass];
end;

function THorseSessions.SetSession(const ASessionClass: TSessionClass; const AInstance: TSession): THorseSessions;
begin
  Result := Self;
  if not ASessionClass.InheritsFrom(AInstance.ClassType) then
    raise Exception.CreateFmt('SessionClass differs from of instance[%s].', [AInstance.ClassType.ClassName]);
  FSessions.AddOrSetValue(ASessionClass, AInstance);
end;

function THorseSessions.Contains(const ASessionClass: TSessionClass): Boolean;
begin
  Result := FSessions.ContainsKey(ASessionClass);
end;

function THorseSessions.TryGetSession<T>(out ASession: T): Boolean;
begin
  Result := FSessions.TryGetValue(TSessionClass(T), TSession(ASession));
end;

end.
