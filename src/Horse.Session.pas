unit Horse.Session;

{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils,
  Generics.Collections;
{$ELSE}
  System.SysUtils,
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
    property Session[const ASessionClass: TSessionClass]: TSession read GetSession;
    property &Object[const ASessionClass: TSessionClass]: TObject read GetObject;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor THorseSessions.Create;
begin
  FSessions := TObjectDictionary<TSessionClass, TSession>.Create([doOwnsValues]);
end;

destructor THorseSessions.Destroy;
begin
  FSessions.Free;
  inherited Destroy;
end;

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
