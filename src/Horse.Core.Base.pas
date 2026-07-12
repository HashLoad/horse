unit Horse.Core.Base;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  {$IF DEFINED(FPC)}
  Generics.Collections,
  syncobjs,
  {$ELSE}
  System.Generics.Collections,
  System.SyncObjs,
  {$ENDIF}
  Horse.Core.Router.Contract,
  Horse.Callback;

type
  THorseCoreBase = class
  public
    function GetRoutes: IHorseRouter; virtual; abstract;
    procedure DoIncrementActiveRequests; virtual; abstract;
    procedure DoDecrementActiveRequests; virtual; abstract;

    function BaseAddCallback(const ACallback: THorseCallback): THorseCoreBase; virtual; abstract;
    {$IF DEFINED(FPC)}
    function BaseAddCallbacks(const ACallbacks: TList<THorseCallback>): THorseCoreBase; virtual; abstract;
    {$ELSE}
    function BaseAddCallbacks(const ACallbacks: TArray<THorseCallback>): THorseCoreBase; virtual; abstract;
    {$ENDIF}
    function BaseUse(const APath: string; const ACallback: THorseCallback): THorseCoreBase; overload; virtual; abstract;
    function BaseUse(const ACallback: THorseCallback): THorseCoreBase; overload; virtual; abstract;
    function BaseUse(const APath: string; const ACallbacks: array of THorseCallback): THorseCoreBase; overload; virtual; abstract;
    function BaseUse(const ACallbacks: array of THorseCallback): THorseCoreBase; overload; virtual; abstract;

    function BaseRoute(const APath: string): IInterface; virtual; abstract;

    function BaseAll(const APath: string; const ACallback: THorseCallback): THorseCoreBase; overload; virtual; abstract;
    function BaseAll(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase; overload; virtual; abstract;
    function BaseAll(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase; overload; virtual; abstract;
    {$IFNDEF FPC}
    function BaseAll(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase; overload; virtual; abstract;
    {$ENDIF}

    function BaseGet(const APath: string; const ACallback: THorseCallback): THorseCoreBase; overload; virtual; abstract;
    function BaseGet(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase; overload; virtual; abstract;
    function BaseGet(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase; overload; virtual; abstract;
    {$IFNDEF FPC}
    function BaseGet(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase; overload; virtual; abstract;
    {$ENDIF}

    // Put
    function BasePut(const APath: string; const ACallback: THorseCallback): THorseCoreBase; overload; virtual; abstract;
    function BasePut(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase; overload; virtual; abstract;
    function BasePut(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase; overload; virtual; abstract;
    {$IFNDEF FPC}
    function BasePut(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase; overload; virtual; abstract;
    {$ENDIF}

    // Head
    function BaseHead(const APath: string; const ACallback: THorseCallback): THorseCoreBase; overload; virtual; abstract;
    function BaseHead(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase; overload; virtual; abstract;
    function BaseHead(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase; overload; virtual; abstract;
    {$IFNDEF FPC}
    function BaseHead(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase; overload; virtual; abstract;
    {$ENDIF}

    // Post
    function BasePost(const APath: string; const ACallback: THorseCallback): THorseCoreBase; overload; virtual; abstract;
    function BasePost(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase; overload; virtual; abstract;
    function BasePost(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase; overload; virtual; abstract;
    {$IFNDEF FPC}
    function BasePost(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase; overload; virtual; abstract;
    {$ENDIF}

    // Delete
    function BaseDelete(const APath: string; const ACallback: THorseCallback): THorseCoreBase; overload; virtual; abstract;
    function BaseDelete(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase; overload; virtual; abstract;
    function BaseDelete(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase; overload; virtual; abstract;
    {$IFNDEF FPC}
    function BaseDelete(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase; overload; virtual; abstract;
    {$ENDIF}

    // Patch
    function BasePatch(const APath: string; const ACallback: THorseCallback): THorseCoreBase; overload; virtual; abstract;
    function BasePatch(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase; overload; virtual; abstract;
    function BasePatch(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase; overload; virtual; abstract;
    {$IFNDEF FPC}
    function BasePatch(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase; overload; virtual; abstract;
    {$IFEND}
  end;

var
  GetHorseCoreInstance: function: THorseCoreBase = nil;
  GInstances: TDictionary<Integer, THorseCoreBase> = nil;
  GInstancesLock: TCriticalSection = nil;

procedure RegisterHorseInstance(const APort: Integer; const AInstance: THorseCoreBase);
procedure UnregisterHorseInstance(const APort: Integer);
function GetHorseInstanceByPort(const APort: Integer): THorseCoreBase;

implementation

uses
  SysUtils;

procedure RegisterHorseInstance(const APort: Integer; const AInstance: THorseCoreBase);
begin
  GInstancesLock.Enter;
  try
    GInstances.AddOrSetValue(APort, AInstance);
  finally
    GInstancesLock.Leave;
  end;
end;

procedure UnregisterHorseInstance(const APort: Integer);
begin
  GInstancesLock.Enter;
  try
    GInstances.Remove(APort);
  finally
    GInstancesLock.Leave;
  end;
end;

function GetHorseInstanceByPort(const APort: Integer): THorseCoreBase;
begin
  GInstancesLock.Enter;
  try
    if not GInstances.TryGetValue(APort, Result) then
    begin
      if Assigned(GetHorseCoreInstance) then
        Result := GetHorseCoreInstance()
      else
        Result := nil;
    end;
  finally
    GInstancesLock.Leave;
  end;
end;

initialization
  GInstances := TDictionary<Integer, THorseCoreBase>.Create;
  GInstancesLock := TCriticalSection.Create;

finalization
  FreeAndNil(GInstances);
  FreeAndNil(GInstancesLock);

end.
