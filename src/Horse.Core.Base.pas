unit Horse.Core.Base;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  {$IF DEFINED(FPC)}
  Generics.Collections,
  {$ENDIF}
  Horse.Callback;

type
  THorseCoreBase = class
  public
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

    function BasePut(const APath: string; const ACallback: THorseCallback): THorseCoreBase; overload; virtual; abstract;
    function BasePut(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase; overload; virtual; abstract;
    function BasePut(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase; overload; virtual; abstract;
    {$IFNDEF FPC}
    function BasePut(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase; overload; virtual; abstract;
    {$ENDIF}

    function BaseHead(const APath: string; const ACallback: THorseCallback): THorseCoreBase; overload; virtual; abstract;
    function BaseHead(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase; overload; virtual; abstract;
    function BaseHead(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase; overload; virtual; abstract;
    {$IFNDEF FPC}
    function BaseHead(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase; overload; virtual; abstract;
    {$ENDIF}

    function BasePost(const APath: string; const ACallback: THorseCallback): THorseCoreBase; overload; virtual; abstract;
    function BasePost(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase; overload; virtual; abstract;
    function BasePost(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase; overload; virtual; abstract;
    {$IFNDEF FPC}
    function BasePost(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase; overload; virtual; abstract;
    {$ENDIF}

    function BaseDelete(const APath: string; const ACallback: THorseCallback): THorseCoreBase; overload; virtual; abstract;
    function BaseDelete(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase; overload; virtual; abstract;
    function BaseDelete(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase; overload; virtual; abstract;
    {$IFNDEF FPC}
    function BaseDelete(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase; overload; virtual; abstract;
    {$ENDIF}

    function BasePatch(const APath: string; const ACallback: THorseCallback): THorseCoreBase; overload; virtual; abstract;
    function BasePatch(const APath: string; const ACallback: THorseCallbackRequestResponse): THorseCoreBase; overload; virtual; abstract;
    function BasePatch(const APath: string; const ACallback: THorseCallbackRequest): THorseCoreBase; overload; virtual; abstract;
    {$IFNDEF FPC}
    function BasePatch(const APath: string; const ACallback: THorseCallbackResponse): THorseCoreBase; overload; virtual; abstract;
    {$ENDIF}
  end;

var
  GetHorseCoreInstance: function: THorseCoreBase = nil;

implementation

end.
