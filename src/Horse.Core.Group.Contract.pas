unit Horse.Core.Group.Contract;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  Horse.Core.Route.Contract,
  Horse.Callback;

type

  IHorseCoreGroup<T: class> = interface
    ['{5EB734D6-6944-473E-9C79-506647E2F5E8}']
    function Prefix(const APrefix: string): IHorseCoreGroup<T>;
    function Route(const APath: string): IHorseCoreRoute<T>;
    function AddCallback(const ACallback: THorseCallback): IHorseCoreGroup<T>;
    function AddCallbacks(const ACallbacks: TArray<THorseCallback>): IHorseCoreGroup<T>;
    function Use(const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Use(const AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Use(const ACallbacks: array of THorseCallback): IHorseCoreGroup<T>; overload;
    function Use(const ACallbacks: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Get(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Get(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function Get(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
{$IFNDEF FPC}
    function Get(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
{$IFEND}
    function Put(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Put(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function Put(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
{$IFNDEF FPC}
    function Put(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
{$IFEND}
    function Head(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Head(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function Head(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
{$IFNDEF FPC}
    function Head(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
{$IFEND}
    function Post(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Post(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function Post(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
{$IFNDEF FPC}
    function Post(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
{$IFEND}
{$IF (defined(fpc) or (CompilerVersion > 27.0))}
    function Patch(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Delete(const APath: string; const ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Patch(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function Patch(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
{$IFNDEF FPC}
    function Patch(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
{$IFEND}
    function Delete(const APath: string; const ACallback: THorseCallbackRequestResponse): IHorseCoreGroup<T>; overload;
    function Delete(const APath: string; const ACallback: THorseCallbackRequest): IHorseCoreGroup<T>; overload;
{$IFNDEF FPC}
    function Delete(const APath: string; const ACallback: THorseCallbackResponse): IHorseCoreGroup<T>; overload;
{$IFEND}
{$IFEND}
    function &End: T;
  end;

implementation

end.
