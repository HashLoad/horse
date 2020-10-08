unit Horse.Core.Group.Contract;
{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}
interface

uses
  Horse.Core.Route.Contract, Horse.Core.RouterTree;

type
   IHorseCoreGroup<T: class> = interface
    ['{5EB734D6-6944-473E-9C79-506647E2F5E8}']
    function Prefix(APrefix: string): IHorseCoreGroup<T>;
    function Route(APath: string): IHorseCoreRoute<T>;

    function Use(ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Use(AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Use(ACallbacks: array of THorseCallback): IHorseCoreGroup<T>; overload;
    function Use(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;

    function Get(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Get(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Get(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup<T>; overload;
    function Get(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;

    function Put(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Put(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Put(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup<T>; overload;
    function Put(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;

    {$IF (defined(fpc) or (CompilerVersion > 27.0))}
    function Patch(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Patch(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Patch(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup<T>; overload;
    function Patch(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    {$IFEND}

    function Head(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Head(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Head(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup<T>; overload;
    function Head(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;

    function Post(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Post(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Post(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup<T>; overload;
    function Post(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;

    {$IF (defined(fpc) or (CompilerVersion > 27.0))}
    function Delete(APath: string; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Delete(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    function Delete(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup<T>; overload;
    function Delete(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup<T>; overload;
    {$IFEND}

    function &End: T;
  end;

implementation

end.
