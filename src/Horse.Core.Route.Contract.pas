unit Horse.Core.Route.Contract;
{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}
interface

uses
  Horse.Core.RouterTree;

type
   IHorseCoreRoute<T: class> = interface
    ['{8D593D98-44B3-4FD2-A21B-BA29F784B3AA}']
    function All(ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function All(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function All(ACallbacks: array of THorseCallback): IHorseCoreRoute<T>; overload;
    function All(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute<T>; overload;

    function Get(ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Get(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Get(ACallbacks: array of THorseCallback): IHorseCoreRoute<T>; overload;
    function Get(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute<T>; overload;

    function Put(ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Put(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Put(ACallbacks: array of THorseCallback): IHorseCoreRoute<T>; overload;
    function Put(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute<T>; overload;

    {$IF (defined(fpc) or (CompilerVersion > 27.0))}
    function Patch(ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Patch(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Patch(ACallbacks: array of THorseCallback): IHorseCoreRoute<T>; overload;
    function Patch(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    {$IFEND}

    function Head(ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Head(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Head(ACallbacks: array of THorseCallback): IHorseCoreRoute<T>; overload;
    function Head(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute<T>; overload;

    function Post(ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Post(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Post(ACallbacks: array of THorseCallback): IHorseCoreRoute<T>; overload;
    function Post(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute<T>; overload;

    {$IF (defined(fpc) or (CompilerVersion > 27.0))}
    function Delete(ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Delete(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Delete(ACallbacks: array of THorseCallback): IHorseCoreRoute<T>; overload;
    function Delete(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    {$IFEND}

    function &End: T;
  end;

implementation

end.
