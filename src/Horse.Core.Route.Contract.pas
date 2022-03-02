unit Horse.Core.Route.Contract;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses Horse.Core.RouterTree, Horse.Callback;

type
  IHorseCoreRoute<T: class> = interface
    ['{8D593D98-44B3-4FD2-A21B-BA29F784B3AA}']
    function AddCallback(const ACallback: THorseCallback): IHorseCoreRoute<T>;

    function All(const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function All(const AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function All(const ACallbacks: array of THorseCallback): IHorseCoreRoute<T>; overload;
    function All(const ACallbacks: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;

    function Get(const ACallback: THorseCallback): IHorseCoreRoute<T>;
    function Put(const ACallback: THorseCallback): IHorseCoreRoute<T>;
    function Head(const ACallback: THorseCallback): IHorseCoreRoute<T>;
    function Post(const ACallback: THorseCallback): IHorseCoreRoute<T>;

    {$IF (DEFINED(FPC) OR (CompilerVersion > 27.0))}
    function Patch(const ACallback: THorseCallback): IHorseCoreRoute<T>;
    function Delete(const ACallback: THorseCallback): IHorseCoreRoute<T>;
    {$IFEND}

    function &End: T;
  end;

implementation

end.
