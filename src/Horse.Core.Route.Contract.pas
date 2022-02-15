unit Horse.Core.Route.Contract;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses Horse.Core.RouterTree;

type
  IHorseCoreRoute<T: class> = interface
    ['{8D593D98-44B3-4FD2-A21B-BA29F784B3AA}']
    function AddCallback(ACallback: THorseCallback): IHorseCoreRoute<T>;

    function All(ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function All(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function All(ACallbacks: array of THorseCallback): IHorseCoreRoute<T>; overload;
    function All(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute<T>; overload;

    function Get(ACallback: THorseCallback): IHorseCoreRoute<T>;
    function Put(ACallback: THorseCallback): IHorseCoreRoute<T>;
    function Head(ACallback: THorseCallback): IHorseCoreRoute<T>;
    function Post(ACallback: THorseCallback): IHorseCoreRoute<T>;

    {$IF (defined(fpc) or (CompilerVersion > 27.0))}
    function Patch(ACallback: THorseCallback): IHorseCoreRoute<T>;
    function Delete(ACallback: THorseCallback): IHorseCoreRoute<T>;
    {$IFEND}

    function &End: T;
  end;

implementation

end.
