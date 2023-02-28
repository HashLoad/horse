unit Horse.Core.Route.Contract;

{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  Horse.Callback;

type
  IHorseCoreRoute<T: class> = interface
    ['{8D593D98-44B3-4FD2-A21B-BA29F784B3AA}']
    function AddCallback(const ACallback: THorseCallback): IHorseCoreRoute<T>;
    function AddCallbacks(const ACallbacks: TArray<THorseCallback>): IHorseCoreRoute<T>;
    function All(const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function All(const AMiddleware, ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function All(const ACallbacks: array of THorseCallback): IHorseCoreRoute<T>; overload;
    function All(const ACallbacks: array of THorseCallback; const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Get(const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Get(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>; overload;
    function Get(const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>; overload;
{$IFNDEF FPC}
    function Get(const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>; overload;
{$IFEND}
    function Put(const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Put(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>; overload;
    function Put(const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>; overload;
{$IFNDEF FPC}
    function Put(const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>; overload;
{$IFEND}
    function Head(const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Head(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>; overload;
    function Head(const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>; overload;
{$IFNDEF FPC}
    function Head(const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>; overload;
{$IFEND}
    function Post(const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Post(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>; overload;
    function Post(const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>; overload;
{$IFNDEF FPC}
    function Post(const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>; overload;
{$IFEND}
{$IF (DEFINED(FPC) OR (CompilerVersion > 27.0))}
    function Patch(const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Delete(const ACallback: THorseCallback): IHorseCoreRoute<T>; overload;
    function Patch(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>; overload;
    function Patch(const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>; overload;
{$IFNDEF FPC}
    function Patch(const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>; overload;
{$IFEND}
    function Delete(const ACallback: THorseCallbackRequestResponse): IHorseCoreRoute<T>; overload;
    function Delete(const ACallback: THorseCallbackRequest): IHorseCoreRoute<T>; overload;
{$IFNDEF FPC}
    function Delete(const ACallback: THorseCallbackResponse): IHorseCoreRoute<T>; overload;
{$IFEND}
{$IFEND}
    function &End: T;
  end;

implementation

end.
