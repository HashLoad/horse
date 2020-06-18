unit Horse.Core.Route.Intf;

interface

uses
  Horse.Router;

type
  IHorseCoreRoute = interface
    ['{39ABE601-E812-40B4-8857-F14C938B9773}']
    function Get(ACallback: THorseCallback): IHorseCoreRoute; overload;
    function Get(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute; overload;
    function Get(ACallbacks: array of THorseCallback): IHorseCoreRoute; overload;
    function Get(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute; overload;

    function Put(ACallback: THorseCallback): IHorseCoreRoute; overload;
    function Put(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute; overload;
    function Put(ACallbacks: array of THorseCallback): IHorseCoreRoute; overload;
    function Put(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute; overload;

    function Patch(ACallback: THorseCallback): IHorseCoreRoute; overload;
    function Patch(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute; overload;
    function Patch(ACallbacks: array of THorseCallback): IHorseCoreRoute; overload;
    function Patch(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute; overload;

    function Head(ACallback: THorseCallback): IHorseCoreRoute; overload;
    function Head(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute; overload;
    function Head(ACallbacks: array of THorseCallback): IHorseCoreRoute; overload;
    function Head(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute; overload;

    function Post(ACallback: THorseCallback): IHorseCoreRoute; overload;
    function Post(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute; overload;
    function Post(ACallbacks: array of THorseCallback): IHorseCoreRoute; overload;
    function Post(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute; overload;

    function Delete(ACallback: THorseCallback): IHorseCoreRoute; overload;
    function Delete(AMiddleware, ACallback: THorseCallback): IHorseCoreRoute; overload;
    function Delete(ACallbacks: array of THorseCallback): IHorseCoreRoute; overload;
    function Delete(ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreRoute; overload;
  end;

implementation

end.
