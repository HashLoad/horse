unit Horse.Core.Group.Intf;

interface

uses
  Horse.Router, Horse.Core.Route.Intf;

type
  IHorseCoreGroup = interface
    ['{1133353C-3651-40D8-9A56-3EC0A0FB755F}']
    function Prefix(APrefix: string): IHorseCoreGroup;
    function Route(APath: string): IHorseCoreRoute;

    function Get(APath: string; ACallback: THorseCallback): IHorseCoreGroup; overload;
    function Get(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup; overload;
    function Get(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup; overload;
    function Get(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup; overload;

    function Put(APath: string; ACallback: THorseCallback): IHorseCoreGroup; overload;
    function Put(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup; overload;
    function Put(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup; overload;
    function Put(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup; overload;

    function Patch(APath: string; ACallback: THorseCallback): IHorseCoreGroup; overload;
    function Patch(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup; overload;
    function Patch(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup; overload;
    function Patch(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup; overload;

    function Head(APath: string; ACallback: THorseCallback): IHorseCoreGroup; overload;
    function Head(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup; overload;
    function Head(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup; overload;
    function Head(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup; overload;

    function Post(APath: string; ACallback: THorseCallback): IHorseCoreGroup; overload;
    function Post(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup; overload;
    function Post(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup; overload;
    function Post(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup; overload;

    function Delete(APath: string; ACallback: THorseCallback): IHorseCoreGroup; overload;
    function Delete(APath: string; AMiddleware, ACallback: THorseCallback): IHorseCoreGroup; overload;
    function Delete(APath: string; ACallbacks: array of THorseCallback): IHorseCoreGroup; overload;
    function Delete(APath: string; ACallbacks: array of THorseCallback; ACallback: THorseCallback): IHorseCoreGroup; overload;
  end;

implementation

end.
