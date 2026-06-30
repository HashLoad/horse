unit Horse.Core.Router.Contract;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  {$IF DEFINED(FPC)}
  httpdefs,
  {$ELSE}
  Web.HTTPApp,
  {$ENDIF}
  Horse.Commons,
  Horse.Callback,
  Horse.Request,
  Horse.Response;

type
  IHorseRouter = interface
    ['{69A48E4B-8E4B-4D4B-A69A-7D8E4B6D8E4B}']
    procedure RegisterRoute(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback);
    procedure RegisterRouteMiddleware(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback);
    procedure RegisterMiddleware(const APath: string; const AMiddleware: THorseCallback); overload;
    procedure RegisterMiddleware(const AMiddleware: THorseCallback); overload;
    function Execute(const ARequest: THorseRequest; const AResponse: THorseResponse): Boolean;
  end;

  PHorseRouter = ^IHorseRouter;

implementation

end.
