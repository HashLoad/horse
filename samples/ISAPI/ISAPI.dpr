library ISAPI;

uses Web.Win.ISAPIApp, Horse.ISAPI, Horse.API;

{$R *.res}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

var
  App: THorse;

begin
  App := THorse.Create;

  App.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('pong');
    end);

  App.Start;
end.
