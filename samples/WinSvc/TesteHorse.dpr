program TesteHorse;

uses
  Vcl.SvcMgr,
  TesteHorse.Servico in 'TesteHorse.Servico.pas' {Service1: TService},
  Horse.Commons in '..\..\LIBs\horse-master\src\Horse.Commons.pas',
  Horse.Constants in '..\..\LIBs\horse-master\src\Horse.Constants.pas',
  Horse.Core in '..\..\LIBs\horse-master\src\Horse.Core.pas',
  Horse.HTTP in '..\..\LIBs\horse-master\src\Horse.HTTP.pas',
  Horse.ISAPI in '..\..\LIBs\horse-master\src\Horse.ISAPI.pas',
  Horse.Router in '..\..\LIBs\horse-master\src\Horse.Router.pas',
  Horse.WebModule in '..\..\LIBs\horse-master\src\Horse.WebModule.pas' {HorseWebModule: TWebModule},
  HorseWS in '..\..\LIBs\horse-master\src\HorseWS.pas',
  Horse in '..\..\LIBs\horse-master\src\Horse.pas';

{$R *.RES}

begin
  // Windows 2003 Server requires StartServiceCtrlDispatcher to be
  // called before CoRegisterClassObject, which can be called indirectly
  // by Application.Initialize. TServiceApplication.DelayInitialize allows
  // Application.Initialize to be called from TService.Main (after
  // StartServiceCtrlDispatcher has been called).
  //
  // Delayed initialization of the Application object may affect
  // events which then occur prior to initialization, such as
  // TService.OnCreate. It is only recommended if the ServiceApplication
  // registers a class object with OLE and is intended for use with
  // Windows 2003 Server.
  //
  // Application.DelayInitialize := True;
  //
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TService1, Service1);
  Application.Run;
end.
