object Service1: TService1
  OldCreateOrder = False
  OnCreate = ServiceCreate
  OnDestroy = ServiceDestroy
  AllowPause = False
  DisplayName = 'Teste Servi'#231'o Horse'
  StartType = stManual
  OnShutdown = ServiceShutdown
  OnStart = ServiceStart
  Height = 150
  Width = 215
end
