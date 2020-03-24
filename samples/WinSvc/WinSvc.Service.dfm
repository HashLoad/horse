object WinService: TWinService
  OldCreateOrder = False
  OnCreate = ServiceCreate
  OnDestroy = ServiceDestroy
  AllowPause = False
  DisplayName = 'WinService'
  StartType = stManual
  OnShutdown = ServiceShutdown
  OnStart = ServiceStart
  Height = 150
  Width = 215
end
