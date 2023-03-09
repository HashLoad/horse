object MainService: TMainService
  OnCreate = ServiceCreate
  DisplayName = 'MainService'
  StartType = stManual
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
