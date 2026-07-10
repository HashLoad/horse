unit Tests.CleanupHelper;

interface

uses
  Horse, Horse.Core, Horse.Core.RouterTree, Horse.Core.Router.Radix, System.SysUtils, System.Rtti,
  System.Generics.Collections;

procedure ClearGlobalState;

implementation

procedure ClearGlobalState;
var
  LContext: TRttiContext;
  LType: TRttiInstanceType;
  LField: TRttiField;
  LList: TList<THorseCallback>;
begin
  // 1. Para a escuta do servidor
  THorse.StopListen;

  // 2. Reseta a arvore de rotas global
  THorse.Routes := nil;
  {$IFDEF HORSE_RADIX_ROUTER}
  THorse.Routes := THorseRadixRouter.Create;
  {$ELSE}
  THorse.Routes := THorseRouterTree.Create;
  {$ENDIF}

  // 3. Reseta propriedades estaticas de rede para o baseline padrao
  THorse.Port := 9000;
  THorse.Host := '0.0.0.0';
  THorse.MaxConnections := 0;

  // 4. Limpa todos os ganchos registrados de forma nativa e estática
  THorseCore.ResetHooks;

  // 5. Limpa a lista privada de middlewares globais (FCallbacks) no THorseCore e THorse via RTTI
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(THorseCore) as TRttiInstanceType;
    if Assigned(LType) then
    begin
      LField := LType.GetField('FCallbacks');
      if Assigned(LField) then
      begin
        LList := TList<THorseCallback>(LField.GetValue(nil).AsObject);
        if Assigned(LList) then
          LList.Clear;
      end;
    end;

    LType := LContext.FindType('Horse.THorse') as TRttiInstanceType;
    if Assigned(LType) then
    begin
      LField := LType.GetField('FCallbacks');
      if Assigned(LField) then
      begin
        LList := TList<THorseCallback>(LField.GetValue(nil).AsObject);
        if Assigned(LList) then
          LList.Clear;
      end;
    end;
  finally
    LContext.Free;
  end;
end;

end.
