program TesteHorse;

{$MODE DELPHI}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Horse, Horse.Controller, Horse.Commons, ControllerTeste;

begin
  // Mapeamento e Registro Automático (Sintaxe idêntica ao Delphi)
  THorseController<TControllerTeste>.Map('/users', mtGet, 'ListUsers');
  THorseController<TControllerTeste>.Map('/users/:id', mtGet, 'GetUserById');

  THorse.Listen(9001);
end.
