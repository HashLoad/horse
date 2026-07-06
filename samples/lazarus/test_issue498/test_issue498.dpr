program test_issue498;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  Horse,
  Horse.Proc,
  Horse.Jhonson,
  Horse.Compression,
  Horse.BasicAuthentication,
  Horse.CORS,
  Horse.OctetStream;

function MyAuthenticate(const AUser, APass: string): Boolean;
begin
  Result := (AUser = 'user') and (APass = 'pass');
end;

procedure GetPing(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send('pong');
end;

procedure MyCORS(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  CORS(Req, Res, Next);
end;

procedure MyOctetStream(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  OctetStream(Req, Res, Next);
end;

begin
  // Inicialização com os 5 middlewares oficiais
  THorse.Use(Jhonson());
  THorse.Use(Compression());

  {$IFDEF FPC}
  THorse.Use(HorseBasicAuthentication(MyAuthenticate));
  {$ELSE}
  THorse.Use(HorseBasicAuthentication(
    function(const AUsername, APassword: string): Boolean
    begin
      Result := MyAuthenticate(AUsername, APassword);
    end));
  {$ENDIF}

  THorse.Use(MyCORS);
  THorse.Use(MyOctetStream);

  THorse.Get('/ping', GetPing);

  Writeln('Servidor ativo!');
end.
