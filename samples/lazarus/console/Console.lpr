program Console;

{$MODE DELPHI}{$H+}

uses
  Horse, Horse.BasicAuthentication, Horse.Cors, Horse.Jhonson, Horse.OctetStream, Horse.HandleException, SysUtils, Classes;

procedure GetPing(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  raise EHorseException.Create('teste',400);
  //Res.Send('pong');
end;

procedure PostMarco(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
var
  LStream: TMemoryStream;
begin
  LStream :=  Req.Body<TMemoryStream>;
  LStream.SaveToFile('c:\sample\demo2.jpg');
  //writeln( LReqBody.DataString );
  //LStream := TFileStream.Create('c:\sample\demo.txt', fmOpenRead);
  //Res.Send<TStream>(LStream);
end;

procedure OnListen(Horse: THorse);
begin
  Writeln(Format('Server is runing on %s:%d', [Horse.Host, Horse.Port]));
end;

function HorseBasicAuthenticationCallback(const AUsername, APassword: string): Boolean;
begin
  Result := AUsername.Equals('user') and APassword.Equals('password');
end;

begin

  THorse.Use(HorseBasicAuthentication(HorseBasicAuthenticationCallback));
  THorse.Use(CORS);
  THorse.Use(OctetStream);
  THorse.Use(Jhonson);
  THorse.Use(HandleException);


  THorse.Get('ping', GetPing);
  THorse.Post('marco', PostMarco);

  THorse.Listen(9000, OnListen);

end.
