program ConsoleBasicAuth;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Horse in '..\..\..\src\Horse.pas',
  System.SysUtils,
  Horse.BasicAuthentication in 'horse-basic-auth\src\Horse.BasicAuthentication.pas';

const
  cVersionAPI = 'v1';
  cAPIName = 'xpos';

function CheckLogin(const AUsername, APassword: string): Boolean;
begin
  Result := FALSE;
end;

procedure DoLogin(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send('DoLogin');
end;

procedure DoLogin2(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send('DoLogin');
end;

procedure DoLogout(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send('DoLogout');
end;

procedure GetPing(Req: THorseRequest; Res: THorseResponse);
begin
  Res.Send('Pong');
end;


begin


  {$IFDEF MSWINDOWS}
    IsConsole := False;
    ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  try
    THorse
      .Group
      .Prefix(cVersionAPI+'/'+cAPIName)
      .Delete('/logout', DoLogout)
      .AddCallback(HorseBasicAuthentication(CheckLogin))
      .Get('/login',DoLogin)
      .Get('/ping', GetPing );
      //.Get('/login',DoLogin2);
    THorse.Listen(9000,
      procedure
      begin
        Writeln(Format('Server is runing on port %d...', [THorse.Port]));
        Readln;
      end);

  except
   on E: Exception do
      Writeln('Route configuration error: ', E.Message);
  end;

{
  THorse.Get('/pong',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send('Ping');
    end);

  THorse.Listen(9000,
    procedure
    begin
      Writeln(Format('Server is runing on port %d...', [THorse.Port]));
      Readln;
    end);
}
end.
