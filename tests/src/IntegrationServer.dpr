program IntegrationServer;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

uses
  {$IFDEF FPC}
    {$IFDEF UNIX}
    cthreads,
    {$ENDIF}
  SysUtils, Classes, fpjson, jsonparser, fphttpclient,
  {$ELSE}
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient,
  {$ENDIF}
  Horse, Horse.Core, Horse.Jhonson, Horse.CORS, Horse.BasicAuthentication, Horse.Commons, Horse.Core.Group.Contract;

const
  PORT = 9999;

procedure GetPing(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
var
  LJSON: TJSONObject;
begin
  LJSON := TJSONObject.Create;
  {$IFDEF FPC}
  LJSON.Add('message', 'pong');
  {$ELSE}
  LJSON.AddPair('message', 'pong');
  {$ENDIF}
  Res.Send<TJSONObject>(LJSON);
end;

procedure GetPrivate(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send('private-ok');
end;

procedure PostEcho(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
var
  LJSON: TJSONObject;
begin
  LJSON := Req.Body<TJSONObject>;
  Res.Send<TJSONObject>(LJSON);
end;

function MyAuth(const AUsername, APassword: string): Boolean;
begin
  Result := (AUsername = 'admin') and (APassword = 'secret');
end;

type
  TTestThread = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TTestThread.Execute;
begin
  try
    THorse.Listen(PORT, '127.0.0.1');
  except
    on E: Exception do
      Writeln('Server Exception: ', E.Message);
  end;
end;

var
  LServerThread: TTestThread;
  LGroup: IHorseCoreGroup<THorseCore>;
  {$IFDEF FPC}
  LClient: TFPHTTPClient;
  {$ELSE}
  LClient: THTTPClient;
  {$ENDIF}
  LRes: string;
  LPostData: TStringStream;
  LSuccess: Boolean;
  LStatusCode: Integer;
begin
  Writeln('Starting integration test server...');
  
  // Registra middlewares globais
  THorse.Use(CORS);
  THorse.Use(Jhonson);
  
  THorse.Get('/ping', GetPing);
  
  LGroup := THorse.Group.Prefix('/secure');
  LGroup.Use(HorseBasicAuthentication(MyAuth));
  LGroup.Get('/private', GetPrivate);

  THorse.Post('/echo', PostEcho);

  LServerThread := TTestThread.Create(True);
  LServerThread.FreeOnTerminate := True;
  LServerThread.Start;

  Writeln('Waiting for server to start...');
  Sleep(1500);

  LSuccess := False;
  Writeln('Creating HTTP client...');
  {$IFDEF FPC}
  LClient := TFPHTTPClient.Create(nil);
  {$ELSE}
  LClient := THTTPClient.Create;
  {$ENDIF}
  try
    try
      // 1. Testa Rota Pública /ping (GET)
      Writeln('Testing GET /ping (CORS + Johnson)...');
      {$IFDEF FPC}
      LClient.AddHeader('Content-Type', 'application/json');
      LRes := LClient.Get('http://127.0.0.1:9999/ping');
      LStatusCode := LClient.ResponseStatusCode;
      {$ELSE}
      with LClient.Get('http://127.0.0.1:9999/ping') do
      begin
        LRes := ContentAsString;
        LStatusCode := StatusCode;
      end;
      {$ENDIF}
      Writeln('GET /ping Response: ', LRes, ' [Status: ', LStatusCode, ']');
      if (LStatusCode <> 200) or (not LRes.Contains('pong')) then
        raise Exception.Create('GET /ping failed');

      // 2. Testa Rota Privada /private sem credenciais (esperado 401 Unauthorized)
      Writeln('Testing GET /private without credentials (Authentication)...');
      {$IFDEF FPC}
      LClient.RequestHeaders.Clear;
      try
        LClient.Get('http://127.0.0.1:9999/secure/private');
      except
        // fphttpclient lança exceção em códigos de erro HTTP, mas podemos ler o status
      end;
      LStatusCode := LClient.ResponseStatusCode;
      {$ELSE}
      LStatusCode := LClient.Get('http://127.0.0.1:9999/secure/private').StatusCode;
      {$ENDIF}
      Writeln('GET /private (No Auth) Status: ', LStatusCode);
      if LStatusCode <> 401 then
        raise Exception.Create('GET /private should return 401 Unauthorized');

      // 3. Testa Rota Privada /private com credenciais corretas (esperado 200 OK)
      Writeln('Testing GET /private with correct credentials (Authentication)...');
      {$IFDEF FPC}
      LClient.RequestHeaders.Clear;
      // admin:secret -> Base64 = YWRtaW46c2VjcmV0
      LClient.AddHeader('Authorization', 'Basic YWRtaW46c2VjcmV0');
      LRes := LClient.Get('http://127.0.0.1:9999/secure/private');
      LStatusCode := LClient.ResponseStatusCode;
      {$ELSE}
      // Configurar no client Delphi se necessário, mas para simplificar no FPC local:
      // No Delphi THTTPClient:
      // O Delphi HTTPClient usa Request.Headers ou Credentials
      {$ENDIF}
      Writeln('GET /private (With Auth) Response: ', LRes, ' [Status: ', LStatusCode, ']');
      if LStatusCode <> 200 then
        raise Exception.Create('GET /private with Auth failed');

      // 4. Testa POST /echo (Johnson Body Parser)
      Writeln('Testing POST /echo (Body Parser)...');
      LPostData := TStringStream.Create('{"input":"hello"}', TEncoding.UTF8);
      try
        {$IFDEF FPC}
        LClient.RequestHeaders.Clear;
        LClient.AddHeader('Content-Type', 'application/json');
        LClient.RequestBody := LPostData;
        LRes := LClient.Post('http://127.0.0.1:9999/echo');
        LStatusCode := LClient.ResponseStatusCode;
        {$ELSE}
        // ...
        {$ENDIF}
        Writeln('POST /echo Response: ', LRes, ' [Status: ', LStatusCode, ']');
        if (LStatusCode <> 200) or (not LRes.Contains('hello')) then
          raise Exception.Create('POST /echo failed');
      finally
        LPostData.Free;
      end;

      LSuccess := True;
    except
      on E: Exception do
        Writeln('HTTP Client Exception: ', E.Message);
    end;
  finally
    LClient.Free;
  end;

  Writeln('Stopping server...');
  {$IFNDEF FPC}
  THorse.StopListen;
  {$ENDIF}
  Sleep(500);

  if LSuccess then
  begin
    Writeln('===================================');
    Writeln('INTEGRATION TEST: SUCCESS (ALL MIDDLEWARES)');
    Writeln('===================================');
    ExitCode := 0;
  end
  else
  begin
    Writeln('===================================');
    Writeln('INTEGRATION TEST: FAILED');
    Writeln('===================================');
    ExitCode := 1;
  end;
end.
