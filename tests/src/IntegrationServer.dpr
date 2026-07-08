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
  Horse, Horse.Jhonson, Horse.Commons;

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

procedure PostEcho(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
var
  LJSON: TJSONObject;
begin
  LJSON := Req.Body<TJSONObject>;
  Res.Send<TJSONObject>(LJSON);
end;

type
  TTestThread = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TTestThread.Execute;
begin
  try
    THorse.Listen(PORT);
  except
    on E: Exception do
      Writeln('Server Exception: ', E.Message);
  end;
end;

var
  LServerThread: TTestThread;
  {$IFDEF FPC}
  LClient: TFPHTTPClient;
  {$ELSE}
  LClient: THTTPClient;
  {$ENDIF}
  LRes: string;
  LPostData: TStringStream;
  LSuccess: Boolean;
begin
  Writeln('Starting integration test server...');
  THorse.Use(Jhonson);
  
  THorse.Get('/ping', GetPing);
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
  LClient.AddHeader('Content-Type', 'application/json');
  {$ELSE}
  LClient := THTTPClient.Create;
  {$ENDIF}
  try
    try
      Writeln('Testing GET /ping...');
      {$IFDEF FPC}
      LRes := LClient.Get('http://127.0.0.1:9999/ping');
      {$ELSE}
      LRes := LClient.Get('http://127.0.0.1:9999/ping').ContentAsString;
      {$ENDIF}
      Writeln('GET /ping Response: ', LRes);
      if not LRes.Contains('pong') then
        raise Exception.Create('GET /ping did not return pong');

      Writeln('Testing POST /echo...');
      LPostData := TStringStream.Create('{"input":"hello"}', TEncoding.UTF8);
      try
        {$IFDEF FPC}
        LClient.RequestBody := LPostData;
        LRes := LClient.Post('http://127.0.0.1:9999/echo');
        {$ELSE}
        LRes := LClient.Post('http://127.0.0.1:9999/echo', LPostData).ContentAsString;
        {$ENDIF}
        Writeln('POST /echo Response: ', LRes);
        if not LRes.Contains('hello') then
          raise Exception.Create('POST /echo did not echo');
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
    Writeln('INTEGRATION TEST: SUCCESS');
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
