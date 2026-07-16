program console_client;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.Net.HttpClient;

type
  TChunkReceivedProc = reference to procedure(const AChunk: string);

  TStreamingReceiveStream = class(TStream)
  private
    FOnChunkReceived: TChunkReceivedProc;
  public
    constructor Create(const AOnChunkReceived: TChunkReceivedProc);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure SetSize(NewSize: Longint); override;
  end;

constructor TStreamingReceiveStream.Create(const AOnChunkReceived: TChunkReceivedProc);
begin
  inherited Create;
  FOnChunkReceived := AOnChunkReceived;
end;

function TStreamingReceiveStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;

function TStreamingReceiveStream.Write(const Buffer; Count: Longint): Longint;
var
  LText: string;
  LBytes: TBytes;
begin
  Result := Count;
  if Count <= 0 then Exit;

  // Copia os dados para um array de bytes para decodificação segura
  SetLength(LBytes, Count);
  Move(Buffer, LBytes[0], Count);
  LText := TEncoding.UTF8.GetString(LBytes);
  
  if Assigned(FOnChunkReceived) then
    FOnChunkReceived(LText);
end;

function TStreamingReceiveStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := 0;
end;

procedure TStreamingReceiveStream.SetSize(NewSize: Longint);
begin
end;

procedure ConsumeStream(const AURL: string; const ALabel: string);
var
  LClient: THTTPClient;
  LStream: TStreamingReceiveStream;
  LRes: IHTTPResponse;
begin
  Writeln('--------------------------------------------------');
  Writeln(' Iniciando leitura do Stream de: ' + ALabel);
  Writeln(' URL: ' + AURL);
  Writeln('--------------------------------------------------');
  
  LClient := THTTPClient.Create;
  LStream := TStreamingReceiveStream.Create(
    procedure(const AChunk: string)
    begin
      // Imprime o pedaço na tela assim que ele chega da rede
      Write(AChunk);
    end);
  try
    try
      LRes := LClient.Get(AURL, LStream);
      Writeln;
      Writeln('-> Conexão fechada com status: ', LRes.StatusCode);
    except
      on E: Exception do
      begin
        Writeln;
        Writeln('-> Erro ao consumir stream: ', E.Message);
      end;
    end;
  finally
    LStream.Free;
    LClient.Free;
  end;
  Writeln;
end;

begin
  try
    Writeln('==================================================');
    Writeln('     Horse Streaming Native Delphi Client');
    Writeln('==================================================');
    Writeln;
    Writeln('Certifique-se de que o servidor console_streaming está rodando.');
    Writeln('Pressione ENTER para iniciar a leitura dos Web Streams...');
    Readln;

    // Consome Web Stream NDJSON
    ConsumeStream('http://localhost:9000/ndjson', 'Web Streams (NDJSON)');

    Writeln('Pressione ENTER para iniciar a leitura dos Server-Sent Events (SSE)...');
    Readln;

    // Consome Server-Sent Events (SSE)
    ConsumeStream('http://localhost:9000/sse', 'Server-Sent Events (SSE)');

    Writeln('Consumo finalizado. Pressione ENTER para sair.');
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
