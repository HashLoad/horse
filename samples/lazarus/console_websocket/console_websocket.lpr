program console_websocket;

{$MODE DELPHI}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  Horse,
  Horse.Core.WebSocket;

const
  HTML_CHAT =
    '<!DOCTYPE html>' +
    '<html lang="pt-BR">' +
    '<head>' +
    '  <meta charset="UTF-8">' +
    '  <meta name="viewport" content="width=device-width, initial-scale=1.0">' +
    '  <title>Horse WebSockets Chat</title>' +
    '  <link href="https://fonts.googleapis.com/css2?family=Outfit:wght@300;400;600;800&display=swap" rel="stylesheet">' +
    '  <style>' +
    '    :root {' +
    '      --bg: #0f0c1b;' +
    '      --panel: rgba(25, 20, 45, 0.45);' +
    '      --border: rgba(255, 255, 255, 0.08);' +
    '      --primary: #8a2be2;' +
    '      --primary-hover: #9d4edd;' +
    '      --text: #e0dcf0;' +
    '      --text-muted: #8c85a8;' +
    '      --success: #00f5d4;' +
    '    }' +
    '    * { box-sizing: border-box; margin: 0; padding: 0; font-family: ''Outfit'', sans-serif; }' +
    '    body {' +
    '      background: radial-gradient(circle at 50% 50%, #1e133a 0%, var(--bg) 100%);' +
    '      color: var(--text);' +
    '      min-height: 100vh;' +
    '      display: flex;' +
    '      justify-content: center;' +
    '      align-items: center;' +
    '      padding: 20px;' +
    '      overflow: hidden;' +
    '    }' +
    '    .chat-container {' +
    '      width: 100%;' +
    '      max-width: 500px;' +
    '      height: 600px;' +
    '      background: var(--panel);' +
    '      backdrop-filter: blur(16px);' +
    '      -webkit-backdrop-filter: blur(16px);' +
    '      border: 1px solid var(--border);' +
    '      border-radius: 24px;' +
    '      box-shadow: 0 20px 50px rgba(0, 0, 0, 0.5);' +
    '      display: flex;' +
    '      flex-direction: column;' +
    '      overflow: hidden;' +
    '      animation: fadeIn 0.8s ease;' +
    '    }' +
    '    @keyframes fadeIn {' +
    '      from { opacity: 0; transform: translateY(20px); }' +
    '      to { opacity: 1; transform: translateY(0); }' +
    '    }' +
    '    .header {' +
    '      padding: 20px;' +
    '      border-bottom: 1px solid var(--border);' +
    '      display: flex;' +
    '      align-items: center;' +
    '      justify-content: space-between;' +
    '    }' +
    '    .logo { display: flex; align-items: center; gap: 10px; }' +
    '    .logo-icon {' +
    '      width: 32px;' +
    '      height: 32px;' +
    '      background: linear-gradient(135deg, var(--primary), var(--primary-hover));' +
    '      border-radius: 8px;' +
    '      display: flex;' +
    '      align-items: center;' +
    '      justify-content: center;' +
    '      font-weight: 800;' +
    '      color: #fff;' +
    '    }' +
    '    .logo-text { font-size: 1.25rem; font-weight: 600; letter-spacing: -0.5px; }' +
    '    .status { display: flex; align-items: center; gap: 8px; font-size: 0.85rem; color: var(--text-muted); }' +
    '    .status-dot { width: 8px; height: 8px; background: #ff4d4d; border-radius: 50%; transition: background 0.3s; }' +
    '    .status-dot.connected { background: var(--success); box-shadow: 0 0 10px var(--success); }' +
    '    .messages-area {' +
    '      flex: 1;' +
    '      padding: 20px;' +
    '      overflow-y: auto;' +
    '      display: flex;' +
    '      flex-direction: column;' +
    '      gap: 15px;' +
    '      scroll-behavior: smooth;' +
    '    }' +
    '    .message {' +
    '      max-width: 80%;' +
    '      padding: 12px 16px;' +
    '      border-radius: 16px;' +
    '      line-height: 1.4;' +
    '      font-size: 0.95rem;' +
    '      animation: popMessage 0.3s cubic-bezier(0.18, 0.89, 0.32, 1.28);' +
    '    }' +
    '    @keyframes popMessage {' +
    '      from { opacity: 0; transform: scale(0.9) translateY(10px); }' +
    '      to { opacity: 1; transform: scale(1) translateY(0); }' +
    '    }' +
    '    .message.received {' +
    '      background: rgba(255, 255, 255, 0.05);' +
    '      border: 1px solid rgba(255, 255, 255, 0.03);' +
    '      align-self: flex-start;' +
    '      border-bottom-left-radius: 4px;' +
    '    }' +
    '    .message.sent {' +
    '      background: linear-gradient(135deg, var(--primary), var(--primary-hover));' +
    '      align-self: flex-end;' +
    '      border-bottom-right-radius: 4px;' +
    '      box-shadow: 0 8px 20px rgba(138, 43, 226, 0.2);' +
    '    }' +
    '    .message.system {' +
    '      background: transparent;' +
    '      color: var(--text-muted);' +
    '      font-size: 0.8rem;' +
    '      align-self: center;' +
    '      text-align: center;' +
    '      padding: 5px;' +
    '    }' +
    '    .input-area {' +
    '      padding: 20px;' +
    '      border-top: 1px solid var(--border);' +
    '      display: flex;' +
    '      gap: 10px;' +
    '    }' +
    '    input {' +
    '      flex: 1;' +
    '      background: rgba(255, 255, 255, 0.04);' +
    '      border: 1px solid var(--border);' +
    '      border-radius: 12px;' +
    '      padding: 12px 16px;' +
    '      color: var(--text);' +
    '      outline: none;' +
    '      font-size: 0.95rem;' +
    '      transition: all 0.3s;' +
    '    }' +
    '    input:focus { border-color: var(--primary); box-shadow: 0 0 10px rgba(138, 43, 226, 0.3); }' +
    '    button {' +
    '      background: linear-gradient(135deg, var(--primary), var(--primary-hover));' +
    '      border: none;' +
    '      border-radius: 12px;' +
    '      padding: 12px 20px;' +
    '      color: #fff;' +
    '      font-weight: 600;' +
    '      cursor: pointer;' +
    '      transition: all 0.3s;' +
    '    }' +
    '    button:hover { transform: translateY(-2px); box-shadow: 0 8px 20px rgba(138, 43, 226, 0.4); }' +
    '    button:active { transform: translateY(0); }' +
    '  </style>' +
    '</head>' +
    '<body>' +
    '  <div class="chat-container">' +
    '    <div class="header">' +
    '      <div class="logo">' +
    '        <div class="logo-icon">H</div>' +
    '        <div class="logo-text">Horse WebSockets</div>' +
    '      </div>' +
    '      <div class="status">' +
    '        <div class="status-dot" id="statusDot"></div>' +
    '        <span id="statusText">Desconectado</span>' +
    '      </div>' +
    '    </div>' +
    '    <div class="messages-area" id="messagesArea"></div>' +
    '    <div class="input-area">' +
    '      <input type="text" id="messageInput" placeholder="Digite uma mensagem..." disabled>' +
    '      <button id="sendBtn" disabled>Enviar</button>' +
    '    </div>' +
    '  </div>' +
    '  <script>' +
    '    const statusDot = document.getElementById(''statusDot'');' +
    '    const statusText = document.getElementById(''statusText'');' +
    '    const messagesArea = document.getElementById(''messagesArea'');' +
    '    const messageInput = document.getElementById(''messageInput'');' +
    '    const sendBtn = document.getElementById(''sendBtn'');' +
    '    let ws;' +
    '    function addMessage(text, type) {' +
    '      const msgDiv = document.createElement(''div'');' +
    '      msgDiv.className = `message ${type}`;' +
    '      msgDiv.innerText = text;' +
    '      messagesArea.appendChild(msgDiv);' +
    '      messagesArea.scrollTop = messagesArea.scrollHeight;' +
    '    }' +
    '    function connect() {' +
    '      const wsUrl = (window.location.protocol === ''https:'' ? ''wss://'' : ''ws://'') + window.location.host + ''/ws'';' +
    '      ws = new WebSocket(wsUrl);' +
    '      ws.onopen = () => {' +
    '        statusDot.className = ''status-dot connected'';' +
    '        statusText.innerText = ''Conectado'';' +
    '        messageInput.disabled = false;' +
    '        sendBtn.disabled = false;' +
    '        messageInput.focus();' +
    '        addMessage(''Conexão estabelecida com o Horse WebSocket.'', ''system'');' +
    '      };' +
    '      ws.onmessage = (event) => {' +
    '        addMessage(event.data, ''received'');' +
    '      };' +
    '      ws.onclose = () => {' +
    '        statusDot.className = ''status-dot'';' +
    '        statusText.innerText = ''Desconectado'';' +
    '        messageInput.disabled = true;' +
    '        sendBtn.disabled = true;' +
    '        addMessage(''Conexão encerrada pelo servidor. Tentando reconectar em 3s...'', ''system'');' +
    '        setTimeout(connect, 3000);' +
    '      };' +
    '    }' +
    '    function sendMessage() {' +
    '      const text = messageInput.value.trim();' +
    '      if (text) {' +
    '        ws.send(text);' +
    '        addMessage(text, ''sent'');' +
    '        messageInput.value = '''';' +
    '      }' +
    '    }' +
    '    sendBtn.onclick = sendMessage;' +
    '    messageInput.onkeypress = (e) => {' +
    '      if (e.key === ''Enter'') sendMessage();' +
    '    };' +
    '    connect();' +
    '  </script>' +
    '</body>' +
    '</html>';

procedure HandleGetIndex(Req: THorseRequest; Res: THorseResponse);
begin
  Res.ContentType('text/html').Send(HTML_CHAT);
end;

procedure OnWebSocketMessage(const AConnection: IHorseWebSocketConnection; const AText: string);
begin
  Writeln('Mensagem recebida de ', AConnection.GetClientIP, ': ', AText);
  // Transmite a mensagem para todos os outros clientes na mesma rota (/ws)
  THorseWebSocketManager.Broadcast('/ws', 'Anonimo (' + AConnection.GetClientIP + '): ' + AText);
end;

procedure OnWebSocketDisconnect(const AConnection: IHorseWebSocketConnection);
begin
  Writeln('Conexão WebSocket encerrada. IP: ', AConnection.GetClientIP);
end;

procedure OnWebSocketError(const AConnection: IHorseWebSocketConnection; const AException: Exception);
begin
  Writeln('Erro na conexão WebSocket (', AConnection.GetClientIP, '): ', AException.Message);
end;

procedure OnWebSocketConnect(const AConn: IHorseWebSocketConnection);
begin
  Writeln('Nova conexão WebSocket aberta. IP: ', AConn.GetClientIP);
  AConn.SetOnMessage(OnWebSocketMessage);
  AConn.SetOnDisconnect(OnWebSocketDisconnect);
  AConn.SetOnError(OnWebSocketError);
end;

procedure HandleGetWS(Req: THorseRequest; Res: THorseResponse);
begin
  Res.UpgradeToWebSocket(OnWebSocketConnect);
end;

begin
  THorse.Get('/', HandleGetIndex);
  THorse.Get('/ws', HandleGetWS);

  Writeln('Servidor Horse WebSocket rodando em http://localhost:9000');
  THorse.Listen(9000);
end.
