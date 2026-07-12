# Pool de Conexões de Banco de Dados (Thread-Safety)

Este guia orienta sobre como configurar e gerenciar conexões com bancos de dados de forma segura e eficiente em aplicações multithreaded desenvolvidas com o framework **Horse**.

---

## 1. A Regra de Ouro em Aplicações Multithreaded

O Horse gerencia cada requisição HTTP recebida em uma thread separada de forma assíncrona. Por conta disso:

> [!CAUTION]
> **NUNCA compartilhe componentes de conexão de banco de dados (como um `TFDConnection` ou `TUniConnection` global/estático) entre diferentes requisições.**
> Compartilhar a mesma instância de conexão causará condições de corrida (*race conditions*), corrupção de memória e as temidas exceções de Violação de Acesso (*Access Violations*).

### Regra de Arquitetura:
Cada thread de requisição deve obter sua própria instância de conexão física com o banco de dados (ou requisitar uma conexão ativa de um pool de conexões thread-safe) e liberá-la imediatamente após a conclusão da resposta.

---

## 2. Pool de Conexões com FireDAC (Nativo)

O FireDAC possui um gerenciador de pools extremamente robusto e de alta performance. Siga as fases abaixo para configurá-lo:

### Fase A: Definição Global do Pool (Bootstrap)
Configure a definição de conexão global do pool no arquivo `.dpr` ou na classe de inicialização da aplicação, utilizando o `TFDManager`.

```pascal
uses
  FireDAC.Stan.Intf, FireDAC.Phys.PG, FireDAC.Comp.Client, System.Classes;

procedure SetupConnectionPool;
var
  LParams: TStrings;
begin
  LParams := TStringList.Create;
  try
    LParams.Values['DriverID'] := 'PG'; // Exemplo com PostgreSQL
    LParams.Values['Server'] := 'localhost';
    LParams.Values['Database'] := 'minha_api_db';
    LParams.Values['User_Name'] := 'postgres';
    LParams.Values['Password'] := 'segredo';
    
    // CRITICAL: Ativa o pooling de conexões
    LParams.Values['Pooled'] := 'True';
    
    // Limites de tamanho do pool
    LParams.Values['POOL_MaximumItems'] := '50';
    LParams.Values['POOL_CleanupTimeout'] := '30000'; // 30s
    
    // Registra a definição no gerenciador global
    FDManager.AddConnectionDef('MyPooledPGDef', 'PG', LParams);
  finally
    LParams.Free;
  end;
end;
```

### Fase B: Uso no Handler de Rota
No handler da rota, crie a conexão localmente e aponte para a definição criada no pool. O FireDAC gerenciará a aquisição física e devolução segura para o pool quando a conexão for destruída.

```pascal
procedure GetUsuariosHandler(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LConnection: TFDConnection;
  LQuery: TFDQuery;
begin
  LConnection := TFDConnection.Create(nil);
  LQuery := TFDQuery.Create(nil);
  try
    // Referencia a conexão poolada. Isso puxa uma conexão do pool instantaneamente
    LConnection.ConnectionDefName := 'MyPooledPGDef';
    LConnection.Connected := True;
    
    LQuery.Connection := LConnection;
    LQuery.SQL.Text := 'SELECT id, nome FROM usuarios';
    LQuery.Open;
    
    Res.Send(LQuery.ToJSONArray); // O middleware Johnson cuidará do envio e liberação do JSON
  finally
    LQuery.Free;
    LConnection.Free; // Libera e devolve a conexão de volta ao pool automaticamente
  end;
end;
```

---

## 3. Pool de Conexões com UniDAC (Devart)

Se você utiliza o Devart UniDAC, a configuração do pool é feita diretamente nas opções da classe de conexão:

```pascal
procedure GetClientesUniDACHandler(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LConnection: TUniConnection;
  LQuery: TUniQuery;
begin
  LConnection := TUniConnection.Create(nil);
  LQuery := TUniQuery.Create(nil);
  try
    LConnection.ProviderName := 'PostgreSQL';
    LConnection.Server := 'localhost';
    LConnection.Database := 'minha_api_db';
    LConnection.Username := 'postgres';
    LConnection.Password := 'segredo';
    
    // Configura e habilita o pooling de conexões
    LConnection.Pooling := True;
    LConnection.PoolingOptions.MaxPoolSize := 50;
    LConnection.PoolingOptions.ConnectionLifetime := 60; // segundos
    
    LConnection.Connect; // Pega conexão ativa ou cria uma nova se abaixo do limite
    
    LQuery.Connection := LConnection;
    LQuery.SQL.Text := 'SELECT id, razao_social FROM clientes';
    LQuery.Open;
    
    Res.Send(LQuery.ToJSONArray);
  finally
    LQuery.Free;
    LConnection.Disconnect; // Libera a conexão mantendo-a disponível no pool do UniDAC
    LConnection.Free;
  end;
end;
```

---

## 4. Integração com IoC no Escopo de Requisição (`Req.Services`)

Para arquiteturas limpas baseadas em serviços e injeção de dependência, o Horse disponibiliza um contêiner no escopo de cada requisição. Isso garante que o serviço de conexão seja criado sob demanda (Lazy) e liberado automaticamente no fim da requisição.

### Passo 1: Registrar o Serviço Local
Crie um middleware ou use o hook `onRequest` para injetar a conexão no escopo do request:

```pascall
THorse.Use(
  procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
  var
    LConnection: TFDConnection;
  begin
    LConnection := TFDConnection.Create(nil);
    LConnection.ConnectionDefName := 'MyPooledPGDef';
    
    // Injeta a conexão no contêiner de serviços.
    // Ela será destruída automaticamente pelo Horse no final da requisição!
    Req.Services.Add(TFDConnection, LConnection);
    Next;
  end
);
```

### Passo 2: Resolver a Conexão nos Controllers
Nos seus controllers ou regras de negócio, resolva a dependência da conexão de forma transparente sem precisar criá-la manualmente:

```pascal
procedure GetRelatorioHandler(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LConnection: TFDConnection;
  LQuery: TFDQuery;
begin
  // Obtém a conexão injetada no início da requisição
  LConnection := Req.Services.Resolve(TFDConnection) as TFDConnection;
  
  LQuery := TFDQuery.Create(nil);
  try
    LQuery.Connection := LConnection;
    LQuery.SQL.Text := 'SELECT * FROM vendas';
    LQuery.Open;
    Res.Send(LQuery.ToJSONArray);
  finally
    LQuery.Free;
    // NÃO libere o LConnection aqui! O Horse gerencia e liberará no fim da requisição.
  end;
end;
```
