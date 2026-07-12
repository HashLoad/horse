# Segurança & Autenticação

Este guia orienta sobre como implementar mecanismos robustos de segurança e autenticação em APIs desenvolvidas com o framework **Horse**, utilizando os middlewares oficiais da organização.

---

## 1. Organização de Rotas Públicas e Protegidas

A primeira regra de segurança em APIs Web é separar claramente quais recursos são públicos (como telas de login, termos de serviço ou endpoints de health check) e quais exigem autorização.

O Horse permite estruturar essa separação de forma limpa usando a propriedade `THorse.Group` (Grupos de Rotas). Isso garante que o middleware de segurança seja aplicado apenas nos caminhos protegidos.

```pascal
begin
  // 1. Middlewares Globais de Infraestrutura (CORS, Johnson, etc.)
  THorse.Use(CORS).Use(Jhonson);

  // 2. Rotas Públicas (Sem necessidade de autenticação)
  THorse.Post('/login', DoLoginHandler);
  THorse.Get('/health', GetHealthHandler);

  // 3. Rotas Protegidas (Agrupadas sob o middleware JWT)
  THorse.Group.Prefix('/api/v1')
    .Use(HorseJWT('sua_chave_secreta_aqui')) // Protege todos os subcaminhos
    .Get('/clientes', GetClientesHandler)
    .Get('/vendas', GetVendasHandler);

  THorse.Listen(9000);
end;
```

---

## 2. Autenticação Baseada em Token JWT

O middleware oficial recomendado para validação de tokens JWT (JSON Web Tokens) no Horse é o [`horse-jwt`](https://github.com/HashLoad/horse-jwt).

### Validação do Token e Leitura das Claims
Uma vez registrado no pipeline de execução, o middleware intercepta as requisições, valida a assinatura criptográfica e descompacta as claims do payload, adicionando-as à propriedade `Session` do objeto `THorseRequest`.

```pascal
procedure GetMeuPerfilHandler(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LClaims: TJWTClaims; // Disponibilizado pelo middleware horse-jwt
  LUsuarioId: string;
begin
  // Recupera o payload decodificado a partir do dicionário de sessões
  LClaims := Req.Session<TJWTClaims>;
  
  // Extrai o ID do usuário (geralmente sob a claim 'sub' ou uma personalizada)
  LUsuarioId := LClaims.Subject; 

  Res.Send(Format('Acesso concedido para o ID: %s', [LUsuarioId]));
end;
```

---

## 3. Autenticação Básica (Basic Authentication)

Para endpoints simples ou serviços internos de integração que necessitam de credenciais de usuário/senha direto nos headers HTTP, use o middleware oficial [`horse-basic-auth`](https://github.com/HashLoad/horse-basic-auth).

```pascal
uses Horse.BasicAuth;

begin
  THorse.Group.Prefix('/admin')
    .Use(HorseBasicAuth(
      function(AUsername, APassword: string): Boolean
      begin
        // Lógica de autenticação (de preferência consultando hash do banco)
        Result := (AUsername = 'admin') and (APassword = 'senha_secreta');
      end
    ))
    .Get('/configuracoes', GetConfigHandler);
end;
```

---

## 4. Boas Práticas de Segurança e Arquitetura

Ao implementar segurança em suas APIs Delphi/Lazarus, siga estas diretrizes:

1. **Nunca Chapeie Chaves Secretas no Código**: Evite deixar a chave do JWT explícita na sintaxe do Delphi. Utilize sempre variáveis de ambiente do sistema operacional ou arquivos de configuração seguros.
   ```pascal
   // Recomendado
   THorse.Use(HorseJWT(GetEnvironmentVariable('API_JWT_SECRET')));
   ```
2. **Ciclo de Vida Curto do Token**: Emita tokens com tempo de expiração curto (claim `exp`) — por exemplo, 15 a 60 minutos — e use um fluxo de *Refresh Token* para reemitir acessos de forma segura.
3. **Transporte Criptografado (HTTPS)**: Nunca transmita tokens JWT ou credenciais básicas via HTTP puro. Use sempre HTTPS em produção (veja o guia de [Configuração SSL/TLS](./ssl-tls.pt-BR.md) para saber mais).
4. **Tratamento de Exceções**: Se a autenticação falhar, os middlewares oficiais do Horse cuidam da interrupção automática do pipeline, retornando status `401 Unauthorized` com formato JSON padronizado para o cliente.
