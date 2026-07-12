# Configuração de SSL/TLS (HTTPS)

Este guia orienta sobre como habilitar a criptografia de dados (HTTPS) em aplicações desenvolvidas com o framework **Horse**, cobrindo os diferentes provedores de transporte disponíveis.

---

## 1. Por que usar SSL/TLS diretamente na aplicação?

Habilitar HTTPS garante que todos os dados transmitidos entre o cliente e a API (como cabeçalhos de autenticação, dados de cartões de crédito e credenciais de usuários) sejam criptografados, protegendo a API contra ataques do tipo *man-in-the-middle* (MITF).

No Horse, dependendo do **Provider de Transporte** escolhido, a configuração de certificados SSL/TLS varia:

---

## 2. Configurando SSL/TLS no Provider Padrão (Indy)

O Indy (Provider padrão do Horse) realiza o gerenciamento de SSL/TLS utilizando a biblioteca OpenSSL (geralmente na versão 1.0.2).

### Pré-requisitos:
*   As DLLs do OpenSSL (`ssleay32.dll` e `libeay32.dll`) compatíveis com a arquitetura do seu executável (32 ou 64 bits) devem estar na mesma pasta da aplicação ou no Path do sistema.
*   Um arquivo de Certificado (ex: `server.crt`) e uma Chave Privada (ex: `server.key`) no formato PEM.

### Exemplo de Configuração:
Para associar a lógica SSL ao servidor, injetamos a configuração utilizando o evento de inicialização de portas e vinculando o componente do Indy:

```pascal
uses
  Horse,
  IdSSLOpenSSL; // Necessário incluir esta unit do Indy

begin
  // Adiciona a configuração no manipulador de SSL
  THorse.OnUseSSL := function: TObject
  var
    LSSLHandler: TIdServerIOHandlerSSLOpenSSL;
  begin
    LSSLHandler := TIdServerIOHandlerSSLOpenSSL.Create(nil);
    LSSLHandler.SSLOptions.CertFile := 'caminho/para/o/certificado.crt';
    LSSLHandler.SSLOptions.KeyFile := 'caminho/para/a/chave.key';
    LSSLHandler.SSLOptions.Method := sslvTLSv1_2; // Força o uso de TLS 1.2
    Result := LSSLHandler;
  end;

  THorse.Listen(9000);
end;
```

---

## 3. Configurando SSL/TLS no Provider HTTP.sys (Windows Nativo)

Ao utilizar o provider `Horse.Provider.HttpSys` no Windows, o gerenciamento de conexões seguras é realizado diretamente pelo kernel do sistema operacional.

### Pré-requisito:
Você não configura o certificado dentro do código Delphi. A vinculação é feita a nível de sistema operacional vinculando a porta TCP escolhida ao hash SHA1 do certificado (previamente instalado no repositório de Certificados do Computador Local).

### Exemplo de Configuração no Sistema:
1. Abra o PowerShell ou Prompt de Comando como Administrador.
2. Registre a associação da porta e do certificado com o comando `netsh`:

```cmd
netsh http add sslcert ipport=0.0.0.0:443 certhash=SUA_CHAVE_SHA1_DO_CERTIFICADO appid={SEU_APP_GUID}
```

No código Delphi, basta iniciar o Horse escutando a porta segura:

```pascal
uses
  Horse,
  Horse.Provider.HttpSys; // Usa o driver do HTTP.sys

begin
  // Apenas inicia a escuta na porta vinculada ao SSL no Windows
  THorse.Listen(443);
end;
```

---

## 4. Prática Recomendada em Ambientes Corporativos: Proxy Reverso

> [!TIP]
> Embora seja possível configurar SSL/TLS diretamente nos executáveis Delphi, em ambientes corporativos e de alta concorrência a prática recomendada de mercado é usar um **Proxy Reverso** (como **Nginx**, **Caddy** ou **Apache**) à frente do seu servidor Horse.

### Vantagens do Proxy Reverso:
1.  **Segurança e Isolação**: O Nginx/Caddy intercepta as requisições públicas expostas e repassa o tráfego de forma limpa (geralmente via HTTP local) para o executável do Horse.
2.  **Renovação Automática**: Servidores de borda modernos como o Caddy gerenciam e renovam automaticamente certificados Let's Encrypt válidos sem que você precise parar ou recompilar sua aplicação Delphi.
3.  **Desempenho**: Reduz o overhead de CPU do seu executável Delphi que não precisará gastar processamento com o aperto de mãos (*TLS Handshake*).

```
[ Cliente ]  --- ( HTTPS / Porta 443 ) --->  [ Nginx / Caddy ]  --- ( HTTP / Porta 9000 ) --->  [ API Horse ]
```
