#!/bin/bash
set -e

echo "=================================================="
echo "   INICIANDO TESTE DE INTEGRIDADE DO LAZARUS (FPC) NO LINUX"
echo "=================================================="

# 1. Compilando o Exemplo
echo "[1/5] Compilando ConsoleComplete.lpr..."
fpc -Mdelphi -Sh -Fu"../../../src" ConsoleComplete.lpr

if [ ! -f "ConsoleComplete" ]; then
    echo "Falha na compilacao! Executavel nao gerado."
    exit 1
fi
echo "Compilado com sucesso!"

# 2. Executando o servidor em background
echo "[2/5] Iniciando o servidor na porta 9086 em segundo plano..."
./ConsoleComplete &
SERVER_PID=$!
sleep 2

# 3. Executando chamadas e validacoes
echo "[3/5] Executando chamadas HTTP..."
errors=0

assert_response() {
    local test_name="$1"
    local actual="$2"
    local expected="$3"
    
    if [[ "$actual" == *"$expected"* ]]; then
        echo -e "  \e[32m[OK] $test_name\e[0m"
    else
        echo -e "  \e[31m[FALHA] $test_name\e[0m"
        echo "    Esperado conter: $expected"
        echo "    Obtido: $actual"
        errors=$((errors+1))
    fi
}

# CORS Preflight (OPTIONS)
res=$(curl -i -s -X OPTIONS http://localhost:9086/ping)
assert_response "CORS Preflight (Status 204)" "$res" "HTTP/1.1 204"
assert_response "CORS Preflight (Origin Header)" "$res" "Access-Control-Allow-Origin: *"
assert_response "CORS Preflight (Methods Header)" "$res" "Access-Control-Allow-Methods: GET, POST, PUT, DELETE, OPTIONS, PATCH, QUERY"
assert_response "CORS Preflight (Headers Header)" "$res" "Access-Control-Allow-Headers: Content-Type, Authorization, X-Requested-With"

# GET /ping (CORS origin header & Status 200)
res=$(curl -i -s http://localhost:9086/ping)
assert_response "GET /ping (Status 200)" "$res" "HTTP/1.1 200"
assert_response "GET /ping (Origin Header)" "$res" "Access-Control-Allow-Origin: *"
assert_response "GET /ping (Body)" "$res" "pong"

# GET /resource/:id (Decoding & Case-Insensitive Headers)
res=$(curl -s -H "AuThOrIzAtIoN: Bearer JWT_123" "http://localhost:9086/resource/caf%C3%A9?q=teste%20completo")
assert_response "GET /resource/:id (Decoding Parameter - Acento)" "$res" '"id":"café"'
assert_response "GET /resource/:id (Decoding Query - Espaço)" "$res" '"query":"teste completo"'
assert_response "GET /resource/:id (Case-Insensitive Header Extraction)" "$res" '"auth":"Bearer JWT_123"'

# POST /resource
res=$(curl -i -s -X POST -H "Content-Type: text/plain" -d "Ola_Horse" http://localhost:9086/resource)
assert_response "POST /resource (Status 201)" "$res" "HTTP/1.1 201"
assert_response "POST /resource (Body)" "$res" "POST OK: Ola_Horse"

# PUT /resource/:id
res=$(curl -i -s -X PUT http://localhost:9086/resource/123)
assert_response "PUT /resource/:id (Status 200)" "$res" "HTTP/1.1 200"
assert_response "PUT /resource/:id (Body)" "$res" "Updated"

# PATCH /resource/:id
res=$(curl -i -s -X PATCH http://localhost:9086/resource/123)
assert_response "PATCH /resource/:id (Status 200)" "$res" "HTTP/1.1 200"
assert_response "PATCH /resource/:id (Body)" "$res" "Patched"

# DELETE /resource/:id
res=$(curl -i -s -X DELETE http://localhost:9086/resource/123)
assert_response "DELETE /resource/:id (Status 200)" "$res" "HTTP/1.1 200"
assert_response "DELETE /resource/:id (Body)" "$res" "Deleted"

# QUERY /search
res=$(curl -i -s -X QUERY -d "filtro_busca" http://localhost:9086/search)
assert_response "QUERY /search (Status 200)" "$res" "HTTP/1.1 200"
assert_response "QUERY /search (Body)" "$res" "SEARCH RESULT FOR: filtro_busca"

# POST /upload (Multipart)
echo "Conteudo de amostra de upload" > temp_sample_upload.txt
res=$(curl -i -s -X POST -F "file=@temp_sample_upload.txt" http://localhost:9086/upload)
assert_response "POST /upload (Status 200)" "$res" "HTTP/1.1 200"
assert_response "POST /upload (Multipart Body)" "$res" "Upload OK"
rm -f temp_sample_upload.txt

# GET /error-trigger (Clean JSON Exception)
res=$(curl -i -s http://localhost:9086/error-trigger)
assert_response "GET /error-trigger (Status 400)" "$res" "HTTP/1.1 400"
assert_response "GET /error-trigger (Clean JSON Exception)" "$res" "Erro de Negocio Simulado"

# 4. Finalizando o Servidor
echo "[4/5] Finalizando processo do servidor..."
kill -9 $SERVER_PID || true
sleep 1

# 5. Limpando binarios temporarios
echo "[5/5] Limpando binarios e arquivos intermediarios..."
rm -f ConsoleComplete ConsoleComplete.o

echo "=================================================="
if [ $errors -eq 0 ]; then
    echo -e "      \e[32mTODOS OS TESTES DE INTEGRIDADE PASSARAM!\e[0m"
    echo "=================================================="
else
    echo -e "      \e[31mOCORRERAM $errors FALHA(S) NO TESTE!\e[0m"
    echo "=================================================="
    exit 1
fi
