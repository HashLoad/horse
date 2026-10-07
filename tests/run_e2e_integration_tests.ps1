param([string]$MiddlewareRoot = '', [string]$Version = '37.0')
$ErrorActionPreference = 'Stop'
$root = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
if (-not $MiddlewareRoot) { $MiddlewareRoot = Join-Path $root '../horse_middlewares' }
$compiler = "C:/Program Files (x86)/Embarcadero/Studio/$Version/bin/dcc32.exe"
$library = "C:/Program Files (x86)/Embarcadero/Studio/$Version/lib/win32/release"
$output = Join-Path $root ('benchmarks/results/e2e-' + (Get-Date -Format 'yyyyMMdd-HHmmss'))
New-Item -ItemType Directory -Force $output | Out-Null

function Assert-PortFree([int]$Port) {
    if (Get-NetTCPConnection -LocalPort $Port -State Listen -ErrorAction SilentlyContinue) {
        throw "Port $Port is occupied; no unrelated process will be stopped."
    }
}

function Build-Server([string]$Source, [string]$ExtraSearchPath = '') {
    & $compiler -B -Q '-NSSystem;Xml;Data;Datasnap;Web;Soap;Winapi' "-U$root/src;$library;$ExtraSearchPath" "-I$root/src;$ExtraSearchPath" "-E$output" "-N0$output" $Source *> "$output/build-$([IO.Path]::GetFileNameWithoutExtension($Source)).log"
    if ($LASTEXITCODE -ne 0) { throw "Build failed; see $output" }
}

Assert-PortFree 9001
Assert-PortFree 9002
Build-Server "$root/samples/delphi/console_multi_instance/ConsoleMultiInstance.dpr"
$process = Start-Process "$output/ConsoleMultiInstance.exe" -ArgumentList '--delay' -WindowStyle Hidden -PassThru -RedirectStandardOutput "$output/multi-instance.log" -RedirectStandardError "$output/multi-instance.err"
try {
    $ready = $false
    $deadline = (Get-Date).AddSeconds(8)
    while ((Get-Date) -lt $deadline) {
        if ($process.HasExited) { throw 'Multi-instance server exited before readiness.' }
        try {
            $public = Invoke-RestMethod 'http://127.0.0.1:9001/api/v1/ping' -TimeoutSec 2
            $admin = Invoke-RestMethod 'http://127.0.0.1:9002/admin/ping' -TimeoutSec 2
            if ($public -ne 'Pong da API Publica (Instancia 1)' -or
                $admin -ne 'Pong da Area Admin (Instancia 2)') { throw 'Incorrect ping response' }
            $ready = $true
            break
        } catch { Start-Sleep -Milliseconds 100 }
    }
    if (-not $ready) { throw 'Multi-instance listener startup timed out.' }
    $isolated = $false
    try { Invoke-WebRequest 'http://127.0.0.1:9001/admin/ping' -TimeoutSec 2 | Out-Null }
    catch { $isolated = [int]$_.Exception.Response.StatusCode -eq 404 }
    if (-not $isolated) { throw 'Instance isolation failed: expected 404.' }
    if (-not $process.WaitForExit(15000)) { throw 'Multi-instance shutdown timed out.' }
    if ($process.ExitCode -ne 0) { throw 'Multi-instance server failed.' }
    Write-Host 'PASS: multi-instance responses, route isolation and graceful exit.'
} finally {
    if (-not $process.HasExited) { Stop-Process -Id $process.Id -Force }
    $process.Dispose()
}

Assert-PortFree 9999
$dependencies = @(
    "$root/tests/src/modules/github_com_hashload_jhonson/src",
    "$root/tests/src/modules/jhonson/src",
    "$root/tests/src/modules/cors/src",
    "$root/tests/src/modules/basic-auth/src",
    "$MiddlewareRoot/horse-jhonson/src",
    "$MiddlewareRoot/horse-cors/src",
    "$MiddlewareRoot/horse-basic-auth/src"
) | Where-Object { Test-Path $_ }
Build-Server "$root/tests/src/IntegrationServer.dpr" ($dependencies -join ';')
# IntegrationServer is a self-testing executable, not a persistent HTTP daemon.
$process = Start-Process "$output/IntegrationServer.exe" -WindowStyle Hidden -PassThru -RedirectStandardOutput "$output/integration.log" -RedirectStandardError "$output/integration.err"
try {
    if (-not $process.WaitForExit(30000)) { throw 'Middleware integration timed out.' }
    if ($process.ExitCode -ne 0) { throw "Middleware integration failed; see $output/integration.log" }
    $text = Get-Content "$output/integration.log" -Raw
    if ($text -notmatch 'INTEGRATION TEST: SUCCESS') { throw 'Integration success marker missing.' }
    Write-Host 'PASS: CORS/Jhonson, unauthenticated 401, authenticated GET and JSON POST.'
} finally {
    if (-not $process.HasExited) { Stop-Process -Id $process.Id -Force }
    $process.Dispose()
}
Write-Host "E2E integration passed. Reports: $output"
