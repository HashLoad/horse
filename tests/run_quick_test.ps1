# run_quick_test.ps1
# Executa um teste unitário individual e rápido no Delphi 13 Florence para comprovar que a fachada estática legada continua funcionando.

$ErrorActionPreference = "Stop"

$StudioPath = "C:\Program Files (x86)\Embarcadero\Studio\37.0"
$RsvarsPath = Join-Path $StudioPath "bin\rsvars.bat"
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$OutputExe = Join-Path $ScriptDir "Console.exe"
$IncPath = Join-Path $ScriptDir "src\HorseTestDefines.inc"
$CfgPath = Join-Path $ScriptDir "src\Console.cfg"

Write-Host "==========================================================" -ForegroundColor Cyan
Write-Host " Executando Teste Rápido: Delphi 13 Florence (Default)   " -ForegroundColor Cyan
Write-Host "==========================================================" -ForegroundColor Cyan

# 1. Limpeza
if (Test-Path $OutputExe) { Remove-Item -Path $OutputExe -Force }

# 2. Configurações
Set-Content -Path $IncPath -Value '{$DEFINE CI}' -Force
$SearchPath = '..\..\src;modules;modules\jhonson\src;modules\restrequest4delphi\src;modules\restrequest4delphi\src\core;modules\restrequest4delphi\src\interfaces'
$CfgContent = @(
    "-B",
    "-Q",
    "-E`"..`"",
    "-NS`"System;Xml;Data;Datasnap;Web;Soap;Winapi`"",
    "-I`"$SearchPath`"",
    "-U`"$SearchPath`""
)
Set-Content -Path $CfgPath -Value ($CfgContent -join "`r`n") -Force

# 3. Compilação
Write-Host " -> Compilando com dcc32.exe..." -ForegroundColor Gray
$BuildCommand = "call `"{0}`" && cd /d `"{1}\src`" && dcc32.exe Console.dpr" -f $RsvarsPath, $ScriptDir
$BuildOutput = cmd.exe /c $BuildCommand 2>&1
$BuildExitCode = $LASTEXITCODE

if ($BuildExitCode -eq 0 -and (Test-Path $OutputExe)) {
    Write-Host " -> Compilação: SUCESSO" -ForegroundColor Green
} else {
    Write-Host " -> Compilação: ERRO" -ForegroundColor Red
    Write-Host $BuildOutput -ForegroundColor DarkRed
    exit 1
}

# 4. Execução dos testes
Write-Host " -> Executando suíte de testes unitários..." -ForegroundColor Gray
$env:HORSE_TEST_SILENCE = "1"
$TestOutput = cmd.exe /c "echo. | `"$OutputExe`"" 2>&1
$TestExitCode = $LASTEXITCODE
$env:HORSE_TEST_SILENCE = $null

# 5. Limpeza de configs temporários
if (Test-Path $CfgPath) { Remove-Item -Path $CfgPath -Force }

if ($TestExitCode -eq 0) {
    Write-Host "==========================================================" -ForegroundColor Green
    Write-Host "  TESTE CONCLUÍDO COM SUCESSO! FACHADA CLÁSSICA PRESERVADA" -ForegroundColor Green
    Write-Host "==========================================================" -ForegroundColor Green
    exit 0
} else {
    Write-Host " -> Falha na execução dos testes!" -ForegroundColor Red
    Write-Host $TestOutput -ForegroundColor DarkRed
    exit 1
}
