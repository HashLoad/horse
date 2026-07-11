# run_delphi_tests.ps1
# Script para automação de testes do Horse em múltiplos compiladores Delphi locais usando dcc32 diretamente.

$ErrorActionPreference = "Stop"

# 1. Encurta a variável de ambiente PATH para evitar qualquer problema de limite de CreateProcess no Windows
Write-Host " -> Otimizando a variável de ambiente PATH para compilação..." -ForegroundColor Gray
$CurrentPath = $env:PATH -split ";"
$CleanPathDirs = @()
foreach ($Dir in $CurrentPath) {
    if ($Dir -like "*C:\Windows*" -or $Dir -like "*Embarcadero*" -or $Dir -like "*Microsoft.NET*") {
        if (Test-Path $Dir) {
            $CleanPathDirs += $Dir
        }
    }
}
$env:PATH = $CleanPathDirs -join ";"
Write-Host " -> PATH otimizado. Novo comprimento: $($env:PATH.Length) caracteres." -ForegroundColor Gray

# Função auxiliar para formatar a linha do relatório (declarada no topo para o PowerShell conhecê-la)
function Format-ListLine($Ver, $DefName, $Build, $Tests) {
    return "{0,-22} | Provedor: {1,-14} | Compilação: {2,-8} | Testes: {3}" -f $Ver, $DefName, $Build, $Tests
}

# Caminho raiz das instalações do Delphi
$StudioPath = "C:\Program Files (x86)\Embarcadero\Studio"

# Determina o diretório onde o script está localizado para resolver caminhos relativos de forma robusta
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$OutputExe = Join-Path $ScriptDir "Console.exe"
$IncPath = Join-Path $ScriptDir "src\HorseTestDefines.inc"
$CfgPath = Join-Path $ScriptDir "src\Console.cfg"

# Mapeamento de nomes amigáveis para as versões do RAD Studio
$FriendlyVersions = @{
    "17.0" = "Delphi 10 Seattle"
    "18.0" = "Delphi 10.1 Berlin"
    "19.0" = "Delphi 10.2 Tokyo"
    "20.0" = "Delphi 10.3 Rio"
    "21.0" = "Delphi 10.4 Sydney"
    "22.0" = "Delphi 11 Alexandria"
    "23.0" = "Delphi 12 Athens"
    "37.0" = "Delphi 13 Florence"
}

# Cenários de defines a serem testados (injetados no arquivo .inc antes do build)
$DefinesToTest = @(
    @{ Name = "Default";       Flags = '{$DEFINE CI}'; DccFlags = "CI" },
    @{ Name = "Default+Radix"; Flags = '{$DEFINE CI}' + "`r`n" + '{$DEFINE HORSE_RADIX_ROUTER}'; DccFlags = "CI;HORSE_RADIX_ROUTER" },
    @{ Name = "HttpSys";       Flags = '{$DEFINE CI}' + "`r`n" + '{$DEFINE HORSE_PROVIDER_HTTPSYS}'; DccFlags = "CI;HORSE_PROVIDER_HTTPSYS" },
    @{ Name = "HttpSys+Radix"; Flags = '{$DEFINE CI}' + "`r`n" + '{$DEFINE HORSE_PROVIDER_HTTPSYS}' + "`r`n" + '{$DEFINE HORSE_RADIX_ROUTER}'; DccFlags = "CI;HORSE_PROVIDER_HTTPSYS;HORSE_RADIX_ROUTER" },
    @{ Name = "IOCP";          Flags = '{$DEFINE CI}' + "`r`n" + '{$DEFINE HORSE_PROVIDER_IOCP}'; DccFlags = "CI;HORSE_PROVIDER_IOCP" },
    @{ Name = "IOCP+Radix";    Flags = '{$DEFINE CI}' + "`r`n" + '{$DEFINE HORSE_PROVIDER_IOCP}' + "`r`n" + '{$DEFINE HORSE_RADIX_ROUTER}'; DccFlags = "CI;HORSE_PROVIDER_IOCP;HORSE_RADIX_ROUTER" }
)

# Verifica se o diretório do Studio existe
if (-not (Test-Path $StudioPath)) {
    Write-Error "O diretório do RAD Studio não foi encontrado em: $StudioPath"
    exit 1
}

# Descobre todas as versões instaladas que possuem o rsvars.bat
$Installations = Get-ChildItem -Path $StudioPath | Where-Object {
    $rsvars = Join-Path $_.FullName "bin\rsvars.bat"
    $_.PsIsContainer -and (Test-Path $rsvars)
}

if ($Installations.Count -eq 0) {
    Write-Error "Nenhuma instalação do Delphi com rsvars.bat foi detectada."
    exit 1
}

Write-Host "==========================================================" -ForegroundColor Cyan
Write-Host " Detetadas $($Installations.Count) instalações do Delphi para teste" -ForegroundColor Cyan
Write-Host "==========================================================" -ForegroundColor Cyan
foreach ($Inst in $Installations) {
    $FriendlyName = $FriendlyVersions[$Inst.Name]
    if (-not $FriendlyName) { $FriendlyName = "Delphi (Versão $($Inst.Name))" }
    Write-Host " - $FriendlyName em $($Inst.FullName)" -ForegroundColor Gray
}
Write-Host ""

$Results = @()

foreach ($Inst in $Installations) {
    $VerKey = $Inst.Name
    $FriendlyName = $FriendlyVersions[$VerKey]
    if (-not $FriendlyName) { $FriendlyName = "Delphi (Versão $VerKey)" }
    
    $RsvarsPath = Join-Path $Inst.FullName "bin\rsvars.bat"
    
    foreach ($Def in $DefinesToTest) {
        $DefName = $Def.Name
        $DefFlags = $Def.Flags
        
        Write-Host "----------------------------------------------------------" -ForegroundColor Yellow
        Write-Host " $FriendlyName -> Provedor: $DefName" -ForegroundColor Yellow
        Write-Host "----------------------------------------------------------" -ForegroundColor Yellow
        
        # 1. Limpeza de compilações anteriores (DCUs e Exe)
        Write-Host " -> Limpando arquivos temporários e binários..." -ForegroundColor Gray
        
        $DcuWin32 = Join-Path $ScriptDir "src\Win32"
        $DcuWin64 = Join-Path $ScriptDir "src\Win64"
        
        if (Test-Path $DcuWin32) { Remove-Item -Path $DcuWin32 -Recurse -Force }
        if (Test-Path $DcuWin64) { Remove-Item -Path $DcuWin64 -Recurse -Force }
        if (Test-Path $OutputExe) { Remove-Item -Path $OutputExe -Force }
        
        # Limpa arquivos DRC e MAP para evitar poluição
        $DrcFile = [IO.Path]::ChangeExtension($OutputExe, "drc")
        $MapFile = [IO.Path]::ChangeExtension($OutputExe, "map")
        if (Test-Path $DrcFile) { Remove-Item -Path $DrcFile -Force }
        if (Test-Path $MapFile) { Remove-Item -Path $MapFile -Force }
        
        # 2. Injeção das diretivas no arquivo .inc de forma isolada
        Set-Content -Path $IncPath -Value $DefFlags -Force
        
        # 3. Geração do arquivo de configuração .cfg do dcc32 temporário para o build
        # Nota: os caminhos de busca devem ser relativos à pasta tests/src/ onde o Console.dpr reside.
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
        
        # 4. Compilação direta usando dcc32.exe a partir da pasta tests/src/
        Write-Host " -> Compilando diretamente com dcc32.exe..." -ForegroundColor Gray
        
        $BuildCommand = "call `"{0}`" && cd /d `"{1}\src`" && dcc32.exe -D{2} Console.dpr" -f $RsvarsPath, $ScriptDir, $Def.DccFlags
        $BuildOutput = cmd.exe /c $BuildCommand 2>&1
        $BuildExitCode = $LASTEXITCODE
        
        $CompileSuccess = $false
        if ($BuildExitCode -eq 0 -and (Test-Path $OutputExe)) {
            $CompileSuccess = $true
            Write-Host " -> Compilação concluída com SUCESSO!" -ForegroundColor Green
        } else {
            Write-Host " -> ERRO na Compilação!" -ForegroundColor Red
            # Exibe as últimas linhas da saída de compilação em caso de erro para ajudar no diagnóstico
            $LinesToShow = 15
            $OutputLines = $BuildOutput -split "`r?`n"
            $StartIndex = [Math]::Max(0, $OutputLines.Count - $LinesToShow)
            Write-Host "Últimas linhas da saída do compilador:" -ForegroundColor DarkRed
            for ($i = $StartIndex; $i -lt $OutputLines.Count; $i++) {
                Write-Host "  $($OutputLines[$i])" -ForegroundColor DarkRed
            }
        }
        
        # 5. Execução dos testes unitários
        $TestsPassed = $false
        $TestExitCode = -1
        $TestOutput = ""
        
        if ($CompileSuccess) {
            Write-Host " -> Executando suíte de testes..." -ForegroundColor Gray
            
            # Executa o executável DUnitX enviando uma entrada vazia via pipe para evitar qualquer Readln bloqueante
            $env:HORSE_TEST_SILENCE = "1"
            $TestOutput = "" | & $OutputExe 2>&1
            $TestExitCode = $LASTEXITCODE
            $env:HORSE_TEST_SILENCE = $null
            
            if ($TestExitCode -eq 0) {
                $TestsPassed = $true
                Write-Host " -> Todos os testes PASSARAM com sucesso!" -ForegroundColor Green
            } else {
                Write-Host " -> Falha em testes unitários! Código de saída: $TestExitCode" -ForegroundColor Red
                Write-Host "Resultado da execução:" -ForegroundColor DarkRed
                Write-Host $TestOutput -ForegroundColor DarkRed
            }
        }
        
        $Results += [PSCustomObject]@{
            DelphiVersion = $FriendlyName
            DefName       = $DefName
            BuildStatus   = if ($CompileSuccess) { "SUCESSO" } else { "FALHA" }
            TestStatus    = if ($CompileSuccess) { if ($TestsPassed) { "PASSOU" } else { "FALHOU" } } else { "N/A" }
        }
        
        Write-Host ""
    }
}

# Restaura o arquivo de include padrão para um estado limpo
Set-Content -Path $IncPath -Value '{$DEFINE CI}' -Force
if (Test-Path $CfgPath) { Remove-Item -Path $CfgPath -Force }

# 6. Exibição do relatório final resumido
Write-Host "======================================================================================" -ForegroundColor Cyan
Write-Host "                                  RESUMO DOS TESTES                                   " -ForegroundColor Cyan
Write-Host "======================================================================================" -ForegroundColor Cyan
$HasFailure = $false
foreach ($Res in $Results) {
    $StatusColor = "Green"
    if ($Res.BuildStatus -eq "FALHA" -or $Res.TestStatus -eq "FALHOU") {
        $StatusColor = "Red"
        $HasFailure = $true
    }
    Write-Host (Format-ListLine $Res.DelphiVersion $Res.DefName $Res.BuildStatus $Res.TestStatus) -ForegroundColor $StatusColor
}
Write-Host "======================================================================================" -ForegroundColor Cyan

if ($HasFailure) {
    Write-Host "Houve falhas na compilação ou execução dos testes!" -ForegroundColor Red
    exit 1
} else {
    Write-Host "Sucesso! Todas as compilações e testes passaram." -ForegroundColor Green
    exit 0
}
