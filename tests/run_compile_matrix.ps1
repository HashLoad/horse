# run_compile_matrix.ps1
# Script para verificação estática de compilação de todas as combinações de Provedores e Roteadores no Horse.
# Compatível com Delphi 10, 11, 12, 13 locais e Lazarus/FPC via Docker Linux.

$ErrorActionPreference = "Continue"

# 1. Otimiza o PATH para evitar limite de caracteres do Windows, preservando o Docker
$CurrentPath = $env:PATH -split ";"
$CleanPathDirs = @()
foreach ($Dir in $CurrentPath) {
    if ($Dir -like "*C:\Windows*" -or $Dir -like "*Embarcadero*" -or $Dir -like "*Microsoft.NET*" -or $Dir -like "*Docker*") {
        if (Test-Path $Dir) { $CleanPathDirs += $Dir }
    }
}
$DockerDefault = "C:\Program Files\Docker\Docker\resources\bin"
if (Test-Path $DockerDefault) { $CleanPathDirs += $DockerDefault }
$env:PATH = ($CleanPathDirs | Select-Object -Unique) -join ";"

# Caminho do Delphi Studio
$StudioPath = "C:\Program Files (x86)\Embarcadero\Studio"
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$CompileTarget = Join-Path $ScriptDir "src\CompileCheck.dpr"

function Limpar-ArquivosTemporarios {
    Get-ChildItem -Path $ScriptDir -Recurse -Include *.dcu, *.ppu, *.o, *.dof, *.identcache, *.local -ErrorAction SilentlyContinue | Remove-Item -Force -ErrorAction SilentlyContinue
    Get-ChildItem -Path (Join-Path $ScriptDir "..\src") -Recurse -Include *.dcu, *.ppu, *.o -ErrorAction SilentlyContinue | Remove-Item -Force -ErrorAction SilentlyContinue
}

# Mapeamento de versões
$FriendlyVersions = @{
    "17.0" = "Delphi 10 Seattle"
    "22.0" = "Delphi 11 Alexandria"
    "23.0" = "Delphi 12 Athens"
    "37.0" = "Delphi 13 Florence"
}

# Combinações a testar no Delphi
$DelphiScenarios = @(
    # Provedor Default (Indy)
    @{ Name = "Default"; Defines = "CI" },
    @{ Name = "Default+Radix"; Defines = "CI;HORSE_RADIX_ROUTER" },
    # IOCP
    @{ Name = "IOCP"; Defines = "CI;HORSE_PROVIDER_IOCP" },
    @{ Name = "IOCP+Radix"; Defines = "CI;HORSE_PROVIDER_IOCP;HORSE_RADIX_ROUTER" },
    # HttpSys
    @{ Name = "HttpSys"; Defines = "CI;HORSE_PROVIDER_HTTPSYS" },
    @{ Name = "HttpSys+Radix"; Defines = "CI;HORSE_PROVIDER_HTTPSYS;HORSE_RADIX_ROUTER" },
    # Apache
    @{ Name = "Apache"; Defines = "CI;HORSE_PROVIDER_APACHE" },
    @{ Name = "Apache+Radix"; Defines = "CI;HORSE_PROVIDER_APACHE;HORSE_RADIX_ROUTER" },
    # CGI
    @{ Name = "CGI"; Defines = "CI;HORSE_PROVIDER_CGI" },
    @{ Name = "CGI+Radix"; Defines = "CI;HORSE_PROVIDER_CGI;HORSE_RADIX_ROUTER" },
    # ISAPI
    @{ Name = "ISAPI"; Defines = "CI;HORSE_PROVIDER_ISAPI" },
    @{ Name = "ISAPI+Radix"; Defines = "CI;HORSE_PROVIDER_ISAPI;HORSE_RADIX_ROUTER" },
    # Daemon
    @{ Name = "Daemon"; Defines = "CI;HORSE_PROVIDER_DAEMON" },
    @{ Name = "Daemon+Radix"; Defines = "CI;HORSE_PROVIDER_DAEMON;HORSE_RADIX_ROUTER" },
    # VCL
    @{ Name = "VCL"; Defines = "CI;HORSE_PROVIDER_VCL" },
    @{ Name = "VCL+Radix"; Defines = "CI;HORSE_PROVIDER_VCL;HORSE_RADIX_ROUTER" }
)

# Combinações a testar no FPC / Lazarus Docker
$FpcScenarios = @(
    @{ Name = "Default"; Defines = "HORSE_CONSOLE" },
    @{ Name = "Default+Radix"; Defines = "HORSE_CONSOLE;HORSE_RADIX_ROUTER" },
    @{ Name = "Epoll"; Defines = "HORSE_CONSOLE;HORSE_PROVIDER_EPOLL" },
    @{ Name = "Epoll+Radix"; Defines = "HORSE_CONSOLE;HORSE_PROVIDER_EPOLL;HORSE_RADIX_ROUTER" },
    @{ Name = "Apache"; Defines = "HORSE_CONSOLE;HORSE_PROVIDER_APACHE" },
    @{ Name = "Apache+Radix"; Defines = "HORSE_CONSOLE;HORSE_PROVIDER_APACHE;HORSE_RADIX_ROUTER" },
    @{ Name = "CGI"; Defines = "HORSE_CONSOLE;HORSE_PROVIDER_CGI" },
    @{ Name = "CGI+Radix"; Defines = "HORSE_CONSOLE;HORSE_PROVIDER_CGI;HORSE_RADIX_ROUTER" },
    @{ Name = "Daemon"; Defines = "HORSE_CONSOLE;HORSE_PROVIDER_DAEMON" },
    @{ Name = "Daemon+Radix"; Defines = "HORSE_CONSOLE;HORSE_PROVIDER_DAEMON;HORSE_RADIX_ROUTER" }
)

$Results = @()

# --- 1. COMPILAÇÃO DELPHI LOCAL ---
if (Test-Path $StudioPath) {
    $Installations = Get-ChildItem -Path $StudioPath | Where-Object {
        $rsvars = Join-Path $_.FullName "bin\rsvars.bat"
        $_.PsIsContainer -and (Test-Path $rsvars)
    }

    foreach ($Inst in $Installations) {
        $FriendlyName = $FriendlyVersions[$Inst.Name]
        if (-not $FriendlyName) { $FriendlyName = "Delphi (Versão $($Inst.Name))" }
        
        $Dcc32 = Join-Path $Inst.FullName "bin\dcc32.exe"
        if (-not (Test-Path $Dcc32)) { continue }

        Write-Host ">>> Iniciando matriz de compilacao para $FriendlyName..." -ForegroundColor Cyan

        foreach ($Scen in $DelphiScenarios) {
            $ScenName = $Scen.Name
            $Defines = $Scen.Defines

            Write-Host " -> Compilando Provedor: $ScenName..." -ForegroundColor Gray

            # Executa a limpeza dos DCUs antigos
            Limpar-ArquivosTemporarios

            # Chama o dcc32 diretamente via array de argumentos para evitar interpretador do PowerShell no ponto e vírgula
            $DccArgs = @(
                "-Q",
                "-Isrc",
                "-Usrc",
                "-NSSystem;Xml;Data;Datasnap;Web;Soap;Winapi",
                "-Imodules;modules\jhonson\src;modules\restrequest4delphi\src",
                "-Umodules;modules\jhonson\src;modules\restrequest4delphi\src",
                "-D$Defines",
                $CompileTarget
            )

            $Output = & $Dcc32 $DccArgs 2>&1

            $BuildStatus = "SUCESSO"
            if ($LastExitCode -ne 0) {
                $BuildStatus = "FALHA"
                Write-Host "   [!] FALHA na compilacao!" -ForegroundColor Red
            } else {
                Write-Host "   [+] OK" -ForegroundColor Green
            }

            $Results += [PSCustomObject]@{
                Compilador = $FriendlyName
                Provedor   = $ScenName
                Defines    = $Defines
                Plataforma = "Windows"
                Status     = $BuildStatus
            }
        }
    }
}

# --- 2. COMPILAÇÃO FPC / LAZARUS DOCKER LINUX ---
$HasDocker = $null -ne (Get-Command docker -ErrorAction SilentlyContinue)

if ($HasDocker) {
    Write-Host ">>> Iniciando matriz de compilacao para FPC/Lazarus no Linux via Docker..." -ForegroundColor Cyan

    foreach ($Scen in $FpcScenarios) {
        $ScenName = $Scen.Name
        $Defines = $Scen.Defines

        # Garante limpeza completa de arquivos binários compilados incompatíveis do Windows
        Limpar-ArquivosTemporarios

        # Traduz defines do FPC (-dDEF1 -dDEF2)
        $FpcDefinesList = $Defines -split ";"
        $FpcFlags = ""
        foreach ($Def in $FpcDefinesList) {
            $FpcFlags += "-d$Def "
        }

        Write-Host " -> Compilando Provedor (FPC Linux): $ScenName..." -ForegroundColor Gray

        $DockerArgs = @(
            "run", "--rm",
            "-v", "$ScriptDir\..\:/usr/src/app",
            "-w", "/usr/src/app/tests/src",
            "horse-tests-lazarus",
            "bash", "-c", "mkdir -p /tmp/fpc_lib /tmp/fpc_bin && fpc -B -Mdelphi -Sh -FE/tmp/fpc_bin -FU/tmp/fpc_lib -Fu../../src:modules/jhonson/src:modules/restrequest4delphi/src:modules/cors/src:modules/basic-auth/src $($FpcFlags.Trim()) CompileCheck.dpr"
        )

        $BuildStatus = "SUCESSO"
        try {
            $DockerOutput = & docker $DockerArgs
            if ($LastExitCode -ne 0) {
                $BuildStatus = "FALHA"
                Write-Host "   [!] FALHA na compilacao!" -ForegroundColor Red
            } else {
                Write-Host "   [+] OK" -ForegroundColor Green
            }
        } catch {
            $BuildStatus = "FALHA"
            Write-Host "   [!] FALHA na execucao do Docker!" -ForegroundColor Red
        }

        $Results += [PSCustomObject]@{
            Compilador = "FPC / Lazarus"
            Provedor   = $ScenName
            Defines    = $Defines
            Plataforma = "Linux (Docker)"
            Status     = $BuildStatus
        }
    }
} else {
    Write-Host ">>> Docker nao encontrado no host. Pulando testes do Lazarus/FPC." -ForegroundColor Yellow
    foreach ($Scen in $FpcScenarios) {
        $Results += [PSCustomObject]@{
            Compilador = "FPC / Lazarus"
            Provedor   = $Scen.Name
            Defines    = $Scen.Defines
            Plataforma = "Linux (Docker)"
            Status     = "DOCKER_INDISPONIVEL"
        }
    }
}

# --- 3. EXIBIÇÃO DO RELATÓRIO FINAL ---
Write-Host ""
Write-Host "==========================================================================" -ForegroundColor Cyan
Write-Host "                  RELATORIO DE COMPILACAO ESTATICA                        " -ForegroundColor Cyan
Write-Host "==========================================================================" -ForegroundColor Cyan
$Results | Format-Table -Property Compilador, Provedor, Plataforma, Status -AutoSize
Write-Host "==========================================================================" -ForegroundColor Cyan

# Retorna código de saída adequado se houver qualquer falha
$FailedCount = ($Results | Where-Object { $_.Status -eq "FALHA" }).Count
if ($FailedCount -gt 0) {
    Write-Host " [!] Encontradas $FailedCount falhas de compilacao!" -ForegroundColor Red
    exit 1
} else {
    Write-Host " [+] Todas as combinacoes compilaram com SUCESSO!" -ForegroundColor Green
    exit 0
}
