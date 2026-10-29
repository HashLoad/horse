param(
    [string]$Version = '37.0',
    [ValidateSet('Windows', 'Linux')][string]$Platform = 'Linux',
    [ValidateSet('Console', 'Daemon', 'VCL')][string]$Provider = 'Console',
    [string]$DockerImage = 'horse-indy-tests:latest',
    [string]$LegacyIndySource = '',
    [string]$LegacyCompilerDefine = '',
    [switch]$SocketOnly,
    [switch]$Radix,
    [string]$HorseSource = ''
)

$ErrorActionPreference = 'Stop'
$root = (Resolve-Path (Join-Path $PSScriptRoot '../..')).Path
if (-not $HorseSource) { $HorseSource = Join-Path $root 'src' }
$HorseSource = (Resolve-Path -LiteralPath $HorseSource).Path
$studio = "C:/Program Files (x86)/Embarcadero/Studio/$Version"
$scenario = "$Version-$Platform-$Provider"
if ($Radix) { $scenario += '-Radix' }
if ($LegacyIndySource) { $scenario += '-legacy-source' }
$output = Join-Path $root ('benchmarks/results/indy-keepalive-' + $scenario + '-' + (Get-Date -Format 'yyyyMMdd-HHmmss'))
New-Item -ItemType Directory -Force -Path $output | Out-Null

if ($Platform -eq 'Windows' -and $Provider -eq 'Daemon') { throw 'Daemon requires Linux' }
if ($Platform -eq 'Linux' -and $Provider -eq 'VCL') { throw 'VCL requires Windows' }
if ($LegacyIndySource -and (-not $SocketOnly -or $Platform -ne 'Linux')) {
    throw 'Legacy source validation is Linux/socket-only; HTTP has a separate legacy-contract control.'
}

$defines = @()
if ($Radix) { $defines += 'HORSE_RADIX_ROUTER' }
if ($Provider -eq 'Daemon') { $defines += 'HORSE_DAEMON' }
if ($Provider -eq 'VCL') { $defines += 'HORSE_VCL' }
$search = @($HorseSource, (Join-Path $root 'src'))
$includes = @()
if ($LegacyIndySource) {
    $legacy = (Resolve-Path -LiteralPath $LegacyIndySource).Path
    $search = @("$legacy/System", "$legacy/Core") + $search
    $includes = @("$legacy/System", "$legacy/Core")
    $defines += 'HORSE_TEST_LEGACY_INDY'
    if ($LegacyCompilerDefine) { $defines += $LegacyCompilerDefine }
}

if ($Platform -eq 'Linux') {
    $library = "$studio/lib/linux64/release"
    $compiler = "$studio/bin/dcclinux64.exe"
} else {
    $library = "$studio/lib/win32/release"
    $compiler = "$studio/bin/dcc32.exe"
}
$search += $library
$programs = @('IndyKeepAliveCheck')
if (-not $SocketOnly) { $programs += 'ProviderKeepAliveCheck' }

foreach ($program in $programs) {
    $programOutput = Join-Path $output $program
    New-Item -ItemType Directory -Force -Path $programOutput | Out-Null
    $arguments = @('-B', '-Q', '-NSSystem;Xml;Data;Datasnap;Web;Soap;Winapi;Posix',
        "-U$($search -join ';')", "-E$programOutput", "-N0$programOutput")
    if ($includes.Count) { $arguments += "-I$($includes -join ';')" }
    if ($defines.Count) { $arguments += "-D$($defines -join ';')" }
    if ($Platform -eq 'Linux') { $arguments += '--save-temps' }
    $arguments += (Join-Path $PSScriptRoot "$program.dpr")
    & $compiler @arguments *> "$programOutput/build.log"
    if ($LASTEXITCODE -ne 0 -and
        ($Platform -ne 'Linux' -or
         (Get-Content "$programOutput/build.log" -Raw) -notmatch 'F2588 Linker error' -or
         -not (Test-Path "$programOutput/$program.lnk"))) {
        throw "Pascal compilation failed: $programOutput/build.log"
    }

    $runs = @('default')
    if ($Platform -eq 'Linux' -and $program -eq 'ProviderKeepAliveCheck') {
        $runs += 'legacy-contract'
    }
    foreach ($run in $runs) {
        $log = "$programOutput/$run.log"
        if ($Platform -eq 'Linux') {
            $dockerArguments = @('run', '--rm',
                '--mount', "type=bind,source=$programOutput,target=/out",
                '--mount', "type=bind,source=$library,target=/delphi,readonly",
                '--mount', "type=bind,source=$PSScriptRoot,target=/scripts,readonly",
                '--entrypoint', 'bash', $DockerImage,
                '/scripts/link-and-run.sh', $program)
            if ($run -eq 'legacy-contract') { $dockerArguments += '-legacy-contract' }
            & docker @dockerArguments *> $log
            $runStatus = $LASTEXITCODE
        } else {
            if ($program -eq 'ProviderKeepAliveCheck') {
                # A just-exited listener can briefly remain visible to the TCP
                # table query. Wait for teardown without stopping any process.
                $portDeadline = (Get-Date).AddSeconds(5)
                while (Get-NetTCPConnection -LocalPort 19263 -State Listen -ErrorAction SilentlyContinue) {
                    if ((Get-Date) -ge $portDeadline) {
                        throw 'Test port 19263 is in use; no process was stopped.'
                    }
                    Start-Sleep -Milliseconds 100
                }
            }
            $process = Start-Process -FilePath "$programOutput/$program.exe" -WindowStyle Hidden `
                -PassThru -RedirectStandardOutput $log -RedirectStandardError "$log.err"
            try {
                if (-not $process.WaitForExit(40000)) {
                    Stop-Process -Id $process.Id -Force
                    throw "Keep-alive regression timed out: $log"
                }
                $runStatus = $process.ExitCode
            } finally {
                $process.Dispose()
            }
            if ((Test-Path "$log.err") -and (Get-Item "$log.err").Length -gt 0) {
                Get-Content "$log.err" | Add-Content $log
            }
        }
        if ($runStatus -ne 0 -or
            (Get-Content $log -Raw) -notmatch 'PASS:' -or
            (Get-Content $log -Raw) -match 'Unexpected Memory Leak') {
            Get-Content $log
            throw "Keep-alive regression failed: $log"
        }
        Write-Host "PASS $scenario / $program / $run"
    }
}
Write-Host "Reports: $output"
