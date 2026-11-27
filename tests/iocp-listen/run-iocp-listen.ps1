param(
    [string]$Version = '19.0',
    [string]$HorseSource = '',
    [string[]]$Tests = @(),
    [int]$TestTimeout = 120
)

# Listen cycle of the IOCP provider, Windows only.
# -Version picks the RAD Studio folder (19.0 = Delphi 10.2 Tokyo).
# -HorseSource points to another src folder for the negative control.
# -Tests runs only the named methods of TTestHorseProviderIOCPListen, each in a
# process of its own; without it the whole IOCPUnitTests runs once.
# -TestTimeout is how many seconds each of those processes gets (120).

$ErrorActionPreference = 'Stop'
# powershell -File hands "-Tests A,B" over as one string.
$Tests = @($Tests | ForEach-Object { $_ -split ',' } | Where-Object { $_ })
$root = (Resolve-Path (Join-Path $PSScriptRoot '../..')).Path
if (-not $HorseSource) { $HorseSource = Join-Path $root 'src' }
$HorseSource = (Resolve-Path -LiteralPath $HorseSource).Path
$studio = "C:/Program Files (x86)/Embarcadero/Studio/$Version"
$compiler = "$studio/bin/dcc32.exe"
$library = "$studio/lib/win32/release"
$dunitx = "$studio/source/DUnitX"
$output = Join-Path $root ('benchmarks/results/iocp-listen-' + $Version + '-' + (Get-Date -Format 'yyyyMMdd-HHmmss'))
New-Item -ItemType Directory -Force -Path $output | Out-Null
# Full path with the separators Windows gives Process.Path, so that an orphan
# holder is recognised below.
$output = [System.IO.Path]::GetFullPath($output)

$search = @($HorseSource, (Join-Path $root 'tests/src/tests'), $dunitx, $library)
$programs = @(
    (Join-Path $PSScriptRoot 'IocpListenHolder.dpr'),
    (Join-Path $root 'tests/src/IOCPUnitTests.dpr')
)
foreach ($program in $programs) {
    $name = [System.IO.Path]::GetFileNameWithoutExtension($program)
    $arguments = @('-B', '-Q', '-NSSystem;Xml;Data;Datasnap;Web;Soap;Winapi',
        "-U$($search -join ';')", "-E$output", "-N0$output", '-DHORSE_PROVIDER_IOCP', $program)
    & $compiler @arguments *> "$output/build-$name.log"
    if ($LASTEXITCODE -ne 0) { throw "Pascal compilation failed: $output/build-$name.log" }
}

function Wait-PortFree {
    $deadline = (Get-Date).AddSeconds(5)
    while (Get-NetTCPConnection -LocalPort 19266 -State Listen -ErrorAction SilentlyContinue) {
        if ((Get-Date) -ge $deadline) {
            throw 'Test port 19266 is in use; no process was stopped.'
        }
        Start-Sleep -Milliseconds 100
    }
}

# Runs IOCPUnitTests once with the given arguments, writes its log and
# returns its exit code. A run that does not end in time is killed and
# recorded as such, so that the next runs still happen.
function Invoke-Tests([string]$log, [string[]]$arguments, [int]$timeoutMs) {
    Wait-PortFree
    $start = @{
        FilePath = "$output/IOCPUnitTests.exe"; WorkingDirectory = $output; WindowStyle = 'Hidden'
        PassThru = $true; RedirectStandardOutput = $log; RedirectStandardError = "$log.err"
    }
    if ($arguments.Count -gt 0) { $start.ArgumentList = $arguments }
    $process = Start-Process @start
    try {
        if ($process.WaitForExit($timeoutMs)) {
            $status = $process.ExitCode
        } else {
            Stop-Process -Id $process.Id -Force
            $status = -1
        }
    } finally {
        $process.Dispose()
        Get-Process -Name IocpListenHolder -ErrorAction SilentlyContinue |
            Where-Object { $_.Path -like "$output*" } | Stop-Process -Force
    }
    if ((Test-Path "$log.err") -and (Get-Item "$log.err").Length -gt 0) {
        Get-Content "$log.err" | Add-Content $log
    }
    if ($status -eq -1) {
        Add-Content $log "TIMEOUT: killed after $($timeoutMs / 1000) s"
    }
    return $status
}

$log = "$output/run.log"
if ($Tests.Count -eq 0) {
    $status = Invoke-Tests $log @() 600000
} else {
    $status = 0
    Set-Content $log ''
    $index = 0
    foreach ($test in $Tests) {
        $index++
        $testLog = "$output/run-$index-$test.log"
        $code = Invoke-Tests $testLog @("--run:Tests.Horse.Provider.IOCP.Listen.TTestHorseProviderIOCPListen.$test") ($TestTimeout * 1000)
        Add-Content $log "== $test (exit $code)"
        Get-Content $testLog | Add-Content $log
        # Exactly the one named test has to have run.
        if ($code -ne -1 -and -not (Select-String -Path $testLog -Pattern 'Tests Found\s*:\s*1\s*$' -Quiet)) {
            Add-Content $log "NOT ISOLATED: the run did not find exactly one test"
            $code = 1
        }
        if ($code -ne 0) { $status = 1 }
    }
}

# What Windows answered (codes and connections), written by the tests.
$observations = Join-Path $output 'iocp-listen-observations.log'
if (Test-Path $observations) {
    Add-Content $log ''
    Add-Content $log 'Observations:'
    Get-Content -Encoding UTF8 $observations | Add-Content $log
}
Get-Content $log
if ($status -ne 0) {
    throw "IOCP listen check failed: $log"
}
Write-Host "PASS $Version / IOCPUnitTests"
Write-Host "Report: $output"
