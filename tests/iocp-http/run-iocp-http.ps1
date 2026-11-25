param(
    [string]$Version = '19.0',
    [string]$HorseSource = ''
)

# HTTP over the IOCP provider, Windows only.
# -Version picks the RAD Studio folder (19.0 = Delphi 10.2 Tokyo).
# -HorseSource points to another src folder for the negative control.

$ErrorActionPreference = 'Stop'
$root = (Resolve-Path (Join-Path $PSScriptRoot '../..')).Path
if (-not $HorseSource) { $HorseSource = Join-Path $root 'src' }
$HorseSource = (Resolve-Path -LiteralPath $HorseSource).Path
$studio = "C:/Program Files (x86)/Embarcadero/Studio/$Version"
$compiler = "$studio/bin/dcc32.exe"
$library = "$studio/lib/win32/release"
$output = Join-Path $root ('benchmarks/results/iocp-http-' + $Version + '-' + (Get-Date -Format 'yyyyMMdd-HHmmss'))
New-Item -ItemType Directory -Force -Path $output | Out-Null

$search = @($HorseSource, (Join-Path $root 'src'), $library)
$program = 'IocpHttpCheck'
$arguments = @('-B', '-Q', '-NSSystem;Xml;Data;Datasnap;Web;Soap;Winapi',
    "-U$($search -join ';')", "-E$output", "-N0$output", '-DHORSE_PROVIDER_IOCP',
    (Join-Path $PSScriptRoot "$program.dpr"))
& $compiler @arguments *> "$output/build.log"
if ($LASTEXITCODE -ne 0) { throw "Pascal compilation failed: $output/build.log" }

$portDeadline = (Get-Date).AddSeconds(5)
while (Get-NetTCPConnection -LocalPort 19265 -State Listen -ErrorAction SilentlyContinue) {
    if ((Get-Date) -ge $portDeadline) {
        throw 'Test port 19265 is in use; no process was stopped.'
    }
    Start-Sleep -Milliseconds 100
}

$log = "$output/run.log"
$process = Start-Process -FilePath "$output/$program.exe" -WindowStyle Hidden `
    -PassThru -RedirectStandardOutput $log -RedirectStandardError "$log.err"
try {
    if (-not $process.WaitForExit(600000)) {
        Stop-Process -Id $process.Id -Force
        throw "IOCP HTTP check timed out: $log"
    }
    $status = $process.ExitCode
} finally {
    $process.Dispose()
}
if ((Test-Path "$log.err") -and (Get-Item "$log.err").Length -gt 0) {
    Get-Content "$log.err" | Add-Content $log
}
Get-Content $log
if ($status -ne 0 -or (Get-Content $log -Raw) -notmatch 'PASS:') {
    throw "IOCP HTTP check failed: $log"
}
Write-Host "PASS $Version / $program"
Write-Host "Report: $output"
