param([string]$Version = '37.0', [string]$DockerImage = 'fpc-test:latest', [switch]$Radix)
$ErrorActionPreference = 'Stop'
$root = (Resolve-Path (Join-Path $PSScriptRoot '../..')).Path
$studio = "C:/Program Files (x86)/Embarcadero/Studio/$Version"
$library = "$studio/lib/linux64/release"
$router = if ($Radix) { 'Radix' } else { 'Tree' }
$defines = 'HORSE_DAEMON;HORSE_TEST_ISOLATED_LIFECYCLE'
if ($Radix) { $defines += ';HORSE_RADIX_ROUTER' }
$output = Join-Path $root ('benchmarks/results/daemon-lifecycle-' + $Version + '-' + $router + '-' + (Get-Date -Format 'yyyyMMdd-HHmmss'))
New-Item -ItemType Directory -Force $output | Out-Null
Push-Location $PSScriptRoot
try {
    & "$studio/bin/dcclinux64.exe" -B -Q --save-temps "-D$defines" '-NSSystem;Xml;Data;Datasnap;Web;Soap;Posix' "-U$root/src;$library;$studio/source/DUnitX" "-E$output" "-N0$output" ProviderLifecycle.dpr *> "$output/build.log"
    $compileCode = $LASTEXITCODE
    if ($compileCode -ne 0 -and
        ((Get-Content "$output/build.log" -Raw) -notmatch 'F2588 Linker error' -or
         -not (Test-Path "$output/ProviderLifecycle.lnk"))) {
        throw "Pascal compilation failed; see $output/build.log"
    }
    # The container supplies native Linux system libraries and the GNU linker.
    $dockerArgs = @('run', '--rm', '--mount', "type=bind,source=$output,target=/out",
        '--mount', "type=bind,source=$library,target=/delphi,readonly",
        '--mount', "type=bind,source=$PSScriptRoot,target=/scripts,readonly",
        '--entrypoint', 'bash', $DockerImage, '-c',
        'sed "s/\r$//" /scripts/link-and-run-daemon.sh > /tmp/run.sh; exec bash /tmp/run.sh')
    & docker @dockerArgs *> "$output/run.log"
    if ($LASTEXITCODE -ne 0) { throw "Linux link/runtime failure; see $output/run.log" }
    [xml]$report = Get-Content "$output/results.xml"
    $results = $report.'test-results'
    if ([int]$results.total -eq 0 -or [int]$results.failures -gt 0 -or
        [int]$results.errors -gt 0 -or
        (Get-Content "$output/run.log" -Raw) -match 'Unexpected Memory Leak') {
        throw "Daemon regression failed; see $output/results.xml"
    }
    Write-Host "Daemon Delphi $Version / $router PASS ($($results.total) tests). Reports: $output"
} finally {
    Pop-Location
}
