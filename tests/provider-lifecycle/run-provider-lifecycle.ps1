param([string[]]$Versions = @())
$ErrorActionPreference = 'Stop'
$root = (Resolve-Path (Join-Path $PSScriptRoot '../..')).Path
$studio = 'C:/Program Files (x86)/Embarcadero/Studio'
$failures = 0
$executions = 0
foreach ($installation in Get-ChildItem $studio -Directory) {
    if ($Versions.Count -gt 0 -and $installation.Name -notin $Versions) { continue }
    $compiler = Join-Path $installation.FullName 'bin/dcc32.exe'
    if (-not (Test-Path $compiler)) { continue }
    foreach ($provider in @('Console', 'VCL')) {
        foreach ($radix in @($false, $true)) {
            $label = "$($installation.Name)-$provider-radix-$radix"
            $executions++
            $output = Join-Path $root "benchmarks/results/provider-lifecycle/$label"
            New-Item -ItemType Directory -Force $output | Out-Null
            $defines = 'HORSE_TEST_ISOLATED_LIFECYCLE'
            if ($provider -eq 'VCL') { $defines += ';HORSE_VCL' }
            if ($radix) { $defines += ';HORSE_RADIX_ROUTER' }
            $library = Join-Path $installation.FullName 'lib/win32/release'
            Push-Location $PSScriptRoot
            try {
                & $compiler -B -Q "-D$defines" '-NSSystem;Xml;Data;Datasnap;Web;Soap;Winapi;Vcl' "-U$root/src;$library" "-E$output" "-N0$output" ProviderLifecycle.dpr *> "$output/build.log"
                if ($LASTEXITCODE -ne 0) { throw "Compilation failed: $output/build.log" }
                & "$output/ProviderLifecycle.exe" "--xml:$output/results.xml" *> "$output/run.log"
                $code = $LASTEXITCODE
                [xml]$report = Get-Content "$output/results.xml"
                $results = $report.'test-results'
                if ($code -ne 0 -or [int]$results.total -eq 0 -or
                    [int]$results.failures -gt 0 -or [int]$results.errors -gt 0 -or
                    (Get-Content "$output/run.log" -Raw) -match 'Unexpected Memory Leak') {
                    throw "Runtime failure: $output/run.log"
                }
                Write-Host "$label PASS ($($results.total) tests)"
            } catch {
                $failures++
                Write-Host "$label FAIL: $_"
            } finally {
                Pop-Location
            }
        }
    }
}
if ($executions -eq 0) { throw 'No Delphi installations selected.' }
if ($failures -gt 0) { exit 1 }
