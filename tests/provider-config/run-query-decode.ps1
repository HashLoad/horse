param([string[]]$Versions = @('17.0', '22.0', '23.0', '37.0'))
$ErrorActionPreference = 'Stop'
$root = (Resolve-Path (Join-Path $PSScriptRoot '../..')).Path
$output = Join-Path $root ('benchmarks/results/query-decode-' + (Get-Date -Format 'yyyyMMdd-HHmmss'))
New-Item -ItemType Directory -Force $output | Out-Null
$count = 0
Push-Location $PSScriptRoot
try {
    foreach ($version in $Versions) {
        $studio = "C:/Program Files (x86)/Embarcadero/Studio/$version"
        if (-not (Test-Path "$studio/bin/dcc32.exe")) { continue }
        foreach ($provider in @('Default', 'HttpSys', 'IOCP')) {
            foreach ($router in @('Tree', 'Radix')) {
                $scenario = Join-Path $output "$version-$provider-$router"
                New-Item -ItemType Directory -Force $scenario | Out-Null
                $defines = 'HORSE_CONSOLE;HORSE_TEST_ISOLATED_QUERY'
                if ($provider -eq 'HttpSys') { $defines += ';HORSE_PROVIDER_HTTPSYS' }
                if ($provider -eq 'IOCP') { $defines += ';HORSE_PROVIDER_IOCP' }
                if ($router -eq 'Radix') { $defines += ';HORSE_RADIX_ROUTER' }
                & "$studio/bin/dcc32.exe" -B -Q "-D$defines" '-NSSystem;Xml;Data;Datasnap;Web;Soap;Winapi' "-U$root/src;$studio/lib/win32/release;$studio/source/DUnitX" "-E$scenario" "-N0$scenario" QueryDecodeCheck.dpr *> "$scenario/build.log"
                if ($LASTEXITCODE -ne 0) { throw "Compilation failed: $scenario/build.log" }
                & "$scenario/QueryDecodeCheck.exe" "--xml:$scenario/results.xml" *> "$scenario/run.log"
                $code = $LASTEXITCODE
                [xml]$report = Get-Content "$scenario/results.xml"
                $results = $report.'test-results'
                if ($code -ne 0 -or [int]$results.total -eq 0 -or
                    [int]$results.failures -gt 0 -or [int]$results.errors -gt 0 -or
                    (Get-Content "$scenario/run.log" -Raw) -match 'Unexpected Memory Leak') {
                    throw "Query/body regression failed: $scenario"
                }
                $count++
                Write-Host "PASS $version / $provider / $router ($($results.total) tests)"
            }
        }
    }
    if ($count -eq 0) { throw 'No installed Delphi compiler was exercised.' }
    Write-Host "PASS $count scenarios. Reports: $output"
} finally {
    Pop-Location
}
