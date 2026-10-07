param([string[]]$Versions = @('23.0', '37.0'), [string]$DockerImage = 'horse-provider-tests:fpc-3.2.2')
$ErrorActionPreference = 'Stop'
$root = (Resolve-Path (Join-Path $PSScriptRoot '../..')).Path
$output = Join-Path $root ('benchmarks/results/apache-delphi-' + (Get-Date -Format 'yyyyMMdd-HHmmss'))
New-Item -ItemType Directory -Force $output | Out-Null
$count = 0
foreach ($version in $Versions) {
    $studio = "C:/Program Files (x86)/Embarcadero/Studio/$version"
    $library = "$studio/lib/linux64/release"
    if (-not (Test-Path "$studio/bin/dcclinux64.exe")) { throw "Missing compiler: $version" }
    foreach ($router in @('Tree', 'Radix')) {
        $scenario = Join-Path $output "$version-$router"
        New-Item -ItemType Directory -Force $scenario | Out-Null
        $defines = 'HORSE_APACHE'
        if ($router -eq 'Radix') { $defines += ';HORSE_RADIX_ROUTER' }
        Push-Location $PSScriptRoot
        try {
            & "$studio/bin/dcclinux64.exe" -B -Q --save-temps "-D$defines" '-NSSystem;Xml;Data;Datasnap;Web;Soap;Posix' "-U$root/src;$library" "-E$scenario" "-N0$scenario" HostedProviderCheck.dpr *> "$scenario/build.log"
            if ($LASTEXITCODE -ne 0 -and
                ((Get-Content "$scenario/build.log" -Raw) -notmatch 'F2588 Linker error' -or
                 -not (Test-Path "$scenario/HostedProviderCheck.lnk"))) {
                throw "Pascal compilation failed: $scenario/build.log"
            }
        } finally { Pop-Location }
        & docker run --rm --mount "type=bind,source=$scenario,target=/out" --mount "type=bind,source=$library,target=/delphi,readonly" --mount "type=bind,source=$root,target=/repo,readonly" --entrypoint bash $DockerImage /repo/tests/hosted-providers/link-and-run-delphi-apache.sh *> "$scenario/run.log"
        if ($LASTEXITCODE -ne 0) { throw "Apache runtime failed: $scenario/run.log" }
        $count++
        Write-Host "PASS Delphi $version / Apache / $router (six HTTP checks)"
    }
}
if ($count -ne ($Versions.Count * 2)) { throw 'Incomplete Apache matrix' }
Write-Host "PASS $count Apache scenarios. Reports: $output"
