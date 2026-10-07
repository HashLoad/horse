param(
    [Parameter(Mandatory)][string]$RuntimeRoot,
    [string[]]$Versions = @('17.0', '22.0', '23.0', '37.0'),
    [int]$Port = 19280,
    [string]$Python = 'python'
)
$ErrorActionPreference = 'Stop'
$root = (Resolve-Path (Join-Path $PSScriptRoot '../..')).Path
$runtime = (Resolve-Path $RuntimeRoot).Path
if (-not (Test-Path "$runtime/config/redirection.config")) {
    $testRuntimeRoot = Join-Path $root 'benchmarks/results/'
    if (-not $runtime.StartsWith($testRuntimeRoot.Replace('/', '\'), [StringComparison]::OrdinalIgnoreCase)) {
        throw 'Runtime has no redirection.config; use a complete runtime extracted under benchmarks/results.'
    }
    Copy-Item "$runtime/config/templates/PersonalWebServer/redirection.config" "$runtime/config/redirection.config"
}
$output = Join-Path $root ('benchmarks/results/iis-hosted-' + (Get-Date -Format 'yyyyMMdd-HHmmss'))
New-Item -ItemType Directory -Force $output | Out-Null
$previousBin = $env:IIS_BIN
$previousHome = $env:IIS_USER_HOME
$count = 0
try {
    foreach ($version in $Versions) {
        $studio = "C:/Program Files (x86)/Embarcadero/Studio/$version"
        if (-not (Test-Path "$studio/bin/dcc32.exe")) { throw "Missing Delphi compiler: $version" }
        foreach ($provider in @('ISAPI', 'CGI')) {
            foreach ($router in @('Tree', 'Radix')) {
                if (Get-NetTCPConnection -LocalPort $Port -State Listen -ErrorAction SilentlyContinue) {
                    throw "Port $Port is occupied; no unrelated process will be stopped."
                }
                $scenario = Join-Path $output "$version-$provider-$router"
                New-Item -ItemType Directory -Force $scenario | Out-Null
                $defines = "HORSE_$provider"
                if ($router -eq 'Radix') { $defines += ';HORSE_RADIX_ROUTER' }
                Push-Location $PSScriptRoot
                try {
                    & "$studio/bin/dcc32.exe" -B -Q "-D$defines" '-NSSystem;Xml;Data;Datasnap;Web;Soap;Winapi' "-U$root/src;$studio/lib/win32/release" "-E$scenario" "-N0$scenario" HostedProviderCheck.dpr *> "$scenario/build.log"
                    if ($LASTEXITCODE -ne 0) { throw "Build failed: $scenario/build.log" }
                } finally { Pop-Location }
                $extension = if ($provider -eq 'ISAPI') { 'dll' } else { 'exe' }
                $binary = Join-Path $scenario "HostedProviderCheck.$extension"
                [xml]$config = Get-Content "$runtime/config/templates/PersonalWebServer/applicationhost.config"
                $site = $config.configuration.'system.applicationHost'.sites.site
                $site.SetAttribute('name', 'HorseHosted')
                $site.application.SetAttribute('applicationPool', 'UnmanagedClassicAppPool')
                $site.application.virtualDirectory.SetAttribute('physicalPath', $scenario)
                $site.bindings.binding.SetAttribute('bindingInformation', ":${Port}:localhost")
                $location = $config.SelectSingleNode('/configuration/location[@path=""]/system.webServer')
                $handlers = $location.SelectSingleNode('handlers')
                $handlers.RemoveAll()
                $handlers.SetAttribute('accessPolicy', 'Read, Script, Execute')
                $handler = $config.CreateElement('add')
                $module = if ($provider -eq 'ISAPI') { 'IsapiModule' } else { 'CgiModule' }
                foreach ($entry in @{name='Horse';path='*';verb='*';modules=$module;scriptProcessor=$binary;resourceType='Unspecified';requireAccess='None';allowPathInfo='true'}.GetEnumerator()) {
                    $handler.SetAttribute($entry.Key, $entry.Value)
                }
                [void]$handlers.AppendChild($handler)
                $modules = $location.SelectSingleNode('modules')
                $modules.RemoveAll()
                foreach ($name in @('AnonymousAuthenticationModule', $module)) {
                    $node = $config.CreateElement('add'); $node.SetAttribute('name', $name)
                    [void]$modules.AppendChild($node)
                }
                $restriction = $config.configuration.'system.webServer'.security.isapiCgiRestriction
                $restriction.SetAttribute('notListedIsapisAllowed', 'true')
                $restriction.SetAttribute('notListedCgisAllowed', 'true')
                foreach ($node in $config.SelectNodes('//*[@directory]')) {
                    $node.SetAttribute('directory', $scenario)
                }
                $configPath = Join-Path $scenario 'applicationhost.config'
                $config.Save($configPath)
                $env:IIS_BIN = $runtime
                $env:IIS_USER_HOME = $scenario
                New-Item -ItemType Directory -Force "$scenario/config" | Out-Null
                Copy-Item "$runtime/config/redirection.config" "$scenario/config/redirection.config"
                $hostProcess = Start-Process "$runtime/iisexpress.exe" -ArgumentList "/config:`"$configPath`"", "/userhome:`"$scenario`"", '/site:HorseHosted', '/systray:false' -WindowStyle Hidden -PassThru -RedirectStandardOutput "$scenario/host.log" -RedirectStandardError "$scenario/host-error.log"
                try {
                    $ready = $false
                    for ($attempt = 0; $attempt -lt 30; $attempt++) {
                        if ($hostProcess.HasExited) { throw "IIS exited: $scenario/host.log" }
                        try {
                            $response = Invoke-WebRequest "http://localhost:$Port/ping" -TimeoutSec 2
                            if ($response.Content -eq 'pong') { $ready = $true; break }
                        } catch { $lastError = $_ }
                        Start-Sleep -Milliseconds 100
                    }
                    if (-not $ready) {
                        $lastError.ErrorDetails.Message | Set-Content "$scenario/readiness-response.log"
                        throw "IIS readiness failed: $lastError; see $scenario"
                    }
                    & $Python "$PSScriptRoot/check-http.py" "http://localhost:$Port" *> "$scenario/check.log"
                    if ($LASTEXITCODE -ne 0) { throw "HTTP regression failed: $scenario/check.log" }
                    $count++
                    Write-Host "PASS $version / $provider / $router (six HTTP checks)"
                } finally {
                    if (-not $hostProcess.HasExited) { Stop-Process -Id $hostProcess.Id; $hostProcess.WaitForExit() }
                    $hostProcess.Dispose()
                }
            }
        }
    }
    if ($count -ne ($Versions.Count * 4)) { throw 'Incomplete CGI/ISAPI matrix.' }
    Write-Host "PASS $count hosted scenarios. Reports: $output"
} finally {
    $env:IIS_BIN = $previousBin
    $env:IIS_USER_HOME = $previousHome
}
