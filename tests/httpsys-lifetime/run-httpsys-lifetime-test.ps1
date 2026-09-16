$ErrorActionPreference = 'Stop'

$testDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$horseRoot = (Resolve-Path (Join-Path $testDir '..\..')).Path
$outputDir = Join-Path ([System.IO.Path]::GetTempPath()) ('horse-httpsys-lifetime-' + [Guid]::NewGuid().ToString('N'))
$compiler = if ($env:FPC) { $env:FPC } else { 'C:\lazarus\fpc\3.2.2\bin\i386-win32\fpc.exe' }

New-Item -ItemType Directory -Path $outputDir | Out-Null
try {
  & $compiler -B -Mdelphi -Sh -gh -gl '-dHORSE_PROVIDER_HTTPSYS' '-dHORSE_CONSOLE' `
    "-Fu$horseRoot\src" "-FU$outputDir" "-FE$outputDir" `
    (Join-Path $testDir 'HttpSysLifetimeCheck.lpr')
  if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

  $executable = Join-Path $outputDir 'HttpSysLifetimeCheck.exe'
  $output = & cmd.exe /d /c ('"' + $executable + '" 2>&1') | Out-String
  $status = $LASTEXITCODE
  Write-Output $output
  if ($status -ne 0) { exit $status }
  if ($output -match '[1-9][0-9]* unfreed memory blocks') {
    throw 'HTTP.sys lifetime regression: FPC reported unfreed memory.'
  }
  Write-Output 'HTTP.sys lifetime regression passed: no FPC heap leak report.'
}
finally {
  Remove-Item -LiteralPath $outputDir -Recurse -Force -ErrorAction SilentlyContinue
}
