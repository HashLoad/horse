$ErrorActionPreference = 'Stop'

$testDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$horseRoot = (Resolve-Path (Join-Path $testDir '..\..')).Path
$outputDir = Join-Path ([System.IO.Path]::GetTempPath()) ('horse-console-stability-' + [Guid]::NewGuid().ToString('N'))
$rsvars = if ($env:RADSTUDIO_RSVARS) { $env:RADSTUDIO_RSVARS } else { 'C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat' }

New-Item -ItemType Directory -Path $outputDir | Out-Null
try {
  $command = 'call "' + $rsvars + '" && dcc32.exe -B -Q ' +
    '-E"' + $outputDir + '" -NS"System;Xml;Data;Datasnap;Web;Soap;Winapi" ' +
    '-I"' + $horseRoot + '\src" -U"' + $horseRoot + '\src" ' +
    '"' + (Join-Path $testDir 'ConsoleStabilityCheck.dpr') + '"'
  & cmd.exe /d /c $command
  if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

  & (Join-Path $outputDir 'ConsoleStabilityCheck.exe')
  if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
}
finally {
  Remove-Item -LiteralPath $outputDir -Recurse -Force -ErrorAction SilentlyContinue
}
