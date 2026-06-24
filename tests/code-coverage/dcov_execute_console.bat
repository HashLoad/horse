@echo off
"..\code-coverage\CodeCoverage.exe" ^
-e "..\Console.exe" ^
-m "..\Console.map" ^
-dproj "..\src\console.dproj" ^
-ife ^
-uf dcov_units.lst ^
-spf dcov_paths.lst ^
-od ".\Console\" ^
-emma ^
-meta ^
-xml ^
-html