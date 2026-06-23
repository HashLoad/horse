@echo off
"..\code-coverage\CodeCoverage.exe" ^
-e "..\VCL.exe" ^
-m "..\VCL.map" ^
-dproj "..\src\VCL.dproj" ^
-ife ^
-uf dcov_units.lst ^
-spf dcov_paths.lst ^
-od ".\VCL\" ^
-emma ^
-meta ^
-xml ^
-html