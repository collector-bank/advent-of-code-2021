ghc .\Main.hs -O3
$d = [System.Diagnostics.StopWatch]::StartNew(); .\Main.exe; $d.Elapsed