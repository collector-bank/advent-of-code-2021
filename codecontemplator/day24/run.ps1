param([switch]$compileOnly, [switch]$noGenerate)

if (!$noGenerate) {
    del .\temp.cpp; g++ .\main.cpp -o main.exe; .\main.exe; 
}
#g++ .\temp.cpp -o temp.exe -O3
del .\temp.exe 
clang++ .\temp.cpp -o temp.exe -O3

if (-not($compileOnly)) {
    $d = [System.Diagnostics.Stopwatch]::StartNew();
    .\temp.exe
    $d.Elapsed
}
