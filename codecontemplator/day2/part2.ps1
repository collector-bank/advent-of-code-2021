$rows = Get-Content .\input.txt
$data = $rows | % { $dir, $amount = $_.split(' '); [PSCustomObject]@{ Dir = $dir; Amount =$amount } }
$data | % {[int]$x = 0; [int]$y=0; [int]$aim = 0} {
    $d = [int]$_.Amount 
    Write-Host $d
    switch ($_.Dir) {
        "forward" { $x += $d; $y += $aim * $d }
        "down" { $aim += $d }
        "up" { $aim -= $d }
    }
} { $x * $y }

