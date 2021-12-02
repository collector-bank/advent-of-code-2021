$rows = Get-Content .\input.txt
$data = $rows | % { $dir, $amount = $_.split(' '); [PSCustomObject]@{ Dir = $dir; Amount = [int]$amount } }
$data | % { $x = 0; $y=0; $aim = 0} {
    $d = $_.Amount 
    switch ($_.Dir) {
        "forward" { $x += $d; $y += $aim * $d }
        "down" { $aim += $d }
        "up" { $aim -= $d }
    }
} { $x * $y }

