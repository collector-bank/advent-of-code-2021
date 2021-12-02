$rows = Get-Content .\input.txt
$data = $rows | % { $dir, $amount = $_.split(' '); [PSCustomObject]@{ Dir = $dir; Amount = [int]$amount } }
$data | % { $x = 0; $y=0} {
    $d = $_.Amount 
    switch ($_.Dir) {
        "forward" { $x += $d }
        "down" { $y += $d }
        "up" { $y -= $d }
    }
} { $x * $y }

