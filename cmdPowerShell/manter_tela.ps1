$objeto = New-Object -com "wscript.Shell"
 
$i = 0
 
WHILE ($i -le 1200){
    $objeto.sendkeys("{F15}")
    Start-Sleep -s 30
    $i++
}