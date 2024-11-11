# Lista de nomes de aplicações para monitorar as portas
#$applications = @("ApServer", "ApLoadBalancerServer", "ApIntegrationServer","bds")  
$applications = @("ApLoadBalancerServer")  
 
$intervalo = 10
 
#monitoramento continuo
while ($true) {
    Clear-Host
    Write-Output "Monitoramento de Conexões`n"
 
    # Obtém as conexões do netstat com PID
    $connections = netstat -aon | Select-String -Pattern "ESTABLISHED|TIME_WAIT"
 
    foreach ($app in $applications) {
        # Lista os PIDs da lista de aplicacoes
        $pids = (Get-Process -Name $app -ErrorAction SilentlyContinue | Select-Object -ExpandProperty Id)
 
        if ($pids) {
            foreach ($taskid in $pids) {
                # Conexões ESTABLISHED
                $establishedCount = ($connections | Select-String -Pattern $taskid | Select-String -Pattern "ESTABLISHED").Count
                # Conexões TIME_WAIT
                $timeWaitCount = ($connections | Select-String -Pattern $taskid | Select-String -Pattern "TIME_WAIT").Count
 
                # Contagens por aplicação e por estado
                Write-Output "Aplicação: $app (PID: $taskid)"
                Write-Output "    Conexões ESTABLISHED: $establishedCount"
                Write-Output "    Conexões TIME_WAIT: $timeWaitCount"
                Write-Output "`n"
 
            }
        } else {
            Write-Output "Aplicação: $app não está em execução.`n"
        }
    }
 
    # Intervalo de monitoracao
    Start-Sleep -Seconds $intervalo
}