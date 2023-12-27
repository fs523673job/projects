# Verificar se o script está sendo executado como administrador
if (-not ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole] "Administrator")) {
    Start-Process powershell.exe "-NoProfile -ExecutionPolicy Bypass -File `"$PSCommandPath`"" -Verb RunAs
    exit
}

# Importar módulo necessário para o IIS
Import-Module WebAdministration

# Nome fixo do site e do aplicativo
$siteName = "559"
$appName = ".net"

# Verificar se o diretório virtual já existe dentro do "Default Web Site"
$virtualDirPath = "IIS:\Sites\Default Web Site\$siteName"
$virtualDirExists = Test-Path $virtualDirPath

if ($virtualDirExists) {
    $userResponse = Read-Host "O diretorio virtual $siteName ja existe dentro do 'Default Web Site'. Deseja deletar e criar um novo? (Sim/Nao)"
    if ($userResponse -eq "Nao") {
        Write-Host "Operacao cancelada pelo usuario."
        exit
    } else {
        Remove-WebAppPool $siteName
        Write-Host "Diretorio virtual $siteName deletado."
    }
}

# Solicitar informações do usuário
$basePhysicalPath = Read-Host "Digite a base do caminho fisico (por exemplo, C:\Apdata_X64)"
$physicalPath = Join-Path $basePhysicalPath "\Aplicacoes\ApWebDispatcher\Site"
$appPhysicalPath = Join-Path $basePhysicalPath "\Aplicacoes\ApWebDispatcher\ApWebDispatcher"

$bindings = Read-Host "Digite as informacoes de binding (por exemplo, *:80:meusite.com)"
$username = Read-Host "Digite o nome de usuario para conectar"
$password = Read-Host "Digite a senha"

if (-not $bindings) {
    # Criar ou atualizar o diretório virtual
    New-WebVirtualDirectory -Site "Default Web Site" -Name $siteName -PhysicalPath $physicalPath -Force

    # Configurar a identidade do diretório virtual
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name='Default Web Site']/virtualDirectory[@path='/`$siteName']" -name "userName" -value $username
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name='Default Web Site']/virtualDirectory[@path='/`$siteName']" -name "password" -value $password
	
    # Criar ou atualizar o aplicativo
    New-WebApplication -Name $appName -Site "Default Web Site" -PhysicalPath $appPhysicalPath -ApplicationPool $siteName -Force

    # Configurar a identidade do aplicativo
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name='Default Web Site']/application[@path='/`$siteName/`$appName']" -name "userName" -value $username
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name='Default Web Site']/application[@path='/`$siteName/`$appName']" -name "password" -value $password

} else {
    # Criar ou atualizar o site com bindings
    New-WebSite -Name $siteName -PhysicalPath $physicalPath -Port $bindings.Split(':')[1] -HostHeader $bindings.Split(':')[2] -Force

    # Configurar a identidade do site
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name=`"$siteName`"]" -name "userName" -value $username
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name=`"$siteName`"]" -name "password" -value $password

    # Criar ou atualizar o aplicativo dentro do site
    New-WebApplication -Name $appName -Site $siteName -PhysicalPath $appPhysicalPath -ApplicationPool $siteName -Force
}

Write-Host "Site/diretorio virtual e aplicativo criados com sucesso!"
