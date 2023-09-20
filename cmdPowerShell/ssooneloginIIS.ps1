# Verificar se o script está sendo executado como administrador
if (-not ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole] "Administrator")) {
    Start-Process powershell.exe "-NoProfile -ExecutionPolicy Bypass -File `"$PSCommandPath`"" -Verb RunAs
    exit
}

Import-Module WebAdministration

# Nome fixo do site e do aplicativo
$siteName = "559"
$appName = ".net"

# Solicitar informações do usuário
$basePhysicalPath = Read-Host "Digite a base do caminho fisico (por exemplo, C:\Apdata_X64)"

# Complementar os caminhos
$physicalPath = Join-Path $basePhysicalPath "\Aplicacoes\ApWebDispatcher\Site"
$appPhysicalPath = Join-Path $basePhysicalPath "\Aplicacoes\ApWebDispatcher\ApWebDispatcher"

$bindings = Read-Host "Digite as informacoes de binding (por exemplo, *:80:meusite.com)"
$username = Read-Host "Digite o nome de usuario para conectar"
$password = Read-Host "Digite a senha"

if (-not $bindings) {
    # Verificar se o diretório virtual já existe
    if (-not (Test-Path "IIS:\Sites\Default Web Site\$siteName")) {
        New-Item "IIS:\Sites\Default Web Site\$siteName" -type VirtualDirectory -physicalPath $physicalPath
    }

    # Verificar se o aplicativo já existe
    if (-not (Test-Path "IIS:\Sites\Default Web Site\$siteName\$appName")) {
        New-Item "IIS:\Sites\Default Web Site\$siteName\$appName" -type Application -physicalPath $appPhysicalPath
    }

    # Configurando a autenticação para o diretório virtual
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name='Default Web Site']/virtualDirectory[@path='/`$siteName']" -name "userName" -value $username
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name='Default Web Site']/virtualDirectory[@path='/`$siteName']" -name "password" -value $password
    
    # Configurando a autenticação para o aplicativo
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name='Default Web Site']/application[@path='/`$siteName/`$appName']" -name "userName" -value $username
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name='Default Web Site']/application[@path='/`$siteName/`$appName']" -name "password" -value $password
} else {
    # Verificar se o site já existe
    if (-not (Test-Path "IIS:\Sites\$siteName")) {
        New-Item "IIS:\Sites\$siteName" -physicalPath $physicalPath -bindings $bindings
    }

    # Configurando a autenticação para o site
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name=`"$siteName`"]/anonymousAuthentication" -name "userName" -value $username
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name=`"$siteName`"]/anonymousAuthentication" -name "password" -value $password
    
    # Verificar se o aplicativo já existe dentro do site
    if (-not (Test-Path "IIS:\Sites\$siteName\$appName")) {
        New-Item "IIS:\Sites\$siteName\$appName" -type Application -physicalPath $appPhysicalPath
    }
}

Write-Host "Site/diretorio virtual e aplicativo criados com sucesso!"
