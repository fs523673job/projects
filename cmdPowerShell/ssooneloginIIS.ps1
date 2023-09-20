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
    # Criando o diretório virtual sob "Default Web Site"
    $virtualDir = New-Item "IIS:\Sites\Default Web Site\$siteName" -type VirtualDirectory -physicalPath $physicalPath -ErrorAction SilentlyContinue
    if (-not $virtualDir) {
        Write-Host "Falha ao criar o diretório virtual."
        exit
    }
    
    # Criando o aplicativo dentro do diretório virtual
    New-Item "IIS:\Sites\Default Web Site\$siteName\$appName" -type Application -physicalPath $appPhysicalPath
    
    # Configurando a autenticação para o diretório virtual
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name='Default Web Site']/virtualDirectory[@path='/`$siteName']" -name "userName" -value $username
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name='Default Web Site']/virtualDirectory[@path='/`$siteName']" -name "password" -value $password
    
    # Configurando a autenticação para o aplicativo
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name='Default Web Site']/application[@path='/`$siteName/`$appName']" -name "userName" -value $username
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name='Default Web Site']/application[@path='/`$siteName/`$appName']" -name "password" -value $password
} else {
    # Criando o site com o binding fornecido
    $site = New-Item "IIS:\Sites\$siteName" -physicalPath $physicalPath -bindings $bindings -ErrorAction SilentlyContinue
    if (-not $site) {
        Write-Host "Falha ao criar o site."
        exit
    }

    # Configurando a autenticação para o site
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name=`"$siteName`"]/anonymousAuthentication" -name "userName" -value $username
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name=`"$siteName`"]/anonymousAuthentication" -name "password" -value $password
    
    # Criando o aplicativo dentro do site
    New-Item "IIS:\Sites\$siteName\$appName" -type Application -physicalPath $appPhysicalPath
}

Write-Host "Site/diretorio virtual e aplicativo criados com sucesso!"
