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
    $virtualDirPath = "IIS:\Sites\Default Web Site\$siteName"
    New-Item $virtualDirPath -type VirtualDirectory -physicalPath $physicalPath -ErrorAction SilentlyContinue
} else {
    # Criando o site com o binding fornecido
    New-Item "IIS:\Sites\$siteName" -physicalPath $physicalPath -bindings $bindings -ErrorAction SilentlyContinue
}

# Configurando a autenticação para o site/diretório virtual
Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name=`"$siteName`"]/anonymousAuthentication" -name "userName" -value $username -ErrorAction SilentlyContinue
Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name=`"$siteName`"]/anonymousAuthentication" -name "password" -value $password -ErrorAction SilentlyContinue

# Configurando as credenciais do caminho físico para o diretório virtual
$virtualDirConfigPath = if ($bindings) { "/system.applicationHost/sites/site[@name=`"$siteName`"]/virtualDirectoryDefaults" } else { "/system.applicationHost/sites/site[@name='Default Web Site']/virtualDirectory[@path='/`$siteName']" }
Set-WebConfiguration -filter $virtualDirConfigPath -value @{userName=$username; password=$password}

# Criando o aplicativo dentro do site/diretório virtual
if (-not (Test-Path "$virtualDirPath\$appName")) {
    New-Item "$virtualDirPath\$appName" -type Application -physicalPath $appPhysicalPath
}

# Configurando as credenciais do caminho físico para o aplicativo
$appConfigPath = "$virtualDirConfigPath/application[@path='/`$appName']"
Set-WebConfiguration -filter $appConfigPath -value @{userName=$username; password=$password}

Write-Host "Site/diretorio virtual e aplicativo criados com sucesso!"
