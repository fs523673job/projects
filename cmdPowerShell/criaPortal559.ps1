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
    # Criar ou atualizar o diretório virtual
    New-Item "IIS:\Sites\Default Web Site\$siteName" -type VirtualDirectory -physicalPath $physicalPath -Force

    # Configurar a identidade do diretório virtual
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name='Default Web Site']/virtualDirectory[@path='/`$siteName']" -name "userName" -value $username
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name='Default Web Site']/virtualDirectory[@path='/`$siteName']" -name "password" -value $password
	
    # Criar ou atualizar o aplicativo
    New-Item "IIS:\Sites\Default Web Site\$siteName\$appName" -type Application -physicalPath $appPhysicalPath -Force

    # Configurar a identidade do aplicativo
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name='Default Web Site']/application[@path='/`$siteName/`$appName']" -name "userName" -value $username
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name='Default Web Site']/application[@path='/`$siteName/`$appName']" -name "password" -value $password
	
	# Configurando as credenciais do caminho físico
	$virtualDirConfigPath = "/system.applicationHost/sites/site[@name='Default Web Site']/application[@path='/`$siteName/`$appName']/virtualDirectoryDefaults" 
	Set-WebConfiguration -filter $virtualDirConfigPath -value @{userName=$username; password=$password}
	
	$virtualDirConfigPath = "/system.applicationHost/sites/site[@name='Default Web Site']/virtualDirectory[@path='/`$siteName']/virtualDirectoryDefaults" 
	Set-WebConfiguration -filter $virtualDirConfigPath -value @{userName=$username; password=$password}
	

} else {
    # Criar ou atualizar o site
    New-Item "IIS:\Sites\$siteName" -physicalPath $physicalPath -bindings $bindings -Force

    # Configurar a identidade do site
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name=`"$siteName`"]" -name "userName" -value $username
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name=`"$siteName`"]" -name "password" -value $password

    # Criar ou atualizar o aplicativo dentro do site
    New-Item "IIS:\Sites\$siteName\$appName" -type Application -physicalPath $appPhysicalPath -Force
}

Write-Host "Site/diretorio virtual e aplicativo criados com sucesso!"
