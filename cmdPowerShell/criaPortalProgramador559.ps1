if (-not ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole] "Administrator")) {
    Start-Process powershell.exe "-NoProfile -ExecutionPolicy Bypass -File `"$PSCommandPath`"" -Verb RunAs
    exit
}

Import-Module WebAdministration

$siteName = "559"
$appName = ".net"

$virtualDirPath = "IIS:\Sites\Default Web Site\$siteName"
$virtualDirExists = Test-Path $virtualDirPath

if ($virtualDirExists) {
    $userResponse = Read-Host "O diretorio virtual $siteName ja existe dentro do 'Default Web Site'. Deseja deletar e criar um novo? (Sim/Nao)"
    if ($userResponse -eq "Nao") {
        $siteName = Read-Host "Digite um novo nome para o site"
        $virtualDirPath = "IIS:\Sites\Default Web Site\$siteName"
        $virtualDirExists = Test-Path $virtualDirPath
        
        while ($virtualDirExists) {
            Write-Host "O nome $siteName já existe. Digite um nome diferente."
            $siteName = Read-Host "Digite um novo nome para o site"
            $virtualDirPath = "IIS:\Sites\Default Web Site\$siteName"
            $virtualDirExists = Test-Path $virtualDirPath
        }
    } else {
        Remove-Item $virtualDirPath -Recurse -Force
        Write-Host "Diretorio virtual $siteName deletado."
    }
}

$basePhysicalPath = Read-Host "Digite a base do caminho fisico (por exemplo, C:\Apdata_X64)"

$physicalPath = Join-Path $basePhysicalPath "\Aplicacoes\ApWebDispatcher\Site"
$appPhysicalPath = Join-Path $basePhysicalPath "\Aplicacoes\ApWebDispatcher\ApWebDispatcher"

$bindings = Read-Host "Digite as informacoes de binding (por exemplo, *:80:meusite.com)"
$username = Read-Host "Digite o nome de usuario para conectar"
$password = Read-Host "Digite a senha"

if (-not $bindings) {
    New-Item "IIS:\Sites\Default Web Site\$siteName" -type VirtualDirectory -physicalPath $physicalPath -Force

    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name='Default Web Site']/virtualDirectory[@path='/`$siteName']" -name "userName" -value $username
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name='Default Web Site']/virtualDirectory[@path='/`$siteName']" -name "password" -value $password
	
	$virtualDirConfigPath = "/system.applicationHost/sites/site[@name='Default Web Site']/virtualDirectory[@path='/`$siteName']"
	Set-WebConfiguration -filter "$virtualDirConfigPath/@userName" -value $username
	Set-WebConfiguration -filter "$virtualDirConfigPath/@password" -value $password	
	
	$virtualDirPath = "IIS:\Sites\Default Web Site\$siteName"

	Set-ItemProperty -Path $virtualDirPath -Name "userName" -Value $username
	Set-ItemProperty -Path $virtualDirPath -Name "password" -Value $password	
	
    New-Item "IIS:\Sites\Default Web Site\$siteName\$appName" -type Application -physicalPath $appPhysicalPath -Force

    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name='Default Web Site']/application[@path='/`$siteName/`$appName']" -name "userName" -value $username
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name='Default Web Site']/application[@path='/`$siteName/`$appName']" -name "password" -value $password

	$virtualDirConfigPathApp = "/system.applicationHost/sites/site[@name='Default Web Site']/application[@path='/`$siteName/`$appName']/virtualDirectory[@path='/']"
	Set-WebConfiguration -filter $virtualDirConfigPathApp -value @{userName=$username; password=$password}

} else {
    New-Item "IIS:\Sites\$siteName" -physicalPath $physicalPath -bindings $bindings -Force

    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name=`"$siteName`"]" -name "userName" -value $username
    Set-WebConfigurationProperty -filter "/system.applicationHost/sites/site[@name=`"$siteName`"]" -name "password" -value $password

    New-Item "IIS:\Sites\$siteName\$appName" -type Application -physicalPath $appPhysicalPath -Force
}

Write-Host "Site/diretorio virtual e aplicativo criados com sucesso!"
