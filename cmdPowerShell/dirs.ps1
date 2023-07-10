param(
    [Parameter(Mandatory=$true)]
    [string]$path
)

Get-ChildItem -Path $path -Recurse