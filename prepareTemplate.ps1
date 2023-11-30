[CmdletBinding()]
param(
    [Parameter(Mandatory)]
    [int] $Year
)

$template = (Get-Content -Path $PSScriptRoot\template.fs) -replace "YYYY", "$Year"

1..25 | ForEach-Object {
    $num = $_.ToString("00")

    $template -replace "XX", "$num" | Out-File -FilePath "$PSScriptRoot\$year\$num.fs"
}