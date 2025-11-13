<#
.SYNOPSIS
    Cross-platform build script in PowerShell.

.DESCRIPTION
    Powershell script that calls "npm run dist" on different OSes and
    architectures, and collect the resulting built binaries into "dist" folder.

.PARAMETER Default
    Put all OS and architecture combinations into the build list.

.PARAMETER Add
    Add specified OS and architecture combination into the build list.

.PARAMETER Remove
    Remove specified OS and architecture combination from the build list.

.INPUTS
    None. build_dist.ps1 does not take pipeline input.

.OUTPUTS
    None. build_dist.ps1 doesn't generate any output.

.EXAMPLE 
    PS> .\build_dist.ps1 -Default
    Build for all default OS and architecture combinations

.EXAMPLE 
    PS> .\build_dist.ps1 -Add "mac:x64:dmg", "linux:arm64:zip"
    Build for specific targets (macOS x64 and Linux arm64)

.EXAMPLE
    PS> .\build_dist.ps1 -Default -Remove "win:x64:zip"
    Build for all default targets except Windows x64
#>

param (
    [Parameter(Mandatory=$false)]
    [switch]$Default = $false,

    [Parameter(Mandatory=$false)]
    [ValidateSet("mac:x64:dmg", "mac:arm64:dmg", "linux:x64:zip", `
                 "linux:arm64:zip", "win:x64:zip", "win:arm64:zip")]
    [String[]]$Add = @(),

    [Parameter(Mandatory=$false)]
    [ValidateSet("mac:x64:dmg", "mac:arm64:dmg", "linux:x64:zip", `
                 "linux:arm64:zip", "win:x64:zip", "win:arm64:zip")]
    [String[]]$Remove = @()
)

$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest

# Create temp directory for files to be published
Remove-Item -Path "dist_tmp" -Force -Recurse -ErrorAction "SilentlyContinue"
New-Item -Path "dist_tmp" -ItemType "Directory" `
    -ErrorAction "SilentlyContinue" | Out-Null

# Parse target list
$DefaultTargets = @("mac:x64:dmg", "mac:arm64:dmg", "linux:x64:zip", `
                    "linux:arm64:zip", "win:x64:zip", "win:arm64:zip")
$BuildTargets = @()
if ($Default) {
    $BuildTargets = $DefaultTargets
}
foreach ($Target in $Add) {
    $BuildTargets = $BuildTargets | Where-Object { $_ -ne $Target}
    $BuildTargets = $($BuildTargets; @($Target))
}
foreach ($Target in $Remove) {
    $BuildTargets = $BuildTargets | Where-Object { $_ -ne $Target}
}

# Build for each OS/arch combination
foreach ($Target in $BuildTargets) {
    $TargetArray = $Target -split ':'
    $Os = $TargetArray[0]
    $Arch = $TargetArray[1]
    $Filetype = $TargetArray[2]

    # TODO: This is a hack, this relies on `electron-builder` being the last 
    # command in the package.json `dist` script
    npm run dist -- "--$Os" "--$Arch" "-p" "never"

    # Find the output file
    $DistPath = Get-ChildItem -Path "dist" -Force -Filter "*.zip" `
        | Select-Object -ExpandProperty FullName
    if (-not $(Test-Path -Path $DistPath)) {
        Write-Host "No output found for target $Target, skipping..."
        continue
    }

    # Construct new name in dist_tmp
    $TempPath = $DistPath -replace "dist","dist_tmp" -replace ".$Filetype","" `
        -replace "-$Arch","" -replace "-$Os",""
    $TempPath = "$TempPath-$Os-$Arch.$Filetype"

    # Move file
    Move-Item -Path $DistPath -Destination $TempPath
}

# Replace dist with dist_tmp
Remove-Item -Path "dist" -Recurse -Force -ErrorAction "SilentlyContinue"
Rename-Item -Path "dist_tmp" -NewName "dist"
