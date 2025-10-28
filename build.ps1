<#
.SYNOPSIS
    Cross-platform set-up helper.

.DESCRIPTION
    Restores .NET tools, installs Paket dependencies, and invokes the build
    script (build.fsx) with the specified target.

.PARAMETER t
    Build target passed to build.fsx. Possible values are: "CleanDev", "Clean",
    "CleanFableJS", "CleanNode", "DotnetRestore", "NpmInstall", "Build", "Dev",
    "Dist", "DistDir", and "KillZombies".

.INPUTS
    None. build.ps1 does not take pipeline input.

.OUTPUTS
    None. build.ps1 doesn't generate any output.

.EXAMPLE
    PS> .\build.ps1
    Run the default development target (Dev)

.EXAMPLE
    PS> .\build.ps1 -t CleanDev
    Clean dev artifacts, restore dotnet, install npm deps and build

.EXAMPLE
    PS> .\build.ps1 -t NpmInstall
    Install npm dependencies (uses npm ci)
#>

param (
    [Parameter(Mandatory=$false)]
    [ValidateSet("CleanDev", "Clean", "CleanFableJS", "CleanNode", 
    "DotnetRestore", "NpmInstall", "Build", "Dev", "Dist", "DistDir", 
    "KillZombies")]
    [string]$t = "Dev"
)

dotnet tool restore
dotnet paket install
dotnet fsi build.fsx -t $t
