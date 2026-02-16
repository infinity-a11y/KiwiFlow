#-----------------------------#
# Script Initialization
#-----------------------------#
param(
    [string]$basePath,
    [string]$userDataPath,
    [string]$envName,
    [string]$logFile,
    [string]$installScope
)

$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue"

# Start logging
Start-Transcript -Path $logFile -Append -Force | Out-Null

Write-Host "### Setting up Conda Environment (conda_env.ps1)"

# Source functions
. "$basePath\functions.ps1"

$condaCmd = Find-CondaExecutable
$environmentYmlPath = Join-Path $basePath "resources\environment.yml"

# Explicitly set free channel priority
& $condaCmd config --set solver libmamba
& $condaCmd config --add channels conda-forge
& $condaCmd config --set channel_priority strict

try {
    Write-Host "Synchronizing environment: $envName"
    # Create or Update
    & $condaCmd env update -n $envName -f "$environmentYmlPath" --prune
    Write-Host "Environment synchronized successfully."
}
catch {
    Write-Host "Error: $($_.Exception.Message)"
    exit 1
}

Stop-Transcript
exit 0