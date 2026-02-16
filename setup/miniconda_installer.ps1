param(
    [string]$basePath,
    [string]$userDataPath,
    [string]$envName,
    [string]$logFile,
    [string]$installScope
)

$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue"

Start-Transcript -Path $logFile -Append | Out-Null

Write-Host "### Miniforge/Conda setup (miniconda_installer.ps1)"

# Source helper functions
. "$basePath\functions.ps1"

# Logic: Check for existing Conda
$condaCmd = Find-CondaExecutable
$foundScope = Get-CondaScope -CondaPath $condaCmd
$needsInstall = $false

if (-not $condaCmd) {
    Write-Host "No existing Conda found. Proceeding with installation."
    $needsInstall = $true
}
elseif ($installScope -eq "allusers" -and $foundScope -eq "currentuser") {
    Write-Host "Conflict: System-wide install requested, but found only User-specific Conda."
    $needsInstall = $true
}
else {
    Write-Host "Compatible Conda found: $condaCmd (Scope: $foundScope)"
    $needsInstall = $false
}

try {
    if ($needsInstall) {
        # Define installation prefix based on scope
        if ($installScope -eq "allusers") {
            $condaPrefix = "$env:ProgramData\miniforge3"
            $installType = "AllUsers"
        }
        else {
            $condaPrefix = "$env:LOCALAPPDATA\miniforge3"
            $installType = "JustMe"
        }

        $tempDir = Join-Path $env:TEMP "kiwims_setup"
        if (-not (Test-Path $tempDir)) { New-Item -ItemType Directory -Path $tempDir | Out-Null }
        
        $installer = Join-Path $tempDir "miniforge.exe"

        # Download Miniforge (Conda-Forge defaults)
        Write-Host "Downloading Miniforge installer..."
        $url = "https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-Windows-x86_64.exe"
        Download-File $url $installer

        # Execute Installation
        Write-Host "Starting silent installation to: $condaPrefix"
        $args = @("/S", "/InstallationType=$installType", "/RegisterPython=0", "/D=$condaPrefix")
        $process = Start-Process -FilePath $installer -ArgumentList $args -Wait -PassThru

        if ($process.ExitCode -ne 0) { throw "Installer failed with code $($process.ExitCode)" }
        
        Remove-Item $installer -Force -ErrorAction SilentlyContinue

        # Verify and Update PATH
        $condaCmd = Join-Path $condaPrefix "Scripts\conda.exe"
        if (-not (Test-Path $condaCmd)) { $condaCmd = Join-Path $condaPrefix "condabin\conda.bat" }

        $newPaths = "$condaPrefix;$condaPrefix\Scripts;$condaPrefix\Library\bin"
        $env:Path = $newPaths + ";" + $env:Path
        
        $target = if ($installScope -eq "allusers") { [System.EnvironmentVariableTarget]::Machine } else { [System.EnvironmentVariableTarget]::User }
        $currentPath = [Environment]::GetEnvironmentVariable("Path", $target)
        if ($currentPath -notlike "*$condaPrefix*") {
            [Environment]::SetEnvironmentVariable("Path", $currentPath + ";" + $newPaths, $target)
        }
    }
}
catch {
    Write-Host "CRITICAL ERROR: $($_.Exception.Message)"
    Stop-Transcript
    exit 1
}

Stop-Transcript
exit 0