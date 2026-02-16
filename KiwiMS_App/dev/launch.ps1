# Find the installation directory
if ($host.Name -eq "Visual Studio Code Host" -or $host.Name -eq "ConsoleHost") {
    # Running as a script (.ps1)
    $basePath = $PSScriptRoot
}
else {
    # Running as a compiled executable (.exe)
    $BinaryPath = [System.Diagnostics.Process]::GetCurrentProcess().MainModule.FileName
    $basePath = Split-Path $BinaryPath -Parent
}

# Set directory to app base path
Set-Location $basePath

# Source helper functions
if (Test-Path "$basePath\functions.ps1") {
    . "$basePath\functions.ps1"
}
else {
    # Attempt fallback to current directory
    if (Test-Path "functions.ps1") {
        . ".\functions.ps1"
    }
    else {
        Add-Type -AssemblyName System.Windows.Forms
        [System.Windows.Forms.MessageBox]::Show("Critical Error: 'functions.ps1' not found in $basePath. Please reinstall the application.", "KiwiMS Launcher Error")
        exit 1
    }
}

#-----------------------------#
# Application Metadata
#-----------------------------#
$versionFile = if (Test-Path "resources\version.txt") { Get-Content -Path "resources\version.txt" | Select-Object -First 1 } else { "v0.3.1" }
$Headless = $args -contains "--headless"

# Branding
Write-Host ""
Write-Host "██╗  ██╗ ██╗            ██╗    ███╗   ███╗  ██████╗ " -ForegroundColor Gray
Write-Host "██║ ██╔╝ ╚═╝            ╚═╝    ████╗ ████║ ██╔════╝ " -ForegroundColor Gray
Write-Host "█████╔╝  ██╗ ██╗    ██╗ ██╗    ██╔████╔██║ ╚█████╗  " -ForegroundColor Gray
Write-Host "██╔═██╗  ██║ ██║ █╗ ██║ ██║    ██║╚██╔╝██║  ╚═══██╗ " -ForegroundColor Gray
Write-Host "██║  ██╗ ██║ ╚███╔███╔╝ ██║    ██║ ╚═╝ ██║ ██████╔╝ " -ForegroundColor Gray
Write-Host "╚═╝  ╚═╝ ╚═╝  ╚══╝╚══╝  ╚═╝    ╚═╝     ╚═╝ ╚═════╝  " -ForegroundColor Gray
Write-Host ""
Write-Host "---------------------------------------------------" -ForegroundColor DarkGray
Write-Host "         Welcome to KiwiMS ($versionFile)          " -ForegroundColor White
Write-Host "---------------------------------------------------" -ForegroundColor DarkGray

# Environment Discovery
$condaCmd = Find-CondaExecutable
$logDirectory = "$env:LOCALAPPDATA\KiwiMS"
$logFile = Join-Path $logDirectory "launch.log"

# Ensure log directory exists
if (-Not (Test-Path $logDirectory)) { New-Item -ItemType Directory -Path $logDirectory -Force | Out-Null }

# Initialize Log
"$(Get-Date) - INFO: Launcher Initialized." | Out-File $logFile

if (-not $condaCmd) {
    Write-Host "ERROR: Conda/Miniforge not found! Please reinstall KiwiMS." -ForegroundColor Red
    "$(Get-Date) - ERROR: Conda executable not found." | Add-Content $logFile
    if (-not $Headless) { pause }
    exit 1
}

Write-Host "Using Conda at: $condaCmd" -ForegroundColor Gray
Write-Host "Starting application..." -ForegroundColor Yellow

# Launch Execution
try {
    "$(Get-Date) - INFO: Executing with Conda: $condaCmd" | Add-Content $logFile
    
    $shinyCmd = "shiny::runApp('app.R', port = 3838, launch.browser = $(if ($Headless) { 'FALSE' } else { 'TRUE' }))"
    & $condaCmd run -n kiwims Rscript.exe -e "$shinyCmd" --vanilla *> $logFile 2>&1

    if ($LASTEXITCODE -ne 0) {
        throw "The R process exited unexpectedly with code $LASTEXITCODE. Check the logs at $logFile"
    }
}
catch {
    $msg = "$(Get-Date) - CRITICAL ERROR: $($_.Exception.Message)"
    $msg | Add-Content $logFile
    
    Write-Host ""
    Write-Host "FAILED TO START" -ForegroundColor Red
    Write-Host "Error: $($_.Exception.Message)"
    Write-Host "Detailed logs: $logFile"
    
    if (-not $Headless) { pause }
    exit 1
}