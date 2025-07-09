# Set temporary download path
$tempPath = "$env:TEMP\rtools_installer.exe"

# Get latest Rtools from CRAN
$rtoolsUrl = "https://cran.r-project.org/bin/windows/Rtools/rtools44/files/rtools44-6459-6401.exe"

# Output download link for logging
Write-Host "Downloading Rtools from: https://cran.r-project.org/bin/windows/Rtools/rtools44/files/rtools44-6459-6401.exe"

# Download the installer
Invoke-WebRequest -Uri $rtoolsUrl -OutFile $tempPath

# Run silent install
Start-Process -FilePath $tempPath -ArgumentList "/SILENT" -Wait

# Cleanup installer
Remove-Item -Path $tempPath -Force

Write-Host "Rtools installation completed."
