param(
    [string]$basePath,
    [string]$installScope
)

# Source your functions
. (Join-Path $basePath "functions.ps1")

$result = 0

# --- Check Miniforge ---
$condaCmd = Find-CondaExecutable
$condaScope = Get-CondaScope -CondaPath $condaCmd
# Valid if found AND (requested user OR found system-wide)
if ($condaCmd -and -not ($installScope -eq "allusers" -and $condaScope -eq "currentuser")) {
    $result += 1
}

# --- Check RTools ---
$rtoolsPath = Find-Rtools45Executable
$rtoolsScope = Get-RtoolsScope -Path $rtoolsPath # Ensure this function exists in functions.ps1
if ($rtoolsPath -and -not ($installScope -eq "allusers" -and $rtoolsScope -eq "currentuser")) {
    $result += 2
}

# --- Check Quarto ---
$quarto = Find-QuartoInstallation
if ($quarto.Found -and -not ($installScope -eq "allusers" -and $quarto.Scope -eq "currentuser")) {
    $result += 4
}

exit $result