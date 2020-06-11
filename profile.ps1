# Find out if the current user identity is elevated (has admin rights)
$identity = [Security.Principal.WindowsIdentity]::GetCurrent()
$principal = New-Object Security.Principal.WindowsPrincipal $identity
$isAdmin = $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

# Useful shortcuts for traversing directories
function cd...  { Set-Location ..\.. }
function cd.... { Set-Location ..\..\.. }

# Compute file hashes - useful for checking successful downloads 
function md5    { Get-FileHash -Algorithm MD5 $args }
function sha1   { Get-FileHash -Algorithm SHA1 $args }
function sha256 { Get-FileHash -Algorithm SHA256 $args }

# Quick shortcuts
function l      { Get-ChildItem $args }
function x      { exit }

# Drive shortcuts
function HKLM:  { Set-Location HKLM: }
function HKCU:  { Set-Location HKCU: }
function Env:   { Set-Location Env: }

# For color excape seq
$ESC = [char]27

# Set up command prompt and window title. Use UNIX-style convention for identifying 
# whether user is elevated (root) or not. Window title shows current version of PowerShell
# and appends [ADMIN] if appropriate for easy taskbar identification
function prompt 
{
    if ($isAdmin) 
    {
        "$ESC[31;1m[PS " + (Get-Location) + "] # $ESC[0m" 
    }
    else 
    {
        "[PS " + (Get-Location) + "] $ "
    }
}

# USeful aliases for creating symbolic link and directory junction, similar to `ln -s` in *nix
function link($target, $link) { New-Item -ItemType SymbolicLink -Path $link -Target $target }
function junction($target, $link) { New-Item -ItemType Junction -Path $link -Target $target }

# Does the the rough equivalent of dir /s /b. For example, dirs *.png is dir /s /b *.png
function dirs
{
    if ($args.Count -gt 0)
    {
        Get-ChildItem -Recurse -Include "$args" | Foreach-Object FullName
    }
    else
    {
        Get-ChildItem -Recurse | Foreach-Object FullName
    }
}

# Make it easy to edit this profile once it's installed
function Edit-Profile
{
    if ($host.Name -match "ise")
    {
        $psISE.CurrentPowerShellTab.Files.Add($profile.CurrentUserAllHosts)
    }
    else
    {
        code $profile.CurrentUserAllHosts
    }
}

# Show a color grid of all available BG&FG colors
function Show-ColorTable
{
    $colors = [enum]::GetValues([System.ConsoleColor])
    Foreach ($bgcolor in $colors){
        Foreach ($fgcolor in $colors) { Write-Host "$fgcolor|"  -ForegroundColor $fgcolor -BackgroundColor $bgcolor -NoNewLine }
        Write-Host " on $bgcolor"
    }
}
Set-Alias -Name colors -Value Show-ColorTable

# Env variables
$Env:WORKON_HOME = $Env:USERPROFILE + "\PyEnvs"

# Import 3rd party modules
Import-Module -Name VirtualEnvWrapper

# We don't need these any more; they were just temporary variables to get to $isAdmin. 
# Delete them to prevent cluttering up the user profile. 
Remove-Variable identity
Remove-Variable principal

# Enable oh-my-posh
Import-Module posh-git
Import-Module oh-my-posh
Set-Theme paradox

# Navigation in command history
Set-PSReadLineKeyHandler -Key UpArrow -Function HistorySearchBackward
Set-PSReadLineKeyHandler -Key DownArrow -Function HistorySearchForward