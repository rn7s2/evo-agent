<#
.SYNOPSIS
  evo — build & install on Windows.  The Makefile's targets, in PowerShell.

.DESCRIPTION
  The Windows counterpart of the Makefile, sitting beside it and taking the
  same targets and the same knobs:

      .\make.ps1 build
      .\make.ps1 install
      .\make.ps1 install-home
      .\make.ps1 test
      .\make.ps1 clean

  Variables are parameters here rather than `NAME=value` arguments, but they
  mean what they mean in the Makefile:

      -Lisp     Common Lisp implementation.  SBCL only on Windows: ECL's
                Unix facilities (fork/setsid/stty/pgrep) have no Windows
                branch in src/port/port.lisp, so anything else is refused
                loudly rather than failing halfway through a build.
      -EvoHome  Global evo home seeded by install-home.  Default $HOME\.evo.
      -Prefix   Install prefix.  Default $HOME\.evo — see `install` below.
      -HeapMb   Dynamic heap (MiB) baked into the binary (D10).

  install differs from the Unix one on purpose.  There is no /usr/local on
  Windows and no sudo to write outside your profile with, so the binary goes
  to $Prefix\bin\evo.exe — $HOME\.evo\bin\evo.exe by default, beside the
  evo home it already owns.  That directory is yours to write, needs no
  elevation, and the script tells you how to put it on PATH.

.EXAMPLE
  .\make.ps1 build
.EXAMPLE
  .\make.ps1 install -Prefix C:\tools\evo
#>

[CmdletBinding()]
param(
    [Parameter(Position = 0)]
    [ValidateSet('build', 'install', 'install-home', 'test', 'console-test', 'integration', 'tui-test', 'clean', 'help')]
    [string]$Target = 'build',

    [string]$Lisp = 'sbcl',
    [string]$EvoHome = (Join-Path $HOME '.evo'),
    [string]$Prefix = (Join-Path $HOME '.evo'),
    [int]$HeapMb = 4096
)

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

$RepoRoot = $PSScriptRoot
$BuildDir = Join-Path $RepoRoot 'build'
$Binary = Join-Path $BuildDir 'evo.exe'

function Write-Step([string]$message) {
    Write-Host "==> $message" -ForegroundColor Cyan
}

# Only SBCL, and say so before doing any work.  ECL builds on Unix through
# monolithic-lib-op + c:build-program, which needs a C toolchain and a port
# layer that does not exist here.
function Resolve-Lisp {
    if ($Lisp -ne 'sbcl') {
        throw "evo on Windows supports SBCL only (got -Lisp '$Lisp'). Build with ECL on Unix."
    }
    # First match only: Get-Command returns *every* sbcl on PATH, and more
    # than one is ordinary (an installer's directory plus a package
    # manager's shim).  Taking the whole list would stringify an array into
    # one nonsense command name; first-on-PATH-wins is what a prompt does.
    $sbcl = Get-Command 'sbcl' -CommandType Application -ErrorAction SilentlyContinue |
        Select-Object -First 1
    if (-not $sbcl) {
        throw "sbcl is not on PATH."
    }
    return $sbcl.Source
}

# Same contract as the Makefile's RUN_SCRIPT/BUILD_SCRIPT: load a script
# non-interactively, let it exit itself, and fail the target when it fails.
function Invoke-LispScript {
    param(
        [Parameter(Mandatory)][string]$Script,
        [switch]$Build
    )
    $sbcl = Resolve-Lisp
    $sbclArgs = @()
    if ($Build) { $sbclArgs += @('--dynamic-space-size', $HeapMb) }
    $sbclArgs += @('--non-interactive', '--load', $Script)

    Push-Location $RepoRoot
    try {
        & $sbcl @sbclArgs
        if ($LASTEXITCODE -ne 0) {
            throw "$(Split-Path -Leaf $Script) failed (exit $LASTEXITCODE)"
        }
    } finally {
        Pop-Location
    }
}

function Invoke-Build {
    Write-Step "building $Binary (heap ${HeapMb}MiB)"
    Invoke-LispScript -Script (Join-Path $RepoRoot 'build.lisp') -Build
    if (-not (Test-Path $Binary)) {
        throw "build reported success but $Binary is missing"
    }
    Write-Host "built $Binary" -ForegroundColor Green
}

# Seed corpus: docs + example extensions into the global evo home.
# Everything under docs/ is reference-only — nothing ships active in
# $EvoHome\extensions except the vendored extensions/*.lisp, which are
# loaded at startup.  The sample init.lisp is reference-only too: evo
# requires a real $EvoHome\init.lisp (no built-in model table).
function Invoke-InstallHome {
    Write-Step "seeding $EvoHome"
    foreach ($dir in @(
            (Join-Path $EvoHome 'extensions'),
            (Join-Path $EvoHome 'docs\examples'),
            (Join-Path $EvoHome 'skills'),
            (Join-Path $EvoHome 'prompts'))) {
        New-Item -ItemType Directory -Force -Path $dir | Out-Null
    }
    Copy-Item (Join-Path $RepoRoot 'docs\*.md') (Join-Path $EvoHome 'docs') -Force
    Copy-Item (Join-Path $RepoRoot 'docs\examples\init.lisp') (Join-Path $EvoHome 'docs\examples') -Force
    Copy-Item (Join-Path $RepoRoot 'extensions\examples\*.lisp') (Join-Path $EvoHome 'docs\examples') -Force
    Copy-Item (Join-Path $RepoRoot 'extensions\*.lisp') (Join-Path $EvoHome 'extensions') -Force
    Get-ChildItem -Path (Join-Path $EvoHome 'extensions') -Filter '*.fasl' -ErrorAction SilentlyContinue |
        Remove-Item -Force
}

function Invoke-Install {
    Invoke-Build
    Invoke-InstallHome
    $binDir = Join-Path $Prefix 'bin'
    $target = Join-Path $binDir 'evo.exe'
    Write-Step "installing $target"
    New-Item -ItemType Directory -Force -Path $binDir | Out-Null
    # A running evo holds its own image open; replacing it in place fails
    # with "being used by another process", and the fix is to say so rather
    # than leave half an install behind.
    try {
        Copy-Item $Binary $target -Force
    } catch {
        throw "cannot replace $target — close any running evo first ($($_.Exception.Message))"
    }
    Write-Host "installed $target" -ForegroundColor Green

    $onPath = ($env:PATH -split ';') -contains $binDir
    if (-not $onPath) {
        Write-Host ""
        Write-Host "$binDir is not on PATH. To add it for future shells:" -ForegroundColor Yellow
        Write-Host "  [Environment]::SetEnvironmentVariable('PATH', `"`$env:PATH;$binDir`", 'User')"
    }
    $initFile = Join-Path $EvoHome 'init.lisp'
    if (-not (Test-Path $initFile)) {
        Write-Host ""
        Write-Host "Next: copy $EvoHome\docs\examples\init.lisp to $initFile and put your model + API key in it." -ForegroundColor Yellow
    }
}

# The unit suite is Lisp-portable and passes on Windows; the Windows-only
# console paths have their own live tests (console-test) that need a real
# console and so run here, not in CI.  The .exp suites still need a pty.
function Invoke-Test {
    Write-Step 'running unit tests'
    Invoke-LispScript -Script (Join-Path $RepoRoot 'tests\run-unit.lisp')
}

# Live console tests: drive the real console (CONIN$/CONOUT$) to prove the
# Windows-only input and output paths — they inject real key events and read
# the glyphs back out of the screen buffer.  They need a real console
# attached, so they run here but not in CI (which has no interactive console).
function Invoke-ConsoleTest {
    foreach ($script in @('tests\windows-input-live.lisp',
                          'tests\windows-console-live.lisp')) {
        Write-Step "running $script"
        Invoke-LispScript -Script (Join-Path $RepoRoot $script)
    }
}

function Invoke-Clean {
    Write-Step "removing $BuildDir"
    if (Test-Path $BuildDir) { Remove-Item -Recurse -Force $BuildDir }
}

function Show-Help {
    Get-Help $PSCommandPath -Detailed
}

switch ($Target) {
    'build' { Invoke-Build }
    'integration' { throw "integration runs tests/integration.sh, which needs a POSIX shell — not supported on Windows yet." }
    'tui-test' { throw "tui-test drives evo through a pty with expect — not supported on Windows yet." }
    'install' { Invoke-Install }
    'install-home' { Invoke-InstallHome }
    'test' { Invoke-Test }
    'console-test' { Invoke-ConsoleTest }
    'clean' { Invoke-Clean }
    'help' { Show-Help }
}
