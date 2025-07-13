# F# AST Analyzer - Windows PowerShell Run Script

param(
    [Parameter(Mandatory=$true, Position=0)]
    [string]$TargetPath,
    
    [Parameter(Mandatory=$false)]
    [string]$OutputDir = ".\output",
    
    [Parameter(Mandatory=$false)]
    [switch]$Serve,
    
    [Parameter(Mandatory=$false)]
    [int]$Port = 8080,
    
    [Parameter(Mandatory=$false)]
    [switch]$Install,
    
    [Parameter(Mandatory=$false)]
    [switch]$Help
)

# Colors for output
$Red = "Red"
$Green = "Green"
$Yellow = "Yellow"
$Blue = "Blue"

# Get script directory
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Definition
$ProjectRoot = Split-Path -Parent $ScriptDir

# Function to print colored output
function Write-Status {
    param([string]$Message)
    Write-Host "[INFO] $Message" -ForegroundColor $Blue
}

function Write-Success {
    param([string]$Message)
    Write-Host "[SUCCESS] $Message" -ForegroundColor $Green
}

function Write-Warning {
    param([string]$Message)
    Write-Host "[WARNING] $Message" -ForegroundColor $Yellow
}

function Write-Error {
    param([string]$Message)
    Write-Host "[ERROR] $Message" -ForegroundColor $Red
}

# Function to show usage
function Show-Usage {
    Write-Host @"
F# AST Analyzer - Call Graph Visualization Tool

Usage: .\run.ps1 [OPTIONS] <target-path>

ARGUMENTS:
    <target-path>           Path to F# file or directory to analyze

OPTIONS:
    -OutputDir DIR          Output directory for generated files (default: .\output)
    -Serve                  Start web server after analysis
    -Port PORT              Port for web server (default: 8080)
    -Install                Install dependencies before running
    -Help                   Show this help message

EXAMPLES:
    .\run.ps1 C:\path\to\project.fs
    .\run.ps1 -OutputDir .\results -Serve C:\path\to\fsharp\project\
    .\run.ps1 -Install -Serve -Port 3000 .\examples\sample.fs

DEPENDENCIES:
    - .NET 6.0 or later
    - Python 3.8 or later
    - Node.js (optional, for advanced web server)
"@
}

# Show help if requested
if ($Help) {
    Show-Usage
    exit 0
}

# Check if target path exists
if (-not (Test-Path $TargetPath)) {
    Write-Error "Target path does not exist: $TargetPath"
    exit 1
}

# Create output directory
if (-not (Test-Path $OutputDir)) {
    New-Item -ItemType Directory -Path $OutputDir -Force | Out-Null
}

Write-Status "Starting F# AST Analysis..."
Write-Status "Target: $TargetPath"
Write-Status "Output: $OutputDir"

# Install dependencies if requested
if ($Install) {
    Write-Status "Installing dependencies..."
    
    # Check .NET
    try {
        $dotnetVersion = dotnet --version
        Write-Status ".NET version: $dotnetVersion"
    } catch {
        Write-Error ".NET is not installed. Please install .NET 6.0 or later."
        exit 1
    }
    
    # Check Python
    try {
        $pythonVersion = python --version
        Write-Status "Python version: $pythonVersion"
    } catch {
        Write-Error "Python is not installed. Please install Python 3.8 or later."
        exit 1
    }
    
    # Install .NET dependencies
    Write-Status "Restoring .NET packages..."
    Set-Location "$ProjectRoot\src\FSharpParser"
    dotnet restore
    if ($LASTEXITCODE -ne 0) {
        Write-Error "Failed to restore .NET packages"
        exit 1
    }
    
    # Install Python dependencies
    Write-Status "Installing Python packages..."
    Set-Location "$ProjectRoot\src\PythonAnalyzer"
    python -m pip install -r requirements.txt
    if ($LASTEXITCODE -ne 0) {
        Write-Error "Failed to install Python packages"
        exit 1
    }
    
    Write-Success "Dependencies installed successfully"
}

# Step 1: Build and run F# Parser
Write-Status "Building F# AST parser..."
Set-Location "$ProjectRoot\src\FSharpParser"

dotnet build
if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to build F# parser"
    exit 1
}

Write-Success "F# parser built successfully"

# Step 2: Run F# AST analysis
Write-Status "Analyzing F# code..."
$AstOutput = Join-Path $OutputDir "ast-analysis.json"

dotnet run $TargetPath $AstOutput
if ($LASTEXITCODE -ne 0) {
    Write-Error "F# AST analysis failed"
    exit 1
}

Write-Success "F# AST analysis completed: $AstOutput"

# Step 3: Run Python analysis
Write-Status "Running Python analysis..."
Set-Location "$ProjectRoot\src\PythonAnalyzer"

$MetricsOutput = Join-Path $OutputDir "analysis-metrics.json"

python analyzer.py $AstOutput $MetricsOutput
if ($LASTEXITCODE -ne 0) {
    Write-Error "Python analysis failed"
    exit 1
}

Write-Success "Python analysis completed: $MetricsOutput"

# Step 4: Generate call graph data
Write-Status "Generating call graph data..."
$GraphOutput = Join-Path $OutputDir "call-graph.json"

python call_graph.py $AstOutput $GraphOutput
if ($LASTEXITCODE -ne 0) {
    Write-Error "Call graph generation failed"
    exit 1
}

Write-Success "Call graph data generated: $GraphOutput"

# Step 5: Copy web visualizer files
Write-Status "Preparing web visualizer..."
$WebVisualizerSource = "$ProjectRoot\src\WebVisualizer"
Copy-Item -Path "$WebVisualizerSource\*" -Destination $OutputDir -Recurse -Force

# Copy the generated call graph data to be loaded by the web interface
Copy-Item -Path $GraphOutput -Destination (Join-Path $OutputDir "call-graph.json") -Force

Write-Success "Web visualizer prepared in: $OutputDir"

# Step 6: Start web server if requested
if ($Serve) {
    Write-Status "Starting web server on port $Port..."
    
    Set-Location $OutputDir
    
    # Try different methods to start a web server
    try {
        python --version | Out-Null
        Write-Status "Using Python HTTP server..."
        Write-Host ""
        Write-Host "🚀 Web server starting..." -ForegroundColor Green
        Write-Host "📊 Open your browser and navigate to: http://localhost:$Port" -ForegroundColor Cyan
        Write-Host "📁 Files are served from: $OutputDir" -ForegroundColor Cyan
        Write-Host ""
        Write-Host "Press Ctrl+C to stop the server" -ForegroundColor Yellow
        Write-Host ""
        
        python -m http.server $Port
    } catch {
        try {
            node --version | Out-Null
            Write-Status "Using Node.js HTTP server..."
            npx http-server -p $Port -o
        } catch {
            Write-Warning "No web server available. Please open $OutputDir\index.html in your browser."
            Write-Warning "Or install Python 3 or Node.js to start a local server."
        }
    }
} else {
    Write-Success "Analysis complete!"
    Write-Host ""
    Write-Host "📊 Results generated in: $OutputDir" -ForegroundColor Cyan
    Write-Host "🌐 To view the call graph, open: $OutputDir\index.html" -ForegroundColor Cyan
    Write-Host "📈 Analysis metrics: $MetricsOutput" -ForegroundColor Cyan
    Write-Host "🔗 Call graph data: $GraphOutput" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "To start a web server, run:" -ForegroundColor Yellow
    Write-Host "  .\run.ps1 -Serve $TargetPath" -ForegroundColor Yellow
}