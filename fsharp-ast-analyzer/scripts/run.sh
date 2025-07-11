#!/bin/bash

# F# AST Analyzer - Linux/Mac Run Script

set -e

# Initialize conda and activate dev environment
source ~/miniconda3/etc/profile.d/conda.sh && conda activate dev

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Default values
TARGET_PATH=""
OUTPUT_DIR="./output"
SERVE_WEB=false
PORT=8080
INSTALL_DEPS=false

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to show usage
show_usage() {
    cat << EOF
F# AST Analyzer - Call Graph Visualization Tool

Usage: $0 [OPTIONS] <target-path>

ARGUMENTS:
    <target-path>           Path to F# file or directory to analyze

OPTIONS:
    -o, --output DIR        Output directory for generated files (default: ./output)
    -s, --serve             Start web server after analysis
    -p, --port PORT         Port for web server (default: 8080)
    -i, --install           Install dependencies before running
    -h, --help              Show this help message

EXAMPLES:
    $0 /path/to/project.fs
    $0 --output ./results --serve /path/to/fsharp/project/
    $0 -i -s -p 3000 ./examples/sample.fs

DEPENDENCIES:
    - .NET 6.0 or later
    - Python 3.8 or later
    - Node.js (optional, for advanced web server)

EOF
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -o|--output)
            OUTPUT_DIR="$2"
            shift 2
            ;;
        -s|--serve)
            SERVE_WEB=true
            shift
            ;;
        -p|--port)
            PORT="$2"
            shift 2
            ;;
        -i|--install)
            INSTALL_DEPS=true
            shift
            ;;
        -h|--help)
            show_usage
            exit 0
            ;;
        -*)
            print_error "Unknown option: $1"
            show_usage
            exit 1
            ;;
        *)
            if [[ -z "$TARGET_PATH" ]]; then
                TARGET_PATH="$1"
            else
                print_error "Multiple target paths specified"
                show_usage
                exit 1
            fi
            shift
            ;;
    esac
done

# Check if target path is provided
if [[ -z "$TARGET_PATH" ]]; then
    print_error "No target path specified"
    show_usage
    exit 1
fi

# Convert target path to absolute path
TARGET_PATH=$(realpath "$TARGET_PATH")

# Check if target path exists
if [[ ! -e "$TARGET_PATH" ]]; then
    print_error "Target path does not exist: $TARGET_PATH"
    exit 1
fi

# Create output directory
OUTPUT_DIR=$(realpath "$OUTPUT_DIR")
mkdir -p "$OUTPUT_DIR" || true

print_status "Starting F# AST Analysis..."
print_status "Target: $TARGET_PATH"
print_status "Output: $OUTPUT_DIR"

# Install dependencies if requested
if [[ "$INSTALL_DEPS" == true ]]; then
    print_status "Installing dependencies..."
    
    # Check .NET
    if ! command -v dotnet &> /dev/null; then
        print_error ".NET is not installed. Please install .NET 6.0 or later."
        exit 1
    fi
    
    # Check Python
    if ! command -v python3 &> /dev/null; then
        print_error "Python 3 is not installed. Please install Python 3.8 or later."
        exit 1
    fi
    
    # Install .NET dependencies
    print_status "Restoring .NET packages..."
    cd "$PROJECT_ROOT/src/FSharpParser"
    dotnet restore
    
    # Install Python dependencies
    print_status "Installing Python packages..."
    cd "$PROJECT_ROOT/src/PythonAnalyzer"
    python3 -m pip install -r requirements.txt
    
    print_success "Dependencies installed successfully"
fi

# Step 1: Build and run F# Parser
print_status "Building F# AST parser..."
cd "$PROJECT_ROOT/src/FSharpParser"

if ! dotnet build; then
    print_error "Failed to build F# parser"
    exit 1
fi

print_success "F# parser built successfully"

# Step 2: Run F# AST analysis
print_status "Analyzing F# code..."
AST_OUTPUT="$OUTPUT_DIR/ast-analysis.json"

# Ensure the output directory exists
mkdir -p "$(dirname "$AST_OUTPUT")"

if ! dotnet run "$TARGET_PATH" "$AST_OUTPUT"; then
    print_error "F# AST analysis failed"
    exit 1
fi

print_success "F# AST analysis completed: $AST_OUTPUT"

# Step 3: Run Python analysis
print_status "Running Python analysis..."
cd "$PROJECT_ROOT/src/PythonAnalyzer"

METRICS_OUTPUT="$OUTPUT_DIR/analysis-metrics.json"

if ! python3 analyzer.py "$AST_OUTPUT" "$METRICS_OUTPUT"; then
    print_error "Python analysis failed"
    exit 1
fi

print_success "Python analysis completed: $METRICS_OUTPUT"

# Step 4: Generate call graph data
print_status "Generating call graph data..."
GRAPH_OUTPUT="$OUTPUT_DIR/call-graph.json"

if ! python3 call_graph.py "$AST_OUTPUT" "$GRAPH_OUTPUT"; then
    print_error "Call graph generation failed"
    exit 1
fi

print_success "Call graph data generated: $GRAPH_OUTPUT"

# Step 5: Copy web visualizer files
print_status "Preparing web visualizer..."
cp -r "$PROJECT_ROOT/src/WebVisualizer"/* "$OUTPUT_DIR/"

# Copy the generated call graph data to be loaded by the web interface (only if different)
if [[ "$GRAPH_OUTPUT" != "$OUTPUT_DIR/call-graph.json" ]]; then
    cp "$GRAPH_OUTPUT" "$OUTPUT_DIR/call-graph.json"
fi

print_success "Web visualizer prepared in: $OUTPUT_DIR"

# Step 6: Start web server if requested
if [[ "$SERVE_WEB" == true ]]; then
    print_status "Starting web server on port $PORT..."
    
    cd "$OUTPUT_DIR"
    
    # Try different methods to start a web server
    if command -v python3 &> /dev/null; then
        print_status "Using Python HTTP server..."
        echo ""
        echo "🚀 Web server starting..."
        echo "📊 Open your browser and navigate to: http://localhost:$PORT"
        echo "📁 Files are served from: $OUTPUT_DIR"
        echo ""
        echo "Press Ctrl+C to stop the server"
        echo ""
        
        python3 -m http.server "$PORT"
    elif command -v node &> /dev/null; then
        print_status "Using Node.js HTTP server..."
        npx http-server -p "$PORT" -o
    else
        print_warning "No web server available. Please open $OUTPUT_DIR/index.html in your browser."
        print_warning "Or install Python 3 or Node.js to start a local server."
    fi
else
    print_success "Analysis complete!"
    echo ""
    echo "📊 Results generated in: $OUTPUT_DIR"
    echo "🌐 To view the call graph, open: $OUTPUT_DIR/index.html"
    echo "📈 Analysis metrics: $METRICS_OUTPUT"
    echo "🔗 Call graph data: $GRAPH_OUTPUT"
    echo ""
    echo "To start a web server, run:"
    echo "  $0 --serve $TARGET_PATH"
fi