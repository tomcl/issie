# F# AST Analyzer & Call Graph Visualizer

A comprehensive tool for analyzing F# code structure and generating interactive call graph visualizations. This tool uses F# Compiler Services (FCS) to parse F# source code, Python for data analysis, and D3.js for interactive web-based visualization.

## 🚀 Features

- **F# AST Parsing**: Uses F# Compiler Services to extract detailed AST information
- **Call Graph Analysis**: Identifies function calls, dependencies, and relationships
- **Interactive Visualization**: Web-based interface with D3.js for exploring call graphs
- **Multiple Layout Options**: Force-directed, hierarchical, and circular layouts
- **Filtering & Search**: Filter by modules, search functions, and focus on specific areas
- **Export Capabilities**: Export visualizations as SVG, PNG, or DOT format
- **Comprehensive Metrics**: Analyze code complexity, coupling, and potential issues
- **Cross-Platform**: Works on Windows, macOS, and Linux

## 📁 Project Structure

```
fsharp-ast-analyzer/
├── src/
│   ├── FSharpParser/          # F# AST parsing using FCS
│   │   ├── Parser.fs          # Main parser implementation
│   │   └── FSharpParser.fsproj
│   ├── PythonAnalyzer/        # Python-based analysis
│   │   ├── analyzer.py        # AST data analyzer
│   │   ├── call_graph.py      # Call graph generator
│   │   └── requirements.txt   # Python dependencies
│   └── WebVisualizer/         # Web-based visualization
│       ├── index.html         # Main web interface
│       ├── app.js            # D3.js visualization logic
│       └── style.css         # UI styling
├── scripts/
│   ├── run.sh                # Linux/macOS automation script
│   └── run.ps1               # Windows PowerShell script
├── examples/
│   └── sample.fs             # Sample F# code for testing
└── README.md
```

## 🛠 Installation

### Prerequisites

1. **.NET 6.0 or later** - [Download .NET](https://dotnet.microsoft.com/download)
2. **Python 3.8 or later** - [Download Python](https://www.python.org/downloads/)
3. **Modern web browser** - For viewing visualizations

### Quick Setup

#### Linux/macOS
```bash
# Clone or download the project
cd fsharp-ast-analyzer

# Install dependencies and run analysis
./scripts/run.sh --install --serve ./examples/sample.fs
```

#### Windows
```powershell
# Clone or download the project
cd fsharp-ast-analyzer

# Install dependencies and run analysis
.\scripts\run.ps1 -Install -Serve .\examples\sample.fs
```

### Manual Installation

1. **Install .NET Dependencies**
   ```bash
   cd src/FSharpParser
   dotnet restore
   ```

2. **Install Python Dependencies**
   ```bash
   cd src/PythonAnalyzer
   pip install -r requirements.txt
   ```

## 🎯 Usage

### Basic Usage

Analyze a single F# file:
```bash
./scripts/run.sh /path/to/your/file.fs
```

Analyze a directory of F# files:
```bash
./scripts/run.sh /path/to/your/project/
```

### Advanced Usage

```bash
# Custom output directory
./scripts/run.sh --output ./results /path/to/project

# Start web server after analysis
./scripts/run.sh --serve --port 3000 /path/to/project

# Install dependencies, analyze, and serve
./scripts/run.sh --install --serve /path/to/project
```

### Windows PowerShell Examples

```powershell
# Basic analysis
.\scripts\run.ps1 C:\MyProject\Program.fs

# With custom output and web server
.\scripts\run.ps1 -OutputDir .\results -Serve -Port 8080 C:\MyProject\
```

### Manual Step-by-Step

1. **Parse F# Code**
   ```bash
   cd src/FSharpParser
   dotnet run /path/to/source.fs output/ast-analysis.json
   ```

2. **Analyze AST Data**
   ```bash
   cd src/PythonAnalyzer
   python analyzer.py output/ast-analysis.json output/metrics.json
   ```

3. **Generate Call Graph**
   ```bash
   python call_graph.py output/ast-analysis.json output/call-graph.json
   ```

4. **View Visualization**
   ```bash
   cd src/WebVisualizer
   # Copy call-graph.json to this directory
   python -m http.server 8080
   # Open http://localhost:8080 in browser
   ```

## 📊 Understanding the Output

### Generated Files

- **`ast-analysis.json`**: Raw AST data with functions, calls, and modules
- **`analysis-metrics.json`**: Computed metrics and statistics
- **`call-graph.json`**: D3.js-compatible visualization data
- **`call-graph.dot`**: Graphviz DOT format for static diagrams

### AST Data Format

```json
{
  "functions": [
    {
      "Name": "functionName",
      "Module": "ModuleName",
      "Parameters": ["param1", "param2"],
      "ReturnType": "int",
      "StartLine": 10,
      "EndLine": 15,
      "FilePath": "/path/to/file.fs"
    }
  ],
  "calls": [
    {
      "Caller": "functionA",
      "Callee": "functionB",
      "Location": [12, 8],
      "FilePath": "/path/to/file.fs"
    }
  ],
  "modules": [
    {
      "Name": "ModuleName",
      "FilePath": "/path/to/file.fs",
      "Functions": ["func1", "func2"]
    }
  ]
}
```

### Metrics Explained

- **Total Functions**: Number of function definitions found
- **Total Calls**: Number of function call sites identified
- **Graph Density**: Ratio of actual connections to possible connections
- **Max Call Depth**: Maximum depth of call chains
- **Cyclic Dependencies**: Functions that call each other (directly or indirectly)
- **Dead Code Candidates**: Functions that are never called
- **Module Coupling**: Cross-module dependencies

## 🎨 Web Interface Features

### Interactive Visualization

- **Pan & Zoom**: Navigate large call graphs easily
- **Node Selection**: Click nodes to see detailed information
- **Search**: Find specific functions quickly
- **Module Filtering**: Focus on specific modules
- **Layout Options**: Choose between different visualization layouts

### Controls

- **File Upload**: Load your own call graph JSON files
- **Search Box**: Filter visible functions by name
- **Module Filter**: Show only functions from specific modules
- **Layout Selector**: Switch between force-directed, hierarchical, and circular layouts
- **Export Buttons**: Save visualizations as SVG or PNG files

### Node Information

- Function name and module
- Parameter list and return type
- Source file location
- Incoming and outgoing call counts
- Related functions (callers and callees)

## 📈 Analysis Capabilities

### Code Structure Analysis

- **Function Dependencies**: Understand which functions depend on others
- **Module Relationships**: Identify cross-module dependencies
- **Call Patterns**: Visualize how functions interact
- **Code Complexity**: Measure interconnectedness and complexity

### Quality Metrics

- **Coupling Analysis**: Identify tightly coupled components
- **Dead Code Detection**: Find unused functions
- **Circular Dependencies**: Detect potential design issues
- **Call Depth Analysis**: Understand call stack complexity

## 🔧 Configuration

### Python Analysis Options

You can customize the analysis by modifying the Python scripts:

```python
# In analyzer.py, adjust these parameters:
filter_options = {
    'modules': ['ModuleA', 'ModuleB'],  # Only analyze specific modules
    'min_degree': 2,                    # Only include well-connected functions
    'max_depth': 5,                     # Limit call depth analysis
    'root_function': 'main'             # Start analysis from specific function
}
```

### Web Interface Customization

Edit `src/WebVisualizer/app.js` to customize visualization:

```javascript
// Adjust force simulation parameters
.force('charge', d3.forceManyBody().strength(-300))  // Node repulsion
.force('link', d3.forceLink().distance(100))         // Link length
.force('collision', d3.forceCollide().radius(30))    // Node collision
```

## 🤝 Contributing

We welcome contributions! Here's how to get started:

1. **Fork the repository**
2. **Create a feature branch**: `git checkout -b feature/amazing-feature`
3. **Make your changes**
4. **Add tests** for new functionality
5. **Commit your changes**: `git commit -m 'Add amazing feature'`
6. **Push to the branch**: `git push origin feature/amazing-feature`
7. **Open a Pull Request**

### Development Setup

```bash
# Clone the repository
git clone <repository-url>
cd fsharp-ast-analyzer

# Install development dependencies
./scripts/run.sh --install

# Run tests
cd src/FSharpParser && dotnet test
cd src/PythonAnalyzer && python -m pytest
```

## 📝 Examples

### Analyzing the Sample Code

The project includes a comprehensive sample F# file demonstrating various language features:

```bash
# Analyze the sample
./scripts/run.sh --serve ./examples/sample.fs

# View the results at http://localhost:8080
```

The sample code includes:
- Multiple modules with different purposes
- Function calls within and across modules
- Different types of functions (recursive, higher-order, etc.)
- Data structures and their manipulation
- Business logic demonstrations

### Real-World Usage

```bash
# Analyze an F# web application
./scripts/run.sh --output ./webapp-analysis /path/to/webapp/src

# Analyze a library project
./scripts/run.sh --serve --port 3000 /path/to/library/src

# Focus on specific modules
# (Edit the Python scripts to add module filters)
```

## 🐛 Troubleshooting

### Common Issues

1. **"dotnet command not found"**
   - Install .NET SDK from https://dotnet.microsoft.com/download

2. **"python command not found"**
   - Install Python from https://www.python.org/downloads/
   - On Windows, use `python` instead of `python3`

3. **"Failed to restore packages"**
   - Check internet connection
   - Try `dotnet nuget locals all --clear`

4. **"Module not found" errors in Python**
   - Ensure all dependencies are installed: `pip install -r requirements.txt`
   - Check Python virtual environment activation

5. **Web interface not loading**
   - Ensure the web server is running
   - Check that call-graph.json is in the web directory
   - Try a different port: `--port 8081`

### Debug Mode

Enable verbose logging:

```bash
# Linux/macOS
DEBUG=1 ./scripts/run.sh /path/to/project

# Windows
$env:DEBUG=1; .\scripts\run.ps1 /path/to/project
```

## 📄 License

This project is licensed under the MIT License - see the LICENSE file for details.

## 🙏 Acknowledgments

- **F# Compiler Services** - For powerful F# AST parsing capabilities
- **D3.js** - For beautiful and interactive data visualizations
- **NetworkX** - For graph analysis algorithms
- **The F# Community** - For inspiration and support

## 📚 Further Reading

- [F# Compiler Services Documentation](https://fsharp.github.io/FSharp.Compiler.Service/)
- [D3.js Documentation](https://d3js.org/)
- [NetworkX Documentation](https://networkx.org/)
- [Call Graph Analysis in Software Engineering](https://en.wikipedia.org/wiki/Call_graph)

---

**Happy analyzing!** 🚀

For questions, issues, or contributions, please visit our [GitHub repository](https://github.com/your-username/fsharp-ast-analyzer).