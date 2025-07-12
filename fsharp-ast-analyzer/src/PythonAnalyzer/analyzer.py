"""
F# AST Analysis Module
Analyzes F# AST data and computes call graph metrics
"""

import json
import pandas as pd
import networkx as nx
from typing import Dict, List, Tuple, Set
from dataclasses import dataclass
from collections import defaultdict, Counter
import os

@dataclass
class FunctionInfo:
    name: str
    module: str
    parameters: List[str]
    return_type: str
    start_line: int
    end_line: int
    file_path: str

@dataclass
class CallInfo:
    caller: str
    callee: str
    location: Tuple[int, int]
    file_path: str
    caller_module: str = ""
    callee_module: str = ""
    is_resolved: bool = False

@dataclass
class ModuleInfo:
    name: str
    file_path: str
    functions: List[str]

@dataclass
class AnalysisMetrics:
    total_functions: int
    total_calls: int
    total_modules: int
    max_call_depth: int
    cyclic_dependencies: List[List[str]]
    most_called_functions: List[Tuple[str, int]]
    most_calling_functions: List[Tuple[str, int]]
    module_coupling: Dict[str, int]
    dead_code_candidates: List[str]
    unresolved_calls: List[str]
    cross_file_calls: int
    resolution_rate: float

class FSharpAnalyzer:
    """Main analyzer class for F# AST data"""
    
    def __init__(self, json_file_path: str):
        """Initialize analyzer with AST data from JSON file"""
        self.json_file_path = json_file_path
        self.functions = []
        self.calls = []
        self.modules = []
        self.unresolved_calls = []
        self.call_graph = nx.DiGraph()
        self.metrics = None
        
        self._load_data()
        self._build_call_graph()
    
    def _load_data(self):
        """Load AST data from JSON file"""
        try:
            with open(self.json_file_path, 'r', encoding='utf-8') as f:
                data = json.load(f)
            
            # Parse functions
            for func_data in data.get('Functions', []):
                func = FunctionInfo(
                    name=func_data['Name'],
                    module=func_data['Module'],
                    parameters=func_data['Parameters'],
                    return_type=func_data.get('ReturnType', 'unknown'),
                    start_line=func_data['StartLine'],
                    end_line=func_data['EndLine'],
                    file_path=func_data['FilePath']
                )
                self.functions.append(func)
            
            # Parse calls
            for call_data in data.get('Calls', []):
                call = CallInfo(
                    caller=call_data['Caller'],
                    callee=call_data['Callee'],
                    location=tuple(call_data['Location']),
                    file_path=call_data['FilePath'],
                    caller_module=call_data.get('CallerModule', ''),
                    callee_module=call_data.get('CalleeModule', ''),
                    is_resolved=call_data.get('IsResolved', False)
                )
                self.calls.append(call)
            
            # Parse unresolved calls
            for call_data in data.get('UnresolvedCalls', []):
                call = CallInfo(
                    caller=call_data['Caller'],
                    callee=call_data['Callee'],
                    location=tuple(call_data['Location']),
                    file_path=call_data['FilePath'],
                    caller_module=call_data.get('CallerModule', ''),
                    callee_module=call_data.get('CalleeModule', ''),
                    is_resolved=False
                )
                self.unresolved_calls.append(call)
            
            # Parse modules
            for module_data in data.get('Modules', []):
                module = ModuleInfo(
                    name=module_data['Name'],
                    file_path=module_data['FilePath'],
                    functions=module_data['Functions']
                )
                self.modules.append(module)
                
            print(f"Loaded {len(self.functions)} functions, {len(self.calls)} calls, {len(self.unresolved_calls)} unresolved calls, {len(self.modules)} modules")
            
        except FileNotFoundError:
            print(f"Error: File {self.json_file_path} not found")
            raise
        except json.JSONDecodeError as e:
            print(f"Error parsing JSON: {e}")
            raise
    
    def _build_call_graph(self):
        """Build NetworkX directed graph from call data"""
        # Add all functions as nodes
        for func in self.functions:
            self.call_graph.add_node(func.name, 
                                   module=func.module,
                                   parameters=func.parameters,
                                   return_type=func.return_type,
                                   file_path=func.file_path,
                                   start_line=func.start_line,
                                   end_line=func.end_line)
        
        # Add call edges
        for call in self.calls:
            if call.caller in self.call_graph.nodes and call.callee in self.call_graph.nodes:
                self.call_graph.add_edge(call.caller, call.callee,
                                       location=call.location,
                                       file_path=call.file_path)
        
        print(f"Built call graph with {self.call_graph.number_of_nodes()} nodes and {self.call_graph.number_of_edges()} edges")
    
    def analyze_metrics(self) -> AnalysisMetrics:
        """Compute various metrics for the call graph"""
        if self.metrics is not None:
            return self.metrics
        
        # Basic counts
        total_functions = len(self.functions)
        total_calls = len(self.calls)
        total_modules = len(self.modules)
        
        # Call depth analysis
        max_call_depth = 0
        try:
            # Find the maximum path length in the DAG (if it's acyclic)
            if nx.is_directed_acyclic_graph(self.call_graph):
                longest_path = nx.dag_longest_path(self.call_graph)
                max_call_depth = len(longest_path) - 1
            else:
                # For cyclic graphs, estimate maximum depth
                max_call_depth = self._estimate_max_depth()
        except:
            max_call_depth = 0
        
        # Cyclic dependencies
        cyclic_dependencies = []
        try:
            cycles = list(nx.simple_cycles(self.call_graph))
            cyclic_dependencies = cycles[:10]  # Limit to first 10 cycles
        except:
            cyclic_dependencies = []
        
        # Most called functions (in-degree)
        in_degrees = dict(self.call_graph.in_degree())
        most_called = sorted(in_degrees.items(), key=lambda x: x[1], reverse=True)[:10]
        
        # Most calling functions (out-degree)
        out_degrees = dict(self.call_graph.out_degree())
        most_calling = sorted(out_degrees.items(), key=lambda x: x[1], reverse=True)[:10]
        
        # Module coupling analysis
        module_coupling = self._analyze_module_coupling()
        
        # Dead code candidates (functions with no incoming calls)
        dead_code_candidates = [func for func, degree in in_degrees.items() if degree == 0]
        
        # Cross-file analysis metrics
        cross_file_calls = sum(1 for call in self.calls if call.caller_module != call.callee_module and call.is_resolved)
        total_calls_attempted = len(self.calls) + len(self.unresolved_calls)
        resolution_rate = len(self.calls) / total_calls_attempted if total_calls_attempted > 0 else 0.0
        unresolved_call_names = [call.callee for call in self.unresolved_calls]
        
        self.metrics = AnalysisMetrics(
            total_functions=total_functions,
            total_calls=total_calls,
            total_modules=total_modules,
            max_call_depth=max_call_depth,
            cyclic_dependencies=cyclic_dependencies,
            most_called_functions=most_called,
            most_calling_functions=most_calling,
            module_coupling=module_coupling,
            dead_code_candidates=dead_code_candidates,
            unresolved_calls=unresolved_call_names,
            cross_file_calls=cross_file_calls,
            resolution_rate=resolution_rate
        )
        
        return self.metrics
    
    def _estimate_max_depth(self) -> int:
        """Estimate maximum call depth for cyclic graphs"""
        max_depth = 0
        for node in self.call_graph.nodes():
            try:
                # Try to find longest path from this node
                paths = nx.single_source_shortest_path_length(self.call_graph, node, cutoff=20)
                if paths:
                    max_depth = max(max_depth, max(paths.values()))
            except:
                continue
        return max_depth
    
    def _analyze_module_coupling(self) -> Dict[str, int]:
        """Analyze coupling between modules"""
        module_calls = defaultdict(int)
        
        for call in self.calls:
            caller_module = self._get_function_module(call.caller)
            callee_module = self._get_function_module(call.callee)
            
            if caller_module != callee_module and caller_module and callee_module:
                module_calls[f"{caller_module} -> {callee_module}"] += 1
        
        return dict(module_calls)
    
    def _get_function_module(self, function_name: str) -> str:
        """Get module name for a function"""
        for func in self.functions:
            if func.name == function_name:
                return func.module
        return ""
    
    def get_function_details(self, function_name: str) -> Dict:
        """Get detailed information about a specific function"""
        for func in self.functions:
            if func.name == function_name:
                # Get calling and called functions
                calling = list(self.call_graph.predecessors(function_name))
                called = list(self.call_graph.successors(function_name))
                
                return {
                    'name': func.name,
                    'module': func.module,
                    'parameters': func.parameters,
                    'return_type': func.return_type,
                    'file_path': func.file_path,
                    'start_line': func.start_line,
                    'end_line': func.end_line,
                    'calling_functions': calling,
                    'called_functions': called,
                    'in_degree': self.call_graph.in_degree(function_name),
                    'out_degree': self.call_graph.out_degree(function_name)
                }
        return {}
    
    def get_module_details(self, module_name: str) -> Dict:
        """Get detailed information about a specific module"""
        for module in self.modules:
            if module.name == module_name:
                module_functions = [f for f in self.functions if f.module == module_name]
                
                return {
                    'name': module.name,
                    'file_path': module.file_path,
                    'function_count': len(module_functions),
                    'functions': [f.name for f in module_functions],
                    'internal_calls': self._count_internal_calls(module_name),
                    'external_calls': self._count_external_calls(module_name)
                }
        return {}
    
    def _count_internal_calls(self, module_name: str) -> int:
        """Count calls within a module"""
        count = 0
        for call in self.calls:
            caller_module = self._get_function_module(call.caller)
            callee_module = self._get_function_module(call.callee)
            if caller_module == module_name and callee_module == module_name:
                count += 1
        return count
    
    def _count_external_calls(self, module_name: str) -> int:
        """Count calls from this module to other modules"""
        count = 0
        for call in self.calls:
            caller_module = self._get_function_module(call.caller)
            callee_module = self._get_function_module(call.callee)
            if caller_module == module_name and callee_module != module_name:
                count += 1
        return count
    
    def export_metrics_to_file(self, output_file: str):
        """Export analysis metrics to JSON file"""
        metrics = self.analyze_metrics()
        
        metrics_dict = {
            'total_functions': metrics.total_functions,
            'total_calls': metrics.total_calls,
            'total_modules': metrics.total_modules,
            'max_call_depth': metrics.max_call_depth,
            'cyclic_dependencies': metrics.cyclic_dependencies,
            'most_called_functions': metrics.most_called_functions,
            'most_calling_functions': metrics.most_calling_functions,
            'module_coupling': metrics.module_coupling,
            'dead_code_candidates': metrics.dead_code_candidates,
            'unresolved_calls': metrics.unresolved_calls,
            'cross_file_calls': metrics.cross_file_calls,
            'resolution_rate': metrics.resolution_rate
        }
        
        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(metrics_dict, f, indent=2)
        
        print(f"Metrics exported to {output_file}")
    
    def print_summary(self):
        """Print a summary of the analysis"""
        metrics = self.analyze_metrics()
        
        print("\n=== F# Call Graph Analysis Summary ===")
        print(f"Total Functions: {metrics.total_functions}")
        print(f"Total Calls: {metrics.total_calls}")
        print(f"Total Modules: {metrics.total_modules}")
        print(f"Maximum Call Depth: {metrics.max_call_depth}")
        print(f"Cyclic Dependencies Found: {len(metrics.cyclic_dependencies)}")
        print(f"Dead Code Candidates: {len(metrics.dead_code_candidates)}")
        print(f"Cross-file Calls: {metrics.cross_file_calls}")
        print(f"Call Resolution Rate: {metrics.resolution_rate:.1%}")
        print(f"Unresolved Calls: {len(metrics.unresolved_calls)}")
        
        if metrics.most_called_functions:
            print(f"\nMost Called Functions:")
            for func, count in metrics.most_called_functions[:5]:
                print(f"  {func}: {count} calls")
        
        if metrics.most_calling_functions:
            print(f"\nMost Calling Functions:")
            for func, count in metrics.most_calling_functions[:5]:
                print(f"  {func}: {count} calls")
        
        if metrics.cyclic_dependencies:
            print(f"\nCyclic Dependencies (first 3):")
            for i, cycle in enumerate(metrics.cyclic_dependencies[:3]):
                print(f"  Cycle {i+1}: {' -> '.join(cycle)}")
        
        print("=" * 40)

def main():
    """Main function for command-line usage"""
    import sys
    
    if len(sys.argv) < 2:
        print("Usage: python analyzer.py <ast-json-file> [output-metrics-file]")
        sys.exit(1)
    
    json_file = sys.argv[1]
    output_file = sys.argv[2] if len(sys.argv) > 2 else "analysis-metrics.json"
    
    try:
        analyzer = FSharpAnalyzer(json_file)
        analyzer.print_summary()
        analyzer.export_metrics_to_file(output_file)
        
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()