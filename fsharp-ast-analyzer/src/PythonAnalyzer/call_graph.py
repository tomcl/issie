"""
Call Graph Generation Module
Generates interactive call graphs for D3.js visualization
"""

import json
import networkx as nx
import math
from typing import Dict, List, Tuple, Any
from analyzer import FSharpAnalyzer
import os

class CallGraphGenerator:
    """Generates call graph data for web visualization"""
    
    def __init__(self, analyzer: FSharpAnalyzer):
        self.analyzer = analyzer
        self.graph = analyzer.call_graph
        self.layout_positions = {}
    
    def generate_d3_data(self, filter_options: Dict[str, Any] = None) -> Dict[str, Any]:
        """Generate D3.js compatible JSON data"""
        if filter_options is None:
            filter_options = {}
        
        # Apply filters
        filtered_graph = self._apply_filters(filter_options)
        
        # Generate layout
        positions = self._generate_layout(filtered_graph)
        
        # Create nodes data
        nodes = []
        for node in filtered_graph.nodes():
            node_data = filtered_graph.nodes[node]
            pos = positions.get(node, (0, 0))
            
            # Calculate node metrics
            in_degree = filtered_graph.in_degree(node)
            out_degree = filtered_graph.out_degree(node)
            
            # Determine node color based on module
            module = node_data.get('module', '')
            color = self._get_module_color(module)
            
            # Determine node size based on degree
            size = self._calculate_node_size(in_degree + out_degree)
            
            nodes.append({
                'id': node,
                'label': node,
                'x': pos[0],
                'y': pos[1],
                'module': module,
                'parameters': node_data.get('parameters', []),
                'return_type': node_data.get('return_type', ''),
                'file_path': node_data.get('file_path', ''),
                'start_line': node_data.get('start_line', 0),
                'end_line': node_data.get('end_line', 0),
                'in_degree': in_degree,
                'out_degree': out_degree,
                'color': color,
                'size': size,
                'type': 'function'
            })
        
        # Create edges data
        edges = []
        for edge in filtered_graph.edges():
            source, target = edge
            edge_data = filtered_graph.edges[edge]
            
            edges.append({
                'source': source,
                'target': target,
                'location': edge_data.get('location', [0, 0]),
                'file_path': edge_data.get('file_path', ''),
                'type': 'call'
            })
        
        # Generate statistics
        stats = self._generate_statistics(filtered_graph)
        
        return {
            'nodes': nodes,
            'edges': edges,
            'statistics': stats,
            'layout': 'force-directed',
            'metadata': {
                'generated_by': 'F# AST Analyzer',
                'total_nodes': len(nodes),
                'total_edges': len(edges),
                'filters_applied': filter_options
            }
        }
    
    def _apply_filters(self, filter_options: Dict[str, Any]) -> nx.DiGraph:
        """Apply filters to the graph"""
        filtered_graph = self.graph.copy()
        
        # Filter by module
        if 'modules' in filter_options:
            modules_to_keep = set(filter_options['modules'])
            nodes_to_remove = []
            for node in filtered_graph.nodes():
                node_module = filtered_graph.nodes[node].get('module', '')
                if node_module not in modules_to_keep:
                    nodes_to_remove.append(node)
            filtered_graph.remove_nodes_from(nodes_to_remove)
        
        # Filter by function name pattern
        if 'function_pattern' in filter_options:
            pattern = filter_options['function_pattern'].lower()
            nodes_to_remove = []
            for node in filtered_graph.nodes():
                if pattern not in node.lower():
                    nodes_to_remove.append(node)
            filtered_graph.remove_nodes_from(nodes_to_remove)
        
        # Filter by minimum degree
        if 'min_degree' in filter_options:
            min_degree = filter_options['min_degree']
            nodes_to_remove = []
            for node in filtered_graph.nodes():
                if filtered_graph.degree(node) < min_degree:
                    nodes_to_remove.append(node)
            filtered_graph.remove_nodes_from(nodes_to_remove)
        
        # Filter by maximum depth from root
        if 'max_depth' in filter_options and 'root_function' in filter_options:
            root = filter_options['root_function']
            max_depth = filter_options['max_depth']
            if root in filtered_graph.nodes():
                # Keep nodes within max_depth from root
                try:
                    paths = nx.single_source_shortest_path_length(filtered_graph, root, cutoff=max_depth)
                    nodes_to_keep = set(paths.keys())
                    nodes_to_remove = [node for node in filtered_graph.nodes() if node not in nodes_to_keep]
                    filtered_graph.remove_nodes_from(nodes_to_remove)
                except:
                    pass
        
        return filtered_graph
    
    def _generate_layout(self, graph: nx.DiGraph) -> Dict[str, Tuple[float, float]]:
        """Generate layout positions for nodes"""
        if len(graph.nodes()) == 0:
            return {}
        
        try:
            # Try spring layout first
            pos = nx.spring_layout(graph, k=3, iterations=50, scale=1000)
        except:
            try:
                # Fallback to circular layout
                pos = nx.circular_layout(graph, scale=1000)
            except:
                # Ultimate fallback to random layout
                pos = {}
                for i, node in enumerate(graph.nodes()):
                    angle = 2 * math.pi * i / len(graph.nodes())
                    pos[node] = (500 * math.cos(angle), 500 * math.sin(angle))
        
        return pos
    
    def _get_module_color(self, module: str) -> str:
        """Get color for a module"""
        colors = [
            '#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd',
            '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf',
            '#aec7e8', '#ffbb78', '#98df8a', '#ff9896', '#c5b0d5'
        ]
        
        if not module:
            return '#cccccc'
        
        # Hash module name to get consistent color
        hash_value = hash(module) % len(colors)
        return colors[hash_value]
    
    def _calculate_node_size(self, degree: int) -> int:
        """Calculate node size based on degree"""
        base_size = 20
        max_size = 100
        
        if degree == 0:
            return base_size
        
        # Logarithmic scaling
        size = base_size + (max_size - base_size) * math.log(degree + 1) / math.log(20)
        return min(max_size, max(base_size, int(size)))
    
    def _generate_statistics(self, graph: nx.DiGraph) -> Dict[str, Any]:
        """Generate statistics for the current graph"""
        stats = {
            'node_count': graph.number_of_nodes(),
            'edge_count': graph.number_of_edges(),
            'density': nx.density(graph) if graph.number_of_nodes() > 0 else 0,
            'is_connected': nx.is_weakly_connected(graph) if graph.number_of_nodes() > 0 else False,
            'is_acyclic': nx.is_directed_acyclic_graph(graph),
            'number_of_components': nx.number_weakly_connected_components(graph)
        }
        
        if graph.number_of_nodes() > 0:
            # Degree statistics
            degrees = [graph.degree(n) for n in graph.nodes()]
            stats['avg_degree'] = sum(degrees) / len(degrees)
            stats['max_degree'] = max(degrees)
            stats['min_degree'] = min(degrees)
            
            # In-degree and out-degree statistics
            in_degrees = [graph.in_degree(n) for n in graph.nodes()]
            out_degrees = [graph.out_degree(n) for n in graph.nodes()]
            stats['avg_in_degree'] = sum(in_degrees) / len(in_degrees)
            stats['avg_out_degree'] = sum(out_degrees) / len(out_degrees)
            stats['max_in_degree'] = max(in_degrees)
            stats['max_out_degree'] = max(out_degrees)
        
        return stats
    
    def generate_hierarchical_data(self, root_function: str = None) -> Dict[str, Any]:
        """Generate hierarchical tree data for tree visualization"""
        if self.graph.number_of_nodes() == 0:
            return {'name': 'empty', 'children': []}
            
        if root_function is None:
            # Find a good root (function with high out-degree and low in-degree)
            candidates = []
            for node in self.graph.nodes():
                out_deg = self.graph.out_degree(node)
                in_deg = self.graph.in_degree(node)
                if out_deg > 0:
                    score = out_deg - in_deg
                    candidates.append((node, score))
            
            if candidates:
                root_function = max(candidates, key=lambda x: x[1])[0]
            else:
                root_function = list(self.graph.nodes())[0] if self.graph.nodes() else None
        
        if root_function is None:
            return {'name': 'empty', 'children': []}
        
        visited = set()
        
        def build_tree(node, depth=0, max_depth=5):
            if depth > max_depth or node in visited:
                return None
            
            visited.add(node)
            node_data = self.graph.nodes.get(node, {})
            
            children = []
            for successor in self.graph.successors(node):
                child = build_tree(successor, depth + 1, max_depth)
                if child:
                    children.append(child)
            
            return {
                'name': node,
                'module': node_data.get('module', ''),
                'parameters': node_data.get('parameters', []),
                'return_type': node_data.get('return_type', ''),
                'size': self._calculate_node_size(self.graph.degree(node)),
                'children': children,
                'depth': depth
            }
        
        return build_tree(root_function)
    
    def export_to_json(self, output_file: str, filter_options: Dict[str, Any] = None):
        """Export call graph data to JSON file"""
        data = self.generate_d3_data(filter_options)
        
        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=2)
        
        print(f"Call graph data exported to {output_file}")
    
    def export_hierarchical_json(self, output_file: str, root_function: str = None):
        """Export hierarchical tree data to JSON file"""
        data = self.generate_hierarchical_data(root_function)
        
        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=2)
        
        print(f"Hierarchical tree data exported to {output_file}")
    
    def generate_graphviz_dot(self, output_file: str, filter_options: Dict[str, Any] = None):
        """Generate Graphviz DOT format for high-quality static visualization"""
        filtered_graph = self._apply_filters(filter_options or {})
        
        dot_content = ['digraph CallGraph {']
        dot_content.append('  rankdir=TB;')
        dot_content.append('  node [shape=box, style=filled];')
        dot_content.append('  edge [color=gray];')
        dot_content.append('')
        
        # Add nodes
        for node in filtered_graph.nodes():
            node_data = filtered_graph.nodes[node]
            module = node_data.get('module', '')
            color = self._get_module_color(module)
            
            # Escape node name for DOT format
            safe_name = node.replace('"', '\\"').replace('\n', '\\n')
            dot_content.append(f'  "{safe_name}" [label="{safe_name}\\n({module})", fillcolor="{color}"];')
        
        dot_content.append('')
        
        # Add edges
        for edge in filtered_graph.edges():
            source, target = edge
            safe_source = source.replace('"', '\\"')
            safe_target = target.replace('"', '\\"')
            dot_content.append(f'  "{safe_source}" -> "{safe_target}";')
        
        dot_content.append('}')
        
        with open(output_file, 'w', encoding='utf-8') as f:
            f.write('\n'.join(dot_content))
        
        print(f"Graphviz DOT file exported to {output_file}")

def main():
    """Main function for command-line usage"""
    import sys
    
    if len(sys.argv) < 2:
        print("Usage: python call_graph.py <ast-json-file> [output-graph-file]")
        sys.exit(1)
    
    json_file = sys.argv[1]
    output_file = sys.argv[2] if len(sys.argv) > 2 else "call-graph.json"
    
    try:
        analyzer = FSharpAnalyzer(json_file)
        generator = CallGraphGenerator(analyzer)
        
        # Generate main call graph
        generator.export_to_json(output_file)
        
        # Generate hierarchical tree
        tree_file = output_file.replace('.json', '-tree.json')
        generator.export_hierarchical_json(tree_file)
        
        # Generate DOT file
        dot_file = output_file.replace('.json', '.dot')
        generator.generate_graphviz_dot(dot_file)
        
        print(f"Call graph generation complete!")
        
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()