// F# Call Graph Visualizer
class CallGraphVisualizer {
    constructor() {
        this.data = null;
        this.filteredData = null;
        this.svg = null;
        this.g = null;
        this.simulation = null;
        this.nodes = [];
        this.edges = [];
        this.selectedNode = null;
        this.searchTerm = '';
        this.moduleFilter = '';
        this.currentLayout = 'force';
        
        this.initializeElements();
        this.setupEventListeners();
        this.initializeSVG();
    }
    
    initializeElements() {
        this.fileInput = document.getElementById('fileInput');
        this.searchInput = document.getElementById('searchInput');
        this.moduleFilter = document.getElementById('moduleFilter');
        this.layoutSelect = document.getElementById('layoutSelect');
        this.resetZoomBtn = document.getElementById('resetZoom');
        this.exportSvgBtn = document.getElementById('exportSvg');
        this.exportPngBtn = document.getElementById('exportPng');
        this.loadingOverlay = document.getElementById('loadingOverlay');
        this.tooltip = document.getElementById('tooltip');
        
        // Statistics elements
        this.totalFunctions = document.getElementById('totalFunctions');
        this.totalCalls = document.getElementById('totalCalls');
        this.totalModules = document.getElementById('totalModules');
        this.graphDensity = document.getElementById('graphDensity');
        this.isAcyclic = document.getElementById('isAcyclic');
        this.nodeDetails = document.getElementById('nodeDetails');
    }
    
    setupEventListeners() {
        this.fileInput.addEventListener('change', (e) => this.handleFileLoad(e));
        this.searchInput.addEventListener('input', (e) => this.handleSearch(e));
        this.moduleFilter.addEventListener('change', (e) => this.handleModuleFilter(e));
        this.layoutSelect.addEventListener('change', (e) => this.handleLayoutChange(e));
        this.resetZoomBtn.addEventListener('click', () => this.resetZoom());
        this.exportSvgBtn.addEventListener('click', () => this.exportSVG());
        this.exportPngBtn.addEventListener('click', () => this.exportPNG());
        
        // Handle file drop
        document.addEventListener('dragover', (e) => {
            e.preventDefault();
            e.stopPropagation();
        });
        
        document.addEventListener('drop', (e) => {
            e.preventDefault();
            e.stopPropagation();
            const files = e.dataTransfer.files;
            if (files.length > 0 && files[0].type === 'application/json') {
                this.loadFile(files[0]);
            }
        });
    }
    
    initializeSVG() {
        this.svg = d3.select('#callGraphSvg');
        this.g = this.svg.append('g');
        
        // Define arrowhead marker
        this.svg.append('defs').append('marker')
            .attr('id', 'arrowhead')
            .attr('viewBox', '0 -5 10 10')
            .attr('refX', 15)
            .attr('refY', 0)
            .attr('markerWidth', 6)
            .attr('markerHeight', 6)
            .attr('orient', 'auto')
            .append('path')
            .attr('d', 'M0,-5L10,0L0,5')
            .attr('fill', '#999');
        
        // Setup zoom behavior
        this.zoom = d3.zoom()
            .scaleExtent([0.1, 10])
            .on('zoom', (event) => {
                this.g.attr('transform', event.transform);
            });
        
        this.svg.call(this.zoom);
        
        // Add zoom controls
        this.addZoomControls();
    }
    
    addZoomControls() {
        const zoomControls = d3.select('.visualization')
            .append('div')
            .attr('class', 'zoom-controls');
        
        zoomControls.append('button')
            .attr('class', 'zoom-button')
            .text('+')
            .on('click', () => {
                this.svg.transition().duration(300).call(
                    this.zoom.scaleBy, 1.5
                );
            });
        
        zoomControls.append('button')
            .attr('class', 'zoom-button')
            .text('−')
            .on('click', () => {
                this.svg.transition().duration(300).call(
                    this.zoom.scaleBy, 1 / 1.5
                );
            });
    }
    
    showLoading(show = true) {
        this.loadingOverlay.classList.toggle('show', show);
    }
    
    handleFileLoad(event) {
        const file = event.target.files[0];
        if (file) {
            this.loadFile(file);
        }
    }
    
    async loadFile(file) {
        this.showLoading(true);
        
        try {
            const text = await file.text();
            const data = JSON.parse(text);
            this.loadData(data);
        } catch (error) {
            console.error('Error loading file:', error);
            alert('Error loading file. Please check the file format.');
        } finally {
            this.showLoading(false);
        }
    }
    
    loadData(data) {
        this.data = data;
        this.filteredData = { ...data };
        
        // Update statistics
        this.updateStatistics();
        
        // Populate module filter
        this.populateModuleFilter();
        
        // Create visualization
        this.createVisualization();
        
        console.log('Data loaded:', data);
    }
    
    updateStatistics() {
        const stats = this.data.statistics || {};
        
        this.totalFunctions.textContent = stats.node_count || this.data.nodes.length;
        this.totalCalls.textContent = stats.edge_count || this.data.edges.length;
        this.totalModules.textContent = this.getUniqueModules().length;
        this.graphDensity.textContent = (stats.density || 0).toFixed(3);
        this.isAcyclic.textContent = stats.is_acyclic ? 'Yes' : 'No';
    }
    
    getUniqueModules() {
        const modules = new Set();
        this.data.nodes.forEach(node => {
            if (node.module) {
                modules.add(node.module);
            }
        });
        return Array.from(modules);
    }
    
    populateModuleFilter() {
        const modules = this.getUniqueModules();
        const select = this.moduleFilter;
        
        // Clear existing options except the first one
        while (select.children.length > 1) {
            select.removeChild(select.lastChild);
        }
        
        modules.forEach(module => {
            const option = document.createElement('option');
            option.value = module;
            option.textContent = module;
            select.appendChild(option);
        });
    }
    
    handleSearch(event) {
        this.searchTerm = event.target.value.toLowerCase();
        this.applyFilters();
    }
    
    handleModuleFilter(event) {
        this.moduleFilter = event.target.value;
        this.applyFilters();
    }
    
    handleLayoutChange(event) {
        this.currentLayout = event.target.value;
        this.createVisualization();
    }
    
    applyFilters() {
        let filteredNodes = this.data.nodes;
        let filteredEdges = this.data.edges;
        
        // Apply search filter
        if (this.searchTerm) {
            filteredNodes = filteredNodes.filter(node => 
                node.id.toLowerCase().includes(this.searchTerm)
            );
        }
        
        // Apply module filter
        if (this.moduleFilter) {
            filteredNodes = filteredNodes.filter(node => 
                node.module === this.moduleFilter
            );
        }
        
        // Filter edges to only include nodes that are still present
        const nodeIds = new Set(filteredNodes.map(node => node.id));
        filteredEdges = filteredEdges.filter(edge => 
            nodeIds.has(edge.source) && nodeIds.has(edge.target)
        );
        
        this.filteredData = {
            ...this.data,
            nodes: filteredNodes,
            edges: filteredEdges
        };
        
        this.updateVisualization();
        this.highlightSearch();
    }
    
    createVisualization() {
        if (!this.filteredData) return;
        
        this.clearVisualization();
        
        switch (this.currentLayout) {
            case 'force':
                this.createForceLayout();
                break;
            case 'hierarchical':
                this.createHierarchicalLayout();
                break;
            case 'circular':
                this.createCircularLayout();
                break;
        }
    }
    
    clearVisualization() {
        this.g.selectAll('*').remove();
        if (this.simulation) {
            this.simulation.stop();
        }
    }
    
    createForceLayout() {
        const width = this.svg.node().clientWidth;
        const height = this.svg.node().clientHeight;
        
        // Create simulation
        this.simulation = d3.forceSimulation(this.filteredData.nodes)
            .force('link', d3.forceLink(this.filteredData.edges).id(d => d.id).distance(100))
            .force('charge', d3.forceManyBody().strength(-300))
            .force('center', d3.forceCenter(width / 2, height / 2))
            .force('collision', d3.forceCollide().radius(30));
        
        this.createElements();
        
        // Update positions on simulation tick
        this.simulation.on('tick', () => {
            this.updatePositions();
        });
    }
    
    createHierarchicalLayout() {
        const width = this.svg.node().clientWidth;
        const height = this.svg.node().clientHeight;
        
        // Create a hierarchical layout
        const nodes = [...this.filteredData.nodes];
        const edges = [...this.filteredData.edges];
        
        // Simple hierarchical positioning
        const levels = this.calculateNodeLevels(nodes, edges);
        const levelHeight = height / (levels.length + 1);
        
        levels.forEach((level, i) => {
            const levelWidth = width / (level.length + 1);
            level.forEach((node, j) => {
                node.x = (j + 1) * levelWidth;
                node.y = (i + 1) * levelHeight;
                node.fx = node.x;
                node.fy = node.y;
            });
        });
        
        this.createElements();
        this.updatePositions();
    }
    
    createCircularLayout() {
        const width = this.svg.node().clientWidth;
        const height = this.svg.node().clientHeight;
        const centerX = width / 2;
        const centerY = height / 2;
        const radius = Math.min(width, height) / 3;
        
        // Position nodes in a circle
        this.filteredData.nodes.forEach((node, i) => {
            const angle = (2 * Math.PI * i) / this.filteredData.nodes.length;
            node.x = centerX + radius * Math.cos(angle);
            node.y = centerY + radius * Math.sin(angle);
            node.fx = node.x;
            node.fy = node.y;
        });
        
        this.createElements();
        this.updatePositions();
    }
    
    calculateNodeLevels(nodes, edges) {
        const levels = [];
        const nodeLevel = {};
        const visited = new Set();
        
        // Find root nodes (nodes with no incoming edges)
        const incomingCount = {};
        nodes.forEach(node => incomingCount[node.id] = 0);
        edges.forEach(edge => incomingCount[edge.target]++);
        
        const roots = nodes.filter(node => incomingCount[node.id] === 0);
        
        // BFS to assign levels
        let currentLevel = roots;
        let levelIndex = 0;
        
        while (currentLevel.length > 0) {
            levels[levelIndex] = [...currentLevel];
            currentLevel.forEach(node => {
                nodeLevel[node.id] = levelIndex;
                visited.add(node.id);
            });
            
            const nextLevel = [];
            currentLevel.forEach(node => {
                edges.forEach(edge => {
                    if (edge.source === node.id && !visited.has(edge.target)) {
                        const targetNode = nodes.find(n => n.id === edge.target);
                        if (targetNode && !nextLevel.includes(targetNode)) {
                            nextLevel.push(targetNode);
                        }
                    }
                });
            });
            
            currentLevel = nextLevel;
            levelIndex++;
        }
        
        // Add any remaining nodes to the last level
        const remainingNodes = nodes.filter(node => !visited.has(node.id));
        if (remainingNodes.length > 0) {
            levels[levelIndex] = remainingNodes;
        }
        
        return levels;
    }
    
    createElements() {
        // Create edges
        this.edges = this.g.append('g')
            .attr('class', 'edges')
            .selectAll('line')
            .data(this.filteredData.edges)
            .join('line')
            .attr('class', 'edge')
            .on('mouseover', (event, d) => this.showTooltip(event, d, 'edge'))
            .on('mouseout', () => this.hideTooltip());
        
        // Create nodes
        this.nodes = this.g.append('g')
            .attr('class', 'nodes')
            .selectAll('circle')
            .data(this.filteredData.nodes)
            .join('circle')
            .attr('class', 'node')
            .attr('r', d => d.size || 20)
            .attr('fill', d => d.color || '#1f77b4')
            .on('click', (event, d) => this.selectNode(d))
            .on('mouseover', (event, d) => this.showTooltip(event, d, 'node'))
            .on('mouseout', () => this.hideTooltip());
        
        // Add labels
        this.labels = this.g.append('g')
            .attr('class', 'labels')
            .selectAll('text')
            .data(this.filteredData.nodes)
            .join('text')
            .attr('class', 'node-label')
            .text(d => d.label || d.id)
            .attr('dy', 5);
        
        // Add drag behavior for force layout
        if (this.currentLayout === 'force') {
            this.nodes.call(d3.drag()
                .on('start', (event, d) => this.dragStarted(event, d))
                .on('drag', (event, d) => this.dragged(event, d))
                .on('end', (event, d) => this.dragEnded(event, d))
            );
        }
    }
    
    updatePositions() {
        this.edges
            .attr('x1', d => d.source.x)
            .attr('y1', d => d.source.y)
            .attr('x2', d => d.target.x)
            .attr('y2', d => d.target.y);
        
        this.nodes
            .attr('cx', d => d.x)
            .attr('cy', d => d.y);
        
        this.labels
            .attr('x', d => d.x)
            .attr('y', d => d.y);
    }
    
    updateVisualization() {
        this.createVisualization();
    }
    
    highlightSearch() {
        if (!this.searchTerm) {
            this.nodes.classed('search-highlight', false);
            return;
        }
        
        this.nodes.classed('search-highlight', d => 
            d.id.toLowerCase().includes(this.searchTerm)
        );
    }
    
    selectNode(node) {
        // Remove previous selection
        this.nodes.classed('selected', false);
        
        // Select new node
        this.nodes.classed('selected', d => d.id === node.id);
        this.selectedNode = node;
        
        // Update node details
        this.updateNodeDetails(node);
        
        // Highlight connected edges
        this.highlightConnectedEdges(node);
    }
    
    updateNodeDetails(node) {
        const details = `
            <div class="node-detail-item">
                <span class="node-detail-label">Name:</span>
                <span class="node-detail-value">${node.id}</span>
            </div>
            <div class="node-detail-item">
                <span class="node-detail-label">Module:</span>
                <span class="node-detail-value">${node.module || 'N/A'}</span>
            </div>
            <div class="node-detail-item">
                <span class="node-detail-label">Parameters:</span>
                <span class="node-detail-value">${node.parameters ? node.parameters.join(', ') : 'N/A'}</span>
            </div>
            <div class="node-detail-item">
                <span class="node-detail-label">Return Type:</span>
                <span class="node-detail-value">${node.return_type || 'N/A'}</span>
            </div>
            <div class="node-detail-item">
                <span class="node-detail-label">Incoming Calls:</span>
                <span class="node-detail-value">${node.in_degree || 0}</span>
            </div>
            <div class="node-detail-item">
                <span class="node-detail-label">Outgoing Calls:</span>
                <span class="node-detail-value">${node.out_degree || 0}</span>
            </div>
            <div class="node-detail-item">
                <span class="node-detail-label">File:</span>
                <span class="node-detail-value">${node.file_path || 'N/A'}</span>
            </div>
        `;
        
        this.nodeDetails.innerHTML = details;
    }
    
    highlightConnectedEdges(node) {
        this.edges.classed('highlighted', d => 
            d.source.id === node.id || d.target.id === node.id
        );
    }
    
    showTooltip(event, data, type) {
        let content = '';
        
        if (type === 'node') {
            content = `
                <strong>${data.id}</strong><br>
                Module: ${data.module || 'N/A'}<br>
                Calls: ${data.in_degree || 0} in, ${data.out_degree || 0} out
            `;
        } else if (type === 'edge') {
            content = `
                <strong>${data.source.id || data.source}</strong><br>
                → <strong>${data.target.id || data.target}</strong><br>
                Location: ${data.location ? data.location.join(', ') : 'N/A'}
            `;
        }
        
        this.tooltip.innerHTML = content;
        this.tooltip.style.left = (event.pageX + 10) + 'px';
        this.tooltip.style.top = (event.pageY + 10) + 'px';
        this.tooltip.classList.add('show');
    }
    
    hideTooltip() {
        this.tooltip.classList.remove('show');
    }
    
    dragStarted(event, d) {
        if (!event.active) this.simulation.alphaTarget(0.3).restart();
        d.fx = d.x;
        d.fy = d.y;
    }
    
    dragged(event, d) {
        d.fx = event.x;
        d.fy = event.y;
    }
    
    dragEnded(event, d) {
        if (!event.active) this.simulation.alphaTarget(0);
        d.fx = null;
        d.fy = null;
    }
    
    resetZoom() {
        this.svg.transition().duration(750).call(
            this.zoom.transform,
            d3.zoomIdentity
        );
    }
    
    exportSVG() {
        const svgData = new XMLSerializer().serializeToString(this.svg.node());
        const svgBlob = new Blob([svgData], { type: 'image/svg+xml;charset=utf-8' });
        const svgUrl = URL.createObjectURL(svgBlob);
        
        const link = document.createElement('a');
        link.href = svgUrl;
        link.download = 'call-graph.svg';
        link.click();
        
        URL.revokeObjectURL(svgUrl);
    }
    
    exportPNG() {
        const svgData = new XMLSerializer().serializeToString(this.svg.node());
        const canvas = document.createElement('canvas');
        const ctx = canvas.getContext('2d');
        const img = new Image();
        
        img.onload = () => {
            canvas.width = img.width;
            canvas.height = img.height;
            ctx.drawImage(img, 0, 0);
            
            canvas.toBlob((blob) => {
                const url = URL.createObjectURL(blob);
                const link = document.createElement('a');
                link.href = url;
                link.download = 'call-graph.png';
                link.click();
                URL.revokeObjectURL(url);
            });
        };
        
        const svgBlob = new Blob([svgData], { type: 'image/svg+xml;charset=utf-8' });
        const url = URL.createObjectURL(svgBlob);
        img.src = url;
    }
}

// Initialize the visualizer when the page loads
document.addEventListener('DOMContentLoaded', () => {
    const visualizer = new CallGraphVisualizer();
    
    // Load sample data if available
    fetch('call-graph.json')
        .then(response => response.json())
        .then(data => visualizer.loadData(data))
        .catch(error => console.log('No sample data available:', error));
});