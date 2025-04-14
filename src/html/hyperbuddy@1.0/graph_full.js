/**
 * HyperBEAM Cache Graph Renderer
 * A 2D force-directed graph visualization for the HyperBEAM cache system
 */
class CacheGraphRenderer {
    constructor(containerId) {
        // DOM elements
        this.container = document.getElementById(containerId);
        this.nodeCountEl = document.getElementById('node-count');
        this.linkCountEl = document.getElementById('link-count');
        this.loadingEl = document.getElementById('loading');
        this.searchInput = document.getElementById('search-input');
        
        // State tracking
        this.selectedNode = null;
        this.neighborNodes = new Set();
        this.activeLinks = new Set();
        this.hoveredNode = null;
        this.previousSearchValue = '';
        this.isUpdatingAutocomplete = false;
        
        // Autocomplete state
        this.autocompleteList = null;
        this.autocompleteSuggestions = [];
        this.autocompleteSelectedIndex = -1;
        
        // Configuration
        this.config = {
            // Node styling
            nodeSize: {
                simple: 6,
                composite: 8
            },
            // Color scheme
            colors: {
                background: 0xf9f9f9,
                simpleNode: 0x6495ED,     // Light blue
                compositeNode: 0xF08080,  // Light coral
                highlight: 0xFFA500,      // Orange for highlighting
                selectedNode: 0xFF5500,   // Orange-red for selected node
                neighborNode: 0x4CAF50,   // Green for neighbor nodes
                link: 0xcccccc,           // Light gray for links
                activeLink: 0x333333,     // Dark gray for active links
                hover: 0xffcc88           // Warm orange/yellow for hover
            },
            // Display options
            showLabels: true,
            physicsEnabled: true,
            // Physics settings
            defaultDistance: 300,
            highConnectionThreshold: 10,
            // Camera settings
            zoomLevel: {
                default: 1.0,
                focused: 2.5
            },
            // Z-positions for layering
            zPos: {
                line: 0,
                node: 5, 
                label: 10
            }
        };
        
        // Scene objects and data
        this.scene = null;
        this.camera = null;
        this.renderer = null;
        this.controls = null;
        this.simulation = null;
        this.raycaster = new THREE.Raycaster();
        this.mouse = new THREE.Vector2();
        
        // Graph data
        this.graphData = { nodes: [], links: [] };
        this.graphObjects = { nodes: new Map(), links: new Map() };
        
        // Initialize and load
        this.initRenderer();
        this.setupEventListeners();
        this.loadGraphData();
    }
    
    /**
     * Initialize the Three.js scene and renderer
     */
    initRenderer() {
        const width = this.container.clientWidth;
        const height = this.container.clientHeight;
        
        // Create scene with background color
        this.scene = new THREE.Scene();
        this.scene.background = new THREE.Color(this.config.colors.background);
        
        // Create orthographic camera for 2D view
        this.camera = new THREE.OrthographicCamera(
            width / -2, width / 2, 
            height / 2, height / -2, 
            0.1, 1000
        );
        this.camera.position.z = 500;
        
        // Create renderer
        this.renderer = new THREE.WebGLRenderer({ 
            antialias: false,
            alpha: true
        });
        this.renderer.setSize(width, height);
        this.renderer.setClearColor(this.config.colors.background, 1);
        this.renderer.sortObjects = true; // Enable sorting for proper z-ordering
        this.container.appendChild(this.renderer.domElement);
        
        // Add orbit controls limited to 2D movement
        this.controls = new THREE.OrbitControls(this.camera, this.renderer.domElement);
        this.controls.enableDamping = true;
        this.controls.dampingFactor = 0.1;
        this.controls.enableRotate = false; // Disable 3D rotation
        
        // Add lighting
        this.scene.add(new THREE.AmbientLight(0xffffff, 0.6));
        const directionalLight = new THREE.DirectionalLight(0xffffff, 0.8);
        directionalLight.position.set(0, 0, 200);
        this.scene.add(directionalLight);
        
        // Initialize force simulation
        this.initForceSimulation();
        
        // Set up event listeners for interaction
        this.setupGraphInteraction();
        
        // Start animation loop
        this.animate();
        
        // Handle window resize
        window.addEventListener('resize', () => this.onWindowResize());
    }
    
    /**
     * Set up graph-specific interaction handlers
     */
    setupGraphInteraction() {
        // Add click event listener for node selection
        this.renderer.domElement.addEventListener('click', this.onMouseClick.bind(this));
        
        // Add double-click event listener for camera focus
        this.renderer.domElement.addEventListener('dblclick', this.onDoubleClick.bind(this));
        
        // Add hover event listeners
        this.renderer.domElement.addEventListener('mousemove', this.onMouseMove.bind(this));
    }
    
    /**
     * Initialize D3 force simulation
     */
    initForceSimulation() {
        // Calculate connection counts for better distribution
        const connectionCounts = new Map();
        
        // Count connections for each node
        this.graphData.links.forEach(link => {
            const sourceId = typeof link.source === 'object' ? link.source.id : link.source;
            const targetId = typeof link.target === 'object' ? link.target.id : link.target;
            
            connectionCounts.set(sourceId, (connectionCounts.get(sourceId) || 0) + 1);
            connectionCounts.set(targetId, (connectionCounts.get(targetId) || 0) + 1);
        });
        
        // Link distance based on connection count
        const linkDistance = link => {
            const sourceId = typeof link.source === 'object' ? link.source.id : link.source;
            const targetId = typeof link.target === 'object' ? link.target.id : link.target;
            
            const sourceConnections = connectionCounts.get(sourceId) || 0;
            const targetConnections = connectionCounts.get(targetId) || 0;
            
            // Scale distance based on connection count
            const baseDistance = this.config.defaultDistance;
            const connectionFactor = Math.max(sourceConnections, targetConnections);
            
            return baseDistance * (1 + Math.log(1 + connectionFactor * 0.2));
        };
        
        // Collision radius based on connection count
        const collisionRadius = node => {
            const connections = connectionCounts.get(node.id) || 0;
            const baseRadius = 15;
            
            // Increase collision radius for highly connected nodes
            if (connections > this.config.highConnectionThreshold) {
                return baseRadius * (1 + Math.log(connections) * 0.1);
            }
            return baseRadius;
        };
        
        // Create the simulation with all forces
        this.simulation = d3.forceSimulation()
            .force('link', d3.forceLink().id(d => d.id).distance(linkDistance))
            .force('charge', d3.forceManyBody().strength(-200))
            .force('center', d3.forceCenter(0, 0))
            .force('collision', d3.forceCollide().radius(collisionRadius))
            .force('x', d3.forceX().strength(0.01))
            .force('y', d3.forceY().strength(0.01))
            .on('tick', () => this.onSimulationTick());
    }
    
    /**
     * Set up event listeners for UI controls
     */
    setupEventListeners() {
        // Create autocomplete UI
        this.createAutocompleteUI();
        
        // Button event listeners
        document.getElementById('refresh-btn')?.addEventListener('click', () => this.loadGraphData());
        document.getElementById('toggle-labels-btn')?.addEventListener('click', () => {
            this.config.showLabels = !this.config.showLabels;
            this.toggleLabels();
        });
        document.getElementById('toggle-physics-btn')?.addEventListener('click', () => {
            this.config.physicsEnabled = !this.config.physicsEnabled;
            this.simulation.alpha(this.config.physicsEnabled ? 0.3 : 0).restart();
        });
        document.getElementById('reset-btn')?.addEventListener('click', () => {
            this.resetView();
            this.hideAutocomplete();
            this.previousSearchValue = '';
        });
        
        // Set up search input with debounced handling
        if (this.searchInput) {
            let searchTimeout = null;
            
            // Input event for search text changes
            this.searchInput.addEventListener('input', (e) => {
                if (this.isUpdatingAutocomplete) return;
                
                const currentValue = e.target.value.trim();
                if (currentValue === this.previousSearchValue) return;
                
                this.previousSearchValue = currentValue;
                clearTimeout(searchTimeout);
                
                if (currentValue.length > 0) {
                    this.isUpdatingAutocomplete = true;
                    searchTimeout = setTimeout(() => {
                        this.updateAutocompleteSuggestions(currentValue);
                        this.isUpdatingAutocomplete = false;
                    }, 250);
                } else {
                    this.hideAutocomplete();
                }
            });
            
            // Keyboard navigation in autocomplete dropdown
            this.searchInput.addEventListener('keydown', (e) => {
                if (['ArrowUp', 'ArrowDown', 'Enter', 'Escape'].includes(e.key)) {
                    this.handleAutocompleteKeydown(e);
                }
            });
            
            // Focus handler to show autocomplete
            this.searchInput.addEventListener('focus', () => {
                if (this.isUpdatingAutocomplete) return;
                
                const currentValue = this.searchInput.value.trim();
                if (currentValue.length > 0) {
                    this.previousSearchValue = currentValue;
                    this.isUpdatingAutocomplete = true;
                    this.updateAutocompleteSuggestions(currentValue);
                    this.isUpdatingAutocomplete = false;
                }
            });
        }
        
        // Hide autocomplete when clicking outside
        document.addEventListener('click', (e) => {
            if (this.autocompleteList && e.target !== this.searchInput && !this.autocompleteList.contains(e.target)) {
                this.hideAutocomplete();
            }
        });
    }
    
    /**
     * Load graph data from the server
     */
    loadGraphData() {
        this.showLoading(true);
        
        fetch('/~hyperbuddy@1.0/graph-data')
            .then(response => {
                if (!response.ok) {
                    throw new Error(`HTTP error ${response.status}`);
                }
                return response.json();
            })
            .then(data => {
                this.clearGraph();
                
                // Validate data
                if (!this.validateGraphData(data)) {
                    return;
                }
                
                this.graphData = data;
                this.createGraph();
                this.updateStats();
                this.showLoading(false);
            })
            .catch(error => {
                console.error('Error loading graph data:', error);
                this.showError('Failed to load graph data: ' + error.message);
                this.showLoading(false);
            });
    }
    
    /**
     * Validate the graph data
     */
    validateGraphData(data) {
        // Check if we have valid data
        if (!data || !data.nodes || !data.links || 
            !Array.isArray(data.nodes) || !Array.isArray(data.links)) {
            this.showError('Invalid data format received from server');
            return false;
        }
        
        // Check if we have any nodes
        if (data.nodes.length === 0) {
            this.showError('No graph data available');
            return false;
        }
        
        return true;
    }
    
    /**
     * Show or hide the loading indicator
     */
    showLoading(show) {
        if (this.loadingEl) {
            this.loadingEl.style.display = show ? 'block' : 'none';
        }
    }
    
    /**
     * Create the 2D graph based on the loaded data
     */
    createGraph() {
        // Apply initial layout
        this.graphData.nodes.forEach(node => {
            // Random initial placement
            node.x = (Math.random() - 0.5) * 500;
            node.y = (Math.random() - 0.5) * 500;
            node.z = 0;
            
            // Determine node type based on ID pattern if not set
            if (!node.type) {
                const pathParts = node.id.split('/').filter(p => p.length > 0);
                node.type = (pathParts.length <= 1 && !node.id.endsWith('/')) ? 'simple' : 'composite';
            }
        });
        
        // Create nodes first
        this.graphData.nodes.forEach(node => this.createNodeObject(node));
        
        // Then create links
        this.graphData.links.forEach(link => this.createLinkObject(link));
        
        // Update simulation
        this.simulation.nodes(this.graphData.nodes);
        this.simulation.force('link').links(this.graphData.links);
        
        // Run simulation for a few ticks to improve initial layout
        for (let i = 0; i < 50; i++) {
            this.simulation.tick();
        }
        
        // Update visual positions
        this.onSimulationTick();
        
        // Start simulation if physics is enabled
        if (this.config.physicsEnabled) {
            this.simulation.alpha(1).restart();
        }
        
        // Update statistics
        this.updateStats();
    }
    
    /**
     * Create a node object in the scene
     */
    createNodeObject(node) {
        // Get node type and size
        const isSimple = node.type === 'simple';
        const size = isSimple ? this.config.nodeSize.simple : this.config.nodeSize.composite;
        const color = isSimple ? this.config.colors.simpleNode : this.config.colors.compositeNode;
        
        // Create geometry and material
        const geometry = new THREE.CircleGeometry(size, 32);
        const material = new THREE.MeshBasicMaterial({ 
            color,
            depthWrite: true,
            depthTest: true
        });
        
        // Create mesh
        const mesh = new THREE.Mesh(geometry, material);
        mesh.position.set(node.x || 0, node.y || 0, this.config.zPos.node);
        mesh.userData = { id: node.id, type: node.type, label: node.label };
        mesh.renderOrder = 10;
        mesh.callback = () => this.selectNode(node.id);
        
        // Add to scene
        this.scene.add(mesh);
        
        // Create label if enabled
        if (this.config.showLabels) {
            this.createLabel(node);
        }
        
        // Store references
        node.object = mesh;
        this.graphObjects.nodes.set(node.id, node);
    }
    
    /**
     * Create a link object between nodes
     */
    createLinkObject(link) {
        // Get node IDs
        const sourceId = typeof link.source === 'object' ? link.source.id : link.source;
        const targetId = typeof link.target === 'object' ? link.target.id : link.target;
        
        // Get node objects
        const sourceNode = this.graphObjects.nodes.get(sourceId);
        const targetNode = this.graphObjects.nodes.get(targetId);
        
        if (!sourceNode || !targetNode) {
            return;
        }
        
        // Create line geometry
        const points = [
            new THREE.Vector3(sourceNode.x, sourceNode.y, this.config.zPos.line),
            new THREE.Vector3(targetNode.x, targetNode.y, this.config.zPos.line)
        ];
        
        // Create material and line
        const material = new THREE.LineDashedMaterial({
            color: this.config.colors.link,
            dashSize: 2.5,
            gapSize: 1.5,
            transparent: true,
            opacity: 0.6,
            linewidth: 1,
            depthWrite: false
        });
        
        const geometry = new THREE.BufferGeometry().setFromPoints(points);
        const line = new THREE.Line(geometry, material);
        line.computeLineDistances();
        line.renderOrder = 0;
        
        this.scene.add(line);
        
        // Store connection info
        link.sourceId = sourceId;
        link.targetId = targetId;
        link.line = line;
        
        // Create label if needed
        if (this.config.showLabels && link.label) {
            this.createLinkLabel(link, sourceNode, targetNode);
        }
        
        // Store reference
        this.graphObjects.links.set(`${sourceId}-${targetId}`, link);
    }
    
    /**
     * Create label for a node
     */
    createLabel(node) {
        const label = new SpriteText(node.label || node.id);
        
        // Improved text rendering settings
        label.fontFace = 'Arial, Helvetica, sans-serif';
        label.fontSize = 12;
        label.fontWeight = '600';
        label.strokeWidth = 0; // No stroke for sharper text
        label.color = '#000000';
        label.backgroundColor = 'rgba(255,255,255,0.9)';
        label.padding = 3;
        label.textHeight = 1; // Force better resolution
        label.borderWidth = 0; // No border for sharper edges
        
        // Position above node with pixel-perfect positioning
        const isSimple = node.type === 'simple';
        const offset = isSimple ? this.config.nodeSize.simple + 2 : this.config.nodeSize.composite + 2;
        
        // Round to whole pixels to avoid subpixel rendering
        const x = Math.round(node.x || 0);
        const y = Math.round((node.y || 0) + offset);
        const z = this.config.zPos.label;
        
        label.position.set(x, y, z);
        label.renderOrder = 20;
        
        this.scene.add(label);
        node.labelObject = label;
    }
    
    /**
     * Create a label for a link
     */
    createLinkLabel(link, sourceNode, targetNode) {
        const midPoint = new THREE.Vector3(
            (sourceNode.x + targetNode.x) / 2,
            (sourceNode.y + targetNode.y) / 2,
            this.config.zPos.label
        );
        
        const label = new SpriteText(link.label);
        
        // Improved text rendering settings
        label.fontFace = 'Arial, Helvetica, sans-serif';
        label.fontSize = 10;
        label.fontWeight = '600';
        label.strokeWidth = 0; // No stroke for sharper text
        label.color = '#000000';
        label.backgroundColor = 'rgba(255,255,255,0.9)';
        label.padding = 2;
        label.textHeight = 1; // Force better resolution
        label.borderWidth = 0; // No border for sharper edges
        
        // Round to whole pixels to avoid subpixel rendering
        midPoint.x = Math.round(midPoint.x);
        midPoint.y = Math.round(midPoint.y);
        
        label.position.copy(midPoint);
        label.renderOrder = 20;
        
        this.scene.add(label);
        link.labelObject = label;
    }
    
    /**
     * Toggle visibility of all labels
     */
    toggleLabels() {
        // Update node labels
        this.graphData.nodes.forEach(node => {
            if (node.labelObject) {
                node.labelObject.visible = this.config.showLabels;
            } else if (this.config.showLabels) {
                // Create label if it doesn't exist yet
                const isSimple = node.type === 'simple';
                const label = new SpriteText(node.label);
                label.color = '#ffffff';
                label.backgroundColor = 'rgba(0,0,0,0.3)';
                label.padding = 2;
                const offset = isSimple ? this.config.nodeSize.simple + 2 : this.config.nodeSize.composite + 2;
                label.position.copy(node.object.position);
                label.position.y += offset;
                this.scene.add(label);
                node.labelObject = label;
            }
        });
        
        // Update link labels
        this.graphData.links.forEach(link => {
            if (link.labelObject) {
                link.labelObject.visible = this.config.showLabels;
            } else if (this.config.showLabels && link.label) {
                // Get source and target nodes using stored IDs
                const sourceId = link.sourceId || (typeof link.source === 'object' ? link.source.id : link.source);
                const targetId = link.targetId || (typeof link.target === 'object' ? link.target.id : link.target);
                
                const sourceNode = this.graphObjects.nodes.get(sourceId);
                const targetNode = this.graphObjects.nodes.get(targetId);
                
                if (sourceNode && targetNode) {
                    // Calculate midpoint between nodes
                    const midPoint = new THREE.Vector3(
                        (sourceNode.x + targetNode.x) / 2,
                        (sourceNode.y + targetNode.y) / 2,
                        0  // Keep z at 0 for 2D
                    );
                    
                    const label = new SpriteText(link.label);
                    label.color = '#ffffff';
                    label.backgroundColor = 'rgba(0,0,0,0.2)';
                    label.padding = 1;
                    label.position.copy(midPoint);
                    this.scene.add(label);
                    link.labelObject = label;
                }
            }
        });
    }
    
    /**
     * Search for nodes with matching ID or label
     */
    searchNodes(searchTerm) {
        if (!searchTerm) {
            this.resetHighlighting();
            return;
        }
        
        let foundAny = false;
        let firstFoundNode = null;
        
        // Reset all nodes to original colors
        this.resetHighlighting();
        
        // Highlight matching nodes
        this.graphData.nodes.forEach(node => {
            const nodeId = node.id.toLowerCase();
            const nodeLabel = (node.label || '').toLowerCase();
            const searchTermLower = searchTerm.toLowerCase();
            
            if (nodeId.includes(searchTermLower) || nodeLabel.includes(searchTermLower)) {
                node.object.material.color.setHex(this.config.colors.highlight);
                foundAny = true;
                
                // Store the first found node to focus on
                if (!firstFoundNode) {
                    firstFoundNode = node;
                }
            }
        });
        
        if (!foundAny) {
            console.log('No nodes found matching:', searchTerm);
        } else if (firstFoundNode && firstFoundNode.object) {
            // Focus camera on the first found node
            this.focusCameraOnNode(firstFoundNode.object);
            // Also select the node to show its connections
            this.selectNode(firstFoundNode.id);
        }
    }
    
    /**
     * Reset node highlighting to original colors
     */
    resetHighlighting() {
        this.graphData.nodes.forEach(node => {
            if (node.object) {
                const color = node.type === 'simple' 
                    ? this.config.colors.simpleNode 
                    : this.config.colors.compositeNode;
                node.object.material.color.setHex(color);
            }
        });
    }
    
    /**
     * Reset camera and controls to default view
     */
    resetView() {
        // Reset camera
        this.camera.position.set(0, 0, 500);
        this.controls.target.set(0, 0, 0);
        this.camera.zoom = this.config.zoomLevel.default;
        this.camera.updateProjectionMatrix();
        this.controls.update();
        
        // Reset search
        if (this.searchInput) {
            this.searchInput.value = '';
        }
        
        // Reset node highlighting
        this.resetHighlighting();
        
        // Clear selection
        this.clearSelection();
    }
    
    /**
     * Update node and link positions on simulation tick
     */
    onSimulationTick() {
		console.log('onSimulationTick');
        if (!this.config.physicsEnabled) return;
        
        // Update node positions
        this.graphData.nodes.forEach(node => {
            if (node.object) {
                // Update node position
                node.object.position.x = node.x;
                node.object.position.y = node.y;
                node.object.position.z = this.config.zPos.node;
                
                // Update label position if it exists
                if (node.labelObject) {
                    const isSimple = node.type === 'simple';
                    const offset = isSimple ? this.config.nodeSize.simple + 2 : this.config.nodeSize.composite + 2;
                    node.labelObject.position.x = node.x;
                    node.labelObject.position.y = node.y + offset;
                    node.labelObject.position.z = this.config.zPos.label;
                }
            }
        });
        
        // Update link positions
        this.graphData.links.forEach(link => {
            if (link.line) {
                this.updateLinkPosition(link);
            }
        });
    }
    
    /**
     * Update a link's position based on its connected nodes
     */
    updateLinkPosition(link) {
        // Get source and target nodes
        const sourceId = link.sourceId || (typeof link.source === 'object' ? link.source.id : link.source);
        const targetId = link.targetId || (typeof link.target === 'object' ? link.target.id : link.target);
        
        const sourceNode = this.graphObjects.nodes.get(sourceId);
        const targetNode = this.graphObjects.nodes.get(targetId);
        
        if (!sourceNode || !targetNode) return;
        
        // Create start and end points for the line
        const start = new THREE.Vector3(sourceNode.x, sourceNode.y, this.config.zPos.line);
        const end = new THREE.Vector3(targetNode.x, targetNode.y, this.config.zPos.line);
        
        // Update line geometry
        link.line.geometry.dispose();
        link.line.geometry = new THREE.BufferGeometry().setFromPoints([start, end]);
        
        // Recompute line distances for dashed lines
        if (link.line.material instanceof THREE.LineDashedMaterial) {
            link.line.computeLineDistances();
        }
        
        // Update label position if it exists
        if (link.labelObject) {
            const mid = new THREE.Vector3().addVectors(start, end).multiplyScalar(0.5);
            mid.z = this.config.zPos.label;
            link.labelObject.position.copy(mid);
        }
    }
    
    /**
     * Animation loop
     */
    animate() {
        requestAnimationFrame(() => this.animate());
        this.controls.update();
        this.renderer.render(this.scene, this.camera);
    }
    
    /**
     * Handle window resize
     */
    onWindowResize() {
        const width = this.container.clientWidth;
        const height = this.container.clientHeight;
        
        // Update orthographic camera
        this.camera.left = width / -2;
        this.camera.right = width / 2;
        this.camera.top = height / 2;
        this.camera.bottom = height / -2;
        this.camera.updateProjectionMatrix();
        
        // Update renderer
        this.renderer.setSize(width, height);
    }
    
    /**
     * Update node and link count statistics
     */
    updateStats() {
        if (this.nodeCountEl) {
            this.nodeCountEl.textContent = this.graphData.nodes.length;
        }
        if (this.linkCountEl) {
            this.linkCountEl.textContent = this.graphData.links.length;
        }
    }
    
    /**
     * Clear the current graph from the scene
     */
    clearGraph() {
        // Remove nodes
        this.graphData.nodes.forEach(node => {
            if (node.object) {
                this.scene.remove(node.object);
            }
            if (node.labelObject) {
                this.scene.remove(node.labelObject);
            }
        });
        
        // Remove links
        this.graphData.links.forEach(link => {
            if (link.line) {
                this.scene.remove(link.line);
            }
            if (link.labelObject) {
                this.scene.remove(link.labelObject);
            }
        });
        
        // Clear data
        this.graphData.nodes = [];
        this.graphData.links = [];
        this.graphObjects.nodes.clear();
        this.graphObjects.links.clear();
        
        // Clear selection state
        this.selectedNode = null;
        this.neighborNodes.clear();
        this.activeLinks.clear();
    }
    
    /**
     * Show an error message in the graph container
     */
    showError(message) {
        this.showLoading(false);
        
        // Create error message element if it doesn't exist
        if (!this.errorEl) {
            this.errorEl = document.createElement('div');
            this.errorEl.style.position = 'absolute';
            this.errorEl.style.top = '50%';
            this.errorEl.style.left = '50%';
            this.errorEl.style.transform = 'translate(-50%, -50%)';
            this.errorEl.style.background = 'rgba(0, 0, 0, 0.8)';
            this.errorEl.style.color = '#ff5555';
            this.errorEl.style.padding = '20px';
            this.errorEl.style.borderRadius = '8px';
            this.errorEl.style.maxWidth = '80%';
            this.errorEl.style.textAlign = 'center';
            this.container.appendChild(this.errorEl);
        }
        
        this.errorEl.textContent = message;
        this.errorEl.style.display = 'block';
    }
    
    /**
     * Handle mouse interactions (click, double-click, hover)
     */
    onMouseClick(event) {
        const intersectedNode = this.getIntersectedNode(event);
        
        if (intersectedNode) {
            // Call the node's callback function
            if (intersectedNode.callback) {
                intersectedNode.callback();
            }
        } else {
            // Deselect if clicking on empty space
            this.clearSelection();
        }
    }
    
    onDoubleClick(event) {
        const intersectedNode = this.getIntersectedNode(event);
        
        if (intersectedNode) {
            this.focusCameraOnNode(intersectedNode);
        }
    }
    
    onMouseMove(event) {
        const intersectedNode = this.getIntersectedNode(event);
        
        if (intersectedNode) {
            // Mouse is over a node
            if (this.hoveredNode !== intersectedNode) {
                // Clear previous hover
                if (this.hoveredNode) {
                    this.clearNodeHover(this.hoveredNode);
                }
                
                // Set new hover
                this.hoveredNode = intersectedNode;
                this.setNodeHover(this.hoveredNode);
                
                // Change cursor to pointer
                this.renderer.domElement.style.cursor = 'pointer';
            }
        } else if (this.hoveredNode) {
            // Mouse is no longer over any node
            this.clearNodeHover(this.hoveredNode);
            this.hoveredNode = null;
            
            // Reset cursor
            this.renderer.domElement.style.cursor = 'auto';
        }
    }
    
    /**
     * Get the node under the mouse pointer
     */
    getIntersectedNode(event) {
        // Calculate mouse position in normalized device coordinates
        const rect = this.renderer.domElement.getBoundingClientRect();
        this.mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
        this.mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;
        
        // Update the raycaster with the camera and mouse position
        this.raycaster.setFromCamera(this.mouse, this.camera);
        
        // Find intersected objects
        const nodeMeshes = this.graphData.nodes
            .filter(node => node.object)
            .map(node => node.object);
        
        const intersects = this.raycaster.intersectObjects(nodeMeshes);
        
        return intersects.length > 0 ? intersects[0].object : null;
    }
    
    /**
     * Apply hover effect to a node
     */
    setNodeHover(nodeMesh) {
        // Don't apply hover effect if this is the selected node
        const nodeId = nodeMesh.userData.id;
        if (nodeId === this.selectedNode) {
            return;
        }
        
        // Store original scale and color for restoration
        if (!nodeMesh.userData.originalScale) {
            nodeMesh.userData.originalScale = nodeMesh.scale.x;
            nodeMesh.userData.originalColor = nodeMesh.material.color.getHex();
        }
        
        // Apply hover effect
        nodeMesh.scale.set(1.2, 1.2, 1.2);
        nodeMesh.material.color.setHex(this.config.colors.hover);
    }
    
    /**
     * Remove hover effect from a node
     */
    clearNodeHover(nodeMesh) {
        // Don't clear hover if this is the selected node
        const nodeId = nodeMesh.userData.id;
        if (nodeId === this.selectedNode) {
            return;
        }
        
        // Restore original scale
        if (nodeMesh.userData.originalScale) {
            nodeMesh.scale.set(
                nodeMesh.userData.originalScale,
                nodeMesh.userData.originalScale,
                nodeMesh.userData.originalScale
            );
        } else {
            nodeMesh.scale.set(1.0, 1.0, 1.0);
        }
        
        // Restore original color
        if (nodeMesh.userData.originalColor !== undefined) {
            nodeMesh.material.color.setHex(nodeMesh.userData.originalColor);
        } else {
            // Fallback to default colors
            const isSimple = nodeMesh.userData.type === 'simple';
            const color = isSimple ? this.config.colors.simpleNode : this.config.colors.compositeNode;
            nodeMesh.material.color.setHex(color);
        }
    }
    
    /**
     * Focus camera on a specific node
     */
    focusCameraOnNode(nodeObject) {
        if (!nodeObject) return;
        
        const targetPosition = nodeObject.position.clone();
        const duration = 500; // milliseconds
        const startTime = Date.now();
        
        // Save starting values
        const startPosition = this.camera.position.clone();
        const startTarget = this.controls.target.clone();
        const startZoom = this.camera.zoom;
        const targetZoom = this.config.zoomLevel.focused;
        
        // Animation function
        const animateCamera = () => {
            const elapsed = Date.now() - startTime;
            const progress = Math.min(elapsed / duration, 1);
            
            // Ease function - ease out cubic
            const easeProgress = 1 - Math.pow(1 - progress, 3);
            
            // Update camera position and controls target
            this.camera.position.x = startPosition.x + (targetPosition.x - startPosition.x) * easeProgress;
            this.camera.position.y = startPosition.y + (targetPosition.y - startPosition.y) * easeProgress;
            
            this.controls.target.x = startTarget.x + (targetPosition.x - startTarget.x) * easeProgress;
            this.controls.target.y = startTarget.y + (targetPosition.y - startTarget.y) * easeProgress;
            this.controls.target.z = startTarget.z + (targetPosition.z - startTarget.z) * easeProgress;
            
            // Update zoom level
            this.camera.zoom = startZoom + (targetZoom - startZoom) * easeProgress;
            this.camera.updateProjectionMatrix();
            
            this.controls.update();
            
            if (progress < 1) {
                requestAnimationFrame(animateCamera);
            }
        };
        
        animateCamera();
    }
    
    /**
     * Select a node and highlight its neighbors
     */
    selectNode(nodeId) {
        // Clear previous selection
        this.clearSelection();
        
        // Get the node
        this.selectedNode = nodeId;
        const selectedNode = this.graphObjects.nodes.get(nodeId);
        if (!selectedNode) return;
        
        // Pause physics during selection
        // const wasPhysicsEnabled = this.config.physicsEnabled;
        // if (wasPhysicsEnabled) {
        //     this.config.physicsEnabled = false;
        //     this.simulation.alpha(0);
        // }
        // this.wasPhysicsEnabled = wasPhysicsEnabled;
        
        // Highlight selected node
        selectedNode.object.material.color.setHex(this.config.colors.selectedNode);
        selectedNode.object.scale.set(1.2, 1.2, 1.2);
        
        // Find and highlight connected nodes and links
        let connectionCount = this.highlightConnections(nodeId);
        
        // Show node info
        this.showNodeInfo(selectedNode, connectionCount);
    }
    
    /**
     * Highlight nodes and links connected to the selected node
     */
    highlightConnections(nodeId) {
        let connectionCount = 0;
        
        // Process all links
        this.graphData.links.forEach(link => {
            const sourceId = typeof link.source === 'object' ? link.source.id : link.source;
            const targetId = typeof link.target === 'object' ? link.target.id : link.target;
            
            // If connected to the selected node
            if (sourceId === nodeId || targetId === nodeId) {
                connectionCount++;
                
                // Highlight the neighbor node
                const neighborId = sourceId === nodeId ? targetId : sourceId;
                const neighborNode = this.graphObjects.nodes.get(neighborId);
                
                if (neighborNode) {
                    // Highlight neighbor
                    neighborNode.object.material.color.setHex(this.config.colors.neighborNode);
                    neighborNode.object.scale.set(1.1, 1.1, 1.1);
                    this.neighborNodes.add(neighborId);
                    
                    // Highlight the connecting link
                    if (link.line && link.line.material) {
                        const oldMaterial = link.line.material;
                        const newMaterial = new THREE.LineBasicMaterial({
                            color: this.config.colors.activeLink,
                            linewidth: 1.5,
                            opacity: 1.0,
                            transparent: false
                        });
                        
                        link.line.material = newMaterial;
                        oldMaterial.dispose();
                        this.activeLinks.add(`${sourceId}-${targetId}`);
                    }
                }
            }
        });
        
        return connectionCount;
    }
    
    /**
     * Show information about the selected node
     */
    showNodeInfo(node, connectionCount) {
        // Create or update the node info panel
        if (!this.nodeInfoPanel) {
            this.nodeInfoPanel = document.createElement('div');
            this.nodeInfoPanel.className = 'node-info-panel';
            this.container.appendChild(this.nodeInfoPanel);
        }
        
        // Set the content with node information
        this.nodeInfoPanel.innerHTML = `
            <h3>Selected Node</h3>
            <p><strong>ID:</strong> ${node.id}</p>
            <p><strong>Label:</strong> ${node.label}</p>
            <p><strong>Type:</strong> ${node.type}</p>
            <p><strong>Connections:</strong> ${connectionCount || this.neighborNodes.size}</p>
        `;
        
        this.nodeInfoPanel.style.display = 'block';
    }
    
    /**
     * Clear current node selection
     */
    clearSelection() {
        // Reset selected node
        if (this.selectedNode) {
            const node = this.graphObjects.nodes.get(this.selectedNode);
            if (node) {
                const color = node.type === 'simple' 
                    ? this.config.colors.simpleNode 
                    : this.config.colors.compositeNode;
                node.object.material.color.setHex(color);
                node.object.scale.set(1.0, 1.0, 1.0);
            }
        }
        
        // Reset neighbor nodes
        this.neighborNodes.forEach(neighborId => {
            const node = this.graphObjects.nodes.get(neighborId);
            if (node) {
                const color = node.type === 'simple' 
                    ? this.config.colors.simpleNode 
                    : this.config.colors.compositeNode;
                node.object.material.color.setHex(color);
                node.object.scale.set(1.0, 1.0, 1.0);
            }
        });
        
        // Reset links
        this.activeLinks.forEach(linkKey => {
            const link = this.graphObjects.links.get(linkKey);
            if (link && link.line) {
                const oldMaterial = link.line.material;
                const newMaterial = new THREE.LineDashedMaterial({
                    color: this.config.colors.link,
                    dashSize: 2.5,
                    gapSize: 1.5,
                    transparent: true,
                    opacity: 0.6,
                    linewidth: 1
                });
                link.line.material = newMaterial;
                link.line.computeLineDistances();
                oldMaterial.dispose();
            }
        });
        
        // Restore physics if needed
        if (this.wasPhysicsEnabled) {
            this.config.physicsEnabled = true;
            this.simulation.alpha(0.1).restart();
            this.wasPhysicsEnabled = false;
        }
        
        // Clear state
        this.selectedNode = null;
        this.neighborNodes.clear();
        this.activeLinks.clear();
        
        // Hide node info
        this.hideNodeInfo();
    }
    
    /**
     * Hide the node info panel
     */
    hideNodeInfo() {
        if (this.nodeInfoPanel) {
            this.nodeInfoPanel.style.display = 'none';
        }
    }
    
    /**
     * Create autocomplete dropdown UI
     */
    createAutocompleteUI() {
        if (!this.autocompleteList && this.searchInput) {
            this.autocompleteList = document.createElement('div');
            this.autocompleteList.className = 'autocomplete-list';
            
            // Insert into search container or after search input
            const searchContainer = document.querySelector('.search-container');
            if (searchContainer) {
                searchContainer.appendChild(this.autocompleteList);
            } else if (this.searchInput.parentNode) {
                this.searchInput.parentNode.insertBefore(
                    this.autocompleteList, 
                    this.searchInput.nextSibling
                );
            }
        }
    }
    
    /**
     * Update autocomplete suggestions based on search input
     */
    updateAutocompleteSuggestions(query) {
        // Handle empty query
        if (!query || query.length < 1) {
            this.hideAutocomplete();
            return;
        }
        
        // Reset suggestions
        this.autocompleteSuggestions = [];
        this.autocompleteSelectedIndex = -1;
        
        // Find matching nodes
        const lowercaseQuery = query.toLowerCase();
        const matches = [];
        
        this.graphData.nodes.forEach(node => {
            const nodeId = node.id.toLowerCase();
            const nodeLabel = (node.label || '').toLowerCase();
            
            if (nodeId.includes(lowercaseQuery) || nodeLabel.includes(lowercaseQuery)) {
                matches.push({
                    text: node.label || node.id,
                    id: node.id,
                    node: node
                });
            }
        });
        
        // Sort and limit results
        this.autocompleteSuggestions = matches
            .sort((a, b) => a.text.localeCompare(b.text))
            .slice(0, 10);
        
        // Render suggestions if we have any
        if (this.autocompleteSuggestions.length > 0) {
            this.renderAutocompleteSuggestions();
        } else {
            this.hideAutocomplete();
        }
    }
    
    /**
     * Render autocomplete suggestions in the dropdown
     */
    renderAutocompleteSuggestions() {
        if (!this.autocompleteList) return;
        
        // Clear existing items
        this.autocompleteList.innerHTML = '';
        
        // Create suggestion elements
        this.autocompleteSuggestions.forEach((suggestion, index) => {
            const item = document.createElement('div');
            item.className = 'autocomplete-item';
            if (index === this.autocompleteSelectedIndex) {
                item.classList.add('selected');
            }
            
            item.textContent = suggestion.text;
            item.dataset.index = index;
            
            // Hover handler
            item.onmouseover = () => {
                this.updateAutocompleteSelection(index);
            };
            
            // Click handler
            item.onclick = (e) => {
                e.preventDefault();
                e.stopPropagation();
                this.selectSuggestion(suggestion);
            };
            
            this.autocompleteList.appendChild(item);
        });
        
        // Show the dropdown
        this.autocompleteList.style.display = 'block';
    }
    
    /**
     * Update the selected item in the autocomplete dropdown
     */
    updateAutocompleteSelection(index) {
        this.autocompleteSelectedIndex = index;
        
        if (!this.autocompleteList) return;
        
        // Update selected class
        Array.from(this.autocompleteList.children).forEach((child, idx) => {
            child.classList.toggle('selected', idx === index);
        });
    }
    
    /**
     * Handle selection of an autocomplete suggestion
     */
    selectSuggestion(suggestion) {
        if (!suggestion) return;
        
        // Update search input
        if (this.searchInput) {
            this.searchInput.value = suggestion.text;
            this.previousSearchValue = suggestion.text;
        }
        
        // Find the node
        const nodeId = suggestion.id;
        const node = this.graphObjects.nodes.get(nodeId);
        
        // Focus on the node if found
        if (node && node.object) {
            this.focusCameraOnNode(node.object);
            this.selectNode(nodeId);
        } else if (suggestion.node && suggestion.node.object) {
            // Fallback to stored node reference
            this.focusCameraOnNode(suggestion.node.object);
            this.selectNode(suggestion.id);
        }
        
        // Hide dropdown
        this.hideAutocomplete();
    }
    
    /**
     * Show or hide the autocomplete dropdown
     */
    showAutocomplete() {
        if (this.autocompleteList) {
            this.autocompleteList.style.display = 'block';
        }
    }
    
    hideAutocomplete() {
        if (this.autocompleteList) {
            this.autocompleteList.style.display = 'none';
        }
    }
    
    /**
     * Handle keyboard navigation in autocomplete
     */
    handleAutocompleteKeydown(event) {
        if (!this.autocompleteList || this.autocompleteSuggestions.length === 0) return;
        
        const { key } = event;
        
        if (key === 'ArrowDown') {
            event.preventDefault();
            const newIndex = Math.min(
                this.autocompleteSelectedIndex + 1,
                this.autocompleteSuggestions.length - 1
            );
            this.updateAutocompleteSelection(newIndex);
        } 
        else if (key === 'ArrowUp') {
            event.preventDefault();
            const newIndex = Math.max(this.autocompleteSelectedIndex - 1, 0);
            this.updateAutocompleteSelection(newIndex);
        } 
        else if (key === 'Enter' && this.autocompleteSelectedIndex >= 0) {
            event.preventDefault();
            this.selectSuggestion(this.autocompleteSuggestions[this.autocompleteSelectedIndex]);
        } 
        else if (key === 'Escape') {
            this.hideAutocomplete();
        }
    }
}

// Initialize the renderer when the page loads
document.addEventListener('DOMContentLoaded', () => {
    new CacheGraphRenderer('graph-container');
}); 