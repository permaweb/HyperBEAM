/**
 * HyperBEAM Cache Graph Renderer - Modular Version
 * A 2D force-directed graph visualization for the HyperBEAM cache system
 */

/**
 * ThemeManager - Handles configuration and visual styling
 */
class ThemeManager {
    constructor() {
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
            defaultDistance: 150,
            highConnectionThreshold: 5,
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
    }
    
    /**
     * Get the color for a node based on its type and state
     * @param {string} nodeType - The type of node ('simple' or 'composite')
     * @param {string} state - The state of the node ('default', 'selected', 'neighbor', 'hover')
     * @returns {number} The color as a hex number
     */
    getNodeColor(nodeType, state = 'default') {
        switch(state) {
            case 'selected':
                return this.config.colors.selectedNode;
            case 'neighbor':
                return this.config.colors.neighborNode;
            case 'hover':
                return this.config.colors.hover;
            default:
                return nodeType === 'simple' ? 
                    this.config.colors.simpleNode : 
                    this.config.colors.compositeNode;
        }
    }
    
    /**
     * Get the color for a link based on its state
     * @param {string} state - The state of the link ('default' or 'active')
     * @returns {number} The color as a hex number
     */
    getLinkColor(state = 'default') {
        return state === 'active' ? 
            this.config.colors.activeLink : 
            this.config.colors.link;
    }
    
    /**
     * Get the size for a node based on its type
     * @param {string} nodeType - The type of node ('simple' or 'composite')
     * @returns {number} The node size
     */
    getNodeSize(nodeType) {
        return nodeType === 'simple' ? 
            this.config.nodeSize.simple : 
            this.config.nodeSize.composite;
    }
    
    /**
     * Toggle label visibility
     * @returns {boolean} The new label visibility state
     */
    toggleLabels() {
        this.config.showLabels = !this.config.showLabels;
        return this.config.showLabels;
    }
    
    /**
     * Toggle physics simulation
     * @returns {boolean} The new physics enabled state
     */
    togglePhysics() {
        this.config.physicsEnabled = !this.config.physicsEnabled;
        return this.config.physicsEnabled;
    }
}

/**
 * SceneManager - Handles Three.js scene, camera, and rendering
 */
class SceneManager {
    constructor(container, themeManager) {
        this.container = container;
        this.themeManager = themeManager;
        
        // Three.js components
        this.scene = null;
        this.camera = null;
        this.renderer = null;
        this.controls = null;
        this.raycaster = new THREE.Raycaster();
        this.mouse = new THREE.Vector2();
        
        // Initialize the scene
        this.initScene();
    }
    
    /**
     * Initialize the Three.js scene and renderer
     */
    initScene() {
        const width = this.container.clientWidth;
        const height = this.container.clientHeight;
        
        // Create scene with background color
        this.scene = new THREE.Scene();
        this.scene.background = new THREE.Color(this.themeManager.config.colors.background);
        
        // Create perspective camera with large clipping plane to prevent culling
        const aspectRatio = width / height;
        this.camera = new THREE.PerspectiveCamera(
            40,  // Narrower field of view for less distortion
            aspectRatio,
            0.1,
            10000  // Increased far clipping plane
        );
        this.camera.position.z = 1000;
        
        // Create renderer
        this.renderer = new THREE.WebGLRenderer({ 
            antialias: false,
            alpha: true
        });
        this.renderer.setSize(width, height);
        this.renderer.setClearColor(this.themeManager.config.colors.background, 1);
        this.renderer.sortObjects = true; // Enable sorting for proper z-ordering
        this.container.appendChild(this.renderer.domElement);
        
        // Add orbit controls limited to 2D movement with perspective camera
        this.controls = new THREE.OrbitControls(this.camera, this.renderer.domElement);
        this.controls.enableDamping = true;
        this.controls.dampingFactor = 0.1;
        this.controls.enableRotate = false; // Disable 3D rotation
        this.controls.screenSpacePanning = true;
        
        // Add lighting
        this.scene.add(new THREE.AmbientLight(0xffffff, 0.6));
        const directionalLight = new THREE.DirectionalLight(0xffffff, 0.8);
        directionalLight.position.set(0, 0, 200);
        this.scene.add(directionalLight);
        
        // Handle window resize
        window.addEventListener('resize', () => this.onWindowResize());
    }
    
    /**
     * Handle window resize events
     */
    onWindowResize() {
        const width = this.container.clientWidth;
        const height = this.container.clientHeight;
        
        // Update perspective camera aspect ratio
        this.camera.aspect = width / height;
        this.camera.updateProjectionMatrix();
        
        // Update renderer
        this.renderer.setSize(width, height);
    }
    
    /**
     * Reset the camera view
     */
    resetView() {
        // Reset camera position for perspective camera
        this.camera.position.set(0, 0, 1000);
        this.controls.target.set(0, 0, 0);
        this.camera.updateProjectionMatrix();
        this.controls.update();
    }
    
    /**
     * Focus camera on a specific position with smooth animation
     * @param {THREE.Vector3} position - The position to focus on
     */
    focusCamera(position) {
        const duration = 500; // milliseconds
        const startTime = Date.now();
        
        // Save starting values
        const startPosition = this.camera.position.clone();
        const startTarget = this.controls.target.clone();
        
        // Define a fixed Z-offset for viewing the target
        const zOffset = 600;
        
        // Animation function
        const animateCamera = () => {
            const elapsed = Date.now() - startTime;
            const progress = Math.min(elapsed / duration, 1);
            
            // Ease function - ease out cubic
            const easeProgress = 1 - Math.pow(1 - progress, 3);
            
            // Only update the target x and y position, keeping rotation consistent
            this.controls.target.x = startTarget.x + (position.x - startTarget.x) * easeProgress;
            this.controls.target.y = startTarget.y + (position.y - startTarget.y) * easeProgress;
            // Keep z at the same value to maintain default camera angle
            
            // Move camera x and y to match target
            this.camera.position.x = startPosition.x + (position.x - startPosition.x) * easeProgress;
            this.camera.position.y = startPosition.y + (position.y - startPosition.y) * easeProgress;
            
            // Adjust Z with fixed offset
            const targetZ = position.z + zOffset;
            this.camera.position.z = startPosition.z + (targetZ - startPosition.z) * easeProgress;
            
            this.camera.updateProjectionMatrix();
            this.controls.update();
            
            if (progress < 1) {
                requestAnimationFrame(animateCamera);
            }
        };
        
        animateCamera();
    }
    
    /**
     * Update the mouse position for raycasting
     * @param {MouseEvent} event - The mouse event
     */
    updateMousePosition(event) {
        const rect = this.renderer.domElement.getBoundingClientRect();
        this.mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
        this.mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;
    }
    
    /**
     * Get objects intersecting with the current mouse position
     * @returns {Array} Array of intersected objects
     */
    getIntersectedObjects() {
        this.raycaster.setFromCamera(this.mouse, this.camera);
        return this.raycaster.intersectObjects(this.scene.children, true);
    }
    
    /**
     * Update the scene (called in animation loop)
     */
    update() {
        // Update controls
        if (this.controls) {
            this.controls.update();
        }
        
        // Render the scene
        this.renderer.render(this.scene, this.camera);
    }
    
    /**
     * Add an object to the scene
     * @param {THREE.Object3D} object - The object to add
     */
    addToScene(object) {
        this.scene.add(object);
    }
    
    /**
     * Remove an object from the scene
     * @param {THREE.Object3D} object - The object to remove
     */
    removeFromScene(object) {
        this.scene.remove(object);
    }
}

/**
 * DataManager - Handles graph data loading and processing
 */
class DataManager {
    constructor() {
        // Graph data
        this.graphData = { nodes: [], links: [] };
        this.graphObjects = { nodes: new Map(), links: new Map() };
        
        // State tracking
        this.selectedNode = null;
        this.neighborNodes = new Set();
        this.activeLinks = new Set();
        this.hoveredNode = null;
    }
    
    /**
     * Load graph data from the server
     * @returns {Promise} Promise that resolves when data is loaded
     */
    loadData() {
        return new Promise((resolve, reject) => {
            fetch('/~hyperbuddy@1.0/graph-data')
                .then(response => {
                    if (!response.ok) {
                        throw new Error(`HTTP error ${response.status}`);
                    }
                    return response.json();
                })
                .then(data => {
                    // Clear existing data
                    this.clearData();
                    
                    // Validate data
                    if (!this.validateData(data)) {
                        reject(new Error('Invalid data format'));
                        return;
                    }
                    
                    this.graphData = data;
                    resolve(data);
                })
                .catch(error => {
                    console.error('Error loading graph data:', error);
                    reject(error);
                });
        });
    }
    
    /**
     * Validate the graph data structure
     * @param {Object} data - The data to validate
     * @returns {boolean} Whether the data is valid
     */
    validateData(data) {
        // Check if we have valid data
        if (!data || !data.nodes || !data.links || 
            !Array.isArray(data.nodes) || !Array.isArray(data.links)) {
            return false;
        }
        
        // Check if we have any nodes
        if (data.nodes.length === 0) {
            return false;
        }
        
        return true;
    }
    
    /**
     * Clear all graph data
     */
    clearData() {
        this.graphData = { nodes: [], links: [] };
        this.graphObjects.nodes.clear();
        this.graphObjects.links.clear();
        
        // Reset state
        this.selectedNode = null;
        this.hoveredNode = null;
        this.neighborNodes.clear();
        this.activeLinks.clear();
    }
    
    /**
     * Determine node type based on ID pattern
     * @param {string} nodeId - The node ID
     * @returns {string} The node type ('simple' or 'composite')
     */
    determineNodeType(nodeId) {
        const pathParts = nodeId.split('/').filter(p => p.length > 0);
        return (pathParts.length <= 1 && !nodeId.endsWith('/')) ? 'simple' : 'composite';
    }
    
    /**
     * Search for nodes matching a term
     * @param {string} searchTerm - The term to search for
     * @returns {Array} Array of matching node IDs
     */
    searchNodes(searchTerm) {
        if (!searchTerm) return [];
        
        const searchLower = searchTerm.toLowerCase();
        
        // Find matching nodes
        return this.graphData.nodes
            .filter(node => 
                (node.id && node.id.toLowerCase().includes(searchLower)) ||
                (node.label && node.label.toLowerCase().includes(searchLower))
            )
            .map(node => node.id);
    }
    
    /**
     * Get nodes connected to a starting node up to a specified depth
     * @param {string} startNodeId - The ID of the starting node
     * @param {number} maxDepth - Maximum depth/distance to traverse
     * @returns {Object} Object containing connected nodes and links
     */
    getConnectedSubgraph(startNodeId, maxDepth = 1) {
        const connectedNodes = new Map();
        const connectedLinks = new Set();
        const queue = [{id: startNodeId, depth: 0}];
        const visited = new Set([startNodeId]);
        
        // First make sure we have the start node
        const startNode = this.graphData.nodes.find(n => n.id === startNodeId);
        if (!startNode) return {nodes: [], links: []};
        
        connectedNodes.set(startNodeId, startNode);
        
        // BFS to find connected nodes up to maxDepth
        while (queue.length > 0) {
            const {id, depth} = queue.shift();
            
            if (depth >= maxDepth) continue;
            
            // Find all links connected to this node
            this.graphData.links.forEach(link => {
                const sourceId = typeof link.source === 'object' ? link.source.id : link.source;
                const targetId = typeof link.target === 'object' ? link.target.id : link.target;
                
                if (sourceId === id || targetId === id) {
                    const linkId = `${sourceId}-${targetId}`;
                    
                    // If we've already processed this link, skip it
                    if (connectedLinks.has(linkId)) return;
                    
                    connectedLinks.add(linkId);
                    
                    // Get the ID of the node on the other end of the link
                    const otherId = sourceId === id ? targetId : sourceId;
                    
                    // If we haven't visited this node yet, add it to the queue
                    if (!visited.has(otherId)) {
                        visited.add(otherId);
                        const otherNode = this.graphData.nodes.find(n => n.id === otherId);
                        if (otherNode) {
                            connectedNodes.set(otherId, otherNode);
                            queue.push({id: otherId, depth: depth + 1});
                        }
                    }
                }
            });
        }
        
        return {
            nodes: Array.from(connectedNodes.values()),
            links: this.graphData.links.filter(link => {
                const sourceId = typeof link.source === 'object' ? link.source.id : link.source;
                const targetId = typeof link.target === 'object' ? link.target.id : link.target;
                return connectedNodes.has(sourceId) && connectedNodes.has(targetId);
            })
        };
    }
    
    /**
     * Store a node object reference
     * @param {string} nodeId - The node ID
     * @param {Object} nodeData - The node data
     */
    storeNodeObject(nodeId, nodeData) {
        this.graphObjects.nodes.set(nodeId, nodeData);
    }
    
    /**
     * Store a link object reference
     * @param {string} linkId - The link ID (format: "sourceId-targetId")
     * @param {Object} linkData - The link data
     */
    storeLinkObject(linkId, linkData) {
        this.graphObjects.links.set(linkId, linkData);
    }
    
    /**
     * Get links connected to a node
     * @param {string} nodeId - The node ID
     * @returns {Array} Array of connected links
     */
    getConnectedLinks(nodeId) {
        return this.graphData.links.filter(link => {
            const sourceId = typeof link.source === 'object' ? link.source.id : link.source;
            const targetId = typeof link.target === 'object' ? link.target.id : link.target;
            return sourceId === nodeId || targetId === nodeId;
        });
    }
    
    /**
     * Track selected node
     * @param {string} nodeId - The selected node ID
     */
    setSelectedNode(nodeId) {
        this.selectedNode = nodeId;
        
        // Find and track connected nodes and links
        if (nodeId) {
            // Clear previous
            this.neighborNodes.clear();
            this.activeLinks.clear();
            
            // Find connected links
            this.graphData.links.forEach(link => {
                const sourceId = typeof link.source === 'object' ? link.source.id : link.source;
                const targetId = typeof link.target === 'object' ? link.target.id : link.target;
                
                if (sourceId === nodeId || targetId === nodeId) {
                    // This is a connected link
                    const otherNodeId = sourceId === nodeId ? targetId : sourceId;
                    this.neighborNodes.add(otherNodeId);
                    
                    // Track active link
                    const linkKey = `${sourceId}-${targetId}`;
                    this.activeLinks.add(linkKey);
                }
            });
        }
    }
    
    /**
     * Clear selected node
     */
    clearSelectedNode() {
        this.selectedNode = null;
        this.neighborNodes.clear();
        this.activeLinks.clear();
    }
    
    /**
     * Track hovered node
     * @param {string} nodeId - The hovered node ID
     */
    setHoveredNode(nodeId) {
        this.hoveredNode = nodeId;
    }
    
    /**
     * Clear hovered node
     */
    clearHoveredNode() {
        this.hoveredNode = null;
    }
}

/**
 * GraphObjectManager - Creates and manages visual objects for nodes and links
 */
class GraphObjectManager {
    constructor(sceneManager, dataManager, themeManager) {
        this.sceneManager = sceneManager;
        this.dataManager = dataManager;
        this.themeManager = themeManager;
    }
    
    /**
     * Create a visual node object
     * @param {Object} node - The node data
     * @returns {Object} The created node object with visual elements
     */
    createNodeObject(node) {
        // Determine node type if not set
        if (!node.type) {
            node.type = this.dataManager.determineNodeType(node.id);
        }
        
        // Get node type and size
        const isSimple = node.type === 'simple';
        const size = this.themeManager.getNodeSize(node.type);
        const color = this.themeManager.getNodeColor(node.type);
        
        // Create 3D geometry and material for better visual effect
        const geometry = new THREE.SphereGeometry(size, 16, 16);
        const material = new THREE.MeshPhongMaterial({ 
            color,
            shininess: 2,
            specular: 0x444444,
            depthWrite: true,
            depthTest: true
        });
        
        // Create mesh
        const mesh = new THREE.Mesh(geometry, material);
        mesh.position.set(node.x || 0, node.y || 0, this.themeManager.config.zPos.node);
        mesh.userData = { id: node.id, type: node.type, label: node.label };
        mesh.renderOrder = 10;
        
        // Add to scene
        this.sceneManager.addToScene(mesh);
        
        // Create label if enabled
        let labelObject = null;
        if (this.themeManager.config.showLabels) {
            labelObject = this.createLabel(node);
        }
        
        // Store reference in the node object
        node.object = mesh;
        node.labelObject = labelObject;
        
        // Store in dataManager
        this.dataManager.storeNodeObject(node.id, node);
        
        return node;
    }
    
    /**
     * Create a visual link object
     * @param {Object} link - The link data
     * @returns {Object} The created link object with visual elements
     */
    createLinkObject(link) {
        // Get node IDs
        const sourceId = typeof link.source === 'object' ? link.source.id : link.source;
        const targetId = typeof link.target === 'object' ? link.target.id : link.target;
        
        // Get node objects
        const sourceNode = this.dataManager.graphObjects.nodes.get(sourceId);
        const targetNode = this.dataManager.graphObjects.nodes.get(targetId);
        
        if (!sourceNode || !targetNode) {
            return null;
        }
        
        // Create line geometry
        const points = [
            new THREE.Vector3(sourceNode.x, sourceNode.y, this.themeManager.config.zPos.line),
            new THREE.Vector3(targetNode.x, targetNode.y, this.themeManager.config.zPos.line)
        ];
        
        // Create material and line
        const material = new THREE.LineDashedMaterial({
            color: this.themeManager.getLinkColor(),
            dashSize: 2.5,
            gapSize: 1.5,
            transparent: true,
            opacity: 0.6,
            linewidth: 1,
            depthWrite: false
        });
        
        const geometry = new THREE.BufferGeometry().setFromPoints(points);
        const line = new THREE.Line(geometry, material);
        
        // Important: Disable frustum culling to ensure lines are always visible
        line.frustumCulled = false;
        line.renderOrder = 0; // Ensure lines render before nodes
        
        this.sceneManager.addToScene(line);
        
        // Store connection info
        link.sourceId = sourceId;
        link.targetId = targetId;
        link.line = line;
        
        // Create label if needed
        let labelObject = null;
        if (this.themeManager.config.showLabels && link.label) {
            labelObject = this.createLinkLabel(link, sourceNode, targetNode);
        }
        
        link.labelObject = labelObject;
        
        // Store reference
        const linkId = `${sourceId}-${targetId}`;
        this.dataManager.storeLinkObject(linkId, link);
        
        return link;
    }
    
    /**
     * Truncate text and add ellipsis in the middle
     * @param {string} text - The text to truncate
     * @returns {string} Truncated text with ellipsis
     */
    truncateWithEllipsis(text) {
        // Show only if longer than 15 characters (6 + 3 + 6)
        if (!text || text.length <= 15) {
            return text;
        }
        
        // Take exactly 6 chars from start and 6 from end
        return text.substring(0, 6) + '...' + text.substring(text.length - 6);
    }
    
    /**
     * Create a label for a node
     * @param {Object} node - The node data
     * @returns {Object} The created label object
     */
    createLabel(node) {
        // Get display text and truncate it if needed
        const displayText = this.truncateWithEllipsis(node.label || node.id);
        const label = new SpriteText(displayText);
        
        // Improved text rendering settings
        label.fontFace = 'Arial, Helvetica, sans-serif';
        label.fontSize = 32;
        label.fontWeight = '600';
        label.strokeWidth = 0; // No stroke for sharper text
        label.color = '#000000';
        label.backgroundColor = 'rgba(255,255,255,0.95)';
        label.padding = 3;
        label.textHeight = 5; // Increased for better resolution with larger text
        label.borderWidth = 0; // No border for sharper edges
        
        // Position above node with pixel-perfect positioning
		const offset_val = 100;	
        const isSimple = node.type === 'simple';
        const offset = isSimple ? 
            this.themeManager.config.nodeSize.simple + offset_val : 
            this.themeManager.config.nodeSize.composite + offset_val;
        
        // Round to whole pixels to avoid subpixel rendering
        const x = Math.round(node.x || 0);
        const y = Math.round((node.y || 0) + offset);
        const z = this.themeManager.config.zPos.label;
        
        label.position.set(x, y, z);
        label.renderOrder = 20;
        
        this.sceneManager.addToScene(label);
        
        return label;
    }
    
    /**
     * Create a label for a link
     * @param {Object} link - The link data
     * @param {Object} sourceNode - The source node
     * @param {Object} targetNode - The target node
     * @returns {Object} The created label object
     */
    createLinkLabel(link, sourceNode, targetNode) {
        const midPoint = new THREE.Vector3(
            (sourceNode.x + targetNode.x) / 2,
            (sourceNode.y + targetNode.y) / 2,
            this.themeManager.config.zPos.label
        );

		let label_text = link.label;
		if (link.label.length == 43) {
			label_text = this.truncateWithEllipsis(link.label);
		}

        const label = new SpriteText(label_text);
        
        // Improved text rendering settings
        label.fontFace = 'Arial, Helvetica, sans-serif';
        label.fontSize = 32;
        label.fontWeight = '600';
        label.strokeWidth = 0; // No stroke for sharper text
        label.color = '#000000';
        label.backgroundColor = 'rgba(255,255,255,0.95)';
        label.padding = 3;
        label.textHeight = 4; // Better resolution for link labels
        label.borderWidth = 0; // No border for sharper edges
        
        // Round to whole pixels to avoid subpixel rendering
        midPoint.x = Math.round(midPoint.x);
        midPoint.y = Math.round(midPoint.y);
        
        label.position.copy(midPoint);
        label.renderOrder = 20;
        
        // Hide link labels by default - only show when node is selected
        label.visible = false;
        
        this.sceneManager.addToScene(label);
        
        return label;
    }
    
    /**
     * Update the position of a node object
     * @param {Object} node - The node object to update
     */
    updateNodePosition(node) {
        if (node.object) {
            // Update the mesh position
            node.object.position.x = node.x || 0;
            node.object.position.y = node.y || 0;
            
            // Update label position if it exists
            if (node.labelObject) {
                const offset_val = 6; // Same value as in createLabel
                const isSimple = node.type === 'simple';
                const offset = isSimple ? 
                    this.themeManager.config.nodeSize.simple + offset_val : 
                    this.themeManager.config.nodeSize.composite + offset_val;
                    
                node.labelObject.position.x = node.x || 0;
                node.labelObject.position.y = (node.y || 0) + offset;
            }
        }
    }
    
    /**
     * Update the position of a link object
     * @param {Object} link - The link object to update
     */
    updateLinkPosition(link) {
        if (!link.line) return;
        
        const sourceId = link.sourceId || (typeof link.source === 'object' ? link.source.id : link.source);
        const targetId = link.targetId || (typeof link.target === 'object' ? link.target.id : link.target);
        
        const sourceNode = this.dataManager.graphObjects.nodes.get(sourceId);
        const targetNode = this.dataManager.graphObjects.nodes.get(targetId);
        
        if (sourceNode && targetNode) {
            // Update the link line geometry
            const points = [
                new THREE.Vector3(sourceNode.x || 0, sourceNode.y || 0, this.themeManager.config.zPos.line),
                new THREE.Vector3(targetNode.x || 0, targetNode.y || 0, this.themeManager.config.zPos.line)
            ];
            
            // Update the line geometry
            link.line.geometry.setFromPoints(points);
            
            // Ensure frustum culling remains disabled after updates
            link.line.frustumCulled = false;
            
            // Update the link label position if it exists
            if (link.labelObject) {
                const midPoint = new THREE.Vector3(
                    (sourceNode.x + targetNode.x) / 2,
                    (sourceNode.y + targetNode.y) / 2,
                    this.themeManager.config.zPos.label
                );
                link.labelObject.position.copy(midPoint);
            }
        }
    }
    
    /**
     * Update node colors based on selection and hover state
     * @param {string} nodeId - The ID of the node to update
     */
    updateNodeColors(nodeId) {
        const node = this.dataManager.graphObjects.nodes.get(nodeId);
        if (!node || !node.object) return;
        
        let state = 'default';
        
        // Determine state based on selection and hover
        if (nodeId === this.dataManager.selectedNode) {
            state = 'selected';
        } else if (this.dataManager.neighborNodes.has(nodeId)) {
            state = 'neighbor';
        } else if (nodeId === this.dataManager.hoveredNode) {
            state = 'hover';
        }
        
        // Set color based on state
        node.object.material.color.set(this.themeManager.getNodeColor(node.type, state));
    }
    
    /**
     * Update link colors based on selection state
     * @param {string} linkId - The ID of the link to update (format: "sourceId-targetId")
     */
    updateLinkColors(linkId) {
        const link = this.dataManager.graphObjects.links.get(linkId);
        if (!link || !link.line) return;
        
        // Determine if this is an active link
        const isActive = this.dataManager.activeLinks.has(linkId);
        
        // Set color and opacity based on active state
        link.line.material.color.set(this.themeManager.getLinkColor(isActive ? 'active' : 'default'));
        link.line.material.opacity = isActive ? 1.0 : 0.6;
    }
    
    /**
     * Remove a node and its visual elements from the scene
     * @param {string} nodeId - The ID of the node to remove
     */
    removeNode(nodeId) {
        const node = this.dataManager.graphObjects.nodes.get(nodeId);
        if (!node) return;
        
        // Remove mesh from scene
        if (node.object) {
            this.sceneManager.removeFromScene(node.object);
        }
        
        // Remove label from scene
        if (node.labelObject) {
            this.sceneManager.removeFromScene(node.labelObject);
        }
        
        // Remove from data manager
        this.dataManager.graphObjects.nodes.delete(nodeId);
    }
    
    /**
     * Remove a link and its visual elements from the scene
     * @param {string} linkId - The ID of the link to remove
     */
    removeLink(linkId) {
        const link = this.dataManager.graphObjects.links.get(linkId);
        if (!link) return;
        
        // Remove line from scene
        if (link.line) {
            this.sceneManager.removeFromScene(link.line);
        }
        
        // Remove label from scene
        if (link.labelObject) {
            this.sceneManager.removeFromScene(link.labelObject);
        }
        
        // Remove from data manager
        this.dataManager.graphObjects.links.delete(linkId);
    }
    
    /**
     * Clear all visible nodes and links from the scene
     */
    clearVisibleObjects() {
        // Remove all objects from the scene
        this.dataManager.graphObjects.nodes.forEach((node, id) => {
            if (node.object) this.sceneManager.removeFromScene(node.object);
            if (node.labelObject) this.sceneManager.removeFromScene(node.labelObject);
        });
        
        this.dataManager.graphObjects.links.forEach((link, id) => {
            if (link.line) this.sceneManager.removeFromScene(link.line);
            if (link.labelObject) this.sceneManager.removeFromScene(link.labelObject);
        });
        
        // Clear references in data manager
        this.dataManager.graphObjects.nodes.clear();
        this.dataManager.graphObjects.links.clear();
    }
    
    /**
     * Toggle visibility of all labels
     * @returns {boolean} The new label visibility state
     */
    toggleLabels() {
        const showLabels = this.themeManager.toggleLabels();
        
        // Update node labels
        this.dataManager.graphObjects.nodes.forEach((node, id) => {
            if (node.labelObject) {
                node.labelObject.visible = showLabels;
            } else if (showLabels) {
                // Create label if it doesn't exist yet
                node.labelObject = this.createLabel(node);
            }
        });
        
        // Update link labels - only show for active links connected to selected node
        this.dataManager.graphObjects.links.forEach((link, id) => {
            if (link.labelObject) {
                // Only show if labels are enabled AND this is an active link
                const isActive = this.dataManager.activeLinks.has(id);
                link.labelObject.visible = showLabels && isActive;
            } else if (showLabels && link.label) {
                // Create label if it doesn't exist yet
                const sourceNode = this.dataManager.graphObjects.nodes.get(link.sourceId);
                const targetNode = this.dataManager.graphObjects.nodes.get(link.targetId);
                
                if (sourceNode && targetNode) {
                    link.labelObject = this.createLinkLabel(link, sourceNode, targetNode);
                    // Only show if this is an active link
                    link.labelObject.visible = this.dataManager.activeLinks.has(id);
                }
            }
        });
        
        return showLabels;
    }
}

/**
 * SimulationManager - Manages the D3 force-directed simulation
 */
class SimulationManager {
    constructor(dataManager, graphObjectManager, themeManager) {
        this.dataManager = dataManager;
        this.graphObjectManager = graphObjectManager;
        this.themeManager = themeManager;
        
        this.simulation = null;
        this.isRunning = false;
        
        // Initialize the simulation
        this.initSimulation();
    }
    
    /**
     * Initialize D3 force simulation
     */
    initSimulation() {
        // Calculate connection counts for better distribution
        const connectionCounts = this.calculateConnectionCounts();
        
        // Link distance based on connection count
        const linkDistance = link => {
            const sourceId = typeof link.source === 'object' ? link.source.id : link.source;
            const targetId = typeof link.target === 'object' ? link.target.id : link.target;
            
            const sourceConnections = connectionCounts.get(sourceId) || 0;
            const targetConnections = connectionCounts.get(targetId) || 0;
            
            // Scale distance based on connection count
            const baseDistance = this.themeManager.config.defaultDistance;
            const connectionFactor = Math.max(sourceConnections, targetConnections);
            
            return baseDistance * (1 + Math.log(1 + connectionFactor * 0.2));
        };
        
        // Collision radius based on connection count
        const collisionRadius = node => {
            const connections = connectionCounts.get(node.id) || 0;
            const baseRadius = 15;
            
            // Increase collision radius for highly connected nodes
            if (connections > this.themeManager.config.highConnectionThreshold) {
                return baseRadius * (1 + Math.log(connections) * 0.1);
            }
            return baseRadius;
        };
        
        // For monitoring simulation progress
        this.tickCounter = 0;
        this.lastLogTime = 0;
        
        // Create the simulation with all forces
        this.simulation = d3.forceSimulation()
            .force('link', d3.forceLink().id(d => d.id).distance(linkDistance))
            .force('charge', d3.forceManyBody().strength(-100))
            .force('center', d3.forceCenter(0, 0))
            .force('collision', d3.forceCollide().radius(collisionRadius))
            .force('x', d3.forceX().strength(0.001))
            .force('y', d3.forceY().strength(0.05))
            .on('tick', () => {
                this.tickCounter++;
                this.onSimulationTick();
                this.monitorSimulationProgress();
            })
            .on('end', () => {
                console.log('Simulation reached equilibrium!');
                console.log('Final alpha:', this.simulation.alpha());
                console.log('Alpha min:', this.simulation.alphaMin());
                console.log('Alpha decay:', this.simulation.alphaDecay());
                console.log('Node count:', this.simulation.nodes().length);
                console.log('Total ticks:', this.tickCounter);
                this.isRunning = false;
            });
        
        // Adjust alpha settings for longer simulation time
        // Reduce decay rate (default is ~0.0228 which is 1% cooling per tick)
        this.simulation.alphaDecay(0.01);  // Slower decay (about 0.5% cooling per tick)
        
        // Lower minimum alpha threshold (default is 0.001)
        this.simulation.alphaMin(0.0005);  // Lower threshold for stopping
        
        // Reduce velocity decay for more momentum (default is 0.4)
        this.simulation.velocityDecay(0.2);
    }
    
    /**
     * Calculate connection counts for each node
     * @returns {Map} Map of node IDs to connection counts
     */
    calculateConnectionCounts() {
        const connectionCounts = new Map();
        
        // Count connections for each node
        this.dataManager.graphData.links.forEach(link => {
            const sourceId = typeof link.source === 'object' ? link.source.id : link.source;
            const targetId = typeof link.target === 'object' ? link.target.id : link.target;
            
            connectionCounts.set(sourceId, (connectionCounts.get(sourceId) || 0) + 1);
            connectionCounts.set(targetId, (connectionCounts.get(targetId) || 0) + 1);
        });
        
        return connectionCounts;
    }
    
    /**
     * Update the simulation with current nodes and links
     * @param {boolean} restart - Whether to restart the simulation
     */
    updateSimulation(restart = true) {
        if (!this.simulation) return;
        
        // Get visible nodes and links from data manager
        const visibleNodes = Array.from(this.dataManager.graphObjects.nodes.values());
        const visibleLinks = Array.from(this.dataManager.graphObjects.links.values());
        
        console.log(`Updating simulation with ${visibleNodes.length} nodes and ${visibleLinks.length} links`);
        
        // Update nodes and links in the simulation
        this.simulation.nodes(visibleNodes);
        this.simulation.force('link').links(visibleLinks);
        
        // Restart simulation if needed
        if (restart && this.themeManager.config.physicsEnabled) {
            // Reset tick counter and timing
            this.tickCounter = 0;
            this.lastLogTime = Date.now();
            
            // Set a higher alpha to ensure thorough exploration of layout space
            const startingAlpha = 1.0;
            console.log(`Starting simulation with alpha=${startingAlpha}, alphaMin=${this.simulation.alphaMin()}, alphaDecay=${this.simulation.alphaDecay()}`);
            this.simulation.alpha(startingAlpha).restart();
            this.isRunning = true;
        } else {
            console.log("Simulation not restarted (either restart=false or physics is disabled)");
            this.simulation.alpha(0);
            this.isRunning = false;
        }
    }
    
    /**
     * Toggle physics simulation on/off
     * @returns {boolean} The new physics state
     */
    togglePhysics() {
        const physicsEnabled = this.themeManager.togglePhysics();
        
        console.log(`Physics simulation ${physicsEnabled ? 'enabled' : 'disabled'}`);
        
        // Use updateSimulation to properly handle the physics state
        this.updateSimulation(physicsEnabled);
        
        return physicsEnabled;
    }
    
    /**
     * Handle force simulation tick events
     * Updates the positions of nodes and links in the visualization
     */
    onSimulationTick() {
        this.updatePositions();
    }
    
    /**
     * Update positions of all nodes and links
     */
    updatePositions() {
        // Update the position of nodes in the scene
        this.dataManager.graphObjects.nodes.forEach((node) => {
            this.graphObjectManager.updateNodePosition(node);
        });
        
        // Update the position of links in the scene
        this.dataManager.graphObjects.links.forEach((link) => {
            this.graphObjectManager.updateLinkPosition(link);
        });
    }
    
    /**
     * Monitor simulation progress with periodic logging
     */
    monitorSimulationProgress() {
        // Only log every 100 ticks to avoid spamming the console
        if (this.tickCounter % 100 === 0) {
            const now = Date.now();
            const timeSinceLastLog = now - this.lastLogTime;
            this.lastLogTime = now;
            
            // Only print if we're still running
            if (this.isRunning) {
                const currentAlpha = this.simulation.alpha();
                console.log(`Simulation progress: tick=${this.tickCounter}, alpha=${currentAlpha.toFixed(6)}, ticks/second=${(100 / (timeSinceLastLog / 1000)).toFixed(1)}`);
            }
        }
    }
}

/**
 * UIManager - Handles UI elements and user interaction
 */
class UIManager {
    constructor(container, dataManager, graphController) {
        this.container = container;
        this.dataManager = dataManager;
        this.graphController = graphController;
        
        // DOM elements
        this.nodeCountEl = document.getElementById('node-count');
        this.linkCountEl = document.getElementById('link-count');
        this.loadingEl = document.getElementById('loading');
        this.searchInput = document.getElementById('search-input');
        this.initialMessageEl = null;
        this.nodeInfoPanel = null;
        
        // Search state
        this.previousSearchValue = '';
        this.isUpdatingAutocomplete = false;
        
        // Autocomplete state
        this.autocompleteList = null;
        this.autocompleteSuggestions = [];
        this.autocompleteSelectedIndex = -1;
        
        // Initialize UI elements
        this.createAutocompleteUI();
        this.createNodeInfoPanel();
        this.setupEventListeners();
    }
    
    /**
     * Set up event listeners for UI controls
     */
    setupEventListeners() {
        // Button event listeners
        const labelsBtn = document.getElementById('toggle-labels-btn');
        if (labelsBtn) {
            // Set initial state based on ThemeManager config
            // The initial state should be true by default
            labelsBtn.classList.add('active');
            
            // Add click handler
            labelsBtn.addEventListener('click', () => {
                const showLabels = this.graphController.toggleLabels();
                // Toggle active class based on the returned state
                labelsBtn.classList.toggle('active', showLabels);
            });
        }

        document.getElementById('reset-btn')?.addEventListener('click', () => {
            this.graphController.resetView();
            this.hideAutocomplete();
            this.previousSearchValue = '';
            this.searchInput.value = '';
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
     * Create the autocomplete UI elements
     */
    createAutocompleteUI() {
        if (!this.searchInput) return;
        
        // Create autocomplete container if it doesn't exist
        if (!this.autocompleteList) {
            this.autocompleteList = document.createElement('div');
            this.autocompleteList.className = 'autocomplete-items';
            this.autocompleteList.style.display = 'none';
            this.autocompleteList.style.position = 'absolute';
            this.autocompleteList.style.zIndex = '999';
            this.autocompleteList.style.maxHeight = '300px';
            this.autocompleteList.style.overflowY = 'auto';
            this.autocompleteList.style.width = '100%';
            this.autocompleteList.style.background = '#fff';
            this.autocompleteList.style.border = '1px solid #ddd';
            this.autocompleteList.style.borderRadius = '0 0 4px 4px';
            this.autocompleteList.style.boxShadow = '0 2px 4px rgba(0,0,0,0.2)';
            
            // Append to parent container
            const searchContainer = this.searchInput.parentNode;
            searchContainer.appendChild(this.autocompleteList);
        }
    }
    
    /**
     * Update autocomplete suggestions based on search term
     * @param {string} searchTerm - The current search term
     */
    updateAutocompleteSuggestions(searchTerm) {
        if (!this.autocompleteList || !searchTerm) {
            this.hideAutocomplete();
            return;
        }
        
        // Clear previous suggestions
        this.autocompleteList.innerHTML = '';
        this.autocompleteSuggestions = [];
        this.autocompleteSelectedIndex = -1;
        
        const maxSuggestions = 10;
        const searchLower = searchTerm.toLowerCase();
        
        // Get all nodes that match the search term
        const matchingNodes = this.dataManager.graphData.nodes
            .filter(node => (
                (node.id && node.id.toLowerCase().includes(searchLower)) ||
                (node.label && node.label.toLowerCase().includes(searchLower))
            ))
            .sort((a, b) => {
                // Prioritize exact matches and matches at the beginning
                const aId = a.id.toLowerCase();
                const bId = b.id.toLowerCase();
                const aLabel = (a.label || '').toLowerCase();
                const bLabel = (b.label || '').toLowerCase();
                
                // Check for exact matches first
                if (aId === searchLower || aLabel === searchLower) return -1;
                if (bId === searchLower || bLabel === searchLower) return 1;
                
                // Then check for starting with search term
                if (aId.startsWith(searchLower) || aLabel.startsWith(searchLower)) return -1;
                if (bId.startsWith(searchLower) || bLabel.startsWith(searchLower)) return 1;
                
                // Fallback to alphabetical
                return aId.localeCompare(bId);
            })
            .slice(0, maxSuggestions);
        
        if (matchingNodes.length === 0) {
            this.hideAutocomplete();
            return;
        }
        
        // Save suggestions for keyboard navigation
        this.autocompleteSuggestions = matchingNodes;
        
        // Create suggestion items
        matchingNodes.forEach((node, index) => {
            const item = document.createElement('div');
            item.className = 'autocomplete-item';
            item.style.padding = '8px 12px';
            item.style.cursor = 'pointer';
            item.style.borderBottom = '1px solid #f4f4f4';
            
            // Highlight matching parts
            const displayText = node.label || node.id;
            const parts = displayText.split(new RegExp(`(${searchTerm})`, 'i'));
            
            parts.forEach(part => {
                const span = document.createElement('span');
                span.textContent = part;
                if (part.toLowerCase() === searchTerm.toLowerCase()) {
                    span.style.fontWeight = 'bold';
                    span.style.backgroundColor = 'rgba(66, 133, 244, 0.1)';
                }
                item.appendChild(span);
            });
            
            // Add node type indicator
            const typeIndicator = document.createElement('span');
            typeIndicator.style.marginLeft = '8px';
            typeIndicator.style.padding = '2px 6px';
            typeIndicator.style.borderRadius = '10px';
            typeIndicator.style.fontSize = '0.8em';
            
            // Different styling for different node types
            if (node.type === 'simple') {
                typeIndicator.textContent = 'item';
                typeIndicator.style.backgroundColor = 'rgba(100, 149, 237, 0.2)';
                typeIndicator.style.color = 'rgb(50, 90, 160)';
            } else {
                typeIndicator.textContent = 'collection';
                typeIndicator.style.backgroundColor = 'rgba(240, 128, 128, 0.2)';
                typeIndicator.style.color = 'rgb(180, 70, 70)';
            }
            
            item.appendChild(typeIndicator);
            
            // Add hover effect
            item.addEventListener('mouseover', () => {
                this.autocompleteSelectedIndex = index;
                this.highlightSelectedSuggestion();
            });
            
            // Add click handler
            item.addEventListener('click', () => {
                this.searchInput.value = node.id;
                this.hideAutocomplete();
                this.graphController.searchNodes(node.id);
            });
            
            this.autocompleteList.appendChild(item);
        });
        
        // Show the autocomplete list
        this.autocompleteList.style.display = 'block';
    }
    
    /**
     * Handle keyboard navigation in autocomplete list
     * @param {KeyboardEvent} event - The keyboard event
     */
    handleAutocompleteKeydown(event) {
        // If no suggestions or hidden, do nothing special except for Enter
        if (this.autocompleteSuggestions.length === 0 || 
            this.autocompleteList.style.display === 'none') {
            if (event.key === 'Enter') {
                const searchTerm = this.searchInput.value.trim();
                if (searchTerm) {
                    this.graphController.searchNodes(searchTerm);
                    this.hideAutocomplete();
                }
            }
            return;
        }
        
        switch (event.key) {
            case 'ArrowDown':
                // Move selection down
                event.preventDefault();
                this.autocompleteSelectedIndex = Math.min(
                    this.autocompleteSelectedIndex + 1,
                    this.autocompleteSuggestions.length - 1
                );
                this.highlightSelectedSuggestion();
                break;
                
            case 'ArrowUp':
                // Move selection up
                event.preventDefault();
                this.autocompleteSelectedIndex = Math.max(this.autocompleteSelectedIndex - 1, -1);
                this.highlightSelectedSuggestion();
                break;
                
            case 'Enter':
                // Select current suggestion or search with current text
                event.preventDefault();
                if (this.autocompleteSelectedIndex >= 0) {
                    const selectedNode = this.autocompleteSuggestions[this.autocompleteSelectedIndex];
                    this.searchInput.value = selectedNode.id;
                    this.graphController.searchNodes(selectedNode.id);
                } else {
                    const searchTerm = this.searchInput.value.trim();
                    if (searchTerm) {
                        this.graphController.searchNodes(searchTerm);
                    }
                }
                this.hideAutocomplete();
                break;
                
            case 'Escape':
                // Hide autocomplete
                event.preventDefault();
                this.hideAutocomplete();
                break;
        }
    }
    
    /**
     * Highlight the currently selected suggestion item
     */
    highlightSelectedSuggestion() {
        // Remove highlight from all items
        const items = this.autocompleteList.querySelectorAll('.autocomplete-item');
        items.forEach(item => {
            item.style.backgroundColor = '';
        });
        
        // Highlight selected item if any
        if (this.autocompleteSelectedIndex >= 0 && this.autocompleteSelectedIndex < items.length) {
            const selectedItem = items[this.autocompleteSelectedIndex];
            selectedItem.style.backgroundColor = 'rgba(66, 133, 244, 0.1)';
            
            // Scroll into view if needed
            if (selectedItem.offsetTop < this.autocompleteList.scrollTop) {
                this.autocompleteList.scrollTop = selectedItem.offsetTop;
            } else if (selectedItem.offsetTop + selectedItem.offsetHeight > 
                       this.autocompleteList.scrollTop + this.autocompleteList.offsetHeight) {
                this.autocompleteList.scrollTop = 
                    selectedItem.offsetTop + selectedItem.offsetHeight - this.autocompleteList.offsetHeight;
            }
        }
    }
    
    /**
     * Hide the autocomplete list
     */
    hideAutocomplete() {
        if (this.autocompleteList) {
            this.autocompleteList.style.display = 'none';
            this.autocompleteSelectedIndex = -1;
        }
    }
    
    /**
     * Show or hide the loading indicator
     * @param {boolean} show - Whether to show or hide the loading indicator
     */
    showLoading(show) {
        if (this.loadingEl) {
            this.loadingEl.style.display = show ? 'block' : 'none';
        }
    }
    
    /**
     * Show an initial message in the graph area
     * @param {string} message - The message to display
     */
    showInitialMessage(message) {
        // Create or update the message element
        if (!this.initialMessageEl) {
            this.initialMessageEl = document.createElement('div');
            this.initialMessageEl.style.position = 'absolute';
            this.initialMessageEl.style.top = '50%';
            this.initialMessageEl.style.left = '50%';
            this.initialMessageEl.style.transform = 'translate(-50%, -50%)';
            this.initialMessageEl.style.background = 'rgba(0, 0, 0, 0.7)';
            this.initialMessageEl.style.color = '#ffffff';
            this.initialMessageEl.style.padding = '20px';
            this.initialMessageEl.style.borderRadius = '8px';
            this.initialMessageEl.style.maxWidth = '80%';
            this.initialMessageEl.style.textAlign = 'center';
            this.initialMessageEl.style.fontSize = '18px';
            this.container.appendChild(this.initialMessageEl);
        }
        
        this.initialMessageEl.textContent = message;
        this.initialMessageEl.style.display = 'block';
    }
    
    /**
     * Hide the initial message
     */
    hideInitialMessage() {
        if (this.initialMessageEl) {
            this.initialMessageEl.style.display = 'none';
        }
    }
    
    /**
     * Show error message
     * @param {string} message - The error message to display
     */
    showError(message) {
        console.error(message);
        this.showInitialMessage(message);
    }
    
    /**
     * Update statistics display
     */
    updateStats() {
        if (this.nodeCountEl) {
            this.nodeCountEl.textContent = this.dataManager.graphData.nodes.length;
        }
        if (this.linkCountEl) {
            this.linkCountEl.textContent = this.dataManager.graphData.links.length;
        }
    }
    
    /**
     * Create the node info panel
     */
    createNodeInfoPanel() {
        if (!this.nodeInfoPanel) {
            this.nodeInfoPanel = document.createElement('div');
            this.nodeInfoPanel.className = 'node-info-panel';
            this.nodeInfoPanel.style.display = 'none';
            this.container.appendChild(this.nodeInfoPanel);
        }
    }
    
    /**
     * Show node information in the info panel
     * @param {string} nodeId - The ID of the node to display info for
     */
    showNodeInfo(nodeId) {
        if (!this.nodeInfoPanel) this.createNodeInfoPanel();
        
        const node = this.dataManager.graphObjects.nodes.get(nodeId);
        if (!node) return;
        
        // Get the node's connections
        const connectedLinks = this.dataManager.getConnectedLinks(nodeId);
        const connectionCount = connectedLinks.length;
        
        // Get any additional properties
        const nodeType = node.type || 'Unknown';
        const nodeLabel = node.label || nodeId;
        
        // Build HTML content
        let html = `
            <h3>${nodeLabel}</h3>
            <p><strong>ID:</strong> ${this.truncateWithEllipsis(nodeId)}</p>
            <p><strong>Type:</strong> ${nodeType}</p>
            <p><strong>Connections:</strong> ${connectionCount}</p>
        `;
        
        // Add any other properties that exist
        if (node.data) {
            html += `<p><strong>Data:</strong> ${JSON.stringify(node.data)}</p>`;
        }
        
        // Add information about connected nodes
        if (connectionCount > 0) {
            html += `<p><strong>Connected Nodes:</strong></p>`;
            html += `<div class="connected-nodes-list" style="max-height: 400px; overflow-y: auto; margin-top: 8px;">`;
            
            // Get info about connected nodes
            const connectedNodes = new Map(); // Use Map to avoid duplicates
            
            connectedLinks.forEach(link => {
                const sourceId = typeof link.source === 'object' ? link.source.id : link.source;
                const targetId = typeof link.target === 'object' ? link.target.id : link.target;
                
                // Get the ID of the connected node (not the current node)
                const connectedNodeId = sourceId === nodeId ? targetId : sourceId;
                
                // Store relationship type if available
                const relationship = link.label || '';
                
                // Get the connected node
                const connectedNode = this.dataManager.graphObjects.nodes.get(connectedNodeId);
                if (connectedNode && !connectedNodes.has(connectedNodeId)) {
                    connectedNodes.set(connectedNodeId, {
                        node: connectedNode,
                        relationship: relationship
                    });
                }
            });
            
            // Display connected nodes (limited to avoid overwhelming the panel)
            const maxNodesToShow = 50;
            let nodeCount = 0;
            
            connectedNodes.forEach((data, connectedNodeId) => {
                if (nodeCount < maxNodesToShow) {
                    const connectedNode = data.node;
                    const relationship = data.relationship;
                    
                    const truncatedId = this.truncateWithEllipsis(connectedNodeId);
                    const nodeLabel = connectedNode.label || truncatedId;
                    const nodeType = connectedNode.type || 'Unknown';
                    
                    html += `<div class="connected-node-item" data-node-id="${connectedNodeId}" 
                            style="margin-bottom: 8px; padding: 5px 10px; border-left: 3px solid #eee; 
                            cursor: pointer; border-radius: 2px;
                            transition: background-color 0.2s;">`;
                    html += `<strong>${nodeLabel}</strong>`;
                    html += `<div style="color: #666; font-size: 0.9em;">${nodeType}</div>`;
                    
                    if (relationship) {
                        html += `<div style="font-style: italic; font-size: 0.85em; margin-top: 2px; color: #888;">
                            Relationship: ${relationship}</div>`;
                    }
                    
                    html += `</div>`;
                    
                    nodeCount++;
                }
            });
            
            // If there are more nodes than we're showing
            if (connectedNodes.size > maxNodesToShow) {
                html += `<div style="font-style: italic; margin-top: 5px;">...and ${connectedNodes.size - maxNodesToShow} more</div>`;
            }
            
            html += `</div>`;
        }
        
        // Set content and show panel
        this.nodeInfoPanel.innerHTML = html;
        this.nodeInfoPanel.style.display = 'block';
        
        // Add event listeners to connected node items
        const nodeItems = this.nodeInfoPanel.querySelectorAll('.connected-node-item');
        nodeItems.forEach(item => {
            // Hover effect
            item.addEventListener('mouseover', () => {
                item.style.backgroundColor = '#f5f5f5';
                item.style.borderLeftColor = '#4D90FE';
            });
            
            item.addEventListener('mouseout', () => {
                item.style.backgroundColor = '';
                item.style.borderLeftColor = '#eee';
            });
            
            // Click to select the node
            item.addEventListener('click', () => {
                const clickedNodeId = item.getAttribute('data-node-id');
                if (clickedNodeId && this.graphController.eventManager) {
                    this.graphController.eventManager.selectNode(clickedNodeId);
                    this.graphController.eventManager.focusOnNode(clickedNodeId);
                }
            });
        });
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
     * Truncate text and add ellipsis in the middle
     * @param {string} text - The text to truncate
     * @returns {string} Truncated text with ellipsis
     */
    truncateWithEllipsis(text) {
        // Show only if longer than 15 characters (6 + 3 + 6)
        if (!text || text.length <= 15) {
            return text;
        }
        
        // Take exactly 6 chars from start and 6 from end
        return text.substring(0, 6) + '...' + text.substring(text.length - 6);
    }
}

/**
 * EventManager - Handles user interaction events
 */
class EventManager {
    constructor(sceneManager, dataManager, graphObjectManager) {
        this.sceneManager = sceneManager;
        this.dataManager = dataManager;
        this.graphObjectManager = graphObjectManager;
        
        // Set up event listeners
        this.setupEventListeners();
    }
    
    /**
     * Set up graph-specific interaction handlers
     */
    setupEventListeners() {
        const renderer = this.sceneManager.renderer;
        if (!renderer || !renderer.domElement) return;
        
        // Add click event listener for node selection
        renderer.domElement.addEventListener('click', this.onMouseClick.bind(this));
        
        // Add double-click event listener for camera focus
        renderer.domElement.addEventListener('dblclick', this.onDoubleClick.bind(this));
        
        // Add hover event listeners
        renderer.domElement.addEventListener('mousemove', this.onMouseMove.bind(this));
    }
    
    /**
     * Handle mouse click events for node selection
     * @param {MouseEvent} event - The mouse event
     */
    onMouseClick(event) {
        // Calculate mouse position and find intersections
        this.sceneManager.updateMousePosition(event);
        const intersects = this.sceneManager.getIntersectedObjects();
        
        // Check if we clicked on a node
        const nodeIntersect = intersects.find(obj => 
            obj.object && obj.object.userData && obj.object.userData.id
        );
        
        if (nodeIntersect) {
            // We clicked on a node
            const nodeId = nodeIntersect.object.userData.id;
            this.selectNode(nodeId);
        } else {
            // Clicked on empty space - deselect
            this.deselectNode();
        }
    }
    
    /**
     * Handle double-click events for focusing on nodes
     * @param {MouseEvent} event - The mouse event
     */
    onDoubleClick(event) {
        // Calculate mouse position and find intersections
        this.sceneManager.updateMousePosition(event);
        const intersects = this.sceneManager.getIntersectedObjects();
        
        // Check if we double-clicked on a node
        const nodeIntersect = intersects.find(obj => 
            obj.object && obj.object.userData && obj.object.userData.id
        );
        
        if (nodeIntersect) {
            // Double-clicked on a node - focus camera on this node
            const nodeId = nodeIntersect.object.userData.id;
            this.focusOnNode(nodeId);
        }
    }
    
    /**
     * Handle mouse movement for hover effects
     * @param {MouseEvent} event - The mouse event
     */
    onMouseMove(event) {
        // Calculate mouse position and find intersections
        this.sceneManager.updateMousePosition(event);
        const intersects = this.sceneManager.getIntersectedObjects();
        
        // Check if we're hovering over a node
        const nodeIntersect = intersects.find(obj => 
            obj.object && obj.object.userData && obj.object.userData.id
        );
        
        if (nodeIntersect) {
            // Hovering over a node
            const nodeId = nodeIntersect.object.userData.id;
            this.hoverNode(nodeId);
        } else {
            // Not hovering over any node
            this.unhoverNode();
        }
    }
    
    /**
     * Select a node and highlight it and its connections
     * @param {string} nodeId - The ID of the node to select
     */
    selectNode(nodeId) {
        if (this.dataManager.selectedNode === nodeId) return;
        
        // Set selection in data manager
        this.dataManager.setSelectedNode(nodeId);
        
        // Update node colors
        this.updateNodeColors();
        
        // Update link colors
        this.updateLinkColors();
        
        // First, hide all link labels
        this.dataManager.graphObjects.links.forEach((link, id) => {
            if (link.labelObject) {
                link.labelObject.visible = false;
            }
        });
        
        // Then show labels for active links
        this.dataManager.activeLinks.forEach(linkId => {
            const link = this.dataManager.graphObjects.links.get(linkId);
            if (link && link.labelObject) {
                link.labelObject.visible = true;
            }
        });
        
        // Show node info panel
        const graphController = this.sceneManager.graphController || 
            (this.graphObjectManager && this.graphObjectManager.graphController);
            
        if (graphController && graphController.uiManager) {
            graphController.uiManager.showNodeInfo(nodeId);
        }
    }
    
    /**
     * Deselect the currently selected node
     */
    deselectNode() {
        if (!this.dataManager.selectedNode) return;
        
        // Hide all link labels before clearing selection
        this.dataManager.activeLinks.forEach(linkId => {
            const link = this.dataManager.graphObjects.links.get(linkId);
            if (link && link.labelObject) {
                link.labelObject.visible = false;
            }
        });
        
        // Clear selection in data manager
        this.dataManager.clearSelectedNode();
        
        // Update node colors
        this.updateNodeColors();
        
        // Update link colors
        this.updateLinkColors();
        
        // Hide node info panel
        const graphController = this.sceneManager.graphController || 
            (this.graphObjectManager && this.graphObjectManager.graphController);
            
        if (graphController && graphController.uiManager) {
            graphController.uiManager.hideNodeInfo();
        }
    }
    
    /**
     * Focus the camera on a specific node
     * @param {string} nodeId - The ID of the node to focus on
     */
    focusOnNode(nodeId) {
        const node = this.dataManager.graphObjects.nodes.get(nodeId);
        if (!node || !node.object) return;
        
        const position = node.object.position.clone();
        this.sceneManager.focusCamera(position);
    }
    
    /**
     * Apply hover effect to a node
     * @param {string} nodeId - The ID of the node to hover
     */
    hoverNode(nodeId) {
        // If already hovering over this node, do nothing
        if (this.dataManager.hoveredNode === nodeId) return;
        
        // Set hover in data manager
        this.dataManager.setHoveredNode(nodeId);
        
        // Update appearance
        this.graphObjectManager.updateNodeColors(nodeId);
        
        // Change cursor to pointer
        this.sceneManager.renderer.domElement.style.cursor = 'pointer';
    }
    
    /**
     * Remove hover effect from the currently hovered node
     */
    unhoverNode() {
        if (!this.dataManager.hoveredNode) return;
        
        // Get the node ID before clearing
        const nodeId = this.dataManager.hoveredNode;
        
        // Clear hover in data manager
        this.dataManager.clearHoveredNode();
        
        // Update appearance
        this.graphObjectManager.updateNodeColors(nodeId);
        
        // Reset cursor
        this.sceneManager.renderer.domElement.style.cursor = 'auto';
    }
    
    /**
     * Update the colors of all visible nodes based on selection state
     */
    updateNodeColors() {
        this.dataManager.graphObjects.nodes.forEach((node, id) => {
            this.graphObjectManager.updateNodeColors(id);
        });
    }
    
    /**
     * Update the colors of all visible links based on selection state
     */
    updateLinkColors() {
        this.dataManager.graphObjects.links.forEach((link, id) => {
            this.graphObjectManager.updateLinkColors(id);
        });
    }
}

/**
 * Main controller class that coordinates all graph components
 */
class GraphController {
    constructor(containerId) {
        // DOM container reference
        this.container = document.getElementById(containerId);
        
        // Initialize component managers
        this.themeManager = new ThemeManager();
        this.sceneManager = new SceneManager(this.container, this.themeManager);
        this.sceneManager.graphController = this; // Add reference to this controller
        
        this.dataManager = new DataManager();
        this.graphObjectManager = new GraphObjectManager(this.sceneManager, this.dataManager, this.themeManager);
        this.graphObjectManager.graphController = this; // Add reference to this controller
        
        this.simulationManager = new SimulationManager(this.dataManager, this.graphObjectManager, this.themeManager);
        this.uiManager = new UIManager(this.container, this.dataManager, this);
        this.eventManager = new EventManager(this.sceneManager, this.dataManager, this.graphObjectManager);
        this.eventManager.graphController = this; // Add reference to this controller
        
        // Load data
        this.loadGraphData();
        
        // Start animation loop
        this.animate();
    }
    
    /**
     * Load graph data from the server
     */
    loadGraphData() {
		this.clearDisplay();
        // Show loading indicator
        this.uiManager.showLoading(true);
        
        // Load data via the data manager
        this.dataManager.loadData()
            .then(data => {
                // Initialize the force simulation with loaded data
                this.simulationManager.updateSimulation(false);
                
                // Update statistics
                this.uiManager.updateStats();
                
                // Show initial message
                this.uiManager.showInitialMessage("Enter a search term to display nodes");
                
                // Hide loading indicator
                this.uiManager.showLoading(false);
            })
            .catch(error => {
                // Show error message
                this.uiManager.showError('Failed to load graph data: ' + error.message);
                this.uiManager.showLoading(false);
            });
    }
    
    /**
     * Search for nodes by term and display them
     * @param {string} searchTerm - The term to search for
     */
    searchNodes(searchTerm) {
        // Clear current display
        this.clearDisplay();
        
        // Hide the initial message
        this.uiManager.hideInitialMessage();
        
        if (!searchTerm) return;
        
        // Find matching nodes
        const matchingNodeIds = this.dataManager.searchNodes(searchTerm);
        
        // If no nodes found, show a message
        if (matchingNodeIds.length === 0) {
            console.log(`No nodes found matching "${searchTerm}"`);
            this.uiManager.showInitialMessage(`No nodes found matching "${searchTerm}"`);
            return;
        }
        
        console.log(`Found ${matchingNodeIds.length} nodes matching "${searchTerm}"`);
        
        // Show loading indicator during simulation
        this.uiManager.showLoading(true);
        
        // Add each matching node and its connections with depth of 5
        const addedNodes = new Set();
        
        matchingNodeIds.forEach(nodeId => {
            // Use getConnectedSubgraph to get nodes and links up to depth 5
            const { nodes, links } = this.dataManager.getConnectedSubgraph(nodeId, 10);
            
            // Add all nodes to the scene
            nodes.forEach(node => {
                if (!this.dataManager.graphObjects.nodes.has(node.id)) {
                    this.graphObjectManager.createNodeObject(node);
                    addedNodes.add(node.id);
                }
            });
            
            // Add all links to the scene
            links.forEach(link => {
                const sourceId = typeof link.source === 'object' ? link.source.id : link.source;
                const targetId = typeof link.target === 'object' ? link.target.id : link.target;
                const linkId = `${sourceId}-${targetId}`;
                
                if (!this.dataManager.graphObjects.links.has(linkId)) {
                    this.graphObjectManager.createLinkObject(link);
                }
            });
        });
        
        // Update simulation and restart it properly
        this.simulationManager.updateSimulation(true);
        
        // Center the view on the found nodes
        this.centerOnNodes(Array.from(addedNodes));
        
        // Hide loading indicator when view is centered
        this.uiManager.showLoading(false);
    }
    
    /**
     * Clear the current display
     */
    clearDisplay() {
        this.graphObjectManager.clearVisibleObjects();
    }
    
    /**
     * Center the view on a set of nodes
     * @param {Array} nodeIds - Array of node IDs to center on
     */
    centerOnNodes(nodeIds) {
        if (!nodeIds || nodeIds.length === 0) return;
        
        // Calculate the center position of the specified nodes
        let center = { x: 0, y: 0, z: 0 };
        let count = 0;
        
        nodeIds.forEach(nodeId => {
            const nodeData = this.dataManager.graphObjects.nodes.get(nodeId);
            if (nodeData && nodeData.object) {
                center.x += nodeData.object.position.x;
                center.y += nodeData.object.position.y;
                center.z += nodeData.object.position.z;
                count++;
            }
        });
        
        if (count === 0) return;
        
        center.x /= count;
        center.y /= count;
        center.z /= count;
        
        // Set the camera target for perspective camera
        this.sceneManager.controls.target.set(center.x, center.y, 0);
        
        // Position the perspective camera
        const distance = 1000;
        this.sceneManager.camera.position.set(
            center.x, 
            center.y, 
            distance
        );
        
        // Update the camera and controls
        this.sceneManager.camera.updateProjectionMatrix();
        this.sceneManager.controls.update();
    }
    
    /**
     * Toggle label visibility
     */
    toggleLabels() {
        const showLabels = this.graphObjectManager.toggleLabels();
        return showLabels;
    }
    
    /**
     * Toggle physics simulation
     */
    togglePhysics() {
        this.simulationManager.togglePhysics();
    }
    
    /**
     * Reset the view
     */
    resetView() {
        // Clear current display
        this.clearDisplay();
        
        // Reset camera
        this.sceneManager.resetView();
        
        // Show initial message
        this.uiManager.showInitialMessage("Enter a search term to display nodes");
    }
    
    /**
     * Animation loop
     */
    animate() {
        requestAnimationFrame(() => this.animate());
        
        // Update scene
        this.sceneManager.update();
    }
}

// Initialize the application when the page loads
document.addEventListener('DOMContentLoaded', () => {
    new GraphController('graph-container');
}); 