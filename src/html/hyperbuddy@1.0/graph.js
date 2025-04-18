/**
 * HyperBEAM Cache Graph Renderer - Modular Version
 * A 2D force-directed graph visualization for the HyperBEAM cache system
 */

/**
 * Utility function to create a circular texture for node rendering
 * @param {number} size - Size of the texture in pixels
 * @param {number|string} color - Color of the circle (hex)
 * @param {boolean} border - Whether to add a border
 * @param {number|string} borderColor - Color of the border (hex)
 * @returns {THREE.Texture} The generated texture
 */
function createCircleTexture(size = 64, color = 0xffffff, border = false, borderColor = 0x000000) {
    // Create a canvas element
    const canvas = document.createElement('canvas');
    canvas.width = size;
    canvas.height = size;
    const context = canvas.getContext('2d');
    
    // Clear canvas with transparent background
    context.clearRect(0, 0, size, size);
    
    // Convert color to string format if it's a number
    const fillColor = typeof color === 'number' ? '#' + color.toString(16).padStart(6, '0') : color;
    const strokeColor = typeof borderColor === 'number' ? '#' + borderColor.toString(16).padStart(6, '0') : borderColor;
    
    // Draw a circle
    const radius = size / 2 - 2;
    context.beginPath();
    context.arc(size / 2, size / 2, radius, 0, 2 * Math.PI, false);
    context.fillStyle = fillColor;
    context.fill();
    
    // Add border if requested
    if (border) {
        context.lineWidth = 1;
        context.strokeStyle = strokeColor;
        context.stroke();
    }
    
    // Create a texture from the canvas
    const texture = new THREE.CanvasTexture(canvas);
    texture.needsUpdate = true;
    
    return texture;
}

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
                hover: 0xfafa33           // Warm orange/yellow for hover
            },
            // Display options
            showLabels: true,
            physicsEnabled: true,
            // Physics settings
            defaultDistance: 150,
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
        
        // Performance optimization
        this.frustum = new THREE.Frustum();
        this.projScreenMatrix = new THREE.Matrix4();
        this.tmpVector = new THREE.Vector3();
        this.enableFrustumCulling = true;
        this.frustumCullingDistance = 1250; // Beyond this distance, apply visibility culling
        
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
            15000  // Increased far clipping plane
        );
        this.camera.position.z = 1000;
        
        // Create renderer
        this.renderer = new THREE.WebGLRenderer({ 
            antialias: true,
            alpha: true
        });
        this.renderer.setSize(width, height);
        this.renderer.setClearColor(this.themeManager.config.colors.background, 1);
        this.renderer.sortObjects = true; // Enable sorting for proper z-ordering
        this.container.appendChild(this.renderer.domElement);
        
        // Configure raycaster for better point detection
        this.raycaster = new THREE.Raycaster();
        this.raycaster.params.Points.threshold = 10; // Increase threshold for easier point selection
        
        // Add orbit controls limited to 2D movement with perspective camera
        this.controls = new THREE.OrbitControls(this.camera, this.renderer.domElement);
        this.controls.enableDamping = true;
        this.controls.dampingFactor = 0.1;
        this.controls.enableRotate = false; // Disable 3D rotation
        this.controls.screenSpacePanning = true;
        
        // Set zoom limits - constrain camera between 750 and 15000 on z-axis
        this.controls.minDistance = 750;
        this.controls.maxDistance = 15000;
        
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
            
            // Enforce camera z-position limits
            if (this.camera.position.z < 750) {
                this.camera.position.z = 750;
            } else if (this.camera.position.z > 15000) {
                this.camera.position.z = 15000;
            }
        }
        
        // Apply frustum culling for distant objects
        if (this.enableFrustumCulling) {
            this.updateFrustumCulling();
        }
        
        // Render the scene
        this.renderer.render(this.scene, this.camera);
    }
    
    /**
     * Update frustum and apply visibility culling for better performance
     */
    updateFrustumCulling() {
        // Update the frustum
        this.projScreenMatrix.multiplyMatrices(
            this.camera.projectionMatrix, 
            this.camera.matrixWorldInverse
        );
        this.frustum.setFromProjectionMatrix(this.projScreenMatrix);
        
        // If we have a reference to the controller, access the dataManager
        if (this.graphController && this.graphController.dataManager) {
            const dataManager = this.graphController.dataManager;
            
            // Process all nodes
            dataManager.graphObjects.nodes.forEach(node => {
                if (!node.object) return;
                
                // Get distance from camera
                this.tmpVector.copy(node.object.position);
                const distance = this.tmpVector.distanceTo(this.camera.position);
                
                // If the node is beyond our threshold, check if it's in the frustum
                if (distance > this.frustumCullingDistance) {
                    // Check if the node is in the frustum
                    const isVisible = this.frustum.containsPoint(node.object.position);
                    
                    // Only update visibility if necessary to avoid unnecessary matrix updates
                    if (node.object.visible !== isVisible) {
                        node.object.visible = isVisible;
                        
                        // Also update label visibility if it exists
                        if (node.labelObject) {
                            node.labelObject.visible = isVisible && this.themeManager.config.showLabels;
                        }
                    }
                } else if (!node.object.visible) {
                    // If node is within threshold distance but not visible, make visible
                    node.object.visible = true;
                    if (node.labelObject) {
                        node.labelObject.visible = this.themeManager.config.showLabels;
                    }
                }
            });
            
            // Optional: Process links for better culling
            // Only show links if both endpoints are visible
            dataManager.graphObjects.links.forEach(link => {
                if (!link.line) return;
                
                const sourceNode = dataManager.graphObjects.nodes.get(link.sourceId);
                const targetNode = dataManager.graphObjects.nodes.get(link.targetId);
                
                if (sourceNode && targetNode && sourceNode.object && targetNode.object) {
                    const sourceDist = sourceNode.object.position.distanceTo(this.camera.position);
                    const targetDist = targetNode.object.position.distanceTo(this.camera.position);
                    
                    // If both nodes are distant, check if they're visible
                    if (sourceDist > this.frustumCullingDistance && targetDist > this.frustumCullingDistance) {
                        const sourceVisible = sourceNode.object.visible;
                        const targetVisible = targetNode.object.visible;
                        
                        // Only show link if both endpoints are visible
                        link.line.visible = sourceVisible && targetVisible;
                        
                        // Update label visibility if needed
                        if (link.labelObject) {
                            link.labelObject.visible = sourceVisible && targetVisible && 
                                this.themeManager.config.showLabels &&
                                dataManager.activeLinks.has(`${link.sourceId}-${link.targetId}`);
                        }
                    } else if (!link.line.visible) {
                        // If at least one endpoint is close, show the link
                        link.line.visible = true;
                    }
                }
            });
        }
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
        
        // Add to NodeCloud for efficient rendering
        if (this.graphController && this.graphController.nodeCloud) {
            // Add to node cloud
            const nodeIndex = this.graphController.nodeCloud.addNode(node);
            
            // Create a virtual object for compatibility
            // This is needed because other code expects a THREE.Object3D
            const virtualObject = {
                position: new THREE.Vector3(node.x || 0, node.y || 0, this.themeManager.config.zPos.node),
                visible: true,
                userData: { id: node.id, type: node.type, label: node.label }
            };
            
            // Store virtual object reference
            node.object = virtualObject;
            node.nodeCloudIndex = nodeIndex;
        }
        
        // Create label if enabled
        let labelObject = null;
        if (this.themeManager.config.showLabels) {
            labelObject = this.createLabel(node);
        }
        
        // Store label reference
        node.labelObject = labelObject;
        
        // Store in dataManager
        this.dataManager.storeNodeObject(node.id, node);
        
        // Add to spatial grid if simulation manager is available
        if (this.graphController && this.graphController.simulationManager) {
            this.graphController.simulationManager.addNodeToSpatialGrid(node);
        }
        
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
        if (!node) return;
        
        if (this.graphController && this.graphController.nodeCloud) {
            // Update position in the NodeCloud
            this.graphController.nodeCloud.updateNodePosition(node.id, node.x || 0, node.y || 0);
            
            // Also update the virtual object for compatibility
            if (node.object && node.object.position) {
                node.object.position.x = node.x || 0;
                node.object.position.y = node.y || 0;
            }
        }
        
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
        if (!node) return;
        
        let state = 'default';
        
        // Determine state based on selection and hover
        if (nodeId === this.dataManager.selectedNode) {
            state = 'selected';
        } else if (this.dataManager.neighborNodes.has(nodeId)) {
            state = 'neighbor';
        } else if (nodeId === this.dataManager.hoveredNode) {
            state = 'hover';
        }
        
        if (this.graphController && this.graphController.nodeCloud) {
            // Update the color in the node cloud
            this.graphController.nodeCloud.updateNodeColor(nodeId, node.type, state);
        }
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
        
        // Remove from NodeCloud if available
        if (this.graphController && this.graphController.nodeCloud) {
            this.graphController.nodeCloud.removeNode(nodeId);
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
        // Clear NodeCloud if available
        if (this.graphController && this.graphController.nodeCloud) {
            this.graphController.nodeCloud.clear();
        }
        
        // Remove label objects from the scene
        this.dataManager.graphObjects.nodes.forEach((node, id) => {
            if (node.labelObject) {
                this.sceneManager.removeFromScene(node.labelObject);
            }
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
 * SpatialGrid - Simple spatial partitioning for efficient queries
 */
class SpatialGrid {
    constructor(cellSize = 200) {
        this.cellSize = cellSize;
        this.grid = new Map();
        this.objects = new Set();
    }
    
    /**
     * Get the cell key for a position
     * @param {THREE.Vector3} position - The position to get the cell for
     * @returns {string} The cell key
     */
    getCellKey(position) {
        const x = Math.floor(position.x / this.cellSize);
        const y = Math.floor(position.y / this.cellSize);
        const z = Math.floor(position.z / this.cellSize);
        return `${x},${y},${z}`;
    }
    
    /**
     * Add an object to the grid
     * @param {Object} object - The object to add
     */
    addObject(object) {
        if (!object.object || !object.object.position) return;
        
        const position = object.object.position;
        const cellKey = this.getCellKey(position);
        
        // Create cell if it doesn't exist
        if (!this.grid.has(cellKey)) {
            this.grid.set(cellKey, new Set());
        }
        
        // Add to cell
        this.grid.get(cellKey).add(object);
        this.objects.add(object);
    }
    
    /**
     * Remove an object from the grid
     * @param {Object} object - The object to remove
     */
    removeObject(object) {
        if (!object.object || !object.object.position) return;
        
        // Remove from all cells (in case it moved)
        this.grid.forEach(cell => {
            cell.delete(object);
        });
        
        this.objects.delete(object);
        
        // Clean up empty cells
        this.grid.forEach((cell, key) => {
            if (cell.size === 0) {
                this.grid.delete(key);
            }
        });
    }
    
    /**
     * Find objects within a radius of a position
     * @param {THREE.Vector3} position - The center position
     * @param {number} radius - The radius to search within
     * @returns {Array} Array of objects within the radius
     */
    findNearbyObjects(position, radius) {
        // Calculate the cell range to check
        const cellRadius = Math.ceil(radius / this.cellSize);
        const centerX = Math.floor(position.x / this.cellSize);
        const centerY = Math.floor(position.y / this.cellSize);
        const centerZ = Math.floor(position.z / this.cellSize);
        
        const result = [];
        const radiusSquared = radius * radius;
        
        // Check each cell in the range
        for (let x = centerX - cellRadius; x <= centerX + cellRadius; x++) {
            for (let y = centerY - cellRadius; y <= centerY + cellRadius; y++) {
                for (let z = centerZ - cellRadius; z <= centerZ + cellRadius; z++) {
                    const cellKey = `${x},${y},${z}`;
                    const cell = this.grid.get(cellKey);
                    
                    if (cell) {
                        // Check each object in the cell
                        cell.forEach(object => {
                            if (object.object && object.object.position) {
                                const distSquared = position.distanceToSquared(object.object.position);
                                if (distSquared <= radiusSquared) {
                                    result.push(object);
                                }
                            }
                        });
                    }
                }
            }
        }
        
        return result;
    }
    
    /**
     * Clear all objects from the grid
     */
    clear() {
        this.grid.clear();
        this.objects.clear();
    }
    
    /**
     * Get the total number of objects in the grid
     * @returns {number} The number of objects
     */
    size() {
        return this.objects.size;
    }
}

/**
 * NodeCloud - Manages efficient point cloud rendering for graph nodes
 */
class NodeCloud {
    constructor(scene, themeManager) {
        this.scene = scene;
        this.sceneManager = null; // Will be set by the EventManager
        this.themeManager = themeManager;
        
        // Capacity tracking
        this.maxNodes = 1000; // Initial capacity
        this.nodeCount = 0;
        
        // Node tracking
        this.nodeIndices = new Map(); // Maps node IDs to their index in the arrays
        this.nodeTypes = new Map();   // Maps node IDs to their types
        this.positions = null;
        this.colors = null;
        this.sizes = null;
        
        // Selection tracking
        this.selectedIndices = new Set();
        this.neighborIndices = new Set();
        this.hoverIndex = -1;
        
        // Create base texture for all nodes
        this.baseTexture = createCircleTexture(64, 0xffffff);
        
        // Initialize geometry and point cloud
        this.initialize();
    }
    
    /**
     * Initialize buffers and point cloud with initial capacity
     */
    initialize() {
        // Create buffer attributes with initial capacity
        this.positions = new Float32Array(this.maxNodes * 3);
        this.colors = new Float32Array(this.maxNodes * 3);
        this.sizes = new Float32Array(this.maxNodes);
        
        // Create buffer geometry
        this.geometry = new THREE.BufferGeometry();
        this.geometry.setAttribute('position', new THREE.BufferAttribute(this.positions, 3));
        this.geometry.setAttribute('color', new THREE.BufferAttribute(this.colors, 3));
        this.geometry.setAttribute('size', new THREE.BufferAttribute(this.sizes, 1));
        
        // Set draw range to only render active nodes
        this.geometry.setDrawRange(0, 0);
        
        // Create point material
        this.material = new THREE.ShaderMaterial({
            uniforms: {
                pointTexture: { value: this.baseTexture }
            },
            vertexShader: `
                precision highp float;
                attribute float size;
                attribute vec3 color;
                varying vec3 vColor;
                
                void main() {
                    vColor = color;
                    vec4 mvPosition = modelViewMatrix * vec4(position, 1.0);
                    gl_PointSize = size * (1200.0 / -mvPosition.z);
                    gl_Position = projectionMatrix * mvPosition;
                }
            `,
            fragmentShader: `
                precision highp float;
                uniform sampler2D pointTexture;
                varying vec3 vColor;
                
                void main() {
                    vec4 texColor = texture2D(pointTexture, gl_PointCoord);
                    if (texColor.a < 0.5) discard;
                    gl_FragColor = vec4(vColor, 1.0) * texColor;
                }
            `,
            transparent: true,
            depthWrite: false,
            blending: THREE.NormalBlending
        });
        
        // Create points
        this.points = new THREE.Points(this.geometry, this.material);
        this.points.frustumCulled = false; // Disable frustum culling
        this.points.renderOrder = 10;
        
        // Add to scene
        this.scene.add(this.points);
    }
    
    /**
     * Resize buffers if needed
     */
    ensureCapacity(requiredNodes) {
        if (requiredNodes <= this.maxNodes) return;
        
        // Calculate new capacity (1.5x required or 2x current, whichever is larger)
        const newCapacity = Math.max(Math.ceil(requiredNodes * 1.5), this.maxNodes * 2);
        
        // Create new arrays
        const newPositions = new Float32Array(newCapacity * 3);
        const newColors = new Float32Array(newCapacity * 3);
        const newSizes = new Float32Array(newCapacity);
        
        // Copy existing data
        newPositions.set(this.positions);
        newColors.set(this.colors);
        newSizes.set(this.sizes);
        
        // Update references
        this.positions = newPositions;
        this.colors = newColors;
        this.sizes = newSizes;
        this.maxNodes = newCapacity;
        
        // Update buffer attributes
        this.geometry.setAttribute('position', new THREE.BufferAttribute(this.positions, 3));
        this.geometry.setAttribute('color', new THREE.BufferAttribute(this.colors, 3));
        this.geometry.setAttribute('size', new THREE.BufferAttribute(this.sizes, 1));
    }
    
    /**
     * Add a node to the point cloud
     * @param {Object} node - Node data with position and type
     * @returns {number} Index of the node in the point cloud
     */
    addNode(node) {
        // Get node ID
        const nodeId = node.id;
        
        // Check if this node is already in the cloud
        if (this.nodeIndices.has(nodeId)) {
            return this.nodeIndices.get(nodeId);
        }
        
        // Ensure we have enough capacity
        this.ensureCapacity(this.nodeCount + 1);
        
        // Add to the end of the arrays
        const index = this.nodeCount;
        const i3 = index * 3;
        
        // Set position
        this.positions[i3] = node.x || 0;
        this.positions[i3 + 1] = node.y || 0;
        this.positions[i3 + 2] = this.themeManager.config.zPos.node;
        
        // Store node type for later reference
        this.nodeTypes.set(nodeId, node.type || 'simple');
        
        // Set color based on node type
        const color = new THREE.Color(this.themeManager.getNodeColor(node.type));
        this.colors[i3] = color.r;
        this.colors[i3 + 1] = color.g;
        this.colors[i3 + 2] = color.b;
        
        // Set size based on node type - use larger sizes to make selection easier
        const baseSize = this.themeManager.getNodeSize(node.type);
        this.sizes[index] = baseSize * 4; // Increase size to improve interaction
        
        // Track this node
        this.nodeIndices.set(nodeId, index);
        this.nodeCount++;
        
        // Update draw range
        this.geometry.setDrawRange(0, this.nodeCount);
        
        // Mark attributes as needing update
        this.geometry.attributes.position.needsUpdate = true;
        this.geometry.attributes.color.needsUpdate = true;
        this.geometry.attributes.size.needsUpdate = true;
        
        return index;
    }
    
    /**
     * Update a node's position
     * @param {string} nodeId - ID of the node to update
     * @param {number} x - New X position
     * @param {number} y - New Y position
     */
    updateNodePosition(nodeId, x, y) {
        if (!this.nodeIndices.has(nodeId)) return;
        
        const index = this.nodeIndices.get(nodeId);
        const i3 = index * 3;
        
        this.positions[i3] = x;
        this.positions[i3 + 1] = y;
        
        // Mark position attribute as needing update
        this.geometry.attributes.position.needsUpdate = true;
    }
    
    /**
     * Update a node's color based on state
     * @param {string} nodeId - ID of the node to update
     * @param {string} nodeType - Type of the node
     * @param {string} state - State of the node (default, selected, neighbor, hover)
     */
    updateNodeColor(nodeId, nodeType, state = 'default') {
        if (!this.nodeIndices.has(nodeId)) return;
        
        // Store node type if provided
        if (nodeType) {
            this.nodeTypes.set(nodeId, nodeType);
        } else {
            // Use stored type if available
            nodeType = this.nodeTypes.get(nodeId) || 'simple';
        }
        
        const index = this.nodeIndices.get(nodeId);
        const i3 = index * 3;
        
        // Get color for this node state
        const color = new THREE.Color(this.themeManager.getNodeColor(nodeType, state));
        
        // Update color in buffer
        this.colors[i3] = color.r;
        this.colors[i3 + 1] = color.g;
        this.colors[i3 + 2] = color.b;
        
        // Mark colors attribute as needing update
        this.geometry.attributes.color.needsUpdate = true;
    }
    
    /**
     * Update colors for all nodes based on selection state
     * @param {string} selectedId - ID of the selected node
     * @param {Set<string>} neighborIds - Set of neighbor node IDs
     * @param {string} hoveredId - ID of the hovered node
     */
    updateColors(selectedId, neighborIds, hoveredId) {
        // Reset tracking sets
        this.selectedIndices.clear();
        this.neighborIndices.clear();
        this.hoverIndex = -1;
        
        // Track indices for faster updates
        if (selectedId && this.nodeIndices.has(selectedId)) {
            this.selectedIndices.add(this.nodeIndices.get(selectedId));
        }
        
        neighborIds.forEach(id => {
            if (this.nodeIndices.has(id)) {
                this.neighborIndices.add(this.nodeIndices.get(id));
            }
        });
        
        if (hoveredId && this.nodeIndices.has(hoveredId)) {
            this.hoverIndex = this.nodeIndices.get(hoveredId);
        }
        
        // Update all node colors based on state
        for (const [nodeId, index] of this.nodeIndices.entries()) {
            const i3 = index * 3;
            let state = 'default';
            
            // Determine state based on selection and hover
            if (index === this.hoverIndex) {
                state = 'hover';
            } else if (this.selectedIndices.has(index)) {
                state = 'selected';
            } else if (this.neighborIndices.has(index)) {
                state = 'neighbor';
            }
            
            // Get node type from our stored map
            const nodeType = this.nodeTypes.get(nodeId) || 'simple';
            
            // Get color for this state
            const color = new THREE.Color(this.themeManager.getNodeColor(nodeType, state));
            
            // Update color
            this.colors[i3] = color.r;
            this.colors[i3 + 1] = color.g;
            this.colors[i3 + 2] = color.b;
        }
        
        // Mark attributes as needing update
        this.geometry.attributes.color.needsUpdate = true;
    }
    
    /**
     * Find the closest node to a mouse position
     * @param {THREE.Raycaster} raycaster - The raycaster
     * @param {number} threshold - Maximum distance to consider a hit (in screen space)
     * @returns {string|null} ID of the closest node or null if none found
     */
    findClosestNode(raycaster, threshold = 0.05) {
        if (this.nodeCount === 0 || !this.positions) return null;
        
        // Get the camera from the scene manager or from the global controller
        let camera = null;
        let mousePosition = null;
        
        if (this.sceneManager) {
            camera = this.sceneManager.camera;
            mousePosition = this.sceneManager.mouse; // This is the normalized mouse position
        } else if (window.lastGraphController && window.lastGraphController.sceneManager) {
            camera = window.lastGraphController.sceneManager.camera;
            mousePosition = window.lastGraphController.sceneManager.mouse;
        }
        
        // If we can't get a camera or mouse position, we can't continue
        if (!camera || !mousePosition) return null;
        
        // Find the closest node based on screen space distance
        let closestDistance = Infinity;
        let closestNodeId = null;
        
        // Check each node
        for (const [nodeId, index] of this.nodeIndices.entries()) {
            const i3 = index * 3;
            
            // Get node position
            const nodePos = new THREE.Vector3(
                this.positions[i3],
                this.positions[i3 + 1],
                this.positions[i3 + 2]
            );
            
            // Project to screen space
            const screenPos = nodePos.clone().project(camera);
            
            // Calculate 2D distance in screen space between mouse and node
            const dx = screenPos.x - mousePosition.x;
            const dy = screenPos.y - mousePosition.y;
            const distance = Math.sqrt(dx * dx + dy * dy);
            
            // Get node size and adjust threshold based on size
            const nodeSize = this.sizes[index];
            const adjustedThreshold = threshold * (1 + (nodeSize / 10));
            
            // If this node is closer than the current closest and within threshold, update
            if (distance < closestDistance && distance < adjustedThreshold) {
                closestDistance = distance;
                closestNodeId = nodeId;
            }
        }
        
        return closestNodeId;
    }
    
    /**
     * Remove a node from the point cloud
     * @param {string} nodeId - ID of the node to remove
     */
    removeNode(nodeId) {
        if (!this.nodeIndices.has(nodeId)) return;
        
        const indexToRemove = this.nodeIndices.get(nodeId);
        
        // Only perform complex removal if not the last node
        if (indexToRemove !== this.nodeCount - 1) {
            // Move the last node to this position
            const lastIndex = this.nodeCount - 1;
            const lastI3 = lastIndex * 3;
            const removeI3 = indexToRemove * 3;
            
            // Copy position
            this.positions[removeI3] = this.positions[lastI3];
            this.positions[removeI3 + 1] = this.positions[lastI3 + 1];
            this.positions[removeI3 + 2] = this.positions[lastI3 + 2];
            
            // Copy color
            this.colors[removeI3] = this.colors[lastI3];
            this.colors[removeI3 + 1] = this.colors[lastI3 + 1];
            this.colors[removeI3 + 2] = this.colors[lastI3 + 2];
            
            // Copy size
            this.sizes[indexToRemove] = this.sizes[lastIndex];
            
            // Find which node was at the last position
            let lastNodeId = null;
            for (const [id, index] of this.nodeIndices.entries()) {
                if (index === lastIndex) {
                    lastNodeId = id;
                    break;
                }
            }
            
            // Update the moved node's index
            if (lastNodeId) {
                this.nodeIndices.set(lastNodeId, indexToRemove);
            }
        }
        
        // Remove the node from tracking
        this.nodeIndices.delete(nodeId);
        this.nodeCount--;
        
        // Update draw range
        this.geometry.setDrawRange(0, this.nodeCount);
        
        // Mark attributes as needing update
        this.geometry.attributes.position.needsUpdate = true;
        this.geometry.attributes.color.needsUpdate = true;
        this.geometry.attributes.size.needsUpdate = true;
    }
    
    /**
     * Clear all nodes from the point cloud
     */
    clear() {
        this.nodeIndices.clear();
        this.nodeTypes.clear();
        this.nodeCount = 0;
        this.geometry.setDrawRange(0, 0);
        
        // Reset state tracking
        this.selectedIndices.clear();
        this.neighborIndices.clear();
        this.hoverIndex = -1;
    }
    
    /**
     * Update all positions from the node objects
     * @param {Map} nodes - Map of nodes with current positions
     */
    updateAllPositions(nodes) {
        // Update all node positions from data
        for (const [nodeId, node] of nodes.entries()) {
            if (this.nodeIndices.has(nodeId) && node.x !== undefined && node.y !== undefined) {
                const index = this.nodeIndices.get(nodeId);
                const i3 = index * 3;
                
                this.positions[i3] = node.x;
                this.positions[i3 + 1] = node.y;
            }
        }
        
        // Mark position attribute as needing update
        this.geometry.attributes.position.needsUpdate = true;
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
        
        // Initialize spatial partitioning
        this.spatialGrid = null;
        this.useSpatialIndex = true;
        this.gridUpdateInterval = 30; // Update grid every 30 frames
        this.gridUpdateCounter = 0;
        this.gridCellSize = 200; // Size of each grid cell
        
        // Initialize the simulation
        this.initSimulation();
        this.initSpatialIndex();
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
            .force('charge', d3.forceManyBody().strength(-15))
            .force('center', d3.forceCenter(0, 0))
            .force('collision', d3.forceCollide().radius(collisionRadius))
            .force('x', d3.forceX().strength(0.001))
            .force('y', d3.forceY().strength(0.005))
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
        this.simulation.alphaDecay(0.0228);  // Slower decay (about 0.5% cooling per tick)
        
        // Lower minimum alpha threshold (default is 0.001)
        this.simulation.alphaMin(0.001);  // Lower threshold for stopping
        
        // Reduce velocity decay for more momentum (default is 0.4)
        this.simulation.velocityDecay(0.1);
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
        
        // Update spatial grid periodically
        if (this.useSpatialIndex) {
            this.updateSpatialGrid();
        }
    }
    
    /**
     * Update positions of all nodes and links
     */
    updatePositions() {
        // Check if we have a NodeCloud available
        if (this.graphController && this.graphController.nodeCloud) {
            // Bulk update the NodeCloud for better performance
            this.graphController.nodeCloud.updateAllPositions(this.dataManager.graphObjects.nodes);
            
            // Update virtual objects for compatibility with other systems
            this.dataManager.graphObjects.nodes.forEach(node => {
                if (node.object && node.object.position) {
                    node.object.position.x = node.x || 0;
                    node.object.position.y = node.y || 0;
                }
                
                // Update labels separately
                if (node.labelObject) {
                    this.graphObjectManager.updateNodePosition(node);
                }
            });
        }
        
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
    
    /**
     * Initialize spatial index using a simple grid system
     */
    initSpatialIndex() {
        this.spatialGrid = new SpatialGrid(this.gridCellSize);
    }
    
    /**
     * Add a node to the spatial grid
     * @param {Object} node - The node to add
     */
    addNodeToSpatialGrid(node) {
        if (!this.useSpatialIndex || !this.spatialGrid || !node.object) return;
        
        // Add node to the grid
        this.spatialGrid.addObject(node);
    }
    
    /**
     * Update the spatial grid with current node positions
     */
    updateSpatialGrid() {
        if (!this.useSpatialIndex || !this.spatialGrid) return;
        
        // Only update periodically for performance
        this.gridUpdateCounter++;
        if (this.gridUpdateCounter < this.gridUpdateInterval) return;
        this.gridUpdateCounter = 0;
        
        // Rebuild the grid
        this.spatialGrid.clear();
        
        // Add all current nodes to the grid
        this.dataManager.graphObjects.nodes.forEach(node => {
            if (node.object) {
                this.spatialGrid.addObject(node);
            }
        });
    }
    
    /**
     * Get nodes within a specific radius of a position
     * @param {THREE.Vector3} position - The center position
     * @param {number} radius - The radius to search within
     * @returns {Array} Array of nodes within the radius
     */
    getNodesInRadius(position, radius) {
        // Use spatial grid for more efficient spatial query
        return this.spatialGrid.findNearbyObjects(position, radius);
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
        
        // Add click handler for the Load All Nodes button
        document.getElementById('load-all-btn')?.addEventListener('click', () => {
            this.graphController.loadAllNodes();
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
        
        // Store the current node ID for the Get Data button
        this.currentNodeId = nodeId;
        
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
        
        // Only show the Get Data button for composite nodes where the ID doesn't start with "data/"
        if (nodeType === 'composite' && !nodeId.startsWith('data/')) {
            html += `
                <button id="get-node-data" class="node-data-btn" style="
                    background-color: #4D90FE;
                    color: white;
                    border: none;
                    padding: 6px 12px;
                    margin: 10px 0;
                    border-radius: 4px;
                    cursor: pointer;
                    font-size: 13px;
                    display: flex;
                    align-items: center;
                    justify-content: center;
                    width: 100%;
                ">
                    <i class="fas fa-download" style="margin-right: 6px;"></i> 
                    Get Node Data
                </button>
                <div id="node-data-result" style="display: none; margin-top: 10px; max-height: 200px; overflow-y: auto;"></div>
            `;
        }
        
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
        
        // Add event listener for the Get Data button
        const getDataBtn = document.getElementById('get-node-data');
        if (getDataBtn) {
            getDataBtn.addEventListener('click', () => this.fetchNodeData(nodeId));
        }
        
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
     * Fetch data for a specific node from the server
     * @param {string} nodeId - The ID of the node to fetch data for
     */
    fetchNodeData(nodeId) {
        // Get the result container
        const resultContainer = document.getElementById('node-data-result');
        if (!resultContainer) return;
        
        // Show loading indicator
        resultContainer.style.display = 'block';
        resultContainer.innerHTML = `
            <div style="display: flex; align-items: center; justify-content: center; padding: 20px;">
                <div style="border: 3px solid #f3f3f3; border-top: 3px solid #3498db; 
                    border-radius: 50%; width: 20px; height: 20px; animation: spin 1s linear infinite;"></div>
                <div style="margin-left: 10px;">Loading data...</div>
            </div>
            <style>
                @keyframes spin {
                    0% { transform: rotate(0deg); }
                    100% { transform: rotate(360deg); }
                }
            </style>
        `;
        
        // Construct the URL for the data endpoint
        const url = `/${nodeId}`;
        
        // Fetch the data
        fetch(url)
            .then(response => {
                if (!response.ok) {
                    throw new Error(`HTTP error ${response.status}`);
                }
                return response.text(); // Using text() instead of json() to handle any type of response
            })
            .then(data => {
                // Try to parse as JSON if possible
                try {
                    const jsonData = JSON.parse(data);
                    this.displayNodeData(jsonData, resultContainer);
                } catch (e) {
                    // If not JSON, display as text
                    this.displayNodeData(data, resultContainer, false);
                }
            })
            .catch(error => {
                // Show error message
                resultContainer.innerHTML = `
                    <div style="color: #e74c3c; padding: 10px; border-left: 3px solid #e74c3c;">
                        Error loading data: ${error.message}
                    </div>
                `;
            });
    }
    
    /**
     * Display node data in the result container
     * @param {Object|string} data - The data to display
     * @param {HTMLElement} container - The container to display the data in
     * @param {boolean} isJson - Whether the data is JSON
     */
    displayNodeData(data, container, isJson = true) {
        if (isJson) {
            // Format JSON for display
            const formattedJson = JSON.stringify(data, null, 2);
            container.innerHTML = `
                <div style="padding: 10px; background-color: #f9f9f9; border-radius: 4px; font-family: monospace; white-space: pre; overflow-x: auto;">
                    ${formattedJson.replace(/</g, '&lt;').replace(/>/g, '&gt;')}
                </div>
            `;
        } else {
            // Display as text
            container.innerHTML = `
                <div style="padding: 10px; background-color: #f9f9f9; border-radius: 4px; white-space: pre-wrap; overflow-x: auto; font-family: monospace;">
                    ${data.toString().replace(/</g, '&lt;').replace(/>/g, '&gt;')}
                </div>
            `;
        }
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
        
        // Set the scene manager on the NodeCloud for camera access
        this.graphController.nodeCloud.sceneManager = this.sceneManager;
        
        const nodeId = this.graphController.nodeCloud.findClosestNode(
            this.sceneManager.raycaster, 
            0.08  // Screen space threshold for clicks (0-1 normalized coordinates)
        );
        
        if (nodeId) {
            // We clicked on a node
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
        
        // Set the scene manager on the NodeCloud for camera access
        this.graphController.nodeCloud.sceneManager = this.sceneManager;
        
        const nodeId = this.graphController.nodeCloud.findClosestNode(
            this.sceneManager.raycaster, 
            0.08  // Screen space threshold for double-clicks (0-1 normalized coordinates)
        );
        
        if (nodeId) {
            // Double-clicked on a node - focus camera on this node
            this.focusOnNode(nodeId);
        }
    }
    
    /**
     * Handle mouse movement for hover effects
     * @param {MouseEvent} event - The mouse event
     */
    onMouseMove(event) {
        // Calculate mouse position
        this.sceneManager.updateMousePosition(event);
        
        // Set the scene manager on the NodeCloud for camera access
        this.graphController.nodeCloud.sceneManager = this.sceneManager;
        
        const nodeId = this.graphController.nodeCloud.findClosestNode(
            this.sceneManager.raycaster, 
            0.04  // Screen space threshold for hover (0-1 normalized coordinates)
        );
        
        if (nodeId) {
            // Hovering over a node
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
        
        // Update colors in bulk
        this.graphController.nodeCloud.updateColors(
            nodeId,
            this.dataManager.neighborNodes,
            this.dataManager.hoveredNode
        );
        
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
        
        // Update colors in bulk
        this.graphController.nodeCloud.updateColors(
            null,  // No selected node
            new Set(),  // No neighbor nodes
            this.dataManager.hoveredNode  // Keep hover state
        );
        
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
        
        // Update colors in bulk
        this.graphController.nodeCloud.updateColors(
            this.dataManager.selectedNode,
            this.dataManager.neighborNodes,
            nodeId
        );
        
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
        
        // Update colors in bulk
        this.graphController.nodeCloud.updateColors(
            this.dataManager.selectedNode,
            this.dataManager.neighborNodes,
            null  // No hover
        );
        
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
 * DebugVisualizer - Generic visualization for debugging graph components
 */
class DebugVisualizer {
    constructor(sceneManager, graphController) {
        this.sceneManager = sceneManager;
        this.graphController = graphController;
        this.debugObjects = [];
        this.enabled = false;
        this.lastUpdateTime = 0;
        this.updateInterval = 1000; // Update debug visuals every second
        this.activeVisualizations = {
            grid: true,
            performance: true,
            nodes: true
        };
        this.stats = {};
    }
    
    /**
     * Toggle debug visualization
     * @param {boolean} enabled - Whether to enable or disable visualization
     */
    toggle(enabled) {
        this.enabled = enabled;
        
        // Show or hide debug UI elements
        const debugPanel = document.getElementById('debug-info-panel');
        const frameGraph = document.getElementById('debug-frame-graph');
        
        if (debugPanel) {
            debugPanel.style.display = enabled ? 'block' : 'none';
        }
        
        if (frameGraph) {
            frameGraph.style.display = enabled ? 'block' : 'none';
        }
        
        if (enabled) {
            // Initialize frame history if needed
            if (!this.frameHistory) {
                const canvas = document.getElementById('debug-frame-canvas');
                if (canvas) {
                    this.frameHistory = new Array(canvas.width).fill(0);
                }
            }
            
            this.createDebugVisualization();
        } else {
            this.clearDebugVisualization();
        }
    }
    
    /**
     * Toggle specific visualization types
     * @param {string} type - Visualization type to toggle
     */
    toggleVisualization(type) {
        if (this.activeVisualizations.hasOwnProperty(type)) {
            this.activeVisualizations[type] = !this.activeVisualizations[type];
            if (this.enabled) {
                this.createDebugVisualization();
            }
        }
    }
    
    /**
     * Create visual representation of debug data
     */
    createDebugVisualization() {
        this.clearDebugVisualization();
        
        // Collect debug stats
        this.collectStats();
        
        // Create visualizations based on active settings
        if (this.activeVisualizations.grid) {
            this.createSpatialGridVisualization();
        }
        
        if (this.activeVisualizations.nodes) {
            this.createNodeStatsVisualization();
        }
        
        if (this.activeVisualizations.performance) {
            this.createPerformanceVisualization();
        }
        
        // Create debug panel with statistics
        this.createDebugPanel();
        
        this.lastUpdateTime = performance.now();
    }
    
    /**
     * Collect statistics for debug display
     */
    collectStats() {
        // Clear previous stats
        this.stats = {
            fps: this.graphController.fps || 0,
            nodeCount: 0,
            visibleNodeCount: 0,
            linkCount: 0,
            gridStats: {
                cells: 0,
                objects: 0,
                avgPerCell: 0
            },
            cameraPosition: {
                x: 0,
                y: 0,
                z: 0
            },
            performanceMode: this.graphController.performanceMode
        };
        
        // Collect node and link stats
        if (this.graphController.dataManager) {
            const dataManager = this.graphController.dataManager;
            
            this.stats.nodeCount = dataManager.graphData.nodes.length;
            this.stats.visibleNodeCount = dataManager.graphObjects.nodes.size;
            this.stats.linkCount = dataManager.graphObjects.links.size;
        }
        
        // Collect grid stats
        if (this.graphController.simulationManager && 
            this.graphController.simulationManager.spatialGrid) {
            
            const grid = this.graphController.simulationManager.spatialGrid;
            this.stats.gridStats.cells = grid.grid.size;
            this.stats.gridStats.objects = grid.objects.size;
            
            if (grid.grid.size > 0 && grid.objects.size > 0) {
                this.stats.gridStats.avgPerCell = 
                    (grid.objects.size / grid.grid.size).toFixed(1);
            }
        }
        
        // Collect camera position
        if (this.graphController.sceneManager && this.graphController.sceneManager.camera) {
            const camera = this.graphController.sceneManager.camera;
            this.stats.cameraPosition.x = camera.position.x.toFixed(1);
            this.stats.cameraPosition.y = camera.position.y.toFixed(1);
            this.stats.cameraPosition.z = camera.position.z.toFixed(1);
        }
    }
    
    /**
     * Create spatial grid visualization
     */
    createSpatialGridVisualization() {
        const spatialGrid = this.graphController.simulationManager?.spatialGrid;
        if (!spatialGrid) return;
        
        // Create wireframe boxes for each cell in the grid
        spatialGrid.grid.forEach((cell, key) => {
            const [x, y, z] = key.split(',').map(Number);
            const cellSize = spatialGrid.cellSize;
            
            // Create box geometry for the cell
            const geometry = new THREE.BoxGeometry(cellSize, cellSize, cellSize);
            const material = new THREE.MeshBasicMaterial({
                color: 0x00ff00,
                wireframe: true,
                transparent: true,
                opacity: 0.05 + (0.05 * Math.min(cell.size, 10)) // Brighter for more populated cells
            });
            
            const box = new THREE.Mesh(geometry, material);
            box.position.set(
                (x + 0.5) * cellSize,
                (y + 0.5) * cellSize,
                (z + 0.5) * cellSize
            );
            
            this.sceneManager.addToScene(box);
            this.debugObjects.push(box);
            
            // Add text label showing object count in cell
            if (cell.size > 0) {
                const text = new SpriteText(`${cell.size}`, 12);
                text.color = '#ffff00';
                text.backgroundColor = 'rgba(0,0,0,0.5)';
                text.padding = 2;
                text.position.copy(box.position);
                this.sceneManager.addToScene(text);
                this.debugObjects.push(text);
            }
        });
    }
    
    /**
     * Create node statistics visualization
     */
    createNodeStatsVisualization() {
        // Highlight nodes with different colors based on properties
        const nodeManager = this.graphController.dataManager;
        const nodeCloud = this.graphController.nodeCloud;
        
        if (!nodeManager || !nodeCloud || !nodeCloud.colors) return;
        
        // Store original colors to restore later
        this.originalColors = new Float32Array(nodeCloud.colors.length);
        this.originalColors.set(nodeCloud.colors); // Make a copy of all colors
        
        // Iterate through nodes and update colors in the buffer
        nodeManager.graphObjects.nodes.forEach((node, id) => {
            if (nodeCloud.nodeIndices.has(id)) {
                // Get the node's index in the color buffer
                const index = nodeCloud.nodeIndices.get(id);
                const i3 = index * 3;
                
                // Get connection count
                const connectedLinks = nodeManager.getConnectedLinks(id);
                const connectionCount = connectedLinks.length;
                
                // Set color based on connection count
                let color;
                if (connectionCount > 10) {
                    color = new THREE.Color(0xff0000); // Red for highly connected
                } else if (connectionCount > 5) {
                    color = new THREE.Color(0xff8800); // Orange for medium
                } else if (connectionCount > 2) {
                    color = new THREE.Color(0xffff00); // Yellow for low
                } else {
                    color = new THREE.Color(0x00ffff); // Cyan for minimal
                }
                
                // Update the color buffer directly
                nodeCloud.colors[i3] = color.r;
                nodeCloud.colors[i3 + 1] = color.g;
                nodeCloud.colors[i3 + 2] = color.b;
            }
        });
        
        // Mark the color buffer as needing update
        if (nodeCloud.geometry && nodeCloud.geometry.attributes.color) {
            nodeCloud.geometry.attributes.color.needsUpdate = true;
        }
    }
    
    /**
     * Create performance metrics visualization
     */
    createPerformanceVisualization() {
        // Update frame history and redraw
        this.updateFrameGraph();
    }
    
    /**
     * Update the frame rate graph
     */
    updateFrameGraph() {
        const canvas = document.getElementById('debug-frame-canvas');
        const fpsLabel = document.getElementById('debug-fps-label');
        
        if (!canvas || !fpsLabel) return;
        
        // Update FPS label
        fpsLabel.textContent = `${this.stats.fps} FPS`;
        
        // Add current FPS to history
        if (!this.frameHistory) {
            this.frameHistory = new Array(canvas.width).fill(0);
        }
        
        this.frameHistory.push(this.stats.fps);
        this.frameHistory.shift();
        
        // Draw frame history
        const ctx = canvas.getContext('2d');
        const width = canvas.width;
        const height = canvas.height;
        
        // Clear canvas
        ctx.clearRect(0, 0, width, height);
        
        // Calculate scale - find max FPS in history for scaling
        const maxFPS = Math.max(60, ...this.frameHistory);
        const scale = height / maxFPS;
        
        // Draw background grid
        ctx.strokeStyle = '#333';
        ctx.lineWidth = 0.5;
        
        // Draw horizontal grid lines at 15, 30, 45, 60 FPS
        [15, 30, 45, 60].forEach(fps => {
            const y = height - (fps * scale);
            if (y >= 0 && y <= height) {
                ctx.beginPath();
                ctx.moveTo(0, y);
                ctx.lineTo(width, y);
                ctx.stroke();
            }
        });
        
        // Draw FPS graph
        ctx.strokeStyle = '#4CAF50';
        ctx.lineWidth = 1.5;
        ctx.beginPath();
        
        // Start at bottom-left corner with 0 FPS
        ctx.moveTo(0, height);
        
        // Draw lines for each frame sample
        this.frameHistory.forEach((fps, x) => {
            const y = height - (fps * scale);
            ctx.lineTo(x, y);
        });
        
        // Finish at bottom-right corner
        ctx.lineTo(width - 1, height);
        ctx.closePath();
        
        // Fill gradient
        const gradient = ctx.createLinearGradient(0, 0, 0, height);
        gradient.addColorStop(0, 'rgba(76, 175, 80, 0.7)');
        gradient.addColorStop(1, 'rgba(76, 175, 80, 0.1)');
        ctx.fillStyle = gradient;
        ctx.fill();
        
        // Stroke the line on top of the fill
        ctx.strokeStyle = '#4CAF50';
        ctx.lineWidth = 1.5;
        ctx.beginPath();
        
        this.frameHistory.forEach((fps, x) => {
            const y = height - (fps * scale);
            if (x === 0) {
                ctx.moveTo(x, y);
            } else {
                ctx.lineTo(x, y);
            }
        });
        
        ctx.stroke();
    }
    
    /**
     * Create debug panel with statistics
     */
    createDebugPanel() {
        // Update debug panel content using the existing HTML element
        document.getElementById('debug-nodes').textContent = `${this.stats.visibleNodeCount}/${this.stats.nodeCount}`;
        document.getElementById('debug-links').textContent = `${this.stats.linkCount}`;
        document.getElementById('debug-cells').textContent = `${this.stats.gridStats.cells}`;
        document.getElementById('debug-objects').textContent = `${this.stats.gridStats.objects}`;
        document.getElementById('debug-avg-per-cell').textContent = `${this.stats.gridStats.avgPerCell}`;
        
        // Update camera position
        document.getElementById('debug-camera-x').textContent = `${this.stats.cameraPosition.x}`;
        document.getElementById('debug-camera-y').textContent = `${this.stats.cameraPosition.y}`;
        document.getElementById('debug-camera-z').textContent = `${this.stats.cameraPosition.z}`;
    }
    
    /**
     * Remove all debug visualization objects
     */
    clearDebugVisualization() {
        // Remove all debug visualization objects from scene
        this.debugObjects.forEach(obj => {
            this.sceneManager.removeFromScene(obj);
        });
        this.debugObjects = [];
        
        // Restore original node colors
        if (this.originalColors && this.graphController.nodeCloud) {
            const nodeCloud = this.graphController.nodeCloud;
            
            // Copy the original colors back to the nodeCloud color buffer
            if (nodeCloud.colors && this.originalColors.length === nodeCloud.colors.length) {
                nodeCloud.colors.set(this.originalColors);
                
                // Mark the color buffer as needing update
                if (nodeCloud.geometry && nodeCloud.geometry.attributes.color) {
                    nodeCloud.geometry.attributes.color.needsUpdate = true;
                }
            }
            
            this.originalColors = null;
        }
    }
    
    /**
     * Update debug visualization
     */
    update() {
        if (!this.enabled) return;
        
        // Update stats more frequently than full visualization refresh
        this.collectStats();
        
        // Update FPS graph and info panel more frequently
        if (this.activeVisualizations.performance && document.getElementById('debug-frame-canvas')) {
            this.updateFrameGraph();
        }
        
        // Update info panel if it exists
        if (document.getElementById('debug-info-panel')) {
            this.createDebugPanel(); // Updates panel content
        }
        
        // Only update full visualization periodically to avoid performance impact
        const now = performance.now();
        if (now - this.lastUpdateTime < this.updateInterval) return;
        
        // Recreate visualization
        this.createDebugVisualization();
    }
    
    /**
     * Handle keyboard shortcuts for debugging
     * @param {KeyboardEvent} event - Keyboard event
     */
    handleKeyPress(event) {
        if (!this.enabled) return;
        
        switch (event.key) {
            case '1':
                this.toggleVisualization('grid');
                break;
            case '2':
                this.toggleVisualization('nodes');
                break;
            case '3':
                this.toggleVisualization('performance');
                break;
        }
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
        
        // Create the node cloud for efficient node rendering
        this.nodeCloud = new NodeCloud(this.sceneManager.scene, this.themeManager);
        
        this.simulationManager = new SimulationManager(this.dataManager, this.graphObjectManager, this.themeManager);
        this.simulationManager.graphController = this; // Add reference to this controller
        
        this.uiManager = new UIManager(this.container, this.dataManager, this);
        this.eventManager = new EventManager(this.sceneManager, this.dataManager, this.graphObjectManager);
        this.eventManager.graphController = this; // Add reference to this controller
        
        // Performance and debug settings
        this.performanceMode = true; // Always on
        this.debugMode = false;
        
        // Initialize generic debugger
        this.debugger = new DebugVisualizer(this.sceneManager, this);
        
        // Initialize FPS counter
        this.fpsCounter = document.getElementById('fps-counter');
        this.frameCount = 0;
        this.lastTime = performance.now();
        this.fps = 0;
        this.fpsUpdateInterval = 500; // Update FPS display every 500ms
        
        // Set up UI button handlers
        this.setupButtonHandlers();
        
        // Setup keyboard listeners for debug controls
        document.addEventListener('keydown', this.handleKeyPress.bind(this));
        
        // Store a global reference for convenience (used by NodeCloud)
        window.lastGraphController = this;
        
        // Load data
        this.loadGraphData();
        
        // Always enable performance optimizations
        this.enablePerformanceMode();
        
        // Start animation loop
        this.animate();
    }
    
    /**
     * Set up handlers for UI buttons
     */
    setupButtonHandlers() {
        // Debug mode button
        const debugBtn = document.getElementById('toggle-debug-btn');
        if (debugBtn) {
            debugBtn.addEventListener('click', () => {
                this.toggleDebugMode();
                debugBtn.classList.toggle('active', this.debugMode);
            });
        }
    }
    
    /**
     * Enable performance optimizations
     */
    enablePerformanceMode() {
        // Enable spatial grid and frustum culling
        if (this.simulationManager) {
            this.simulationManager.useSpatialIndex = true;
        }
        if (this.sceneManager) {
            this.sceneManager.enableFrustumCulling = true;
        }
    }
    
    /**
     * Handle keyboard shortcuts
     * @param {KeyboardEvent} event - Keyboard event
     */
    handleKeyPress(event) {
        // Pass to debugger if debug mode is on
        if (this.debugMode && this.debugger) {
            this.debugger.handleKeyPress(event);
        }
    }
    
    /**
     * Toggle debug visualization mode
     */
    toggleDebugMode() {
        this.debugMode = !this.debugMode;
        
        if (this.debugger) {
            this.debugger.toggle(this.debugMode);
        }
        
        return this.debugMode;
    }
    
    /**
     * Animation loop
     */
    animate() {
        requestAnimationFrame(() => this.animate());
        
        // Update FPS calculation
        this.frameCount++;
        const currentTime = performance.now();
        const elapsed = currentTime - this.lastTime;
        
        // Update FPS counter every interval
        if (elapsed > this.fpsUpdateInterval) {
            this.fps = Math.round((this.frameCount * 1000) / elapsed);
            this.fpsCounter.textContent = `FPS: ${this.fps}`;
            
            // Reset counters
            this.frameCount = 0;
            this.lastTime = currentTime;
        }
        
        // Update debug visualization if enabled
        if (this.debugMode && this.debugger) {
            this.debugger.update();
        }
        
        // Update scene
        this.sceneManager.update();
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
        
        // Add each matching node and its connections with depth of 10
        const addedNodes = new Set();
        
        matchingNodeIds.forEach(nodeId => {
            // Use getConnectedSubgraph to get nodes and links up to depth 10
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
     * Load all nodes in the graph
     */
    loadAllNodes() {
        // Clear current display
        this.clearDisplay();
        
        // Hide the initial message
        this.uiManager.hideInitialMessage();
        
        // Show loading indicator
        this.uiManager.showLoading(true);
        
        console.log(`Loading all ${this.dataManager.graphData.nodes.length} nodes`);
        
        // Store all added node IDs
        const addedNodes = new Set();
        
        // Add all nodes to the scene
        this.dataManager.graphData.nodes.forEach(node => {
            if (!this.dataManager.graphObjects.nodes.has(node.id)) {
                this.graphObjectManager.createNodeObject(node);
                addedNodes.add(node.id);
            }
        });
        
        // Add all links between the visible nodes
        this.dataManager.graphData.links.forEach(link => {
            const sourceId = typeof link.source === 'object' ? link.source.id : link.source;
            const targetId = typeof link.target === 'object' ? link.target.id : link.target;
            const linkId = `${sourceId}-${targetId}`;
            
            // Only add links between nodes that are visible
            if (addedNodes.has(sourceId) && addedNodes.has(targetId) && 
                !this.dataManager.graphObjects.links.has(linkId)) {
                this.graphObjectManager.createLinkObject(link);
            }
        });
        
        // Update simulation and restart it
        this.simulationManager.updateSimulation(true);
        
        // Center the view on all nodes
        this.centerOnNodes(Array.from(addedNodes));
        
        // Hide loading indicator
        this.uiManager.showLoading(false);
    }
}

// Initialize the application when the page loads
document.addEventListener('DOMContentLoaded', () => {
    new GraphController('graph-container');
}); 