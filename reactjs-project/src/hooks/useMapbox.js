import { useEffect, useRef, useState } from "react";
import mapboxgl from 'mapbox-gl';
export const useMapbox = ({ accessToken, center, zoom, style = 'mapbox://styles/mapbox/streets-v11' }) => {
    const mapContainer = useRef(null);
    const map = useRef(null);
    const [isLoaded, setIsLoaded] = useState(false);
    const [mapInstance, setMapInstance] = useState(null);
    const [error, setError] = useState(null);
    useEffect(() => {
        if (map.current)
            return; // Initialize map only once
        console.log('useMapbox useEffect triggered');
        if (!accessToken || accessToken === "your_mapbox_token_here") {
            setError("Mapbox token is missing or invalid");
            return;
        }
        // Validate token format
        if (!accessToken.startsWith('pk.')) {
            console.warn('Invalid Mapbox token format - should start with pk.');
        }
        console.log('Using Mapbox token:', accessToken.substring(0, 20) + '...');
        // Capture the container reference for cleanup
        const containerRef = mapContainer.current;
        const initializeMap = () => {
            if (!mapContainer.current)
                return;
            // Check if container has dimensions
            const rect = mapContainer.current.getBoundingClientRect();
            if (rect.width === 0 || rect.height === 0) {
                console.log("Container has no dimensions, retrying...");
                setTimeout(initializeMap, 500);
                return;
            }
            console.log("Initializing map with token:", accessToken);
            console.log("Container dimensions:", mapContainer.current.offsetWidth, 'x', mapContainer.current.offsetHeight);
            // Clear the container to avoid Mapbox warning
            mapContainer.current.innerHTML = '';
            mapboxgl.accessToken = accessToken;
            try {
                map.current = new mapboxgl.Map({
                    container: mapContainer.current,
                    style,
                    center,
                    zoom,
                    attributionControl: false,
                    preserveDrawingBuffer: true,
                    failIfMajorPerformanceCaveat: false
                });
                console.log('Map instance created:', !!map.current);
                console.log('Style URL:', style);
                setMapInstance(map.current);
                // Add navigation control
                map.current.addControl(new mapboxgl.NavigationControl(), 'top-right');
                // Set timeout for style loading - fallback if events don't fire
                const styleTimeout = setTimeout(() => {
                    console.warn('Style loading timeout, trying to proceed anyway');
                    if (map.current) {
                        try {
                            console.log('Forcing map to be marked as loaded due to timeout');
                            setIsLoaded(true);
                            if (map.current.getCanvas()) {
                                map.current.resize();
                                console.log('Map forced to load after timeout');
                            }
                        }
                        catch (err) {
                            console.error('Error during timeout resize:', err);
                            // If there's still an issue, try to recreate with a simpler style
                            console.log('Attempting map recreation with simpler configuration');
                            try {
                                if (mapContainer.current) {
                                    map.current.remove();
                                    map.current = new mapboxgl.Map({
                                        container: mapContainer.current,
                                        style: 'mapbox://styles/mapbox/basic-v9', // Simpler fallback style
                                        center,
                                        zoom,
                                        attributionControl: false
                                    });
                                    setMapInstance(map.current);
                                    setIsLoaded(true);
                                    console.log('Map recreated with basic style');
                                }
                            }
                            catch (recreateErr) {
                                console.error('Failed to recreate map:', recreateErr);
                                setError('Failed to initialize map');
                            }
                        }
                    }
                }, 8000); // Increased timeout
                // Primary event - when style is fully loaded
                map.current.on('style.load', () => {
                    clearTimeout(styleTimeout);
                    console.log('Map style loaded successfully');
                    // Wait a bit more for DOM to settle
                    setTimeout(() => {
                        setIsLoaded(true);
                        if (map.current) {
                            map.current.resize();
                            console.log('Map resized after style load');
                        }
                    }, 200);
                });
                // Fallback event - when map is loaded
                map.current.on('load', () => {
                    clearTimeout(styleTimeout);
                    console.log('Map loaded successfully');
                    setTimeout(() => {
                        setIsLoaded(true);
                        if (map.current) {
                            map.current.resize();
                            console.log('Map resized after load');
                        }
                    }, 200);
                });
                // Style error handling
                map.current.on('styleimagemissing', (e) => {
                    console.warn('Style image missing:', e.id);
                });
                // Error handling
                map.current.on('error', (e) => {
                    clearTimeout(styleTimeout);
                    console.error('Map error:', e);
                    setError(`Map loading error: ${e.error?.message || 'Unknown error'}`);
                });
            }
            catch (error) {
                console.error('Error initializing map:', error);
                setError('Failed to initialize map');
            }
        };
        // Start initialization with a delay
        const timer = setTimeout(initializeMap, 200);
        return () => {
            clearTimeout(timer);
            if (map.current) {
                try {
                    // Stop all map events
                    map.current.stop();
                    // Remove all markers before removing map
                    const markers = document.querySelectorAll('.mapboxgl-marker');
                    markers.forEach(marker => {
                        try {
                            if (marker.parentNode) {
                                marker.remove();
                            }
                        }
                        catch (e) {
                            console.warn('Error removing marker during cleanup:', e);
                        }
                    });
                    // Clear the container before removing map to prevent React conflicts
                    if (containerRef) {
                        // Stop all map rendering
                        map.current.stop();
                        // Let Mapbox clean up first
                        map.current.remove();
                        // Give DOM time to settle before clearing
                        setTimeout(() => {
                            if (containerRef && containerRef.parentNode) {
                                try {
                                    containerRef.innerHTML = '';
                                }
                                catch (e) {
                                    console.warn('Container already cleared:', e);
                                }
                            }
                        }, 150);
                    }
                    else {
                        map.current.remove();
                    }
                    map.current = null;
                    setMapInstance(null);
                }
                catch (error) {
                    console.warn('Error during map cleanup:', error);
                }
            }
        };
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [accessToken]); // Only re-create map if token changes
    // Sync map instance state with ref
    useEffect(() => {
        if (map.current && !mapInstance) {
            console.log('Syncing map instance state with ref');
            setMapInstance(map.current);
        }
    }, [mapInstance]);
    return {
        mapContainer,
        map: mapInstance,
        isLoaded,
        error
    };
};
export default useMapbox;
