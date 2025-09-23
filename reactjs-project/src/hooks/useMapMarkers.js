import { useEffect } from 'react';
import mapboxgl from 'mapbox-gl';
export const useMapMarkers = (map, tracking, selectedVehicle, onMarkerClick, isMapLoaded) => {
    useEffect(() => {
        console.log('useMapMarkers called with:', {
            map: !!map,
            trackingLength: tracking?.length || 0,
            selectedVehicle,
            isMapLoaded
        });
        if (!map || !tracking || tracking.length === 0) {
            console.log('Exiting early - map or tracking missing:', { map: !!map, tracking: tracking?.length || 0 });
            return;
        }
        // Skip all style checking if we know the map is loaded from the hook
        if (!isMapLoaded) {
            console.log('Map not fully loaded according to hook, waiting...');
            return;
        }
        // Simplified marker creation - since we know the map is loaded
        const addMarkers = () => {
            // Basic container check
            const container = map.getContainer();
            if (!container) {
                console.log('Map container not ready, will retry in next effect cycle');
                return;
            }
            // Basic canvas check
            const canvas = map.getCanvas();
            if (!canvas) {
                console.log('Map canvas not ready, will retry in next effect cycle');
                return;
            }
            // Clear existing markers safely
            const existingMarkers = document.querySelectorAll('.mapboxgl-marker');
            if (existingMarkers.length > 0) {
                existingMarkers.forEach(marker => {
                    try {
                        // Use the safer remove method
                        marker.remove();
                    }
                    catch (error) {
                        console.warn('Error removing existing marker:', error);
                    }
                });
                console.log('Cleared', existingMarkers.length, 'existing markers');
                // Give DOM a moment to settle after cleanup
                setTimeout(() => createNewMarkers(), 50);
            }
            else {
                createNewMarkers();
            }
        };
        const createNewMarkers = () => {
            console.log('Creating markers for', tracking.length, 'vehicles');
            tracking.forEach((point) => {
                try {
                    // Validate point data
                    if (!point || typeof point.vehicleId === 'undefined' ||
                        typeof point.latitude !== 'number' || typeof point.longitude !== 'number') {
                        console.warn('Invalid tracking point:', point);
                        return;
                    }
                    const el = document.createElement('div');
                    if (!el) {
                        console.error('Failed to create marker element');
                        return;
                    }
                    el.className = 'block border-0 cursor-pointer p-0 z-50';
                    const innerDiv = document.createElement('div');
                    if (!innerDiv) {
                        console.error('Failed to create inner div');
                        return;
                    }
                    const isSelected = selectedVehicle === point.vehicleId;
                    innerDiv.className = `px-2 py-1 rounded-full text-xs font-bold shadow-lg transition-all duration-300 ${isSelected
                        ? 'bg-orange-500 text-white ring-2 ring-orange-300 scale-110'
                        : 'bg-blue-500 text-white hover:bg-blue-600'}`;
                    innerDiv.textContent = `${point.vehicleId}`;
                    el.addEventListener('click', () => {
                        onMarkerClick(point);
                    });
                    el.appendChild(innerDiv);
                    // Validate coordinates before creating marker
                    const longitude = Number(point.longitude);
                    const latitude = Number(point.latitude);
                    if (isNaN(longitude) || isNaN(latitude)) {
                        console.error('Invalid coordinates for vehicle', point.vehicleId, { longitude, latitude });
                        return;
                    }
                    // Create marker with additional safety checks
                    const marker = new mapboxgl.Marker(el);
                    if (marker && map) {
                        marker.setLngLat([longitude, latitude]).addTo(map);
                        console.log(`Created marker for vehicle ${point.vehicleId} at [${longitude}, ${latitude}]`);
                    }
                }
                catch (error) {
                    console.error('Error creating marker for vehicle', point?.vehicleId, ':', error);
                }
            });
        };
        // Simply call addMarkers since we know the map is loaded
        console.log('Map is loaded, creating markers immediately');
        addMarkers();
        // Cleanup function - improved to prevent DOM conflicts
        return () => {
            // Clean up markers when component unmounts or dependencies change
            try {
                // Use a more targeted approach to find our markers
                const existingMarkers = document.querySelectorAll('.mapboxgl-marker');
                console.log('Cleanup: Found', existingMarkers.length, 'markers to remove');
                existingMarkers.forEach((marker, index) => {
                    try {
                        // Add a small delay between removals to prevent DOM conflicts
                        setTimeout(() => {
                            if (marker && marker.parentNode) {
                                marker.remove();
                            }
                        }, index * 10); // Staggered removal
                    }
                    catch (error) {
                        console.warn('Error removing marker during cleanup:', error);
                    }
                });
            }
            catch (error) {
                console.warn('Error during marker cleanup:', error);
            }
        };
    }, [map, tracking, selectedVehicle, onMarkerClick, isMapLoaded]);
};
export default useMapMarkers;
