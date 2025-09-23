import { useEffect } from "react";
import mapboxgl from 'mapbox-gl';
export const addMarkersToMap = (map, trackingPoints, onMarkerClick) => {
    // Check if map is ready
    if (!map || !map.getContainer()) {
        console.warn('Map not ready for markers');
        return;
    }
    // Clear existing markers
    const existingMarkers = document.querySelectorAll('.custom-marker');
    existingMarkers.forEach(marker => marker.remove());
    // Add new markers
    trackingPoints.forEach((point) => {
        try {
            const el = document.createElement('div');
            el.className = 'custom-marker';
            el.innerHTML = `
        <div class="bg-blue-500 text-white px-2 py-1 rounded-full text-xs font-bold shadow-lg cursor-pointer hover:bg-blue-600 transition-colors">
          ${point.vehicleId}
        </div>
      `;
            // Add click event for marker info
            el.addEventListener('click', () => {
                // Call custom click handler if provided
                if (onMarkerClick) {
                    onMarkerClick(point);
                    return;
                }
                // Default popup behavior
                new mapboxgl.Popup({
                    closeButton: true,
                    closeOnClick: false,
                    maxWidth: '300px'
                })
                    .setLngLat([point.longitude, point.latitude])
                    .setHTML(`
            <div class="p-3">
              <h3 class="font-semibold text-gray-800 mb-2">Vehicle ${point.vehicleId}</h3>
              <div class="space-y-1 text-sm">
                <div class="flex justify-between">
                  <span class="text-gray-600">Status:</span>
                  <span class="font-medium text-blue-600">${point.status}</span>
                </div>
                <div class="flex justify-between">
                  <span class="text-gray-600">Location:</span>
                  <span class="text-gray-800">${point.latitude.toFixed(4)}, ${point.longitude.toFixed(4)}</span>
                </div>
                <div class="flex justify-between">
                  <span class="text-gray-600">Updated:</span>
                  <span class="text-gray-800">${new Date(point.timestamp).toLocaleString('vi-VN')}</span>
                </div>
              </div>
            </div>
          `)
                    .addTo(map);
            });
            new mapboxgl.Marker(el)
                .setLngLat([point.longitude, point.latitude])
                .addTo(map);
        }
        catch (error) {
            console.error('Error adding marker:', error);
        }
    });
};
export const useMapMarkers = (map, trackingPoints, onMarkerClick) => {
    useEffect(() => {
        if (!map || trackingPoints.length === 0)
            return;
        // Wait for map to be fully loaded
        if (map.loaded()) {
            addMarkersToMap(map, trackingPoints, onMarkerClick);
        }
        else {
            map.on('load', () => {
                addMarkersToMap(map, trackingPoints, onMarkerClick);
            });
        }
    }, [map, trackingPoints, onMarkerClick]);
};
