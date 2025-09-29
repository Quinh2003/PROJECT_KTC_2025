import { useEffect, useRef, type RefObject } from 'react';
import mapboxgl from 'mapbox-gl';

export function useMapResize(
  map: mapboxgl.Map | null,
  mapContainer: RefObject<HTMLDivElement | null>
) {
  const resizeTimeoutRef = useRef<number | null>(null);

  useEffect(() => {
    if (!map || !mapContainer || !mapContainer.current) return;

    // Clear any existing timeout
    const clearResizeTimeout = () => {
      if (resizeTimeoutRef.current) {
        clearTimeout(resizeTimeoutRef.current);
        resizeTimeoutRef.current = null;
      }
    };

    // Safe resize function with all checks
    const safeResize = () => {
      if (!map || !mapContainer || !mapContainer.current) {
        console.log('Map or container not available for resize');
        return;
      }
      
      try {
        // Check container dimensions first
        const rect = mapContainer.current.getBoundingClientRect();
        if (rect.width <= 0 || rect.height <= 0) {
          console.log('Container has invalid dimensions, skipping resize');
          return;
        }

        // If style is not loaded, wait for it
        if (!map.isStyleLoaded()) {
          console.log('Map style not loaded, waiting...');
          map.once('styledata', () => {
            console.log('Style loaded, now resizing...');
            setTimeout(() => safeResize(), 100);
          });
          return;
        }
        
        // Check if map has a canvas
        const canvas = map.getCanvas();
        if (!canvas) {
          console.log('Map canvas not ready, waiting for load...');
          map.once('load', () => {
            console.log('Map loaded, now resizing...');
            setTimeout(() => safeResize(), 100);
          });
          return;
        }
        
        map.resize();
        console.log('Map resized successfully');
      } catch (error) {
        console.error('Error resizing map:', error);
      }
    };

    // Debounced resize function
    const debouncedResize = (delay: number = 200) => {
      clearResizeTimeout();
      resizeTimeoutRef.current = window.setTimeout(() => {
        safeResize();
      }, delay) as unknown as number;
    };

    const handleResize = () => {
      if (map && mapContainer && mapContainer.current) {
        const rect = mapContainer.current.getBoundingClientRect();
        if (rect.width > 0 && rect.height > 0) {
          debouncedResize(300);
        }
      }
    };

    const handleVisibilityChange = () => {
      if (!document.hidden && map && mapContainer && mapContainer.current) {
        debouncedResize(500);
      }
    };

    const handleOrientationChange = () => {
      if (map && mapContainer && mapContainer.current) {
        setTimeout(() => {
          if (mapContainer.current) {
            const rect = mapContainer.current.getBoundingClientRect();
            if (rect.width > 0 && rect.height > 0) {
              debouncedResize(100);
            }
          }
        }, 800);
      }
    };

    window.addEventListener('resize', handleResize);
    document.addEventListener('visibilitychange', handleVisibilityChange);
    window.addEventListener('orientationchange', handleOrientationChange);
    
    let resizeObserver: ResizeObserver | null = null;
    
    if (mapContainer && mapContainer.current && 'ResizeObserver' in window) {
      resizeObserver = new ResizeObserver((entries) => {
        for (const entry of entries) {
          if (entry.contentRect.width > 0 && entry.contentRect.height > 0) {
            debouncedResize(200);
          }
        }
      });
      
      resizeObserver.observe(mapContainer.current);
    }

    return () => {
      clearResizeTimeout();
      window.removeEventListener('resize', handleResize);
      document.removeEventListener('visibilitychange', handleVisibilityChange);
      window.removeEventListener('orientationchange', handleOrientationChange);
      if (resizeObserver) {
        resizeObserver.disconnect();
      }
    };
  }, [map, mapContainer]);
}

export default useMapResize;
