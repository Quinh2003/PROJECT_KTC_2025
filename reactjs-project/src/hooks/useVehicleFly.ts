import { useState, useCallback } from 'react';
import mapboxgl from 'mapbox-gl';
import type { TrackingPoint } from '../types';

export const useVehicleFly = (map: mapboxgl.Map | null) => {
  const [selectedVehicle, setSelectedVehicle] = useState<number | null>(null);

  const flyToVehicle = useCallback((point: TrackingPoint) => {
    if (!map) return;
    
    setSelectedVehicle(point.vehicleId);
    
    map.flyTo({
      center: [point.longitude, point.latitude],
      zoom: 15,
      speed: 1.2,
      curve: 1,
      easing: (t: number) => t,
    });
    
    setTimeout(() => {
      setSelectedVehicle(null);
    }, 3000);
  }, [map]);

  return {
    selectedVehicle,
    flyToVehicle
  };
};

export default useVehicleFly;
