import { useState, useCallback } from 'react';
import mapboxgl from 'mapbox-gl';
export const useVehicleFly = (map) => {
    const [selectedVehicle, setSelectedVehicle] = useState(null);
    const flyToVehicle = useCallback((point) => {
        if (!map)
            return;
        setSelectedVehicle(point.vehicleId);
        map.flyTo({
            center: [point.longitude, point.latitude],
            zoom: 15,
            speed: 1.2,
            curve: 1,
            easing: (t) => t,
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
