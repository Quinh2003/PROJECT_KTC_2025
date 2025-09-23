import mapboxgl from 'mapbox-gl';
import type { TrackingPoint } from '../types';
export declare const useVehicleFly: (map: mapboxgl.Map | null) => {
    selectedVehicle: number | null;
    flyToVehicle: (point: TrackingPoint) => void;
};
export default useVehicleFly;
