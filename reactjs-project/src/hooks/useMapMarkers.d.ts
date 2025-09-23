import mapboxgl from 'mapbox-gl';
import type { TrackingPoint } from '../types';
export declare const useMapMarkers: (map: mapboxgl.Map | null, tracking: TrackingPoint[], selectedVehicle: number | null, onMarkerClick: (point: TrackingPoint) => void, isMapLoaded?: boolean) => void;
export default useMapMarkers;
