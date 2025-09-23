import mapboxgl from 'mapbox-gl';
import type { TrackingPoint } from "../types";
export declare const addMarkersToMap: (map: mapboxgl.Map, trackingPoints: TrackingPoint[], onMarkerClick?: (point: TrackingPoint) => void) => void;
export declare const useMapMarkers: (map: mapboxgl.Map | null, trackingPoints: TrackingPoint[], onMarkerClick?: (point: TrackingPoint) => void) => void;
