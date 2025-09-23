import { type RefObject } from 'react';
import mapboxgl from 'mapbox-gl';
export declare function useMapResize(map: mapboxgl.Map | null, mapContainer: RefObject<HTMLDivElement | null>): void;
export default useMapResize;
