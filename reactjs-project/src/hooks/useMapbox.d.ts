import mapboxgl from 'mapbox-gl';
interface UseMapboxOptions {
    accessToken: string;
    center: [number, number];
    zoom: number;
    style?: string;
}
export declare const useMapbox: ({ accessToken, center, zoom, style }: UseMapboxOptions) => {
    mapContainer: import("react").RefObject<HTMLDivElement | null>;
    map: mapboxgl.Map | null;
    isLoaded: boolean;
    error: string | null;
};
export default useMapbox;
