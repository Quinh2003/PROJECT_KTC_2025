import React from "react";
interface MapContainerProps {
    mapRef: React.RefObject<HTMLDivElement | null>;
    height?: string;
    className?: string;
    style?: React.CSSProperties;
}
export declare const MapContainer: React.FC<MapContainerProps>;
export {};
