import React from "react";
interface BaseMapProps {
    accessToken: string;
    center: [number, number];
    zoom: number;
    showDebugInfo?: boolean;
    showVehicleList?: boolean;
    height?: string;
    className?: string;
    mapStyle?: string;
}
export declare const BaseMap: React.FC<BaseMapProps>;
export {};
