import { jsx as _jsx } from "react/jsx-runtime";
import React from "react";
export const MapContainer = ({ mapRef, height = "350px", className = "", style = {} }) => {
    return (_jsx("div", { ref: mapRef, className: `mapbox-map-container w-full overflow-hidden ${className}`, style: {
            height,
            minHeight: height,
            position: 'relative',
            background: '#f8f9fa',
            ...style
        } }));
};
