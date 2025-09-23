import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import React from "react";
import { useMapbox } from "../../hooks/useMapbox";
import useSimpleTracking from "../../hooks/useSimpleTracking";
import { useMapMarkers } from "../../utils/mapUtils";
import { MapContainer } from "./MapContainer";
import { VehicleList } from "../VehicleList";
export const BaseMap = ({ accessToken, center, zoom, showVehicleList = true, height = "350px", className = "", mapStyle }) => {
    const { mapContainer, map, error: mapError } = useMapbox({
        accessToken,
        center,
        zoom,
        style: mapStyle
    });
    const { tracking, error: trackingError } = useSimpleTracking();
    useMapMarkers(map, tracking);
    const error = mapError || trackingError;
    const loading = false; // useSimpleTracking doesn't have loading state
    if (loading && tracking.length === 0) {
        return (_jsx("div", { className: "bg-white rounded-xl shadow p-4 h-full min-h-[300px] flex items-center justify-center", children: _jsx("span", { className: "text-gray-400", children: "Loading tracking data..." }) }));
    }
    if (error) {
        return (_jsx("div", { className: "bg-white rounded-xl shadow p-4 h-full min-h-[300px] flex items-center justify-center", children: _jsx("div", { className: "text-red-500", children: error }) }));
    }
    return (_jsx("div", { className: `bg-white rounded-xl shadow overflow-hidden ${className}`, children: _jsxs("div", { className: "h-full", children: [_jsx(MapContainer, { mapRef: mapContainer, height: height }), showVehicleList && (_jsx("div", { className: "p-4 border-t", children: _jsx(VehicleList, { tracking: tracking, selectedVehicle: null, onVehicleClick: () => { } }) }))] }) }));
};
