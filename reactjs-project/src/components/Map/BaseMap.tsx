import React from "react";
import { useTranslation } from 'react-i18next';
import { useMapbox } from "../../hooks/useMapbox";
import useSimpleTracking from "../../hooks/useSimpleTracking";
import { useMapMarkers } from "../../utils/mapUtils";
import { MapContainer } from "./MapContainer";
import VehicleList from "../VehicleList";

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

export const BaseMap: React.FC<BaseMapProps> = ({
  accessToken,
  center,
  zoom,
  showVehicleList = true,
  height = "350px",
  className = "",
  mapStyle
}) => {
  const { t } = useTranslation();
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
    return (
      <div className="bg-white rounded-xl shadow p-4 h-full min-h-[300px] flex items-center justify-center">
        <span className="text-gray-400">{t('common.loading', 'Loading')}...</span>
      </div>
    );
  }

  if (error) {
    return (
      <div className="bg-white rounded-xl shadow p-4 h-full min-h-[300px] flex items-center justify-center">
        <div className="text-red-500">{error}</div>
      </div>
    );
  }

  return (
    <div className={`bg-white rounded-xl shadow overflow-hidden ${className}`}>
      <div className="h-full">
        <MapContainer 
          mapRef={mapContainer}
          height={height}
        />
        
        {showVehicleList && (
          <div className="p-4 border-t">
            <VehicleList 
              tracking={tracking} 
              selectedVehicle={null}
              onVehicleClick={() => {}}
            />
          </div>
        )}
        
      </div>
    </div>
  );
};
