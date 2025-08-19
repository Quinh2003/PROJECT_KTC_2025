import React from "react";

interface MapContainerProps {
  mapRef: React.RefObject<HTMLDivElement | null>;
  height?: string;
  className?: string;
  style?: React.CSSProperties;
}

export const MapContainer: React.FC<MapContainerProps> = ({ 
  mapRef, 
  height = "350px", 
  className = "", 
  style = {} 
}) => {
  return (
    <div 
      ref={mapRef}
      className={`mapbox-map-container w-full overflow-hidden ${className}`}
      style={{ 
        height,
        minHeight: height,
        position: 'relative',
        background: '#f8f9fa',
        ...style
      }}
    />
  );
<<<<<<< HEAD
};
=======
};
>>>>>>> 042a7c16d89d185c6e74a32de79f098e8a6971b5
