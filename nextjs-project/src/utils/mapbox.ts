/**
 * Interface cho kết quả từ Mapbox Directions API
 */
export interface MapboxRoute {
  routes: Array<{
    geometry: {
      coordinates: [number, number][];
    };
  }>;
}

/**
 * Lấy route từ Mapbox Directions API
 */
export const getMapboxRoute = async (
  startLng: number,
  startLat: number,
  endLng: number,
  endLat: number
): Promise<[number, number][]> => {
  const MAPBOX_TOKEN = process.env.NEXT_PUBLIC_MAPBOX_ACCESS_TOKEN || 
    "pk.eyJ1IjoieHVhbmh1eTEiLCJhIjoiY21lN3liN21tMDlzaTJtbXF3MjU0Z2JzaSJ9.vmH3qH_f7qf1ewBC_pJoSg";
  
  const url = `https://api.mapbox.com/directions/v5/mapbox/driving/${startLng},${startLat};${endLng},${endLat}?geometries=geojson&access_token=${MAPBOX_TOKEN}`;
  
  const response = await fetch(url);
  
  if (!response.ok) {
    throw new Error("Không lấy được route từ Mapbox");
  }
  
  const data: MapboxRoute = await response.json();
  
  if (!data.routes || !data.routes[0]) {
    throw new Error("Không có route trả về");
  }
  
  return data.routes[0].geometry.coordinates;
};
