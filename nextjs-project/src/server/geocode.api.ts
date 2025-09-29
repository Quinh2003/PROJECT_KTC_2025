// src/server/geocode.api.ts
// Hàm lấy tọa độ từ địa chỉ sử dụng Nominatim OpenStreetMap API (có thể tái sử dụng cho nhiều bảng)

export interface Coordinates {
  latitude: number | null;
  longitude: number | null;
}

/**
 * Lấy tọa độ (latitude, longitude) từ địa chỉ chuỗi (address)
 * @param address Địa chỉ cần geocode
 * @returns Promise<Coordinates>
 */

export const getCoordinatesFromAddress = async (address: string): Promise<Coordinates> => {
  if (!address.trim()) {
    return { latitude: null, longitude: null };
  }

  // Lấy token từ biến môi trường (cấu hình trong .env hoặc hệ thống)
  const MAPBOX_TOKEN = process.env.NEXT_PUBLIC_MAPBOX_TOKEN || "YOUR_MAPBOX_TOKEN";
  const url = `https://api.mapbox.com/geocoding/v5/mapbox.places/${encodeURIComponent(address)}.json?access_token=${MAPBOX_TOKEN}&limit=1&country=VN`;

  try {
    const response = await fetch(url);
    const data = await response.json();
    if (data && data.features && data.features.length > 0) {
      const [longitude, latitude] = data.features[0].center;
      return { latitude, longitude };
    } else {
      return { latitude: null, longitude: null };
    }
  } catch (error) {
    return { latitude: null, longitude: null };
  }
};
