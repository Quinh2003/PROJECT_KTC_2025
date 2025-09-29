/**
 * Bảng giá vận chuyển theo khoảng cách
 */
export interface DistanceFeeResult {
  fee: number;
  region: string;
  base: number;
  perKm: number;
}

/**
 * Tính phí vận chuyển theo khoảng cách
 */
export const calculateDistanceFee = (distance: number): DistanceFeeResult => {
  if (distance <= 50) {
    const fee = 15000 + distance * 1800;
    return {
      fee,
      region: "Inner city (0-50km)",
      base: 15000,
      perKm: 1800,
    };
  } else if (distance <= 150) {
    const fee = 25000 + distance * 1500;
    return {
      fee,
      region: "Suburban (50-150km)",
      base: 25000,
      perKm: 1500,
    };
  } else {
    const fee = 40000 + distance * 500;
    return {
      fee,
      region: "Inter-provincial (>150km)",
      base: 40000,
      perKm: 500,
    };
  }
};

/**
 * Tính khoảng cách Haversine giữa 2 điểm [lng, lat]
 */
export const haversineDistance = (
  a: [number, number],
  b: [number, number]
): number => {
  const toRad = (deg: number) => (deg * Math.PI) / 180;
  const R = 6371; // bán kính Trái Đất (km)

  const dLat = toRad(b[1] - a[1]);
  const dLng = toRad(b[0] - a[0]);
  const lat1 = toRad(a[1]);
  const lat2 = toRad(b[1]);

  const aVal =
    Math.sin(dLat / 2) ** 2 +
    Math.cos(lat1) * Math.cos(lat2) * Math.sin(dLng / 2) ** 2;
  const c = 2 * Math.atan2(Math.sqrt(aVal), Math.sqrt(1 - aVal));

  return R * c;
};

/**
 * Tính tổng khoảng cách qua các waypoint
 */
export const calculateTotalDistance = (points: [number, number][]): number => {
  if (!points || points.length < 2) return 0;

  let total = 0;
  for (let i = 1; i < points.length; i++) {
    total += haversineDistance(points[i - 1], points[i]);
  }
  return total;
};
