// Các hàm gọi API Mapbox và Nominatim cho lộ trình, tìm kiếm địa điểm, v.v.

export async function searchLocation(query: string) {
  // Gọi API Nominatim hoặc Mapbox Search
  const res = await fetch(`/api/nominatim/search?q=${encodeURIComponent(query)}&format=json&addressdetails=1&limit=5`);
  if (!res.ok) throw new Error('Không tìm thấy địa điểm');
  return res.json();
}

export async function getRoute(
  start: [number, number],
  waypoints: [number, number][],
  end: [number, number],
  token: string
) {
  // Tạo chuỗi waypoint cho Mapbox Directions API
  const waypointsStr = waypoints.map(wp => `${wp[0]},${wp[1]}`).join(';');
  const url = `https://api.mapbox.com/directions/v5/mapbox/driving/${start[0]},${start[1]};${waypointsStr}${waypointsStr ? ';' : ''}${end[0]},${end[1]}?geometries=geojson&steps=true&access_token=${token}`;
  const res = await fetch(url);
  if (!res.ok) throw new Error('Không lấy được lộ trình');
  return res.json();
}

export async function postRoute(route: any, waypoints: [number, number][]) {
  // Gửi lộ trình về backend
  const res = await fetch('/api/routes', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ route, waypoints })
  });
  if (!res.ok) throw new Error('Không gửi được lộ trình');
  return res.json();
}

export async function postTruckPosition(position: [number, number]) {
  // Gửi vị trí xe về backend
  const res = await fetch('/api/truck-position', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ position })
  });
  if (!res.ok) throw new Error('Không gửi được vị trí xe');
  return res.json();
}
