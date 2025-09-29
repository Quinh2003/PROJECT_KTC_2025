import type { TimelineStepDto } from '../types/Order';

// API lấy danh sách các bước checklist chuẩn từ bảng checklist step
export async function fetchChecklistSteps(token?: string): Promise<TimelineStepDto[]> {
  const authToken = token || localStorage.getItem('token') || '';
  const res = await fetch(`http://localhost:8080/api/checklist/steps`, {
    headers: authToken ? { 'Authorization': `Bearer ${authToken}` } : undefined,
  });
  if (!res.ok) return [];
  const data = await res.json();
  return Array.isArray(data) ? data : [];
}

// API lấy tiến trình checklist cho đơn hàng (chỉ các bước đã hoàn thành)
export async function fetchOrderChecklistProgress(orderId: string | number, token?: string): Promise<TimelineStepDto[]> {
  const authToken = token || localStorage.getItem('token') || '';
  const res = await fetch(`http://localhost:8080/api/checklist/orders/${orderId}/timeline`, {
    headers: authToken ? { 'Authorization': `Bearer ${authToken}` } : undefined,
  });
  if (!res.ok) return [];
  const data = await res.json();
  return Array.isArray(data.timeline) ? data.timeline : [];
}

// API cũ để lấy timeline (giữ lại để tương thích)
export async function fetchOrderTimeline(orderId: string | number, token?: string): Promise<TimelineStepDto[]> {
  const authToken = token || localStorage.getItem('token') || '';
  const res = await fetch(`http://localhost:8080/api/checklist/orders/${orderId}/timeline`, {
    headers: authToken ? { 'Authorization': `Bearer ${authToken}` } : undefined,
  });
  if (!res.ok) return [];
  const data = await res.json();
  // data.timeline là mảng các bước checklist
  return Array.isArray(data.timeline) ? data.timeline : [];
}
