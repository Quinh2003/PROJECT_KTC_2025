import type { TimelineStepDto } from "@/types/orders";

const BASE_URL = process.env.NEXT_PUBLIC_API_BASE_URL || "http://localhost:8080";

export async function fetchChecklistSteps(token?: string): Promise<TimelineStepDto[]> {
  const headers: Record<string, string> = {};
  if (token) headers["Authorization"] = `Bearer ${token}`;
  try {
    const res = await fetch(`${BASE_URL}/api/checklist/steps`, { headers, cache: "no-store" });
    if (res.ok) {
      const data = await res.json();
      if (Array.isArray(data)) return data;
    }
  } catch (e) {
    if (process.env.NODE_ENV !== "production") console.warn("[Checklist] steps default failed", e);
  }

  // Fallback: steps theo role customer
  try {
    const res2 = await fetch(`${BASE_URL}/api/checklist/steps/customer`, { headers, cache: "no-store" });
    if (res2.ok) {
      const data2 = await res2.json();
      if (Array.isArray(data2)) return data2;
    }
  } catch (e) {
    if (process.env.NODE_ENV !== "production") console.warn("[Checklist] steps customer failed", e);
  }
  return [];
}

export async function fetchOrderChecklistProgress(orderId: string | number, token?: string): Promise<TimelineStepDto[]> {
  const headers: Record<string, string> = {};
  if (token) headers["Authorization"] = `Bearer ${token}`;
  try {
    const res = await fetch(`${BASE_URL}/api/checklist/orders/${orderId}/timeline`, { headers, cache: "no-store" });
    if (!res.ok) {
      if (process.env.NODE_ENV !== "production") console.warn("[Checklist] timeline response not ok", res.status);
      return [];
    }
    const data = await res.json();
    if (Array.isArray(data?.timeline)) return data.timeline as TimelineStepDto[];
    if (Array.isArray(data)) return data as TimelineStepDto[];
    // Một số API có thể bọc { data: { timeline: [] } }
    if (Array.isArray((data as any)?.data?.timeline)) return (data as any).data.timeline as TimelineStepDto[];
    return [];
  } catch (e) {
    if (process.env.NODE_ENV !== "production") console.error("[Checklist] timeline fetch error", e);
    return [];
  }
}


