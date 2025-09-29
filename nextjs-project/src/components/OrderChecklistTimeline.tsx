"use client";
import React, { useEffect, useMemo, useState } from "react";
import type { TimelineStepDto } from "@/types/orders";
import { fetchChecklistSteps, fetchOrderChecklistProgress } from "@/services/checklist";

interface Props {
  orderId: string | number;
  orderStatus?: string;
}

export default function OrderChecklistTimeline({ orderId, orderStatus }: Props) {
  const [steps, setSteps] = useState<TimelineStepDto[]>([]);
  const [loading, setLoading] = useState<boolean>(true);

  useEffect(() => {
    let mounted = true;
    const load = async () => {
      try {
        setLoading(true);
        const numericId = typeof orderId === "number" ? orderId : parseInt(String(orderId), 10);
        const safeOrderId = Number.isFinite(numericId) ? numericId : String(orderId);
        const [standardSteps, progress] = await Promise.all([
          fetchChecklistSteps(),
          fetchOrderChecklistProgress(safeOrderId),
        ]);
        if (process.env.NODE_ENV !== "production") {
          console.debug("[Checklist] standardSteps:", standardSteps);
          console.debug("[Checklist] progress:", progress);
        }

        const hasCompleted =
          progress.some(p => (p.stepCode === "DRIVER_COMPLETE_DELIVERY" || p.stepCode === "COMPLETED") && p.completedAt && p.completed) ||
          (!!orderStatus && orderStatus.toLowerCase().includes("delivered"));

        let merged = (Array.isArray(standardSteps) ? standardSteps : []).map(s => {
          const p = progress.find(x => x.stepCode === s.stepCode);
          const base = {
            ...s,
            completed: !!(p?.completedAt && p?.completed),
            completedAt: p?.completedAt,
            actor: p?.actor,
            details: p?.details,
            status: p?.status,
          } as TimelineStepDto;
          if (hasCompleted) {
            base.completed = true;
            base.completedAt = base.completedAt || new Date().toISOString();
          }
          return base;
        });

        // Nếu không lấy được danh sách chuẩn nhưng có progress, dùng progress làm nguồn hiển thị
        if ((!merged || merged.length === 0) && Array.isArray(progress) && progress.length > 0) {
          merged = progress.map(p => ({
            stepCode: p.stepCode,
            stepName: p.stepName || p.stepCode,
            description: p.description,
            completed: p.completed,
            completedAt: p.completedAt,
            actor: p.actor,
            details: p.details,
            status: p.status,
          }));
        }

        if (mounted) setSteps(merged.filter(Boolean));
      } finally {
        if (mounted) setLoading(false);
      }
    };
    if (orderId) load();
    return () => {
      mounted = false;
    };
  }, [orderId, orderStatus]);

  const display = useMemo(() => steps, [steps]);

  if (loading) {
    return (
      <div className="text-gray-500 text-sm">Đang tải checklist...</div>
    );
  }

  if (!steps || steps.length === 0) {
    return (
      <div className="mt-4 text-sm text-gray-600 bg-gray-50 border border-gray-200 rounded-lg p-4">
        Chưa có checklist cho đơn hàng này.
      </div>
    );
  }

  return (
    <div className="mt-6">
      {/* Lưới đều cột, mỗi ô cùng chiều cao */}
      <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4">
        {display.map((step, idx) => {
          const isDone = !!step.completedAt || step.completed;
          return (
            <div key={`${step.stepCode}-${idx}`} className="h-full">
              <div
                className={`h-full rounded-xl border p-4 flex flex-col justify-between ${
                  isDone ? "bg-emerald-50 border-emerald-200" : "bg-gray-50 border-gray-200"
                }`}
              >
                <div className={`font-semibold mb-2 ${isDone ? "text-emerald-700" : "text-gray-700"}`}>
                  {step.stepName}
                </div>
                <div className="text-xs text-gray-500">
                  {isDone
                    ? typeof step.completedAt === "string"
                      ? new Date(step.completedAt).toLocaleString("vi-VN")
                      : new Date(step.completedAt as any).toLocaleString("vi-VN")
                    : "Chưa hoàn thành"}
                </div>
              </div>
            </div>
          );
        })}
      </div>
    </div>
  );
}


