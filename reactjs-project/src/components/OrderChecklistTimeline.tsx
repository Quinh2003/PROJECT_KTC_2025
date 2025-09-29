import React, { useEffect, useState } from "react";
import type { TimelineStepDto } from "../types/Order";
import { fetchChecklistSteps, fetchOrderChecklistProgress } from "../services/ChecklistAPI";
import { MdAccessTime, MdInventory, MdLocalShipping, MdLocationOn, MdCheckCircle } from "react-icons/md";

interface Props {
  orderId: string | number;
  // currentStepCode có thể dùng để highlight bước hiện tại nếu cần
}

const OrderChecklistTimeline: React.FC<Props> = ({ orderId }) => {
  const [mergedSteps, setMergedSteps] = useState<TimelineStepDto[]>([]);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    const fetchData = async () => {
      try {
        setLoading(true);
        // Gọi 2 API song song
        const [standardSteps, progressSteps] = await Promise.all([
          fetchChecklistSteps(),
          fetchOrderChecklistProgress(orderId)
        ]);

        // DEBUG: Log dữ liệu từ API
        console.log('🔍 [DEBUG] Standard steps:', standardSteps);
        console.log('🔍 [DEBUG] Progress steps:', progressSteps);

        // Merge dữ liệu: lấy cấu trúc từ standardSteps, cập nhật completedAt từ progressSteps
        const merged = standardSteps.map(standardStep => {
          const progressStep = progressSteps.find(p => p.stepCode === standardStep.stepCode);
          const mergedStep = {
            ...standardStep,
            completedAt: progressStep?.completedAt || undefined,
            completed: !!progressStep?.completedAt,
            actor: progressStep?.actor || undefined,
            details: progressStep?.details || undefined,
            status: progressStep?.status || undefined
          };
          
          // DEBUG: Log từng bước merge
          if (progressStep) {
            console.log(`🔍 [DEBUG] Merged step ${standardStep.stepCode}:`, {
              stepCode: mergedStep.stepCode,
              stepName: mergedStep.stepName,
              completed: mergedStep.completed,
              completedAt: mergedStep.completedAt
            });
          }
          
          return mergedStep;
        });

        // Sắp xếp theo stepOrder
        merged.sort((a, b) => (a.stepOrder || 0) - (b.stepOrder || 0));
        
        console.log('🔍 [DEBUG] Final merged steps:', merged);
        setMergedSteps(merged);
      } catch (error) {
        console.error('Lỗi khi tải dữ liệu checklist:', error);
      } finally {
        setLoading(false);
      }
    };

    if (orderId) {
      fetchData();
    }
  }, [orderId]);

  if (loading) {
    return (
      <div className="relative py-3 px-2 flex justify-center">
        <div className="text-gray-500">Đang tải...</div>
      </div>
    );
  }

  // Gộp 2 bước cuối nếu vừa giao vừa thanh toán luôn
  let displaySteps = [...mergedSteps];
  const lastIdx = mergedSteps.length - 1;
  if (
    mergedSteps.length >= 2 &&
    mergedSteps[lastIdx].stepCode === 'COMPLETED' &&
    mergedSteps[lastIdx].completedAt &&
    mergedSteps[lastIdx - 1].stepCode === 'ORDER_DELIVERED_PAYMENT_PENDING' &&
    !mergedSteps[lastIdx - 1].completedAt
  ) {
    // Gộp 2 bước cuối thành 1
    displaySteps = [
      ...mergedSteps.slice(0, lastIdx - 1),
      {
        ...mergedSteps[lastIdx],
        stepName: `${mergedSteps[lastIdx - 1].stepName} & ${mergedSteps[lastIdx].stepName}`,
      }
    ];
  }


  return (
    <div className="relative py-6 px-6 bg-gradient-to-r from-blue-50/30 to-indigo-50/30 rounded-2xl border border-blue-100/50">
      <div className="relative max-w-5xl w-full mx-auto">
        {/* Timeline Header */}
        <div className="text-center mb-8">
          <h3 className="text-xl font-bold text-gray-800 mb-2">🚚 Trạng thái đơn hàng</h3>
          <p className="text-sm text-gray-600">Theo dõi tiến trình giao hàng từ lúc tạo đơn đến hoàn thành</p>
        </div>
        
        {/* Horizontal timeline container */}
        <div className="relative flex justify-center">
          {/* Continuous line behind all dots */}
          <div className="absolute top-8 left-16 right-16 h-2 bg-gray-200 rounded-full z-0 shadow-inner"></div>
          
          {/* Animated progress line overlay */}
          {(() => {
            const lastCompletedIdx = displaySteps.reduce((acc, step, idx) => step.completedAt ? idx : acc, -1);
            if (lastCompletedIdx >= 0) {
              const progressPercent = (lastCompletedIdx / (displaySteps.length - 1)) * 100;
              return (
                <div 
                  className="absolute top-8 left-16 h-2 bg-gradient-to-r from-emerald-400 via-green-500 to-emerald-600 rounded-full z-10 shadow-lg transition-all duration-1000 ease-out"
                  style={{ 
                    width: `calc((100% - 8rem) * ${progressPercent / 100})`,
                    boxShadow: '0 0 10px rgba(16, 185, 129, 0.5)'
                  }}
                ></div>
              );
            }
            return null;
          })()}

          {/* Steps container */}
          <div className="flex items-start relative px-4 justify-between w-full max-w-4xl">
            {displaySteps.map((step: TimelineStepDto, idx: number) => {
              const lastCompletedIdx = displaySteps.reduce((acc, step, idx) => step.completedAt ? idx : acc, -1);
              const isCompleted = !!step.completedAt;
              // Chỉ hiển thị "đang xử lý" nếu là bước tiếp theo sau bước hoàn thành cuối cùng
              // và không có bước nào sau nó đã hoàn thành
              const isActive = !isCompleted && idx === lastCompletedIdx + 1;
              
              return (
                <div key={step.stepCode + idx} className="flex flex-col items-center relative flex-1 min-w-0">
                  {/* Dot with icon */}
                  <div className={`w-16 h-16 rounded-full border-4 z-20 flex items-center justify-center transition-all duration-300 shadow-lg transform hover:scale-105 ${
                    isCompleted 
                      ? 'bg-gradient-to-br from-emerald-400 to-green-600 border-white shadow-emerald-300/50' 
                      : isActive
                      ? 'bg-gradient-to-br from-blue-400 to-blue-600 border-white shadow-blue-300/50'
                      : 'bg-gradient-to-br from-gray-300 to-gray-400 border-gray-200 shadow-gray-200/50'
                  }`}>
                    {/* Icon based on step */}
                    <div className="text-white text-2xl flex items-center justify-center">
                      {step.stepCode === 'CUSTOMER_CREATE_ORDER' && <MdAccessTime className="drop-shadow-sm" />}
                      {step.stepCode === 'DISPATCHER_ASSIGN_DRIVER' && <MdInventory className="drop-shadow-sm" />}
                      {step.stepCode === 'DRIVER_RECEIVE_ORDER' && <MdLocalShipping className="drop-shadow-sm" />}
                      {(step.stepCode === 'DRIVER_COMPLETE_DELIVERY' || step.stepCode === 'COMPLETED') && <MdCheckCircle className="drop-shadow-sm" />}
                      {/* Fallback icon */}
                      {!['CUSTOMER_CREATE_ORDER', 'DISPATCHER_ASSIGN_DRIVER', 'DRIVER_RECEIVE_ORDER', 'DRIVER_COMPLETE_DELIVERY', 'COMPLETED'].includes(step.stepCode) && <MdLocationOn className="drop-shadow-sm" />}
                    </div>
                  </div>
                  
                  {/* Step content below dot */}
                  <div className="flex flex-col items-center mt-4 px-2">
                    <div className={`font-bold text-center mb-3 leading-tight ${
                      isCompleted 
                        ? 'text-emerald-700 text-sm' 
                        : isActive 
                        ? 'text-blue-700 text-sm' 
                        : 'text-gray-600 text-sm'
                    }`}>
                      {step.stepName}
                    </div>
                    <div className="text-xs text-center">
                      {step.completedAt ? (
                        <div className="bg-white/80 backdrop-blur-sm rounded-lg p-3 border border-emerald-200/50 shadow-sm">
                          <div className="font-semibold text-emerald-800 flex items-center gap-1">
                            <MdCheckCircle className="text-emerald-500" />
                            {typeof step.completedAt === 'string' ? new Date(step.completedAt).toLocaleDateString('vi-VN') : new Date(step.completedAt!).toLocaleDateString('vi-VN')}
                          </div>
                          <div className="text-emerald-600 mt-1 text-xs">
                            {typeof step.completedAt === 'string' ? new Date(step.completedAt).toLocaleTimeString('vi-VN', { hour: '2-digit', minute: '2-digit' }) : new Date(step.completedAt).toLocaleTimeString('vi-VN', { hour: '2-digit', minute: '2-digit' })}
                          </div>
                        </div>
                      ) : isActive ? (
                        <div className="bg-blue-50/80 backdrop-blur-sm rounded-lg p-3 border border-blue-200/50 shadow-sm">
                          <span className="text-blue-600 font-medium flex items-center gap-1">
                            <div className="w-2 h-2 bg-blue-500 rounded-full"></div>
                            Đang xử lý
                          </span>
                        </div>
                      ) : (
                        <div className="bg-gray-50/80 backdrop-blur-sm rounded-lg p-3 border border-gray-200/50 shadow-sm">
                          <span className="text-gray-500 font-medium">Chưa hoàn thành</span>
                        </div>
                      )}
                    </div>
                  </div>
                </div>
              );
            })}
          </div>
        </div>
        
        {/* Progress indicator */}
        <div className="mt-8 text-center">
          {(() => {
            const completedCount = displaySteps.filter(step => step.completedAt).length;
            const totalSteps = displaySteps.length;
            const progressPercent = (completedCount / totalSteps) * 100;
            
            return (
              <div className="inline-flex items-center gap-3 bg-white/80 backdrop-blur-sm rounded-full px-6 py-3 border border-gray-200/50 shadow-lg">
                <div className="text-sm font-semibold text-gray-700">
                  Tiến trình: {completedCount}/{totalSteps} bước
                </div>
                <div className="w-32 h-2 bg-gray-200 rounded-full overflow-hidden">
                  <div 
                    className="h-full bg-gradient-to-r from-emerald-400 to-green-500 rounded-full transition-all duration-1000 ease-out"
                    style={{ width: `${progressPercent}%` }}
                  ></div>
                </div>
                <div className="text-sm font-bold text-emerald-600">
                  {Math.round(progressPercent)}%
                </div>
              </div>
            );
          })()}
        </div>
      </div>
    </div>
  );
};

export default OrderChecklistTimeline;
