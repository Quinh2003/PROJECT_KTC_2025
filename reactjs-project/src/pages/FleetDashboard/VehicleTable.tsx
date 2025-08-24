
import React, { useState, useMemo } from "react";
import { Edit, Eye, MoreVertical, Calendar, AlertCircle, CheckCircle, Clock } from "lucide-react";

export interface Vehicle {
  id: number;
  licensePlate: string;
  type: string;
  capacityWeightKg?: number;
  capacityVolumeM3?: number;
  year: number;
  status: "Hoạt động" | "Bảo trì" | "Cần bảo trì";
  lastMaintenance: string;
  nextMaintenance: string;
  driver: string;
}


interface VehicleTableProps {
  vehicles: Vehicle[];
  onEdit?: (vehicle: Vehicle) => void;
  onView?: (vehicle: Vehicle) => void;
  onAssignDriver?: (vehicleId: number) => void;
  onScheduleMaintenance?: (vehicleId: number) => void;
}

// Enhanced Status Badge Component
const StatusBadge = React.memo<{ status: Vehicle["status"] }>(({ status }) => {
  const statusConfig = useMemo(() => {
    switch (status) {
      case "Hoạt động":
        return {
          icon: <CheckCircle size={14} />,
          className: "bg-green-50 text-green-700 border-green-200",
          text: "Hoạt động"
        };
      case "Bảo trì":
        return {
          icon: <Clock size={14} />,
          className: "bg-yellow-50 text-yellow-700 border-yellow-200",
          text: "Bảo trì"
        };
      case "Cần bảo trì":
        return {
          icon: <AlertCircle size={14} />,
          className: "bg-red-50 text-red-700 border-red-200",
          text: "Cần bảo trì"
        };
      default:
        return {
          icon: null,
          className: "bg-gray-50 text-gray-700 border-gray-200",
          text: status
        };
    }
  }, [status]);

  return (
    <span className={`inline-flex items-center gap-1 px-2.5 py-1 rounded-full text-xs font-medium border ${statusConfig.className}`}>
      {statusConfig.icon}
      {statusConfig.text}
    </span>
  );
});

StatusBadge.displayName = "StatusBadge";

// Action Dropdown Component
const ActionDropdown = React.memo<{
  vehicle: Vehicle;
  onEdit?: (vehicle: Vehicle) => void;
  onView?: (vehicle: Vehicle) => void;
  onAssignDriver?: (vehicleId: number) => void;
  onScheduleMaintenance?: (vehicleId: number) => void;
}>(({ vehicle, onEdit, onView, onAssignDriver, onScheduleMaintenance }) => {
  const [isOpen, setIsOpen] = useState(false);

  return (
    <div className="relative">
      <button
        onClick={() => setIsOpen(!isOpen)}
        className="p-2 rounded-full hover:bg-gray-100 transition-colors"
        aria-label="Tùy chọn"
      >
        <MoreVertical size={18} />
      </button>
      
      {isOpen && (
        <>
          <div 
            className="fixed inset-0 z-10" 
            onClick={() => setIsOpen(false)}
          />
          <div className="absolute right-0 top-full mt-1 w-48 bg-white rounded-lg shadow-lg border border-gray-200 py-1 z-20">
            {onView && (
              <button
                onClick={() => {
                  onView(vehicle);
                  setIsOpen(false);
                }}
                className="w-full px-4 py-2 text-left text-sm hover:bg-gray-50 flex items-center gap-2"
              >
                <Eye size={16} />
                Xem chi tiết
              </button>
            )}
            {onEdit && (
              <button
                onClick={() => {
                  onEdit(vehicle);
                  setIsOpen(false);
                }}
                className="w-full px-4 py-2 text-left text-sm hover:bg-gray-50 flex items-center gap-2"
              >
                <Edit size={16} />
                Chỉnh sửa
              </button>
            )}
            {onAssignDriver && (
              <button
                onClick={() => {
                  onAssignDriver(vehicle.id);
                  setIsOpen(false);
                }}
                className="w-full px-4 py-2 text-left text-sm hover:bg-gray-50"
              >
                {vehicle.driver ? "Thay đổi tài xế" : "Gán tài xế"}
              </button>
            )}
            {onScheduleMaintenance && (
              <button
                onClick={() => {
                  onScheduleMaintenance(vehicle.id);
                  setIsOpen(false);
                }}
                className="w-full px-4 py-2 text-left text-sm hover:bg-gray-50 flex items-center gap-2"
              >
                <Calendar size={16} />
                Lập lịch bảo trì
              </button>
            )}
          </div>
        </>
      )}
    </div>
  );
});

ActionDropdown.displayName = "ActionDropdown";

// Main Vehicle Table Component
const VehicleTable: React.FC<VehicleTableProps> = ({ 
  vehicles, 
  onEdit, 
  onView, 
  onAssignDriver, 
  onScheduleMaintenance 
}) => {
  // Calculate days until next maintenance
  const getDaysUntilMaintenance = (nextMaintenance: string): number | null => {
    if (!nextMaintenance) return null;
    const today = new Date();
    const maintenanceDate = new Date(nextMaintenance);
    const diffTime = maintenanceDate.getTime() - today.getTime();
    const diffDays = Math.ceil(diffTime / (1000 * 60 * 60 * 24));
    return diffDays;
  };

  // Get maintenance urgency color
  const getMaintenanceUrgencyColor = (days: number | null): string => {
    if (days === null) return "text-gray-500";
    if (days < 0) return "text-red-600 font-semibold";
    if (days <= 7) return "text-orange-600 font-semibold";
    if (days <= 30) return "text-yellow-600";
    return "text-green-600";
  };

  return (
    <div className="space-y-4">
      {vehicles.map((vehicle) => {
        const daysUntilMaintenance = getDaysUntilMaintenance(vehicle.nextMaintenance);
        const maintenanceUrgencyColor = getMaintenanceUrgencyColor(daysUntilMaintenance);
        
        return (
          <div 
            key={vehicle.id} 
            className="bg-white border border-gray-200 rounded-xl p-6 hover:shadow-md transition-all duration-200"
          >
            <div className="flex flex-col lg:flex-row lg:items-center lg:justify-between gap-4">
              {/* Vehicle Info */}
              <div className="flex-1 space-y-3">
                {/* Header with License Plate and Status */}
                <div className="flex flex-wrap items-center gap-3">
                  <h3 className="text-xl font-bold text-gray-900">
                    {vehicle.licensePlate}
                  </h3>
                  <StatusBadge status={vehicle.status} />
                </div>

                {/* Vehicle Details Grid */}
                <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4 text-sm">
                  <div className="space-y-1">
                    <span className="text-gray-500 font-medium">Loại xe:</span>
                    <div className="text-gray-900">{vehicle.type}</div>
                  </div>
                  <div className="space-y-1">
                    <span className="text-gray-500 font-medium">Trọng tải (kg):</span>
                    <div className="text-gray-900 font-semibold">{vehicle.capacityWeightKg?.toLocaleString() ?? "-"} kg</div>
                  </div>
                  <div className="space-y-1">
                    <span className="text-gray-500 font-medium">Thể tích (m³):</span>
                    <div className="text-gray-900 font-semibold">{vehicle.capacityVolumeM3?.toLocaleString() ?? "-"} m³</div>
                  </div>
                  <div className="space-y-1">
                    <span className="text-gray-500 font-medium">Năm sản xuất:</span>
                    <div className="text-gray-900">{vehicle.year}</div>
                  </div>
                  <div className="space-y-1">
                    <span className="text-gray-500 font-medium">Tài xế:</span>
                    <div className="text-gray-900">
                      {vehicle.driver || (
                        <span className="text-gray-400 italic">Chưa gán</span>
                      )}
                    </div>
                  </div>
                  {/* <div className="space-y-1">
                    <span className="text-gray-500 font-medium">Km đã chạy:</span>
                    <div className="text-gray-900 font-semibold">
                      {vehicle.mileage?.toLocaleString()} km
                    </div>
                  </div> */}
                </div>

                {/* Maintenance Info */}
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4 text-sm pt-2 border-t border-gray-100">
                  <div className="space-y-1">
                    <span className="text-gray-500 font-medium">Bảo trì gần nhất:</span>
                    <div className="text-gray-900">
                      {vehicle.lastMaintenance || (
                        <span className="text-gray-400 italic">Chưa có thông tin</span>
                      )}
                    </div>
                  </div>
                  <div className="space-y-1">
                    <span className="text-gray-500 font-medium">Bảo trì tiếp theo:</span>
                    <div className={maintenanceUrgencyColor}>
                      {vehicle.nextMaintenance ? (
                        <div className="flex items-center gap-2">
                          <span>{vehicle.nextMaintenance}</span>
                          {daysUntilMaintenance !== null && (
                            <span className="text-xs px-2 py-1 rounded-full bg-gray-100">
                              {daysUntilMaintenance < 0 
                                ? `Quá hạn ${Math.abs(daysUntilMaintenance)} ngày`
                                : daysUntilMaintenance === 0
                                ? "Hôm nay"
                                : `Còn ${daysUntilMaintenance} ngày`
                              }
                            </span>
                          )}
                        </div>
                      ) : (
                        <span className="text-gray-400 italic">Chưa lập lịch</span>
                      )}
                    </div>
                  </div>
                </div>
              </div>

              {/* Action Button */}
              <div className="flex-shrink-0">
                <ActionDropdown
                  vehicle={vehicle}
                  onEdit={onEdit}
                  onView={onView}
                  onAssignDriver={onAssignDriver}
                  onScheduleMaintenance={onScheduleMaintenance}
                />
              </div>
            </div>
          </div>
        );
      })}
    </div>
  );
};

export default React.memo(VehicleTable);