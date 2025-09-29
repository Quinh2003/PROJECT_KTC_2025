import React, { useState, useMemo } from "react";
import { useTranslation } from 'react-i18next';
import {
  Edit,
  MoreVertical,
  AlertCircle,
  CheckCircle,
  Clock,
} from "lucide-react";
import type { Vehicle } from "../../types/Operations";

export type FleetVehicle = Vehicle;

interface VehicleTableProps {
  vehicles: Vehicle[];
  onEdit?: (vehicle: Vehicle) => void;
  onDelete?: (vehicleId: string | number) => void;
}

// Enhanced Status Badge Component

const StatusBadge = React.memo<{ status: string; t: any }>(({ status, t }) => {
  const statusConfig = useMemo(() => {
    switch (status) {
      case "MAINTENANCE_PENDING":
        return {
          icon: <AlertCircle size={14} />,
          className: "bg-red-50 text-red-700 border-red-200",
          text: t('dashboard.fleet.status.needMaintenance', 'Need Maintenance'),
        };
      case "MAINTENANCE":
        return {
          icon: <Clock size={14} />,
          className: "bg-yellow-50 text-yellow-700 border-yellow-200",
          text: t('dashboard.fleet.status.underMaintenance', 'Under Maintenance'),
        };
      case "IN_USE":
        return {
          icon: <CheckCircle size={14} />,
          className: "bg-blue-50 text-blue-700 border-blue-200",
          text: t('dashboard.fleet.status.inUse', 'In Use'),
        };
      case "AVAILABLE":
      default:
        return {
          icon: <CheckCircle size={14} />,
          className: "bg-green-50 text-green-700 border-green-200",
          text: t('dashboard.fleet.status.available', 'Available'),
        };
    }
  }, [status]);

  return (
    <span
      className={`inline-flex items-center gap-1 px-2.5 py-1 rounded-full text-xs font-medium border ${statusConfig.className}`}
    >
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
  onDelete?: (vehicleId: string | number) => void;
}>(({ vehicle, onEdit, onDelete }) => {
  const { t } = useTranslation();
  const [isOpen, setIsOpen] = useState(false);

  return (
    <div className="relative">
      <button
        onClick={() => setIsOpen(!isOpen)}
        className="p-2 rounded-full hover:bg-gray-100 transition-colors"
        aria-label={t('common.options', 'Options')}
      >
        <MoreVertical size={18} />
      </button>
      {isOpen && (
        <>
          <div
            className="fixed inset-0 z-10"
            onClick={() => setIsOpen(false)}
          />
          <div className="absolute right-0 top-full mt-1 w-48 bg-white rounded-lg shadow-xl border border-gray-200 py-1 z-30">
            {onEdit && (
              <button
                onClick={() => {
                  onEdit(vehicle);
                  setIsOpen(false);
                }}
                className="w-full px-4 py-2 text-left text-sm hover:bg-gray-50 flex items-center gap-2"
              >
                <Edit size={16} />
                {t('common.edit')}
              </button>
            )}
            {onDelete && (
              <button
                onClick={() => {
                  onDelete(vehicle.id);
                  setIsOpen(false);
                }}
                className="w-full px-4 py-2 text-left text-sm hover:bg-gray-50 flex items-center gap-2 text-red-600"
              >
                <svg
                  width="16"
                  height="16"
                  fill="none"
                  stroke="currentColor"
                  strokeWidth="2"
                  viewBox="0 0 24 24"
                >
                  <path d="M3 6h18M9 6V4a1 1 0 0 1 1-1h4a1 1 0 0 1 1 1v2m2 0v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6h14z" />
                </svg>
                {t('common.delete')}
              </button>
            )}
          </div>
        </>
      )}
    </div>
  );
});

ActionDropdown.displayName = "ActionDropdown";

// Helper function to convert status to string
const getStatusString = (status: any): string => {
  if (typeof status === 'string') return status;
  if (typeof status === 'number') return status.toString();
  if (typeof status === 'object' && status?.name) return status.name;
  return 'AVAILABLE';
};

// Main Vehicle Table Component
const VehicleTable: React.FC<VehicleTableProps> = ({
  vehicles,
  onEdit,
  onDelete,
}) => {
  const { t } = useTranslation();
  // Calculate days until next maintenance
  const getDaysUntilMaintenance = (nextMaintenance?: string): number | null => {
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
        const daysUntilMaintenance = getDaysUntilMaintenance(
          vehicle.nextMaintenance
        );
        const maintenanceUrgencyColor =
          getMaintenanceUrgencyColor(daysUntilMaintenance);

        // Logic xác định trạng thái xe - ưu tiên trạng thái từ database
        let computedStatus: string = "AVAILABLE";

        console.log("Vehicle data:", vehicle); // Debug log để kiểm tra dữ liệu

        // Nếu có trạng thái từ database, sử dụng nó
        if (vehicle.status) {
          computedStatus = getStatusString(vehicle.status);
          console.log("Using status from database:", vehicle.status); // Debug log
        } else {
          console.log("No status from database, using fallback logic"); // Debug log
          // Fallback: tính toán dựa trên thời gian bảo trì và tài xế
          const today = new Date();

          // Kiểm tra xem xe có đang trong thời gian bảo trì không
          if (vehicle.lastMaintenance && vehicle.nextMaintenance) {
            const maintenanceStartDate = new Date(vehicle.lastMaintenance);
            const maintenanceEndDate = new Date(vehicle.nextMaintenance);

            // Nếu hôm nay nằm trong khoảng thời gian bảo trì
            if (today >= maintenanceStartDate && today <= maintenanceEndDate) {
              computedStatus = "MAINTENANCE";
            } else if (
              vehicle.driver?.name ||
              vehicle.currentDriver?.fullName
            ) {
              computedStatus = "IN_USE";
            } else {
              computedStatus = "AVAILABLE";
            }
          } else if (vehicle.driver?.name || vehicle.currentDriver?.fullName) {
            computedStatus = "IN_USE";
          }
        }

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
                  <StatusBadge status={computedStatus} t={t} />
                </div>

                {/* Vehicle Details Grid */}
                <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4 text-sm">
                  <div className="space-y-1">
                    <span className="text-gray-500 font-medium">{t('dashboard.fleet.vehicleType', 'Vehicle Type')}:</span>
                    <div className="text-gray-900">{vehicle.type}</div>
                  </div>
                  <div className="space-y-1">
                    <span className="text-gray-500 font-medium">
                      {t('dashboard.fleet.capacity', 'Capacity')} (kg):
                    </span>
                    <div className="text-gray-900 font-semibold">
                      {vehicle.capacityWeightKg?.toLocaleString() ?? "-"} kg
                    </div>
                  </div>
                  <div className="space-y-1">
                    <span className="text-gray-500 font-medium">
                      {t('dashboard.fleet.volume', 'Volume')} (m³):
                    </span>
                    <div className="text-gray-900 font-semibold">
                      {vehicle.capacityVolumeM3?.toLocaleString() ?? "-"} m³
                    </div>
                  </div>
                  <div className="space-y-1">
                    <span className="text-gray-500 font-medium">{t('dashboard.fleet.driver', 'Driver')}:</span>
                    <div className="text-gray-900">
                      {vehicle.driver?.name ||
                        vehicle.currentDriver?.fullName || (
                          <span className="text-gray-400 italic">{t('fleet.notAssigned', 'Not Assigned')}</span>
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
                    <span className="text-gray-500 font-medium">
                      Bảo trì gần nhất:
                    </span>
                    <div className="text-gray-900">
                      {vehicle.lastMaintenance || (
                        <span className="text-gray-400 italic">
                          Chưa có thông tin
                        </span>
                      )}
                    </div>
                  </div>
                  <div className="space-y-1">
                    <span className="text-gray-500 font-medium">
                      Bảo trì tiếp theo:
                    </span>
                    <div className={maintenanceUrgencyColor}>
                      {vehicle.nextMaintenance ? (
                        <div className="flex items-center gap-2">
                          <span>{vehicle.nextMaintenance}</span>
                          {daysUntilMaintenance !== null && (
                            <span className="text-xs px-2 py-1 rounded-full bg-gray-100">
                              {daysUntilMaintenance < 0
                                ? t('fleet.maintenance.overdue', 'Overdue {{days}} days', { days: Math.abs(daysUntilMaintenance) })
                                : daysUntilMaintenance === 0
                                ? t('common.today', 'Today')
                                : t('fleet.maintenance.remaining', '{{days}} days remaining', { days: daysUntilMaintenance })}
                            </span>
                          )}
                        </div>
                      ) : (
                        <span className="text-gray-400 italic">
                          Chưa lập lịch
                        </span>
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
                  onDelete={onDelete}
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
