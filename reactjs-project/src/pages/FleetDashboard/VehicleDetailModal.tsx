import {
  X,
  Truck,
  User,
  Phone,
  Calendar,
  Wrench,
  FileText,
} from "lucide-react";
import { useTranslation } from 'react-i18next';
import type { Vehicle } from "../../types/Operations";
import { useEffect, useState } from "react";
import { fetchVehicleMaintenanceByVehicleId } from "../../services/VehicleMaintenanceAPI";

interface VehicleDetailModalProps {
  vehicle: Vehicle;
  isOpen: boolean;
  onClose: () => void;
  onScheduleMaintenance?: (vehicle: Vehicle, emergencyRequest?: any) => void;
}

export default function VehicleDetailModal({
  vehicle,
  isOpen,
  onClose,
  onScheduleMaintenance,
}: VehicleDetailModalProps) {
  const { t } = useTranslation();
  const [maintenances, setMaintenances] = useState<any[]>([]);
  const [emergencyRequests, setEmergencyRequests] = useState<any[]>([]);
  const [loading, setLoading] = useState(false);

  useEffect(() => {
    if (isOpen && vehicle?.id) {
      setLoading(true);
      // Chỉ gọi 1 API để lấy tất cả maintenance records
      fetchVehicleMaintenanceByVehicleId(vehicle.id)
        .then((allMaintenanceData) => {
          console.log("allMaintenanceData:", allMaintenanceData);

          // Phân loại dựa trên status
          const emergencyRequests = allMaintenanceData.filter(
            (m) =>
              m.status?.id === 51 || m.status?.name === "MAINTENANCE_PENDING"
          );
          const regularMaintenance = allMaintenanceData.filter(
            (m) =>
              m.status?.id !== 51 && m.status?.name !== "MAINTENANCE_PENDING"
          );

          setEmergencyRequests(emergencyRequests);
          setMaintenances(regularMaintenance);
        })
        .catch((error) => {
          console.error("Error fetching maintenance data:", error);
          setEmergencyRequests([]);
          setMaintenances([]);
        })
        .finally(() => setLoading(false));
    } else {
      setMaintenances([]);
      setEmergencyRequests([]);
    }
  }, [isOpen, vehicle]);

  if (!isOpen) return null;

  const formatDate = (dateString: string | undefined) => {
    if (!dateString) return t('common.notAvailable', 'Not available');
    return new Date(dateString).toLocaleDateString("vi-VN");
  };

  const getStatusText = (status: any) => {
    if (typeof status === "string") {
      return status === "MAINTENANCE_PENDING"
        ? t('fleet.status.needMaintenance', 'Need Maintenance')
        : status === "MAINTENANCE"
        ? t('fleet.status.underMaintenance', 'Under Maintenance')
        : status === "IN_USE"
        ? t('fleet.status.inUse', 'In Use')
        : t('fleet.status.available', 'Available');
    }
    if (typeof status === "object" && status?.name) {
      return status.name === "MAINTENANCE_PENDING"
        ? t('fleet.status.needMaintenance', 'Need Maintenance')
        : status.name === "MAINTENANCE"
        ? t('fleet.status.underMaintenance', 'Under Maintenance')
        : status.name === "IN_USE"
        ? t('fleet.status.inUse', 'In Use')
        : t('fleet.status.available', 'Available');
    }
    return t('common.unknown', 'Unknown');
  };

  const getStatusColor = (status: any) => {
    const statusKey = (typeof status === "string" ? status : status?.name) || "";
    if (statusKey === "MAINTENANCE_PENDING") return "text-red-600 bg-red-100";
    if (statusKey === "MAINTENANCE") return "text-orange-600 bg-orange-100";
    if (statusKey === "IN_USE") return "text-blue-600 bg-blue-100";
    return "text-green-600 bg-green-100";
  };

  return (
    <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
      <div className="bg-white rounded-lg p-6 w-full max-w-2xl max-h-[80vh] overflow-y-auto">
        {/* Header */}
        <div className="flex justify-between items-center mb-6">
          <h2 className="text-xl font-semibold text-gray-900">
            Chi tiết xe {vehicle.licensePlate}
          </h2>
          <button
            onClick={onClose}
            className="text-gray-400 hover:text-gray-600"
          >
            <X size={24} />
          </button>
        </div>

        {/* Vehicle Info */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          {/* Basic Info */}
          <div className="space-y-4">
            <h3 className="font-medium text-gray-900 border-b pb-2">
              Thông tin xe
            </h3>

            <div className="flex items-center space-x-3">
              <Truck className="text-gray-400" size={20} />
              <div>
                <p className="text-sm text-gray-500">Biển số xe</p>
                <p className="font-medium">{vehicle.licensePlate}</p>
              </div>
            </div>

            <div className="flex items-center space-x-3">
              <div className="w-5 h-5 bg-gray-400 rounded-full"></div>
              <div>
                <p className="text-sm text-gray-500">Loại xe</p>
                <p className="font-medium">{vehicle.vehicleType || "N/A"}</p>
              </div>
            </div>

            <div className="flex items-center space-x-3">
              <div className="w-5 h-5 bg-gray-400 rounded-full"></div>
              <div>
                <p className="text-sm text-gray-500">Tải trọng</p>
                <p className="font-medium">
                  {vehicle.capacityWeightKg || "N/A"} kg
                </p>
              </div>
            </div>

            <div className="flex items-center space-x-3">
              <div className="w-5 h-5 bg-gray-400 rounded-full"></div>
              <div>
                <p className="text-sm text-gray-500">Trạng thái</p>
                <span
                  className={`inline-block px-2 py-1 rounded-full text-xs font-medium ${getStatusColor(
                    vehicle.status
                  )}`}
                >
                  {getStatusText(vehicle.status)}
                </span>
              </div>
            </div>
          </div>

          {/* Driver Info */}
          <div className="space-y-4">
            <h3 className="font-medium text-gray-900 border-b pb-2">
              Thông tin tài xế
            </h3>

            {vehicle.driver || vehicle.currentDriver ? (
              <>
                <div className="flex items-center space-x-3">
                  <User className="text-gray-400" size={20} />
                  <div>
                    <p className="text-sm text-gray-500">Tên tài xế</p>
                    <p className="font-medium">
                      {vehicle.driver?.name ||
                        vehicle.currentDriver?.fullName ||
                        "N/A"}
                    </p>
                  </div>
                </div>

                <div className="flex items-center space-x-3">
                  <Phone className="text-gray-400" size={20} />
                  <div>
                    <p className="text-sm text-gray-500">Số điện thoại</p>
                    <p className="font-medium">
                      {vehicle.driver?.phone ||
                        vehicle.currentDriver?.phone ||
                        "N/A"}
                    </p>
                  </div>
                </div>
              </>
            ) : (
              <div className="text-gray-500 italic">
                Chưa có tài xế được phân công
              </div>
            )}
          </div>

          {/* Maintenance Info */}
          <div className="space-y-4 md:col-span-2">
            <h3 className="font-medium text-gray-900 border-b pb-2 flex items-center gap-2">
              <FileText className="w-5 h-5 text-blue-400" />
              Thông tin bảo trì
            </h3>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4 mb-2">
              <div className="flex items-center space-x-3">
                <Calendar className="text-gray-400" size={20} />
                <div>
                  <p className="text-sm text-gray-500">Bảo trì gần nhất</p>
                  <p className="font-medium">
                    {formatDate(vehicle.lastMaintenance)}
                  </p>
                </div>
              </div>
              <div className="flex items-center space-x-3">
                <Wrench className="text-gray-400" size={20} />
                <div>
                  <p className="text-sm text-gray-500">Bảo trì tiếp theo</p>
                  <p className="font-medium">
                    {formatDate(vehicle.nextMaintenance)}
                  </p>
                </div>
              </div>
            </div>
            {/* Yêu cầu bảo trì khẩn cấp */}
            <div className="mt-2">
              <h4 className="font-semibold text-red-700 mb-2">
                Yêu cầu bảo trì khẩn cấp
              </h4>
              {loading ? (
                <div className="text-gray-500 italic">Đang tải...</div>
              ) : emergencyRequests.length === 0 ? (
                <div className="text-gray-500 italic">
                  Không có yêu cầu bảo trì khẩn cấp
                </div>
              ) : (
                <div className="space-y-2 max-h-32 overflow-y-auto">
                  {emergencyRequests.map((e) => (
                    <div
                      key={e.id}
                      className="border rounded p-2 bg-red-50 flex flex-col md:flex-row md:items-center md:justify-between"
                    >
                      <div>
                        <span className="font-medium text-red-700">
                          {e.maintenanceType || "Khẩn cấp"}
                        </span>
                        <span className="mx-2 text-gray-400">|</span>
                        <span className="text-gray-700">{e.description}</span>
                      </div>
                      <div className="flex flex-col md:items-end md:justify-end">
                        <div className="text-xs text-gray-500 mt-1 md:mt-0">
                          {e.createdAt
                            ? new Date(e.createdAt).toLocaleString("vi-VN")
                            : ""}
                        </div>
                        {/* Đã loại bỏ nút Đặt lịch bảo trì để chỉ sử dụng 1 nút duy nhất ở MaintenanceModal */}
                      </div>
                    </div>
                  ))}
                </div>
              )}
            </div>
          </div>
        </div>

        {/* Footer */}
        <div className="flex justify-end mt-6 pt-4 border-t">
          <button
            onClick={onClose}
            className="px-4 py-2 text-gray-600 bg-gray-100 rounded-lg hover:bg-gray-200 transition-colors"
          >
            Đóng
          </button>
        </div>
      </div>
    </div>
  );
}
