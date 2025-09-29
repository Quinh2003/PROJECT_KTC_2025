import { X, AlertCircle, User, Calendar, Phone } from "lucide-react";
import type { Vehicle } from "../../types/Operations";

interface MaintenanceModalProps {
  isOpen: boolean;
  onClose: () => void;
  vehicles: Vehicle[];
  emergencyRequestsByVehicleId: Record<string, any[]>;
  onViewDetails: (vehicle: Vehicle) => void;
  onScheduleMaintenance: (vehicle: Vehicle, emergencyRequest?: any) => void;
}

export default function MaintenanceModal({
  isOpen,
  onClose,
  vehicles,
  emergencyRequestsByVehicleId,
  onViewDetails,
  onScheduleMaintenance,
}: MaintenanceModalProps) {
  if (!isOpen) return null;

  // Debug: Log vehicles data to see structure
  console.log('MaintenanceModal vehicles:', vehicles);
  console.log('MaintenanceModal emergencyRequestsByVehicleId:', emergencyRequestsByVehicleId);

  return (
    <div className="fixed inset-0 z-50 overflow-auto bg-black bg-opacity-50 flex items-center justify-center p-4">
      <div className="relative bg-white rounded-lg w-full max-w-4xl max-h-[90vh] overflow-hidden">
        {/* Header */}
        <div className="flex items-center justify-between p-6 border-b border-gray-200">
          <div className="flex items-center space-x-3">
            <div className="flex-shrink-0 w-10 h-10 bg-red-100 rounded-lg flex items-center justify-center">
              <AlertCircle className="w-5 h-5 text-red-600" />
            </div>
            <div>
              <h2 className="text-xl font-semibold text-gray-900">
                Danh sách xe cần bảo trì
              </h2>
              <p className="text-sm text-gray-500">
                {vehicles.length} xe cần được bảo trì
              </p>
            </div>
          </div>
          <button
            onClick={onClose}
            className="flex-shrink-0 w-8 h-8 bg-gray-100 hover:bg-gray-200 rounded-lg flex items-center justify-center transition-colors"
          >
            <X className="w-4 h-4 text-gray-600" />
          </button>
        </div>

        {/* Content */}
        <div className="p-6 overflow-y-auto max-h-[calc(90vh-120px)]">
          {vehicles.length === 0 ? (
            <div className="text-center py-12">
              <AlertCircle className="w-12 h-12 text-gray-400 mx-auto mb-4" />
              <h3 className="text-lg font-medium text-gray-900 mb-2">
                Không có xe cần bảo trì
              </h3>
              <p className="text-gray-500">
                Tất cả xe hiện tại đều đang trong tình trạng tốt.
              </p>
            </div>
          ) : (
            <div className="space-y-4">
              {vehicles.map((vehicle) => (
                <div
                  key={vehicle.id}
                  className="bg-white border border-gray-200 rounded-lg p-6 hover:shadow-md transition-shadow"
                >
                  <div className="flex items-start justify-between">
                    {/* Vehicle Info */}
                    <div className="flex-1">
                      <div className="flex items-center space-x-3 mb-3">
                        <div className="w-3 h-3 bg-red-500 rounded-full"></div>
                        <h3 className="text-lg font-semibold text-gray-900">
                          {vehicle.licensePlate}
                        </h3>
                        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
                          <AlertCircle className="w-3 h-3 mr-1" />
                          Cần bảo trì
                        </span>
                      </div>

                      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4 mb-4">
                        <div className="flex items-center space-x-2 text-sm text-gray-600">
                          <span className="font-medium">Loại xe:</span>
                          <span>{vehicle.vehicleType}</span>
                        </div>
                        <div className="flex items-center space-x-2 text-sm text-gray-600">
                          <span className="font-medium">Tải trọng:</span>
                          <span>{vehicle.capacityWeightKg || 0} kg</span>
                        </div>
                        
                      </div>

                      {/* Driver Info */}
                      {(vehicle.currentDriver || vehicle.driver) && (
                        <div className="bg-gray-50 rounded-lg p-4">
                          <h4 className="text-sm font-medium text-gray-900 mb-2 flex items-center">
                            <User className="w-4 h-4 mr-2" />
                            Tài xế hiện tại
                          </h4>
                          <div className="grid grid-cols-1 md:grid-cols-2 gap-4 text-sm">
                            <div>
                              <span className="text-gray-600">Tên: </span>
                              <span className="font-medium">
                                {vehicle.currentDriver?.fullName || vehicle.driver?.name || "N/A"}
                              </span>
                            </div>
                            <div className="flex items-center">
                              <Phone className="w-3 h-3 mr-1 text-gray-400" />
                              <span className="text-gray-600">SĐT: </span>
                              <span className="font-medium">
                                {vehicle.currentDriver?.phone || vehicle.driver?.phone || "N/A"}
                              </span>
                            </div>
                          </div>
                        </div>
                      )}

                      {/* Maintenance History Preview */}
                      <div className="mt-4 pt-4 border-t border-gray-200">
                        <div className="flex items-center text-sm text-gray-600">
                          <Calendar className="w-4 h-4 mr-2" />
                          <span>Bảo trì gần nhất: </span>
                          <span className="font-medium">
                            {vehicle.lastMaintenance
                              ? new Date(vehicle.lastMaintenance).toLocaleDateString('vi-VN')
                              : "Chưa có"}
                          </span>
                        </div>
                      </div>
                    </div>

                    {/* Action Buttons */}
                    <div className="flex flex-col space-y-2 ml-4">
                      <button 
                        onClick={() => onViewDetails(vehicle)}
                        className="px-4 py-2 bg-blue-600 text-white text-sm font-medium rounded-lg hover:bg-blue-700 transition-colors"
                      >
                        Xem chi tiết
                      </button>
                      <button 
                        onClick={() => {
                          const emergencyRequests = emergencyRequestsByVehicleId[vehicle.id] || [];
                          const emergencyRequest = emergencyRequests.find((e) => e.status?.id === 51 || e.status?.name === 'MAINTENANCE_PENDING');
                          console.log('DEBUG emergencyRequests:', emergencyRequests);
                          console.log('DEBUG emergencyRequest:', emergencyRequest);
                          onScheduleMaintenance(vehicle, emergencyRequest);
                        }}
                        className="px-4 py-2 bg-green-600 text-white text-sm font-medium rounded-lg hover:bg-green-700 transition-colors"
                      >
                        Đặt lịch bảo trì
                      </button>
                    </div>
                  </div>
                </div>
              ))}
            </div>
          )}
        </div>

        {/* Footer */}
        <div className="flex items-center justify-end space-x-3 p-6 border-t border-gray-200 bg-gray-50">
          <button
            onClick={onClose}
            className="px-4 py-2 text-gray-700 bg-white border border-gray-300 rounded-lg hover:bg-gray-50 font-medium transition-colors"
          >
            Đóng
          </button>
          <button className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 font-medium transition-colors">
            Xuất báo cáo
          </button>
        </div>
      </div>
    </div>
  );
}