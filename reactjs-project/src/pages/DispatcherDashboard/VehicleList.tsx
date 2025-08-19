import { useState } from "react";
import { assignDriverToVehicle, updateVehicleStatus } from "../../services/VehicleListAPI";
import type { Vehicle } from "../../types/Operations";
import type { User } from "../../types/User";
import { useDispatcherContext } from "../../contexts/DispatcherContext";

export default function VehicleList() {
  const { 
    vehicles, 
    vehiclesLoading, 
    vehiclesError, 
    refreshVehicles, 
    updateVehicleInList,
    drivers,
    driversLoading,
    driversError,
    refreshDrivers
  } = useDispatcherContext();

  const [showAssignModal, setShowAssignModal] = useState(false);
  const [selectedVehicle, setSelectedVehicle] = useState<Vehicle | null>(null);
  const [selectedDriverId, setSelectedDriverId] = useState<string | number | null>(null);
  const [assignError, setAssignError] = useState("");
  const [assignSuccess, setAssignSuccess] = useState("");
  const [assigning, setAssigning] = useState(false);
  const [searchTerm, setSearchTerm] = useState("");
  const [statusFilter, setStatusFilter] = useState("all");
  const [updatingStatus, setUpdatingStatus] = useState<string | number | null>(null);

  const handleAssignDriver = async () => {
    if (!selectedVehicle) return;
    setAssigning(true);
    setAssignError("");
    setAssignSuccess("");
    try {
      if (!selectedDriverId) {
        // Nếu chọn 'Chưa gán tài xế', gửi driverId là null
        await assignDriverToVehicle(selectedVehicle.id, null);
        setAssignSuccess("Đã bỏ gán tài xế!");
      } else {
        const driverObj = drivers.find(d => String(d.id) === String(selectedDriverId));
        if (!driverObj) throw new Error("Không tìm thấy tài xế");
        await assignDriverToVehicle(selectedVehicle.id, driverObj.id ?? "");
        setAssignSuccess("Gán tài xế thành công!");
      }
      await refreshVehicles(true); // Force refresh vehicles
      setTimeout(() => {
        closeAssignModal();
      }, 1000);
    } catch (err: any) {
      setAssignError(err.message || "Gán tài xế thất bại");
    } finally {
      setAssigning(false);
    }
  };

  const openAssignModal = async (vehicle: Vehicle) => {
    setSelectedVehicle(vehicle);
    setShowAssignModal(true);
    setAssignError("");
    setSelectedDriverId(null);
    
    // Refresh drivers if needed
    if (drivers.length === 0) {
      await refreshDrivers(true);
    }
  };

  const closeAssignModal = () => {
    setShowAssignModal(false);
    setSelectedVehicle(null);
    setSelectedDriverId(null);
    setAssignError("");
    setAssignSuccess("");
  };

  const toggleVehicleStatus = async (vehicleId: string | number, currentStatus: any) => {
    try {
      setUpdatingStatus(vehicleId);
      // Chỉ chuyển đổi giữa AVAILABLE <-> IN_USE
      let newStatus = "AVAILABLE";
      if (currentStatus?.name === "AVAILABLE") {
        newStatus = "IN_USE";
      } else if (currentStatus?.name === "IN_USE") {
        newStatus = "AVAILABLE";
      } else if (currentStatus?.name === "MAINTENANCE") {
        // Nếu đang bảo trì, chuyển về AVAILABLE (hoặc có thể bỏ qua tuỳ nghiệp vụ)
        newStatus = "AVAILABLE";
      }
      await updateVehicleStatus(vehicleId, newStatus);
      
      // Update vehicle in context
      updateVehicleInList(vehicleId, {
        status: {
          id: 1, // temporary id
          name: newStatus,
          statusType: "VEHICLE_STATUS",
          description: ""
        }
      });
    } catch (err: any) {
      console.error("Failed to update vehicle status:", err);
      alert(`Lỗi cập nhật trạng thái xe: ${err.message}`);
    } finally {
      setUpdatingStatus(null);
    }
  };

  const filteredVehicles = vehicles.filter(vehicle => {
    const matchesSearch = 
      vehicle.licensePlate.toLowerCase().includes(searchTerm.toLowerCase()) ||
      vehicle.vehicleType.toLowerCase().includes(searchTerm.toLowerCase()) ||
      (vehicle.currentDriver?.fullName || vehicle.currentDriver?.username || "").toLowerCase().includes(searchTerm.toLowerCase());
    
    const matchesStatus = statusFilter === "all" || 
      (statusFilter === "available" && vehicle.status?.name === "AVAILABLE") ||
      (statusFilter === "assigned" && vehicle.currentDriver) ||
      (statusFilter === "unassigned" && !vehicle.currentDriver);
    
    return matchesSearch && matchesStatus;
  });

  const getStatusBadge = (vehicle: Vehicle) => {
    const statusName = vehicle.status?.name;
    const isUpdating = updatingStatus === vehicle.id;
    let badgeProps = {
      color: '',
      text: '',
      border: '',
      bg: '',
      icon: '',
    };
    switch (statusName) {
      case 'AVAILABLE':
        badgeProps = {
          color: 'green-800',
          text: 'Sẵn sàng',
          border: 'green-200',
          bg: 'green-100',
          icon: 'green-500',
        };
        break;
      case 'IN_USE':
        badgeProps = {
          color: 'red-800',
          text: 'Đang sử dụng',
          border: 'red-200',
          bg: 'red-100',
          icon: 'red-500',
        };
        break;
      case 'MAINTENANCE':
        badgeProps = {
          color: 'gray-800',
          text: 'Bảo trì',
          border: 'gray-200',
          bg: 'gray-100',
          icon: 'gray-500',
        };
        break;
      default:
        badgeProps = {
          color: 'gray-800',
          text: 'Không xác định',
          border: 'gray-200',
          bg: 'gray-100',
          icon: 'gray-500',
        };
    }
    return (
      <button
        onClick={() => toggleVehicleStatus(vehicle.id, vehicle.status)}
        disabled={isUpdating}
        className={`inline-flex items-center gap-1 px-3 py-1 rounded-full text-xs font-semibold bg-${badgeProps.bg} text-${badgeProps.color} border border-${badgeProps.border} hover:bg-${badgeProps.bg.replace('100','200')} hover:scale-105 transition-all duration-200 disabled:opacity-50 disabled:cursor-not-allowed`}
      >
        {isUpdating ? (
          <div className="animate-spin w-2 h-2 border border-current border-t-transparent rounded-full"></div>
        ) : (
          <div className={`w-2 h-2 bg-${badgeProps.icon} rounded-full`}></div>
        )}
        {badgeProps.text}
      </button>
    );
  };

  return (
    <div className="space-y-6">
      {/* Header Section */}
      <div className="bg-gradient-to-r from-blue-600 to-indigo-700 rounded-2xl p-6 text-white shadow-lg">
        <div className="flex flex-col md:flex-row md:items-center md:justify-between gap-4">
          <div>
            <h2 className="text-2xl font-bold mb-2">Quản lý phương tiện</h2>
            <button
              onClick={() => refreshVehicles(true)}
              disabled={vehiclesLoading}
              className="px-4 py-2 bg-white/20 hover:bg-white/30 disabled:opacity-50 text-white rounded-lg transition-colors duration-200 flex items-center gap-2"
            >
              {vehiclesLoading ? (
                <>
                  <div className="animate-spin w-4 h-4 border-2 border-white border-t-transparent rounded-full"></div>
                  Đang tải...
                </>
              ) : (
                <>
                  <svg className="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15" />
                  </svg>
                  Làm mới
                </>
              )}
            </button>
          </div>
          <div className="grid grid-cols-3 gap-4">
            
          </div>
        </div>
      </div>

      {/* Search and Filter Section */}
      <div className="bg-white/80 backdrop-blur-sm rounded-xl p-6 shadow-md border border-white/50">
        <div className="flex flex-col md:flex-row gap-4 items-center">
          <div className="relative flex-1">
            <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
              <svg className="h-5 w-5 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />
              </svg>
            </div>
            <input
              type="text"
              placeholder="Tìm kiếm theo biển số, loại xe hoặc tài xế..."
              value={searchTerm}
              onChange={(e) => setSearchTerm(e.target.value)}
              className="w-full pl-10 pr-4 py-3 border border-gray-200 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent bg-white/90 backdrop-blur-sm"
            />
          </div>
          <select
            value={statusFilter}
            onChange={(e) => setStatusFilter(e.target.value)}
            className="px-4 py-3 border border-gray-200 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent bg-white/90 backdrop-blur-sm"
          >
            <option value="all">Tất cả trạng thái</option>
            <option value="available">Sẵn sàng</option>
            <option value="assigned">Có tài xế</option>
            <option value="unassigned">Chưa có tài xế</option>
          </select>
          
        </div>
      </div>

      {/* Content Section */}
      <div className="bg-white/80 backdrop-blur-sm rounded-xl shadow-md border border-white/50">
        {vehiclesLoading ? (
          <div className="flex items-center justify-center p-12">
            <div className="flex items-center gap-3">
              <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600"></div>
              <span className="text-gray-600 font-medium">Đang tải dữ liệu...</span>
            </div>
          </div>
        ) : vehiclesError ? (
          <div className="flex items-center justify-center p-12">
            <div className="text-center">
              <div className="w-12 h-12 bg-red-100 rounded-full flex items-center justify-center mx-auto mb-4">
                <svg className="w-6 h-6 text-red-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
                </svg>
              </div>
              <h3 className="text-lg font-semibold text-gray-900 mb-2">Có lỗi xảy ra</h3>
              <p className="text-red-600">{vehiclesError}</p>
            </div>
          </div>
        ) : filteredVehicles.length === 0 ? (
          <div className="flex items-center justify-center p-12">
            <div className="text-center">
              <div className="w-12 h-12 bg-gray-100 rounded-full flex items-center justify-center mx-auto mb-4">
                <svg className="w-6 h-6 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 17a2 2 0 11-4 0 2 2 0 014 0zM19 17a2 2 0 11-4 0 2 2 0 014 0z" />
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M13 16V6a1 1 0 00-1-1H4a1 1 0 00-1 1v10a1 1 0 001 1h1m8-1a1 1 0 01-1 1H9m4-1V8a1 1 0 011-1h2.586a1 1 0 01.707.293l2.414 2.414a1 1 0 01.293.707V16a1 1 0 01-1 1h-1m-6-1a1 1 0 001 1h1M5 17a2 2 0 104 0M15 17a2 2 0 104 0" />
                </svg>
              </div>
              <h3 className="text-lg font-semibold text-gray-900 mb-2">Không tìm thấy phương tiện</h3>
              <p className="text-gray-500">
                {searchTerm || statusFilter !== "all" ? "Không có phương tiện nào phù hợp với bộ lọc" : "Chưa có phương tiện nào trong hệ thống"}
              </p>
            </div>
          </div>
        ) : (
          <div className="space-y-4 p-6">
            {filteredVehicles.map((vehicle) => (
              <div key={vehicle.id} className="bg-white/90 backdrop-blur-sm rounded-xl border border-gray-200 shadow-sm hover:shadow-md transition-all duration-300 overflow-hidden">
                <div className="flex items-center p-4 gap-4">
                  {/* Vehicle Icon & Info */}
                  <div className="flex items-center gap-4 flex-1">
                    <div className="w-12 h-12 bg-gradient-to-r from-blue-500 to-indigo-600 rounded-lg flex items-center justify-center flex-shrink-0">
                      <svg className="w-6 h-6 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 17a2 2 0 11-4 0 2 2 0 014 0zM19 17a2 2 0 11-4 0 2 2 0 014 0z" />
                        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M13 16V6a1 1 0 00-1-1H4a1 1 0 00-1 1v10a1 1 0 001 1h1m8-1a1 1 0 01-1 1H9m4-1V8a1 1 0 011-1h2.586a1 1 0 01.707.293l2.414 2.414a1 1 0 01.293.707V16a1 1 0 01-1 1h-1m-6-1a1 1 0 001 1h1M5 17a2 2 0 104 0M15 17a2 2 0 104 0" />
                      </svg>
                    </div>
                    <div className="min-w-0">
                      <h3 className="font-bold text-gray-900 text-lg">{vehicle.licensePlate}</h3>
                      <p className="text-sm text-gray-600 uppercase tracking-wide">{vehicle.vehicleType}</p>
                    </div>
                  </div>

                  {/* Status Badge */}
                  <div className="flex-shrink-0">
                    {getStatusBadge(vehicle)}
                  </div>
                </div>

                {/* Driver Info */}
                <div className="px-4 pb-2">
                  <div className="flex items-center gap-3">
                    <div className="w-6 h-6 bg-blue-100 rounded-full flex items-center justify-center flex-shrink-0">
                      <svg className="w-3 h-3 text-blue-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z" />
                      </svg>
                    </div>
                    <div className="flex-1 min-w-0">
                      <span className="text-sm text-gray-500">Tài xế: </span>
                      {vehicle.currentDriver ? (
                        <span className="font-medium text-gray-900">
                          {vehicle.currentDriver.fullName || vehicle.currentDriver.username}
                        </span>
                      ) : (
                        <span className="text-gray-400 italic">Chưa có tài xế</span>
                      )}
                    </div>
                  </div>
                </div>

                {/* Specifications */}
                <div className="px-4 pb-2">
                  <div className="flex gap-8 text-sm">
                    <div>
                      <span className="text-gray-500">Trọng tải: </span>
                      <span className="font-semibold text-gray-900">{vehicle.capacityWeightKg || "-"} tấn</span>
                    </div>
                    <div>
                      <span className="text-gray-500">Thể tích: </span>
                      <span className="font-semibold text-gray-900">{vehicle.capacityVolumeM3 || "-"} m³</span>
                    </div>
                  </div>
                </div>

                {/* Notes (if exists) */}
                {vehicle.notes && (
                  <div className="px-4 pb-2">
                    <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-2">
                      <span className="text-xs text-yellow-700">Ghi chú: </span>
                      <span className="text-sm text-yellow-800">{vehicle.notes}</span>
                    </div>
                  </div>
                )}

                {/* Footer with Update Time and Action */}
                <div className="px-4 py-3 bg-gray-50 border-t border-gray-200 flex items-center justify-between">
                  <span className="text-xs text-gray-400">
                    Cập nhật: {vehicle.updatedAt ? new Date(vehicle.updatedAt).toLocaleString('vi-VN') : "-"}
                  </span>
                  <button
                    onClick={() => openAssignModal(vehicle)}
                    className="bg-gradient-to-r from-blue-500 to-indigo-600 hover:from-blue-600 hover:to-indigo-700 text-white font-semibold py-2 px-4 rounded-lg transition-all duration-200 shadow-sm hover:shadow-md text-sm"
                  >
                    Gán tài xế
                  </button>
                </div>
              </div>
            ))}
          </div>
        )}
      </div>

      {/* Assign Driver Modal */}
      {showAssignModal && (
        <div className="fixed inset-0 bg-black/50 backdrop-blur-sm flex items-center justify-center z-50 p-4">
          <div className="bg-white rounded-2xl shadow-2xl w-full max-w-md transform transition-all duration-300 scale-100">
            {/* Modal Header */}
            <div className="bg-gradient-to-r from-blue-500 to-indigo-600 text-white p-6 rounded-t-2xl">
              <div className="flex items-center justify-between">
                <h2 className="text-xl font-bold">Gán tài xế cho xe</h2>
                <button
                  onClick={closeAssignModal}
                  className="w-8 h-8 bg-white/20 hover:bg-white/30 rounded-full flex items-center justify-center transition-colors duration-200"
                >
                  <svg className="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M6 18L18 6M6 6l12 12" />
                  </svg>
                </button>
              </div>
              <p className="text-blue-100 mt-2">
                Biển số: <span className="font-semibold">{selectedVehicle?.licensePlate}</span>
              </p>
            </div>

            {/* Modal Content */}
            <div className="p-6">
              {driversLoading ? (
                <div className="flex items-center justify-center py-8">
                  <div className="flex items-center gap-3">
                    <div className="animate-spin rounded-full h-6 w-6 border-b-2 border-blue-600"></div>
                    <span className="text-gray-600">Đang tải danh sách tài xế...</span>
                  </div>
                </div>
              ) : (
                <>
                  {/* Alert Messages */}
                  {assignError && (
                    <div className="mb-4 p-4 bg-red-50 border border-red-200 rounded-lg">
                      <div className="flex items-center gap-2">
                        <svg className="w-5 h-5 text-red-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
                        </svg>
                        <span className="text-red-800 font-medium">{assignError}</span>
                      </div>
                    </div>
                  )}
                  
                  {assignSuccess && (
                    <div className="mb-4 p-4 bg-green-50 border border-green-200 rounded-lg">
                      <div className="flex items-center gap-2">
                        <svg className="w-5 h-5 text-green-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M5 13l4 4L19 7" />
                        </svg>
                        <span className="text-green-800 font-medium">{assignSuccess}</span>
                      </div>
                    </div>
                  )}

                  {/* Driver Selection */}
                  <div className="space-y-4">
                    <label className="block text-sm font-medium text-gray-700">
                      Chọn tài xế
                    </label>
                    <select
                      value={selectedDriverId ?? ""}
                      onChange={(e) => setSelectedDriverId(e.target.value)}
                      disabled={assigning}
                      className="w-full px-4 py-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent bg-white disabled:bg-gray-50 disabled:cursor-not-allowed"
                    >
                      <option value="">-- Chưa gán tài xế --</option>
                      {drivers.map(driver => (
                        <option key={driver.id} value={driver.id}>
                          {driver.fullName || driver.username} ({driver.email})
                        </option>
                      ))}
                    </select>

                    {/* Action Buttons */}
                    <div className="flex gap-3 pt-4">
                      <button
                        onClick={closeAssignModal}
                        disabled={assigning}
                        className="flex-1 px-4 py-3 border border-gray-300 text-gray-700 rounded-lg hover:bg-gray-50 font-medium transition-colors duration-200 disabled:opacity-50 disabled:cursor-not-allowed"
                      >
                        Hủy
                      </button>
                      <button
                        onClick={handleAssignDriver}
                        disabled={assigning}
                        className="flex-1 px-4 py-3 bg-gradient-to-r from-blue-500 to-indigo-600 hover:from-blue-600 hover:to-indigo-700 text-white rounded-lg font-medium transition-all duration-200 disabled:opacity-50 disabled:cursor-not-allowed disabled:hover:from-blue-500 disabled:hover:to-indigo-600"
                      >
                        {assigning ? (
                          <div className="flex items-center justify-center gap-2">
                            <div className="animate-spin rounded-full h-4 w-4 border-b-2 border-white"></div>
                            Đang gán...
                          </div>
                        ) : (
                          "Xác nhận gán"
                        )}
                      </button>
                    </div>
                  </div>
                </>
              )}
            </div>
          </div>
        </div>
      )}
    </div>
  );
}