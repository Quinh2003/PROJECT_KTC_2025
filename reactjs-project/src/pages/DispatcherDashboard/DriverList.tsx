import { useState } from "react";
import { useTranslation } from 'react-i18next';
import { editUser, updateUserStatus } from "../../services/adminAPI";
import type { User } from "../../types/User";
import { useDispatcherContext } from "../../contexts/DispatcherContext";

export default function DriverList() {
  const { t } = useTranslation();
  const { drivers, driversLoading, driversError, refreshDrivers } = useDispatcherContext();
  const [searchTerm, setSearchTerm] = useState("");
  const [updatingStatus, setUpdatingStatus] = useState<number | null>(null);

  const toggleDriverStatus = async (driverId: string | number, currentStatus: any) => {
    try {
      setUpdatingStatus(Number(driverId));
      
      // Xác định trạng thái mới dựa trên status hiện tại
      const currentStatusName = currentStatus?.name?.toLowerCase() || "";
      let newStatus: string;
      
      if (currentStatusName === "active") {
        newStatus = "Inactive";
      } else if (currentStatusName === "inactive") {
        newStatus = "Active";
      } else if (currentStatusName === "pending") {
        newStatus = "Inactive"; // Pending -> Inactive
      } else {
        newStatus = "Active"; // Default to Active
      }
      
      console.log(`[DriverList] Updating driver ${driverId} from ${currentStatus?.name} to ${newStatus}`);
      
      try {
        // Thử dùng endpoint riêng cho status trước
        console.log("[DriverList] Trying updateUserStatus endpoint...");
        try {
          await updateUserStatus(driverId, newStatus);
          console.log("[DriverList] updateUserStatus successful");
        } catch (statusError) {
          console.log("[DriverList] updateUserStatus failed, using editUser fallback...", statusError);
          throw statusError; // Force fallback to editUser
        }
      } catch (statusError) {
        console.log("[DriverList] Using editUser fallback...", statusError);
        // Nếu endpoint status không có, fallback về editUser
        const currentDriver = drivers.find(d => d.id === driverId);
        if (!currentDriver) {
          throw new Error(t('dashboard.dispatcher.drivers.driverNotFound', 'Driver not found'));
        }

        console.log("[DriverList] Current driver found:", currentDriver);

        // Map newStatus to correct statusId
        const statusMap: Record<string, number> = {
          "Active": 7,
          "Inactive": 8,
          "Pending": 1
        };

        const updatedDriver: User = {
          ...currentDriver,
          status: {
            id: statusMap[newStatus] || 7,
            name: newStatus,
            statusType: "USER",
            description: (typeof currentDriver.status === 'object' && currentDriver.status?.description) ? currentDriver.status.description : ""
          }
        };

        console.log("[DriverList] Payload for editUser:", updatedDriver);
        await editUser(driverId, updatedDriver);
        console.log("[DriverList] editUser successful");
      }
      
      console.log("[DriverList] API call successful, refreshing data...");
      
      // Refresh drivers data from context
      await refreshDrivers(true);
      
      console.log("[DriverList] Data refreshed successfully");
      
    } catch (err: any) {
      console.error("Failed to update driver status:", err);
      // Thông báo lỗi cho user
      alert(`Error updating driver status: ${err.message}`);
    } finally {
      setUpdatingStatus(null);
    }
  };

  const filteredDrivers = drivers.filter(driver => {
    const name = (driver.fullName ?? driver.name ?? "") as string;
    return (
      name.toLowerCase().includes(searchTerm.toLowerCase()) ||
      (driver.email?.toLowerCase().includes(searchTerm.toLowerCase())) ||
      (driver.phone && driver.phone.includes(searchTerm))
    );
  });

  const getStatusBadge = (status: any, driverId: string | number) => {
    // Kiểm tra nhiều trường hợp status name
    const statusName = status?.name?.toLowerCase() || "";
    const isActive = statusName === "active" || statusName === "pending"; // Coi Pending như Active cho test
    const isUpdating = updatingStatus === Number(driverId);
    
    console.log(`[DriverList] Status for driver ${driverId}:`, status, "isActive:", isActive);
    
    return (
      <button
        onClick={() => toggleDriverStatus(driverId, status)}
        disabled={isUpdating}
        className={`inline-flex items-center gap-2 px-3 py-1.5 rounded-full text-sm font-medium transition-all duration-200 hover:scale-105 disabled:opacity-50 disabled:cursor-not-allowed ${
          isActive 
            ? "bg-green-100 text-green-800 border border-green-200 hover:bg-green-200" 
            : "bg-gray-100 text-gray-600 border border-gray-200 hover:bg-gray-200"
        }`}
      >
        {isUpdating ? (
          <div className="animate-spin w-2 h-2 border border-current border-t-transparent rounded-full"></div>
        ) : (
          <div className={`w-2 h-2 rounded-full ${isActive ? "bg-green-500" : "bg-gray-400"}`}></div>
        )}
        <span className="text-xs opacity-75">{status?.name || "N/A"}</span>
      </button>
    );
  };

  return (
    <div className="space-y-6">
      {/* Header Section */}
        <div className="bg-gradient-to-r from-blue-600 to-indigo-700 rounded-2xl p-6 text-white shadow-lg">
        <div className="flex flex-col md:flex-row md:items-center md:justify-between gap-4">
          <div>
            <h2 className="text-2xl font-bold mb-2">{t('dashboard.dispatcher.drivers.title', 'Driver Management')}</h2>
          </div>
        </div>
      </div>      {/* Search and Filter Section */}
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
              placeholder={t('dashboard.dispatcher.drivers.searchPlaceholder', 'Search by name, email or phone...')}
              value={searchTerm}
              onChange={(e) => setSearchTerm(e.target.value)}
              className="w-full pl-10 pr-4 py-3 border border-gray-200 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent bg-white/90 backdrop-blur-sm"
            />
          </div>
          
        </div>
      </div>

      {/* Content Section */}
      <div className="bg-white/80 backdrop-blur-sm rounded-xl shadow-md border border-white/50 overflow-hidden">
        {driversLoading ? (
          <div className="flex items-center justify-center p-12">
            <div className="flex items-center gap-3">
              <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600"></div>
              <span className="text-gray-600 font-medium">{t('common.loading', 'Loading data...')}</span>
            </div>
          </div>
        ) : driversError ? (
          <div className="flex items-center justify-center p-12">
            <div className="text-center">
              <div className="w-12 h-12 bg-red-100 rounded-full flex items-center justify-center mx-auto mb-4">
                <svg className="w-6 h-6 text-red-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
                </svg>
              </div>
              <h3 className="text-lg font-semibold text-gray-900 mb-2">An error occurred</h3>
              <p className="text-red-600">{driversError}</p>
            </div>
          </div>
        ) : filteredDrivers.length === 0 ? (
          <div className="flex items-center justify-center p-12">
            <div className="text-center">
              <div className="w-12 h-12 bg-gray-100 rounded-full flex items-center justify-center mx-auto mb-4">
                <svg className="w-6 h-6 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 015.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 019.288 0M15 7a3 3 0 11-6 0 3 3 0 016 0zm6 3a2 2 0 11-4 0 2 2 0 014 0zM7 10a2 2 0 11-4 0 2 2 0 014 0z" />
                </svg>
              </div>
              <h3 className="text-lg font-semibold text-gray-900 mb-2">{t('dashboard.dispatcher.drivers.noDriversFound', 'No drivers found')}</h3>
              <p className="text-gray-500">
                {searchTerm ? "No drivers match the search criteria" : "No drivers in the system yet"}
                {searchTerm ? t('dashboard.dispatcher.drivers.noSearchResults', 'No drivers match your search criteria') : t('dashboard.dispatcher.drivers.noDriversInSystem', 'No drivers in the system yet')}
              </p>
            </div>
          </div>
        ) : (
          <>
            {/* Desktop Table View */}
            <div className="hidden md:block overflow-x-auto">
              <table className="min-w-full divide-y divide-gray-200">
                <thead className="bg-gray-50/80 backdrop-blur-sm">
                  <tr>
                    <th className="px-6 py-4 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Driver
                      {t('dashboard.dispatcher.drivers.name', 'Tài xế')}
                    </th>
                    <th className="px-6 py-4 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Contact Information
                      {t('dashboard.dispatcher.drivers.contactInfo', 'Thông tin liên hệ')}
                    </th>
                    <th className="px-6 py-4 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Status
                      {t('dashboard.dispatcher.drivers.status', 'Trạng thái')}
                    </th>
                  </tr>
                </thead>
                <tbody className="bg-white/60 backdrop-blur-sm divide-y divide-gray-200">
                  {filteredDrivers.map((driver) => (
                    <tr key={driver.id} className="hover:bg-white/80 transition-colors duration-200">
                      <td className="px-6 py-4 whitespace-nowrap">
                        <div className="flex items-center">
                          <div className="flex-shrink-0 h-12 w-12">
                            <div className="h-12 w-12 rounded-full bg-gradient-to-r from-blue-500 to-indigo-600 flex items-center justify-center text-white font-semibold text-lg">
                              {((driver.fullName ?? driver.name ?? "").toString().charAt(0) || "?").toUpperCase()}
                            </div>
                          </div>
                          <div className="ml-4">
                            <div className="text-sm font-medium text-gray-900">
                              {driver.fullName ?? driver.name ?? "(No name)"}
                              {driver.fullName ?? driver.name ?? t('common.noName', '(No name)')}
                            </div>
                            <div className="text-sm text-gray-500">ID: {driver.id}</div>
                          </div>
                        </div>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap">
                        <div className="text-sm text-gray-900">{driver.email}</div>
                        <div className="text-sm text-gray-500">{driver.phone || "Not updated"}</div>
                        <div className="text-sm text-gray-500">{driver.phone || t('common.notUpdated', 'Not updated')}</div>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap">
                        {getStatusBadge(driver.status, driver.id ?? "")}
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>

            {/* Mobile Card View */}
            <div className="md:hidden space-y-4 p-4">
              {filteredDrivers.map((driver) => (
                <div key={driver.id} className="bg-white/80 backdrop-blur-sm rounded-xl p-4 shadow-sm border border-gray-200">
                  <div className="flex items-start gap-4">
                    <div className="flex-shrink-0 h-12 w-12 rounded-full bg-gradient-to-r from-blue-500 to-indigo-600 flex items-center justify-center text-white font-semibold text-lg">
                      {((driver.fullName ?? driver.name ?? "").toString().charAt(0) || "?").toUpperCase()}
                    </div>
                    <div className="flex-1 min-w-0">
                      <div className="flex items-start justify-between">
                        <div>
                          <h3 className="text-sm font-medium text-gray-900 truncate">
                            {driver.fullName ?? driver.name}
                          </h3>
                          <p className="text-sm text-gray-500">ID: {driver.id}</p>
                        </div>
                        {getStatusBadge(driver.status, driver.id ?? "")}
                      </div>
                      <div className="mt-2 space-y-1">
                        <p className="text-sm text-gray-600">{driver.email}</p>
                        <p className="text-sm text-gray-600">{driver.phone || "Phone not updated"}</p>
                        <p className="text-sm text-gray-600">{driver.phone || t('common.noPhone', 'No phone number')}</p>
                      </div>
                    </div>
                  </div>
                </div>
              ))}
            </div>
          </>
        )}
      </div>
    </div>
  );
}