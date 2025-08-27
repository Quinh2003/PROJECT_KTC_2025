import React from "react";
import { Truck, CircleDot, Wrench, AlertTriangle } from "lucide-react";
import Navbar from "../../components/Navbar";
import Sidebar from "../../components/Sidebar";
import type { User } from "../../types/User";
import MaintenanceHistory from "./MaintenanceHistory";
import VehicleTable from "./VehicleTable";
import AddVehicleForm from "./AddVehicleForm";
import MaintenanceSchedulePage from "./MaintenanceSchedulePage";
import SearchAndFilter from "./SearchAndFilter";
import Pagination from "./Pagination";
import { useFleetDashboard } from "./useFleetDashboard";

interface FleetDashboardProps {
  user: User;
  onLogout: () => void;
}

// Memoized Stats Card Component
const StatsCard = React.memo<{
  title: string;
  value: number;
  icon: React.ReactNode;
  color: string;
}>(({ title, value, icon, color }) => (
  <div className={`bg-white/30 hover:bg-white/40 rounded-xl p-6 shadow-lg hover:shadow-xl transition-all duration-300 border-l-4 ${color}`}>
    <div className="flex items-center justify-between">
      <div>
        <p className="text-sm font-medium text-gray-600 mb-1">{title}</p>
        <p className="text-3xl font-bold text-gray-900">{value}</p>
      </div>
      <div className="flex-shrink-0">
        {icon}
      </div>
    </div>
  </div>
));

StatsCard.displayName = "StatsCard";

export default function FleetDashboard({ user, onLogout }: FleetDashboardProps) {
  const {
    // State
    tab,
    vehicles,
    searchTerm,
    statusFilter,
    isLoading,
    showAddForm,
    setShowAddForm,
    pagination,
    // Computed values
    fleetStats,
    filteredVehicles,
    // Handlers
    handleAddVehicle,
    handleTabChange,
    handleSearch,
    handleStatusFilter,
    handlePageChange,
  } = useFleetDashboard();

  return (
    <div className="min-h-screen flex bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100">
      <Sidebar
        activeTab={tab as any}
        onTabChange={tab => handleTabChange(tab as any)}
        role="fleet"
      />
      <main className="flex-1 flex flex-col">
        <Navbar
          user={user}
          onLogout={onLogout}
          title="Dashboard Quản lý đội xe"
          subtitle=""
        />
        <div className="p-6 md:p-10 space-y-8">
          {tab === "vehicles" && (
            <>
              <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-6">
                <StatsCard
                  title="Tổng phương tiện"
                  value={fleetStats.total}
                  icon={<Truck size={32} className="text-blue-600" />}
                  color="border-blue-500"
                />
                <StatsCard
                  title="Đang hoạt động"
                  value={fleetStats.active}
                  icon={<CircleDot size={32} className="text-green-600" />}
                  color="border-green-500"
                />
                <StatsCard
                  title="Đang bảo trì"
                  value={fleetStats.maintenance}
                  icon={<Wrench size={32} className="text-yellow-600" />}
                  color="border-yellow-500"
                />
                <StatsCard
                  title="Cần bảo trì"
                  value={fleetStats.needMaintenance}
                  icon={<AlertTriangle size={32} className="text-red-600" />}
                  color="border-red-500"
                />
              </div>
              <SearchAndFilter
                searchTerm={searchTerm}
                statusFilter={statusFilter}
                onSearch={handleSearch}
                onStatusFilter={handleStatusFilter}
                onToggleAddForm={() => setShowAddForm(!showAddForm)}
                showAddForm={showAddForm}
                resultsCount={filteredVehicles.length}
                totalCount={vehicles.length}
              />
              {showAddForm && (
                <div className="bg-white rounded-xl p-6 shadow-lg border-l-4 border-violet-500">
                  <div className="flex items-center justify-between mb-6">
                    <div>
                      <h3 className="text-xl font-bold text-gray-900">Thêm phương tiện mới</h3>
                      <p className="text-gray-600 mt-1">Đăng ký phương tiện mới vào hệ thống quản lý</p>
                    </div>
                    {isLoading && (
                      <div className="flex items-center gap-2 text-violet-600">
                        <div className="w-4 h-4 border-2 border-violet-600 border-t-transparent rounded-full animate-spin"></div>
                        <span className="text-sm">Đang thêm...</span>
                      </div>
                    )}
                  </div>
                  <AddVehicleForm onAdd={handleAddVehicle} />
                </div>
              )}
              <div className="bg-white/30 hover:bg-white/40 rounded-xl p-6 shadow-lg">
                <div className="flex items-center justify-between mb-6">
                  <div>
                    <h1 className="text-xl font-bold text-gray-900">Danh sách phương tiện</h1>
                  </div>
                </div>
                {filteredVehicles.length === 0 ? (
                  <div className="text-center py-12">
                    <Truck size={48} className="mx-auto text-gray-400 mb-4" />
                    <h4 className="text-lg font-medium text-gray-900 mb-2">
                      {searchTerm || statusFilter !== "all"
                        ? "Không tìm thấy phương tiện nào"
                        : "Chưa có phương tiện nào"}
                    </h4>
                    <p className="text-gray-600">
                      {searchTerm || statusFilter !== "all"
                        ? "Thử thay đổi bộ lọc hoặc từ khóa tìm kiếm"
                        : "Thêm phương tiện đầu tiên vào hệ thống"}
                    </p>
                  </div>
                ) : (
                  <>
                    <VehicleTable vehicles={filteredVehicles} />
                    <Pagination
                      page={pagination.page}
                      totalPages={pagination.totalPages}
                      onPageChange={handlePageChange}
                    />
                  </>
                )}
              </div>
            </>
          )}
          {tab === "maintenance" && (
            <div className="animate-fadeIn">
              <MaintenanceHistory />
            </div>
          )}
          {tab === "schedule" && (
            <div className="animate-fadeIn">
              <MaintenanceSchedulePage />
            </div>
          )}
        </div>
      </main>
    </div>
  );
}