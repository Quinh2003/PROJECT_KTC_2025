import { fetchEmergencyRequestsByVehicleId } from "../../services/VehicleMaintenanceAPI";
import React, { useState } from "react";
import { useTranslation } from 'react-i18next';
import { Truck, CircleDot, Wrench, AlertTriangle } from "lucide-react";
import { MdManageAccounts } from "react-icons/md";
import { AiOutlineSetting } from "react-icons/ai";
import { FiActivity } from "react-icons/fi";
import Navbar from "../../components/Navbar";
import Sidebar from "../../components/Sidebar";
import type { User } from "../../types/User";
import type { Vehicle } from "../../types/Operations";
import MaintenanceHistory from "./MaintenanceHistory";
import VehicleTable from "./VehicleTable";
import AddVehicleForm from "./AddVehicleForm";
import SearchAndFilter from "./SearchAndFilter";
import Pagination from "./Pagination";
import { useFleetDashboard } from "./useFleetDashboard";
import MaintenanceForm from "./MaintenanceForm";
import MaintenanceModal from "./MaintenanceModal";
import VehicleDetailModal from "./VehicleDetailModal";

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
  onClick?: () => void;
  style?: React.CSSProperties;
}>(({ title, value, icon, color, onClick, style }) => (
  <div
    className={`bg-white/30 hover:bg-white/40 rounded-xl p-3 md:p-6 shadow-lg hover:shadow-xl transition-all duration-300 border-l-4 ${color} ${
      onClick ? "cursor-pointer" : ""
    }`}
    onClick={onClick}
    style={style}
  >
    <div className="flex items-center justify-between">
      <div>
        <p className="text-xs md:text-sm font-medium text-gray-600 mb-1">{title}</p>
        <p className="text-lg md:text-3xl font-bold text-gray-900">{value}</p>
      </div>
      <div className="flex-shrink-0 text-lg md:text-3xl">{icon}</div>
    </div>
  </div>
));

StatsCard.displayName = "StatsCard";

export default function FleetDashboard({
  user,
  onLogout,
}: FleetDashboardProps) {
  const { t } = useTranslation();
  const {
    // State
    tab,
    vehicles,
    searchTerm,
    statusFilter,
    isLoading,
  showAddForm,
  setShowAddForm,
  showEditForm,
  editingVehicle,
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
    handleDeleteVehicle,
    handleEditVehicle,
    handleUpdateVehicle,
    handleCancelEdit,
    refreshVehicles, // Add this new handler
  } = useFleetDashboard();

  // State lưu emergency requests cho từng xe: { [vehicleId]: emergencyRequest[] }
  const [emergencyRequestsByVehicleId, setEmergencyRequestsByVehicleId] =
    useState<Record<string, any[]>>({});

  // Fetch emergency requests cho tất cả xe cần bảo trì khi load dashboard
  React.useEffect(() => {
    async function fetchAllEmergencyRequests() {
      const requests: Record<string, any[]> = {};
      for (const v of vehicles) {
        if (v.status === "MAINTENANCE_PENDING") {
          try {
            const data = await fetchEmergencyRequestsByVehicleId(v.id);
            console.log(
              "DEBUG fetchEmergencyRequestsByVehicleId for vehicle",
              v.id,
              data
            );
            requests[v.id] = data;
          } catch (e) {
            console.log(
              "DEBUG fetchEmergencyRequestsByVehicleId ERROR for vehicle",
              v.id,
              e
            );
            requests[v.id] = [];
          }
        }
      }
      console.log("DEBUG emergencyRequestsByVehicleId mapping:", requests);
      setEmergencyRequestsByVehicleId(requests);
    }
    if (vehicles.length > 0) fetchAllEmergencyRequests();
  }, [vehicles]);

  // State để lưu thông tin xe và emergency request khi đặt lịch
  const [selectedMaintenance, setSelectedMaintenance] = useState<{
    vehicle: Vehicle;
    emergencyRequest?: any;
  } | null>(null);

  // State for maintenance modal
  const [showMaintenanceModal, setShowMaintenanceModal] = useState(false);

  // State for vehicle detail modal
  const [showVehicleDetailModal, setShowVehicleDetailModal] = useState(false);
  const [selectedVehicle, setSelectedVehicle] = useState<Vehicle | null>(null);

  // State for maintenance form modal
  const [showMaintenanceFormModal, setShowMaintenanceFormModal] =
    useState(false);

  function handleAddMaintenance(data: any): void {
    // TODO: Gửi dữ liệu bảo trì lên server hoặc cập nhật state
    // Hiện tại chỉ log ra console
    console.log("Add maintenance:", data);
  }

  // Get vehicles that need maintenance
  const vehiclesNeedMaintenance = vehicles.filter(
    (v) => v.status === "MAINTENANCE_PENDING"
  );

  // Handler to open maintenance modal
  const handleShowMaintenanceModal = () => {
    setShowMaintenanceModal(true);
  };

  // Handler to view vehicle details
  const handleViewVehicleDetails = (vehicle: Vehicle) => {
    setSelectedVehicle(vehicle);
    setShowVehicleDetailModal(true);
    setShowMaintenanceModal(false); // Close maintenance modal
  };

  // Handler to schedule maintenance
  const handleScheduleMaintenance = async (
    vehicle: Vehicle,
    emergencyRequest?: any
  ) => {
    console.log("DEBUG handleScheduleMaintenance called with:", {
      vehicle: vehicle.licensePlate,
      emergencyRequest,
    });

    let finalEmergencyRequest = emergencyRequest;

    // Nếu không có emergency request từ props, fetch từ API
    if (!finalEmergencyRequest || !finalEmergencyRequest.id) {
      try {
        console.log(
          "DEBUG Fetching emergency request for vehicle:",
          vehicle.id
        );
        const requests = await fetchEmergencyRequestsByVehicleId(vehicle.id);
        console.log("DEBUG Fetched emergency requests:", requests);

        // Tìm request có status MAINTENANCE_PENDING (id=51)
        finalEmergencyRequest = requests.find(
          (req: any) =>
            req.status?.id === 51 || req.status?.name === "MAINTENANCE_PENDING"
        );

        console.log("DEBUG Found emergency request:", finalEmergencyRequest);
      } catch (error) {
        console.error("DEBUG Error fetching emergency request:", error);
        finalEmergencyRequest = null;
      }
    }

    console.log("DEBUG Setting selectedMaintenance with:", {
      vehicle: vehicle.licensePlate,
      emergencyRequestId: finalEmergencyRequest?.id,
      hasId: Boolean(finalEmergencyRequest?.id),
    });

    setSelectedMaintenance({
      vehicle,
      emergencyRequest: finalEmergencyRequest,
    });
    setShowMaintenanceFormModal(true);
    setShowMaintenanceModal(false);
  };

  return (
    <div className="min-h-screen flex bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100">
      {/* Sidebar - Hidden on mobile, visible on md+ */}
      <div className="hidden md:block">
        <Sidebar
          // eslint-disable-next-line @typescript-eslint/no-explicit-any
          activeTab={tab as any}
          // eslint-disable-next-line @typescript-eslint/no-explicit-any
          onTabChange={(tab) => handleTabChange(tab as any)}
          role="fleet"
        />
      </div>
      {/* Main content */}
      <main className="flex-1 flex flex-col bg-transparent min-h-screen w-full md:w-auto">
        <Navbar
          user={user}
          onLogout={onLogout}
          title={t('dashboard.fleet.title', 'Fleet Management Dashboard')}
          subtitle={t('dashboard.fleet.subtitle', 'Manage vehicles and maintenance')}
        />
        {/* Mobile Navigation - Tab bar at bottom for mobile */}
        <div className="md:hidden fixed bottom-0 left-0 right-0 bg-white/90 backdrop-blur-lg border-t border-white/30 px-4 py-2 z-50">
          <div className="flex justify-around items-center">
            <button
              onClick={() => handleTabChange("vehicles")}
              className={`flex flex-col items-center py-2 px-1 ${tab === "vehicles" ? "text-blue-600" : "text-gray-600"}`}
            >
              <MdManageAccounts className="text-xl mb-1" />
              <span className="text-xs">Phương tiện</span>
            </button>
            <button
              onClick={() => handleTabChange("maintenance")}
              className={`flex flex-col items-center py-2 px-1 ${tab === "maintenance" ? "text-blue-600" : "text-gray-600"}`}
            >
              <AiOutlineSetting className="text-xl mb-1" />
              <span className="text-xs">Bảo trì</span>
            </button>
            <button
              onClick={() => handleTabChange("schedule")}
              className={`flex flex-col items-center py-2 px-1 ${tab === "schedule" ? "text-blue-600" : "text-gray-600"}`}
            >
              <FiActivity className="text-xl mb-1" />
              <span className="text-xs">Lịch</span>
            </button>
          </div>
        </div>
        <div className="p-3 md:p-10 space-y-6 md:space-y-8 pb-16 md:pb-0">
          {tab === "vehicles" && (
            <>
              <div className="relative mb-4">
                <button
                  onClick={refreshVehicles}
                  className="absolute right-0 top-0 flex items-center gap-2 px-3 md:px-4 py-2 bg-gradient-to-r from-violet-500 to-indigo-500 hover:from-violet-600 hover:to-indigo-600 text-white rounded-full font-semibold shadow-lg transition-all duration-200 focus:outline-none focus:ring-2 focus:ring-violet-400"
                  title={t('dashboard.fleet.refreshVehicles', 'Refresh vehicle list')}
                >
                  <svg className="animate-spin-slow" width="20" height="20" fill="none" stroke="currentColor" strokeWidth="2" viewBox="0 0 24 24"><path d="M4.93 4.93a10 10 0 1 1-1.32 2.09"/><path d="M4 4V8h4"/></svg>
                  <span className="hidden md:inline">{t('common.refresh')}</span>
                </button>
              </div>
              <div className="grid grid-cols-2 md:grid-cols-4 gap-3 md:gap-6">
                <StatsCard
                  title={t('dashboard.fleet.stats.totalVehicles', 'Total Vehicles')}
                  value={fleetStats.total}
                  icon={<Truck size={32} className="text-blue-600" />}
                  color="border-blue-500"
                />
                <StatsCard
                  title={t('dashboard.fleet.stats.inUse', 'In Use')}
                  value={fleetStats.inUse}
                  icon={<CircleDot size={32} className="text-green-600" />}
                  color="border-green-500"
                />
                <StatsCard
                  title={t('dashboard.fleet.stats.maintenance', 'Under Maintenance')}
                  value={fleetStats.maintenance}
                  icon={<Wrench size={32} className="text-yellow-600" />}
                  color="border-yellow-500"
                />
                <StatsCard
                  title={t('dashboard.fleet.stats.needMaintenance', 'Need Maintenance')}
                  value={fleetStats.needMaintenance}
                  icon={<AlertTriangle size={32} className="text-red-600" />}
                  color="border-red-500"
                  onClick={handleShowMaintenanceModal}
                  style={{ cursor: "pointer" }}
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
                <div className="bg-white rounded-xl p-3 md:p-6 shadow-lg border-l-4 border-violet-500">
                  <div className="flex items-center justify-between mb-4 md:mb-6">
                    <div>
                      <h3 className="text-lg md:text-xl font-bold text-gray-900">
                        {t('fleet.form.addVehicle', 'Thêm phương tiện mới')}
                      </h3>
                      <p className="text-gray-600 mt-1 text-sm md:text-base">
                        {t('fleet.form.addVehicleSubtitle', 'Đăng ký phương tiện mới vào hệ thống quản lý')}
                      </p>
                    </div>
                    {isLoading && (
                      <div className="flex items-center gap-2 text-violet-600">
                        <div className="w-4 h-4 border-2 border-violet-600 border-t-transparent rounded-full animate-spin"></div>
                        <span className="text-sm">{t('fleet.form.adding', 'Đang thêm...')}</span>
                      </div>
                    )}
                  </div>
                  <AddVehicleForm onSuccess={handleAddVehicle} />
                </div>
              )}
              {showEditForm && editingVehicle && (
                <AddVehicleForm
                  mode="edit"
                  editingVehicle={editingVehicle}
                  onUpdate={handleUpdateVehicle}
                  onCancel={handleCancelEdit}
                />
              )}
              <div className="bg-white/30 hover:bg-white/40 rounded-xl p-3 md:p-6 shadow-lg">
                <div className="flex items-center justify-between mb-4 md:mb-6">
                  <div>
                    <h1 className="text-lg md:text-xl font-bold text-gray-900">
                      {t('fleet.tabs.vehicles', 'Vehicle Management')}
                    </h1>
                  </div>
                </div>
                {filteredVehicles.length === 0 ? (
                  <div className="text-center py-12">
                    <Truck size={48} className="mx-auto text-gray-400 mb-4" />
                    <h4 className="text-lg font-medium text-gray-900 mb-2">
                      {searchTerm || statusFilter !== "all"
                        ? t('fleet.emptyState.noResults', 'No vehicles found')
                        : t('fleet.emptyState.noVehicles', 'No vehicles yet')}
                    </h4>
                    <p className="text-gray-600">
                      {searchTerm || statusFilter !== "all"
                        ? t('fleet.emptyState.tryFilters', 'Try changing filters or search terms')
                        : t('fleet.emptyState.addFirst', 'Add your first vehicle to the system')}
                    </p>
                  </div>
                ) : (
                  <>
                    <VehicleTable
                      vehicles={filteredVehicles.slice(
                        (pagination.page - 1) * pagination.size,
                        pagination.page * pagination.size
                      )}
                      onEdit={handleEditVehicle}
                      onDelete={(vehicleId) =>
                        handleDeleteVehicle(Number(vehicleId))
                      }
                    />
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
              <MaintenanceForm
                onAddMaintenance={handleAddMaintenance}
                onMaintenanceCreated={refreshVehicles}
              />
            </div>
          )}
        </div>
      </main>

      {/* Maintenance Modal */}
      <MaintenanceModal
        isOpen={showMaintenanceModal}
        onClose={() => setShowMaintenanceModal(false)}
        vehicles={vehiclesNeedMaintenance}
        emergencyRequestsByVehicleId={emergencyRequestsByVehicleId}
        onViewDetails={handleViewVehicleDetails}
        onScheduleMaintenance={handleScheduleMaintenance}
      />

      {/* Vehicle Detail Modal */}
      {selectedVehicle && (
        <VehicleDetailModal
          vehicle={selectedVehicle}
          isOpen={showVehicleDetailModal}
          onClose={() => {
            setShowVehicleDetailModal(false);
            setSelectedVehicle(null);
          }}
          onScheduleMaintenance={handleScheduleMaintenance}
        />
      )}

      {/* Maintenance Form Modal */}
      {showMaintenanceFormModal && selectedMaintenance && (
        <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50 p-4">
          <div className="bg-white rounded-lg p-4 md:p-6 w-full max-w-2xl max-h-[90vh] overflow-y-auto">
            <div className="flex justify-between items-center mb-4 md:mb-6">
              <h2 className="text-lg md:text-xl font-semibold text-gray-900">
                Đặt lịch bảo trì - {selectedMaintenance.vehicle.licensePlate}
              </h2>
              <button
                onClick={() => {
                  setShowMaintenanceFormModal(false);
                  setSelectedMaintenance(null);
                }}
                className="text-gray-400 hover:text-gray-600 text-xl md:text-2xl"
              >
                ×
              </button>
            </div>
            <MaintenanceForm
              initialVehicle={selectedMaintenance.vehicle}
              initialDescription={
                selectedMaintenance.emergencyRequest?.description || ""
              }
              initialType={
                selectedMaintenance.emergencyRequest?.maintenanceType || ""
              }
              initialMaintenanceId={selectedMaintenance.emergencyRequest?.id}
              onAddMaintenance={handleAddMaintenance}
              onMaintenanceCreated={() => {
                console.log(
                  "DEBUG: Maintenance created/updated, closing modal and refreshing"
                );
                setShowMaintenanceFormModal(false);
                setSelectedMaintenance(null);
                refreshVehicles();
              }}
            />
          </div>
        </div>
      )}
    </div>
  );
}
