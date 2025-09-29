import { memo } from "react";
import { useTranslation } from 'react-i18next';
import { Search, Filter, Plus } from "lucide-react";

type VehicleStatus = "AVAILABLE" | "IN_USE" | "MAINTENANCE" | "MAINTENANCE_PENDING";

interface SearchAndFilterProps {
  searchTerm: string;
  statusFilter: VehicleStatus | "all";
  onSearch: (term: string) => void;
  onStatusFilter: (status: VehicleStatus | "all") => void;
  onToggleAddForm: () => void;
  showAddForm: boolean;
  resultsCount: number;
  totalCount: number;
}

const SearchAndFilter = memo<SearchAndFilterProps>(({
  searchTerm,
  statusFilter,
  onSearch,
  onStatusFilter,
  onToggleAddForm,
  showAddForm,
  resultsCount,
  totalCount
}) => {
  const { t } = useTranslation();
  return (
    <div className="bg-white/30 hover:bg-white/40 rounded-xl p-6 shadow-lg">
      <div className="flex flex-col lg:flex-row gap-4 items-start lg:items-center justify-between mb-6">
        <div>
          <h2 className="text-2xl font-bold text-gray-900">{t('fleet.search.title', 'Vehicle Management')}</h2>
          <p className="text-gray-600 mt-1">{t('fleet.search.subtitle', 'Search and filter vehicle list')}</p>
        </div>
        <button
          onClick={onToggleAddForm}
          className="bg-violet-600 hover:bg-violet-700 text-white px-6 py-2 rounded-lg font-medium transition-colors duration-200 flex items-center gap-2"
        >
          <Plus size={20} />
{showAddForm ? t('fleet.search.hideForm', 'Hide Form') : t('fleet.form.addVehicle', 'Add Vehicle')}
        </button>
      </div>

      {/* Search and Filter Bar */}
      <div className="flex flex-col md:flex-row gap-4 mb-6">
        <div className="flex-1 relative">
          <input
            type="text"
placeholder={t('fleet.search.placeholder', 'Search by license plate, brand, model, driver...')}
            value={searchTerm}
            onChange={(e) => onSearch(e.target.value)}
            className="w-full pl-10 pr-4 py-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-violet-500 focus:border-violet-500 transition-colors"
          />
          <Search size={20} className="absolute left-3 top-3.5 text-gray-400" />
        </div>
        <div className="relative">
          <select
            value={statusFilter}
            onChange={(e) => onStatusFilter(e.target.value as VehicleStatus | "all")}
            className="appearance-none bg-white border border-gray-300 rounded-lg px-4 py-3 pr-8 focus:ring-2 focus:ring-violet-500 focus:border-violet-500 transition-colors"
          >
            <option value="all">{t('fleet.search.allStatus', 'All Status')}</option>
            <option value="AVAILABLE">{t('dashboard.fleet.status.available')}</option>
            <option value="IN_USE">{t('dashboard.fleet.status.inUse')}</option>
            <option value="MAINTENANCE">{t('dashboard.fleet.status.underMaintenance')}</option>
            <option value="MAINTENANCE_PENDING">{t('dashboard.fleet.status.needMaintenance')}</option>
          </select>
          <Filter size={20} className="absolute right-2 top-3.5 text-gray-400 pointer-events-none" />
        </div>
      </div>      
    </div>
  );
});

SearchAndFilter.displayName = "SearchAndFilter";

export default SearchAndFilter;