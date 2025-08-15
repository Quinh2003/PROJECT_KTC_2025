import { memo } from "react";
import { Search, Filter, Plus } from "lucide-react";

type VehicleStatus = "Hoạt động" | "Bảo trì" | "Cần bảo trì";

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
  return (
    <div className="bg-white/30 hover:bg-white/40 rounded-xl p-6 shadow-lg">
      <div className="flex flex-col lg:flex-row gap-4 items-start lg:items-center justify-between mb-6">
        <div>
          <h2 className="text-2xl font-bold text-gray-900">Quản lý phương tiện</h2>
          <p className="text-gray-600 mt-1">Tìm kiếm và lọc danh sách phương tiện</p>
        </div>
        <button
          onClick={onToggleAddForm}
          className="bg-violet-600 hover:bg-violet-700 text-white px-6 py-2 rounded-lg font-medium transition-colors duration-200 flex items-center gap-2"
        >
          <Plus size={20} />
          {showAddForm ? "Ẩn form" : "Thêm phương tiện"}
        </button>
      </div>

      {/* Search and Filter Bar */}
      <div className="flex flex-col md:flex-row gap-4 mb-6">
        <div className="flex-1 relative">
          <input
            type="text"
            placeholder="Tìm kiếm theo biển số, hãng xe, model, tài xế..."
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
            <option value="all">Tất cả trạng thái</option>
            <option value="Hoạt động">Đang hoạt động</option>
            <option value="Bảo trì">Đang bảo trì</option>
            <option value="Cần bảo trì">Cần bảo trì</option>
          </select>
          <Filter size={20} className="absolute right-2 top-3.5 text-gray-400 pointer-events-none" />
        </div>
      </div>      
    </div>
  );
});

SearchAndFilter.displayName = "SearchAndFilter";

export default SearchAndFilter;