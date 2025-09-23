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
declare const SearchAndFilter: import("react").NamedExoticComponent<SearchAndFilterProps>;
export default SearchAndFilter;
