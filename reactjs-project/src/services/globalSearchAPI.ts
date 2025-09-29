import type { 
  GlobalSearchRequest, 
  GlobalSearchResponse, 
  SearchResult, 
  SearchSuggestion,
  SearchItemType,
} from '../types/GlobalSearch';
import type { Order } from '../types/Order';
import type { User } from '../types/User';
import type { Vehicle } from '../types/Operations';
import { fetchVehicles } from './VehicleListAPI';
import { fetchOrderStats } from './orderAPI';

const API_BASE_URL = "http://localhost:8080/api";

function getAuthHeaders(): HeadersInit {
  const token = localStorage.getItem("token");
  return token ? { 
    "Authorization": `Bearer ${token}`,
    "Content-Type": "application/json"
  } : {
    "Content-Type": "application/json"
  };
}

// Tính điểm liên quan dựa trên việc match với query
function calculateRelevance(query: string, text: string): number {
  const lowerQuery = query.toLowerCase();
  const lowerText = text.toLowerCase();
  
  if (lowerText === lowerQuery) return 100;
  if (lowerText.startsWith(lowerQuery)) return 90;
  if (lowerText.includes(lowerQuery)) return 70;
  
  // Fuzzy matching - check individual words
  const queryWords = lowerQuery.split(' ');
  const textWords = lowerText.split(' ');
  let matchCount = 0;
  
  queryWords.forEach(queryWord => {
    if (textWords.some(textWord => textWord.includes(queryWord))) {
      matchCount++;
    }
  });
  
  return Math.max(0, (matchCount / queryWords.length) * 60);
}

// Tìm kiếm trong vehicles
async function searchVehicles(query: string, limit: number = 10): Promise<SearchResult[]> {
  try {
    // Lấy toàn bộ danh sách xe (tối đa 1000 xe)
    const { data: vehicles } = await (await import('./VehicleListAPI')).fetchVehiclesRaw(1, 1000);
    const results: SearchResult[] = [];
    // Hàm normalize: loại bỏ mọi ký tự không phải chữ/số, chuyển về lowercase
    function normalize(str: string) {
      return (str || '').replace(/[^a-zA-Z0-9]/g, '').toLowerCase();
    }
    
    // Helper function to convert status to string
    function getStatusString(status: any): string {
      if (typeof status === 'string') return status;
      if (typeof status === 'number') return status.toString();
      if (typeof status === 'object' && status?.name) return status.name;
      return '';
    }
    const normalizedQuery = normalize(query);
    vehicles.forEach((vehicle: Vehicle) => {
      const normalizedPlate = normalize(vehicle.licensePlate);
      const normalizedBrand = normalize(vehicle.brand || '');
      const normalizedModel = normalize(vehicle.model || '');
      const normalizedDriver = normalize(vehicle.currentDriver?.fullName || '');
      const normalizedStatus = normalize(getStatusString(vehicle.status));
      // Nếu query khớp với bất kỳ trường nào đã normalize
      const match =
        normalizedPlate.includes(normalizedQuery) ||
        normalizedBrand.includes(normalizedQuery) ||
        normalizedModel.includes(normalizedQuery) ||
        normalizedDriver.includes(normalizedQuery) ||
        normalizedStatus.includes(normalizedQuery);
      if (match) {
        results.push({
          id: vehicle.id,
          type: 'vehicle',
          title: `${vehicle.brand || ''} ${vehicle.model || ''}`.trim() || vehicle.licensePlate,
          subtitle: vehicle.licensePlate,
          description: `Tài xế: ${vehicle.currentDriver?.fullName || 'Chưa gán'} • Trạng thái: ${getStatusString(vehicle.status)}`,
          status: getStatusString(vehicle.status),
          url: `/fleet-dashboard`,
          relevance: 100, // Ưu tiên match tuyệt đối
          data: vehicle
        });
      }
    });
    return results.slice(0, limit);
  } catch (error) {
    console.error('Error searching vehicles:', error);
    return [];
  }
}

// Tìm kiếm trong orders
async function searchOrders(query: string, limit: number = 10): Promise<SearchResult[]> {
  try {
    const { sampleOrders } = await fetchOrderStats();
    const results: SearchResult[] = [];
    
    sampleOrders.forEach((order: Order) => {
      const searchText = `${order.id} ${order.store?.storeName || ''} ${order.store?.address || ''} ${order.address?.address || ''} ${order.status?.name || ''}`;
      const relevance = calculateRelevance(query, searchText);
      
      if (relevance > 0) {
        results.push({
          id: order.id,
          type: 'order',
          title: `Đơn hàng #${order.id}`,
          subtitle: order.store?.storeName || 'Cửa hàng không rõ',
          description: `${order.store?.address || 'N/A'} → ${order.address?.address || 'N/A'}`,
          status: order.status?.name || 'Không rõ',
          url: `/operations-dashboard`,
          relevance,
          data: order
        });
      }
    });
    
    return results
      .sort((a, b) => b.relevance - a.relevance)
      .slice(0, limit);
  } catch (error) {
    console.error('Error searching orders:', error);
    return [];
  }
}

// Tìm kiếm users/drivers
async function searchUsers(query: string, limit: number = 10): Promise<SearchResult[]> {
  try {
    const response = await fetch(`${API_BASE_URL}/users`, {
      headers: getAuthHeaders(),
    });
    
    if (!response.ok) return [];
    
    const data = await response.json();
    const users = Array.isArray(data.data) ? data.data : [];
    const results: SearchResult[] = [];
    
    users.forEach((user: User) => {
      if (!user.id) return; // Skip users without ID
      
      const searchText = `${user.name || ''} ${user.email || ''} ${user.phone || ''} ${user.role || ''}`;
      const relevance = calculateRelevance(query, searchText);
      
      if (relevance > 0) {
        const isDriver = user.role === 'DRIVER';
        results.push({
          id: user.id,
          type: isDriver ? 'driver' : 'user',
          title: user.name || user.email,
          subtitle: user.email,
          description: `Vai trò: ${user.role} • SĐT: ${user.phone || 'N/A'}`,
          status: user.isActive ? 'Hoạt động' : 'Không hoạt động',
          url: isDriver ? '/fleet-dashboard' : '/admin-dashboard',
          relevance,
          data: user
        });
      }
    });
    
    return results
      .sort((a, b) => b.relevance - a.relevance)
      .slice(0, limit);
  } catch (error) {
    console.error('Error searching users:', error);
    return [];
  }
}

// Main search function
export async function globalSearch(request: GlobalSearchRequest): Promise<GlobalSearchResponse> {
  const startTime = Date.now();
  const { query, types = ['vehicle', 'order', 'user', 'driver'], limit = 20 } = request;
  
  if (!query || query.trim().length < 2) {
    return {
      results: [],
      totalCount: 0,
      query,
      searchTime: Date.now() - startTime
    };
  }
  
  const searchPromises: Promise<SearchResult[]>[] = [];
  const itemLimit = Math.ceil(limit / types.length);
  
  if (types.includes('vehicle')) {
    searchPromises.push(searchVehicles(query, itemLimit));
  }
  
  if (types.includes('order')) {
    searchPromises.push(searchOrders(query, itemLimit));
  }
  
  if (types.includes('user') || types.includes('driver')) {
    searchPromises.push(searchUsers(query, itemLimit));
  }
  
  try {
    const searchResults = await Promise.all(searchPromises);
    const allResults = searchResults.flat();
    
    // Sort by relevance and apply final limit
    const sortedResults = allResults
      .sort((a, b) => b.relevance - a.relevance)
      .slice(0, limit);
    
    return {
      results: sortedResults,
      totalCount: allResults.length,
      query,
      searchTime: Date.now() - startTime
    };
  } catch (error) {
    console.error('Global search error:', error);
    return {
      results: [],
      totalCount: 0,
      query,
      searchTime: Date.now() - startTime
    };
  }
}

// Get search suggestions based on recent/popular searches
export async function getSearchSuggestions(query: string): Promise<SearchSuggestion[]> {
  if (!query || query.length < 2) return [];
  
  const suggestions: SearchSuggestion[] = [];
  
  // Add some common search patterns
  const commonSuggestions = [
    { text: 'phương tiện', type: 'vehicle' as SearchItemType, count: 0 },
    { text: 'đơn hàng', type: 'order' as SearchItemType, count: 0 },
    { text: 'tài xế', type: 'driver' as SearchItemType, count: 0 },
    { text: 'bảo trì', type: 'vehicle' as SearchItemType, count: 0 },
    { text: 'hoạt động', type: 'vehicle' as SearchItemType, count: 0 },
  ];
  
  const lowerQuery = query.toLowerCase();
  commonSuggestions.forEach(suggestion => {
    if (suggestion.text.includes(lowerQuery)) {
      suggestions.push(suggestion);
    }
  });
  
  return suggestions.slice(0, 5);
}

// Quick search for autocomplete
export async function quickSearch(query: string): Promise<SearchResult[]> {
  if (!query || query.length < 2) return [];
  
  const response = await globalSearch({
    query,
    limit: 5
  });
  
  return response.results;
}