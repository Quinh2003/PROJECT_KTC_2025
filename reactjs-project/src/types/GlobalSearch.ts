import type { User } from './User';
import type { Order } from './Order';
import type { Vehicle } from './Operations';

// Các loại item có thể tìm kiếm
export type SearchItemType = 'vehicle' | 'order' | 'user' | 'driver';

// Interface chung cho kết quả tìm kiếm
export interface SearchResult {
  id: string | number;
  type: SearchItemType;
  title: string;
  subtitle: string;
  description?: string;
  status?: string;
  url: string; // Đường dẫn để navigate đến item
  relevance: number; // Điểm liên quan (0-100)
  data: Vehicle | Order | User; // Dữ liệu gốc
}

// Request params cho search API
export interface GlobalSearchRequest {
  query: string;
  types?: SearchItemType[]; // Lọc theo loại
  limit?: number;
  page?: number;
}

// Response từ search API
export interface GlobalSearchResponse {
  results: SearchResult[];
  totalCount: number;
  query: string;
  searchTime: number; // ms
}

// Filter options cho advanced search
export interface SearchFilters {
  types: SearchItemType[];
  dateRange?: {
    from: Date;
    to: Date;
  };
  status?: string[];
}

// Search suggestions
export interface SearchSuggestion {
  text: string;
  type: SearchItemType;
  count: number;
}