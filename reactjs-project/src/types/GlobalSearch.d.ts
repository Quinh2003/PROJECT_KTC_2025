import type { User } from './User';
import type { Order } from './Order';
import type { Vehicle } from './Operations';
export type SearchItemType = 'vehicle' | 'order' | 'user' | 'driver';
export interface SearchResult {
    id: string | number;
    type: SearchItemType;
    title: string;
    subtitle: string;
    description?: string;
    status?: string;
    url: string;
    relevance: number;
    data: Vehicle | Order | User;
}
export interface GlobalSearchRequest {
    query: string;
    types?: SearchItemType[];
    limit?: number;
    page?: number;
}
export interface GlobalSearchResponse {
    results: SearchResult[];
    totalCount: number;
    query: string;
    searchTime: number;
}
export interface SearchFilters {
    types: SearchItemType[];
    dateRange?: {
        from: Date;
        to: Date;
    };
    status?: string[];
}
export interface SearchSuggestion {
    text: string;
    type: SearchItemType;
    count: number;
}
