import type { GlobalSearchRequest, GlobalSearchResponse, SearchResult, SearchSuggestion } from '../types/GlobalSearch';
export declare function globalSearch(request: GlobalSearchRequest): Promise<GlobalSearchResponse>;
export declare function getSearchSuggestions(query: string): Promise<SearchSuggestion[]>;
export declare function quickSearch(query: string): Promise<SearchResult[]>;
