import { useState, useEffect, useRef, useCallback } from "react";
import { useTranslation } from 'react-i18next';
import { Search, X, Clock, ArrowRight, Filter } from "lucide-react";
import { globalSearch, quickSearch, getSearchSuggestions } from "../../services/globalSearchAPI";
import type { SearchResult, SearchSuggestion, SearchItemType } from "../../types/GlobalSearch";

interface GlobalSearchProps {
  onSelectResult?: (result: SearchResult) => void;
  onClose?: () => void;
  className?: string;
  placeholder?: string;
  showFilters?: boolean;
}

// Move this inside component to use t()
// const searchTypeLabels will be defined inside component

const searchTypeColors: Record<SearchItemType, string> = {
  vehicle: "bg-blue-100 text-blue-800",
  order: "bg-green-100 text-green-800",
  user: "bg-purple-100 text-purple-800", 
  driver: "bg-orange-100 text-orange-800"
};

export default function GlobalSearch({ 
  onSelectResult, 
  onClose, 
  className = "",
  placeholder,
  showFilters = true 
}: GlobalSearchProps) {
  const { t } = useTranslation();
  
  const searchTypeLabels: Record<SearchItemType, string> = {
    vehicle: t('search.types.vehicle', 'Phương tiện'),
    order: t('search.types.order', 'Đơn hàng'), 
    user: t('search.types.user', 'Người dùng'),
    driver: t('search.types.driver', 'Tài xế')
  };
  
  const [query, setQuery] = useState("");
  const [results, setResults] = useState<SearchResult[]>([]);
  const [suggestions, setSuggestions] = useState<SearchSuggestion[]>([]);
  const [isLoading, setIsLoading] = useState(false);
  const [isOpen, setIsOpen] = useState(false);
  const [selectedIndex, setSelectedIndex] = useState(-1);
  const [searchTime, setSearchTime] = useState(0);
  const [selectedTypes, setSelectedTypes] = useState<SearchItemType[]>(['vehicle', 'order', 'user', 'driver']);
  
  const searchRef = useRef<HTMLInputElement>(null);
  const resultsRef = useRef<HTMLDivElement>(null);
  
  // Debounced search
  const debouncedSearch = useCallback(
    async (searchQuery: string) => {
      if (!searchQuery.trim() || searchQuery.length < 2) {
        setResults([]);
        setSuggestions([]);
        return;
      }
      
      setIsLoading(true);
      try {
        const [searchResults, searchSuggestions] = await Promise.all([
          quickSearch(searchQuery),
          getSearchSuggestions(searchQuery)
        ]);
        
        setResults(searchResults);
        setSuggestions(searchSuggestions);
        setSearchTime(Date.now());
      } catch (error) {
        console.error('Search error:', error);
        setResults([]);
        setSuggestions([]);
      } finally {
        setIsLoading(false);
      }
    },
    []
  );
  
  useEffect(() => {
    const timer = setTimeout(() => {
      if (query.trim()) {
        debouncedSearch(query);
      }
    }, 300);
    
    return () => clearTimeout(timer);
  }, [query, debouncedSearch]);
  
  // Handle keyboard navigation
  useEffect(() => {
    const handleKeyDown = (e: KeyboardEvent) => {
      if (!isOpen) return;
      
      switch (e.key) {
        case 'ArrowDown':
          e.preventDefault();
          setSelectedIndex(prev => 
            prev < results.length - 1 ? prev + 1 : prev
          );
          break;
        case 'ArrowUp':
          e.preventDefault();
          setSelectedIndex(prev => prev > 0 ? prev - 1 : -1);
          break;
        case 'Enter':
          e.preventDefault();
          if (selectedIndex >= 0 && results[selectedIndex]) {
            handleSelectResult(results[selectedIndex]);
          }
          break;
        case 'Escape':
          e.preventDefault();
          handleClose();
          break;
      }
    };
    
    document.addEventListener('keydown', handleKeyDown);
    return () => document.removeEventListener('keydown', handleKeyDown);
  }, [isOpen, selectedIndex, results]);
  
  // Handle outside click
  useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
      if (resultsRef.current && !resultsRef.current.contains(event.target as Node)) {
        setIsOpen(false);
      }
    };
    
    if (isOpen) {
      document.addEventListener('mousedown', handleClickOutside);
      return () => document.removeEventListener('mousedown', handleClickOutside);
    }
  }, [isOpen]);
  
  const handleSelectResult = (result: SearchResult) => {
    onSelectResult?.(result);
    setQuery("");
    setResults([]);
    setIsOpen(false);
    
    // Navigate to result URL
    if (result.url) {
      window.location.href = result.url;
    }
  };
  
  const handleClose = () => {
    setQuery("");
    setResults([]);
    setIsOpen(false);
    onClose?.();
  };
  
  const handleAdvancedSearch = async () => {
    if (!query.trim()) return;
    
    setIsLoading(true);
    try {
      const response = await globalSearch({
        query,
        types: selectedTypes,
        limit: 50
      });
      
      setResults(response.results);
      setSearchTime(response.searchTime);
    } catch (error) {
      console.error('Advanced search error:', error);
    } finally {
      setIsLoading(false);
    }
  };
  
  const toggleSearchType = (type: SearchItemType) => {
    setSelectedTypes(prev => 
      prev.includes(type) 
        ? prev.filter(t => t !== type)
        : [...prev, type]
    );
  };
  
  return (
    <div className={`relative ${className}`} ref={resultsRef}>
      {/* Search Input */}
      <div className="relative">
        <input
          ref={searchRef}
          type="text"
          value={query}
          onChange={(e) => {
            setQuery(e.target.value);
            setIsOpen(true);
            setSelectedIndex(-1);
          }}
          onFocus={() => setIsOpen(true)}
          placeholder={placeholder || t('search.placeholder', 'Tìm kiếm tất cả...')}
          className="w-full pl-10 pr-10 py-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-violet-500 focus:border-violet-500 transition-colors bg-white shadow-sm"
        />
        <Search size={20} className="absolute left-3 top-3.5 text-gray-400" />
        {query && (
          <button
            onClick={() => {
              setQuery("");
              setResults([]);
              setIsOpen(false);
            }}
            className="absolute right-3 top-3.5 text-gray-400 hover:text-gray-600"
          >
            <X size={20} />
          </button>
        )}
      </div>
      
      {/* Search Filters */}
      {showFilters && isOpen && (
        <div className="absolute top-full left-0 right-0 mt-1 bg-white rounded-lg shadow-lg border border-gray-200 p-3 z-50">
          <div className="flex items-center gap-2 mb-2">
            <Filter size={16} className="text-gray-500" />
            <span className="text-sm text-gray-600 font-medium">Lọc theo:</span>
          </div>
          <div className="flex flex-wrap gap-2">
            {Object.entries(searchTypeLabels).map(([type, label]) => (
              <button
                key={type}
                onClick={() => toggleSearchType(type as SearchItemType)}
                className={`px-3 py-1 rounded-full text-xs font-medium transition-colors ${
                  selectedTypes.includes(type as SearchItemType)
                    ? searchTypeColors[type as SearchItemType]
                    : "bg-gray-100 text-gray-600 hover:bg-gray-200"
                }`}
              >
                {label}
              </button>
            ))}
          </div>
          {query.trim() && (
            <button
              onClick={handleAdvancedSearch}
              className="mt-2 w-full bg-violet-600 hover:bg-violet-700 text-white py-2 px-4 rounded-lg text-sm font-medium transition-colors"
            >
{t('search.advancedSearch', 'Tìm kiếm nâng cao')}
            </button>
          )}
        </div>
      )}
      
      {/* Search Results */}
      {isOpen && (query.trim() || results.length > 0) && (
        <div className="absolute top-full left-0 right-0 mt-1 bg-white rounded-lg shadow-lg border border-gray-200 max-h-96 overflow-y-auto z-40">
          {isLoading && (
            <div className="p-4 text-center text-gray-500">
              <div className="inline-block animate-spin rounded-full h-6 w-6 border-b-2 border-violet-600"></div>
              <p className="mt-2 text-sm">Đang tìm kiếm...</p>
            </div>
          )}
          
          {!isLoading && results.length === 0 && query.trim() && (
            <div className="p-4 text-center text-gray-500">
              <p className="text-sm">{t('search.noResults', 'Không tìm thấy kết quả nào cho "{{query}}"', { query })}</p>
              {suggestions.length > 0 && (
                <div className="mt-2">
                  <p className="text-xs text-gray-400 mb-1">{t('search.suggestions', 'Gợi ý:')}</p>
                  <div className="flex flex-wrap gap-1">
                    {suggestions.map((suggestion, index) => (
                      <button
                        key={index}
                        onClick={() => setQuery(suggestion.text)}
                        className="text-xs px-2 py-1 bg-gray-100 hover:bg-gray-200 rounded transition-colors"
                      >
                        {suggestion.text}
                      </button>
                    ))}
                  </div>
                </div>
              )}
            </div>
          )}
          
          {!isLoading && results.length > 0 && (
            <>
              <div className="p-2 border-b border-gray-100 text-xs text-gray-500">
                {results.length} kết quả {searchTime > 0 && `(${searchTime}ms)`}
              </div>
              {results.map((result, index) => (
                <div
                  key={`${result.type}-${result.id}`}
                  onClick={() => handleSelectResult(result)}
                  className={`p-3 hover:bg-gray-50 cursor-pointer border-b border-gray-50 last:border-b-0 ${
                    selectedIndex === index ? "bg-violet-50" : ""
                  }`}
                >
                  <div className="flex items-start justify-between">
                    <div className="flex-1 min-w-0">
                      <div className="flex items-center gap-2 mb-1">
                        <span className={`text-xs px-2 py-1 rounded-full font-medium ${searchTypeColors[result.type]}`}>
                          {searchTypeLabels[result.type]}
                        </span>
                        {result.status && (
                          <span className="text-xs text-gray-500">
                            {result.status}
                          </span>
                        )}
                      </div>
                      <h4 className="font-medium text-gray-900 truncate">
                        {result.title}
                      </h4>
                      <p className="text-sm text-gray-600 truncate">
                        {result.subtitle}
                      </p>
                      {result.description && (
                        <p className="text-xs text-gray-500 mt-1 line-clamp-2">
                          {result.description}
                        </p>
                      )}
                    </div>
                    <ArrowRight size={16} className="text-gray-400 ml-2 flex-shrink-0" />
                  </div>
                </div>
              ))}
            </>
          )}
        </div>
      )}
    </div>
  );
}