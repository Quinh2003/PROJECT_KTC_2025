import { jsx as _jsx, jsxs as _jsxs, Fragment as _Fragment } from "react/jsx-runtime";
import { useState, useEffect, useRef, useCallback } from "react";
import { Search, X, Clock, ArrowRight, Filter } from "lucide-react";
import { globalSearch, quickSearch, getSearchSuggestions } from "../../services/globalSearchAPI";
const searchTypeLabels = {
    vehicle: "Phương tiện",
    order: "Đơn hàng",
    user: "Người dùng",
    driver: "Tài xế"
};
const searchTypeColors = {
    vehicle: "bg-blue-100 text-blue-800",
    order: "bg-green-100 text-green-800",
    user: "bg-purple-100 text-purple-800",
    driver: "bg-orange-100 text-orange-800"
};
export default function GlobalSearch({ onSelectResult, onClose, className = "", placeholder = "Tìm kiếm tất cả...", showFilters = true }) {
    const [query, setQuery] = useState("");
    const [results, setResults] = useState([]);
    const [suggestions, setSuggestions] = useState([]);
    const [isLoading, setIsLoading] = useState(false);
    const [isOpen, setIsOpen] = useState(false);
    const [selectedIndex, setSelectedIndex] = useState(-1);
    const [searchTime, setSearchTime] = useState(0);
    const [selectedTypes, setSelectedTypes] = useState(['vehicle', 'order', 'user', 'driver']);
    const searchRef = useRef(null);
    const resultsRef = useRef(null);
    // Debounced search
    const debouncedSearch = useCallback(async (searchQuery) => {
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
        }
        catch (error) {
            console.error('Search error:', error);
            setResults([]);
            setSuggestions([]);
        }
        finally {
            setIsLoading(false);
        }
    }, []);
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
        const handleKeyDown = (e) => {
            if (!isOpen)
                return;
            switch (e.key) {
                case 'ArrowDown':
                    e.preventDefault();
                    setSelectedIndex(prev => prev < results.length - 1 ? prev + 1 : prev);
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
        const handleClickOutside = (event) => {
            if (resultsRef.current && !resultsRef.current.contains(event.target)) {
                setIsOpen(false);
            }
        };
        if (isOpen) {
            document.addEventListener('mousedown', handleClickOutside);
            return () => document.removeEventListener('mousedown', handleClickOutside);
        }
    }, [isOpen]);
    const handleSelectResult = (result) => {
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
        if (!query.trim())
            return;
        setIsLoading(true);
        try {
            const response = await globalSearch({
                query,
                types: selectedTypes,
                limit: 50
            });
            setResults(response.results);
            setSearchTime(response.searchTime);
        }
        catch (error) {
            console.error('Advanced search error:', error);
        }
        finally {
            setIsLoading(false);
        }
    };
    const toggleSearchType = (type) => {
        setSelectedTypes(prev => prev.includes(type)
            ? prev.filter(t => t !== type)
            : [...prev, type]);
    };
    return (_jsxs("div", { className: `relative ${className}`, ref: resultsRef, children: [_jsxs("div", { className: "relative", children: [_jsx("input", { ref: searchRef, type: "text", value: query, onChange: (e) => {
                            setQuery(e.target.value);
                            setIsOpen(true);
                            setSelectedIndex(-1);
                        }, onFocus: () => setIsOpen(true), placeholder: placeholder, className: "w-full pl-10 pr-10 py-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-violet-500 focus:border-violet-500 transition-colors bg-white shadow-sm" }), _jsx(Search, { size: 20, className: "absolute left-3 top-3.5 text-gray-400" }), query && (_jsx("button", { onClick: () => {
                            setQuery("");
                            setResults([]);
                            setIsOpen(false);
                        }, className: "absolute right-3 top-3.5 text-gray-400 hover:text-gray-600", children: _jsx(X, { size: 20 }) }))] }), showFilters && isOpen && (_jsxs("div", { className: "absolute top-full left-0 right-0 mt-1 bg-white rounded-lg shadow-lg border border-gray-200 p-3 z-50", children: [_jsxs("div", { className: "flex items-center gap-2 mb-2", children: [_jsx(Filter, { size: 16, className: "text-gray-500" }), _jsx("span", { className: "text-sm text-gray-600 font-medium", children: "L\u1ECDc theo:" })] }), _jsx("div", { className: "flex flex-wrap gap-2", children: Object.entries(searchTypeLabels).map(([type, label]) => (_jsx("button", { onClick: () => toggleSearchType(type), className: `px-3 py-1 rounded-full text-xs font-medium transition-colors ${selectedTypes.includes(type)
                                ? searchTypeColors[type]
                                : "bg-gray-100 text-gray-600 hover:bg-gray-200"}`, children: label }, type))) }), query.trim() && (_jsx("button", { onClick: handleAdvancedSearch, className: "mt-2 w-full bg-violet-600 hover:bg-violet-700 text-white py-2 px-4 rounded-lg text-sm font-medium transition-colors", children: "T\u00ECm ki\u1EBFm n\u00E2ng cao" }))] })), isOpen && (query.trim() || results.length > 0) && (_jsxs("div", { className: "absolute top-full left-0 right-0 mt-1 bg-white rounded-lg shadow-lg border border-gray-200 max-h-96 overflow-y-auto z-40", children: [isLoading && (_jsxs("div", { className: "p-4 text-center text-gray-500", children: [_jsx("div", { className: "inline-block animate-spin rounded-full h-6 w-6 border-b-2 border-violet-600" }), _jsx("p", { className: "mt-2 text-sm", children: "\u0110ang t\u00ECm ki\u1EBFm..." })] })), !isLoading && results.length === 0 && query.trim() && (_jsxs("div", { className: "p-4 text-center text-gray-500", children: [_jsxs("p", { className: "text-sm", children: ["Kh\u00F4ng t\u00ECm th\u1EA5y k\u1EBFt qu\u1EA3 n\u00E0o cho \"", query, "\""] }), suggestions.length > 0 && (_jsxs("div", { className: "mt-2", children: [_jsx("p", { className: "text-xs text-gray-400 mb-1", children: "G\u1EE3i \u00FD:" }), _jsx("div", { className: "flex flex-wrap gap-1", children: suggestions.map((suggestion, index) => (_jsx("button", { onClick: () => setQuery(suggestion.text), className: "text-xs px-2 py-1 bg-gray-100 hover:bg-gray-200 rounded transition-colors", children: suggestion.text }, index))) })] }))] })), !isLoading && results.length > 0 && (_jsxs(_Fragment, { children: [_jsxs("div", { className: "p-2 border-b border-gray-100 text-xs text-gray-500", children: [results.length, " k\u1EBFt qu\u1EA3 ", searchTime > 0 && `(${searchTime}ms)`] }), results.map((result, index) => (_jsx("div", { onClick: () => handleSelectResult(result), className: `p-3 hover:bg-gray-50 cursor-pointer border-b border-gray-50 last:border-b-0 ${selectedIndex === index ? "bg-violet-50" : ""}`, children: _jsxs("div", { className: "flex items-start justify-between", children: [_jsxs("div", { className: "flex-1 min-w-0", children: [_jsxs("div", { className: "flex items-center gap-2 mb-1", children: [_jsx("span", { className: `text-xs px-2 py-1 rounded-full font-medium ${searchTypeColors[result.type]}`, children: searchTypeLabels[result.type] }), result.status && (_jsx("span", { className: "text-xs text-gray-500", children: result.status }))] }), _jsx("h4", { className: "font-medium text-gray-900 truncate", children: result.title }), _jsx("p", { className: "text-sm text-gray-600 truncate", children: result.subtitle }), result.description && (_jsx("p", { className: "text-xs text-gray-500 mt-1 line-clamp-2", children: result.description }))] }), _jsx(ArrowRight, { size: 16, className: "text-gray-400 ml-2 flex-shrink-0" })] }) }, `${result.type}-${result.id}`)))] }))] }))] }));
}
