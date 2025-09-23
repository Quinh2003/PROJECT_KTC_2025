import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import React, { useState } from "react";
const MAPBOX_TOKEN = "pk.eyJ1IjoieHVhbmh1eTEiLCJhIjoiY21lN3liN21tMDlzaTJtbXF3MjU0Z2JzaSJ9.vmH3qH_f7qf1ewBC_pJoSg";
const SearchBox = ({ placeholder, onSelect }) => {
    const [query, setQuery] = useState("");
    const [suggestions, setSuggestions] = useState([]);
    const [loading, setLoading] = useState(false);
    const [selectedInfo, setSelectedInfo] = useState(null);
    // Tạo session_token cho mỗi phiên nhập
    const sessionTokenRef = React.useRef(Math.random().toString(36).substring(2));
    // Suggest API
    const handleSearch = async (value) => {
        setQuery(value);
        setSelectedInfo(null);
        if (!value.trim())
            return setSuggestions([]);
        setLoading(true);
        try {
            const session_token = sessionTokenRef.current;
            const res = await fetch(`https://api.mapbox.com/search/searchbox/v1/suggest?q=${encodeURIComponent(value)}&language=vi&session_token=${session_token}&access_token=${MAPBOX_TOKEN}`);
            const data = await res.json();
            setSuggestions(data.suggestions || []);
        }
        catch {
            setSuggestions([]);
        }
        setLoading(false);
    };
    // Retrieve API
    const handleSelect = async (suggestion) => {
        setQuery(suggestion.name);
        setSuggestions([]);
        setLoading(true);
        try {
            const session_token = sessionTokenRef.current;
            const res = await fetch(`https://api.mapbox.com/search/searchbox/v1/retrieve/${suggestion.mapbox_id}?session_token=${session_token}&access_token=${MAPBOX_TOKEN}`);
            const data = await res.json();
            if (data && data.features && data.features[0]) {
                const feature = data.features[0];
                setSelectedInfo({
                    name: feature.properties.name,
                    address: feature.properties.full_address,
                    type: feature.properties.feature_type,
                    coords: feature.geometry.coordinates
                });
                if (feature.geometry && feature.geometry.coordinates) {
                    onSelect([feature.geometry.coordinates[0], feature.geometry.coordinates[1]]);
                }
            }
        }
        catch { }
        setLoading(false);
    };
    return (_jsxs("div", { className: "relative w-64", children: [_jsx("input", { type: "text", value: query, placeholder: placeholder, onChange: e => handleSearch(e.target.value), className: "border p-2 rounded w-full" }), loading && _jsx("div", { className: "absolute left-0 top-full bg-white p-2 text-xs", children: "\u0110ang t\u00ECm ki\u1EBFm..." }), !!suggestions.length && (_jsx("ul", { className: "absolute left-0 top-full bg-white border rounded shadow w-full z-10", children: suggestions.map((s, idx) => (_jsx("li", { className: "p-2 cursor-pointer hover:bg-blue-100 text-xs", onClick: () => handleSelect(s), children: s.name }, idx))) })), selectedInfo && (_jsxs("div", { className: "mt-2 p-2 bg-green-50 border rounded text-xs", children: [_jsxs("div", { children: [_jsx("b", { children: "\u0110\u1ECBa \u0111i\u1EC3m:" }), " ", selectedInfo.name] }), _jsxs("div", { children: [_jsx("b", { children: "\u0110\u1ECBa ch\u1EC9:" }), " ", selectedInfo.address] }), _jsxs("div", { children: [_jsx("b", { children: "Lo\u1EA1i:" }), " ", selectedInfo.type] }), _jsxs("div", { children: [_jsx("b", { children: "T\u1ECDa \u0111\u1ED9:" }), " ", selectedInfo.coords[0], ", ", selectedInfo.coords[1]] })] }))] }));
};
export default SearchBox;
