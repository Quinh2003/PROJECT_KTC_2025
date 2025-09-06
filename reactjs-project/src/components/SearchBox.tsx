import React, { useState } from "react";

export interface SearchBoxProps {
  placeholder: string;
  onSelect: (coords: [number, number]) => void;
}

const MAPBOX_TOKEN = "pk.eyJ1IjoieHVhbmh1eTEiLCJhIjoiY21lN3liN21tMDlzaTJtbXF3MjU0Z2JzaSJ9.vmH3qH_f7qf1ewBC_pJoSg";

const SearchBox: React.FC<SearchBoxProps> = ({ placeholder, onSelect }) => {
  const [query, setQuery] = useState("");
  const [suggestions, setSuggestions] = useState<any[]>([]);
  const [loading, setLoading] = useState(false);
  const [selectedInfo, setSelectedInfo] = useState<any | null>(null);
  // Tạo session_token cho mỗi phiên nhập
  const sessionTokenRef = React.useRef<string>(Math.random().toString(36).substring(2));

  // Suggest API
  const handleSearch = async (value: string) => {
    setQuery(value);
    setSelectedInfo(null);
    if (!value.trim()) return setSuggestions([]);
    setLoading(true);
    try {
      const session_token = sessionTokenRef.current;
      const res = await fetch(`https://api.mapbox.com/search/searchbox/v1/suggest?q=${encodeURIComponent(value)}&language=vi&session_token=${session_token}&access_token=${MAPBOX_TOKEN}`);
      const data = await res.json();
      setSuggestions(data.suggestions || []);
    } catch {
      setSuggestions([]);
    }
    setLoading(false);
  };

  // Retrieve API
  const handleSelect = async (suggestion: any) => {
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
    } catch {}
    setLoading(false);
  };

  return (
    <div className="relative w-64">
      <input
        type="text"
        value={query}
        placeholder={placeholder}
        onChange={e => handleSearch(e.target.value)}
        className="border p-2 rounded w-full"
      />
      {loading && <div className="absolute left-0 top-full bg-white p-2 text-xs">Đang tìm kiếm...</div>}
      {!!suggestions.length && (
        <ul className="absolute left-0 top-full bg-white border rounded shadow w-full z-10">
          {suggestions.map((s, idx) => (
            <li
              key={idx}
              className="p-2 cursor-pointer hover:bg-blue-100 text-xs"
              onClick={() => handleSelect(s)}
            >{s.name}</li>
          ))}
        </ul>
      )}
      {selectedInfo && (
        <div className="mt-2 p-2 bg-green-50 border rounded text-xs">
          <div><b>Địa điểm:</b> {selectedInfo.name}</div>
          <div><b>Địa chỉ:</b> {selectedInfo.address}</div>
          <div><b>Loại:</b> {selectedInfo.type}</div>
          <div><b>Tọa độ:</b> {selectedInfo.coords[0]}, {selectedInfo.coords[1]}</div>
        </div>
      )}
    </div>
  );
};

export default SearchBox;
