import React from "react";
export interface SearchBoxProps {
    placeholder: string;
    onSelect: (coords: [number, number]) => void;
}
declare const SearchBox: React.FC<SearchBoxProps>;
export default SearchBox;
