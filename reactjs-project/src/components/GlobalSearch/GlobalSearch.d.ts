import type { SearchResult } from "../../types/GlobalSearch";
interface GlobalSearchProps {
    onSelectResult?: (result: SearchResult) => void;
    onClose?: () => void;
    className?: string;
    placeholder?: string;
    showFilters?: boolean;
}
export default function GlobalSearch({ onSelectResult, onClose, className, placeholder, showFilters }: GlobalSearchProps): import("react/jsx-runtime").JSX.Element;
export {};
