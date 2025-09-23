import type { ReactNode } from 'react';
interface DataTableProps {
    headers: string[];
    children: ReactNode;
    className?: string;
}
export default function DataTable({ headers, children, className }: DataTableProps): import("react/jsx-runtime").JSX.Element;
interface TableRowProps {
    children: ReactNode;
    onClick?: () => void;
    className?: string;
}
export declare function TableRow({ children, onClick, className }: TableRowProps): import("react/jsx-runtime").JSX.Element;
interface TableCellProps {
    children: ReactNode;
    className?: string;
}
export declare function TableCell({ children, className }: TableCellProps): import("react/jsx-runtime").JSX.Element;
export {};
