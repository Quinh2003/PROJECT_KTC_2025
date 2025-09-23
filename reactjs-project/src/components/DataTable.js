import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
export default function DataTable({ headers, children, className = '' }) {
    return (_jsx("div", { className: `backdrop-blur-lg bg-white/40 border border-white/30 rounded-xl overflow-hidden ${className}`, children: _jsx("div", { className: "overflow-x-auto", children: _jsxs("table", { className: "w-full", children: [_jsx("thead", { className: "bg-white/20 border-b border-white/30", children: _jsx("tr", { children: headers.map((header, index) => (_jsx("th", { className: "px-6 py-4 text-left text-sm font-medium text-gray-700 uppercase tracking-wider", children: header }, index))) }) }), _jsx("tbody", { className: "divide-y divide-white/30", children: children })] }) }) }));
}
export function TableRow({ children, onClick, className = '' }) {
    return (_jsx("tr", { onClick: onClick, className: `
        transition-colors duration-200 hover:bg-white/20
        ${onClick ? 'cursor-pointer' : ''}
        ${className}
      `, children: children }));
}
export function TableCell({ children, className = '' }) {
    return (_jsx("td", { className: `px-6 py-4 text-sm text-gray-800 ${className}`, children: children }));
}
