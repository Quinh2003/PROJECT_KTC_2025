import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import React from "react";
const Pagination = ({ page, totalPages, onPageChange }) => {
    if (totalPages <= 1)
        return null;
    return (_jsxs("div", { className: "flex justify-center items-center gap-2 mt-6", children: [_jsx("button", { className: "px-3 py-1 rounded border bg-white text-gray-700 disabled:opacity-50", onClick: () => onPageChange(page - 1), disabled: page <= 1, children: "<" }), _jsxs("span", { className: "px-2 text-gray-700 font-medium", children: ["Trang ", page, " / ", totalPages] }), _jsx("button", { className: "px-3 py-1 rounded border bg-white text-gray-700 disabled:opacity-50", onClick: () => onPageChange(page + 1), disabled: page >= totalPages, children: ">" })] }));
};
export default Pagination;
