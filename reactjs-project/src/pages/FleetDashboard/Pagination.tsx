import React from "react";

interface PaginationProps {
  page: number;
  totalPages: number;
  onPageChange: (page: number) => void;
}

const Pagination: React.FC<PaginationProps> = ({ page, totalPages, onPageChange }) => {
  if (totalPages <= 1) return null;
  return (
    <div className="flex justify-center items-center gap-2 mt-6">
      <button
        className="px-3 py-1 rounded border bg-white text-gray-700 disabled:opacity-50"
        onClick={() => onPageChange(page - 1)}
        disabled={page <= 1}
      >
        &lt;
      </button>
      <span className="px-2 text-gray-700 font-medium">
        Trang {page} / {totalPages}
      </span>
      <button
        className="px-3 py-1 rounded border bg-white text-gray-700 disabled:opacity-50"
        onClick={() => onPageChange(page + 1)}
        disabled={page >= totalPages}
      >
        &gt;
      </button>
    </div>
  );
};

export default Pagination;
