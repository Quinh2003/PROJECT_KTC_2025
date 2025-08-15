import type { ReactNode } from 'react';

interface DataTableProps {
  headers: string[];
  children: ReactNode;
  className?: string;
}

export default function DataTable({ headers, children, className = '' }: DataTableProps) {
  return (
    <div className={`backdrop-blur-lg bg-white/40 border border-white/30 rounded-xl overflow-hidden ${className}`}>
      <div className="overflow-x-auto">
        <table className="w-full">
          <thead className="bg-white/20 border-b border-white/30">
            <tr>
              {headers.map((header, index) => (
                <th
                  key={index}
                  className="px-6 py-4 text-left text-sm font-medium text-gray-700 uppercase tracking-wider"
                >
                  {header}
                </th>
              ))}
            </tr>
          </thead>
          <tbody className="divide-y divide-white/30">
            {children}
          </tbody>
        </table>
      </div>
    </div>
  );
}

interface TableRowProps {
  children: ReactNode;
  onClick?: () => void;
  className?: string;
}

export function TableRow({ children, onClick, className = '' }: TableRowProps) {
  return (
    <tr
      onClick={onClick}
      className={`
        transition-colors duration-200 hover:bg-white/20
        ${onClick ? 'cursor-pointer' : ''}
        ${className}
      `}
    >
      {children}
    </tr>
  );
}

interface TableCellProps {
  children: ReactNode;
  className?: string;
}

export function TableCell({ children, className = '' }: TableCellProps) {
  return (
    <td className={`px-6 py-4 text-sm text-gray-800 ${className}`}>
      {children}
    </td>
  );
}
