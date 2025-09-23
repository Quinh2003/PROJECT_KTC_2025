import type { Order } from '../../types/dashboard';
interface RecentOrdersTableProps {
    orders: Order[];
    onRefresh: () => void;
    loading: boolean;
}
export default function RecentOrdersTable({ orders, onRefresh, loading }: RecentOrdersTableProps): import("react/jsx-runtime").JSX.Element;
export {};
