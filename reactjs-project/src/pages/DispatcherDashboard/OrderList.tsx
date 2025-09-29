import { useState, useEffect } from "react";
import OrderChecklistTimeline from "../../components/OrderChecklistTimeline";
import { useTranslation } from 'react-i18next';
import { fetchOrders, fetchOrderById, fetchNotCompletedOrders, type FetchNotCompletedOrdersResponse } from "../../services/orderAPI";
import { useDispatcherContext } from "../../contexts/DispatcherContext";
import type { Order } from "../../types/Order";
import { useQuery } from "@tanstack/react-query";

export default function OrderList() {
  const { t } = useTranslation();
  const { selectedOrder, setSelectedOrder } = useDispatcherContext();
  const [expandedOrderId, setExpandedOrderId] = useState<number | null>(null);
  const [searchId, setSearchId] = useState("");
  const [searching, setSearching] = useState(false);
  const [isSearchMode, setIsSearchMode] = useState(false);

  const [page, setPage] = useState(1);
  const PAGE_SIZE = 5;
  const [searchResults, setSearchResults] = useState<Order[]>([]);
  const [error, setError] = useState("");

  // Use React Query to fetch orders with pagination
  const {
    data: ordersResponse,
    isLoading: loading,
    error: fetchError
  } = useQuery<FetchNotCompletedOrdersResponse>({
    queryKey: ['ordersForList', 'not-completed', page],
    queryFn: async () => {
      const token = localStorage.getItem("token") || "";
      return await fetchNotCompletedOrders(page, PAGE_SIZE, token);
    },
    enabled: !isSearchMode,
    staleTime: 30 * 1000,
    refetchOnWindowFocus: true,
  });

  const paginatedOrders: Order[] = isSearchMode
    ? searchResults
    : (ordersResponse?.content || []).slice().sort((a, b) => (b.id ?? 0) - (a.id ?? 0));
  const totalPages = isSearchMode ? 1 : (ordersResponse?.totalPages || 1);

  // Function to select order for route display
  const handleOrderClick = async (order: Order) => {
    setSelectedOrder(order);
    if (expandedOrderId === order.id) {
      setExpandedOrderId(null);
      return;
    }
    setExpandedOrderId(order.id);
  };

  // Function to search order by ID
  const handleSearch = async () => {
    if (!searchId.trim()) return;
    setSearching(true);
    setError("");
    try {
      const token = localStorage.getItem("token") || "";
      const foundOrder = await fetchOrderById(searchId.trim(), token);
      if (foundOrder) {
        setSearchResults([foundOrder]);
        setIsSearchMode(true);
        setPage(1);
      } else {
        setSearchResults([]);
        setIsSearchMode(true);
        setPage(1);
        setError(t('dashboard.dispatcher.orders.orderNotFound'));
      }
    } catch (err) {
      setError(t('dashboard.dispatcher.orders.searchError'));
    } finally {
      setSearching(false);
    }
  };

  useEffect(() => {
    if (searchId.trim() === "") {
      setIsSearchMode(false);
      setSearchResults([]);
      setError("");
    }
  }, [searchId]);

  return (
    <div className="bg-gradient-to-br from-blue-50/80 via-white/90 to-blue-100/80 backdrop-blur-2xl rounded-3xl p-8 border border-white/40 shadow-2xl max-w-full overflow-x-auto">
      <div className="flex flex-col md:flex-row md:items-center md:justify-between mb-6 gap-4">
        <div>
          <div className="text-3xl font-extrabold mb-2 text-blue-900 tracking-tight">{t('dashboard.dispatcher.orders.title')}</div>
          <div className="text-gray-500 text-base">{t('dashboard.dispatcher.subtitle')}</div>
          <div className="text-sm text-blue-600 mt-1">💡 {t('dashboard.dispatcher.orders.clickToViewMap', 'Click on order to view route on map')}</div>
        </div>
        
        {/* Search order by ID */}
        <div className="flex items-center gap-2 bg-white/80 border border-blue-100 rounded-xl px-3 py-2 shadow">
          <input
            type="text"
            placeholder={t('dashboard.dispatcher.orders.enterOrderId')}
            className="px-2 py-1 rounded border border-blue-200 focus:outline-none focus:ring-2 focus:ring-blue-300 text-base"
            value={searchId}
            onChange={e => setSearchId(e.target.value)}
            onKeyDown={e => { if (e.key === 'Enter') handleSearch(); }}
            disabled={loading || searching}
          />
          <button
            onClick={handleSearch}
            disabled={!searchId.trim() || loading || searching}
            className="px-3 py-1 bg-blue-500 hover:bg-blue-600 disabled:opacity-50 text-white rounded transition-colors duration-200 font-semibold"
          >
            {searching ? t('common.loading') : t('dashboard.dispatcher.orders.search')}
          </button>
        </div>
      </div>
      
      {error || fetchError ? (
        <div className="text-center py-8 px-4 bg-red-100/80 border border-red-200 rounded-xl text-red-700 font-semibold shadow flex items-center justify-center gap-2">
          {error || (fetchError as Error)?.message || t('common.error')}
        </div>
      ) : (
        <div className="relative">
          {/* Order list */}
          <div className="flex flex-col gap-6">
            {paginatedOrders.map((order: Order) => (
              <div key={order.id} className="space-y-4">
                {/* Order Card */}
                <div
                  onClick={() => handleOrderClick(order)}
                  className={`rounded-2xl border p-6 flex flex-col lg:flex-row lg:items-center lg:justify-between gap-4 shadow-lg hover:shadow-xl transition-all duration-300 cursor-pointer transform hover:scale-[1.02] ${
                    selectedOrder?.id === order.id 
                      ? 'bg-gradient-to-r from-blue-50 to-indigo-50 border-blue-400 ring-2 ring-blue-300 shadow-blue-200/50' 
                      : 'bg-white/95 backdrop-blur-sm border-gray-200 hover:bg-gradient-to-r hover:from-blue-50/50 hover:to-indigo-50/50'
                  }`}
                >
                  {/* Left: Order info */}
                  <div className="flex-1 min-w-0 flex flex-col gap-3">
                    <div className="flex flex-wrap gap-3 items-center mb-2">
                      <span className="font-bold text-xl text-blue-900 bg-blue-100 px-3 py-1 rounded-full shadow-sm">
                        #{order.id}
                      </span>
                      <span className={`px-3 py-1.5 rounded-full text-xs font-bold border-2 shadow-md transition-all duration-200 hover:scale-105 ${
                        order.status?.name === 'Pending'
                          ? 'bg-gradient-to-r from-yellow-100 to-yellow-200 text-yellow-800 border-yellow-400 shadow-yellow-200/50'
                          : order.status?.name === 'Processing'
                          ? 'bg-gradient-to-r from-purple-100 to-purple-200 text-purple-800 border-purple-400 shadow-purple-200/50'
                          : order.status?.name === 'Shipping'
                          ? 'bg-gradient-to-r from-blue-100 to-blue-200 text-blue-800 border-blue-400 shadow-blue-200/50'
                          : order.status?.name === 'Delivered'
                          ? 'bg-gradient-to-r from-green-100 to-green-200 text-green-800 border-green-400 shadow-green-200/50'
                          : order.status?.name === 'Completed'
                          ? 'bg-gradient-to-r from-emerald-100 to-emerald-200 text-emerald-800 border-emerald-400 shadow-emerald-200/50'
                          : order.status?.name === 'Cancelled'
                          ? 'bg-gradient-to-r from-red-100 to-red-200 text-red-800 border-red-400 shadow-red-200/50'
                          : order.status?.name === 'FAILED'
                          ? 'bg-gradient-to-r from-red-100 to-red-200 text-red-800 border-red-400 shadow-red-200/50'
                          : 'bg-gradient-to-r from-gray-100 to-gray-200 text-gray-700 border-gray-400 shadow-gray-200/50'
                      }`}>
                        {order.status?.name}
                      </span>
                      {order.status?.statusType && (
                        <span className={`px-3 py-1.5 rounded-full text-xs font-bold border-2 shadow-md ${
                          order.status?.statusType === 'High'
                            ? 'bg-gradient-to-r from-red-100 to-red-200 text-red-700 border-red-400 shadow-red-200/50'
                            : order.status?.statusType === 'Medium'
                            ? 'bg-gradient-to-r from-orange-100 to-orange-200 text-orange-700 border-orange-400 shadow-orange-200/50'
                            : 'bg-gradient-to-r from-green-100 to-green-200 text-green-700 border-green-400 shadow-green-200/50'
                        }`}>
                          📊 {order.status?.statusType}
                        </span>
                      )}
                    </div>
                    
                    <div className="space-y-2 text-sm">
                      <div className="flex items-center gap-2">
                        <span className="font-semibold text-blue-700 bg-blue-50 px-2 py-1 rounded-md text-xs">
                          🏪 {t('dashboard.dispatcher.orders.customer')}:
                        </span>
                        <span className="text-gray-800 font-medium">{order.store?.storeName}</span>
                      </div>
                      <div className="flex items-start gap-2">
                        <span className="font-semibold text-green-700 bg-green-50 px-2 py-1 rounded-md text-xs">
                          📍 {t('common.from')}:
                        </span>
                        <span className="text-gray-700 flex-1">{order.store?.address}</span>
                      </div>
                      <div className="flex items-start gap-2">
                        <span className="font-semibold text-orange-700 bg-orange-50 px-2 py-1 rounded-md text-xs">
                          🎯 {t('common.to')}:
                        </span>
                        <span className="text-gray-700 flex-1">
                          {order.address?.address}{order.address?.city ? ", " + order.address.city : ""}
                        </span>
                      </div>
                    </div>
                  </div>
                  
                  {/* Right: Date & driver/vehicle info */}
                  <div className="flex flex-col items-end min-w-[220px] gap-2 bg-gray-50/80 backdrop-blur-sm rounded-xl p-4 border border-gray-200/50">
                    <div className="text-base text-blue-900 font-bold bg-blue-100 px-3 py-1 rounded-lg shadow-sm">
                      📅 {order.createdAt?.slice(0, 10)}
                    </div>
                    <div className="text-sm text-gray-700 text-right space-y-1 w-full">
                      <div className="flex items-center justify-between gap-2">
                        <span className="font-semibold text-gray-600 text-xs">👤 {t('dashboard.dispatcher.orders.driver')}:</span> 
                        <span className="font-bold text-blue-800 bg-blue-50 px-2 py-1 rounded-md text-xs">
                          {order.vehicle?.currentDriver?.fullName || t('dashboard.dispatcher.orders.notAssigned')}
                        </span>
                      </div>
                      {order.vehicle?.licensePlate && (
                        <div className="flex items-center justify-between gap-2">
                          <span className="font-semibold text-gray-600 text-xs">🚛 {t('dashboard.dispatcher.orders.vehicle')}:</span>
                          <span className="font-bold text-indigo-800 bg-indigo-50 px-2 py-1 rounded-md text-xs">
                            {order.vehicle.licensePlate}
                          </span>
                        </div>
                      )}
                    </div>
                  </div>
                </div>
                
                {/* Checklist timeline - display when expanded */}
                {expandedOrderId === order.id && (
                  <div className="px-2">
                    <OrderChecklistTimeline orderId={order.id} />
                  </div>
                )}
              </div>
            ))}
          </div>
          
          {/* Pagination controls */}
          <div className="flex justify-center items-center gap-4 mt-8">
            <button
              className="px-6 py-3 rounded-xl bg-gradient-to-r from-blue-100 to-blue-200 text-blue-700 font-bold shadow-md disabled:opacity-50 transition-all duration-200 hover:from-blue-200 hover:to-blue-300 hover:scale-105"
              onClick={() => setPage((p) => Math.max(1, p - 1))}
              disabled={page === 1 || loading}
            >
              ← {t('common.previous')}
            </button>
            <div className="bg-white/90 backdrop-blur-sm rounded-xl px-4 py-2 border border-gray-200 shadow-sm">
              <span className="text-blue-900 font-semibold text-base">
                {t('common.page', 'Page')} {page} / {totalPages}
              </span>
            </div>
            <button
              className="px-6 py-3 rounded-xl bg-gradient-to-r from-blue-100 to-blue-200 text-blue-700 font-bold shadow-md disabled:opacity-50 transition-all duration-200 hover:from-blue-200 hover:to-blue-300 hover:scale-105"
              onClick={() => setPage((p) => Math.min(totalPages, p + 1))}
              disabled={page === totalPages || loading}
            >
              {t('common.next')} →
            </button>
          </div>
        </div>
      )}
    </div>
  );
}