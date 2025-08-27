import { useState } from "react";
import OrderDetailModal from "./OrderDetailModal";
import OrderRow from "./OrderRow";
import { useQuery, useQueryClient } from '@tanstack/react-query';
import { fetchOrdersRaw, updateOrderVehicle } from "../../services/OrderAPI";
import { fetchVehicleStats } from "../../services/VehicleListAPI";
import type { Vehicle } from "../../types";
import { FaUserCog, FaCheck, FaTimes, FaCar, FaSync } from "react-icons/fa";

interface OrdersAssignmentProps {
  orders?: any[];
}


export default function OrdersAssignment({}: OrdersAssignmentProps) {
  const [detailOrder, setDetailOrder] = useState<any | null>(null);
  const [detailOpen, setDetailOpen] = useState(false);
  const queryClient = useQueryClient();
  const [selectedVehicles, setSelectedVehicles] = useState<{ [orderId: string]: string }>({});
  const [assigningOrders, setAssigningOrders] = useState<{ [orderId: string]: boolean }>({});
  const [successMessage, setSuccessMessage] = useState("");
  const [editingOrders, setEditingOrders] = useState<{ [orderId: string]: boolean }>({});
  const [currentPage, setCurrentPage] = useState(1);
  const PAGE_SIZE = 5;

  // Sử dụng React Query để cache dữ liệu orders theo trang (server-side pagination)
  const {
    data: ordersPage = { data: [], total: 0 },
    isLoading: ordersLoading,
    error: ordersError
  } = useQuery<{ data: any[]; total: number }, Error>({
    queryKey: ['orders', currentPage, PAGE_SIZE],
    queryFn: ({ queryKey }) => {
      const [, page, size] = queryKey as [string, number, number];
      return fetchOrdersRaw(page, size);
    },
  placeholderData: (previous) => previous,
    staleTime: 5 * 60 * 1000,
    refetchOnWindowFocus: false,
  });

  // Sử dụng React Query để cache dữ liệu vehicles
  const { data: vehiclesData, isLoading: vehiclesLoading, error: vehiclesError } = useQuery({
    queryKey: ['vehicles'],
    queryFn: fetchVehicleStats,
    staleTime: 3 * 60 * 1000, // Cache 3 phút cho vehicles
    refetchOnWindowFocus: false,
  });

  // Extract vehicles array from the response
  const vehicles = vehiclesData?.sampleVehicles || [];

  // Map dữ liệu orders - server trả về { data: [], total: number }
  const data = (Array.isArray(ordersPage.data) ? ordersPage.data : []).map((item: any): any => ({
    id: Number(item.id),
    code: item.code || item.orderCode || item.id,
    customer: item.customer || item.store?.storeName || "",
    address: item.address?.address || item.toAddress || item.to || "",
    note: item.note || "",
    date: item.date || item.createdAt?.slice(0, 10) || "",
    from: item.from || item.fromAddress || item.store?.address || "",
    to: item.to || item.toAddress || item.address?.address || "",
    description: item.description || "",
    status: item.status?.name || item.status || "",
    priority: item.priority || item.status?.statusType || "",
    currentDriver: item.currentDriver || item.driver || item.assignedDriver || null,
    assignedVehicle: item.assignedVehicle || item.vehicle || null,
    createdAt: item.createdAt || "", // Giữ lại để hiển thị
  }));
  const totalOrders =
    typeof ordersPage === "object" &&
    ordersPage !== null &&
    "total" in ordersPage
      ? (ordersPage.total as number)
      : 0;
  const totalPages = Math.ceil(totalOrders / PAGE_SIZE);
  const paginatedData: any[] = data; // Đã là dữ liệu trang hiện tại

  const loading = ordersLoading || vehiclesLoading;
  const error = ordersError || vehiclesError;

  // Helper function to refresh current page data
  const refreshData = async () => {
    await Promise.all([
      queryClient.invalidateQueries({ queryKey: ['orders', currentPage, PAGE_SIZE] }),
      queryClient.invalidateQueries({ queryKey: ['vehicles'] })
    ]);
  };

  // Helper function to get available vehicles (vehicles with assigned drivers)
  const getAvailableVehicles = (): Vehicle[] => {
    if (!Array.isArray(vehicles)) {
      return [];
    }
    return vehicles.filter(vehicle => 
      vehicle.currentDriver // Chỉ lấy xe có tài xế
    );
  };

  // Helper function to get vehicle by ID
  const getVehicleById = (vehicleId: string | number): Vehicle | undefined => {
    if (!Array.isArray(vehicles)) {
      return undefined;
    }
    return vehicles.find(vehicle => vehicle.id.toString() === vehicleId.toString());
  };

  // Helper function to get driver's vehicle info
  const getDriverVehicle = (driverId: string | number): Vehicle | undefined => {
    if (!Array.isArray(vehicles)) {
      return undefined;
    }
    return vehicles.find(vehicle => 
      vehicle.currentDriver?.id?.toString() === driverId.toString()
    );
  };

  const handleVehicleSelect = (orderId: string, vehicleId: string) => {
    setSelectedVehicles(prev => ({
      ...prev,
      [orderId]: vehicleId
    }));
  };

  const handleAssignVehicle = async (orderId: string) => {
    const vehicleId = selectedVehicles[orderId];
    if (!vehicleId) return;

    const selectedVehicle = getVehicleById(vehicleId);
    if (!selectedVehicle || !selectedVehicle.currentDriver) return;

    setAssigningOrders(prev => ({ ...prev, [orderId]: true }));
    
    try {
      // Update order with assigned vehicle using the new API endpoint
      await updateOrderVehicle(orderId, Number(selectedVehicle.id));
      
      // Invalidate queries để refetch dữ liệu mới
      await Promise.all([
        queryClient.invalidateQueries({ queryKey: ['orders', currentPage, PAGE_SIZE] }),
        queryClient.invalidateQueries({ queryKey: ['vehicles'] })
      ]);
      
      // Clear selection and edit mode
      setSelectedVehicles(prev => {
        const newState = { ...prev };
        delete newState[orderId];
        return newState;
      });
      
      setEditingOrders(prev => {
        const newState = { ...prev };
        delete newState[orderId];
        return newState;
      });
      
      // Hiển thị thông báo thành công
      const isEditing = editingOrders[orderId];
      setSuccessMessage(`Vehicle ${selectedVehicle.licensePlate} ${isEditing ? 'updated' : 'assigned'} successfully to order ${orderId}!`);
      setTimeout(() => setSuccessMessage(""), 3000);
      
    } catch (error) {
      console.error("Failed to assign vehicle:", error);
      alert("Failed to assign vehicle: " + (error as Error).message);
    } finally {
      setAssigningOrders(prev => ({ ...prev, [orderId]: false }));
    }
  };

  const handleCancelVehicleAssignment = (orderId: string) => {
    setSelectedVehicles(prev => {
      const newState = { ...prev };
      delete newState[orderId];
      return newState;
    });
  };

  const handleCancelEdit = (orderId: string) => {
    setEditingOrders(prev => {
      const newState = { ...prev };
      delete newState[orderId];
      return newState;
    });
    setSelectedVehicles(prev => {
      const newState = { ...prev };
      delete newState[orderId];
      return newState;
    });
  };

  const handleUnassignVehicle = async (orderId: string) => {
    setAssigningOrders(prev => ({ ...prev, [orderId]: true }));
    
    try {
      // Gọi API để gỡ bỏ vehicle (gán vehicleId = null hoặc 0)
      await updateOrderVehicle(orderId, 0);
      
      // Invalidate queries để refetch dữ liệu mới
      await Promise.all([
        queryClient.invalidateQueries({ queryKey: ['orders', currentPage, PAGE_SIZE] }),
        queryClient.invalidateQueries({ queryKey: ['vehicles'] })
      ]);
      
      // Hiển thị thông báo thành công
      setSuccessMessage(`Vehicle unassigned successfully from order ${orderId}!`);
      setTimeout(() => setSuccessMessage(""), 3000);
      
    } catch (error) {
      console.error("Failed to unassign vehicle:", error);
      alert("Failed to unassign vehicle: " + (error as Error).message);
    } finally {
      setAssigningOrders(prev => ({ ...prev, [orderId]: false }));
    }
  };


  return (
    <>
      {/* Modal chi tiết đơn hàng */}
      <OrderDetailModal open={detailOpen} onClose={() => setDetailOpen(false)} orderItem={detailOrder} />
      <div className="bg-gradient-to-br from-blue-50/80 via-white/80 to-blue-100/80 backdrop-blur-2xl rounded-3xl p-8 border border-white/40 shadow-2xl max-w-full overflow-x-auto">
      <div className="flex flex-col md:flex-row items-center justify-between mb-8 gap-4">
        <div className="flex items-center gap-4">
          <span className="inline-flex items-center justify-center w-12 h-12 rounded-full bg-blue-100 shadow-lg">
            <FaUserCog className="text-3xl text-blue-600" />
          </span>
          <div>
            <h3 className="text-3xl font-extrabold text-gray-900 tracking-tight">Quản lý phân công đơn hàng</h3>
            <p className="text-gray-600 mt-1">Tổng cộng {totalOrders} đơn hàng</p>
          </div>
        </div>
        {/* <button
          onClick={refreshData}
          disabled={loading}
          className="flex items-center gap-2 px-6 py-3 bg-blue-600 hover:bg-blue-700 disabled:bg-blue-300 text-white rounded-xl shadow-md font-semibold text-base transition-all duration-200 focus:outline-none focus:ring-2 focus:ring-blue-400"
          title="Làm mới dữ liệu"
        >
          <FaSync className={`text-lg ${loading ? 'animate-spin' : ''}`} />
          Làm mới
        </button> */}
      </div>

      {loading ? (
        <div className="text-center py-16 text-gray-500 text-lg animate-pulse">Đang tải dữ liệu...</div>
      ) : error ? (
        <div className="text-center py-12 px-4 bg-red-100/80 border border-red-200 rounded-xl text-red-700 font-semibold shadow flex items-center justify-center gap-2">
          <FaTimes className="text-xl text-red-500" />
          {(error as any)?.message || "Đã xảy ra lỗi khi tải dữ liệu"}
        </div>
      ) : (
        <>
          {/* Success message */}
          {successMessage && (
            <div className="mb-6 p-4 bg-green-100/90 border border-green-300 rounded-xl text-green-900 flex items-center gap-3 shadow-lg animate-fade-in">
              <FaCheck className="text-2xl text-green-600" />
              <span className="font-semibold text-base">{successMessage}</span>
            </div>
          )}

          <div className="bg-white/70 backdrop-blur-xl rounded-2xl border border-white/60 overflow-x-auto shadow-xl">
            <div className="overflow-x-auto">
              <table className="w-full min-w-[900px]">
                <thead className="sticky top-0 z-10">
                  <tr className="bg-gradient-to-r from-blue-100/80 via-white/80 to-blue-50/80 border-b border-blue-200/60 shadow-sm">
                    <th className="text-left p-5 font-bold text-gray-900 tracking-wide">Mã đơn</th>
                    <th className="text-left p-5 font-bold text-gray-900 tracking-wide">Sản phẩm</th>
                    <th className="text-left p-5 font-bold text-gray-900 tracking-wide">Khách hàng</th>
                    <th className="text-left p-5 font-bold text-gray-900 tracking-wide">Lộ trình</th>
                    <th className="text-left p-5 font-bold text-gray-900 tracking-wide">Chi tiết đơn hàng</th>
                    <th className="text-left p-5 font-bold text-gray-900 tracking-wide">Ngày tạo</th>
                    <th className="text-left p-5 font-bold text-gray-900 tracking-wide">Xe & Tài xế</th>
                    <th className="text-left p-5 font-bold text-gray-900 tracking-wide">Thao tác</th>
                  </tr>
                </thead>
                <tbody>
                  {paginatedData.map((order: any, index: number) => (
                    <tr
                      key={order.id}
                      className={`border-b border-blue-100/40 hover:bg-blue-50/40 transition-all duration-200 ${
                        index % 2 === 0 ? 'bg-white/60' : 'bg-blue-50/30'
                      }`}
                    >
                      <td className="p-5 align-top">
                        <div className="flex items-center gap-2">
                          <span className="font-bold text-lg text-blue-900">{order.code}</span>
                          <span
                            className={`px-3 py-1 rounded-full text-xs font-bold border shadow-sm ml-2
                              ${order.status === 'Pending'
                                ? 'bg-yellow-100 text-yellow-800 border-yellow-300'
                                : order.status === 'Completed'
                                ? 'bg-green-100 text-green-800 border-green-300'
                                : 'bg-gray-100 text-gray-700 border-gray-300'}
                            `}
                          >
                            {order.status}
                          </span>
                        </div>
                      </td>
                      <td className="p-5 align-top min-w-[200px]">
                        {/* Hiển thị tên sản phẩm và tổng số lượng */}
                        <div className="max-w-xs">
                          <OrderRow orderId={order.id} />
                        </div>
                      </td>
                      <td className="p-5 align-top min-w-[160px]">
                        <div className="font-semibold text-gray-900 text-base">{order.customer}</div>
                        <div className="text-sm text-gray-500 mt-1">{order.description}</div>
                      </td>
                      <td className="p-5 align-top min-w-[180px]">
                        <div className="text-sm text-gray-700">
                          <div><span className="font-semibold text-blue-700">Từ:</span> {order.from}</div>
                          <div><span className="font-semibold text-blue-700">Đến:</span> {order.to}</div>
                        </div>
                      </td>
                      <td className="p-5 align-top">
                        <button
                          className="px-3 py-1 rounded-lg bg-blue-100 hover:bg-blue-200 text-blue-700 font-semibold text-sm border border-blue-200 shadow transition-all duration-150"
                          onClick={() => { setDetailOrder(order); setDetailOpen(true); }}
                        >
                          Xem chi tiết
                        </button>
                      </td>
                      <td className="p-5 align-top">
                        <div className="text-blue-900 font-semibold text-base">{order.date}</div>
                      </td>
                      <td className="p-5 align-top min-w-[220px]">
                        <div className="space-y-2">
                          {/* Hiển thị xe đã được gán */}
                          {order.assignedVehicle && !editingOrders[order.id] ? (
                            <div className="bg-green-50/90 border border-green-200 rounded-xl p-3 shadow flex flex-col gap-1">
                              <div className="flex items-center gap-2 mb-1">
                                <FaCar className="text-green-600 text-lg" />
                                <span className="font-bold text-green-900">
                                  {order.assignedVehicle.licensePlate || `Vehicle #${order.assignedVehicle.id}`}
                                </span>
                                <span className="text-xs text-green-700">
                                  ({order.assignedVehicle.vehicleType})
                                </span>
                              </div>
                              {order.assignedVehicle.currentDriver && (
                                <>
                                  <div className="text-sm font-semibold text-gray-800">
                                    👤 {order.assignedVehicle.currentDriver.fullName || order.assignedVehicle.currentDriver.username}
                                  </div>
                                  <div className="text-xs text-gray-600">
                                    📞 {(order.assignedVehicle.currentDriver as any)?.phone || 'Chưa có SĐT'}
                                  </div>
                                </>
                              )}
                            </div>
                          ) : order.currentDriver && !editingOrders[order.id] ? (
                            <div className="text-sm text-gray-700">
                              {(() => {
                                // Tìm xe của tài xế hiện tại
                                const driverVehicle = getDriverVehicle(order.currentDriver.id);
                                return driverVehicle ? (
                                  <div className="bg-green-50/90 border border-green-200 rounded-xl p-3 shadow flex flex-col gap-1">
                                    <div className="flex items-center gap-2 mb-1">
                                      <FaCar className="text-green-600 text-lg" />
                                      <span className="font-bold text-green-900">
                                        {driverVehicle.licensePlate}
                                      </span>
                                      <span className="text-xs text-green-700">
                                        ({driverVehicle.vehicleType})
                                      </span>
                                    </div>
                                    <div className="text-sm font-semibold text-gray-800">
                                      👤 {order.currentDriver.fullName || order.currentDriver.username}
                                    </div>
                                    <div className="text-xs text-gray-600">
                                      📞 {(order.currentDriver as any)?.phone || 'Chưa có SĐT'}
                                    </div>
                                  </div>
                                ) : (
                                  <div className="bg-orange-50/90 border border-orange-200 rounded-xl p-3 shadow flex flex-col gap-1">
                                    <div className="text-sm font-semibold text-gray-800">
                                      👤 {order.currentDriver.fullName || order.currentDriver.username}
                                    </div>
                                    <div className="text-xs text-gray-600">
                                      📞 {(order.currentDriver as any)?.phone || 'Chưa có SĐT'}
                                    </div>
                                    <div className="text-xs text-orange-600 mt-1 font-bold">
                                      ⚠️ Chưa có xe được gán
                                    </div>
                                  </div>
                                );
                              })()}
                            </div>
                          ) : (
                            // Vehicle Assignment Dropdown (hiển thị khi chưa có xe được gán HOẶC đang edit)
                            <div className="space-y-2">
                              {selectedVehicles[order.id] ? (
                                // Show selected vehicle info
                                <div className="bg-blue-50/90 border border-blue-200 rounded-xl p-3 shadow flex flex-col gap-1">
                                  {(() => {
                                    const selectedVehicle = getVehicleById(selectedVehicles[order.id]);
                                    return selectedVehicle && selectedVehicle.currentDriver ? (
                                      <div>
                                        <div className="flex items-center gap-2 mb-1">
                                          <FaCar className="text-blue-600 text-lg" />
                                          <span className="text-base font-bold text-blue-900">
                                            {selectedVehicle.licensePlate}
                                          </span>
                                          <span className="text-xs text-blue-700">
                                            ({selectedVehicle.vehicleType})
                                          </span>
                                        </div>
                                        <div className="text-sm font-semibold text-blue-800">
                                          👤 {selectedVehicle.currentDriver.fullName}
                                        </div>
                                        <div className="text-xs text-blue-700">
                                          📞 {(selectedVehicle.currentDriver as any)?.phone || 'Chưa có SĐT'}
                                        </div>
                                      </div>
                                    ) : null;
                                  })()}
                                </div>
                              ) : (
                                // Vehicle Selection Dropdown
                                <select
                                  className="w-full px-4 py-3 bg-white/90 border border-blue-200 rounded-xl focus:ring-2 focus:ring-blue-400 focus:border-blue-400 text-base shadow-sm font-medium text-blue-900"
                                  value={selectedVehicles[order.id] || ""}
                                  onChange={(e) => handleVehicleSelect(order.id.toString(), e.target.value)}
                                >
                                  <option value="">Chọn xe...</option>
                                  {getAvailableVehicles().map((vehicle) => (
                                    <option key={vehicle.id} value={vehicle.id}>
                                      {vehicle.licensePlate} - {vehicle.currentDriver?.fullName}
                                    </option>
                                  ))}
                                </select>
                              )}
                            </div>
                          )}
                        </div>
                      </td>
                      <td className="p-5 align-top min-w-[160px]">
                        {/* Chỉ hiển thị Actions khi chưa có vehicle được gán và đã chọn vehicle HOẶC đang trong edit mode */}
                        {(!order.assignedVehicle && !order.currentDriver && selectedVehicles[order.id]) || 
                         (editingOrders[order.id] && selectedVehicles[order.id]) ? (
                          <div className="flex gap-3">
                            <button
                              onClick={() => handleAssignVehicle(order.id.toString())}
                              disabled={assigningOrders[order.id]}
                              className="flex items-center justify-center w-10 h-10 bg-green-500 hover:bg-green-600 disabled:bg-green-300 text-white rounded-full shadow-lg transition-all duration-200 text-lg font-bold focus:outline-none focus:ring-2 focus:ring-green-400"
                              title={editingOrders[order.id] ? "Cập nhật xe" : "Gán xe"}
                            >
                              {assigningOrders[order.id] ? (
                                <div className="w-5 h-5 border-2 border-white border-t-transparent rounded-full animate-spin"></div>
                              ) : (
                                <FaCheck className="text-lg" />
                              )}
                            </button>
                            <button
                              onClick={() => editingOrders[order.id] ? handleCancelEdit(order.id.toString()) : handleCancelVehicleAssignment(order.id.toString())}
                              className="flex items-center justify-center w-10 h-10 bg-red-500 hover:bg-red-600 text-white rounded-full shadow-lg transition-all duration-200 text-lg font-bold focus:outline-none focus:ring-2 focus:ring-red-400"
                              title="Hủy"
                            >
                              <FaTimes className="text-lg" />
                            </button>
                          </div>
                        ) : 
                        /* Hiển thị trạng thái đã gán với nút Unassign */
                        (order.assignedVehicle || order.currentDriver) && !editingOrders[order.id] ? (
                          <div className="flex items-center gap-3">
                            <div className="flex items-center text-green-700 font-bold">
                              <FaCheck className="text-xl" />
                              <span className="ml-2 text-base">Đã gán</span>
                            </div>
                            <button
                              onClick={() => handleUnassignVehicle(order.id.toString())}
                              disabled={assigningOrders[order.id]}
                              className="flex items-center justify-center w-9 h-9 bg-red-500 hover:bg-red-600 disabled:bg-red-300 text-white rounded-full shadow-lg transition-all duration-200 text-base font-bold focus:outline-none focus:ring-2 focus:ring-red-400"
                              title="Gỡ gán xe"
                            >
                              {assigningOrders[order.id] ? (
                                <div className="w-4 h-4 border-2 border-white border-t-transparent rounded-full animate-spin"></div>
                              ) : (
                                <FaTimes className="text-base" />
                              )}
                            </button>
                          </div>
                        ) : null}
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>

          {/* Pagination Controls */}
          {totalPages > 1 && (
            <div className="flex flex-col sm:flex-row justify-center items-center gap-4 mt-8">
              <button
                onClick={() => setCurrentPage(p => Math.max(1, p - 1))}
                disabled={currentPage === 1}
                className="px-4 py-2 rounded-xl bg-blue-100 hover:bg-blue-200 disabled:opacity-50 disabled:cursor-not-allowed text-blue-700 font-bold shadow transition-all duration-150"
              >
                &lt; Trước
              </button>
              
              {/* Desktop pagination - hiển thị nhiều trang hơn */}
              <div className="hidden sm:flex items-center gap-2">
                {Array.from({ length: Math.min(5, totalPages) }, (_, i) => {
                  let pageNum;
                  if (totalPages <= 5) {
                    pageNum = i + 1;
                  } else if (currentPage <= 3) {
                    pageNum = i + 1;
                  } else if (currentPage >= totalPages - 2) {
                    pageNum = totalPages - 4 + i;
                  } else {
                    pageNum = currentPage - 2 + i;
                  }
                  
                  return (
                    <button
                      key={pageNum}
                      onClick={() => setCurrentPage(pageNum)}
                      className={`w-10 h-10 rounded-lg font-bold transition-all duration-150 ${
                        currentPage === pageNum
                          ? 'bg-blue-600 text-white shadow-lg'
                          : 'bg-white hover:bg-blue-50 text-blue-700 border border-blue-200'
                      }`}
                    >
                      {pageNum}
                    </button>
                  );
                })}
              </div>
              
              {/* Mobile pagination - hiển thị ít trang hơn */}
              <div className="sm:hidden flex items-center gap-2">
                {Array.from({ length: Math.min(3, totalPages) }, (_, i) => {
                  let pageNum;
                  if (totalPages <= 3) {
                    pageNum = i + 1;
                  } else if (currentPage <= 2) {
                    pageNum = i + 1;
                  } else if (currentPage >= totalPages - 1) {
                    pageNum = totalPages - 2 + i;
                  } else {
                    pageNum = currentPage - 1 + i;
                  }
                  
                  return (
                    <button
                      key={pageNum}
                      onClick={() => setCurrentPage(pageNum)}
                      className={`w-10 h-10 rounded-lg font-bold transition-all duration-150 ${
                        currentPage === pageNum
                          ? 'bg-blue-600 text-white shadow-lg'
                          : 'bg-white hover:bg-blue-50 text-blue-700 border border-blue-200'
                      }`}
                    >
                      {pageNum}
                    </button>
                  );
                })}
              </div>
              
              <button
                onClick={() => setCurrentPage(p => Math.min(totalPages, p + 1))}
                disabled={currentPage === totalPages}
                className="px-4 py-2 rounded-xl bg-blue-100 hover:bg-blue-200 disabled:opacity-50 disabled:cursor-not-allowed text-blue-700 font-bold shadow transition-all duration-150"
              >
                Tiếp &gt;
              </button>
            </div>
          )}

          {/* Thông tin trang hiện tại */}
          <div className="text-center mt-4 text-gray-600">
            Hiển thị {data.length > 0 ? ((currentPage - 1) * PAGE_SIZE + 1) : 0}
            -
            {data.length > 0 ? ((currentPage - 1) * PAGE_SIZE + data.length) : 0}
            trong tổng số {totalOrders} đơn hàng
            {totalPages > 1 && (
              <span className="ml-2">| Trang {currentPage} / {totalPages}</span>
            )}
          </div>

          {data.length === 0 && (
            <div className="text-center py-16 text-gray-500">
              <FaUserCog className="text-5xl mx-auto mb-4 opacity-40" />
              <p className="text-xl font-semibold">Không có đơn hàng chờ phân công</p>
            </div>
          )}
        </>
      )}
      
      </div>
    </>
  );
}