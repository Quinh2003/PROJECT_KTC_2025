import { useTranslation } from 'react-i18next';
import { useState, useMemo, useEffect } from "react";
import OrderDetailModal from "./OrderDetailModal";
import { fetchOrderItemsByOrderIdPaged, fetchOrdersTotalQuantityBatch } from "../../services/OrderItemAPI";
import type { ProductItem } from "../../services/OrderItemAPI";
import { useQuery, useQueryClient } from '@tanstack/react-query';
import { fetchOrdersRaw, updateOrderVehicle, fetchOrderById, fetchNotCompletedOrders, type FetchNotCompletedOrdersResponse } from "../../services/orderAPI";
import { fetchVehicleStats } from "../../services/VehicleListAPI";
import type { Vehicle } from "../../types";
import { FaUserCog, FaCheck, FaTimes, FaCar } from "react-icons/fa";
import { useDispatcherContext } from "../../contexts/DispatcherContext";
import { trackingService } from "../../services/trackingService";
// Import test function for development
import { testDeliveryTrackingFlow } from "../../services/testDeliveryTracking";

type OrderType = {
  id: number;
  code: string;
  customer: string;
  address: string;
  note: string;
  date: string;
  from: string;
  to: string;
  description: string;
  status: {
    name: string;
    statusType?: string;
  };
  priority: string;
  storeId?: number;
  currentDriver: {
    id: number;
    fullName?: string;
    username: string;
    phone?: string;
  } | null;
  assignedVehicle: {
    id: number;
    licensePlate: string;
    vehicleType: string;
    currentDriver?: {
      id: number;
      fullName?: string;
      username: string;
      phone?: string;
    };
  } | null;
  createdAt: string;
  addressDetail?: {
    contactName?: string;
    contactPhone?: string;
  };
  order?: any;
};

// interface OrdersAssignmentProps {
//   orders?: OrderType[];
// }

export default function OrdersAssignment(_props: any) {
  const { t } = useTranslation();
  const { selectedOrder, setSelectedOrder } = useDispatcherContext();
  const [detailOrder, setDetailOrder] = useState<OrderType | null>(null);
  const [detailOpen, setDetailOpen] = useState(false);
  const [orderProducts, setOrderProducts] = useState<ProductItem[]>([]);
  const [orderProductsPage, setOrderProductsPage] = useState(0);
  const [orderProductsTotalPages, setOrderProductsTotalPages] = useState(1);
  const [deliveryFee, setDeliveryFee] = useState<number | undefined>();
  const queryClient = useQueryClient();
  const [selectedVehicles, setSelectedVehicles] = useState<{ [orderId: string]: string }>({});
  const [assigningOrders, setAssigningOrders] = useState<{ [orderId: string]: boolean }>({});
  const [successMessage, setSuccessMessage] = useState("");
  const [editingOrders, setEditingOrders] = useState<{ [orderId: string]: boolean }>({});
  const [currentPage, setCurrentPage] = useState(1);
  const PAGE_SIZE = 5;

  // Hàm tạo/cập nhật tracking cho đơn hàng
  const createTrackingForOrder = async (orderId: number, vehicleId: number) => {
    try {
      // Lấy deliveryId từ orderId trước khi lưu tracking
      let deliveryId = null;
      try {
        const deliveryResponse = await fetch(`http://localhost:8080/api/deliveries/order/${orderId}`, {
          method: 'GET',
          headers: {
            'Authorization': `Bearer ${localStorage.getItem('token')}`
          }
        });
        
        if (deliveryResponse.ok) {
          const deliveries = await deliveryResponse.json();
          if (deliveries && deliveries.length > 0) {
            deliveryId = deliveries[0].id;
            console.log('🔍 OrderAssignment: Found deliveryId:', deliveryId, 'for orderId:', orderId);
          }
        }
      } catch (error) {
        console.error('❌ OrderAssignment: Error fetching delivery:', error);
      }

      if (!deliveryId) {
        console.warn('⚠️ OrderAssignment: No delivery found, backend should have created one...');
        return;
      }

      // Lấy thông tin order để có store coordinates
  const order = paginatedData.find((o: OrderType) => o.id === orderId);
      if (!order) return;

      const trackingData = {
        vehicleId: vehicleId,
        deliveryId: deliveryId,
        latitude: order.storeId ? 10.77653 : 10.762622, // Store latitude hoặc fallback
        longitude: order.storeId ? 106.700981 : 106.660172, // Store longitude hoặc fallback
        location: `Auto-created for order #${orderId}`,
        notes: `Vehicle assigned to order #${orderId}`
      };
      
      console.log('🔍 OrderAssignment: Creating tracking:', trackingData);
      
      // Tạo tracking record mới
      const response = await fetch('http://localhost:8080/api/tracking/vehicle-location', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${localStorage.getItem('token')}`
        },
        body: JSON.stringify(trackingData)
      });
      
      if (response.ok) {
        const result = await response.json();
        console.log('✅ OrderAssignment: Tracking created successfully:', result);
      } else {
        const errorText = await response.text();
        console.log('❌ OrderAssignment: Failed to create tracking:', response.status, errorText);
      }
    } catch (error) {
      console.error('❌ OrderAssignment: Error in createTrackingForOrder:', error);
    }
  };

  // Development: Add test function to window for testing
  if (import.meta.env.DEV) {
    (window as any).testDeliveryTracking = testDeliveryTrackingFlow;
    console.log('🛠️ Development mode: Use window.testDeliveryTracking() to test delivery tracking flow');
  }

  // Đã thay thế bằng rawOrdersPage ở dưới để tránh trùng biến

  // Sử dụng React Query để cache dữ liệu vehicles
  const { data: vehiclesData, isLoading: vehiclesLoading, error: vehiclesError } = useQuery({
    queryKey: ['vehicles'],
    queryFn: fetchVehicleStats,
    staleTime: 30 * 1000, // Giảm cache xuống 30 giây cho vehicles
    refetchOnWindowFocus: true, // Cho phép refetch khi focus lại window
  });

  // Extract vehicles array from the response
  const vehicles = vehiclesData?.sampleVehicles || [];

  // Để luôn đủ 5 đơn chưa hoàn thành trên mỗi trang:
  // 1. Lấy nhiều đơn hơn từ API (gấp 3-4 lần PAGE_SIZE)
  // 2. Map và filter loại bỏ đơn completed/status_id=2
  // 3. Phân trang lại ở client
  // Để không bị giới hạn số lượng đơn hàng, lấy một số rất lớn hoặc lấy total từ API nếu có
  // Lấy danh sách đơn hàng chưa hoàn thành từ API /not-completed (phân trang backend)
  const {
    data: ordersPage,
    isLoading: ordersLoading,
    error: ordersError
  } = useQuery<FetchNotCompletedOrdersResponse>({
    queryKey: ['orders', 'not-completed', currentPage],
    queryFn: async () => {
      const token = localStorage.getItem("token") || "";
      return await fetchNotCompletedOrders(currentPage, PAGE_SIZE, token);
    },
    staleTime: 5 * 60 * 1000,
    refetchOnWindowFocus: false,
  });

  // Map dữ liệu trả về từ backend sang OrderType[]
  const paginatedData: OrderType[] = (ordersPage?.content || [])
    .map((item: unknown): OrderType => {
      const orderItem = item as Record<string, unknown>;
      let addressValue: any = orderItem.address;
      let addressField: any = addressValue;
      let addressDetail: { contactName?: string; contactPhone?: string } | undefined = undefined;
      if (addressValue && typeof addressValue === 'object') {
        addressDetail = {
          contactName: (addressValue as any).contactName,
          contactPhone: (addressValue as any).contactPhone,
        };
        addressField = addressValue;
      } else {
        addressField = addressValue || orderItem.toAddress || orderItem.to || "";
      }
      let statusObj: { name: string; statusType?: string } = { name: '', statusType: '' };
      if (typeof orderItem.status === 'object' && orderItem.status !== null && 'name' in orderItem.status && typeof (orderItem.status as any).name === 'string' && (orderItem.status as any).name.trim() !== '') {
        statusObj = {
          name: (orderItem.status as any).name,
          statusType: (orderItem.status as any).statusType || String(orderItem.priority || "")
        };
      } else if (typeof orderItem.status === 'string' && orderItem.status.trim() !== '') {
        statusObj = {
          name: orderItem.status as string,
          statusType: String(orderItem.priority || "")
        };
      } else {
        statusObj = { name: 'Unknown', statusType: '' };
      }
      return {
        id: Number(orderItem.id),
        code: (orderItem.code || orderItem.orderCode || orderItem.id) as string,
        customer: (orderItem.customer || (orderItem.store as { storeName: string })?.storeName || "") as string,
        address: addressField,
        note: (orderItem.note || "") as string,
        date: (orderItem.date || (orderItem.createdAt as string)?.slice(0, 10) || "") as string,
        from: (orderItem.from || orderItem.fromAddress || (orderItem.store as { address: string })?.address || "") as string,
        to: (orderItem.to || orderItem.toAddress || (typeof addressValue === 'object' && addressValue !== null && 'address' in addressValue ? (addressValue as any).address : addressValue) || "") as string,
        description: (orderItem.description || "") as string,
        status: statusObj,
        priority: (orderItem.priority || (orderItem.status as { statusType: string })?.statusType || "") as string,
        storeId: orderItem.storeId ? Number(orderItem.storeId) : ((orderItem.store as { id: number })?.id ? Number((orderItem.store as { id: number }).id) : undefined),
        currentDriver: orderItem.currentDriver ? {
          id: Number((orderItem.currentDriver as { id: number }).id),
          fullName: (orderItem.currentDriver as { fullName?: string }).fullName,
          username: (orderItem.currentDriver as { username: string }).username,
          phone: (orderItem.currentDriver as { phone?: string }).phone,
        } : null,
        assignedVehicle: orderItem.assignedVehicle ? {
          id: Number((orderItem.assignedVehicle as { id: number }).id),
          licensePlate: (orderItem.assignedVehicle as { licensePlate: string }).licensePlate,
          vehicleType: (orderItem.assignedVehicle as { vehicleType: string }).vehicleType,
          currentDriver: (orderItem.assignedVehicle as { currentDriver?: unknown }).currentDriver ? {
            id: Number(((orderItem.assignedVehicle as { currentDriver: { id: number } }).currentDriver as { id: number }).id),
            fullName: ((orderItem.assignedVehicle as { currentDriver: { fullName?: string } }).currentDriver as { fullName?: string }).fullName,
            username: ((orderItem.assignedVehicle as { currentDriver: { username: string } }).currentDriver as { username: string }).username,
            phone: ((orderItem.assignedVehicle as { currentDriver: { phone?: string } }).currentDriver as { phone?: string }).phone,
          } : undefined,
        } : (orderItem.vehicle ? {
          id: Number((orderItem.vehicle as { id: number }).id),
          licensePlate: (orderItem.vehicle as { licensePlate: string }).licensePlate,
          vehicleType: (orderItem.vehicle as { vehicleType: string }).vehicleType || 'TRUCK',
          currentDriver: (orderItem.vehicle as { currentDriver?: unknown }).currentDriver ? {
            id: Number(((orderItem.vehicle as { currentDriver: { id: number } }).currentDriver as { id: number }).id),
            fullName: ((orderItem.vehicle as { currentDriver: { fullName?: string } }).currentDriver as { fullName?: string }).fullName,
            username: ((orderItem.vehicle as { currentDriver: { username: string } }).currentDriver as { username: string }).username,
            phone: ((orderItem.vehicle as { currentDriver: { phone?: string } }).currentDriver as { phone?: string }).phone,
          } : undefined,
        } : null),
        createdAt: (orderItem.createdAt || "") as string,
        addressDetail,
        order: orderItem,
      };
    });
  const totalPages = ordersPage?.totalPages || 1;
  const totalOrders = ordersPage?.totalElements || paginatedData.length;

  const loading = ordersLoading || vehiclesLoading;
  const error = ordersError || vehiclesError;

  // (Đã bỏ hàm getAvailableVehicles vì không sử dụng)

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
    return vehicles.find(vehicle => {
      const drv = vehicle.currentDriver as { id?: number } | undefined;
      return drv && typeof drv.id !== 'undefined' && drv.id?.toString() === driverId.toString();
    });
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
      // --- Giữ lại code cũ: Gán xe cho đơn hàng ---
      await updateOrderVehicle(orderId, Number(selectedVehicle.id));

      // --- Gọi thêm API ghi log checklist ---
      const assignDriverPayload = {
        driverId: selectedVehicle.currentDriver.id,
        vehicleId: selectedVehicle.id
      };
      const dispatcherApiUrl = `http://localhost:8080/api/dispatcher/orders/${orderId}/assign-driver`;
      const dispatcherResponse = await fetch(dispatcherApiUrl, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${localStorage.getItem('token')}`
        },
        body: JSON.stringify(assignDriverPayload)
      });
      let dispatcherResult = null;
      if (dispatcherResponse.ok) {
        dispatcherResult = await dispatcherResponse.json();
        // Nếu có dữ liệu timeline/checklistLog thì log ra để debug
        if (dispatcherResult.timeline) {
          console.log('📝 Timeline:', dispatcherResult.timeline);
        }
        if (dispatcherResult.checklistLog) {
          console.log('📝 ChecklistLog:', dispatcherResult.checklistLog);
        }
      } else {
        const errorText = await dispatcherResponse.text();
        console.error('❌ Lỗi khi gọi API assign-driver:', dispatcherResponse.status, errorText);
      }

      // --- Giữ lại code cũ: Tạo/cập nhật tracking ---
      // const updatedOrder = data.find(o => o.id.toString() === orderId);

      // Sau khi gán xe thành công, tự động tạo/cập nhật tracking
      const updatedOrder = paginatedData.find((o: OrderType) => o.id.toString() === orderId);
      if (updatedOrder && selectedVehicle.id) {
        try {
          await createTrackingForOrder(updatedOrder.id, Number(selectedVehicle.id));
          console.log('✅ OrderAssignment: Tracking created/updated successfully for order:', updatedOrder.id);
        } catch (err) {
          console.error('❌ OrderAssignment: Error creating tracking:', err);
        }
      }

      // Nếu trạng thái đơn hàng là Pending (id 1), chuyển sang Processing (id 4)
      const statusAny = updatedOrder?.status as any;
      if (
        updatedOrder && (
          updatedOrder.status?.name === 'Pending' ||
          updatedOrder.status?.statusType === '1' ||
          (typeof statusAny?.id === 'number' && statusAny.id === 1)
        )
      ) {
        try {
          await fetch(`http://localhost:8080/api/orders/${orderId}/status`, {
            method: 'PATCH',
            headers: {
              'Content-Type': 'application/json',
              'Authorization': `Bearer ${localStorage.getItem('token')}`
            },
            body: JSON.stringify({ statusId: 4 })
          });
        } catch (err) {
          console.error('Failed to update order status to Processing:', err);
        }
      }

      // Không cần cập nhật local list nữa, chỉ cần refetch lại query
      setDetailOrder(prev => {
        if (prev && prev.id.toString() === orderId) {
          return {
            ...prev,
            assignedVehicle: {
              id: Number(selectedVehicle.id),
              licensePlate: selectedVehicle.licensePlate,
              vehicleType: selectedVehicle.vehicleType,
              currentDriver: selectedVehicle.currentDriver ? {
                id: Number(selectedVehicle.currentDriver.id),
                fullName: selectedVehicle.currentDriver.fullName,
                username: (selectedVehicle.currentDriver as any).username || "",
                phone: selectedVehicle.currentDriver.phone,
              } : undefined,
            },
            currentDriver: selectedVehicle.currentDriver ? {
              id: Number(selectedVehicle.currentDriver.id),
              fullName: selectedVehicle.currentDriver.fullName,
              username: (selectedVehicle.currentDriver as any).username || "",
              phone: selectedVehicle.currentDriver.phone,
            } : null,
          };
        }
        return prev;
      });

      // Refetch đúng queryKey để đảm bảo UI cập nhật xe mới ngay
      await Promise.all([
        queryClient.invalidateQueries({ queryKey: ['orders', 'not-completed'] }),
        queryClient.refetchQueries({ queryKey: ['orders', 'not-completed', currentPage] }),
        queryClient.refetchQueries({ queryKey: ['ordersForList'] }),
        queryClient.refetchQueries({ queryKey: ['vehicles'] }),
        queryClient.invalidateQueries({ queryKey: ['ordersTotalQuantity'] })
      ]);
      console.log('✅ OrderAssignment: Orders and vehicles cache refreshed after assignment');

      // Cập nhật selectedOrder nếu đây là order đang được chọn để tracking
      if (selectedOrder && selectedOrder.id.toString() === orderId) {
        const updatedOrderObj = {
          ...selectedOrder,
          vehicle: {
            id: Number(selectedVehicle.id),
            licensePlate: selectedVehicle.licensePlate,
            currentDriver: selectedVehicle.currentDriver ? {
              fullName: selectedVehicle.currentDriver.fullName || 'Unknown Driver'
            } : undefined,
          }
        };
        setSelectedOrder(updatedOrderObj);
      }

      // Debug: Log updated order data
      setTimeout(() => {
        const updatedOrder = paginatedData.find((o: OrderType) => o.id.toString() === orderId);
        console.log('🔍 Updated order after assignment:', updatedOrder);
      }, 200);

      // Chỉ reset local state sau khi data đã được cập nhật
      setTimeout(() => {
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
      }, 100);

      const isEditing = editingOrders[orderId];
      const messageKey = isEditing ? 'dashboard.dispatcher.assignment.vehicleUpdated' : 'dashboard.dispatcher.assignment.vehicleAssigned';
      setSuccessMessage(t(messageKey, `Vehicle ${selectedVehicle.licensePlate} ${isEditing ? 'updated' : 'assigned'} successfully to order ${orderId}! Delivery tracking auto-updated.`, { 
        vehicle: selectedVehicle.licensePlate, 
        orderId: orderId 
      }));
      setTimeout(() => setSuccessMessage(""), 3000);
    } catch (error) {
      console.error("Failed to assign vehicle:", error);
      alert(t('dashboard.dispatcher.assignment.assignError', 'Failed to assign vehicle: ') + (error as Error).message);
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
      // Bỏ gán xe cho đơn hàng
      await updateOrderVehicle(orderId, 0);
      
      // Force refetch ngay lập tức tất cả các cache liên quan để đảm bảo dữ liệu mới nhất
      await Promise.all([
        queryClient.refetchQueries({ queryKey: ['orders', currentPage, PAGE_SIZE] }),
        queryClient.refetchQueries({ queryKey: ['ordersForList'] }), // Cập nhật OrderList
        queryClient.refetchQueries({ queryKey: ['vehicles'] }),
        queryClient.invalidateQueries({ queryKey: ['ordersTotalQuantity'] })
      ]);
      
      setSuccessMessage(t('dashboard.dispatcher.assignment.vehicleUnassigned', `Vehicle unassigned successfully from order ${orderId}!`, { orderId: orderId }));
      setTimeout(() => setSuccessMessage(""), 3000);
    } catch (error) {
      console.error("Failed to unassign vehicle:", error);
      alert(t('dashboard.dispatcher.assignment.unassignError', 'Failed to unassign vehicle: ') + (error as Error).message);
    } finally {
      setAssigningOrders(prev => ({ ...prev, [orderId]: false }));
    }
  };


  // Hàm mở modal chi tiết đơn hàng, fetch thêm sản phẩm và deliveryFee
  const handleOpenDetail = async (order: OrderType) => {
    try {
      // Fetch order detail từ API để lấy đầy đủ thông tin
      const orderDetail = await fetchOrderById(order.id);
      
      // Lấy contact info từ order detail nếu có
      const contactName = orderDetail?.address?.contactName;
      const contactPhone = orderDetail?.address?.contactPhone;
      
      console.log('🔍 OrderAssignment: orderDetail from API:', orderDetail);
      console.log('🔍 OrderAssignment: contactName from API:', contactName);
      console.log('🔍 OrderAssignment: contactPhone from API:', contactPhone);
      
      // Cập nhật order với contact info, đảm bảo giữ nguyên id
      const orderWithContact = {
        ...order,
        id: order.id, // Đảm bảo id được giữ lại
        addressDetail: {
          contactName,
          contactPhone
        }
      };
      
      console.log('🔍 OrderAssignment: orderWithContact.id:', orderWithContact.id);
      
      setDetailOrder(orderWithContact);
      setOrderProductsPage(0);
      setDetailOpen(true);
      
      // Fetch products như cũ
      fetchOrderProductsPaged(order.id, 0);
    } catch (error) {
      console.error('Error fetching order detail:', error);
      // Đảm bảo order có id khi có lỗi
      setDetailOrder({...order, id: order.id});
      setOrderProductsPage(0);
      setDetailOpen(true);
      fetchOrderProductsPaged(order.id, 0);
    }
  };

  // Hàm fetch sản phẩm theo trang
  const fetchOrderProductsPaged = async (orderId: number, page: number) => {
    try {
      const res = await fetchOrderItemsByOrderIdPaged(orderId, page, 5);
      setOrderProducts(res.content);
      setOrderProductsTotalPages(res.totalPages);
      // Tính tổng shippingFee nếu có
      const fee = res.content.reduce((sum, item) => sum + (item.shippingFee || 0), 0);
      setDeliveryFee(fee > 0 ? fee : undefined);
    } catch {
      setOrderProducts([]);
      setOrderProductsTotalPages(1);
      setDeliveryFee(undefined);
    }
  };

  // Use React Query for batch total quantity calls with proper caching
  const orderIds = useMemo(() => paginatedData.map((order: OrderType) => order.id), [paginatedData]);
  
  const {
    data: batchCounts = {},
  } = useQuery({
    queryKey: ['ordersTotalQuantity', orderIds],
    queryFn: async () => {
      if (orderIds.length === 0) return {};
      try {
        return await fetchOrdersTotalQuantityBatch(orderIds);
      } catch {
        // Nếu lỗi, set tất cả về 0
        const fallback: { [orderId: number]: number } = {};
        orderIds.forEach(id => { fallback[id] = 0; });
        return fallback;
      }
    },
    enabled: orderIds.length > 0,
    staleTime: 2 * 60 * 1000, // Cache for 2 minutes
    refetchOnWindowFocus: false,
  });

  // Use the cached batch counts as productCounts
  const productCounts = batchCounts;

  return (
    <>
      {/* Modal chi tiết đơn hàng */}
      <OrderDetailModal
        open={detailOpen}
        onClose={() => setDetailOpen(false)}
        orderItem={detailOrder ? {
          id: detailOrder.id,
          code: detailOrder.code,
          customer: detailOrder.customer,
          status: detailOrder.status.name, // Pass only the status name as string
          date: detailOrder.date,
          address: detailOrder.address,
          from: detailOrder.from,
          to: detailOrder.to,
          note: detailOrder.note,
          description: detailOrder.description,
          assignedVehicle: detailOrder.assignedVehicle && detailOrder.assignedVehicle.licensePlate && detailOrder.assignedVehicle.vehicleType
            ? {
                licensePlate: detailOrder.assignedVehicle.licensePlate,
                vehicleType: detailOrder.assignedVehicle.vehicleType,
              }
            : undefined,
          currentDriver: detailOrder.currentDriver && detailOrder.currentDriver.username
            ? {
                fullName: detailOrder.currentDriver.fullName,
                username: detailOrder.currentDriver.username,
              }
            : undefined,
          addressDetail: (detailOrder && detailOrder.addressDetail) 
            ? detailOrder.addressDetail
            : (typeof detailOrder.address === 'object' && detailOrder.address !== null)
            ? {
                contactName: (detailOrder.address as any).contactName,
                contactPhone: (detailOrder.address as any).contactPhone,
              }
            : undefined,
        } : null}
        products={orderProducts}
        deliveryFee={deliveryFee}
        productsPage={orderProductsPage}
        productsTotalPages={orderProductsTotalPages}
        onProductsPageChange={(page: number) => {
          setOrderProductsPage(page);
          if (detailOrder) fetchOrderProductsPaged(detailOrder.id, page);
        }}
      />
      <div className="bg-gradient-to-br from-blue-50/80 via-white/80 to-blue-100/80 backdrop-blur-2xl rounded-3xl p-8 border border-white/40 shadow-2xl max-w-full overflow-x-auto">
      <div className="flex flex-col md:flex-row items-center justify-between mb-8 gap-4">
        <div className="flex items-center gap-4">
          <span className="inline-flex items-center justify-center w-12 h-12 rounded-full bg-blue-100 shadow-lg">
            <FaUserCog className="text-3xl text-blue-600" />
          </span>
          <div>
            <h3 className="text-3xl font-extrabold text-gray-900 tracking-tight">{t('dashboard.dispatcher.assignment.title', 'Order Assignment Management')}</h3>
            <p className="text-gray-600 mt-1">{t('common.total')} {totalOrders} {t('navigation.orders', 'orders')}</p>
          </div>
        </div>
      </div>

      {loading ? (
        <div className="text-center py-16 text-gray-500 text-lg animate-pulse">{t('common.loading')}...</div>
      ) : error ? (
        <div className="text-center py-12 px-4 bg-red-100/80 border border-red-200 rounded-xl text-red-700 font-semibold shadow flex items-center justify-center gap-2">
          <FaTimes className="text-xl text-red-500" />
          {(error as Error)?.message || t('common.error')}
        </div>
      ) : (
        <>
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
                    <th className="text-left p-5 font-bold text-gray-900 tracking-wide">{t('dashboard.dispatcher.assignment.headers.orderId', 'Mã đơn')}</th>
                    <th className="text-left p-5 font-bold text-gray-900 tracking-wide">{t('dashboard.dispatcher.assignment.headers.products', 'Sản phẩm')}</th>
                    <th className="text-left p-5 font-bold text-gray-900 tracking-wide">{t('dashboard.dispatcher.assignment.headers.customer', 'Khách hàng')}</th>
                    <th className="text-left p-5 font-bold text-gray-900 tracking-wide">{t('dashboard.dispatcher.assignment.headers.route', 'Lộ trình')}</th>
                    <th className="text-left p-5 font-bold text-gray-900 tracking-wide">{t('dashboard.dispatcher.assignment.headers.orderDetails', 'Chi tiết đơn hàng')}</th>
                    <th className="text-left p-5 font-bold text-gray-900 tracking-wide">{t('dashboard.dispatcher.assignment.headers.createdDate', 'Ngày tạo')}</th>
                    <th className="text-left p-5 font-bold text-gray-900 tracking-wide">{t('dashboard.dispatcher.assignment.headers.vehicleDriver', 'Xe & Tài xế')}</th>
                    <th className="text-left p-5 font-bold text-gray-900 tracking-wide">{t('dashboard.dispatcher.assignment.headers.actions', 'Thao tác')}</th>
                  </tr>
                </thead>
                <tbody>
                  {paginatedData.map((order: OrderType, index: number) => (
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
                              ${order.status.name === 'Pending'
                                ? 'bg-yellow-100 text-yellow-800 border-yellow-300'
                                : order.status.name === 'Processing'
                                ? 'bg-purple-100 text-purple-800 border-purple-300'
                                : order.status.name === 'Shipping'
                                ? 'bg-blue-100 text-blue-800 border-blue-300'
                                : order.status.name === 'Delivered'
                                ? 'bg-green-100 text-green-800 border-green-300'
                                : order.status.name === 'Completed'
                                ? 'bg-green-100 text-green-800 border-green-300'
                                : order.status.name === 'Cancelled'
                                ? 'bg-red-100 text-red-800 border-red-300'
                                : order.status.name === 'FAILED'
                                ? 'bg-red-100 text-red-800 border-red-300'
                                : 'bg-gray-100 text-gray-700 border-gray-300'}
                            `}
                          >
                            {order.status.name}
                          </span>
                        </div>
                      </td>
                      <td className="p-5 align-top min-w-[200px]">
                        {/* Chỉ hiển thị tổng số lượng sản phẩm */}
                        <div className="max-w-xs font-bold text-blue-900 text-lg">
                          {typeof productCounts[order.id] === "number"
                            ? `${productCounts[order.id]} ${t('dashboard.dispatcher.assignment.products', 'products')}`
                            : t('common.loading')}
                        </div>
                      </td>
                      <td className="p-5 align-top min-w-[160px]">
                        <div className="font-semibold text-gray-900 text-base">{order.customer}</div>
                        <div className="text-sm text-gray-500 mt-1">{order.description}</div>
                      </td>
                      <td className="p-5 align-top min-w-[180px]">
                        <div className="text-sm text-gray-700">
                          <div><span className="font-semibold text-blue-700">{t('common.from', 'From')}:</span> {order.from}</div>
                          <div>
                            <span className="font-semibold text-blue-700">{t('common.to', 'To')}:</span> {order.to}
                            {typeof order.address === 'object' && (order.address as any)?.city ? `, ${(order.address as any).city}` : ""}
                          </div>
                        </div>
                      </td>
                      <td className="p-5 align-top">
                        <button
                          className="px-3 py-1 rounded-lg bg-blue-100 hover:bg-blue-200 text-blue-700 font-semibold text-sm border border-blue-200 shadow transition-all duration-150"
                          onClick={() => handleOpenDetail(order)}
                        >
                          {t('dashboard.dispatcher.assignment.viewDetails', 'View details')}
                        </button>
                      </td>
                      <td className="p-5 align-top">
                        <div className="text-blue-900 font-semibold text-base">{order.date}</div>
                      </td>
                      <td className="p-5 align-top min-w-[220px]">
                        <div className="space-y-2">
                          {/* Ưu tiên hiển thị xe đã được gán từ server data */}
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
                                    📞 {order.assignedVehicle.currentDriver?.phone || t('dashboard.dispatcher.assignment.noPhone', 'Chưa có SĐT')}
                                  </div>
                                </>
                              )}
                              <button
                                className="mt-2 px-3 py-1 bg-blue-100 hover:bg-blue-200 text-blue-700 rounded-lg text-xs font-semibold border border-blue-200 transition-all duration-150"
                                onClick={() => setEditingOrders(prev => ({ ...prev, [order.id]: true }))}
                              >
                                {t('dashboard.dispatcher.assignment.edit', 'Chỉnh sửa')}
                              </button>
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
                                      📞 {order.currentDriver?.phone || t('dashboard.dispatcher.assignment.noPhone', 'Chưa có SĐT')}
                                    </div>
                                  </div>
                                ) : (
                                  <div className="bg-orange-50/90 border border-orange-200 rounded-xl p-3 shadow flex flex-col gap-1">
                                    <div className="text-sm font-semibold text-gray-800">
                                      👤 {order.currentDriver.fullName || order.currentDriver.username}
                                    </div>
                                    <div className="text-xs text-gray-600">
                                      📞 {order.currentDriver?.phone || t('dashboard.dispatcher.assignment.noPhone', 'Chưa có SĐT')}
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
                                          📞 {((selectedVehicle.currentDriver as { phone?: string })?.phone) || t('dashboard.dispatcher.assignment.noPhone', 'Chưa có SĐT')}
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
                                  <option value="">{t('dashboard.dispatcher.assignment.selectVehicle', 'Select vehicle...')}</option>
                                  {vehicles
                                    .filter(vehicle => {
                                      // Luôn giữ lại xe đã chọn cho đơn này
                                      if (selectedVehicles[order.id] && vehicle.id.toString() === selectedVehicles[order.id]) return true;
                                      // Chỉ cho phép xe có tài xế chưa được gán cho đơn khác
                                      if (!vehicle.currentDriver || typeof vehicle.currentDriver.id === 'undefined') return false;
                                      const driverId = vehicle.currentDriver.id;
                                      return !vehicles.some(v => v.currentDriver && typeof v.currentDriver.id !== 'undefined' && v.currentDriver.id === driverId && v.id !== vehicle.id);
                                    })
                                    .map(vehicle => (
                                      <option key={vehicle.id} value={vehicle.id}>
                                        {vehicle.licensePlate} - {vehicle.currentDriver?.fullName || t('dashboard.dispatcher.assignment.unknownDriver', 'Không rõ tài xế')}
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
                              title={editingOrders[order.id] ? t('dashboard.dispatcher.assignment.updateVehicle', 'Update vehicle') : t('dashboard.dispatcher.assignment.assignVehicle', 'Assign vehicle')}
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
                              title={t('common.cancel', 'Cancel')}
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
                              <span className="ml-2 text-base">{t('dashboard.dispatcher.assignment.assigned', 'Đã gán')}</span>
                            </div>
                            <button
                              onClick={() => handleUnassignVehicle(order.id.toString())}
                              disabled={assigningOrders[order.id]}
                              className="flex items-center justify-center w-9 h-9 bg-red-500 hover:bg-red-600 disabled:bg-red-300 text-white rounded-full shadow-lg transition-all duration-200 text-base font-bold focus:outline-none focus:ring-2 focus:ring-red-400"
                              title={t('dashboard.dispatcher.assignment.unassignVehicle', 'Gỡ gán xe')}
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
                &lt; {t('common.previous')}
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
                {t('common.next')} &gt;
              </button>
            </div>
          )}

          {/* Thông tin trang hiện tại */}
          <div className="text-center mt-4 text-gray-600">
            {t('dashboard.dispatcher.pagination.showing', 'Showing')} {paginatedData.length > 0 ? ((currentPage - 1) * PAGE_SIZE + 1) : 0}
            -
            {paginatedData.length > 0 ? ((currentPage - 1) * PAGE_SIZE + paginatedData.length) : 0}
            {t('dashboard.dispatcher.pagination.of')} {totalOrders} {t('navigation.orders', 'orders')}
            {totalPages > 1 && (
              <span className="ml-2">| {t('dashboard.dispatcher.pagination.page', 'Page {{current}} / {{total}}', { current: currentPage, total: totalPages })}</span>
            )}
          </div>

          {paginatedData.length === 0 && (
            <div className="text-center py-16 text-gray-500">
              <FaUserCog className="text-5xl mx-auto mb-4 opacity-40" />
              <p className="text-xl font-semibold">{t('dashboard.dispatcher.assignment.noOrders', 'No orders waiting for assignment')}</p>
            </div>
          )}
        </>
      )}
      
      </div>
    </>
  );
}