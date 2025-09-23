import { jsx as _jsx, jsxs as _jsxs, Fragment as _Fragment } from "react/jsx-runtime";
import { useState, useMemo } from "react";
import OrderDetailModal from "./OrderDetailModal";
import { fetchOrderItemsByOrderIdPaged, fetchOrdersTotalQuantityBatch } from "../../services/OrderItemAPI";
import { useQuery, useQueryClient } from '@tanstack/react-query';
import { fetchOrdersRaw, updateOrderVehicle } from "../../services/OrderAPI";
import { fetchVehicleStats } from "../../services/VehicleListAPI";
import { FaUserCog, FaCheck, FaTimes, FaCar } from "react-icons/fa";
import { useDispatcherContext } from "../../contexts/DispatcherContext";
import { trackingService } from "../../services/trackingService";
// Import test function for development
import { testDeliveryTrackingFlow } from "../../services/testDeliveryTracking";
// interface OrdersAssignmentProps {
//   orders?: OrderType[];
// }
export default function OrdersAssignment(_props) {
    const { selectedOrder, setSelectedOrder } = useDispatcherContext();
    const [detailOrder, setDetailOrder] = useState(null);
    const [detailOpen, setDetailOpen] = useState(false);
    const [orderProducts, setOrderProducts] = useState([]);
    const [orderProductsPage, setOrderProductsPage] = useState(0);
    const [orderProductsTotalPages, setOrderProductsTotalPages] = useState(1);
    const [deliveryFee, setDeliveryFee] = useState();
    const queryClient = useQueryClient();
    const [selectedVehicles, setSelectedVehicles] = useState({});
    const [assigningOrders, setAssigningOrders] = useState({});
    const [successMessage, setSuccessMessage] = useState("");
    const [editingOrders, setEditingOrders] = useState({});
    const [currentPage, setCurrentPage] = useState(1);
    const PAGE_SIZE = 5;
    // Hàm tạo/cập nhật tracking cho đơn hàng
    const createTrackingForOrder = async (orderId, vehicleId) => {
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
            }
            catch (error) {
                console.error('❌ OrderAssignment: Error fetching delivery:', error);
            }
            if (!deliveryId) {
                console.warn('⚠️ OrderAssignment: No delivery found, backend should have created one...');
                return;
            }
            // Lấy thông tin order để có store coordinates
            const order = data.find(o => o.id === orderId);
            if (!order)
                return;
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
            }
            else {
                const errorText = await response.text();
                console.log('❌ OrderAssignment: Failed to create tracking:', response.status, errorText);
            }
        }
        catch (error) {
            console.error('❌ OrderAssignment: Error in createTrackingForOrder:', error);
        }
    };
    // Development: Add test function to window for testing
    if (import.meta.env.DEV) {
        window.testDeliveryTracking = testDeliveryTrackingFlow;
        console.log('🛠️ Development mode: Use window.testDeliveryTracking() to test delivery tracking flow');
    }
    // Sử dụng React Query để cache dữ liệu orders theo trang (server-side pagination)
    const { data: ordersPage = { data: [], total: 0 }, isLoading: ordersLoading, error: ordersError } = useQuery({
        queryKey: ['orders', currentPage, PAGE_SIZE],
        queryFn: ({ queryKey }) => {
            const [, page, size] = queryKey;
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
        staleTime: 30 * 1000, // Giảm cache xuống 30 giây cho vehicles
        refetchOnWindowFocus: true, // Cho phép refetch khi focus lại window
    });
    // Extract vehicles array from the response
    const vehicles = vehiclesData?.sampleVehicles || [];
    // Map dữ liệu orders - server trả về { data: [], total: number }
    const data = (Array.isArray(ordersPage?.data) ? ordersPage.data : []).map((item) => {
        const orderItem = item;
        // Debug log để kiểm tra structure
        if (import.meta.env.DEV) {
            console.log('🔍 OrderAssignment: Raw order item:', orderItem);
        }
        return {
            id: Number(orderItem.id),
            code: (orderItem.code || orderItem.orderCode || orderItem.id),
            customer: (orderItem.customer || orderItem.store?.storeName || ""),
            address: (orderItem.address?.address || orderItem.toAddress || orderItem.to || ""),
            note: (orderItem.note || ""),
            date: (orderItem.date || orderItem.createdAt?.slice(0, 10) || ""),
            from: (orderItem.from || orderItem.fromAddress || orderItem.store?.address || ""),
            to: (orderItem.to || orderItem.toAddress || orderItem.address?.address || ""),
            description: (orderItem.description || ""),
            status: (orderItem.status?.name || orderItem.status || ""),
            priority: (orderItem.priority || orderItem.status?.statusType || ""),
            storeId: orderItem.storeId ? Number(orderItem.storeId) : (orderItem.store?.id ? Number(orderItem.store.id) : undefined),
            currentDriver: orderItem.currentDriver ? {
                id: Number(orderItem.currentDriver.id),
                fullName: orderItem.currentDriver.fullName,
                username: orderItem.currentDriver.username,
                phone: orderItem.currentDriver.phone,
            } : null,
            assignedVehicle: orderItem.assignedVehicle ? {
                id: Number(orderItem.assignedVehicle.id),
                licensePlate: orderItem.assignedVehicle.licensePlate,
                vehicleType: orderItem.assignedVehicle.vehicleType,
                currentDriver: orderItem.assignedVehicle.currentDriver ? {
                    id: Number(orderItem.assignedVehicle.currentDriver.id),
                    fullName: orderItem.assignedVehicle.currentDriver.fullName,
                    username: orderItem.assignedVehicle.currentDriver.username,
                    phone: orderItem.assignedVehicle.currentDriver.phone,
                } : undefined,
            } : (orderItem.vehicle ? {
                id: Number(orderItem.vehicle.id),
                licensePlate: orderItem.vehicle.licensePlate,
                vehicleType: orderItem.vehicle.vehicleType || 'TRUCK',
                currentDriver: orderItem.vehicle.currentDriver ? {
                    id: Number(orderItem.vehicle.currentDriver.id),
                    fullName: orderItem.vehicle.currentDriver.fullName,
                    username: orderItem.vehicle.currentDriver.username,
                    phone: orderItem.vehicle.currentDriver.phone,
                } : undefined,
            } : null),
            createdAt: (orderItem.createdAt || ""),
        };
    });
    const totalOrders = typeof ordersPage === "object" &&
        ordersPage !== null &&
        "total" in ordersPage
        ? ordersPage.total
        : 0;
    const totalPages = Math.ceil(totalOrders / PAGE_SIZE);
    const paginatedData = data; // Đã là dữ liệu trang hiện tại
    const loading = ordersLoading || vehiclesLoading;
    const error = ordersError || vehiclesError;
    // (Đã bỏ hàm getAvailableVehicles vì không sử dụng)
    // Helper function to get vehicle by ID
    const getVehicleById = (vehicleId) => {
        if (!Array.isArray(vehicles)) {
            return undefined;
        }
        return vehicles.find(vehicle => vehicle.id.toString() === vehicleId.toString());
    };
    // Helper function to get driver's vehicle info
    const getDriverVehicle = (driverId) => {
        if (!Array.isArray(vehicles)) {
            return undefined;
        }
        return vehicles.find(vehicle => {
            const drv = vehicle.currentDriver;
            return drv && typeof drv.id !== 'undefined' && drv.id?.toString() === driverId.toString();
        });
    };
    const handleVehicleSelect = (orderId, vehicleId) => {
        setSelectedVehicles(prev => ({
            ...prev,
            [orderId]: vehicleId
        }));
    };
    const handleAssignVehicle = async (orderId) => {
        const vehicleId = selectedVehicles[orderId];
        if (!vehicleId)
            return;
        const selectedVehicle = getVehicleById(vehicleId);
        if (!selectedVehicle || !selectedVehicle.currentDriver)
            return;
        setAssigningOrders(prev => ({ ...prev, [orderId]: true }));
        try {
            // Gán xe cho đơn hàng
            await updateOrderVehicle(orderId, Number(selectedVehicle.id));
            // Sau khi gán xe thành công, tự động tạo/cập nhật tracking
            const updatedOrder = data.find(o => o.id.toString() === orderId);
            if (updatedOrder && selectedVehicle.id) {
                try {
                    await createTrackingForOrder(updatedOrder.id, Number(selectedVehicle.id));
                    console.log('✅ OrderAssignment: Tracking created/updated successfully for order:', updatedOrder.id);
                }
                catch (err) {
                    console.error('❌ OrderAssignment: Error creating tracking:', err);
                }
            }
            // Force refetch ngay lập tức tất cả các cache liên quan để đảm bảo dữ liệu mới nhất
            await Promise.all([
                queryClient.refetchQueries({ queryKey: ['orders', currentPage, PAGE_SIZE] }),
                queryClient.refetchQueries({ queryKey: ['ordersForList'] }), // Cập nhật OrderList
                queryClient.refetchQueries({ queryKey: ['vehicles'] }),
                queryClient.invalidateQueries({ queryKey: ['ordersTotalQuantity'] })
            ]);
            console.log('✅ OrderAssignment: Cache refreshed successfully');
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
                const updatedOrder = data.find(o => o.id.toString() === orderId);
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
            setSuccessMessage(`Vehicle ${selectedVehicle.licensePlate} ${isEditing ? 'updated' : 'assigned'} successfully to order ${orderId}! Delivery tracking auto-updated.`);
            setTimeout(() => setSuccessMessage(""), 3000);
        }
        catch (error) {
            console.error("Failed to assign vehicle:", error);
            alert("Failed to assign vehicle: " + error.message);
        }
        finally {
            setAssigningOrders(prev => ({ ...prev, [orderId]: false }));
        }
    };
    const handleCancelVehicleAssignment = (orderId) => {
        setSelectedVehicles(prev => {
            const newState = { ...prev };
            delete newState[orderId];
            return newState;
        });
    };
    const handleCancelEdit = (orderId) => {
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
    const handleUnassignVehicle = async (orderId) => {
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
            setSuccessMessage(`Vehicle unassigned successfully from order ${orderId}!`);
            setTimeout(() => setSuccessMessage(""), 3000);
        }
        catch (error) {
            console.error("Failed to unassign vehicle:", error);
            alert("Failed to unassign vehicle: " + error.message);
        }
        finally {
            setAssigningOrders(prev => ({ ...prev, [orderId]: false }));
        }
    };
    // Hàm mở modal chi tiết đơn hàng, fetch thêm sản phẩm và deliveryFee
    const handleOpenDetail = async (order) => {
        setDetailOrder(order);
        setOrderProductsPage(0);
        setDetailOpen(true);
        fetchOrderProductsPaged(order.id, 0);
    };
    // Hàm fetch sản phẩm theo trang
    const fetchOrderProductsPaged = async (orderId, page) => {
        try {
            const res = await fetchOrderItemsByOrderIdPaged(orderId, page, 5);
            setOrderProducts(res.content);
            setOrderProductsTotalPages(res.totalPages);
            // Tính tổng shippingFee nếu có
            const fee = res.content.reduce((sum, item) => sum + (item.shippingFee || 0), 0);
            setDeliveryFee(fee > 0 ? fee : undefined);
        }
        catch {
            setOrderProducts([]);
            setOrderProductsTotalPages(1);
            setDeliveryFee(undefined);
        }
    };
    // Use React Query for batch total quantity calls with proper caching
    const orderIds = useMemo(() => data.map(order => order.id), [data]);
    const { data: batchCounts = {}, } = useQuery({
        queryKey: ['ordersTotalQuantity', orderIds],
        queryFn: async () => {
            if (orderIds.length === 0)
                return {};
            try {
                return await fetchOrdersTotalQuantityBatch(orderIds);
            }
            catch {
                // Nếu lỗi, set tất cả về 0
                const fallback = {};
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
    return (_jsxs(_Fragment, { children: [_jsx(OrderDetailModal, { open: detailOpen, onClose: () => setDetailOpen(false), orderItem: detailOrder ? {
                    code: detailOrder.code,
                    customer: detailOrder.customer,
                    status: detailOrder.status,
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
                } : null, products: orderProducts, deliveryFee: deliveryFee, productsPage: orderProductsPage, productsTotalPages: orderProductsTotalPages, onProductsPageChange: (page) => {
                    setOrderProductsPage(page);
                    if (detailOrder)
                        fetchOrderProductsPaged(detailOrder.id, page);
                } }), _jsxs("div", { className: "bg-gradient-to-br from-blue-50/80 via-white/80 to-blue-100/80 backdrop-blur-2xl rounded-3xl p-8 border border-white/40 shadow-2xl max-w-full overflow-x-auto", children: [_jsx("div", { className: "flex flex-col md:flex-row items-center justify-between mb-8 gap-4", children: _jsxs("div", { className: "flex items-center gap-4", children: [_jsx("span", { className: "inline-flex items-center justify-center w-12 h-12 rounded-full bg-blue-100 shadow-lg", children: _jsx(FaUserCog, { className: "text-3xl text-blue-600" }) }), _jsxs("div", { children: [_jsx("h3", { className: "text-3xl font-extrabold text-gray-900 tracking-tight", children: "Qu\u1EA3n l\u00FD ph\u00E2n c\u00F4ng \u0111\u01A1n h\u00E0ng" }), _jsxs("p", { className: "text-gray-600 mt-1", children: ["T\u1ED5ng c\u1ED9ng ", totalOrders, " \u0111\u01A1n h\u00E0ng"] })] })] }) }), loading ? (_jsx("div", { className: "text-center py-16 text-gray-500 text-lg animate-pulse", children: "\u0110ang t\u1EA3i d\u1EEF li\u1EC7u..." })) : error ? (_jsxs("div", { className: "text-center py-12 px-4 bg-red-100/80 border border-red-200 rounded-xl text-red-700 font-semibold shadow flex items-center justify-center gap-2", children: [_jsx(FaTimes, { className: "text-xl text-red-500" }), error?.message || "Đã xảy ra lỗi khi tải dữ liệu"] })) : (_jsxs(_Fragment, { children: [successMessage && (_jsxs("div", { className: "mb-6 p-4 bg-green-100/90 border border-green-300 rounded-xl text-green-900 flex items-center gap-3 shadow-lg animate-fade-in", children: [_jsx(FaCheck, { className: "text-2xl text-green-600" }), _jsx("span", { className: "font-semibold text-base", children: successMessage })] })), _jsx("div", { className: "bg-white/70 backdrop-blur-xl rounded-2xl border border-white/60 overflow-x-auto shadow-xl", children: _jsx("div", { className: "overflow-x-auto", children: _jsxs("table", { className: "w-full min-w-[900px]", children: [_jsx("thead", { className: "sticky top-0 z-10", children: _jsxs("tr", { className: "bg-gradient-to-r from-blue-100/80 via-white/80 to-blue-50/80 border-b border-blue-200/60 shadow-sm", children: [_jsx("th", { className: "text-left p-5 font-bold text-gray-900 tracking-wide", children: "M\u00E3 \u0111\u01A1n" }), _jsx("th", { className: "text-left p-5 font-bold text-gray-900 tracking-wide", children: "S\u1EA3n ph\u1EA9m" }), _jsx("th", { className: "text-left p-5 font-bold text-gray-900 tracking-wide", children: "Kh\u00E1ch h\u00E0ng" }), _jsx("th", { className: "text-left p-5 font-bold text-gray-900 tracking-wide", children: "L\u1ED9 tr\u00ECnh" }), _jsx("th", { className: "text-left p-5 font-bold text-gray-900 tracking-wide", children: "Chi ti\u1EBFt \u0111\u01A1n h\u00E0ng" }), _jsx("th", { className: "text-left p-5 font-bold text-gray-900 tracking-wide", children: "Ng\u00E0y t\u1EA1o" }), _jsx("th", { className: "text-left p-5 font-bold text-gray-900 tracking-wide", children: "Xe & T\u00E0i x\u1EBF" }), _jsx("th", { className: "text-left p-5 font-bold text-gray-900 tracking-wide", children: "Thao t\u00E1c" })] }) }), _jsx("tbody", { children: paginatedData.map((order, index) => (_jsxs("tr", { className: `border-b border-blue-100/40 hover:bg-blue-50/40 transition-all duration-200 ${index % 2 === 0 ? 'bg-white/60' : 'bg-blue-50/30'}`, children: [_jsx("td", { className: "p-5 align-top", children: _jsxs("div", { className: "flex items-center gap-2", children: [_jsx("span", { className: "font-bold text-lg text-blue-900", children: order.code }), _jsx("span", { className: `px-3 py-1 rounded-full text-xs font-bold border shadow-sm ml-2
                              ${order.status === 'Pending'
                                                                            ? 'bg-yellow-100 text-yellow-800 border-yellow-300'
                                                                            : order.status === 'Completed'
                                                                                ? 'bg-green-100 text-green-800 border-green-300'
                                                                                : 'bg-gray-100 text-gray-700 border-gray-300'}
                            `, children: order.status })] }) }), _jsx("td", { className: "p-5 align-top min-w-[200px]", children: _jsx("div", { className: "max-w-xs font-bold text-blue-900 text-lg", children: typeof productCounts[order.id] === "number"
                                                                    ? `${productCounts[order.id]} sản phẩm`
                                                                    : "Đang tải..." }) }), _jsxs("td", { className: "p-5 align-top min-w-[160px]", children: [_jsx("div", { className: "font-semibold text-gray-900 text-base", children: order.customer }), _jsx("div", { className: "text-sm text-gray-500 mt-1", children: order.description })] }), _jsx("td", { className: "p-5 align-top min-w-[180px]", children: _jsxs("div", { className: "text-sm text-gray-700", children: [_jsxs("div", { children: [_jsx("span", { className: "font-semibold text-blue-700", children: "T\u1EEB:" }), " ", order.from] }), _jsxs("div", { children: [_jsx("span", { className: "font-semibold text-blue-700", children: "\u0110\u1EBFn:" }), " ", order.to] })] }) }), _jsx("td", { className: "p-5 align-top", children: _jsx("button", { className: "px-3 py-1 rounded-lg bg-blue-100 hover:bg-blue-200 text-blue-700 font-semibold text-sm border border-blue-200 shadow transition-all duration-150", onClick: () => handleOpenDetail(order), children: "Xem chi ti\u1EBFt" }) }), _jsx("td", { className: "p-5 align-top", children: _jsx("div", { className: "text-blue-900 font-semibold text-base", children: order.date }) }), _jsx("td", { className: "p-5 align-top min-w-[220px]", children: _jsx("div", { className: "space-y-2", children: order.assignedVehicle && !editingOrders[order.id] ? (_jsxs("div", { className: "bg-green-50/90 border border-green-200 rounded-xl p-3 shadow flex flex-col gap-1", children: [_jsxs("div", { className: "flex items-center gap-2 mb-1", children: [_jsx(FaCar, { className: "text-green-600 text-lg" }), _jsx("span", { className: "font-bold text-green-900", children: order.assignedVehicle.licensePlate || `Vehicle #${order.assignedVehicle.id}` }), _jsxs("span", { className: "text-xs text-green-700", children: ["(", order.assignedVehicle.vehicleType, ")"] })] }), order.assignedVehicle.currentDriver && (_jsxs(_Fragment, { children: [_jsxs("div", { className: "text-sm font-semibold text-gray-800", children: ["\uD83D\uDC64 ", order.assignedVehicle.currentDriver.fullName || order.assignedVehicle.currentDriver.username] }), _jsxs("div", { className: "text-xs text-gray-600", children: ["\uD83D\uDCDE ", order.assignedVehicle.currentDriver?.phone || 'Chưa có SĐT'] })] })), _jsx("button", { className: "mt-2 px-3 py-1 bg-blue-100 hover:bg-blue-200 text-blue-700 rounded-lg text-xs font-semibold border border-blue-200 transition-all duration-150", onClick: () => setEditingOrders(prev => ({ ...prev, [order.id]: true })), children: "Ch\u1EC9nh s\u1EEDa" })] })) : order.currentDriver && !editingOrders[order.id] ? (_jsx("div", { className: "text-sm text-gray-700", children: (() => {
                                                                        // Tìm xe của tài xế hiện tại
                                                                        const driverVehicle = getDriverVehicle(order.currentDriver.id);
                                                                        return driverVehicle ? (_jsxs("div", { className: "bg-green-50/90 border border-green-200 rounded-xl p-3 shadow flex flex-col gap-1", children: [_jsxs("div", { className: "flex items-center gap-2 mb-1", children: [_jsx(FaCar, { className: "text-green-600 text-lg" }), _jsx("span", { className: "font-bold text-green-900", children: driverVehicle.licensePlate }), _jsxs("span", { className: "text-xs text-green-700", children: ["(", driverVehicle.vehicleType, ")"] })] }), _jsxs("div", { className: "text-sm font-semibold text-gray-800", children: ["\uD83D\uDC64 ", order.currentDriver.fullName || order.currentDriver.username] }), _jsxs("div", { className: "text-xs text-gray-600", children: ["\uD83D\uDCDE ", order.currentDriver?.phone || 'Chưa có SĐT'] })] })) : (_jsxs("div", { className: "bg-orange-50/90 border border-orange-200 rounded-xl p-3 shadow flex flex-col gap-1", children: [_jsxs("div", { className: "text-sm font-semibold text-gray-800", children: ["\uD83D\uDC64 ", order.currentDriver.fullName || order.currentDriver.username] }), _jsxs("div", { className: "text-xs text-gray-600", children: ["\uD83D\uDCDE ", order.currentDriver?.phone || 'Chưa có SĐT'] }), _jsx("div", { className: "text-xs text-orange-600 mt-1 font-bold", children: "\u26A0\uFE0F Ch\u01B0a c\u00F3 xe \u0111\u01B0\u1EE3c g\u00E1n" })] }));
                                                                    })() })) : (
                                                                // Vehicle Assignment Dropdown (hiển thị khi chưa có xe được gán HOẶC đang edit)
                                                                _jsx("div", { className: "space-y-2", children: selectedVehicles[order.id] ? (
                                                                    // Show selected vehicle info
                                                                    _jsx("div", { className: "bg-blue-50/90 border border-blue-200 rounded-xl p-3 shadow flex flex-col gap-1", children: (() => {
                                                                            const selectedVehicle = getVehicleById(selectedVehicles[order.id]);
                                                                            return selectedVehicle && selectedVehicle.currentDriver ? (_jsxs("div", { children: [_jsxs("div", { className: "flex items-center gap-2 mb-1", children: [_jsx(FaCar, { className: "text-blue-600 text-lg" }), _jsx("span", { className: "text-base font-bold text-blue-900", children: selectedVehicle.licensePlate }), _jsxs("span", { className: "text-xs text-blue-700", children: ["(", selectedVehicle.vehicleType, ")"] })] }), _jsxs("div", { className: "text-sm font-semibold text-blue-800", children: ["\uD83D\uDC64 ", selectedVehicle.currentDriver.fullName] }), _jsxs("div", { className: "text-xs text-blue-700", children: ["\uD83D\uDCDE ", (selectedVehicle.currentDriver?.phone) || 'Chưa có SĐT'] })] })) : null;
                                                                        })() })) : (
                                                                    // Vehicle Selection Dropdown
                                                                    _jsxs("select", { className: "w-full px-4 py-3 bg-white/90 border border-blue-200 rounded-xl focus:ring-2 focus:ring-blue-400 focus:border-blue-400 text-base shadow-sm font-medium text-blue-900", value: selectedVehicles[order.id] || "", onChange: (e) => handleVehicleSelect(order.id.toString(), e.target.value), children: [_jsx("option", { value: "", children: "Ch\u1ECDn xe..." }), vehicles
                                                                                .filter(vehicle => {
                                                                                // Luôn giữ lại xe đã chọn cho đơn này
                                                                                if (selectedVehicles[order.id] && vehicle.id.toString() === selectedVehicles[order.id])
                                                                                    return true;
                                                                                // Chỉ cho phép xe có tài xế chưa được gán cho đơn khác
                                                                                if (!vehicle.currentDriver || typeof vehicle.currentDriver.id === 'undefined')
                                                                                    return false;
                                                                                const driverId = vehicle.currentDriver.id;
                                                                                return !vehicles.some(v => v.currentDriver && typeof v.currentDriver.id !== 'undefined' && v.currentDriver.id === driverId && v.id !== vehicle.id);
                                                                            })
                                                                                .map(vehicle => (_jsxs("option", { value: vehicle.id, children: [vehicle.licensePlate, " - ", vehicle.currentDriver?.fullName || 'Không rõ tài xế'] }, vehicle.id)))] })) })) }) }), _jsx("td", { className: "p-5 align-top min-w-[160px]", children: (!order.assignedVehicle && !order.currentDriver && selectedVehicles[order.id]) ||
                                                                (editingOrders[order.id] && selectedVehicles[order.id]) ? (_jsxs("div", { className: "flex gap-3", children: [_jsx("button", { onClick: () => handleAssignVehicle(order.id.toString()), disabled: assigningOrders[order.id], className: "flex items-center justify-center w-10 h-10 bg-green-500 hover:bg-green-600 disabled:bg-green-300 text-white rounded-full shadow-lg transition-all duration-200 text-lg font-bold focus:outline-none focus:ring-2 focus:ring-green-400", title: editingOrders[order.id] ? "Cập nhật xe" : "Gán xe", children: assigningOrders[order.id] ? (_jsx("div", { className: "w-5 h-5 border-2 border-white border-t-transparent rounded-full animate-spin" })) : (_jsx(FaCheck, { className: "text-lg" })) }), _jsx("button", { onClick: () => editingOrders[order.id] ? handleCancelEdit(order.id.toString()) : handleCancelVehicleAssignment(order.id.toString()), className: "flex items-center justify-center w-10 h-10 bg-red-500 hover:bg-red-600 text-white rounded-full shadow-lg transition-all duration-200 text-lg font-bold focus:outline-none focus:ring-2 focus:ring-red-400", title: "H\u1EE7y", children: _jsx(FaTimes, { className: "text-lg" }) })] })) :
                                                                /* Hiển thị trạng thái đã gán với nút Unassign */
                                                                (order.assignedVehicle || order.currentDriver) && !editingOrders[order.id] ? (_jsxs("div", { className: "flex items-center gap-3", children: [_jsxs("div", { className: "flex items-center text-green-700 font-bold", children: [_jsx(FaCheck, { className: "text-xl" }), _jsx("span", { className: "ml-2 text-base", children: "\u0110\u00E3 g\u00E1n" })] }), _jsx("button", { onClick: () => handleUnassignVehicle(order.id.toString()), disabled: assigningOrders[order.id], className: "flex items-center justify-center w-9 h-9 bg-red-500 hover:bg-red-600 disabled:bg-red-300 text-white rounded-full shadow-lg transition-all duration-200 text-base font-bold focus:outline-none focus:ring-2 focus:ring-red-400", title: "G\u1EE1 g\u00E1n xe", children: assigningOrders[order.id] ? (_jsx("div", { className: "w-4 h-4 border-2 border-white border-t-transparent rounded-full animate-spin" })) : (_jsx(FaTimes, { className: "text-base" })) })] })) : null })] }, order.id))) })] }) }) }), totalPages > 1 && (_jsxs("div", { className: "flex flex-col sm:flex-row justify-center items-center gap-4 mt-8", children: [_jsx("button", { onClick: () => setCurrentPage(p => Math.max(1, p - 1)), disabled: currentPage === 1, className: "px-4 py-2 rounded-xl bg-blue-100 hover:bg-blue-200 disabled:opacity-50 disabled:cursor-not-allowed text-blue-700 font-bold shadow transition-all duration-150", children: "< Tr\u01B0\u1EDBc" }), _jsx("div", { className: "hidden sm:flex items-center gap-2", children: Array.from({ length: Math.min(5, totalPages) }, (_, i) => {
                                            let pageNum;
                                            if (totalPages <= 5) {
                                                pageNum = i + 1;
                                            }
                                            else if (currentPage <= 3) {
                                                pageNum = i + 1;
                                            }
                                            else if (currentPage >= totalPages - 2) {
                                                pageNum = totalPages - 4 + i;
                                            }
                                            else {
                                                pageNum = currentPage - 2 + i;
                                            }
                                            return (_jsx("button", { onClick: () => setCurrentPage(pageNum), className: `w-10 h-10 rounded-lg font-bold transition-all duration-150 ${currentPage === pageNum
                                                    ? 'bg-blue-600 text-white shadow-lg'
                                                    : 'bg-white hover:bg-blue-50 text-blue-700 border border-blue-200'}`, children: pageNum }, pageNum));
                                        }) }), _jsx("div", { className: "sm:hidden flex items-center gap-2", children: Array.from({ length: Math.min(3, totalPages) }, (_, i) => {
                                            let pageNum;
                                            if (totalPages <= 3) {
                                                pageNum = i + 1;
                                            }
                                            else if (currentPage <= 2) {
                                                pageNum = i + 1;
                                            }
                                            else if (currentPage >= totalPages - 1) {
                                                pageNum = totalPages - 2 + i;
                                            }
                                            else {
                                                pageNum = currentPage - 1 + i;
                                            }
                                            return (_jsx("button", { onClick: () => setCurrentPage(pageNum), className: `w-10 h-10 rounded-lg font-bold transition-all duration-150 ${currentPage === pageNum
                                                    ? 'bg-blue-600 text-white shadow-lg'
                                                    : 'bg-white hover:bg-blue-50 text-blue-700 border border-blue-200'}`, children: pageNum }, pageNum));
                                        }) }), _jsx("button", { onClick: () => setCurrentPage(p => Math.min(totalPages, p + 1)), disabled: currentPage === totalPages, className: "px-4 py-2 rounded-xl bg-blue-100 hover:bg-blue-200 disabled:opacity-50 disabled:cursor-not-allowed text-blue-700 font-bold shadow transition-all duration-150", children: "Ti\u1EBFp >" })] })), _jsxs("div", { className: "text-center mt-4 text-gray-600", children: ["Hi\u1EC3n th\u1ECB ", data.length > 0 ? ((currentPage - 1) * PAGE_SIZE + 1) : 0, "-", data.length > 0 ? ((currentPage - 1) * PAGE_SIZE + data.length) : 0, "trong t\u1ED5ng s\u1ED1 ", totalOrders, " \u0111\u01A1n h\u00E0ng", totalPages > 1 && (_jsxs("span", { className: "ml-2", children: ["| Trang ", currentPage, " / ", totalPages] }))] }), data.length === 0 && (_jsxs("div", { className: "text-center py-16 text-gray-500", children: [_jsx(FaUserCog, { className: "text-5xl mx-auto mb-4 opacity-40" }), _jsx("p", { className: "text-xl font-semibold", children: "Kh\u00F4ng c\u00F3 \u0111\u01A1n h\u00E0ng ch\u1EDD ph\u00E2n c\u00F4ng" })] }))] }))] })] }));
}
