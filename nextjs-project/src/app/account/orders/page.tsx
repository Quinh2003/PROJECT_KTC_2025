"use client";

import { useEffect, useState, useCallback } from "react";
import {
  Table,
  Card,
  Tag,
  Button,
  Space,
  Typography,
  Row,
  Col,
  Tooltip,
  Modal,
  Timeline,
  message,
} from "antd";
import {
  orderApi,
  type PaginatedOrderSummaryResponse,
  type OrderSummary,
} from "@/services/orderService";
import OrderDetailModal from "./components/OrderDetailModal";
import { OrderFilters } from "./components/OrderFilters";
import type { Dayjs } from "dayjs";
import {
  PlusOutlined,
  EyeOutlined,
  HistoryOutlined,
} from "@ant-design/icons";
import InvoiceButton from "@/components/InvoiceButton";
import { useRouter } from "next/navigation";

const { Title } = Typography;
interface Order {
  id: string;
  created_at: string;
  store_name: string;
  shipping_address: string;
  total_items: number;
  cod_amount: number;
  shipping_fee: number;
  status: {
    id: number;
    name: string;
    color: string;
  };
  tracking_updates: {
    time: string;
    status: string;
    description: string;
  }[];
}

const getStatusId = (status: string): number => {
  const statusMap: { [key: string]: number } = {
    PENDING: 1,
    PROCESSING: 2,
    SHIPPED: 3,
    DELIVERED: 4,
    COMPLETED: 5,
    CANCELLED: 6,
  };
  return statusMap[status] || 1;
};

const getStatusColor = (status: string): string => {
  const colorMap: { [key: string]: string } = {
    PENDING: "default",
    PROCESSING: "processing",
    SHIPPED: "warning",
    DELIVERED: "success",
    COMPLETED: "success",
    CANCELLED: "error",
    FAILED: "error",
  };
  return colorMap[status] || "default";
};

export default function OrdersPage() {
  const router = useRouter();
  const [messageApi, contextHolder] = message.useMessage();
  const [orders, setOrders] = useState<Order[]>([]);
  const [loading, setLoading] = useState(true);
  const [searchText, setSearchText] = useState("");
  const [dateRange, setDateRange] = useState<[Dayjs | null, Dayjs | null]>([
    null,
    null,
  ]);
  const [statusFilter, setStatusFilter] = useState<number[]>([]);
  const [isTrackingModalVisible, setIsTrackingModalVisible] = useState(false);
  const [selectedOrder, setSelectedOrder] = useState<Order | null>(null);
  // Pagination states
  const [currentPage, setCurrentPage] = useState(1);
  const [pageSize, setPageSize] = useState(10);
  const [totalRecords, setTotalRecords] = useState(0);

  // Modal chi tiết đơn hàng
  const [isDetailModalVisible, setIsDetailModalVisible] = useState(false);
  const [detailOrderId, setDetailOrderId] = useState<string | null>(null);

  // Store ID để sử dụng cho search
  const [storeId, setStoreId] = useState<number | null>(null);

  const fetchOrders = useCallback(
    async (page: number = 1, size: number = 10) => {
      setLoading(true);
      try {
        // Get user from localStorage
        const userStr = localStorage.getItem("user");
        if (!userStr) {
          console.error("User not found in localStorage");
          return;
        }
        const user = JSON.parse(userStr);
        const response = await orderApi.getOrdersByUserPaginated(
          user.id,
          page,
          size
        );

        const formattedOrders: Order[] = response.data.map((order) => ({
          id: order.orderId?.toString() || "N/A",
          created_at: order.createdAt || new Date().toISOString(),
          store_name: `Store ${order.storeId || "Unknown"}`,
          shipping_address: order.deliveryAddress || "No address provided",
          total_items: order.totalItems || 0,
          cod_amount: 0, // Not available in the API
          shipping_fee: order.deliveryFee || 0,
          status: {
            id: getStatusId(order.orderStatus || "PENDING"),
            name: order.orderStatus || "PENDING",
            color: getStatusColor(order.orderStatus || "PENDING"),
          },
          tracking_updates: [
            {
              time: order.createdAt || new Date().toISOString(),
              status: order.orderStatus || "PENDING",
              description: `Order ${(
                order.orderStatus || "PENDING"
              ).toLowerCase()}`,
            },
          ],
        }));

        setOrders(formattedOrders);
        setTotalRecords(response.totalRecords);
        setCurrentPage(response.pageNumber);

        // Lấy storeId từ đơn hàng đầu tiên để sử dụng cho search
        if (response.data.length > 0 && !storeId) {
          setStoreId(response.data[0].storeId);
        }
      } catch (error) {
        console.error("Failed to fetch orders:", error);
      } finally {
        setLoading(false);
      }
    },
    [storeId]
  );

  useEffect(() => {
    fetchOrders(currentPage, pageSize);
  }, [currentPage, pageSize, fetchOrders]);

  // Effect để xử lý tìm kiếm thống nhất
  useEffect(() => {
    const performUnifiedSearch = async () => {
      if (!storeId) return;

      try {
        setLoading(true);

        // Prepare search parameters
        let orderId: number | undefined;
        let fromDate: string | undefined;
        let toDate: string | undefined;

        // Check if searchText is a valid orderId
        const orderIdNumber = parseInt(searchText.trim());
        if (!isNaN(orderIdNumber) && searchText.trim()) {
          orderId = orderIdNumber;
        }

        // Prepare date range
        if (dateRange && dateRange[0] && dateRange[1]) {
          fromDate = dateRange[0].format("YYYY-MM-DD");
          toDate = dateRange[1].format("YYYY-MM-DD");
        }

        // Prepare status filter - convert status IDs to status names (support multiple)
        let statusList: string[] | undefined;
        if (statusFilter.length > 0) {
          const statusMap: { [key: number]: string } = {
            1: "PENDING",
            2: "PROCESSING",
            3: "SHIPPED",
            4: "DELIVERED",
            5: "COMPLETED",
            6: "CANCELLED",
          };
          statusList = statusFilter
            .map((statusId) => statusMap[statusId])
            .filter((statusName) => statusName); // Filter out undefined values
        }

        // Call unified search API
        const searchResults = await orderApi.searchOrdersUnified(
          storeId,
          currentPage,
          pageSize,
          orderId,
          fromDate,
          toDate,
          statusList
        );

        // Format results
        const formattedSearchResults: Order[] = searchResults.data.map(
          (order: OrderSummary) => ({
            id: order.orderId?.toString() || "N/A",
            created_at: order.createdAt || new Date().toISOString(),
            store_name: `Store ${order.storeId || "Unknown"}`,
            shipping_address: order.deliveryAddress || "No address provided",
            total_items: order.totalItems || 0,
            cod_amount: 0,
            shipping_fee: order.deliveryFee || 0,
            status: {
              id: getStatusId(order.orderStatus || "PENDING"),
              name: order.orderStatus || "PENDING",
              color: getStatusColor(order.orderStatus || "PENDING"),
            },
            tracking_updates: [
              {
                time: order.createdAt || new Date().toISOString(),
                status: order.orderStatus || "PENDING",
                description: `Order ${(
                  order.orderStatus || "PENDING"
                ).toLowerCase()}`,
              },
            ],
          })
        );

        setOrders(formattedSearchResults);
        setTotalRecords(searchResults.totalRecords);
        setLoading(false);
      } catch (error) {
        console.error("Unified search failed:", error);
        setLoading(false);
        messageApi.error("Tìm kiếm thất bại");
      }
    };

    // Determine if we should use unified search or load regular list
    const hasSearchCriteria =
      (searchText.trim() && !isNaN(parseInt(searchText.trim()))) || // Valid orderId
      (dateRange && dateRange[0] && dateRange[1]) || // Date range selected
      statusFilter.length > 0; // Status filter selected

    if (hasSearchCriteria) {
      // Use unified search
      const timeoutId = setTimeout(performUnifiedSearch, 500);
      return () => clearTimeout(timeoutId);
    } else {
      // Load regular list when no search criteria
      fetchOrders(currentPage, pageSize);
    }
  }, [
    searchText,
    dateRange,
    statusFilter,
    storeId,
    currentPage,
    pageSize,
    messageApi,
    fetchOrders,
  ]);

  const showTrackingModal = (order: Order) => {
    setSelectedOrder(order);
    setIsTrackingModalVisible(true);
  };

  const handleCopyOrderId = (orderId: string) => {
    navigator.clipboard
      .writeText(orderId)
      .then(() => {
        messageApi.success(`Đã sao chép mã đơn hàng: ${orderId}`);
      })
      .catch(() => {
        messageApi.error("Không thể sao chép mã đơn hàng");
      });
  };

  const handleInvoiceCreated = (invoice: { invoiceNumber: string }) => {
    // Refresh orders list or show success message
    messageApi.success(`Hóa đơn ${invoice.invoiceNumber} đã được tạo thành công!`);
    
    // Optionally refresh the orders list to reflect any changes
    // This can help ensure the UI is in sync with the backend state
    setTimeout(() => {
      fetchOrders(currentPage, pageSize);
    }, 1500); // Small delay to ensure backend is updated
  };

  const columns = [
    {
      title: "Mã đơn hàng",
      dataIndex: "id",
      key: "id",
      render: (text: string) => (
        <a
          onClick={() => handleCopyOrderId(text)}
          style={{ cursor: "pointer" }}
          title="Click để sao chép mã đơn hàng"
        >
          {text}
        </a>
      ),
    },
    {
      title: "Ngày tạo",
      dataIndex: "created_at",
      key: "created_at",
      render: (date: string) => new Date(date).toLocaleDateString("vi-VN"),
    },
    {
      title: "Địa chỉ nhận hàng",
      dataIndex: "shipping_address",
      key: "shipping_address",
      ellipsis: true,
    },
    {
      title: "Số SP",
      dataIndex: "total_items",
      key: "total_items",
      align: "center" as const,
    },
    {
      title: "Phí vận chuyển",
      dataIndex: "shipping_fee",
      key: "shipping_fee",
      align: "right" as const,
      render: (amount: number | null) =>
        (amount || 0).toLocaleString("vi-VN", {
          style: "currency",
          currency: "VND",
        }),
    },
    {
      title: "Trạng thái",
      dataIndex: "status",
      key: "status",
      render: (status: { name: string; color: string }) => (
        <Tag color={status.color}>{status.name}</Tag>
      ),
    },
    {
      title: "Thao tác",
      key: "action",
      render: (_: unknown, record: Order) => (
        <Space size="middle">
          <Tooltip title="Xem chi tiết">
            <Button
              type="text"
              icon={<EyeOutlined />}
              onClick={() => {
                setDetailOrderId(record.id);
                setIsDetailModalVisible(true);
              }}
            />
          </Tooltip>
          <Tooltip title="Lịch sử vận chuyển">
            <Button
              type="text"
              icon={<HistoryOutlined />}
              onClick={() => showTrackingModal(record)}
            />
          </Tooltip>
          <InvoiceButton
            orderId={parseInt(record.id)}
            orderStatus={record.status?.name}
            onInvoiceCreated={handleInvoiceCreated}
            type="text"
          />
        </Space>
      ),
    },
  ];

  return (
    <>
      <Card className="orders-page" style={{ margin: "24px" }}>
        <div className="orders-header" style={{ marginBottom: "24px" }}>
          <Row
            justify="space-between"
            align="middle"
            style={{ marginBottom: 24 }}
          >
            <Col>
              <Title level={2}>Quản lý đơn hàng</Title>
            </Col>
            <Col>
              <Button
                type="primary"
                icon={<PlusOutlined />}
                onClick={() => router.push("/account/orders/new")}
              >
                Tạo đơn hàng
              </Button>
            </Col>
          </Row>

          <OrderFilters
            searchText={searchText}
            dateRange={dateRange}
            statusFilter={statusFilter}
            onSearchChange={setSearchText}
            onDateRangeChange={(dates) => setDateRange(dates)}
            onStatusFilterChange={setStatusFilter}
          />
        </div>

        <Table
          columns={columns}
          dataSource={orders}
          loading={loading}
          rowKey="id"
          pagination={{
            current: currentPage,
            pageSize: pageSize,
            total: totalRecords,
            showTotal: (total, range) =>
              `${range[0]}-${range[1]} của ${total} đơn hàng`,
            position: ["bottomCenter"],
            onChange: (page, size) => {
              setCurrentPage(page);
              setPageSize(size || 10);
            },
            showSizeChanger: true,
            pageSizeOptions: ["10", "20", "50"],
          }}
        />
      </Card>

      {/* Modal chi tiết đơn hàng */}
      {isDetailModalVisible && detailOrderId && (
        <OrderDetailModal
          orderId={Number(detailOrderId)}
          onClose={() => setIsDetailModalVisible(false)}
        />
      )}

      {/* Tracking Modal */}
      <Modal
        title={
          <Space>
            <HistoryOutlined />
            <span>Lịch sử vận chuyển</span>
          </Space>
        }
        open={isTrackingModalVisible}
        onCancel={() => setIsTrackingModalVisible(false)}
        footer={null}
        width={600}
      >
        {selectedOrder && (
          <>
            <p>
              <strong>Mã đơn hàng:</strong> {selectedOrder.id}
            </p>
            <Timeline
              items={selectedOrder.tracking_updates.map((update) => ({
                color: "blue",
                children: (
                  <>
                    <p style={{ margin: 0 }}>
                      <strong>{update.status}</strong>
                    </p>
                    <p style={{ margin: 0 }}>{update.description}</p>
                    <p style={{ margin: 0, color: "#8c8c8c" }}>
                      {new Date(update.time).toLocaleString("vi-VN")}
                    </p>
                  </>
                ),
              }))}
            />
          </>
        )}
      </Modal>
      {contextHolder}
    </>
  );
}
// ...existing code...
