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
  Grid,
} from "antd";
import { orderApi, type OrderSummary } from "@/services/orderService";
import OrderDetailModal from "./components/OrderDetailModal";
import { OrderFilters } from "./components/OrderFilters";
import type { Dayjs } from "dayjs";
import { PlusOutlined, EyeOutlined, HistoryOutlined } from "@ant-design/icons";
import InvoiceButton from "@/components/InvoiceButton";
import { useRouter } from "next/navigation";

const { Title } = Typography;
const { useBreakpoint } = Grid;

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
    COMPLETED: 2,
    CANCELLED: 3,
    PROCESSING: 4,
    SHIPPING: 5,
    DELIVERED: 6,
  };
  return statusMap[status] || 1;
};

const getStatusColor = (status: string): string => {
  const colorMap: { [key: string]: string } = {
    PENDING: "default",
    COMPLETED: "success",
    CANCELLED: "error",
    PROCESSING: "processing",
    SHIPPING: "warning",
    DELIVERED: "success",
    FAILED: "error",
  };
  return colorMap[status] || "default";
};

export default function OrdersPage() {
  const router = useRouter();
  const screens = useBreakpoint();
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
  const [currentPage, setCurrentPage] = useState(1);
  const [pageSize, setPageSize] = useState(10);
  const [totalRecords, setTotalRecords] = useState(0);
  const [isDetailModalVisible, setIsDetailModalVisible] = useState(false);
  const [detailOrderId, setDetailOrderId] = useState<string | null>(null);
  const [storeId, setStoreId] = useState<number | null>(null);

  const fetchOrders = useCallback(
    async (page: number = 1, size: number = 10) => {
      setLoading(true);
      try {
        const userStr = localStorage.getItem("user");
        if (!userStr) return;
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
        }));

        setOrders(formattedOrders);
        setTotalRecords(response.totalRecords);
        setCurrentPage(response.pageNumber);

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

  useEffect(() => {
    const performUnifiedSearch = async () => {
      if (!storeId) return;
      try {
        setLoading(true);

        let orderId: number | undefined;
        let fromDate: string | undefined;
        let toDate: string | undefined;

        const orderIdNumber = parseInt(searchText.trim());
        if (!isNaN(orderIdNumber) && searchText.trim()) {
          orderId = orderIdNumber;
        }

        if (dateRange && dateRange[0] && dateRange[1]) {
          fromDate = dateRange[0].format("YYYY-MM-DD");
          toDate = dateRange[1].format("YYYY-MM-DD");
        }

        let statusList: string[] | undefined;
        if (statusFilter.length > 0) {
          const statusMap: { [key: number]: string } = {
            1: "PENDING",
            2: "COMPLETED",
            3: "CANCELLED",
            4: "PROCESSING",
            5: "SHIPPING",
            6: "DELIVERED",
          };
          statusList = statusFilter
            .map((id) => statusMap[id])
            .filter(Boolean) as string[];
        }

        const searchResults = await orderApi.searchOrdersUnified(
          storeId,
          currentPage,
          pageSize,
          orderId,
          fromDate,
          toDate,
          statusList
        );

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
        messageApi.error("Search failed");
      }
    };

    const hasSearchCriteria =
      (searchText.trim() && !isNaN(parseInt(searchText.trim()))) ||
      (dateRange && dateRange[0] && dateRange[1]) ||
      statusFilter.length > 0;

    if (hasSearchCriteria) {
      const timeoutId = setTimeout(performUnifiedSearch, 500);
      return () => clearTimeout(timeoutId);
    } else {
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
      .then(() => messageApi.success(`Order ID copied: ${orderId}`))
      .catch(() => messageApi.error("Unable to copy Order ID"));
  };

  const handleInvoiceCreated = (invoice: { invoiceNumber: string }) => {
    messageApi.success(
      `Invoice ${invoice.invoiceNumber} has been created successfully!`
    );
    setTimeout(() => {
      fetchOrders(currentPage, pageSize);
    }, 1500);
  };

  const columns = [
    {
      title: "Order ID",
      dataIndex: "id",
      key: "id",
      width: 100,
      fixed: "left" as const,
      render: (text: string) => (
        <a
          onClick={() => handleCopyOrderId(text)}
          style={{
            cursor: "pointer",
            color: "#15803d",
          }}
          title="Click to copy Order ID"
        >
          {text}
        </a>
      ),
    },
    {
      title: "Created Date",
      dataIndex: "created_at",
      key: "created_at",
      width: 120,
      render: (date: string) => new Date(date).toLocaleDateString("en-US"),
    },
    {
      title: "Shipping Address",
      dataIndex: "shipping_address",
      key: "shipping_address",
      width: 200,
      ellipsis: {
        showTitle: false,
      },
      render: (address: string) => (
        <Tooltip placement="topLeft" title={address}>
          {address}
        </Tooltip>
      ),
    },
    {
      title: "Total Items",
      dataIndex: "total_items",
      key: "total_items",
      width: 100,
      align: "center" as const,
    },
    {
      title: "Shipping Fee",
      dataIndex: "shipping_fee",
      key: "shipping_fee",
      width: 120,
      align: "right" as const,
      render: (amount: number | null) =>
        (amount || 0).toLocaleString("vi-VN", {
          style: "currency",
          currency: "VND",
        }),
    },
    {
      title: "Status",
      dataIndex: "status",
      key: "status",
      width: 100,
      render: (status: { name: string; color: string }) => (
        <Tag color={status.color}>{status.name}</Tag>
      ),
    },
    {
      title: "Actions",
      key: "action",
      width: 150,
      fixed: "right" as const,
      render: (_: unknown, record: Order) => (
        <Space size="middle">
          <Tooltip title="View Details">
            <Button
              type="text"
              icon={<EyeOutlined />}
              onClick={() => {
                setDetailOrderId(record.id);
                setIsDetailModalVisible(true);
              }}
            />
          </Tooltip>
          <Tooltip title="Tracking History">
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
    <div
      style={{
        minHeight: "100vh",
        padding: "24px",
      }}
    >
      <div className="orders-header" style={{ marginBottom: "24px" }}>
        <Row
          justify="space-between"
          align="middle"
          style={{ marginBottom: 24 }}
          wrap
        >
          <Col xs={24} sm={12} md={16} lg={18}>
            <Title
              level={2}
              style={{
                marginBottom: 12,
                color: "#15803d",
                fontSize: "clamp(20px, 4vw, 28px)",
              }}
            >
              Order Management
            </Title>
          </Col>
          <Col xs={24} sm={12} md={8} lg={6} style={{ textAlign: "right" }}>
            <Button
              type="primary"
              icon={<PlusOutlined />}
              onClick={() => router.push("/account/orders/new")}
              block={!screens.md}
              style={{
                backgroundColor: "#15803d",
                borderColor: "#15803d",
                borderRadius: 8,
                height: 40,
                fontWeight: 500,
              }}
            >
              Create Order
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

      <div
        style={{
          width: "100%",
          overflowX: "auto",
          border: "1px solid #f0f0f0",
          borderRadius: 8,
          backgroundColor: "#fff",
        }}
      >
        <Table
          columns={columns}
          dataSource={orders}
          loading={loading}
          rowKey="id"
          scroll={{ x: 890 }}
          style={{
            minWidth: 890,
          }}
          size="middle"
          pagination={{
            current: currentPage,
            pageSize: pageSize,
            total: totalRecords,
            showTotal: (total, range) =>
              `${range[0]}-${range[1]} of ${total} orders`,
            position: ["bottomCenter"],
            onChange: (page, size) => {
              setCurrentPage(page);
              setPageSize(size || 10);
            },
            showSizeChanger: true,
            pageSizeOptions: ["10", "20", "50"],
          }}
        />
      </div>

      {isDetailModalVisible && detailOrderId && (
        <OrderDetailModal
          orderId={Number(detailOrderId)}
          onClose={() => {
            setIsDetailModalVisible(false);
            setDetailOrderId(null);
          }}
        />
      )}

      <Modal
        title={
          <div
            style={{
              color: "#15803d",
              fontSize: "18px",
              fontWeight: 600,
            }}
          >
            <Space>
              <HistoryOutlined />
              <span>Tracking History</span>
            </Space>
          </div>
        }
        open={isTrackingModalVisible}
        onCancel={() => setIsTrackingModalVisible(false)}
        footer={null}
        width={screens.md ? 600 : "90%"}
        style={{ top: screens.md ? 80 : 30 }}
        styles={{
          body: {
            padding: screens.md ? 24 : 16,
            backgroundColor: "#fafafa",
          },
          header: {
            backgroundColor: "#fff",
            borderBottom: "1px solid #f0f0f0",
            padding: "16px 24px",
          },
        }}
      >
        {selectedOrder && (
          <div
            style={{
              backgroundColor: "#fff",
              padding: "20px",
              borderRadius: 8,
              boxShadow: "0 1px 4px rgba(0,0,0,0.04)",
            }}
          >
            <p
              style={{
                marginBottom: 16,
                padding: "12px",
                backgroundColor: "#f6ffed",
                borderRadius: 6,
                border: "1px solid #d9f7be",
              }}
            >
              <strong style={{ color: "#15803d" }}>Order ID:</strong>{" "}
              {selectedOrder.id}
            </p>
            <Timeline
              items={selectedOrder.tracking_updates.map((update) => ({
                color: "#15803d",
                children: (
                  <>
                    <p style={{ margin: 0, fontWeight: 600, color: "#15803d" }}>
                      {update.status}
                    </p>
                    <p style={{ margin: "4px 0", color: "#666" }}>
                      {update.description}
                    </p>
                    <p style={{ margin: 0, color: "#999", fontSize: "12px" }}>
                      {new Date(update.time).toLocaleString("en-US")}
                    </p>
                  </>
                ),
              }))}
            />
          </div>
        )}
      </Modal>
      {contextHolder}
    </div>
  );
}
