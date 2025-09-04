"use client";

import { useEffect, useState } from "react";
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
} from "antd";
import { orderApi } from "@/services/orderService";
import { OrderFilters } from "./components/OrderFilters";
import type { Dayjs } from "dayjs";
import {
  PlusOutlined,
  EyeOutlined,
  HistoryOutlined,
  FilePdfOutlined,
} from "@ant-design/icons";
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
    RECEIVED: 2,
    IN_PROGRESS: 3,
    COMPLETED: 4,
    CANCELLED: 5,
  };
  return statusMap[status] || 1;
};

const getStatusColor = (status: string): string => {
  const colorMap: { [key: string]: string } = {
    PENDING: "default",
    RECEIVED: "processing",
    IN_PROGRESS: "warning",
    COMPLETED: "success",
    CANCELLED: "error",
  };
  return colorMap[status] || "default";
};

export default function OrdersPage() {
  const router = useRouter();
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

  useEffect(() => {
    const fetchOrders = async () => {
      setLoading(true);
      try {
        // Get user from localStorage
        const userStr = localStorage.getItem("user");
        if (!userStr) {
          console.error("User not found in localStorage");
          return;
        }
        const user = JSON.parse(userStr);
        const ordersData = await orderApi.getOrdersByUser(user.id);
        const formattedOrders: Order[] = ordersData.map((order, index) => ({
          id: `ORD${user.id}${String(order.orderId).padStart(5, "0")}-${index}`,
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
      } catch (error) {
        console.error("Failed to fetch orders:", error);
      } finally {
        setLoading(false);
      }
    };

    fetchOrders();
  }, []);

  const showTrackingModal = (order: Order) => {
    setSelectedOrder(order);
    setIsTrackingModalVisible(true);
  };

  const handleViewInvoice = (orderId: string) => {
    // TODO: Implement invoice viewing functionality
    console.log(`View invoice for order ${orderId}`);
  };

  const columns = [
    {
      title: "Mã đơn hàng",
      dataIndex: "id",
      key: "id",
      render: (text: string) => {
        // Chỉ hiển thị phần mã đơn hàng không bao gồm index
        const orderCode = text.split("-")[0];
        return <a>{orderCode}</a>;
      },
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
              onClick={() => router.push(`/account/orders/${record.id}`)}
            />
          </Tooltip>
          <Tooltip title="Lịch sử vận chuyển">
            <Button
              type="text"
              icon={<HistoryOutlined />}
              onClick={() => showTrackingModal(record)}
            />
          </Tooltip>
          <Tooltip title="Hoá đơn điện tử">
            <Button
              type="text"
              icon={<FilePdfOutlined />}
              onClick={() => handleViewInvoice(record.id)}
            />
          </Tooltip>
        </Space>
      ),
    },
  ];

  return (
    <Card className="orders-page" style={{ margin: "24px" }}>
      <div className="orders-header" style={{ marginBottom: "24px" }}>
        <Row justify="space-between" align="middle" style={{ marginBottom: 24 }}>
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
          total: orders.length,
          pageSize: 10,
          showTotal: (total) => `${total} đơn hàng`,
          position: ['bottomCenter']
        }}
      />

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
    </Card>
  );
}
