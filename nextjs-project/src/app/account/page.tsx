"use client";

import {
  Typography,
  Card,
  Row,
  Col,
  Button,
  Statistic,
  Table,
  Tag,
  Space,
  Tooltip,
  message,
} from "antd";
import Link from "next/link";
import {
  PlusOutlined,
  BoxPlotOutlined,
  EnvironmentOutlined,
  DollarOutlined,
  BarChartOutlined,
  CarOutlined,
  CheckCircleOutlined,
  EyeOutlined,
} from "@ant-design/icons";
import { useEffect, useState, useCallback } from "react";
import { useRouter } from "next/navigation";
import { orderApi } from "@/services/orderService";
import { storeService } from "@/services/storeService";
import OrderDetailModal from "./orders/components/OrderDetailModal";

const { Title, Text } = Typography;

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
  };
  return colorMap[status] || "default";
};

export default function CustomerAccount() {
  const router = useRouter();
  const [messageApi, contextHolder] = message.useMessage();
  const [recentOrders, setRecentOrders] = useState<Order[]>([]);
  const [loading, setLoading] = useState(true);
  const [stats, setStats] = useState({
    totalOrders: 0,
    shippingOrders: 0,
    completedOrders: 0,
  });

  // Modal chi tiết đơn hàng
  const [isDetailModalVisible, setIsDetailModalVisible] = useState(false);
  const [detailOrderId, setDetailOrderId] = useState<string | null>(null);

  const fetchRecentOrders = useCallback(async () => {
    setLoading(true);
    try {
      const userStr = localStorage.getItem("user");
      if (!userStr) return;
      const user = JSON.parse(userStr);

      const response = await orderApi.getOrdersByUserPaginated(user.id, 1, 3);

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
      }));

      setRecentOrders(formattedOrders);
    } catch (error) {
      messageApi.error("Unable to load order list");
    } finally {
      setLoading(false);
    }
  }, [messageApi]);

  const fetchOrderStats = useCallback(async () => {
    try {
      const userStr = localStorage.getItem("user");
      if (!userStr) return;
      const user = JSON.parse(userStr);

      const stores = await storeService.getStoresByUserId(user.id.toString());
      if (stores.length === 0) {
        setStats({
          totalOrders: 0,
          shippingOrders: 0,
          completedOrders: 0,
        });
        return;
      }

      const storeId = stores[0].id;
      const statsResponse = await orderApi.getUserOrderStats(storeId);

      setStats({
        totalOrders: statsResponse.totalOrders,
        shippingOrders: statsResponse.processingOrders,
        completedOrders: statsResponse.completedOrders,
      });
    } catch (error) {
      messageApi.error("Unable to load order statistics");
    }
  }, [messageApi]);

  useEffect(() => {
    fetchRecentOrders();
    fetchOrderStats();
  }, [fetchRecentOrders, fetchOrderStats]);

  const recentOrdersColumns = [
    {
      title: "Order ID",
      dataIndex: "id",
      key: "id",
      render: (text: string) => (
        <a
          onClick={() => {
            setDetailOrderId(text);
            setIsDetailModalVisible(true);
          }}
          style={{ cursor: "pointer", color: "#15803d" }}
        >
          {text}
        </a>
      ),
    },
    {
      title: "Created Date",
      dataIndex: "created_at",
      key: "created_at",
      render: (date: string) => new Date(date).toLocaleDateString("en-US"),
    },
    {
      title: "Delivery Address",
      dataIndex: "shipping_address",
      key: "shipping_address",
      ellipsis: true,
    },
    {
      title: "Items",
      dataIndex: "total_items",
      key: "total_items",
      align: "center" as const,
    },
    {
      title: "Shipping Fee",
      dataIndex: "shipping_fee",
      key: "shipping_fee",
      align: "right" as const,
      render: (amount: number) =>
        amount.toLocaleString("en-US", {
          style: "currency",
          currency: "USD",
        }),
    },
    {
      title: "Status",
      dataIndex: "status",
      key: "status",
      render: (status: { name: string; color: string }) => (
        <Tag color={status.color}>{status.name}</Tag>
      ),
    },
    {
      title: "Actions",
      key: "action",
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
        </Space>
      ),
    },
  ];

  return (
    <div style={{ maxWidth: "100%", padding: "24px" }}>
      <div style={{ display: "flex", flexDirection: "column", gap: "24px" }}>
        {/* Welcome Section */}
        <Card
          style={{
            borderRadius: 16,
            border: "1px solid #f0f0f0",
          }}
        >
          <Title
            level={2}
            style={{
              marginBottom: 16,
              fontSize: "clamp(20px, 4vw, 28px)",
              color: "#15803d",
              textAlign: "center",
            }}
          >
            Welcome to Fast Route! <CarOutlined />
          </Title>
          <Text
            style={{
              fontSize: "clamp(14px, 3vw, 16px)",
              display: "block",
              textAlign: "center",
              color: "#4b5563",
            }}
          >
            Smart delivery service with route optimization technology. We are
            committed to providing fast, safe, and cost-effective delivery
            experience.
          </Text>
        </Card>

        {/* Feature cards */}
        <Row gutter={[24, 24]}>
          <Col xs={24} sm={12} md={8}>
            <Link href="/account/orders/new">
              <Card
                hoverable
                style={{
                  borderRadius: 16,
                  height: "100%",
                  border: "1px solid #e5f3ff",
                  background:
                    "linear-gradient(135deg, #f0f9ff 0%, #e0f2fe 100%)",
                  transition: "all 0.3s ease",
                }}
                styles={{ body: { padding: "32px 24px" } }}
              >
                <div style={{ textAlign: "center", marginBottom: 16 }}>
                  <BoxPlotOutlined style={{ fontSize: 48, color: "#0284c7" }} />
                </div>
                <Title
                  level={4}
                  style={{
                    textAlign: "center",
                    color: "#0284c7",
                    marginBottom: 12,
                  }}
                >
                  Create Order
                </Title>
                <Text
                  type="secondary"
                  style={{
                    display: "block",
                    textAlign: "center",
                    fontSize: 14,
                    color: "#0369a1",
                  }}
                >
                  Create new delivery orders quickly
                </Text>
              </Card>
            </Link>
          </Col>

          <Col xs={24} sm={12} md={8}>
            <Link href="/account/orders">
              <Card
                hoverable
                style={{
                  borderRadius: 16,
                  height: "100%",
                  border: "1px solid #dcfce7",
                  background:
                    "linear-gradient(135deg, #f0fdf4 0%, #dcfce7 100%)",
                  transition: "all 0.3s ease",
                }}
                styles={{ body: { padding: "32px 24px" } }}
              >
                <div style={{ textAlign: "center", marginBottom: 16 }}>
                  <EnvironmentOutlined
                    style={{ fontSize: 48, color: "#15803d" }}
                  />
                </div>
                <Title
                  level={4}
                  style={{
                    textAlign: "center",
                    color: "#15803d",
                    marginBottom: 12,
                  }}
                >
                  Track Orders
                </Title>
                <Text
                  type="secondary"
                  style={{
                    display: "block",
                    textAlign: "center",
                    fontSize: 14,
                    color: "#166534",
                  }}
                >
                  View order status and location in real-time
                </Text>
              </Card>
            </Link>
          </Col>

          <Col xs={24} sm={12} md={8}>
            <Link href="/account/estimate">
              <Card
                hoverable
                style={{
                  borderRadius: 16,
                  height: "100%",
                  border: "1px solid #fef3c7",
                  background:
                    "linear-gradient(135deg, #fffbeb 0%, #fef3c7 100%)",
                  transition: "all 0.3s ease",
                }}
                styles={{ body: { padding: "32px 24px" } }}
              >
                <div style={{ textAlign: "center", marginBottom: 16 }}>
                  <DollarOutlined style={{ fontSize: 48, color: "#d97706" }} />
                </div>
                <Title
                  level={4}
                  style={{
                    textAlign: "center",
                    color: "#d97706",
                    marginBottom: 12,
                  }}
                >
                  Calculate Shipping Fee
                </Title>
                <Text
                  type="secondary"
                  style={{
                    display: "block",
                    textAlign: "center",
                    fontSize: 14,
                    color: "#92400e",
                  }}
                >
                  Estimate shipping costs before placing orders
                </Text>
              </Card>
            </Link>
          </Col>
        </Row>

        {/* Recent Orders */}
        <Card
          style={{
            borderRadius: 16,
            border: "1px solid #f0f0f0",
          }}
          title={
            <div
              style={{
                display: "flex",
                justifyContent: "space-between",
                flexWrap: "wrap",
              }}
            >
              <Title
                level={3}
                style={{
                  margin: 0,
                  fontSize: "clamp(18px, 3vw, 22px)",
                  color: "#15803d",
                }}
              >
                Recent Orders
              </Title>
              <Link href="/account/orders">
                <Button
                  type="link"
                  style={{ color: "#15803d", fontWeight: 500 }}
                >
                  View All
                </Button>
              </Link>
            </div>
          }
        >
          {recentOrders.length > 0 ? (
            <Table
              columns={recentOrdersColumns}
              dataSource={recentOrders}
              loading={loading}
              rowKey="id"
              pagination={false}
              size="small"
              scroll={{ x: "max-content" }}
            />
          ) : (
            <div style={{ textAlign: "center", padding: "32px 0" }}>
              <BoxPlotOutlined style={{ fontSize: 64, color: "#bfbfbf" }} />
              <Title level={4} type="secondary">
                No orders yet
              </Title>
              <Text
                type="secondary"
                style={{ display: "block", marginBottom: 24 }}
              >
                Create your first order to get started!
              </Text>
              <Link href="/account/orders/new">
                <Button
                  type="primary"
                  icon={<PlusOutlined />}
                  size="large"
                  style={{
                    backgroundColor: "#15803d",
                    borderColor: "#15803d",
                    borderRadius: 8,
                    height: 48,
                    fontSize: 16,
                    fontWeight: 600,
                    paddingLeft: 32,
                    paddingRight: 32,
                  }}
                >
                  Create Order Now
                </Button>
              </Link>
            </div>
          )}
        </Card>

        {/* Stats Overview */}
        <Row gutter={[24, 24]}>
          <Col xs={24} sm={12} md={8}>
            <Card
              style={{
                borderRadius: 16,
                border: "1px solid #f0f0f0",
              }}
            >
              <Statistic
                title="Total Orders"
                value={stats.totalOrders}
                prefix={<BarChartOutlined style={{ color: "#15803d" }} />}
                valueStyle={{ color: "#15803d", fontWeight: 600 }}
              />
            </Card>
          </Col>

          <Col xs={24} sm={12} md={8}>
            <Card
              style={{
                borderRadius: 16,
                border: "1px solid #f0f0f0",
              }}
            >
              <Statistic
                title="In Transit"
                value={stats.shippingOrders}
                prefix={<CarOutlined style={{ color: "#1890ff" }} />}
                valueStyle={{ color: "#1890ff", fontWeight: 600 }}
              />
            </Card>
          </Col>

          <Col xs={24} sm={12} md={8}>
            <Card
              style={{
                borderRadius: 16,
                border: "1px solid #f0f0f0",
              }}
            >
              <Statistic
                title="Completed"
                value={stats.completedOrders}
                prefix={<CheckCircleOutlined style={{ color: "#52c41a" }} />}
                valueStyle={{ color: "#52c41a", fontWeight: 600 }}
              />
            </Card>
          </Col>
        </Row>

        {/* Modal */}
        {isDetailModalVisible && detailOrderId && (
          <OrderDetailModal
            orderId={Number(detailOrderId)}
            onClose={() => setIsDetailModalVisible(false)}
          />
        )}

        {contextHolder}
      </div>
    </div>
  );
}
