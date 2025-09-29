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

      const response = await orderApi.getOrdersByUserPaginated(
        user.id,
        1,
        3
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
      }));

      setRecentOrders(formattedOrders);
    } catch (error) {
      messageApi.error("Không thể tải danh sách đơn hàng");
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
      messageApi.error("Không thể tải thống kê đơn hàng");
    }
  }, [messageApi]);

  useEffect(() => {
    fetchRecentOrders();
    fetchOrderStats();
  }, [fetchRecentOrders, fetchOrderStats]);

  const recentOrdersColumns = [
    {
      title: "Mã đơn hàng",
      dataIndex: "id",
      key: "id",
      render: (text: string) => (
        <a
          onClick={() => {
            setDetailOrderId(text);
            setIsDetailModalVisible(true);
          }}
          style={{ cursor: "pointer", color: "#1890ff" }}
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
      render: (amount: number) =>
        amount.toLocaleString("vi-VN", {
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
        </Space>
      ),
    },
  ];

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: "24px" }}>
      {/* Welcome Section */}
      <Card>
        <Title level={2} style={{ marginBottom: 16, fontSize: "clamp(20px, 4vw, 28px)" }}>
          Chào mừng đến với Fast Route! <CarOutlined />
        </Title>
        <Text style={{ fontSize: "clamp(14px, 3vw, 16px)" }}>
          Dịch vụ giao hàng thông minh với công nghệ tối ưu hóa tuyến đường.
          Chúng tôi cam kết mang đến trải nghiệm giao hàng nhanh chóng, an toàn
          và tiết kiệm chi phí.
        </Text>
      </Card>

      {/* Feature cards */}
      <Row gutter={[24, 24]}>
        <Col xs={24} sm={12} md={8}>
          <Link href="/account/orders/new">
            <Card hoverable>
              <div style={{ textAlign: "center", marginBottom: 16 }}>
                <BoxPlotOutlined style={{ fontSize: 32, color: "#1890ff" }} />
              </div>
              <Title level={4} style={{ textAlign: "center" }}>
                Tạo đơn hàng
              </Title>
              <Text type="secondary" style={{ display: "block", textAlign: "center" }}>
                Tạo đơn hàng giao hàng mới nhanh chóng
              </Text>
            </Card>
          </Link>
        </Col>

        <Col xs={24} sm={12} md={8}>
          <Link href="/account/orders">
            <Card hoverable>
              <div style={{ textAlign: "center", marginBottom: 16 }}>
                <EnvironmentOutlined style={{ fontSize: 32, color: "#1890ff" }} />
              </div>
              <Title level={4} style={{ textAlign: "center" }}>
                Theo dõi đơn hàng
              </Title>
              <Text type="secondary" style={{ display: "block", textAlign: "center" }}>
                Xem trạng thái và vị trí đơn hàng real-time
              </Text>
            </Card>
          </Link>
        </Col>

        <Col xs={24} sm={12} md={8}>
          <Link href="/account/estimate">
            <Card hoverable>
              <div style={{ textAlign: "center", marginBottom: 16 }}>
                <DollarOutlined style={{ fontSize: 32, color: "#1890ff" }} />
              </div>
              <Title level={4} style={{ textAlign: "center" }}>
                Tính phí giao hàng
              </Title>
              <Text type="secondary" style={{ display: "block", textAlign: "center" }}>
                Ước tính chi phí giao hàng trước khi đặt
              </Text>
            </Card>
          </Link>
        </Col>
      </Row>

      {/* Recent Orders */}
      <Card
        title={
          <div style={{ display: "flex", justifyContent: "space-between", flexWrap: "wrap" }}>
            <Title level={3} style={{ margin: 0, fontSize: "clamp(18px, 3vw, 22px)" }}>
              Đơn hàng gần đây
            </Title>
            <Link href="/account/orders">
              <Button type="link">Xem tất cả</Button>
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
              Chưa có đơn hàng nào
            </Title>
            <Text type="secondary" style={{ display: "block", marginBottom: 24 }}>
              Tạo đơn hàng đầu tiên để bắt đầu!
            </Text>
            <Link href="/account/orders/new">
              <Button type="primary" icon={<PlusOutlined />} size="large">
                Tạo đơn hàng ngay
              </Button>
            </Link>
          </div>
        )}
      </Card>

      {/* Stats Overview */}
      <Row gutter={[24, 24]}>
        <Col xs={24} sm={12} md={8}>
          <Card>
            <Statistic title="Tổng đơn hàng" value={stats.totalOrders} prefix={<BarChartOutlined />} />
          </Card>
        </Col>

        <Col xs={24} sm={12} md={8}>
          <Card>
            <Statistic
              title="Đang vận chuyển"
              value={stats.shippingOrders}
              prefix={<CarOutlined />}
              valueStyle={{ color: "#1890ff" }}
            />
          </Card>
        </Col>

        <Col xs={24} sm={12} md={8}>
          <Card>
            <Statistic
              title="Đã hoàn thành"
              value={stats.completedOrders}
              prefix={<CheckCircleOutlined />}
              valueStyle={{ color: "#52c41a" }}
            />
          </Card>
        </Col>
      </Row>

      {/* Modal */}
      {isDetailModalVisible && detailOrderId && (
        <OrderDetailModal orderId={Number(detailOrderId)} onClose={() => setIsDetailModalVisible(false)} />
      )}

      {contextHolder}
    </div>
  );
}
