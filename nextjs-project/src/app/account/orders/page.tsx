"use client";

import { useEffect, useState } from "react";
import {
  Table,
  Card,
  Tag,
  Button,
  Space,
  Typography,
  Input,
  DatePicker,
  Select,
  Row,
  Col,
  Tooltip,
  Badge,
  Modal,
  Timeline,
} from "antd";
import {
  SearchOutlined,
  PlusOutlined,
  EyeOutlined,
  HistoryOutlined,
} from "@ant-design/icons";
import { useRouter } from "next/navigation";

const { Title } = Typography;
const { RangePicker } = DatePicker;

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

const orderStatuses = [
  { id: 1, name: "Chờ xử lý", color: "default" },
  { id: 2, name: "Đã tiếp nhận", color: "processing" },
  { id: 3, name: "Đang giao hàng", color: "warning" },
  { id: 4, name: "Đã giao hàng", color: "success" },
  { id: 5, name: "Đã huỷ", color: "error" },
];

export default function OrdersPage() {
  const router = useRouter();
  const [orders, setOrders] = useState<Order[]>([]);
  const [loading, setLoading] = useState(true);
  const [searchText, setSearchText] = useState("");
  const [dateRange, setDateRange] = useState<[any, any]>([null, null]);
  const [statusFilter, setStatusFilter] = useState<number[]>([]);
  const [isTrackingModalVisible, setIsTrackingModalVisible] = useState(false);
  const [selectedOrder, setSelectedOrder] = useState<Order | null>(null);

  useEffect(() => {
    // TODO: Fetch orders from API
    const fetchOrders = async () => {
      try {
        // Temporary mock data
        const mockOrders: Order[] = Array(10)
          .fill(null)
          .map((_, index) => ({
            id: `ORD${String(index + 1).padStart(5, "0")}`,
            created_at: new Date(
              Date.now() - Math.random() * 30 * 24 * 60 * 60 * 1000
            ).toISOString(),
            store_name: "Cửa hàng A",
            shipping_address: "123 Đường ABC, Quận XYZ, TP.HCM",
            total_items: Math.floor(Math.random() * 5) + 1,
            cod_amount: Math.floor(Math.random() * 1000000),
            shipping_fee: Math.floor(Math.random() * 200000),
            status: orderStatuses[Math.floor(Math.random() * 5)],
            tracking_updates: [
              {
                time: new Date().toISOString(),
                status: "Đã tiếp nhận",
                description: "Đơn hàng đã được tiếp nhận",
              },
            ],
          }));
        setOrders(mockOrders);
        setLoading(false);
      } catch (error) {
        console.error("Failed to fetch orders:", error);
        setLoading(false);
      }
    };

    fetchOrders();
  }, []);

  const showTrackingModal = (order: Order) => {
    setSelectedOrder(order);
    setIsTrackingModalVisible(true);
  };

  const columns = [
    {
      title: "Mã đơn hàng",
      dataIndex: "id",
      key: "id",
      render: (text: string) => <a>{text}</a>,
    },
    {
      title: "Ngày tạo",
      dataIndex: "created_at",
      key: "created_at",
      render: (date: string) => new Date(date).toLocaleDateString("vi-VN"),
    },
    {
      title: "Cửa hàng",
      dataIndex: "store_name",
      key: "store_name",
    },
    {
      title: "Địa chỉ giao hàng",
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
      title: "COD",
      dataIndex: "cod_amount",
      key: "cod_amount",
      align: "right" as const,
      render: (amount: number) =>
        amount.toLocaleString("vi-VN", {
          style: "currency",
          currency: "VND",
        }),
    },
    {
      title: "Phí ship",
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
      render: (_: any, record: Order) => (
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
        </Space>
      ),
    },
  ];

  return (
    <Card>
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

      {/* Filters */}
      <Card size="small" style={{ marginBottom: 24 }}>
        <Row gutter={[16, 16]}>
          <Col xs={24} sm={8} md={6}>
            <Input
              placeholder="Tìm mã đơn hàng..."
              prefix={<SearchOutlined />}
              value={searchText}
              onChange={(e) => setSearchText(e.target.value)}
            />
          </Col>
          <Col xs={24} sm={8} md={6}>
            <RangePicker
              style={{ width: "100%" }}
              value={dateRange}
              onChange={(dates) => setDateRange(dates)}
              placeholder={["Từ ngày", "Đến ngày"]}
            />
          </Col>
          <Col xs={24} sm={8} md={6}>
            <Select
              mode="multiple"
              style={{ width: "100%" }}
              placeholder="Lọc theo trạng thái"
              value={statusFilter}
              onChange={setStatusFilter}
              options={orderStatuses.map((status) => ({
                label: status.name,
                value: status.id,
              }))}
            />
          </Col>
        </Row>
      </Card>

      <Table
        columns={columns}
        dataSource={orders}
        loading={loading}
        rowKey="id"
        pagination={{
          total: orders.length,
          pageSize: 10,
          showTotal: (total) => `${total} đơn hàng`,
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
