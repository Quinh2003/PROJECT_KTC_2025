"use client";

import Link from "next/link";
import { Typography, Card, Row, Col, Button, Statistic } from "antd";
import {
  PlusOutlined,
  BoxPlotOutlined,
  EnvironmentOutlined,
  DollarOutlined,
  BarChartOutlined,
  CarOutlined,
  CheckCircleOutlined,
} from "@ant-design/icons";

const { Title, Text } = Typography;

export default function CustomerAccount() {
  return (
    <div style={{ display: "flex", flexDirection: "column", gap: "24px" }}>
      {/* Welcome Section */}
      <Card>
        <Title level={2} style={{ marginBottom: 16 }}>
          Chào mừng đến với Fast Route! <CarOutlined />
        </Title>
        <Text style={{ fontSize: 16 }}>
          Dịch vụ giao hàng thông minh với công nghệ tối ưu hóa tuyến đường.
          Chúng tôi cam kết mang đến trải nghiệm giao hàng nhanh chóng, an toàn
          và tiết kiệm chi phí.
        </Text>
      </Card>

      {/* Quick Actions */}
      <Row gutter={[24, 24]}>
        <Col xs={24} md={8}>
          <Card
            hoverable
            onClick={() => (window.location.href = "/account/orders/new")}
          >
            <div style={{ textAlign: "center", marginBottom: 16 }}>
              <BoxPlotOutlined style={{ fontSize: 32, color: "#1890ff" }} />
            </div>
            <Title level={4} style={{ marginBottom: 8, textAlign: "center" }}>
              Tạo đơn hàng
            </Title>
            <Text
              type="secondary"
              style={{ textAlign: "center", display: "block" }}
            >
              Tạo đơn hàng giao hàng mới một cách nhanh chóng
            </Text>
          </Card>
        </Col>

        <Col xs={24} md={8}>
          <Card
            hoverable
            onClick={() => (window.location.href = "/account/orders")}
          >
            <div style={{ textAlign: "center", marginBottom: 16 }}>
              <EnvironmentOutlined style={{ fontSize: 32, color: "#1890ff" }} />
            </div>
            <Title level={4} style={{ marginBottom: 8, textAlign: "center" }}>
              Theo dõi đơn hàng
            </Title>
            <Text
              type="secondary"
              style={{ textAlign: "center", display: "block" }}
            >
              Xem trạng thái và vị trí đơn hàng real-time
            </Text>
          </Card>
        </Col>

        <Col xs={24} md={8}>
          <Card
            hoverable
            onClick={() => (window.location.href = "/account/estimate")}
          >
            <div style={{ textAlign: "center", marginBottom: 16 }}>
              <DollarOutlined style={{ fontSize: 32, color: "#1890ff" }} />
            </div>
            <Title level={4} style={{ marginBottom: 8, textAlign: "center" }}>
              Tính phí giao hàng
            </Title>
            <Text
              type="secondary"
              style={{ textAlign: "center", display: "block" }}
            >
              Ước tính chi phí giao hàng trước khi đặt
            </Text>
          </Card>
        </Col>
      </Row>

      {/* Recent Orders */}
      <Card
        title={
          <div
            style={{
              display: "flex",
              justifyContent: "space-between",
              alignItems: "center",
            }}
          >
            <Title level={3} style={{ margin: 0 }}>
              Đơn hàng gần đây
            </Title>
            <Link href="/account/orders">
              <Button type="link">Xem tất cả</Button>
            </Link>
          </div>
        }
      >
        <div style={{ textAlign: "center", padding: "32px 0" }}>
          <BoxPlotOutlined
            style={{ fontSize: 64, color: "#bfbfbf", marginBottom: 16 }}
          />
          <Title level={4} type="secondary">
            Chưa có đơn hàng nào
          </Title>
          <Text type="secondary" style={{ display: "block", marginBottom: 24 }}>
            Tạo đơn hàng đầu tiên để bắt đầu!
          </Text>
          <Button
            type="primary"
            icon={<PlusOutlined />}
            size="large"
            href="/account/orders/new"
          >
            Tạo đơn hàng ngay
          </Button>
        </div>
      </Card>

      {/* Stats Overview */}
      <Row gutter={[24, 24]}>
        <Col xs={24} md={8}>
          <Card>
            <Statistic
              title="Tổng đơn hàng"
              value={0}
              prefix={<BarChartOutlined />}
            />
          </Card>
        </Col>

        <Col xs={24} md={8}>
          <Card>
            <Statistic
              title="Đang vận chuyển"
              value={0}
              prefix={<CarOutlined />}
              valueStyle={{ color: "#1890ff" }}
            />
          </Card>
        </Col>

        <Col xs={24} md={8}>
          <Card>
            <Statistic
              title="Đã hoàn thành"
              value={0}
              prefix={<CheckCircleOutlined />}
              valueStyle={{ color: "#52c41a" }}
            />
          </Card>
        </Col>
      </Row>
    </div>
  );
}
