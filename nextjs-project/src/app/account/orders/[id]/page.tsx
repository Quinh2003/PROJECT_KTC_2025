"use client";

import { useParams } from "next/navigation";
import { Card, Spin, Alert, Button, Descriptions, Tag } from "antd";
import { ArrowLeftOutlined } from "@ant-design/icons";
import { useOrder } from "@/hooks/useOrders";

export default function OrderDetailPage() {
  const params = useParams();
  const orderId = params?.id as string;
  
  const { data: order, isLoading, error } = useOrder(orderId);

  if (isLoading) return <Spin size="large" />;
  if (error) return <Alert message="Đơn hàng không tồn tại" type="error" />;
  if (!order) return <Alert message="Không tìm thấy đơn hàng" type="warning" />;

  return (
    <div>
      <Button
        icon={<ArrowLeftOutlined />}
        onClick={() => window.history.back()}
        style={{ marginBottom: 16 }}
      >
        Quay lại
      </Button>
      
      <Card title={`Chi tiết đơn hàng #${order.id}`}>
        <Descriptions column={2}>
          <Descriptions.Item label="Mã đơn hàng">#{order.id}</Descriptions.Item>
          <Descriptions.Item label="Trạng thái">
            <Tag color="blue">{order.status}</Tag>
          </Descriptions.Item>
          <Descriptions.Item label="Địa chỉ giao">
            {order.address}
          </Descriptions.Item>
          <Descriptions.Item label="Tổng tiền">
            {order.total?.toLocaleString()}đ
          </Descriptions.Item>
        </Descriptions>
      </Card>
    </div>
  );
}