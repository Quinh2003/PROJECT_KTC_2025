"use client";

import { useParams } from "next/navigation";
import { Card, Spin, Alert, Button, Timeline, Empty } from "antd";
import { ArrowLeftOutlined, CheckCircleOutlined } from "@ant-design/icons";
import { useOrderTracking } from "@/hooks/useOrders";
import dayjs from "dayjs";

export default function OrderTrackingPage() {
  const params = useParams();
  const orderId = params?.id as string;
  
  const { data: tracking, isLoading, error } = useOrderTracking(orderId);

  if (isLoading) return <Spin size="large" />;
  if (error) return <Alert message="Không thể tải thông tin theo dõi" type="error" />;

  return (
    <div>
      <Button
        icon={<ArrowLeftOutlined />}
        onClick={() => window.history.back()}
        style={{ marginBottom: 16 }}
      >
        Quay lại
      </Button>
      
      <Card title={`Theo dõi đơn hàng #${orderId}`}>
        {tracking && tracking.length > 0 ? (
          <Timeline>
            {tracking.map((item: any, index: number) => (
              <Timeline.Item
                key={index}
                color={index === 0 ? 'green' : 'blue'}
                dot={index === 0 ? <CheckCircleOutlined /> : undefined}
              >
                <div>
                  <strong>{item.status}</strong>
                  <br />
                  <span style={{ color: '#666' }}>{item.location}</span>
                  <br />
                  <small>{dayjs(item.timestamp).format('DD/MM/YYYY HH:mm')}</small>
                  <br />
                  {item.description}
                </div>
              </Timeline.Item>
            ))}
          </Timeline>
        ) : (
          <Empty description="Chưa có thông tin theo dõi" />
        )}
      </Card>
    </div>
  );
}