import { Modal, Button, Typography, Divider, Space } from "antd";
import { DownloadOutlined } from "@ant-design/icons";
import dayjs from "dayjs";

const { Title, Text } = Typography;

interface InvoiceModalProps {
  open: boolean;
  onClose: () => void;
  orderData: {
    orderId: string;
    createdAt: string;
    items: Array<{
      name: string;
      quantity: number;
      price: number;
    }>;
    shippingFee: number;
    total: number;
  };
}

export default function InvoiceModal({
  open,
  onClose,
  orderData,
}: InvoiceModalProps) {
  const handleDownload = () => {
    // Implement PDF download logic here
    console.log("Downloading invoice...");
  };

  return (
    <Modal
      open={open}
      onCancel={onClose}
      title={<Title level={4}>Hoá đơn #{orderData.orderId}</Title>}
      footer={[
        <Button
          key="download"
          type="primary"
          icon={<DownloadOutlined />}
          onClick={handleDownload}
        >
          Tải hoá đơn
        </Button>,
        <Button key="close" onClick={onClose}>
          Đóng
        </Button>,
      ]}
    >
      <Text type="secondary">
        Ngày tạo: {dayjs(orderData.createdAt).format("DD/MM/YYYY")}
      </Text>

      <Divider />

      <Title level={5}>Chi tiết sản phẩm</Title>
      <Space direction="vertical" style={{ width: "100%" }}>
        {orderData.items.map((item, index) => (
          <div
            key={index}
            style={{ display: "flex", justifyContent: "space-between" }}
          >
            <Text>
              {item.name} x{item.quantity}
            </Text>
            <Text>{item.price.toLocaleString()}đ</Text>
          </div>
        ))}
      </Space>

      <Divider />

      <div style={{ display: "flex", justifyContent: "space-between" }}>
        <Text>Phí vận chuyển:</Text>
        <Text>{orderData.shippingFee.toLocaleString()}đ</Text>
      </div>
      <div
        style={{
          display: "flex",
          justifyContent: "space-between",
          marginTop: 8,
        }}
      >
        <Text strong>Tổng cộng:</Text>
        <Text strong>{orderData.total.toLocaleString()}đ</Text>
      </div>
    </Modal>
  );
}
