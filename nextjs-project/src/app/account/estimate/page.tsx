"use client";

import { useState, useEffect } from "react";
import {
  Card,
  Form,
  Input,
  InputNumber,
  Button,
  Typography,
  Row,
  Col,
  Space,
  Divider,
  Alert,
} from "antd";
import { CalculatorOutlined } from "@ant-design/icons";

const { Title, Text } = Typography;
const { TextArea } = Input;

interface EstimateForm {
  pickupAddress: string;
  deliveryAddress: string;
  weight: number;
  length: number;
  width: number;
  height: number;
  distance: number;
}

const calculateShippingFee = (values: EstimateForm): number => {
  // Công thức tính phí giao hàng (ví dụ):
  // 1. Phí cơ bản: 15,000đ
  let baseFee = 15000;

  // 2. Phí theo khối lượng: 10,000đ/kg
  const weightFee = values.weight * 10000;

  // 3. Phí theo thể tích (dài x rộng x cao): 500đ/cm³
  const volumeFee = values.length * values.width * values.height * 500;

  // 4. Phí theo khoảng cách: 5,000đ/km
  const distanceFee = values.distance * 5000;

  // Tổng phí = Phí cơ bản + Phí khối lượng + Phí thể tích + Phí khoảng cách
  const totalFee = baseFee + weightFee + volumeFee + distanceFee;

  return totalFee;
};

export default function EstimatePage() {
  const [form] = Form.useForm<EstimateForm>();
  const [fee, setFee] = useState<number>(0);
  const [pickupAddress, setPickupAddress] = useState<string>("");

  useEffect(() => {
    // TODO: Fetch user's address from account
    const userAddress = "123 Đường ABC, Quận 1, TP.HCM"; // This should come from user's data
    setPickupAddress(userAddress);
    form.setFieldsValue({ pickupAddress: userAddress });
  }, [form]);

  const handleCalculate = (values: EstimateForm) => {
    const estimatedFee = calculateShippingFee(values);
    setFee(estimatedFee);
  };

  return (
    <Card>
      <Title level={2}>Ước tính phí vận chuyển</Title>
      <Form
        form={form}
        layout="vertical"
        onFinish={handleCalculate}
        initialValues={{
          weight: 0,
          length: 0,
          width: 0,
          height: 0,
          distance: 0,
        }}
      >
        <Row gutter={[24, 0]}>
          <Col xs={24} md={12}>
            <Form.Item
              name="pickupAddress"
              label="Địa chỉ lấy hàng"
              rules={[
                { required: true, message: "Vui lòng nhập địa chỉ lấy hàng!" },
              ]}
            >
              <TextArea rows={3} disabled placeholder="Địa chỉ lấy hàng" />
            </Form.Item>
          </Col>

          <Col xs={24} md={12}>
            <Form.Item
              name="deliveryAddress"
              label="Địa chỉ giao hàng"
              rules={[
                { required: true, message: "Vui lòng nhập địa chỉ giao hàng!" },
              ]}
            >
              <TextArea rows={3} placeholder="Nhập địa chỉ giao hàng" />
            </Form.Item>
          </Col>
        </Row>

        <Row gutter={[24, 0]}>
          <Col xs={24} md={8}>
            <Form.Item
              name="weight"
              label="Khối lượng (kg)"
              rules={[{ required: true, message: "Vui lòng nhập khối lượng!" }]}
            >
              <InputNumber
                min={0}
                step={0.1}
                style={{ width: "100%" }}
                placeholder="Nhập khối lượng"
              />
            </Form.Item>
          </Col>

          <Col xs={24} md={16}>
            <Row gutter={[16, 0]}>
              <Col span={8}>
                <Form.Item
                  name="length"
                  label="Chiều dài (cm)"
                  rules={[
                    { required: true, message: "Vui lòng nhập chiều dài!" },
                  ]}
                >
                  <InputNumber
                    min={0}
                    style={{ width: "100%" }}
                    placeholder="Chiều dài"
                  />
                </Form.Item>
              </Col>
              <Col span={8}>
                <Form.Item
                  name="width"
                  label="Chiều rộng (cm)"
                  rules={[
                    { required: true, message: "Vui lòng nhập chiều rộng!" },
                  ]}
                >
                  <InputNumber
                    min={0}
                    style={{ width: "100%" }}
                    placeholder="Chiều rộng"
                  />
                </Form.Item>
              </Col>
              <Col span={8}>
                <Form.Item
                  name="height"
                  label="Chiều cao (cm)"
                  rules={[
                    { required: true, message: "Vui lòng nhập chiều cao!" },
                  ]}
                >
                  <InputNumber
                    min={0}
                    style={{ width: "100%" }}
                    placeholder="Chiều cao"
                  />
                </Form.Item>
              </Col>
            </Row>
          </Col>
        </Row>

        <Row gutter={[24, 0]}>
          <Col xs={24} md={8}>
            <Form.Item
              name="distance"
              label="Khoảng cách (km)"
              rules={[
                { required: true, message: "Vui lòng nhập khoảng cách!" },
              ]}
            >
              <InputNumber
                min={0}
                step={0.1}
                style={{ width: "100%" }}
                placeholder="Nhập khoảng cách"
              />
            </Form.Item>
          </Col>
        </Row>

        <Divider />

        <Row>
          <Col span={24} style={{ textAlign: "center" }}>
            <Space direction="vertical" size="large">
              <Button
                type="primary"
                htmlType="submit"
                icon={<CalculatorOutlined />}
                size="large"
              >
                Tính phí vận chuyển
              </Button>

              {fee > 0 && (
                <Alert
                  message="Ước tính phí vận chuyển"
                  description={
                    <Space direction="vertical">
                      <Text>Chi tiết phí:</Text>
                      <Text>• Phí cơ bản: 15,000đ</Text>
                      <Text>• Phí khối lượng: 10,000đ/kg</Text>
                      <Text>• Phí thể tích: 500đ/cm³</Text>
                      <Text>• Phí khoảng cách: 5,000đ/km</Text>
                      <Divider style={{ margin: "12px 0" }} />
                      <Text strong style={{ fontSize: "18px" }}>
                        Tổng phí: {fee.toLocaleString("vi-VN")}đ
                      </Text>
                    </Space>
                  }
                  type="info"
                  showIcon
                />
              )}
            </Space>
          </Col>
        </Row>
      </Form>
    </Card>
  );
}
