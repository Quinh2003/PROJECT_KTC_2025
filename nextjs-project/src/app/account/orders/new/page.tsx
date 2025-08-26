"use client";

import { useState } from "react";
import {
  Form,
  Input,
  Card,
  Button,
  Steps,
  Row,
  Col,
  InputNumber,
  Typography,
  message,
  Table,
  Divider,
} from "antd";
import {
  ShopOutlined,
  BoxPlotOutlined,
  DollarOutlined,
  PlusOutlined,
  DeleteOutlined,
} from "@ant-design/icons";

interface OrderItem {
  product_name: string;
  quantity: number;
  weight: number;
  height: number;
  width: number;
}

interface OrderForm {
  store_id: number;
  description?: string;
  notes?: string;
  shipping_address: string;
  cod_amount: number;
  items: OrderItem[];
}

const { Title, Text } = Typography;
const { TextArea } = Input;

// Hàm tính phí ship dựa trên kích thước và cân nặng
const calculateShippingFee = (items: OrderItem[]): number => {
  if (!items || items.length === 0) return 0;

  return items.reduce((total, item) => {
    // Phí cơ bản cho mỗi sản phẩm
    let fee = 15000;

    // Phí theo cân nặng (10000đ/kg)
    fee += item.weight * 10000;

    // Phí theo kích thước (1000đ/cm cho tổng chiều cao và chiều rộng)
    fee += (item.height + item.width) * 1000;

    // Nhân với số lượng
    fee *= item.quantity;

    return total + fee;
  }, 0);
};

export default function CreateOrder() {
  const [currentStep, setCurrentStep] = useState(0);
  const [form] = Form.useForm();

  const handleSubmit = async (values: OrderForm) => {
    try {
      message.loading({ content: "Đang tạo đơn hàng...", key: "createOrder" });

      const orderData = {
        ...values,
        status_id: 1, // Initial status
        created_at: new Date().toISOString(),
        updated_at: new Date().toISOString(),
      };

      // TODO: Implement API call here
      console.log(orderData);
      message.success({
        content: "Tạo đơn hàng thành công!",
        key: "createOrder",
      });
    } catch (err: any) {
      message.error({
        content: err?.message || "Có lỗi xảy ra khi tạo đơn hàng!",
        key: "createOrder",
      });
    }
  };

  const steps = [
    {
      title: "Thông tin cửa hàng",
      icon: <ShopOutlined />,
      content: (
        <Row gutter={[16, 16]}>
          <Col xs={24} md={12}>
            <Form.Item name="store_id" label="Cửa hàng">
              <Input disabled />
            </Form.Item>
          </Col>
          <Col xs={24} md={12}>
            <Form.Item
              name="shipping_address"
              label="Địa chỉ giao hàng"
              rules={[
                { required: true, message: "Vui lòng nhập địa chỉ giao hàng!" },
              ]}
            >
              <TextArea rows={2} placeholder="Nhập địa chỉ giao hàng" />
            </Form.Item>
          </Col>
          <Col xs={24}>
            <Form.Item name="description" label="Mô tả đơn hàng">
              <TextArea
                rows={3}
                placeholder="Nhập mô tả chi tiết về đơn hàng (không bắt buộc)"
              />
            </Form.Item>
          </Col>
          <Col xs={24}>
            <Form.Item name="notes" label="Ghi chú">
              <TextArea rows={3} placeholder="Nhập ghi chú bổ sung (nếu có)" />
            </Form.Item>
          </Col>
        </Row>
      ),
    },
    {
      title: "Chi tiết đơn hàng",
      icon: <BoxPlotOutlined />,
      content: (
        <Form.List name="items">
          {(fields, { add, remove }) => (
            <div
              style={{ display: "flex", flexDirection: "column", gap: "16px" }}
            >
              <Row gutter={[16, 16]}>
                <Col span={24}>
                  <Table
                    dataSource={fields.map((field) => ({
                      ...field,
                      key: field.key,
                    }))}
                    pagination={false}
                    columns={[
                      {
                        title: "Tên sản phẩm",
                        key: "product_name",
                        render: (_, __, index) => (
                          <Form.Item
                            name={[index, "product_name"]}
                            rules={[
                              {
                                required: true,
                                message: "Vui lòng nhập tên sản phẩm!",
                              },
                            ]}
                            style={{ margin: 0 }}
                          >
                            <Input placeholder="Nhập tên sản phẩm" />
                          </Form.Item>
                        ),
                      },
                      {
                        title: "Số lượng",
                        key: "quantity",
                        render: (_, __, index) => (
                          <Form.Item
                            name={[index, "quantity"]}
                            rules={[
                              {
                                required: true,
                                message: "Vui lòng nhập số lượng!",
                              },
                            ]}
                            style={{ margin: 0 }}
                          >
                            <InputNumber
                              min={1}
                              style={{ width: "100%" }}
                              placeholder="Số lượng"
                            />
                          </Form.Item>
                        ),
                      },
                      {
                        title: "Cân nặng (kg)",
                        key: "weight",
                        render: (_, __, index) => (
                          <Form.Item
                            name={[index, "weight"]}
                            rules={[
                              {
                                required: true,
                                message: "Vui lòng nhập cân nặng!",
                              },
                            ]}
                            style={{ margin: 0 }}
                          >
                            <InputNumber
                              min={0}
                              step={0.1}
                              style={{ width: "100%" }}
                              placeholder="Cân nặng"
                            />
                          </Form.Item>
                        ),
                      },
                      {
                        title: "Chiều cao (cm)",
                        key: "height",
                        render: (_, __, index) => (
                          <Form.Item
                            name={[index, "height"]}
                            rules={[
                              {
                                required: true,
                                message: "Vui lòng nhập chiều cao!",
                              },
                            ]}
                            style={{ margin: 0 }}
                          >
                            <InputNumber
                              min={0}
                              style={{ width: "100%" }}
                              placeholder="Chiều cao"
                            />
                          </Form.Item>
                        ),
                      },
                      {
                        title: "Chiều rộng (cm)",
                        key: "width",
                        render: (_, __, index) => (
                          <Form.Item
                            name={[index, "width"]}
                            rules={[
                              {
                                required: true,
                                message: "Vui lòng nhập chiều rộng!",
                              },
                            ]}
                            style={{ margin: 0 }}
                          >
                            <InputNumber
                              min={0}
                              style={{ width: "100%" }}
                              placeholder="Chiều rộng"
                            />
                          </Form.Item>
                        ),
                      },
                      {
                        title: "",
                        key: "action",
                        render: (_, __, index) => (
                          <Button
                            type="text"
                            danger
                            icon={<DeleteOutlined />}
                            onClick={() => remove(index)}
                          />
                        ),
                      },
                    ]}
                  />
                </Col>
              </Row>
              <Button
                type="dashed"
                onClick={() => add()}
                icon={<PlusOutlined />}
                block
              >
                Thêm sản phẩm
              </Button>
            </div>
          )}
        </Form.List>
      ),
    },
    {
      title: "Hoá đơn",
      icon: <DollarOutlined />,
      content: (
        <Card>
          <Title level={4}>Chi tiết đơn hàng</Title>

          {/* Thông tin cửa hàng và địa chỉ */}
          <Row gutter={[16, 24]}>
            <Col xs={24} md={12}>
              <Card size="small" title="Thông tin giao hàng">
                <Form.Item noStyle shouldUpdate>
                  {({ getFieldsValue }) => {
                    const { store_id, shipping_address } = getFieldsValue();
                    return (
                      <>
                        <Text strong>Từ: </Text>
                        <Text>{store_id || "---"}</Text>
                        <br />
                        <Text strong>Đến: </Text>
                        <Text>{shipping_address || "---"}</Text>
                      </>
                    );
                  }}
                </Form.Item>
              </Card>
            </Col>

            <Col xs={24}>
              <Card size="small" title="Danh sách sản phẩm">
                <Form.Item noStyle shouldUpdate>
                  {({ getFieldsValue }) => {
                    const { items } = getFieldsValue();
                    return (
                      <Table
                        dataSource={items || []}
                        pagination={false}
                        columns={[
                          {
                            title: "Tên sản phẩm",
                            dataIndex: "product_name",
                            key: "product_name",
                          },
                          {
                            title: "Số lượng",
                            dataIndex: "quantity",
                            key: "quantity",
                          },
                          {
                            title: "Kích thước",
                            key: "dimensions",
                            render: (_, record: OrderItem) => (
                              <span>
                                {record.height}x{record.width}cm,{" "}
                                {record.weight}kg
                              </span>
                            ),
                          },
                        ]}
                      />
                    );
                  }}
                </Form.Item>
              </Card>
            </Col>

            <Col xs={24}>
              <Card size="small" title="Chi phí">
                <Form.Item noStyle shouldUpdate>
                  {({ getFieldsValue }) => {
                    const { items } = getFieldsValue();
                    // Tính phí ship dựa trên kích thước và cân nặng
                    const shippingFee = calculateShippingFee(items || []);

                    return (
                      <Row gutter={[16, 16]}>
                        <Col span={12}>
                          <Text>Phí vận chuyển:</Text>
                        </Col>
                        <Col span={12} style={{ textAlign: "right" }}>
                          <Text strong>
                            {shippingFee.toLocaleString("vi-VN")} ₫
                          </Text>
                        </Col>

                        <Col span={24}>
                          <Form.Item
                            name="cod_amount"
                            label="Tiền thu hộ (COD)"
                            initialValue={0}
                          >
                            <InputNumber
                              style={{ width: "100%" }}
                              min={0}
                              step={1000}
                              formatter={(value) =>
                                `${value}`.replace(/\B(?=(\d{3})+(?!\d))/g, ",")
                              }
                              parser={(value) => value!.replace(/\D/g, "")}
                              placeholder="Nhập số tiền thu hộ"
                            />
                          </Form.Item>
                        </Col>

                        <Col span={24}>
                          <Divider style={{ margin: "12px 0" }} />
                        </Col>

                        <Col span={12}>
                          <Text strong>Tổng phí vận chuyển:</Text>
                        </Col>
                        <Col span={12} style={{ textAlign: "right" }}>
                          <Text
                            strong
                            style={{ fontSize: "18px", color: "#1890ff" }}
                          >
                            {shippingFee.toLocaleString("vi-VN")} ₫
                          </Text>
                        </Col>
                      </Row>
                    );
                  }}
                </Form.Item>
              </Card>
            </Col>
          </Row>
        </Card>
      ),
    },
  ];

  const next = async () => {
    try {
      // Validate current form fields
      await form.validateFields();
      setCurrentStep(currentStep + 1);
    } catch (error) {
      // Form validation failed
      console.error("Validation failed:", error);
    }
  };

  const prev = () => {
    setCurrentStep(currentStep - 1);
  };

  return (
    <Card>
      <Title level={2}>Tạo đơn hàng mới</Title>
      <Steps
        current={currentStep}
        items={steps.map((item) => ({
          title: item.title,
          icon: item.icon,
        }))}
        style={{ marginBottom: 40 }}
      />
      <Form
        form={form}
        layout="vertical"
        onFinish={handleSubmit}
        requiredMark={false}
      >
        {steps[currentStep].content}

        <div style={{ marginTop: 24, textAlign: "right" }}>
          {currentStep > 0 && (
            <Button style={{ margin: "0 8px" }} onClick={prev}>
              Quay lại
            </Button>
          )}
          {currentStep < steps.length - 1 && (
            <Button type="primary" onClick={next}>
              Tiếp tục
            </Button>
          )}
          {currentStep === steps.length - 1 && (
            <Button type="primary" onClick={() => form.submit()}>
              Tạo đơn hàng
            </Button>
          )}
        </div>
      </Form>
    </Card>
  );
}
