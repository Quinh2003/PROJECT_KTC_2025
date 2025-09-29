import React from "react";
import {
  Card,
  Row,
  Col,
  Divider,
  Typography,
  Table,
  Select,
  Form,
} from "antd";
import { Store } from "@/types/Store";
import { OrderItem } from "@/types/orders";
import { FormInstance } from "antd";
import {
  getServiceMultiplier,
  calculateBaseShippingFee,
} from "@/utils/shipping";
import {
  calculateDistanceFee,
  calculateTotalDistance,
} from "@/utils/distance";
import { getMapboxRoute } from "@/utils/mapbox";
import { isValidItem, calculateVolume } from "@/utils/orderItems";

const { Text, Title } = Typography;

interface Props {
  form: FormInstance<any>;
  store: Store | null;
}

export default function StepInvoice({ form, store }: Props) {
  const shippingAddress = form.getFieldValue("shipping_address");
  const receiverName = form.getFieldValue("receiver_name");
  const receiverPhone = form.getFieldValue("receiver_phone");
  const receiverEmail = form.getFieldValue("receiver_email");
  const description = form.getFieldValue("description");
  const notes = form.getFieldValue("notes");
  const items: OrderItem[] = form.getFieldValue("items") || [];

  const serviceType = Form.useWatch("service_type", form) ?? "STANDARD";

  const [distanceKm, setDistanceKm] = React.useState<number | null>(null);
  const [distanceFee, setDistanceFee] = React.useState<number | null>(null);
  const [distanceRegion, setDistanceRegion] = React.useState<string>("");

  React.useEffect(() => {
    const fetchRouteAndCalculate = async () => {
      setDistanceKm(null);
      setDistanceFee(null);
      setDistanceRegion("");

      if (!store?.longitude || !store?.latitude) return;

      const endLat = form.getFieldValue("latitude");
      const endLng = form.getFieldValue("longitude");
      if (!endLat || !endLng) return;

      try {
        const coordinates = await getMapboxRoute(
          store.longitude,
          store.latitude,
          endLng,
          endLat
        );

        if (coordinates.length >= 2) {
          const distance = calculateTotalDistance(coordinates);
          setDistanceKm(distance);

          const feeResult = calculateDistanceFee(distance);
          setDistanceFee(feeResult.fee);
          setDistanceRegion(feeResult.region);
        }
      } catch (error) {
        console.error("Lỗi khi tính toán route:", error);
      }
    };

    fetchRouteAndCalculate();
  }, [
    store?.longitude,
    store?.latitude,
    form.getFieldValue("latitude"),
    form.getFieldValue("longitude"),
  ]);

  const serviceFeeMultiplier = getServiceMultiplier(serviceType);

  let baseShippingFee = 0;
  items.forEach((item) => {
    if (isValidItem(item)) {
      const itemFragile = (item as any)?.is_fragile || false;
      const itemFee = calculateBaseShippingFee([item], itemFragile);
      baseShippingFee += itemFee;
    }
  });

  const totalFee = Math.round(
    baseShippingFee * serviceFeeMultiplier + (distanceFee || 0)
  );

  React.useEffect(() => {
    form.setFieldValue("delivery_fee", totalFee);
  }, [totalFee, form]);

  return (
    <Card>
      <Title level={4}>Chi tiết đơn hàng</Title>
      <Row gutter={[16, 24]}>
        {/* ===== Thông tin giao hàng ===== */}
        <Col xs={24}>
          <Card size="small" title="Thông tin giao hàng">
            <Row gutter={[16, 16]}>
              <Col xs={24} md={12}>
                <Title level={5} style={{ marginBottom: 12, color: "#1890ff" }}>
                  📍 Địa chỉ lấy hàng
                </Title>
                <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
                  <Text>
                    <Text strong>Tên cửa hàng: </Text>
                    {store?.storeName || "Đang tải..."}
                  </Text>
                  <Text>
                    <Text strong>Số điện thoại: </Text>
                    {store?.phone || "Đang tải..."}
                  </Text>
                  <Text>
                    <Text strong>Email: </Text>
                    {store?.email || "Đang tải..."}
                  </Text>
                  <Text>
                    <Text strong>Địa chỉ: </Text>
                    {store?.address || "Đang tải..."}
                  </Text>
                </div>
              </Col>

              <Col xs={24} md={12}>
                <Title level={5} style={{ marginBottom: 12, color: "#52c41a" }}>
                  🏠 Địa chỉ nhận hàng
                </Title>
                <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
                  <Text>
                    <Text strong>Tên người nhận: </Text>
                    {receiverName || "Chưa nhập"}
                  </Text>
                  <Text>
                    <Text strong>Số điện thoại: </Text>
                    {receiverPhone || "Chưa nhập"}
                  </Text>
                  <Text>
                    <Text strong>Email: </Text>
                    {receiverEmail || "Không có"}
                  </Text>
                  <Text>
                    <Text strong>Địa chỉ: </Text>
                    {shippingAddress || "Chưa nhập"}
                  </Text>
                </div>
              </Col>
            </Row>

            {(description || notes) && (
              <>
                <Divider style={{ margin: "16px 0" }} />
                <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
                  {description && (
                    <Text>
                      <Text strong>Mô tả đơn hàng: </Text>
                      {description}
                    </Text>
                  )}
                  {notes && (
                    <Text>
                      <Text strong>Ghi chú: </Text>
                      {notes}
                    </Text>
                  )}
                </div>
              </>
            )}
          </Card>
        </Col>

        {/* ===== Danh sách sản phẩm ===== */}
        <Col xs={24}>
          <Card size="small" title="Danh sách sản phẩm">
            {items.length === 0 ? (
              <div style={{ textAlign: "center", padding: 20, color: "#999" }}>
                <Text>Chưa có sản phẩm nào được thêm</Text>
              </div>
            ) : (
              <Table
                dataSource={items.map((item, index) => ({
                  ...item,
                  key: `item-${index}`,
                }))}
                pagination={false}
                size="small"
                scroll={{ x: 800 }}
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
                    title: "Cân nặng (kg)",
                    dataIndex: "weight",
                    key: "weight",
                    
                    render: (w: number) => `${w || 0} kg`,
                  },
                  {
                    title: "Thể tích (cm³)",
                    key: "volume",
                    responsive: ["lg"],
                    render: (_, r: OrderItem) => {
                      const volume = calculateVolume(r);
                      return volume > 0
                        ? volume.toLocaleString("vi-VN") + " cm³"
                        : "-";
                    },
                  },
                  {
                    title: "Hàng dễ vỡ",
                    key: "is_fragile",
                    responsive: ["md"],
                    render: (_, r: OrderItem) => {
                      const fragile = (r as any)?.is_fragile || false;
                      return (
                        <Text style={{ color: fragile ? "#ff4d4f" : "#52c41a" }}>
                          {fragile ? "Có" : "Không"}
                        </Text>
                      );
                    },
                  },
                  {
                    title: "Phí vận chuyển",
                    key: "shipping_fee",
                    render: (_, r: OrderItem) => {
                      const fragile = (r as any)?.is_fragile || false;
                      const fee = calculateBaseShippingFee([r], fragile);
                      return (
                        <Text strong style={{ color: "#1890ff" }}>
                          {fee.toLocaleString("vi-VN")} ₫
                        </Text>
                      );
                    },
                  },
                ]}
              />
            )}
          </Card>
        </Col>

        {/* ===== Chi phí ===== */}
        <Col xs={24}>
          <Card size="small" title="Chi phí">
            <Row gutter={[16, 16]}>
              <Col xs={24} sm={12}>
                <Form.Item
                  name="service_type"
                  label="Loại dịch vụ"
                  initialValue="STANDARD"
                  rules={[{ required: true, message: "Chọn loại dịch vụ" }]}
                >
                  <Select placeholder="Chọn loại dịch vụ" style={{ width: "100%" }}>
                    <Select.Option value="SECOND_CLASS">
                      Tiết kiệm (-20%)
                    </Select.Option>
                    <Select.Option value="STANDARD">Tiêu chuẩn</Select.Option>
                    <Select.Option value="FIRST_CLASS">
                      Cao cấp (+30%)
                    </Select.Option>
                    <Select.Option value="EXPRESS">Hỏa tốc (+80%)</Select.Option>
                  </Select>
                </Form.Item>
              </Col>

              <Col xs={24}>
                <div
                  style={{
                    background: "#f5f5f5",
                    padding: 16,
                    borderRadius: 6,
                  }}
                >
                  <Row gutter={[8, 8]}>
                    <Col xs={24} sm={12}>
                      <Text>Phí sản phẩm:</Text>
                    </Col>
                    <Col xs={24} sm={12} style={{ textAlign: "right" }}>
                      <Text>{baseShippingFee.toLocaleString("vi-VN")} ₫</Text>
                    </Col>

                    <Col xs={24} sm={12}>
                      <Text>Loại dịch vụ:</Text>
                    </Col>
                    <Col xs={24} sm={12} style={{ textAlign: "right" }}>
                      <Text>x {serviceFeeMultiplier}</Text>
                    </Col>

                    {distanceFee !== null && (
                      <Col xs={24} style={{ display: "flex", justifyContent: "space-between" }}>
                        <span>
                          Phí theo khoảng cách ({distanceRegion}
                          {distanceKm && (
                            <span style={{ color: "#888" }}>
                              {" "}
                              ~{distanceKm.toFixed(2)} km
                            </span>
                          )}
                          )
                        </span>
                        <span style={{ fontWeight: 500 }}>
                          {Math.round(distanceFee).toLocaleString("vi-VN")} ₫
                        </span>
                      </Col>
                    )}

                    <Col span={24}>
                      <Divider style={{ margin: "12px 0" }} />
                    </Col>

                    <Col xs={12}>
                      <Text strong style={{ fontSize: 16 }}>
                        Tổng phí vận chuyển:
                      </Text>
                    </Col>
                    <Col xs={12} style={{ textAlign: "right" }}>
                      <Text strong style={{ fontSize: 18, color: "#1890ff" }}>
                        {totalFee.toLocaleString("vi-VN")} ₫
                      </Text>
                    </Col>
                  </Row>
                </div>
              </Col>
            </Row>
          </Card>
        </Col>
      </Row>
    </Card>
  );
}
