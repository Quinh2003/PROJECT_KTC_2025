"use client";

import React from "react";
import { Card, Row, Col, Divider, Typography, Table, Select, Form } from "antd";
import { Store } from "@/types/Store";
import { OrderItem, OrderForm } from "@/types/orders";
import { FormInstance } from "antd";
import {
  getServiceMultiplier,
  calculateBaseShippingFee,
} from "@/utils/shipping";
import { calculateDistanceFee, calculateTotalDistance } from "@/utils/distance";
import { getMapboxRoute } from "@/utils/mapbox";
import { isValidItem, calculateVolume } from "@/utils/orderItems";

const { Text, Title } = Typography;

interface Props {
  form: FormInstance<OrderForm>;
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
        console.error("Error calculating route:", error);
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
      const itemFragile = item.is_fragile || false;
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
    <>
      <Title level={4}>Order Details</Title>
      <Row gutter={[16, 24]}>
        {/* ===== Shipping Information ===== */}
        <Col xs={24}>
          <Card size="small" title="Shipping Information">
            <Row gutter={[16, 16]}>
              <Col xs={24} md={12}>
                <Title level={5} style={{ marginBottom: 12, color: "#d97706" }}>
                  📍 Pickup Address
                </Title>
                <div
                  style={{ display: "flex", flexDirection: "column", gap: 8 }}
                >
                  <Text>
                    <Text strong>Store Name: </Text>
                    {store?.storeName || "Loading..."}
                  </Text>
                  <Text>
                    <Text strong>Phone: </Text>
                    {store?.phone || "Loading..."}
                  </Text>
                  <Text>
                    <Text strong>Email: </Text>
                    {store?.email || "Loading..."}
                  </Text>
                  <Text>
                    <Text strong>Address: </Text>
                    {store?.address || "Loading..."}
                  </Text>
                </div>
              </Col>

              <Col xs={24} md={12}>
                <Title level={5} style={{ marginBottom: 12, color: "#d97706" }}>
                  🏠 Delivery Address
                </Title>
                <div
                  style={{ display: "flex", flexDirection: "column", gap: 8 }}
                >
                  <Text>
                    <Text strong>Recipient Name: </Text>
                    {receiverName || "Not provided"}
                  </Text>
                  <Text>
                    <Text strong>Phone: </Text>
                    {receiverPhone || "Not provided"}
                  </Text>
                  <Text>
                    <Text strong>Email: </Text>
                    {receiverEmail || "None"}
                  </Text>
                  <Text>
                    <Text strong>Address: </Text>
                    {shippingAddress || "Not provided"}
                  </Text>
                </div>
              </Col>
            </Row>

            {(description || notes) && (
              <>
                <Divider style={{ margin: "16px 0" }} />
                <div
                  style={{ display: "flex", flexDirection: "column", gap: 8 }}
                >
                  {description && (
                    <Text>
                      <Text strong>Order Description: </Text>
                      {description}
                    </Text>
                  )}
                  {notes && (
                    <Text>
                      <Text strong>Notes: </Text>
                      {notes}
                    </Text>
                  )}
                </div>
              </>
            )}
          </Card>
        </Col>

        {/* ===== Product List ===== */}
        <Col xs={24}>
          <Card size="small" title="Product List">
            {items.length === 0 ? (
              <div style={{ textAlign: "center", padding: 20, color: "#999" }}>
                <Text>No products added yet</Text>
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
                    title: "Product Name",
                    dataIndex: "product_name",
                    key: "product_name",
                    align: "left",
                  },
                  {
                    title: "Quantity",
                    dataIndex: "quantity",
                    key: "quantity",
                    align: "right",
                  },
                  {
                    title: "Weight (kg)",
                    dataIndex: "weight",
                    key: "weight",
                    align: "right",
                    render: (w: number) => w || 0,
                  },
                  {
                    title: "Volume (cm³)",
                    key: "volume",
                    align: "right",
                    responsive: ["lg"],
                    render: (_, r: OrderItem) => {
                      const volume = calculateVolume(r);
                      return volume > 0 ? volume.toLocaleString("en-US") : "-";
                    },
                  },
                  {
                    title: "Fragile",
                    key: "is_fragile",
                    align: "left",
                    responsive: ["md"],
                    render: (_, r: OrderItem) => {
                      const fragile = r.is_fragile || false;
                      return (
                        <Text
                          style={{ color: fragile ? "#ff4d4f" : "#52c41a" }}
                        >
                          {fragile ? "Yes" : "No"}
                        </Text>
                      );
                    },
                  },
                  {
                    title: "Shipping Fee",
                    key: "shipping_fee",
                    align: "right",
                    render: (_, r: OrderItem) => {
                      const fragile = r.is_fragile || false;
                      const fee = calculateBaseShippingFee([r], fragile);
                      return (
                        <Text strong>{fee.toLocaleString("en-US")} ₫</Text>
                      );
                    },
                  },
                ]}
              />
            )}
          </Card>
        </Col>

        {/* ===== Costs ===== */}
        <Col xs={24}>
          <Card size="small" title="Costs">
            <Row gutter={[16, 16]}>
              <Col xs={24} sm={12}>
                <Form.Item
                  name="service_type"
                  label="Service Type"
                  initialValue="STANDARD"
                  rules={[
                    { required: true, message: "Please select service type" },
                  ]}
                >
                  <Select
                    placeholder="Select service type"
                    style={{ width: "100%" }}
                  >
                    <Select.Option value="SECOND_CLASS">
                      Second Class (-20%)
                    </Select.Option>
                    <Select.Option value="STANDARD">Standard</Select.Option>
                    <Select.Option value="FIRST_CLASS">
                      First Class (+30%)
                    </Select.Option>
                    <Select.Option value="EXPRESS">
                      Express (+80%)
                    </Select.Option>
                    <Select.Option value="PRIORITY">
                      Priority (+100%)
                    </Select.Option>
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
                      <Text>Product Fee:</Text>
                    </Col>
                    <Col xs={24} sm={12} style={{ textAlign: "right" }}>
                      <Text>{baseShippingFee.toLocaleString("en-US")} ₫</Text>
                    </Col>

                    <Col xs={24} sm={12}>
                      <Text>Service Type:</Text>
                    </Col>
                    <Col xs={24} sm={12} style={{ textAlign: "right" }}>
                      <Text>x {serviceFeeMultiplier}</Text>
                    </Col>

                    {distanceFee !== null && (
                      <Col
                        xs={24}
                        style={{
                          display: "flex",
                          justifyContent: "space-between",
                        }}
                      >
                        <span>
                          Distance Fee ({distanceRegion}
                          {distanceKm && (
                            <span style={{ color: "#888" }}>
                              {" "}
                              ~{distanceKm.toFixed(2)} km
                            </span>
                          )}
                          )
                        </span>
                        <span style={{ fontWeight: 500 }}>
                          {Math.round(distanceFee).toLocaleString("en-US")} ₫
                        </span>
                      </Col>
                    )}

                    <Col span={24}>
                      <Divider style={{ margin: "12px 0" }} />
                    </Col>

                    <Col xs={12}>
                      <Text strong style={{ fontSize: 16 }}>
                        Total Shipping Fee:
                      </Text>
                    </Col>
                    <Col xs={12} style={{ textAlign: "right" }}>
                      <Text strong style={{ fontSize: 18, color: "#15803d" }}>
                        {totalFee.toLocaleString("en-US")} ₫
                      </Text>
                    </Col>
                  </Row>
                </div>
              </Col>
            </Row>
          </Card>
        </Col>
      </Row>
    </>
  );
}
