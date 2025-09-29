"use client";
import { useState, useEffect } from "react";
import { Card, Steps, Typography, Button, Form, message } from "antd";
import {
  ShopOutlined,
  BoxPlotOutlined,
  DollarOutlined,
} from "@ant-design/icons";
import { Store } from "@/types/Store";
import { storeService } from "@/services/storeService";
import StepStoreInfo from "../components/StepStoreInfo";
import StepOrderItems from "../components/StepOrderItems";
import { OrderForm } from "@/types/orders";
import StepInvoice from "../components/StepInvoice";
import {
  createAddressPayload,
  createProductPayload,
  createOrderPayload,
  createOrderItemPayload,
  createDeliveryPayload,
  getCurrentUserId,
} from "@/utils/orderFlow";
import { OrderFlowService } from "@/services/orderFlowService";
import { isValidItem } from "@/utils/orderItems";

const { Title } = Typography;

export default function CreateOrder() {
  const [currentStep, setCurrentStep] = useState(0);
  const [form] = Form.useForm();
  const [store, setStore] = useState<Store | null>(null);

  useEffect(() => {
    const fetchStore = async () => {
      try {
        const userStr = localStorage.getItem("user");
        if (!userStr) return;
        const user = JSON.parse(userStr);
        const data = await storeService.getStoresByUserId(user.id.toString());
        if (data && data.length > 0) setStore(data[0]);
      } catch (e) {
        message.error("Lỗi tải cửa hàng");
      }
    };
    fetchStore();
  }, []);

  const steps = [
    {
      title: "Store Information",
      icon: <ShopOutlined style={{ color: "#0284c7" }} />,
      content: <StepStoreInfo store={store} />,
      color: "#0284c7",
      // backgroundColor: "linear-gradient(135deg, #f0f9ff 0%, #e0f2fe 100%)",
    },
    {
      title: "Order Details",
      icon: <BoxPlotOutlined style={{ color: "#15803d" }} />,
      content: <StepOrderItems form={form} />, // chỉ truyền form, không cần items props
      color: "#15803d",
      // backgroundColor: "linear-gradient(135deg, #f0fdf4 0%, #dcfce7 100%)",
    },
    {
      title: "Invoice",
      icon: <DollarOutlined style={{ color: "#d97706" }} />,
      content: <StepInvoice form={form} store={store} />,
      color: "#d97706",
      // backgroundColor: "linear-gradient(135deg, #fffbeb 0%, #fef3c7 100%)",
    },
  ];

  const next = async () => {
    try {
      await form.validateFields();
      setCurrentStep((prev) => prev + 1);
    } catch {
      // bỏ qua error log
    }
  };

  const prev = () => setCurrentStep((prev) => prev - 1);

  // Hàm submit tạo đơn hàng thực tế sử dụng utility functions
  const handleTestCompleteFlow = async (values: OrderForm) => {
    try {
      // Lấy tất cả dữ liệu từ form (bao gồm cả hidden fields)
      const allFormValues = form.getFieldsValue(true);

      // Merge data từ cả parameter và form
      const mergedValues = { ...allFormValues, ...values };

      // Kiểm tra dữ liệu bắt buộc
      if (!store) {
        message.error("Store information not found!");
        return;
      }

      if (!mergedValues.address || !mergedValues.city) {
        message.error("Please select a complete delivery address!");
        return;
      }

      if (!mergedValues.items || mergedValues.items.length === 0) {
        message.error("You need to add at least one product!");
        return;
      }

      if (!mergedValues.pickup_date || !mergedValues.pickup_time_period) {
        message.error("Please select a pickup time!");
        return;
      }

      const loadingMessage = message.loading("Order is being created...", 0);

      try {
        // BƯỚC 1: Lưu Address
        console.log("📍 Creating address with values:", mergedValues);

        // Tạo payload chỉ với những field cần thiết cho address
        const addressOnlyValues = {
          addressType: mergedValues.addressType,
          address: mergedValues.address,
          city: mergedValues.city,
          receiver_name: mergedValues.receiver_name,
          receiver_phone: mergedValues.receiver_phone,
          receiver_email: mergedValues.receiver_email,
          latitude: mergedValues.latitude,
          longitude: mergedValues.longitude,
        };

        const addressPayload = createAddressPayload(addressOnlyValues);
        console.log("📍 Address payload:", addressPayload);
        const addressResult = await OrderFlowService.createAddress(
          addressPayload
        );

        if (!addressResult.id) {
          throw new Error("Không lấy được ID địa chỉ vừa tạo!");
        }

        // BƯỚC 2: Lưu Products
        const productResults = [];
        for (const item of mergedValues.items) {
          if (isValidItem(item)) {
            try {
              const productPayload = createProductPayload(item, store.id);
              const productResult = await OrderFlowService.createProduct(
                productPayload
              );
              productResults.push({
                name: item.product_name,
                result: productResult,
              });
            } catch (error: unknown) {
              productResults.push({
                name: item.product_name,
                error: error instanceof Error ? error.message : "Unknown error",
              });
            }
          }
        }

        // BƯỚC 3: Tạo Order
        const currentUserId = getCurrentUserId();
        const orderPayload = createOrderPayload(
          store,
          addressResult.id,
          mergedValues,
          currentUserId
        );
        const orderResult = await OrderFlowService.createOrder(orderPayload);

        // BƯỚC 4: Tạo Order Items
        const orderItemResults = [];
        const serviceType = mergedValues.service_type || "STANDARD";

        for (let i = 0; i < productResults.length; i++) {
          const productResult = productResults[i];
          const originalItem = mergedValues.items[i];

          if (
            productResult.result &&
            originalItem &&
            isValidItem(originalItem)
          ) {
            try {
              const orderItemPayload = createOrderItemPayload(
                orderResult.id,
                productResult.result.id,
                originalItem,
                serviceType
              );

              const orderItemResult = await OrderFlowService.createOrderItem(
                orderItemPayload
              );
              orderItemResults.push({
                productName: productResult.name,
                result: orderItemResult,
              });
            } catch (error: unknown) {
              orderItemResults.push({
                productName: productResult.name,
                error: error instanceof Error ? error.message : "Unknown error",
              });
            }
          }
        }

        // BƯỚC 5: Tạo Delivery
        let deliveryResult = null;
        try {
          const deliveryPayload = createDeliveryPayload(
            orderResult.id,
            form,
            serviceType,
            mergedValues.notes
          );

          deliveryResult = await OrderFlowService.createDelivery(
            deliveryPayload
          );
        } catch (error: unknown) {
          console.error("❌ Delivery creation failed:", error);
        }

        loadingMessage();

        const successfulProducts = productResults.filter(
          (p) => p.result
        ).length;
        const successfulOrderItems = orderItemResults.filter(
          (oi) => oi.result
        ).length;
        const deliveryStatus = deliveryResult ? "Thành công" : "Lỗi";

        // Log tổng kết chi tiết
        console.log("🎯 COMPLETE FLOW SUMMARY:");
        console.log("📍 Address:", addressResult);
        console.log("📦 Products:", productResults);
        console.log("📋 Order:", orderResult);
        console.log("📄 Order Items:", orderItemResults);
        console.log("🚚 Delivery:", deliveryResult);

        message.success(
          `Tạo đơn hàng thành công!\n✅ Mã đơn hàng: ${orderResult.id}\n✅ ${successfulProducts} sản phẩm\n✅ ${successfulOrderItems} order items\n✅ Delivery: ${deliveryStatus}`
        );

        // Reset form sau khi tạo thành công
        form.resetFields();
        setCurrentStep(0);
      } catch (error: unknown) {
        loadingMessage();
        throw error;
      }
    } catch (error: unknown) {
      console.error("💥 Complete flow error:", error);
      message.error(
        error instanceof Error ? error.message : "Lỗi khi tạo đơn hàng"
      );
    }
  };

  return (
    <Card>
      <Title level={2}>Create New Order</Title>
      <Steps
        current={currentStep}
        items={steps.map((s) => ({ title: s.title, icon: s.icon }))}
      />
      <div
        style={{
          
          borderRadius: 8,
          padding: "24px",
          marginTop: 24,
          marginBottom: 24,
          // border: `1px solid ${steps[currentStep].color}20`,
        }}
      >
        <Form form={form} layout="vertical" onFinish={handleTestCompleteFlow}>
          {steps[currentStep].content}
          <div style={{ marginTop: 24, textAlign: "right" }}>
            {currentStep > 0 && (
              <Button onClick={prev} style={{ marginRight: 8 }}>
                Back
              </Button>
            )}
            {currentStep < steps.length - 1 && (
              <Button
                type="primary"
                onClick={next}
                style={{
                  marginRight: 8,
                  backgroundColor: steps[currentStep].color,
                  borderColor: steps[currentStep].color,
                }}
              >
                Next
              </Button>
            )}
            {currentStep === steps.length - 1 && (
              <Button
                type="primary"
                htmlType="submit"
                style={{
                  marginRight: 8,
                  backgroundColor: steps[currentStep].color,
                  borderColor: steps[currentStep].color,
                }}
              >
                Create New Order
              </Button>
            )}
          </div>
        </Form>
      </div>
    </Card>
  );
}
