"use client";

import { useState, useEffect } from "react";
import {
  Card,
  Form,
  Input,
  Button,
  Typography,
  Row,
  Col,
  Space,
  message,
  Descriptions,
  Modal,
  Spin,
  Switch,
  Grid,
} from "antd";
import {
  ShopOutlined,
  MailOutlined,
  PhoneOutlined,
  EditOutlined,
  SaveOutlined,
} from "@ant-design/icons";
import { Store, UpdateStoreInfoDto } from "@/types/Store";
import { storeService } from "@/services/storeService";

const { Title } = Typography;
const { TextArea } = Input;
const { useBreakpoint } = Grid;

export default function StorePage() {
  const [form] = Form.useForm();
  const [store, setStore] = useState<Store | null>(null);
  const [loading, setLoading] = useState(true);
  const [isEditModalVisible, setIsEditModalVisible] = useState(false);
  const screens = useBreakpoint();

  // Watch form field value for switch color
  const isActiveValue = Form.useWatch("isActive", form);

  useEffect(() => {
    const fetchStoreData = async () => {
      try {
        const userStr = localStorage.getItem("user");
        if (!userStr) {
          message.error("Please login to view store information");
          setLoading(false);
          return;
        }

        let userId;
        try {
          const user = JSON.parse(userStr);
          userId = user.id;
        } catch (parseError) {
          console.error("Error parsing user from localStorage:", parseError);
          message.error("Invalid login information");
          setLoading(false);
          return;
        }

        if (!userId) {
          message.error("User information not found");
          setLoading(false);
          return;
        }

        const data = await storeService.getStoresByUserId(userId.toString());
        if (data && data.length > 0) {
          setStore(data[0]);
        } else {
          message.info("Store information not found");
        }
      } catch (error) {
        console.error("Failed to fetch store:", error);
        if (error instanceof Error) {
          message.error(`Error loading store information: ${error.message}`);
        } else {
          message.error("Unable to connect to server");
        }
      } finally {
        setLoading(false);
      }
    };

    fetchStoreData();
  }, []);

  const showEditModal = () => {
    form.setFieldsValue({
      storeName: store?.storeName,
      phone: store?.phone,
      email: store?.email,
      notes: store?.notes,
      isActive: store?.isActive,
    });
    setIsEditModalVisible(true);
  };

  const handleUpdate = async (values: UpdateStoreInfoDto) => {
    try {
      if (!store?.id) return;

      message.loading({ content: "Updating...", key: "updateStore" });
      const updatedStoreInfo = await storeService.updateStoreInfo(
        store.id.toString(),
        values
      );

      setStore({
        ...store,
        storeName: updatedStoreInfo.storeName,
        email: updatedStoreInfo.email,
        phone: updatedStoreInfo.phone,
        isActive: updatedStoreInfo.isActive,
        notes: updatedStoreInfo.notes,
        updatedAt: updatedStoreInfo.updatedAt,
      });

      message.success({
        content: "Information updated successfully!",
        key: "updateStore",
      });
      setIsEditModalVisible(false);
    } catch (error) {
      console.error("Failed to update store:", error);
      message.error({
        content: "Unable to update information!",
        key: "updateStore",
      });
    }
  };

  if (loading) {
    return (
      <div style={{ textAlign: "center", padding: "50px" }}>
        <Spin size="large" />
      </div>
    );
  }

  return (
    <div
      style={{
        minHeight: "100vh",
        padding: "24px",
      }}
    >
      
        {/* Header responsive */}
        <Row
          justify="space-between"
          align="middle"
          style={{ marginBottom: 24 }}
          wrap
        >
          <Col xs={24} sm={16}>
            <Title
              level={2}
              style={{
                marginBottom: 12,
                color: "#15803d",
                fontSize: "clamp(20px, 4vw, 28px)",
              }}
            >
              Store Information
            </Title>
          </Col>
          <Col xs={24} sm={8} style={{ textAlign: "right" }}>
            <Button
              type="primary"
              icon={<EditOutlined />}
              onClick={showEditModal}
              block={!screens.sm} // mobile full width
              style={{
                backgroundColor: "#15803d",
                borderColor: "#15803d",
                borderRadius: 8,
                height: 40,
                fontWeight: 500,
              }}
            >
              Update Information
            </Button>
          </Col>
        </Row>

        {!store ? (
          <Card
            style={{
              borderRadius: 16,
              border: "1px solid #f0f0f0",
            }}
          >
            <div style={{ textAlign: "center", padding: "40px" }}>
              <ShopOutlined
                style={{
                  fontSize: "48px",
                  color: "#d9d9d9",
                  marginBottom: "16px",
                }}
              />
              <Title level={4} style={{ color: "#666" }}>
                Store information not found
              </Title>
              <p style={{ color: "#666", marginBottom: "24px" }}>
                You may not have registered a store yet or login information is
                incorrect.
              </p>
              <Space direction={screens.md ? "horizontal" : "vertical"}>
                <Button
                  type="primary"
                  onClick={() => window.location.reload()}
                  style={{
                    backgroundColor: "#15803d",
                    borderColor: "#15803d",
                    borderRadius: 8,
                  }}
                >
                  Reload Page
                </Button>
                <Button
                  onClick={() => {
                    localStorage.removeItem("user");
                    window.location.href = "/login";
                  }}
                  style={{
                    borderRadius: 8,
                  }}
                >
                  Login Again
                </Button>
              </Space>
            </div>
          </Card>
        ) : (
          <Card
            style={{
              borderRadius: 16,
              border: "1px solid #f0f0f0",
            }}
          >
            <Descriptions
              bordered
              column={{ xxl: 2, xl: 2, lg: 2, md: 1, sm: 1, xs: 1 }}
            >
              <Descriptions.Item label="Store Name">
                {store?.storeName}
              </Descriptions.Item>
              <Descriptions.Item label="Status">
                {store?.isActive ? "Active" : "Inactive"}
              </Descriptions.Item>
              <Descriptions.Item label="Email">
                {store?.email}
              </Descriptions.Item>
              <Descriptions.Item label="Phone Number">
                {store?.phone}
              </Descriptions.Item>
              <Descriptions.Item label="Address" span={2}>
                {store?.address}
              </Descriptions.Item>
              <Descriptions.Item label="Notes" span={2}>
                {store?.notes || "No notes"}
              </Descriptions.Item>
              <Descriptions.Item label="Created Date">
                {store?.createdAt &&
                  new Date(store.createdAt).toLocaleDateString("en-US")}
              </Descriptions.Item>
              <Descriptions.Item label="Last Updated">
                {store?.updatedAt &&
                  new Date(store.updatedAt).toLocaleDateString("en-US")}
              </Descriptions.Item>
            </Descriptions>
          </Card>
        )}

        {/* Modal cập nhật */}
        <Modal
          title={
            <div
              style={{
                color: "#15803d",
                fontSize: "18px",
                fontWeight: 600,
                paddingBottom: "8px",
                borderBottom: "1px solid #f0f0f0",
              }}
            >
              Update Store Information
            </div>
          }
          open={isEditModalVisible}
          onCancel={() => setIsEditModalVisible(false)}
          footer={null}
          width={screens.md ? 700 : "90%"}
          style={{ top: screens.md ? 80 : 30 }}
          styles={{
            body: {
              padding: screens.md ? "22px" : "16px",
            },
            header: {
              backgroundColor: "#fff",
              borderBottom: "none",
              padding: "16px 24px 12px",
            },
          }}
        >
          <div
            style={{
              backgroundColor: "#e8f5e8",
              border: "1px solid #b7eb8f",
              borderRadius: 6,
              padding: "12px",
              marginBottom: 20,
            }}
          >
            <Typography.Text style={{ color: "#389e0d", fontSize: "13px" }}>
              <strong>💡 Note:</strong> Only store name, email, phone number,
              status and notes can be updated. Address is read-only and cannot
              be edited.
            </Typography.Text>
          </div>
          <Form
            form={form}
            layout="vertical"
            onFinish={handleUpdate}
            initialValues={store || {}}
            style={{
              backgroundColor: "#fff",
              padding: "24px",
              borderRadius: 8,
              boxShadow: "0 1px 4px rgba(0,0,0,0.04)",
            }}
          >
            <Row gutter={[20, 12]}>
              <Col xs={24} md={12}>
                <Form.Item
                  name="storeName"
                  label="Store Name"
                  rules={[
                    { required: true, message: "Please enter store name!" },
                  ]}
                >
                  <Input
                    prefix={<ShopOutlined style={{ color: "#15803d" }} />}
                    placeholder="Enter store name"
                    style={{
                      borderRadius: 6,
                      height: 38,
                      fontSize: "14px",
                    }}
                  />
                </Form.Item>
              </Col>
              <Col xs={24} md={12}>
                <Form.Item
                  name="phone"
                  label="Phone Number"
                  rules={[
                    { required: true, message: "Please enter phone number!" },
                    {
                      pattern: /^[0-9]{10}$/,
                      message: "Invalid phone number!",
                    },
                  ]}
                >
                  <Input
                    prefix={<PhoneOutlined style={{ color: "#15803d" }} />}
                    placeholder="Enter phone number"
                    style={{
                      borderRadius: 6,
                      height: 38,
                      fontSize: "14px",
                    }}
                  />
                </Form.Item>
              </Col>
            </Row>

            <Form.Item
              name="email"
              label="Email"
              rules={[
                { required: true, message: "Please enter email!" },
                { type: "email", message: "Invalid email!" },
              ]}
            >
              <Input
                prefix={<MailOutlined style={{ color: "#15803d" }} />}
                placeholder="Enter email"
                style={{
                  borderRadius: 6,
                  height: 38,
                  fontSize: "14px",
                }}
              />
            </Form.Item>

            <Form.Item
              label="Address (Read-only)"
              extra="Address cannot be updated through this form. Please contact administrator to change address."
            >
              <Input.TextArea
                value={store?.address}
                disabled
                rows={2}
                style={{
                  backgroundColor: "#f8f8f8",
                  color: "#666",
                  borderRadius: 6,
                  fontSize: "13px",
                  border: "1px solid #e8e8e8",
                }}
              />
            </Form.Item>

            <Form.Item name="notes" label="Notes">
              <TextArea
                placeholder="Enter notes (optional)"
                rows={3}
                style={{
                  borderRadius: 6,
                  fontSize: "14px",
                }}
              />
            </Form.Item>

            <Form.Item
              name="isActive"
              label="Status"
              valuePropName="checked"
              dependencies={["isActive"]}
            >
              <Switch
                checkedChildren="Active"
                unCheckedChildren="Inactive"
                style={{
                  backgroundColor: isActiveValue ? "#15803d" : "#bfbfbf",
                }}
              />
            </Form.Item>

            <Form.Item
              style={{
                marginBottom: 0,
                textAlign: "right",
                paddingTop: "20px",
                borderTop: "1px solid #f0f0f0",
              }}
            >
              <Space size="middle">
                <Button
                  onClick={() => setIsEditModalVisible(false)}
                  style={{
                    borderRadius: 6,
                    height: 38,
                    paddingLeft: 20,
                    paddingRight: 20,
                    fontWeight: 500,
                    border: "1px solid #d9d9d9",
                    color: "#666",
                  }}
                >
                  Cancel
                </Button>
                <Button
                  type="primary"
                  htmlType="submit"
                  icon={<SaveOutlined />}
                  style={{
                    backgroundColor: "#15803d",
                    borderColor: "#15803d",
                    borderRadius: 6,
                    height: 38,
                    paddingLeft: 24,
                    paddingRight: 24,
                    fontWeight: 600,
                    fontSize: "14px",
                    boxShadow: "0 1px 3px rgba(21,128,61,0.2)",
                  }}
                >
                  Save Changes
                </Button>
              </Space>
            </Form.Item>
          </Form>
        </Modal>
      
    </div>
  );
}
