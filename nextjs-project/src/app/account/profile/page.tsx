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

  useEffect(() => {
    const fetchStoreData = async () => {
      try {
        const userStr = localStorage.getItem("user");
        if (!userStr) {
          message.error("Vui lòng đăng nhập để xem thông tin cửa hàng");
          setLoading(false);
          return;
        }

        let userId;
        try {
          const user = JSON.parse(userStr);
          userId = user.id;
        } catch (parseError) {
          console.error("Error parsing user from localStorage:", parseError);
          message.error("Thông tin đăng nhập không hợp lệ");
          setLoading(false);
          return;
        }

        if (!userId) {
          message.error("Không tìm thấy thông tin người dùng");
          setLoading(false);
          return;
        }

        const data = await storeService.getStoresByUserId(userId.toString());
        if (data && data.length > 0) {
          setStore(data[0]);
        } else {
          message.info("Không tìm thấy thông tin cửa hàng");
        }
      } catch (error) {
        console.error("Failed to fetch store:", error);
        if (error instanceof Error) {
          message.error(`Lỗi khi tải thông tin cửa hàng: ${error.message}`);
        } else {
          message.error("Không thể kết nối đến máy chủ");
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

      message.loading({ content: "Đang cập nhật...", key: "updateStore" });
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
        content: "Cập nhật thông tin thành công!",
        key: "updateStore",
      });
      setIsEditModalVisible(false);
    } catch (error) {
      console.error("Failed to update store:", error);
      message.error({
        content: "Không thể cập nhật thông tin!",
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
    <Card>
      {/* Header responsive */}
      <Row
        justify="space-between"
        align="middle"
        style={{ marginBottom: 24 }}
        wrap
      >
        <Col xs={24} sm="auto">
          <Title level={2} style={{ marginBottom: 12 }}>
            Thông tin cửa hàng
          </Title>
        </Col>
        <Col xs={24} sm="auto">
          <Button
            type="primary"
            icon={<EditOutlined />}
            onClick={showEditModal}
            block={!screens.md} // mobile full width
          >
            Cập nhật thông tin
          </Button>
        </Col>
      </Row>

      {!store ? (
        <Card>
          <div style={{ textAlign: "center", padding: "40px" }}>
            <ShopOutlined
              style={{
                fontSize: "48px",
                color: "#d9d9d9",
                marginBottom: "16px",
              }}
            />
            <Title level={4} style={{ color: "#999" }}>
              Không tìm thấy thông tin cửa hàng
            </Title>
            <p style={{ color: "#666", marginBottom: "24px" }}>
              Có thể bạn chưa đăng ký cửa hàng hoặc thông tin đăng nhập không
              chính xác.
            </p>
            <Space direction={screens.md ? "horizontal" : "vertical"}>
              <Button type="primary" onClick={() => window.location.reload()}>
                Tải lại trang
              </Button>
              <Button
                onClick={() => {
                  localStorage.removeItem("user");
                  window.location.href = "/auth/login";
                }}
              >
                Đăng nhập lại
              </Button>
            </Space>
          </div>
        </Card>
      ) : (
        <Card>
          <Descriptions
            bordered
            column={{ xxl: 2, xl: 2, lg: 2, md: 1, sm: 1, xs: 1 }}
          >
            <Descriptions.Item label="Tên cửa hàng">
              {store?.storeName}
            </Descriptions.Item>
            <Descriptions.Item label="Trạng thái">
              {store?.isActive ? "Đang hoạt động" : "Ngừng hoạt động"}
            </Descriptions.Item>
            <Descriptions.Item label="Email">{store?.email}</Descriptions.Item>
            <Descriptions.Item label="Số điện thoại">
              {store?.phone}
            </Descriptions.Item>
            <Descriptions.Item label="Địa chỉ" span={2}>
              {store?.address}
            </Descriptions.Item>
            <Descriptions.Item label="Ghi chú" span={2}>
              {store?.notes || "Không có ghi chú"}
            </Descriptions.Item>
            <Descriptions.Item label="Ngày tạo">
              {store?.createdAt &&
                new Date(store.createdAt).toLocaleDateString("vi-VN")}
            </Descriptions.Item>
            <Descriptions.Item label="Cập nhật lần cuối">
              {store?.updatedAt &&
                new Date(store.updatedAt).toLocaleDateString("vi-VN")}
            </Descriptions.Item>
          </Descriptions>
        </Card>
      )}

      {/* Modal cập nhật */}
      <Modal
        title="Cập nhật thông tin cửa hàng"
        open={isEditModalVisible}
        onCancel={() => setIsEditModalVisible(false)}
        footer={null}
        width={screens.md ? 800 : "100%"}
        style={{ top: screens.md ? 100 : 0 }}
        styles={{ body: { padding: screens.md ? 24 : 12 } }}
      >
        <div style={{ marginBottom: 16 }}>
          <Typography.Text type="secondary">
            <strong>Lưu ý:</strong> Chỉ có thể cập nhật tên cửa hàng, email, số
            điện thoại, trạng thái và ghi chú. Địa chỉ chỉ được hiển thị và
            không thể chỉnh sửa.
          </Typography.Text>
        </div>
        <Form
          form={form}
          layout="vertical"
          onFinish={handleUpdate}
          initialValues={store || {}}
        >
          <Row gutter={16}>
            <Col xs={24} md={12}>
              <Form.Item
                name="storeName"
                label="Tên cửa hàng"
                rules={[
                  { required: true, message: "Vui lòng nhập tên cửa hàng!" },
                ]}
              >
                <Input
                  prefix={<ShopOutlined />}
                  placeholder="Nhập tên cửa hàng"
                />
              </Form.Item>
            </Col>
            <Col xs={24} md={12}>
              <Form.Item
                name="phone"
                label="Số điện thoại"
                rules={[
                  { required: true, message: "Vui lòng nhập số điện thoại!" },
                  {
                    pattern: /^[0-9]{10}$/,
                    message: "Số điện thoại không hợp lệ!",
                  },
                ]}
              >
                <Input
                  prefix={<PhoneOutlined />}
                  placeholder="Nhập số điện thoại"
                />
              </Form.Item>
            </Col>
          </Row>

          <Form.Item
            name="email"
            label="Email"
            rules={[
              { required: true, message: "Vui lòng nhập email!" },
              { type: "email", message: "Email không hợp lệ!" },
            ]}
          >
            <Input prefix={<MailOutlined />} placeholder="Nhập email" />
          </Form.Item>

          <Form.Item
            label="Địa chỉ (chỉ xem)"
            extra="Địa chỉ không thể cập nhật qua form này. Vui lòng liên hệ quản trị viên để thay đổi địa chỉ."
          >
            <Input.TextArea
              value={store?.address}
              disabled
              rows={3}
              style={{ backgroundColor: "#f5f5f5", color: "#666" }}
            />
          </Form.Item>

          <Form.Item name="notes" label="Ghi chú">
            <TextArea placeholder="Nhập ghi chú (không bắt buộc)" rows={3} />
          </Form.Item>

          <Form.Item name="isActive" label="Trạng thái" valuePropName="checked">
            <Switch
              checkedChildren="Hoạt động"
              unCheckedChildren="Ngừng hoạt động"
            />
          </Form.Item>

          <Form.Item style={{ marginBottom: 0, textAlign: "right" }}>
            <Space>
              <Button onClick={() => setIsEditModalVisible(false)}>Hủy</Button>
              <Button type="primary" htmlType="submit" icon={<SaveOutlined />}>
                Lưu thay đổi
              </Button>
            </Space>
          </Form.Item>
        </Form>
      </Modal>
    </Card>
  );
}
