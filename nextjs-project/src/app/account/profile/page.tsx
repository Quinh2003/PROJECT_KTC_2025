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
  Divider,
  message,
  Descriptions,
  Modal,
  Spin,
} from "antd";
import {
  UserOutlined,
  MailOutlined,
  PhoneOutlined,
  EditOutlined,
  HomeOutlined,
  SaveOutlined,
} from "@ant-design/icons";

const { Title, Text } = Typography;
const { TextArea } = Input;

interface UserProfile {
  id: number;
  username: string;
  email: string;
  full_name: string;
  phone: string;
  address: string; // We'll add this to the user's notes field
  notes: string;
  created_at: string;
  updated_at: string;
}

export default function ProfilePage() {
  const [form] = Form.useForm();
  const [user, setUser] = useState<UserProfile | null>(null);
  const [loading, setLoading] = useState(true);
  const [isEditModalVisible, setIsEditModalVisible] = useState(false);

  useEffect(() => {
    // TODO: Replace with actual API call
    const fetchUserProfile = async () => {
      try {
        // Simulate API call
        const mockUser = {
          id: 1,
          username: "user123",
          email: "user@example.com",
          full_name: "Nguyễn Văn A",
          phone: "0123456789",
          address: "123 Đường ABC, Quận 1, TP.HCM",
          notes: "",
          created_at: "2025-08-25T00:00:00.000Z",
          updated_at: "2025-08-25T00:00:00.000Z",
        };

        setUser(mockUser);
        setLoading(false);
      } catch (error) {
        console.error("Failed to fetch user profile:", error);
        message.error("Không thể tải thông tin người dùng");
        setLoading(false);
      }
    };

    fetchUserProfile();
  }, []);

  const showEditModal = () => {
    form.setFieldsValue({
      full_name: user?.full_name,
      phone: user?.phone,
      email: user?.email,
      address: user?.address,
      notes: user?.notes,
    });
    setIsEditModalVisible(true);
  };

  const handleUpdate = async (values: Partial<UserProfile>) => {
    try {
      // TODO: Replace with actual API call
      console.log("Updating profile with:", values);
      message.loading({ content: "Đang cập nhật...", key: "updateProfile" });

      // Simulate API delay
      await new Promise((resolve) => setTimeout(resolve, 1000));

      // Update local state
      setUser((prev) => (prev ? { ...prev, ...values } : null));

      message.success({
        content: "Cập nhật thông tin thành công!",
        key: "updateProfile",
      });
      setIsEditModalVisible(false);
    } catch (error) {
      console.error("Failed to update profile:", error);
      message.error({
        content: "Không thể cập nhật thông tin!",
        key: "updateProfile",
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
      <Row justify="space-between" align="middle" style={{ marginBottom: 24 }}>
        <Title level={2}>Thông tin cá nhân</Title>
        <Button type="primary" icon={<EditOutlined />} onClick={showEditModal}>
          Cập nhật thông tin
        </Button>
      </Row>

      <Card>
        <Descriptions
          bordered
          column={{ xxl: 2, xl: 2, lg: 2, md: 1, sm: 1, xs: 1 }}
        >
          <Descriptions.Item label="Họ và tên">
            {user?.full_name}
          </Descriptions.Item>
          <Descriptions.Item label="Tên đăng nhập">
            {user?.username}
          </Descriptions.Item>
          <Descriptions.Item label="Email">{user?.email}</Descriptions.Item>
          <Descriptions.Item label="Số điện thoại">
            {user?.phone}
          </Descriptions.Item>
          <Descriptions.Item label="Địa chỉ" span={2}>
            {user?.address}
          </Descriptions.Item>
          <Descriptions.Item label="Ghi chú" span={2}>
            {user?.notes || "Không có ghi chú"}
          </Descriptions.Item>
          <Descriptions.Item label="Ngày tạo">
            {new Date(user?.created_at || "").toLocaleDateString("vi-VN")}
          </Descriptions.Item>
          <Descriptions.Item label="Cập nhật lần cuối">
            {new Date(user?.updated_at || "").toLocaleDateString("vi-VN")}
          </Descriptions.Item>
        </Descriptions>
      </Card>

      <Modal
        title="Cập nhật thông tin cá nhân"
        open={isEditModalVisible}
        onCancel={() => setIsEditModalVisible(false)}
        footer={null}
        width={800}
      >
        <Form
          form={form}
          layout="vertical"
          onFinish={handleUpdate}
          initialValues={user || {}}
        >
          <Row gutter={16}>
            <Col span={12}>
              <Form.Item
                name="full_name"
                label="Họ và tên"
                rules={[
                  { required: true, message: "Vui lòng nhập họ và tên!" },
                ]}
              >
                <Input prefix={<UserOutlined />} placeholder="Nhập họ và tên" />
              </Form.Item>
            </Col>
            <Col span={12}>
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
            name="address"
            label="Địa chỉ"
            rules={[{ required: true, message: "Vui lòng nhập địa chỉ!" }]}
          >
            <TextArea
              prefix={<HomeOutlined />}
              placeholder="Nhập địa chỉ đầy đủ"
              rows={3}
            />
          </Form.Item>

          <Form.Item name="notes" label="Ghi chú">
            <TextArea placeholder="Nhập ghi chú (không bắt buộc)" rows={3} />
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
