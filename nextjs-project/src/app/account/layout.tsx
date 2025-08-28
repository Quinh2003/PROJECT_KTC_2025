"use client";

import { useEffect, useState } from "react";
import { useRouter, usePathname } from "next/navigation";
import Link from "next/link";
import { removeTokenCookie, removeRefreshTokenCookie } from "../../lib/auth";
import { User } from "../../types/User";
import { Layout, Menu, Button, Typography, Avatar, Spin, Space } from "antd";
import {
  LogoutOutlined,
  HomeOutlined,
  ShoppingOutlined,
  WalletOutlined,
  UserOutlined,
  PlusOutlined,
} from "@ant-design/icons";

const { Header, Sider, Content, Footer } = Layout;

interface AccountLayoutProps {
  children: React.ReactNode;
}

export default function AccountLayout({ children }: AccountLayoutProps) {
  const [user, setUser] = useState<User | null>(null);
  const [loading, setLoading] = useState(true);
  const router = useRouter();
  const pathname = usePathname();

  useEffect(() => {
    // Check authentication and role
    let storedUser = null;
    let token = null;
    if (typeof window !== "undefined") {
      storedUser = localStorage.getItem("user");
      token = localStorage.getItem("token");
    }

    if (!storedUser || !token) {
      router.push("/login");
      return;
    }

    const userData = JSON.parse(storedUser);

    // Verify user is customer
    if (userData.role?.toLowerCase() !== "customer") {
      alert("Truy cập không được phép!");
      if (typeof window !== "undefined") {
        localStorage.clear();
      }
      router.push("/login");
      return;
    }

    setUser(userData);
    setLoading(false);
  }, [router]);

  const handleLogout = () => {
    if (typeof window !== "undefined") {
      localStorage.clear();
      removeTokenCookie();
      removeRefreshTokenCookie();
      console.log("[Logout] Token và RefreshToken đã bị xóa khỏi cookie.");
    }
    router.push("/login");
  };

  if (loading) {
    return (
      <div
        style={{
          height: "100vh",
          display: "flex",
          justifyContent: "center",
          alignItems: "center",
          background: "#f0f2f5",
        }}
      >
        <Spin size="large">
          <div
            style={{
              padding: "50px",
              background: "rgba(0, 0, 0, 0.05)",
              borderRadius: "4px",
            }}
          >
            <Typography.Text type="secondary">Đang tải...</Typography.Text>
          </div>
        </Spin>
      </div>
    );
  }

  const menuItems = [
    { key: "/account", label: "Trang chủ", icon: <HomeOutlined /> },
    {
      key: "/account/orders/new",
      label: "Tạo đơn hàng",
      icon: <PlusOutlined />,
    },
    { key: "/account/orders", label: "Đơn hàng", icon: <ShoppingOutlined /> },

    { key: "/account/estimate", label: "Tính phí", icon: <WalletOutlined /> },
    {
      key: "/account/profile",
      label: "Thông tin cá nhân",
      icon: <UserOutlined />,
    },
  ];

  return (
    <Layout style={{ minHeight: "100vh" }}>
      <Header
        style={{
          background: "#001529",
          padding: "0 24px",
          display: "flex",
          alignItems: "center",
          justifyContent: "space-between",
          position: "fixed",
          zIndex: 1000,
          width: "100%",
          left: 0,
          top: 0,
        }}
      >
        <div style={{ display: "flex", alignItems: "center", gap: "16px" }}>
          <Typography.Title level={4} style={{ margin: 0 }}>
            <Link href="/account" style={{ color: "#fff" }}>
              Fast Route Logistics
            </Link>
          </Typography.Title>
          <Button
            type="text"
            size="small"
            style={{ background: "rgba(255,255,255,0.1)", color: "#fff" }}
          >
            Khách hàng
          </Button>
        </div>
        <Space>
          <Typography.Text style={{ color: "#fff" }}>
            Xin chào,{" "}
            <Typography.Text strong style={{ color: "#fff" }}>
              {user?.fullName}
            </Typography.Text>
          </Typography.Text>
          <Button
            onClick={handleLogout}
            icon={<LogoutOutlined />}
            type="default"
            ghost
            style={{ borderColor: "#fff", color: "#fff" }}
          >
            Đăng xuất
          </Button>
        </Space>
      </Header>

      <Layout>
        <Sider
          width={200}
          style={{
            background: "#001529",
            position: "fixed",
            height: "100vh",
            left: 0,
            top: 64, // Header height
            overflow: "auto",
          }}
        >
          <Menu
            mode="inline"
            selectedKeys={[pathname]}
            style={{ height: "100%" }}
            theme="dark"
            items={menuItems.map((item) => ({
              key: item.key,
              icon: item.icon,
              label: (
                <Link href={item.key} style={{ color: "inherit" }}>
                  {item.label}
                </Link>
              ),
            }))}
          />
        </Sider>

        <Layout
          style={{ marginLeft: 200, marginTop: 64, background: "#ffffff" }}
        >
          <Content
            style={{
              padding: 34,
              margin: 0,
              background: "#ffffff",
              borderRadius: 8,
              minHeight: "calc(100vh - 64px - 70px)", // viewport height - header height - footer height
            }}
          >
            {children}
          </Content>

          <Footer
            style={{
              textAlign: "center",
              background: "transparent",
              height: "70px",
              padding: "24px",
            }}
          >
            <Typography.Text style={{ color: "#666" }}>
              © 2025 Fast Route Logistics. Tất cả quyền được bảo lưu.
            </Typography.Text>
          </Footer>
        </Layout>
      </Layout>
    </Layout>
  );
}
