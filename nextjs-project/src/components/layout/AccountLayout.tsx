"use client";

import { useEffect, useState } from "react";
import { useRouter, usePathname } from "next/navigation";
import { Layout, Typography, Spin, Grid } from "antd";
import { removeTokenCookie, removeRefreshTokenCookie } from "../../lib/auth";
import { User } from "../../types/User";

import AccountHeader from "./AccountHeader";
import AccountSidebar from "./AccountSidebar";
import AccountFooter from "./AccountFooter";

const { Content } = Layout;
const { useBreakpoint } = Grid;

interface AccountLayoutProps {
  children: React.ReactNode;
}

export default function AccountLayout({ children }: AccountLayoutProps) {
  const [user, setUser] = useState<User | null>(null);
  const [loading, setLoading] = useState(true);
  const [sidebarOpen, setSidebarOpen] = useState(false); // quản lý Sidebar mobile
  const router = useRouter();
  const pathname = usePathname();
  const screens = useBreakpoint();

  // --- Check authentication + role ---
  useEffect(() => {
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
    if (userData.role?.toLowerCase() !== "customer") {
      alert("Truy cập không được phép!");
      localStorage.clear();
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
    }
    router.push("/login");
  };

  // --- Loading state ---
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

  // --- Layout render ---
  return (
    <Layout style={{ minHeight: "100vh" }}>
      <AccountHeader
        userName={user?.fullName}
        onLogout={handleLogout}
        onMenuToggle={() => setSidebarOpen(true)} // mở Sidebar mobile
      />

      <Layout>
        <AccountSidebar
          pathname={pathname}
          open={sidebarOpen}
          onClose={() => setSidebarOpen(false)}
        />

        <Layout
          style={{
            marginLeft: screens.md ? 200 : 0, // có Sider cố định ở desktop
            marginTop: 64, // chiều cao header
            background: "#ffffff",
          }}
        >
          <Content
            style={{
              padding: screens.md ? 34 : 16,
              margin: 0,
              background: "#ffffff",
              borderRadius: screens.md ? 8 : 0,
              minHeight: "calc(100vh - 64px - 70px)",
            }}
          >
            {children}
          </Content>

          <AccountFooter />
        </Layout>
      </Layout>
    </Layout>
  );
}
