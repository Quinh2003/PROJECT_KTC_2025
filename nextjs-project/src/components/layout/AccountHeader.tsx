"use client";

import { Button, Typography, Space, Grid } from "antd";
import Link from "next/link";
import { LogoutOutlined, MenuOutlined } from "@ant-design/icons";

const { useBreakpoint } = Grid;

interface AccountHeaderProps {
  userName: string | undefined;
  onLogout: () => void;
  onMenuToggle?: () => void; // để mở Sidebar (Drawer) ở mobile
}

export default function AccountHeader({ userName, onLogout, onMenuToggle }: AccountHeaderProps) {
  const screens = useBreakpoint();

  return (
    <header
      style={{
        background: "#001529",
        padding: "0 16px",
        display: "flex",
        alignItems: "center",
        justifyContent: "space-between",
        position: "fixed",
        zIndex: 1000,
        width: "100%",
        left: 0,
        top: 0,
        height: 64,
      }}
    >
      {/* Logo + Tagline */}
      <div style={{ display: "flex", alignItems: "center", gap: "12px" }}>
        {/* Hamburger menu chỉ hiện ở mobile */}
        {!screens.md && (
          <Button
            type="text"
            icon={<MenuOutlined />}
            style={{ color: "#fff" }}
            onClick={onMenuToggle}
          />
        )}

        <Typography.Title level={4} style={{ margin: 0 }}>
          <Link href="/account" style={{ color: "#fff" }}>
            FastRoute Logistics
          </Link>
        </Typography.Title>

        {/* Chỉ hiển thị tag "Khách hàng" ở desktop */}
        {screens.md && (
          <Button
            type="text"
            size="small"
            style={{ background: "rgba(255,255,255,0.1)", color: "#fff" }}
          >
            Khách hàng
          </Button>
        )}
      </div>

      {/* User info + Logout */}
      {screens.md ? (
        // Desktop: hiện cả tên user + nút logout
        <Space>
          <Typography.Text style={{ color: "#fff" }}>
            Xin chào,{" "}
            <Typography.Text strong style={{ color: "#fff" }}>
              {userName}
            </Typography.Text>
          </Typography.Text>
          <Button
            onClick={onLogout}
            icon={<LogoutOutlined />}
            type="default"
            ghost
            style={{ borderColor: "#fff", color: "#fff" }}
          >
            Đăng xuất
          </Button>
        </Space>
      ) : (
        // Mobile: chỉ hiện nút logout icon
        <Button
          onClick={onLogout}
          icon={<LogoutOutlined />}
          type="text"
          style={{ color: "#fff" }}
        />
      )}
    </header>
  );
}
