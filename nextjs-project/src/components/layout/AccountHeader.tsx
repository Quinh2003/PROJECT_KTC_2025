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

export default function AccountHeader({
  userName,
  onLogout,
  onMenuToggle,
}: AccountHeaderProps) {
  const screens = useBreakpoint();

  return (
    <header
      style={{
        position: "fixed",
        zIndex: 1000,
        width: "100%",
        left: 0,
        top: 0,
        boxShadow: "0 2px 8px rgba(0,0,0,0.1)",
      }}
    >
      {/* Top Header - Green background như public */}
      <div
        style={{
          background: "#15803d", // green-700
          padding: "0 16px",
          display: "flex",
          alignItems: "center",
          justifyContent: "space-between",
          height: 64,
        }}
      >
        {/* Logo + Hamburger */}
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
            <Link
              href="/account"
              style={{ color: "#fff", textDecoration: "none" }}
            >
              FastRoute Logistics
            </Link>
          </Typography.Title>

          {/* Tag Customer ở desktop */}
          {screens.md && (
            <Button
              type="text"
              size="small"
              style={{
                background: "rgba(255,255,255,0.2)",
                color: "#fff",
                borderRadius: "6px",
                fontSize: "12px",
              }}
            >
              Customer Portal
            </Button>
          )}
        </div>

        {/* User info + Logout */}
        {screens.md ? (
          // Desktop: hiện cả tên user + nút logout
          <Space>
            <Typography.Text style={{ color: "#fff" }}>
              Hello,{" "}
              <Typography.Text strong style={{ color: "#fff" }}>
                {userName}
              </Typography.Text>
            </Typography.Text>
            <Button
              onClick={onLogout}
              icon={<LogoutOutlined />}
              style={{
                background: "#fff",
                color: "#c9184a",
                border: "none",
                borderRadius: "6px",
              }}
            >
              Logout
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
      </div>
    </header>
  );
}
