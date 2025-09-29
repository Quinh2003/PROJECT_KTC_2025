"use client";

import React from "react";
import { Menu, Layout, Drawer, Grid, Typography } from "antd";
import Link from "next/link";
import {
  HomeOutlined,
  ShoppingOutlined,
  WalletOutlined,
  UserOutlined,
  PlusOutlined,
} from "@ant-design/icons";

const { Sider } = Layout;
const { useBreakpoint } = Grid;

interface AccountSidebarProps {
  pathname: string;
  open?: boolean;
  onClose?: () => void;
}

export default function AccountSidebar({
  pathname,
  open,
  onClose,
}: AccountSidebarProps) {
  const screens = useBreakpoint();

  const menuItems = [
    { key: "/account", label: "Home", icon: <HomeOutlined /> },
    {
      key: "/account/orders/new",
      label: "Create Order",
      icon: <PlusOutlined />,
    },
    { key: "/account/orders", label: "Orders", icon: <ShoppingOutlined /> },
    { key: "/account/estimate", label: "Estimates", icon: <WalletOutlined /> },
    {
      key: "/account/profile",
      label: "Store Information",
      icon: <UserOutlined />,
    },
  ];

  const renderMenu = (isDesktop: boolean) => (
    <Menu
      mode="inline"
      selectedKeys={[pathname]}
      style={{
        borderRight: 0,
        background: "#fff",
      }}
      items={menuItems.map((item) => ({
        key: item.key,
        icon: React.cloneElement(item.icon, {
          style: { color: "#15803d" },
        }),
        style: {
          borderRadius: isDesktop ? "8px" : "6px",
          margin: isDesktop ? "4px 8px" : "2px 4px",
        },
        label: (
          <Link
            href={item.key}
            style={{
              color: pathname === item.key ? "#15803d" : "#666",
              textDecoration: "none",
              fontSize: isDesktop ? "14px" : "12px",
              fontWeight: pathname === item.key ? 600 : 400,
            }}
            onClick={() => {
              if (!isDesktop && onClose) {
                onClose();
              }
            }}
          >
            {item.label}
          </Link>
        ),
      }))}
    />
  );

  if (screens.md) {
    // Desktop sidebar
    return (
      <Sider
        width={200}
        style={{
          background: "#fff",
          position: "fixed",
          height: "calc(100vh - 64px)",
          left: 0,
          top: 64,
          overflow: "auto",
          boxShadow: "2px 0 8px rgba(0,0,0,0.1)",
          borderRight: "1px solid #f0f0f0",
        }}
      >
        <div style={{ paddingTop: "16px" }}>{renderMenu(true)}</div>
      </Sider>
    );
  }

  // Mobile sidebar (Drawer)
  return (
    <Drawer
      open={open}
      onClose={onClose}
      placement="left"
      width={180}
      style={{ top: 64, height: "calc(100vh - 64px)" }}
      styles={{
        content: {
          background: "#fff",
          padding: 0,
        },
        body: {
          padding: 0,
          height: "100%",
        },
        header: { display: "none" },
      }}
    >
      <div
        style={{
          padding: "12px 0",
          borderBottom: "1px solid #f0f0f0",
          background: "#fafafa",
        }}
      >
        <Typography.Text
          strong
          style={{
            paddingLeft: "16px",
            color: "#15803d",
            fontSize: "14px",
          }}
        >
          Menu
        </Typography.Text>
      </div>
      <div style={{ paddingTop: "8px" }}>{renderMenu(false)}</div>
    </Drawer>
  );
}
