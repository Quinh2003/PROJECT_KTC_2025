"use client";

import { Menu, Layout, Drawer, Grid } from "antd";
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
    { key: "/account", label: "Trang chủ", icon: <HomeOutlined /> },
    { key: "/account/orders/new", label: "Tạo đơn", icon: <PlusOutlined /> },
    { key: "/account/orders", label: "Đơn hàng", icon: <ShoppingOutlined /> },
    { key: "/account/estimate", label: "Cước phí", icon: <WalletOutlined /> },
    { key: "/account/profile", label: "Cửa hàng", icon: <UserOutlined /> },
  ];

  const renderMenu = (isDesktop: boolean) => (
    <Menu
      mode="inline"
      selectedKeys={[pathname]}
      theme="dark"
      items={menuItems.map((item) => ({
        key: item.key,
        icon: item.icon,
        label: (
          <Link
            href={item.key}
            style={{ color: "inherit", fontSize: isDesktop ? "14px" : "12px" }}
            onClick={() => {
              // Nếu đang ở mobile thì đóng Drawer sau khi chọn menu
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
          background: "#001529",
          position: "fixed",
          height: "100vh",
          left: 0,
          top: 64,
          overflow: "auto",
        }}
      >
        {renderMenu(true)}
      </Sider>
    );
  }

  // Mobile sidebar (Drawer)
  return (
    <Drawer
      open={open}
      onClose={onClose}
      placement="left"
      width={130}
      style={{ top: 64, height: "calc(100%)" }}
      styles={{
        content: { background: "#001529" },
        body: { padding: 0, height: "100%" },
      }}
    >
      {renderMenu(false)}
    </Drawer>
  );
}
