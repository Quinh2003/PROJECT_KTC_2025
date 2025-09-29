"use client";

import { Typography, Layout, Grid } from "antd";
const { Footer } = Layout;
const { useBreakpoint } = Grid;

export default function AccountFooter() {
  const screens = useBreakpoint();

  return (
    <Footer
      style={{
        textAlign: "center",
        background: "transparent",
        height: screens.md ? "70px" : "50px",
        padding: screens.md ? "24px" : "12px",
      }}
    >
      <Typography.Text
        style={{
          color: "#666",
          fontSize: screens.md ? "14px" : "12px",
        }}
      >
        © 2025 Fast Route Logistics.
        {screens.md && " All rights reserved."}
      </Typography.Text>
    </Footer>
  );
}
