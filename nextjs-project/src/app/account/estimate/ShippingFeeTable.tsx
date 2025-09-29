"use client";

import { Typography } from "antd";

const { Text } = Typography;

interface ServiceResult {
  serviceType: string;
  serviceName: string;
  multiplier: number;
  totalFee: number;
  baseFee: number;
  distanceFee: number;
  distanceKm: number;
}

interface Props {
  services: ServiceResult[];
}

export default function ShippingFeeTable({ services }: Props) {
  if (!services || services.length === 0) return null;

  return (
    <div style={{ overflowX: "auto" }}>
      <table
        style={{
          width: "100%",
          background: "white",
          borderRadius: 6,
        }}
      >
        <thead>
          <tr style={{ background: "#f0f0f0" }}>
            <th style={{ padding: 12, fontSize: 14, whiteSpace: "nowrap" }}>
              Loại dịch vụ
            </th>
            <th style={{ padding: 12, fontSize: 14, whiteSpace: "nowrap" }}>
              Hệ số
            </th>
            <th style={{ padding: 12, fontSize: 14, whiteSpace: "nowrap" }}>
              Khoảng cách
            </th>
            <th
              style={{
                padding: 12,
                fontSize: 14,
                textAlign: "right",
                whiteSpace: "nowrap",
              }}
            >
              Tổng phí
            </th>
          </tr>
        </thead>
        <tbody>
          {services.map((service, i) => (
            <tr
              key={service.serviceType}
              style={{
                background:
                  service.serviceType === "STANDARD" ? "#f6ffed" : "white",
                borderBottom:
                  i < services.length - 1 ? "1px solid #f0f0f0" : "none",
              }}
            >
              <td style={{ padding: 12 }}>
                <div style={{ fontWeight: 500 }}>{service.serviceName}</div>
                {service.serviceType === "STANDARD" && (
                  <Text type="success" style={{ fontSize: 12 }}>
                    (Được khuyến nghị)
                  </Text>
                )}
              </td>
              <td style={{ textAlign: "center" }}>x{service.multiplier}</td>
              <td style={{ textAlign: "center" }}>
                {service.distanceKm.toFixed(1)} km
              </td>
              <td
                style={{
                  textAlign: "right",
                  fontWeight: "bold",
                  color:
                    service.serviceType === "STANDARD" ? "#52c41a" : "#1890ff",
                }}
              >
                {service.totalFee.toLocaleString("vi-VN")}đ
              </td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}
