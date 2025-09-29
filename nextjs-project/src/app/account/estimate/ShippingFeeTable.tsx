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
    <div style={{ marginTop: 32, textAlign: "center" }}>
      {/* Results section */}
      <div>
        {/* Results table */}
        <div style={{ overflowX: "auto" }}>
          <table
            style={{
              width: "100%",
              backgroundColor: "white",
              border: "1px solid #d9d9d9",
              borderRadius: 8,
              borderCollapse: "separate",
              borderSpacing: 0,
            }}
          >
            <thead>
              <tr style={{ backgroundColor: "#fafafa" }}>
                <th
                  style={{
                    padding: "12px 16px",
                    fontSize: 14,
                    fontWeight: 500,
                    color: "#262626",
                    textAlign: "left",
                    borderBottom: "1px solid #f0f0f0",
                  }}
                >
                  Service Type
                </th>
                <th
                  style={{
                    padding: "12px 16px",
                    fontSize: 14,
                    fontWeight: 500,
                    color: "#262626",
                    textAlign: "center",
                    borderBottom: "1px solid #f0f0f0",
                  }}
                >
                  Multiplier
                </th>
                <th
                  style={{
                    padding: "12px 16px",
                    fontSize: 14,
                    fontWeight: 500,
                    color: "#262626",
                    textAlign: "center",
                    borderBottom: "1px solid #f0f0f0",
                  }}
                >
                  Distance
                </th>
                <th
                  style={{
                    padding: "12px 16px",
                    fontSize: 14,
                    fontWeight: 500,
                    color: "#262626",
                    textAlign: "right",
                    borderBottom: "1px solid #f0f0f0",
                  }}
                >
                  Total Fee
                </th>
              </tr>
            </thead>
            <tbody>
              {services.map((service, i) => (
                <tr
                  key={service.serviceType}
                  style={{
                    backgroundColor:
                      service.serviceType === "STANDARD" ? "#f6ffed" : "white",
                  }}
                >
                  <td
                    style={{
                      padding: "12px 16px",
                      borderBottom:
                        i < services.length - 1 ? "1px solid #f0f0f0" : "none",
                    }}
                  >
                    <div style={{ fontWeight: 500, color: "#262626" }}>
                      {service.serviceName}
                    </div>
                    {service.serviceType === "STANDARD" && (
                      <Text type="success" style={{ fontSize: 12 }}>
                        (Recommended)
                      </Text>
                    )}
                  </td>
                  <td
                    style={{
                      padding: "12px 16px",
                      textAlign: "center",
                      borderBottom:
                        i < services.length - 1 ? "1px solid #f0f0f0" : "none",
                    }}
                  >
                    x{service.multiplier}
                  </td>
                  <td
                    style={{
                      padding: "12px 16px",
                      textAlign: "center",
                      borderBottom:
                        i < services.length - 1 ? "1px solid #f0f0f0" : "none",
                    }}
                  >
                    {service.distanceKm.toFixed(1)} km
                  </td>
                  <td
                    style={{
                      padding: "12px 16px",
                      textAlign: "right",
                      fontWeight: "bold",
                      color: "#52c41a",
                      borderBottom:
                        i < services.length - 1 ? "1px solid #f0f0f0" : "none",
                    }}
                  >
                    {service.totalFee.toLocaleString("vi-VN")}đ
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>

        <div
          style={{
            textAlign: "center",
            fontSize: 12,
            color: "#8c8c8c",
            marginTop: 16,
          }}
        >
          <p style={{ margin: "4px 0" }}>* Prices are for reference only</p>
          <p style={{ margin: "4px 0" }}>
            Official shipping fee will be confirmed when creating the order
          </p>
        </div>
      </div>
    </div>
  );
}
