"use client";

import {
  Modal,
  Button,
  Upload,
  Typography,
  Divider,
  Table,
  message,
} from "antd";
import {
  CloudUploadOutlined,
  CloseOutlined,
  DeleteOutlined,
} from "@ant-design/icons";
import { useState } from "react";
import type { UploadProps } from "antd";
import { OrderItem } from "@/types/orders";
import * as XLSX from "xlsx";

const { Title } = Typography;

interface Props {
  open: boolean;
  onClose: () => void;
  onSaveData: (data: OrderItem[]) => void;
}

export default function ExcelUploadModal({ open, onClose, onSaveData }: Props) {
  const [uploadedFile, setUploadedFile] = useState<File | null>(null);
  const [excelData, setExcelData] = useState<OrderItem[]>([]);

  const handleDownloadSample = () => {
    const sampleData = [
      [
        "Product Name",
        "Quantity",
        "Weight (kg)",
        "Height (cm)",
        "Width (cm)",
        "Length (cm)",
      ],
      ["Sample Product 1", 2, 1.5, 30, 20, 40],
      ["Sample Product 2", 1, 0.8, 15, 15, 25],
    ];
    const wb = XLSX.utils.book_new();
    const ws = XLSX.utils.aoa_to_sheet(sampleData);
    XLSX.utils.book_append_sheet(wb, ws, "Order Items");
    XLSX.writeFile(wb, "sample_order_items.xlsx");
  };

  const processExcelFile = (file: File) => {
    const reader = new FileReader();
    reader.onload = (e) => {
      try {
        const data = e.target?.result;
        const workbook = XLSX.read(data, { type: "binary" });
        const worksheet = workbook.Sheets[workbook.SheetNames[0]];
        const jsonData = XLSX.utils.sheet_to_json(worksheet);

        const orderItems: OrderItem[] = (
          jsonData as Record<string, unknown>[]
        ).map((row) => ({
          product_name: String(
            row["Product Name"] || row["product_name"] || ""
          ),
          quantity: Number(row["Quantity"] || row["quantity"]) || 1,
          weight: Number(row["Weight (kg)"] || row["weight"]) || 0,
          height: Number(row["Height (cm)"] || row["height"]) || 0,
          width: Number(row["Width (cm)"] || row["width"]) || 0,
          length: Number(row["Length (cm)"] || row["length"]) || 0,
        }));

        setExcelData(orderItems);
        message.success(
          `Successfully read ${orderItems.length} products from file`
        );
      } catch (error) {
        console.error("Error reading Excel file:", error);
        message.error("Unable to read Excel file, please check the format.");
      }
    };
    reader.readAsBinaryString(file);
  };

  const uploadProps: UploadProps = {
    name: "file",
    multiple: false,
    accept: ".xlsx,.xls",
    disabled: uploadedFile !== null,
    beforeUpload: (file) => {
      if (!file.name.endsWith(".xlsx") && !file.name.endsWith(".xls")) {
        message.error("Only Excel files are supported (.xlsx, .xls)");
        return Upload.LIST_IGNORE;
      }
      setUploadedFile(file);
      processExcelFile(file);
      return false;
    },
    onRemove: () => {
      setUploadedFile(null);
      setExcelData([]);
    },
  };

  const handleSaveData = () => {
    if (excelData.length > 0) {
      onSaveData(excelData);
      handleCloseModal();
    }
  };

  const handleCloseModal = () => {
    setUploadedFile(null);
    setExcelData([]);
    onClose();
  };

  const tableColumns = [
    {
      title: "Product Name",
      dataIndex: "product_name",
      key: "product_name",
      width: 200,
    },
    { title: "Quantity", dataIndex: "quantity", key: "quantity", width: 80 },
    { title: "Weight (kg)", dataIndex: "weight", key: "weight", width: 100 },
    { title: "Height (cm)", dataIndex: "height", key: "height", width: 100 },
    { title: "Width (cm)", dataIndex: "width", key: "width", width: 100 },
    { title: "Length (cm)", dataIndex: "length", key: "length", width: 100 },
  ];

  return (
    <Modal
      open={open}
      onCancel={handleCloseModal}
      width="100%"
      style={{ maxWidth: 650 }}
      footer={null}
      closeIcon={<CloseOutlined />}
      centered
    >
      <div style={{ padding: "20px 0" }}>
        <Title level={3} style={{ marginBottom: 8, textAlign: "center" }}>
          Upload Excel
        </Title>

        <div style={{ textAlign: "center", marginBottom: 24 }}>
          <Button onClick={handleDownloadSample}>Download Sample Excel</Button>
        </div>

        <Upload.Dragger
          {...uploadProps}
          style={{
            backgroundColor: uploadedFile ? "#f6f6f6" : "#fafafa",
            border: "2px dashed #d9d9d9",
            borderRadius: 8,
            padding: "40px 20px",
            marginBottom: 24,
            opacity: uploadedFile ? 0.5 : 1,
          }}
        >
          <p className="ant-upload-drag-icon">
            <CloudUploadOutlined style={{ fontSize: 48, color: "#bfbfbf" }} />
          </p>
          <p style={{ fontSize: 16, color: "#8c8c8c" }}>
            <strong>Click to upload</strong> or drag and drop files
          </p>
          <p style={{ fontSize: 14, color: "#bfbfbf" }}>
            Only Excel files are supported (.xlsx, .xls){" "}
          </p>
        </Upload.Dragger>

        {uploadedFile && (
          <div
            style={{
              marginBottom: 16,
              display: "flex",
              alignItems: "center",
              justifyContent: "space-between",
              padding: "8px 12px",
              backgroundColor: "#f0f0f0",
              borderRadius: 6,
              border: "1px solid #d9d9d9",
            }}
          >
            <span style={{ fontSize: 14 }}>📎 {uploadedFile.name}</span>
            <Button
              type="text"
              size="small"
              icon={<DeleteOutlined />}
              onClick={() => {
                setUploadedFile(null);
                setExcelData([]);
              }}
              style={{ color: "#ff4d4f" }}
            />
          </div>
        )}

        {excelData.length > 0 && (
          <div style={{ marginBottom: 24 }}>
            <Divider orientation="left">Preview Data</Divider>
            <div style={{ overflowX: "auto" }}>
              <Table
                dataSource={excelData.map((item, index) => ({
                  ...item,
                  key: index,
                }))}
                columns={tableColumns}
                pagination={false}
                scroll={{ y: 300, x: 600 }}
                size="small"
              />
            </div>
          </div>
        )}

        <div style={{ display: "flex", justifyContent: "flex-end", gap: 12 }}>
          <Button onClick={handleCloseModal}>Cancel</Button>
          <Button
            type="primary"
            onClick={handleSaveData}
            disabled={excelData.length === 0}
          >
            Save Data
          </Button>
        </div>
      </div>
    </Modal>
  );
}
