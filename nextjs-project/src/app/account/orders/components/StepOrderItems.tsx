import { Form, Input, InputNumber, Button, Table, Checkbox } from "antd";
import {
  PlusOutlined,
  DeleteOutlined,
  DownloadOutlined,
  UploadOutlined,
} from "@ant-design/icons";
import { useState } from "react";
import { OrderItem } from "@/types/orders";
import { FormInstance } from "antd";
import ExcelUploadModal from "./ExcelUploadModal";

interface Props {
  form: FormInstance;
}

export default function StepOrderItems({ form }: Props) {
  const [isExcelModalOpen, setIsExcelModalOpen] = useState(false);

  const handleDownloadSample = () => {
    // Import XLSX dynamically to avoid SSR issues
    import("xlsx").then((XLSX) => {
      const sampleData = [
        [
          "Tên sản phẩm",
          "Số lượng",
          "Cân nặng (kg)",
          "Chiều cao (cm)",
          "Chiều rộng (cm)",
          "Chiều dài (cm)",
          "Hàng dễ vỡ",
        ],
        ["Sản phẩm mẫu 1", 2, 1.5, 30, 20, 40, "Không"],
        ["Sản phẩm mẫu 2", 1, 0.8, 15, 15, 25, "Có"],
      ];

      // Create a new workbook
      const wb = XLSX.utils.book_new();
      const ws = XLSX.utils.aoa_to_sheet(sampleData);

      // Set column widths
      const colWidths = [
        { wch: 20 }, // Tên sản phẩm
        { wch: 10 }, // Số lượng
        { wch: 12 }, // Cân nặng
        { wch: 12 }, // Chiều cao
        { wch: 12 }, // Chiều rộng
        { wch: 12 }, // Chiều dài
        { wch: 10 }, // Hàng dễ vỡ
      ];
      ws["!cols"] = colWidths;

      // Add the worksheet to the workbook
      XLSX.utils.book_append_sheet(wb, ws, "Mẫu đơn hàng");

      // Generate Excel file
      XLSX.writeFile(wb, "mau_danh_sach_san_pham.xlsx");
    });
  };

  const handleExcelData = (data: OrderItem[]) => {
    const currentItems: OrderItem[] = form.getFieldValue("items") || [];
    const updatedItems = [...currentItems, ...data];
    form.setFieldValue("items", updatedItems);
  };

  return (
    <div>
      {/* Action buttons */}
      <div
        style={{
          marginBottom: 16,
          display: "flex",
          gap: 8,
          flexWrap: "wrap",
          justifyContent: "flex-end",
        }}
      >
        <Button
          type="default"
          icon={<DownloadOutlined />}
          onClick={handleDownloadSample}
        >
          Tải file mẫu
        </Button>
        <Button
          type="primary"
          icon={<UploadOutlined />}
          onClick={() => setIsExcelModalOpen(true)}
        >
          Nhập file Excel
        </Button>
      </div>

      {/* Table with form list */}
      <Form.List name="items">
        {(fields, { add, remove }) => {
          const items: OrderItem[] = form.getFieldValue("items") || [];

          return (
            <div style={{ display: "flex", flexDirection: "column", gap: 16 }}>
              <Table
                dataSource={fields.map((field, index) => ({
                  key: field.key,
                  ...(items[index] || {}),
                }))}
                pagination={false}
                scroll={{ x: 900 }}
                size="small"
                columns={[
                  {
                    title: "Tên sản phẩm",
                    key: "product_name",
                    render: (_, __, index) => (
                      <Form.Item
                        name={[index, "product_name"]}
                        rules={[
                          { required: true, message: "Nhập tên sản phẩm" },
                        ]}
                        style={{ margin: 0 }}
                      >
                        <Input placeholder="Tên sản phẩm" />
                      </Form.Item>
                    ),
                  },
                  {
                    title: "Số lượng",
                    key: "quantity",
                    width: 120,
                    render: (_, __, index) => (
                      <Form.Item
                        name={[index, "quantity"]}
                        rules={[{ required: true, message: "Nhập số lượng" }]}
                        style={{ margin: 0 }}
                      >
                        <InputNumber min={1} style={{ width: "100%" }} />
                      </Form.Item>
                    ),
                  },
                  {
                    title: "Cân nặng (kg)",
                    key: "weight",
                    width: 150,
                    render: (_, __, index) => (
                      <Form.Item
                        name={[index, "weight"]}
                        rules={[{ required: true, message: "Nhập cân nặng" }]}
                        style={{ margin: 0 }}
                      >
                        <InputNumber
                          min={0}
                          step={0.1}
                          style={{ width: "100%" }}
                        />
                      </Form.Item>
                    ),
                  },
                  {
                    title: "Chiều cao (cm)",
                    key: "height",
                    width: 150,
                    render: (_, __, index) => (
                      <Form.Item
                        name={[index, "height"]}
                        rules={[{ required: true, message: "Nhập chiều cao" }]}
                        style={{ margin: 0 }}
                      >
                        <InputNumber min={0} style={{ width: "100%" }} />
                      </Form.Item>
                    ),
                  },
                  {
                    title: "Chiều rộng (cm)",
                    key: "width",
                    width: 150,
                    render: (_, __, index) => (
                      <Form.Item
                        name={[index, "width"]}
                        rules={[{ required: true, message: "Nhập chiều rộng" }]}
                        style={{ margin: 0 }}
                      >
                        <InputNumber min={0} style={{ width: "100%" }} />
                      </Form.Item>
                    ),
                  },
                  {
                    title: "Chiều dài (cm)",
                    key: "length",
                    width: 150,
                    render: (_, __, index) => (
                      <Form.Item
                        name={[index, "length"]}
                        rules={[{ required: true, message: "Nhập chiều dài" }]}
                        style={{ margin: 0 }}
                      >
                        <InputNumber min={0} style={{ width: "100%" }} />
                      </Form.Item>
                    ),
                  },
                  {
                    title: "Hàng dễ vỡ",
                    key: "is_fragile",
                    width: 120,
                    render: (_, __, index) => (
                      <Form.Item
                        name={[index, "is_fragile"]}
                        valuePropName="checked"
                        style={{ margin: 0 }}
                      >
                        <Checkbox>Dễ vỡ</Checkbox>
                      </Form.Item>
                    ),
                  },
                  {
                    title: "",
                    key: "action",
                    width: 60,
                    render: (_, __, index) => (
                      <Button
                        type="text"
                        danger
                        icon={<DeleteOutlined />}
                        onClick={() => remove(index)}
                      />
                    ),
                  },
                ]}
              />

              {/* Add button */}
              <Button
                type="dashed"
                onClick={() => add()}
                icon={<PlusOutlined />}
                block
              >
                Thêm sản phẩm
              </Button>
            </div>
          );
        }}
      </Form.List>

      {/* Modal nhập Excel */}
      <ExcelUploadModal
        open={isExcelModalOpen}
        onClose={() => setIsExcelModalOpen(false)}
        onSaveData={handleExcelData}
      />
    </div>
  );
}
