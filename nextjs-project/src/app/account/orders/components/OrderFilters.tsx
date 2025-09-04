import { SearchOutlined } from "@ant-design/icons";
import { Input, DatePicker, Select, Row, Col, Card } from "antd";
import type { Dayjs } from "dayjs";

const { RangePicker } = DatePicker;

interface OrderFiltersProps {
  searchText: string;
  dateRange: [Dayjs | null, Dayjs | null];
  statusFilter: number[];
  onSearchChange: (value: string) => void;
  onDateRangeChange: (dates: [Dayjs | null, Dayjs | null]) => void;
  onStatusFilterChange: (values: number[]) => void;
}

const orderStatuses = [
  { id: 1, name: "Chờ xử lý", color: "default" },
  { id: 2, name: "Đã tiếp nhận", color: "processing" },
  { id: 3, name: "Đang giao hàng", color: "warning" },
  { id: 4, name: "Đã giao hàng", color: "success" },
  { id: 5, name: "Đã huỷ", color: "error" },
];

export const OrderFilters = ({
  searchText,
  dateRange,
  statusFilter,
  onSearchChange,
  onDateRangeChange,
  onStatusFilterChange,
}: OrderFiltersProps) => {
  return (
    <Card size="small" className="order-filters">
      <Row gutter={[16, 16]}>
        <Col xs={24} sm={8} md={6}>
          <Input
            placeholder="Tìm mã đơn hàng..."
            prefix={<SearchOutlined />}
            value={searchText}
            onChange={(e) => onSearchChange(e.target.value)}
          />
        </Col>
        <Col xs={24} sm={8} md={6}>
          <RangePicker
            style={{ width: "100%" }}
            value={dateRange}
            onChange={(dates) =>
              onDateRangeChange(dates as [Dayjs | null, Dayjs | null])
            }
            placeholder={["Từ ngày", "Đến ngày"]}
          />
        </Col>
        <Col xs={24} sm={8} md={6}>
          <Select
            mode="multiple"
            style={{ width: "100%" }}
            placeholder="Lọc theo trạng thái"
            value={statusFilter}
            onChange={onStatusFilterChange}
            options={orderStatuses.map((status) => ({
              label: status.name,
              value: status.id,
            }))}
          />
        </Col>
      </Row>
    </Card>
  );
};
