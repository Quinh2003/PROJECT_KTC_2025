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
  { id: 1, name: "Pending", color: "default" },
  { id: 2, name: "Completed", color: "success" },
  { id: 3, name: "Cancelled", color: "error" },
  { id: 4, name: "Processing", color: "processing" },
  { id: 5, name: "Shipping", color: "warning" },
  { id: 6, name: "Delivered", color: "success" },
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
            placeholder="Search by Order ID..."
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
            placeholder={["From", "To"]}
          />
        </Col>
        <Col xs={24} sm={8} md={6}>
          <Select
            mode="multiple"
            style={{ width: "100%" }}
            placeholder="Filter by status"
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
