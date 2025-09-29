"use client";

import { useState, useEffect } from "react";
import {
  Card,
  Form,
  Input,
  InputNumber,
  Button,
  Typography,
  Row,
  Col,
  Space,
  Divider,
  Select,
  Checkbox,
} from "antd";
import { CalculatorOutlined, CloseCircleOutlined } from "@ant-design/icons";
import {
  addressService,
  Province,
  District,
  Ward,
} from "@/services/addressService";
import { getCoordinatesFromAddress } from "@/server/geocode.api";
import { Store } from "@/types/Store";
import { storeService } from "@/services/storeService";
import {
  calculateBaseShippingFee,
  calculateShippingFee as calculateShippingFeeUtil,
  SERVICE_MULTIPLIERS,
  ServiceType,
} from "@/utils/shipping";
import { calculateDistanceFee, haversineDistance } from "@/utils/distance";
import { getMapboxRoute } from "@/utils/mapbox";
import ShippingFeeTable from "./ShippingFeeTable";

const { Title, Text } = Typography;
const { TextArea } = Input;

interface EstimateForm {
  pickupAddress: string;
  deliveryAddress: string;
  weight: number;
  distance: number;
  // Thêm các field cho địa chỉ mới
  delivery_city?: string;
  delivery_address_detail?: string;
  delivery_latitude?: number | null;
  delivery_longitude?: number | null;
  // Thêm field cho dịch vụ và hàng dễ vỡ
  service_type?: ServiceType;
  is_fragile?: boolean;
}

const calculateShippingFee = async (
  values: EstimateForm,
  store: Store | null
): Promise<{
  totalFee: number;
  baseFee: number;
  distanceFee: number;
  serviceFeeMultiplier: number;
  distanceKm: number;
  region: string;
}> => {
  // Tạo mock OrderItem từ form values
  const mockOrderItem = {
    product_name: "Sản phẩm ước tính",
    quantity: 1,
    weight: values.weight,
    height: 10, // Giá trị mặc định
    width: 10, // Giá trị mặc định
    length: 10, // Giá trị mặc định
    is_fragile: values.is_fragile || false,
  };

  // Tính phí cơ bản dựa trên sản phẩm (chưa có hệ số dịch vụ)
  const baseFee = calculateBaseShippingFee([mockOrderItem], values.is_fragile);

  let distanceFee = 0;
  let distanceKm = 0;
  let region = "Không xác định";

  // Tính phí khoảng cách nếu có tọa độ
  let storeLatitude = store?.latitude;
  let storeLongitude = store?.longitude;

  // Nếu store chưa có tọa độ, thử geocode địa chỉ store
  if (store?.address && (!storeLatitude || !storeLongitude)) {
    try {
      const storeCoords = await getCoordinatesFromAddress(
        store.address + ", Việt Nam"
      );
      if (storeCoords.latitude && storeCoords.longitude) {
        storeLatitude = storeCoords.latitude;
        storeLongitude = storeCoords.longitude;
      }
    } catch (error) {
      console.warn("Không thể lấy tọa độ cho store:", error);
    }
  }

  if (
    storeLongitude &&
    storeLatitude &&
    values.delivery_latitude &&
    values.delivery_longitude
  ) {
    try {
      // Sử dụng Mapbox để tính khoảng cách thực tế
      const coordinates = await getMapboxRoute(
        storeLongitude,
        storeLatitude,
        values.delivery_longitude,
        values.delivery_latitude
      );

      if (coordinates.length >= 2) {
        // Tính tổng khoảng cách qua các waypoint
        const points: [number, number][] = coordinates.map((coord) => [
          coord[0],
          coord[1],
        ]);
        distanceKm = 0;

        for (let i = 1; i < points.length; i++) {
          distanceKm += haversineDistance(points[i - 1], points[i]);
        }

        // Tính phí theo khoảng cách
        const distanceResult = calculateDistanceFee(distanceKm);
        distanceFee = distanceResult.fee;
        region = distanceResult.region;
      }
    } catch (error) {
      console.warn(
        "Failed to get Mapbox route, using Haversine distance:",
        error
      );

      // Fallback: sử dụng khoảng cách thẳng
      distanceKm = haversineDistance(
        [storeLongitude, storeLatitude],
        [values.delivery_longitude, values.delivery_latitude]
      );

      const distanceResult = calculateDistanceFee(distanceKm);
      distanceFee = distanceResult.fee;
      region = distanceResult.region;
    }
  }

  // Áp dụng hệ số dịch vụ
  const serviceType = values.service_type || "STANDARD";
  const serviceFeeMultiplier =
    SERVICE_MULTIPLIERS[serviceType as ServiceType] || 1.0;

  // Tính tổng phí theo đúng công thức
  const totalFee = Math.round(baseFee * serviceFeeMultiplier + distanceFee);

  return {
    totalFee,
    baseFee,
    distanceFee,
    serviceFeeMultiplier,
    distanceKm,
    region,
  };
};

export default function EstimatePage() {
  const [form] = Form.useForm<EstimateForm>();
  const [store, setStore] = useState<Store | null>(null);
  const [feeDetails, setFeeDetails] = useState<{
    baseFee: number;
    distanceFee: number;
    serviceFeeMultiplier: number;
    distanceKm: number;
    region: string;
    allServices?: {
      serviceType: string;
      serviceName: string;
      multiplier: number;
      totalFee: number;
      baseFee: number;
      distanceFee: number;
      distanceKm: number;
    }[];
  } | null>(null);

  // States cho địa chỉ giao hàng
  const [provinces, setProvinces] = useState<Province[]>([]);
  const [districts, setDistricts] = useState<District[]>([]);
  const [wards, setWards] = useState<Ward[]>([]);
  const [deliveryAddressValue, setDeliveryAddressValue] = useState<string>("");
  const [selectedProvince, setSelectedProvince] = useState<string>("");
  const [selectedDistrict, setSelectedDistrict] = useState<string>("");
  const [selectedWard, setSelectedWard] = useState<string>("");
  const [streetAddress, setStreetAddress] = useState<string>("");
  const [coordinates, setCoordinates] = useState<{
    latitude: number | null;
    longitude: number | null;
  }>({ latitude: null, longitude: null });
  const [isGeocodingLoading, setIsGeocodingLoading] = useState(false);

  useEffect(() => {
    const fetchStore = async () => {
      try {
        const userStr = localStorage.getItem("user");
        if (!userStr) return;

        const user = JSON.parse(userStr);
        const data = await storeService.getStoresByUserId(user.id.toString());

        if (data && data.length > 0) {
          const userStore = data[0];
          setStore(userStore);
          form.setFieldsValue({ pickupAddress: userStore.address || "" });
        }
      } catch (error) {
        console.error("Error loading store:", error);
        // Fallback to default address if store loading fails
        const defaultAddress = "Không thể tải địa chỉ cửa hàng";
        form.setFieldsValue({ pickupAddress: defaultAddress });
      }
    };

    fetchStore();
  }, [form]);

  // Load danh sách tỉnh/thành phố khi component mount
  useEffect(() => {
    const loadProvinces = async () => {
      try {
        const provincesData = await addressService.getProvinces();
        setProvinces(provincesData);
      } catch (error) {
        console.error("Error loading provinces:", error);
      }
    };
    loadProvinces();
  }, []);

  // Load danh sách quận/huyện khi chọn tỉnh
  const handleProvinceChange = async (value: string) => {
    setSelectedProvince(value);
    setSelectedDistrict("");
    setSelectedWard("");
    setDistricts([]);
    setWards([]);

    await updateAddressDisplay(value, "", "", streetAddress);

    try {
      const districtsData = await addressService.getDistricts(value);
      setDistricts(districtsData);
    } catch (error) {
      console.error("Error loading districts:", error);
    }
  };

  // Load danh sách xã/phường khi chọn quận
  const handleDistrictChange = async (value: string) => {
    setSelectedDistrict(value);
    setSelectedWard("");
    setWards([]);

    await updateAddressDisplay(selectedProvince, value, "", streetAddress);

    try {
      const wardsData = await addressService.getWards(value);
      setWards(wardsData);
    } catch (error) {
      console.error("Error loading wards:", error);
    }
  };

  // Xử lý khi chọn xã/phường
  const handleWardChange = async (value: string) => {
    setSelectedWard(value);
    await updateAddressDisplay(
      selectedProvince,
      selectedDistrict,
      value,
      streetAddress
    );
  };

  // Xử lý khi nhập số nhà/đường
  const handleStreetAddressChange = async (
    e: React.ChangeEvent<HTMLInputElement>
  ) => {
    const value = e.target.value;
    setStreetAddress(value);
    await updateAddressDisplay(
      selectedProvince,
      selectedDistrict,
      selectedWard,
      value
    );
  };

  // Cập nhật hiển thị địa chỉ đầy đủ
  const updateAddressDisplay = async (
    provinceCode: string,
    districtCode: string,
    wardCode: string,
    street: string
  ) => {
    const provinceName =
      provinces.find((p) => p.code === provinceCode)?.name || "";
    const districtName =
      districts.find((d) => d.code === districtCode)?.name || "";
    const wardName = wards.find((w) => w.code === wardCode)?.name || "";

    // Địa chỉ lưu backend chỉ gồm số nhà, xã/phường, quận/huyện
    const addressParts = [];
    if (street.trim()) {
      addressParts.push(street.trim());
    }
    if (wardName) {
      addressParts.push(wardName);
    }
    if (districtName) {
      addressParts.push(districtName);
    }
    const addressForBackend = addressParts.join(", ");

    // Địa chỉ hiển thị cho user vẫn gồm cả tỉnh/thành phố
    const displayParts = [...addressParts];
    if (provinceName) {
      displayParts.push(provinceName);
    }
    const displayAddress = displayParts.join(", ");

    setDeliveryAddressValue(displayAddress);

    // Chỉ lấy tọa độ khi có đủ thông tin
    if (provinceName && districtName && displayAddress.trim()) {
      let geocodeAddress = displayAddress;
      if (!displayAddress.toLowerCase().includes(provinceName.toLowerCase())) {
        geocodeAddress += `, ${provinceName}`;
      }
      if (!displayAddress.toLowerCase().includes("việt nam")) {
        geocodeAddress += ", Việt Nam";
      }

      try {
        setIsGeocodingLoading(true);
        const coords = await getCoordinatesFromAddress(geocodeAddress);
        setCoordinates(coords);

        // Cập nhật form
        form.setFieldsValue({
          deliveryAddress: displayAddress,
          delivery_city: provinceName,
          delivery_address_detail: addressForBackend,
          delivery_latitude: coords.latitude,
          delivery_longitude: coords.longitude,
        });
      } catch (error) {
        console.error("Error in geocoding process:", error);
        setCoordinates({ latitude: null, longitude: null });
      } finally {
        setIsGeocodingLoading(false);
      }
    } else {
      setCoordinates({ latitude: null, longitude: null });
      if (addressForBackend && provinceName) {
        form.setFieldsValue({
          deliveryAddress: displayAddress,
          delivery_city: provinceName,
          delivery_address_detail: addressForBackend,
          delivery_latitude: null,
          delivery_longitude: null,
        });
      }
    }
  };

  // Hàm xóa toàn bộ địa chỉ và reset
  const handleClearAddress = () => {
    setSelectedProvince("");
    setSelectedDistrict("");
    setSelectedWard("");
    setStreetAddress("");
    setDeliveryAddressValue("");
    setCoordinates({ latitude: null, longitude: null });
    setDistricts([]);
    setWards([]);

    form.setFieldsValue({
      deliveryAddress: "",
      delivery_city: "",
      delivery_address_detail: "",
      delivery_latitude: null,
      delivery_longitude: null,
    });
  };

  const handleCalculate = async (values: EstimateForm) => {
    try {
      // Merge coordinates từ state vào values
      const valuesWithCoords = {
        ...values,
        delivery_latitude: coordinates.latitude,
        delivery_longitude: coordinates.longitude,
      };

      // Tính toán cho tất cả loại dịch vụ
      const allServiceResults = [];

      for (const [serviceKey, multiplier] of Object.entries(
        SERVICE_MULTIPLIERS
      )) {
        const serviceValues = {
          ...valuesWithCoords,
          service_type: serviceKey as ServiceType,
        };

        const result = await calculateShippingFee(serviceValues, store);
        allServiceResults.push({
          serviceType: serviceKey,
          serviceName: getServiceName(serviceKey as ServiceType),
          multiplier,
          ...result,
        });
      }

      // Set kết quả đầu tiên làm mặc định (STANDARD)
      const standardResult =
        allServiceResults.find((r) => r.serviceType === "STANDARD") ||
        allServiceResults[0];
      setFeeDetails({
        baseFee: standardResult.baseFee,
        distanceFee: standardResult.distanceFee,
        serviceFeeMultiplier: standardResult.serviceFeeMultiplier,
        distanceKm: standardResult.distanceKm,
        region: standardResult.region,
        allServices: allServiceResults,
      });
    } catch (error) {
      console.error("Error calculating shipping fee:", error);
      setFeeDetails(null);
    }
  };

  // Helper function để lấy tên dịch vụ
  const getServiceName = (serviceType: ServiceType): string => {
    switch (serviceType) {
      case "SECOND_CLASS":
        return "Tiết kiệm";
      case "STANDARD":
        return "Tiêu chuẩn";
      case "FIRST_CLASS":
        return "Nhanh";
      case "EXPRESS":
        return "Hỏa tốc";
      case "PRIORITY":
        return "Ưu tiên";
      default:
        return serviceType;
    }
  };

  return (
    <Card>
      <Title level={2}>Ước tính phí vận chuyển</Title>
      <Form
        form={form}
        layout="vertical"
        onFinish={handleCalculate}
        initialValues={{
          weight: 0,
          distance: 0,
          is_fragile: false,
        }}
      >
        <Row gutter={[24, 16]}>
          {/* Địa chỉ lấy hàng - Full width */}
          <Col xs={24}>
            <Form.Item
              name="pickupAddress"
              label="Địa chỉ lấy hàng (Cửa hàng của bạn)"
              rules={[
                { required: true, message: "Vui lòng nhập địa chỉ lấy hàng!" },
              ]}
            >
              <TextArea
                rows={2}
                disabled
                placeholder={
                  store ? "Đang tải địa chỉ cửa hàng..." : "Địa chỉ cửa hàng"
                }
                style={{ backgroundColor: "#f5f5f5" }}
              />
            </Form.Item>
          </Col>

          {/* Địa chỉ giao hàng - Full width với hệ thống chọn địa chỉ */}
          <Col xs={24}>
            <Form.Item label="Địa chỉ giao hàng" required>
              <Input
                placeholder="Địa chỉ sẽ hiển thị sau khi chọn tỉnh/huyện/xã"
                value={deliveryAddressValue}
                readOnly
                style={{
                  marginBottom: 16,
                  borderRadius: 6,
                  backgroundColor: deliveryAddressValue ? "#f5f5f5" : "#fff",
                  cursor: "default",
                }}
                suffix={
                  <CloseCircleOutlined
                    onClick={handleClearAddress}
                    style={{
                      cursor: "pointer",
                      color: "#999",
                      visibility: deliveryAddressValue ? "visible" : "hidden",
                    }}
                  />
                }
              />

              {/* Hiển thị tọa độ nếu có */}
              {isGeocodingLoading && (
                <div
                  style={{
                    fontSize: "12px",
                    color: "#1890ff",
                    marginBottom: 12,
                    padding: "4px 8px",
                    background: "#f0f8ff",
                    borderRadius: 4,
                    border: "1px solid #91d5ff",
                  }}
                >
                  🔄 Đang lấy tọa độ...
                </div>
              )}
              {!isGeocodingLoading &&
                coordinates.latitude &&
                coordinates.longitude && (
                  <div
                    style={{
                      fontSize: "12px",
                      color: "#52c41a",
                      marginBottom: 12,
                      padding: "4px 8px",
                      background: "#f6ffed",
                      borderRadius: 4,
                      border: "1px solid #b7eb8f",
                    }}
                  >
                    📍 Tọa độ: {coordinates.latitude.toFixed(6)},{" "}
                    {coordinates.longitude.toFixed(6)}
                  </div>
                )}
              {!isGeocodingLoading &&
                deliveryAddressValue &&
                !coordinates.latitude && (
                  <div
                    style={{
                      fontSize: "12px",
                      color: "#ff4d4f",
                      marginBottom: 12,
                      padding: "4px 8px",
                      background: "#fff2f0",
                      borderRadius: 4,
                      border: "1px solid #ffccc7",
                    }}
                  >
                    ⚠️ Không tìm thấy tọa độ cho địa chỉ này
                  </div>
                )}

              <Row gutter={[12, 12]}>
                <Col xs={24} sm={12} md={6}>
                  <Select
                    placeholder="Tỉnh/Thành phố"
                    style={{ width: "100%" }}
                    value={selectedProvince || undefined}
                    onChange={handleProvinceChange}
                    showSearch
                    filterOption={(input, option) =>
                      option?.label
                        ?.toString()
                        .toLowerCase()
                        .includes(input.toLowerCase()) ?? false
                    }
                  >
                    {provinces.map((province) => (
                      <Select.Option
                        key={province.code}
                        value={province.code}
                        label={province.name}
                      >
                        {province.name}
                      </Select.Option>
                    ))}
                  </Select>
                </Col>
                <Col xs={24} sm={12} md={6}>
                  <Select
                    placeholder="Huyện/Quận"
                    style={{ width: "100%" }}
                    value={selectedDistrict || undefined}
                    onChange={handleDistrictChange}
                    disabled={!selectedProvince}
                    showSearch
                    filterOption={(input, option) =>
                      option?.label
                        ?.toString()
                        .toLowerCase()
                        .includes(input.toLowerCase()) ?? false
                    }
                  >
                    {districts.map((district) => (
                      <Select.Option
                        key={district.code}
                        value={district.code}
                        label={district.name}
                      >
                        {district.name}
                      </Select.Option>
                    ))}
                  </Select>
                </Col>
                <Col xs={24} sm={12} md={6}>
                  <Select
                    placeholder="Xã/Phường"
                    style={{ width: "100%" }}
                    value={selectedWard || undefined}
                    onChange={handleWardChange}
                    disabled={!selectedDistrict}
                    showSearch
                    filterOption={(input, option) =>
                      option?.label
                        ?.toString()
                        .toLowerCase()
                        .includes(input.toLowerCase()) ?? false
                    }
                  >
                    {wards.map((ward) => (
                      <Select.Option
                        key={ward.code}
                        value={ward.code}
                        label={ward.name}
                      >
                        {ward.name}
                      </Select.Option>
                    ))}
                  </Select>
                </Col>
                <Col xs={24} sm={12} md={6}>
                  <Input
                    placeholder="Đường/Thôn/Xóm/Số nhà"
                    style={{ width: "100%" }}
                    value={streetAddress}
                    onChange={handleStreetAddressChange}
                  />
                </Col>
              </Row>
            </Form.Item>
          </Col>
        </Row>

        <Row gutter={[24, 0]}>
          <Col xs={24} md={12}>
            <Form.Item
              name="weight"
              label="Khối lượng (kg)"
              rules={[{ required: true, message: "Vui lòng nhập khối lượng!" }]}
            >
              <InputNumber
                min={0}
                step={0.1}
                style={{ width: "100%" }}
                placeholder="Nhập khối lượng"
              />
            </Form.Item>
          </Col>

          <Col xs={24} md={12}>
            <Form.Item
              name="is_fragile"
              label="Hàng dễ vỡ"
              valuePropName="checked"
            >
              <Checkbox>Hàng dễ vỡ (phụ phí x1.3)</Checkbox>
            </Form.Item>
          </Col>
        </Row>

        <Divider />

        <Row>
          <Col span={24} style={{ textAlign: "center" }}>
            <Space direction="vertical" size="large">
              <Button
                type="primary"
                htmlType="submit"
                icon={<CalculatorOutlined />}
                size="large"
                disabled={!deliveryAddressValue || !coordinates.latitude}
              >
                Tính phí vận chuyển
              </Button>

              {(!deliveryAddressValue || !coordinates.latitude) && (
                <Text type="secondary" style={{ fontSize: "12px" }}>
                  Vui lòng chọn địa chỉ giao hàng để tính phí chính xác
                </Text>
              )}
            </Space>
          </Col>
        </Row>

        {feeDetails && feeDetails.allServices && (
          <Row style={{ marginTop: 24 }}>
            <Col span={24}>
              <Title
                level={4}
                style={{ textAlign: "center", marginBottom: 20 }}
              >
                Bảng giá các loại dịch vụ vận chuyển
              </Title>
              <ShippingFeeTable services={feeDetails.allServices} />
            </Col>
          </Row>
        )}
      </Form>
    </Card>
  );
}
