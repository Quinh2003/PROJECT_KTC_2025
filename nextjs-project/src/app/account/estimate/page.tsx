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
  delivery_city?: string;
  delivery_address_detail?: string;
  delivery_latitude?: number | null;
  delivery_longitude?: number | null;
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
  const mockOrderItem = {
    product_name: "Estimated Product",
    quantity: 1,
    weight: values.weight,
    height: 10,
    width: 10,
    length: 10,
    is_fragile: values.is_fragile || false,
  };

  const baseFee = calculateBaseShippingFee([mockOrderItem], values.is_fragile);

  let distanceFee = 0;
  let distanceKm = 0;
  let region = "Unknown";

  let storeLatitude = store?.latitude;
  let storeLongitude = store?.longitude;

  if (store?.address && (!storeLatitude || !storeLongitude)) {
    try {
      const storeCoords = await getCoordinatesFromAddress(
        store.address + ", Vietnam"
      );
      if (storeCoords.latitude && storeCoords.longitude) {
        storeLatitude = storeCoords.latitude;
        storeLongitude = storeCoords.longitude;
      }
    } catch (error) {
      console.warn("Could not get store coordinates:", error);
    }
  }

  if (
    storeLongitude &&
    storeLatitude &&
    values.delivery_latitude &&
    values.delivery_longitude
  ) {
    try {
      const coordinates = await getMapboxRoute(
        storeLongitude,
        storeLatitude,
        values.delivery_longitude,
        values.delivery_latitude
      );

      if (coordinates.length >= 2) {
        const points: [number, number][] = coordinates.map((coord) => [
          coord[0],
          coord[1],
        ]);
        distanceKm = 0;

        for (let i = 1; i < points.length; i++) {
          distanceKm += haversineDistance(points[i - 1], points[i]);
        }

        const distanceResult = calculateDistanceFee(distanceKm);
        distanceFee = distanceResult.fee;
        region = distanceResult.region;
      }
    } catch (error) {
      console.warn(
        "Failed to get Mapbox route, using Haversine distance:",
        error
      );

      distanceKm = haversineDistance(
        [storeLongitude, storeLatitude],
        [values.delivery_longitude, values.delivery_latitude]
      );

      const distanceResult = calculateDistanceFee(distanceKm);
      distanceFee = distanceResult.fee;
      region = distanceResult.region;
    }
  }

  const serviceType = values.service_type || "STANDARD";
  const serviceFeeMultiplier =
    SERVICE_MULTIPLIERS[serviceType as ServiceType] || 1.0;

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
        const defaultAddress = "Unable to load store address";
        form.setFieldsValue({ pickupAddress: defaultAddress });
      }
    };

    fetchStore();
  }, [form]);

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

  const handleWardChange = async (value: string) => {
    setSelectedWard(value);
    await updateAddressDisplay(
      selectedProvince,
      selectedDistrict,
      value,
      streetAddress
    );
  };

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

    const displayParts = [...addressParts];
    if (provinceName) {
      displayParts.push(provinceName);
    }
    const displayAddress = displayParts.join(", ");

    setDeliveryAddressValue(displayAddress);

    if (provinceName && districtName && displayAddress.trim()) {
      let geocodeAddress = displayAddress;
      if (!displayAddress.toLowerCase().includes(provinceName.toLowerCase())) {
        geocodeAddress += `, ${provinceName}`;
      }
      if (!displayAddress.toLowerCase().includes("vietnam")) {
        geocodeAddress += ", Vietnam";
      }

      try {
        setIsGeocodingLoading(true);
        const coords = await getCoordinatesFromAddress(geocodeAddress);
        setCoordinates(coords);

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
      const valuesWithCoords = {
        ...values,
        delivery_latitude: coordinates.latitude,
        delivery_longitude: coordinates.longitude,
      };

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

  const getServiceName = (serviceType: ServiceType): string => {
    switch (serviceType) {
      case "SECOND_CLASS":
        return "Second Class";
      case "STANDARD":
        return "Standard";
      case "FIRST_CLASS":
        return "First Class";
      case "EXPRESS":
        return "Express";
      case "PRIORITY":
        return "Priority";
      default:
        return serviceType;
    }
  };

  return (
    <div style={{ maxWidth: "100%", padding: "24px" }}>
      <Title
        level={2}
        style={{
          textAlign: "left",
            color: "#15803d",
            marginBottom: 24,
            fontSize: "clamp(1.5rem, 4vw, 2rem)",
          }}
        >
          Shipping Fee Estimation
        </Title>
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
            <Col xs={24}>
              <Form.Item
                name="pickupAddress"
                label="Pickup Address (Your Store)"
                rules={[
                  {
                    required: true,
                    message: "Please enter the pickup address!",
                  },
                ]}
              >
                <TextArea
                  rows={2}
                  disabled
                  placeholder={
                    store ? "Loading store address..." : "Store address"
                  }
                  style={{ backgroundColor: "#f5f5f5" }}
                />
              </Form.Item>
            </Col>

            <Col xs={24}>
              <Form.Item label="Delivery Address" required>
                <Input
                  placeholder="The address will be displayed after selecting province/district/ward"
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
                    🔄 Retrieving coordinates...
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
                      📍 Coordinates: {coordinates.latitude.toFixed(6)},{" "}
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
                      ⚠️ Could not find coordinates for this address
                    </div>
                  )}

                <Row gutter={[12, 12]}>
                  <Col xs={24} sm={12} md={6}>
                    <Select
                      placeholder="Province/City"
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
                      placeholder="District"
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
                      placeholder="Ward/Commune"
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
                      placeholder="Street/Village/House number"
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
                label="Weight (kg)"
                rules={[
                  { required: true, message: "Please enter the weight!" },
                ]}
              >
                <InputNumber
                  min={0}
                  step={0.1}
                  style={{ width: "100%" }}
                  placeholder="Enter weight"
                />
              </Form.Item>
            </Col>

            <Col xs={24} md={12}>
              <Form.Item
                name="is_fragile"
                label="Fragile Item"
                valuePropName="checked"
              >
                <Checkbox>Fragile (extra fee x1.3)</Checkbox>
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
                  style={{
                    backgroundColor: "#15803d",
                    borderColor: "#15803d",
                    borderRadius: 8,
                    height: 48,
                    fontSize: 16,
                    fontWeight: 600,
                    paddingLeft: 32,
                    paddingRight: 32,
                    minWidth: 200,
                  }}
                >
                  Calculate Shipping Fee
                </Button>

                {(!deliveryAddressValue || !coordinates.latitude) && (
                  <Text type="secondary" style={{ fontSize: "12px" }}>
                    Please select a delivery address to calculate the fee
                    accurately
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
                  Delivery Services Price Table
                </Title>
                <ShippingFeeTable services={feeDetails.allServices} />
              </Col>
            </Row>
          )}
        </Form>
      
    </div>
  );
}
