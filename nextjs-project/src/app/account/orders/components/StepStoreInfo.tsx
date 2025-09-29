import {
  Row,
  Col,
  Form,
  Input,
  Button,
  Select,
  InputNumber,
  DatePicker,
} from "antd";
import { CloseCircleOutlined } from "@ant-design/icons";
import { Store } from "@/types/Store";
import { useState, useEffect } from "react";
import React from "react";

import {
  addressService,
  Province,
  District,
  Ward,
} from "@/services/addressService";
import { getCoordinatesFromAddress } from "@/server/geocode.api";

interface Props {
  store: Store | null;
}

export default function StepStoreInfo({ store }: Props) {
  const form = Form.useFormInstance();

  // States for address
  const [provinces, setProvinces] = useState<Province[]>([]);
  const [districts, setDistricts] = useState<District[]>([]);
  const [wards, setWards] = useState<Ward[]>([]);
  const [addressValue, setAddressValue] = useState<string>(
    () => form.getFieldValue("shipping_address") || ""
  );
  const [selectedProvince, setSelectedProvince] = useState<string>(
    () => form.getFieldValue("provinceCode") || ""
  );
  const [selectedDistrict, setSelectedDistrict] = useState<string>(
    () => form.getFieldValue("districtCode") || ""
  );
  const [selectedWard, setSelectedWard] = useState<string>(
    () => form.getFieldValue("wardCode") || ""
  );
  const [streetAddress, setStreetAddress] = useState<string>(
    () => form.getFieldValue("streetAddress") || ""
  );
  const [coordinates, setCoordinates] = useState<{
    latitude: number | null;
    longitude: number | null;
  }>(() => ({
    latitude: form.getFieldValue("latitude") ?? null,
    longitude: form.getFieldValue("longitude") ?? null,
  }));
  const [isGeocodingLoading, setIsGeocodingLoading] = useState(false);

  // Validation function
  const validateAddressData = () => {
    const values = form.getFieldsValue();
    console.log("Validating address data:", values);

    if (!values.address || values.address.trim() === "") {
      console.error("Missing address field");
      return false;
    }
    if (!values.city || values.city.trim() === "") {
      console.error("Missing city field");
      return false;
    }
    if (!values.receiver_name || values.receiver_name.trim() === "") {
      console.error("Missing receiver_name field");
      return false;
    }
    if (!values.receiver_phone || values.receiver_phone.trim() === "") {
      console.error("Missing receiver_phone field");
      return false;
    }
    return true;
  };

  React.useImperativeHandle(React.createRef(), () => ({
    validateAddressData,
  }));

  useEffect(() => {
    const shipping_address = form.getFieldValue("shipping_address") || "";
    if (shipping_address && !addressValue) setAddressValue(shipping_address);
    const provinceCode = form.getFieldValue("provinceCode") || "";
    if (provinceCode && !selectedProvince) setSelectedProvince(provinceCode);
    const districtCode = form.getFieldValue("districtCode") || "";
    if (districtCode && !selectedDistrict) setSelectedDistrict(districtCode);
    const wardCode = form.getFieldValue("wardCode") || "";
    if (wardCode && !selectedWard) setSelectedWard(wardCode);
    const street = form.getFieldValue("streetAddress") || "";
    if (street && !streetAddress) setStreetAddress(street);
    const lat = form.getFieldValue("latitude");
    const lng = form.getFieldValue("longitude");
    if ((lat || lng) && (!coordinates.latitude || !coordinates.longitude)) {
      setCoordinates({ latitude: lat ?? null, longitude: lng ?? null });
    }
  }, []);

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

    setAddressValue(displayAddress);

    if (provinceName && districtName && displayAddress.trim()) {
      let geocodeAddress = displayAddress;
      if (!displayAddress.toLowerCase().includes(provinceName.toLowerCase())) {
        geocodeAddress += `, ${provinceName}`;
      }
      if (!displayAddress.toLowerCase().includes("vietnam")) {
        geocodeAddress += ", Vietnam";
      }
      try {
        const coords = await getCoordinatesFromAddress(geocodeAddress);
        setCoordinates(coords);
        const formValues = {
          shipping_address: displayAddress,
          city: provinceName,
          address: addressForBackend,
          latitude: coords.latitude,
          longitude: coords.longitude,
        };
        form.setFieldsValue(formValues);
      } catch (error) {
        console.error("Error in geocoding process:", error);
        setCoordinates({ latitude: null, longitude: null });
      }
    } else {
      setCoordinates({ latitude: null, longitude: null });
      if (addressForBackend && provinceName) {
        form.setFieldsValue({
          shipping_address: displayAddress,
          city: provinceName,
          address: addressForBackend,
          latitude: null,
          longitude: null,
        });
      }
    }
  };

  const handleClearAddress = () => {
    setSelectedProvince("");
    setSelectedDistrict("");
    setSelectedWard("");
    setStreetAddress("");
    setAddressValue("");
    setCoordinates({ latitude: null, longitude: null });
    setIsGeocodingLoading(false);
    setDistricts([]);
    setWards([]);

    form.setFieldsValue({
      shipping_address: "",
      city: "",
      address: "",
      latitude: null,
      longitude: null,
    });
  };

  return (
    <>
      {/* Hidden fields for backend */}
      <Form.Item name="city" style={{ display: "none" }}>
        <Input />
      </Form.Item>
      <Form.Item name="address" style={{ display: "none" }}>
        <Input />
      </Form.Item>
      <Form.Item name="latitude" style={{ display: "none" }}>
        <InputNumber style={{ width: "100%" }} />
      </Form.Item>
      <Form.Item name="longitude" style={{ display: "none" }}>
        <InputNumber style={{ width: "100%" }} />
      </Form.Item>

      {/* Pickup date & time */}
      <Row gutter={[24, 16]}>
        <Col xs={24} lg={12}>
          <Form.Item
            name="pickup_date"
            label="Pickup Date"
            rules={[{ required: true, message: "Please select pickup date!" }]}
          >
            <DatePicker
              style={{ width: "100%" }}
              format="DD/MM/YYYY"
              placeholder="Select pickup date"
              disabledDate={(current) =>
                current && current.valueOf() < Date.now() - 24 * 60 * 60 * 1000
              }
            />
          </Form.Item>
        </Col>
        <Col xs={24} lg={12}>
          <Form.Item
            name="pickup_time_period"
            label="Pickup Time Period"
            rules={[{ required: true, message: "Please select time period!" }]}
          >
            <Select placeholder="Select pickup time">
              <Select.Option value="morning">
                Morning (7:30 - 11:00)
              </Select.Option>
              <Select.Option value="afternoon">
                Afternoon (14:00 - 17:00)
              </Select.Option>
              <Select.Option value="evening">
                Evening (18:00 - 21:00)
              </Select.Option>
            </Select>
          </Form.Item>
        </Col>
      </Row>

      <Row gutter={[24, 16]}>
        <Col xs={24}>
          <Form.Item label="Store Address">
            <Input
              value={store?.address || "Loading..."}
              disabled
              placeholder="Store Address"
              style={{ backgroundColor: "#f5f5f5" }}
            />
          </Form.Item>
        </Col>

        <Col xs={24} lg={12}>
          <Form.Item
            name="receiver_name"
            label="Recipient Name"
            rules={[{ required: true, message: "Please enter recipient name!" }]}
          >
            <Input placeholder="Enter recipient name" />
          </Form.Item>
        </Col>
        <Col xs={24} lg={12}>
          <Form.Item
            name="receiver_phone"
            label="Phone Number"
            rules={[
              { required: true, message: "Please enter phone number!" },
              { pattern: /^[0-9]{10,11}$/, message: "Invalid phone number!" },
            ]}
          >
            <Input placeholder="Enter phone number" />
          </Form.Item>
        </Col>
        <Col xs={24} lg={12}>
          <Form.Item
            name="receiver_email"
            label="Recipient Email"
            rules={[{ type: "email", message: "Invalid email!" }]}
          >
            <Input placeholder="Enter email (optional)" />
          </Form.Item>
        </Col>
        <Col xs={24} lg={12}>
          <Form.Item
            name="addressType"
            label="Delivery Address Type"
            initialValue="DELIVERY"
            rules={[{ required: true, message: "Please select address type!" }]}
          >
            <Select placeholder="Select address type">
              <Select.Option value="HOME">Home</Select.Option>
              <Select.Option value="STORE">Store</Select.Option>
              <Select.Option value="OFFICE">Office</Select.Option>
            </Select>
          </Form.Item>
        </Col>

        <Col xs={24}>
          <Form.Item label="Delivery Address" required>
            <Input
              placeholder="Address will be displayed after selecting province/district/ward"
              value={addressValue}
              readOnly
              style={{
                marginBottom: 16,
                // borderRadius: 6,
                // backgroundColor: addressValue ? "#f5f5f5" : "#fff",
                cursor: "default",
              }}
              suffix={
                <CloseCircleOutlined
                  onClick={handleClearAddress}
                  style={{
                    cursor: "pointer",
                    color: "#999",
                    visibility: addressValue ? "visible" : "hidden",
                    opacity: addressValue ? 1 : 0,
                    transition: "opacity 0.2s",
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
                🔄 Fetching coordinates...
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
            {!isGeocodingLoading && addressValue && !coordinates.latitude && (
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
                  placeholder="Street/House number"
                  style={{ width: "100%" }}
                  value={streetAddress}
                  onChange={handleStreetAddressChange}
                />
              </Col>
            </Row>
          </Form.Item>
        </Col>

        <Col xs={24} lg={12}>
          <Form.Item name="description" label="Order Description">
            <Input.TextArea rows={4} placeholder="Detailed description (optional)" />
          </Form.Item>
        </Col>
        <Col xs={24} lg={12}>
          <Form.Item name="notes" label="Notes">
            <Input.TextArea rows={4} placeholder="Additional notes (if any)" />
          </Form.Item>
        </Col>
      </Row>
    </>
  );
}
