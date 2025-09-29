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

  // States cho địa chỉ
  const [provinces, setProvinces] = useState<Province[]>([]);
  const [districts, setDistricts] = useState<District[]>([]);
  const [wards, setWards] = useState<Ward[]>([]);
  // Khởi tạo state từ form nếu có dữ liệu, nếu không thì để rỗng
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
  // TODO: Uncomment khi cần dùng geocoding
  const [coordinates, setCoordinates] = useState<{
    latitude: number | null;
    longitude: number | null;
  }>(() => ({
    latitude: form.getFieldValue("latitude") ?? null,
    longitude: form.getFieldValue("longitude") ?? null,
  })); // State cho tọa độ
  const [isGeocodingLoading, setIsGeocodingLoading] = useState(false); // State cho loading geocoding

  // Validation function để kiểm tra form có đầy đủ dữ liệu không
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

  // Expose validation function to parent
  React.useImperativeHandle(React.createRef(), () => ({
    validateAddressData,
  }));

  // Khi component mount, nếu form đã có dữ liệu thì khôi phục lại các state địa chỉ
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

  // Hàm getCoordinatesFromAddress đã được tách ra thành API riêng ở server/geocode.api.ts
  // Sử dụng: import { getCoordinatesFromAddress } from "@/server/geocode.api";

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

    // Cập nhật địa chỉ ngay khi chọn tỉnh
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

    // Cập nhật địa chỉ khi chọn huyện
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
    console.log("updateAddressDisplay called with:", {
      provinceCode,
      districtCode,
      wardCode,
      street,
    });

    const provinceName =
      provinces.find((p) => p.code === provinceCode)?.name || "";
    const districtName =
      districts.find((d) => d.code === districtCode)?.name || "";
    const wardName = wards.find((w) => w.code === wardCode)?.name || "";

    console.log("Found names:", { provinceName, districtName, wardName });

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
    let displayParts = [...addressParts];
    if (provinceName) {
      displayParts.push(provinceName);
    }
    const displayAddress = displayParts.join(", ");

    setAddressValue(displayAddress);

    // Chỉ lấy tọa độ khi có đủ thông tin: ít nhất phải có tỉnh và quận
    if (provinceName && districtName && displayAddress.trim()) {
      // Luôn nối tỉnh/thành phố và 'Việt Nam' vào địa chỉ để tăng độ chính xác
      let geocodeAddress = displayAddress;
      if (!displayAddress.toLowerCase().includes(provinceName.toLowerCase())) {
        geocodeAddress += `, ${provinceName}`;
      }
      if (!displayAddress.toLowerCase().includes("việt nam")) {
        geocodeAddress += ", Việt Nam";
      }
      console.log("Getting coordinates for:", geocodeAddress);
      try {
        const coords = await getCoordinatesFromAddress(geocodeAddress);
        setCoordinates(coords);
        console.log("Coordinates received:", coords);
        // Cập nhật form với cả địa chỉ và tọa độ
        const formValues = {
          shipping_address: displayAddress, // Hiển thị cho user
          city: provinceName, // Lưu tỉnh/thành phố riêng biệt cho backend
          address: addressForBackend, // Lưu địa chỉ chỉ gồm 3 trường
          latitude: coords.latitude, // Thêm latitude
          longitude: coords.longitude, // Thêm longitude
        };
        form.setFieldsValue(formValues);
        console.log("Form values set with coordinates:", formValues);
        // Kiểm tra lại form values sau khi set
        setTimeout(() => {
          const currentFormValues = form.getFieldsValue();
          console.log("Current form values after set:", currentFormValues);
        }, 100);
      } catch (error) {
        console.error("Error in geocoding process:", error);
        setCoordinates({ latitude: null, longitude: null });
      }
    } else {
      // Reset tọa độ nếu không có đủ thông tin
      console.log("Not enough address info for geocoding:", {
        provinceName,
        districtName,
        displayAddress,
      });
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

  // Hàm xóa toàn bộ địa chỉ và reset form
  const handleClearAddress = () => {
    setSelectedProvince("");
    setSelectedDistrict("");
    setSelectedWard("");
    setStreetAddress("");
    setAddressValue("");
    setCoordinates({ latitude: null, longitude: null }); // Reset tọa độ
    setIsGeocodingLoading(false); // Reset loading state
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

  // Lấy tên tỉnh/thành phố từ mã
  const getProvinceName = (provinceCode: string) => {
    return provinces.find((p) => p.code === provinceCode)?.name || "";
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

      {/* Trường ngày lấy hàng và thời gian buổi lấy hàng */}
      <Row gutter={[24, 16]}>
        <Col xs={24} lg={12}>
          <Form.Item
            name="pickup_date"
            label="Ngày lấy hàng"
            rules={[
              { required: true, message: "Vui lòng chọn ngày lấy hàng!" },
            ]}
          >
            <DatePicker
              style={{ width: "100%" }}
              format="DD/MM/YYYY"
              placeholder="Chọn ngày lấy hàng"
              disabledDate={(current) =>
                current && current.valueOf() < Date.now() - 24 * 60 * 60 * 1000
              }
            />
          </Form.Item>
        </Col>
        <Col xs={24} lg={12}>
          <Form.Item
            name="pickup_time_period"
            label="Thời gian buổi lấy hàng"
            rules={[
              {
                required: true,
                message: "Vui lòng chọn thời gian buổi lấy hàng!",
              },
            ]}
          >
            <Select placeholder="Chọn buổi lấy hàng">
              <Select.Option value="morning">Sáng (7h30 - 11h00)</Select.Option>
              <Select.Option value="afternoon">
                Chiều (14h00 - 17h00)
              </Select.Option>
              <Select.Option value="evening">Tối (18h00 - 21h00)</Select.Option>
            </Select>
          </Form.Item>
        </Col>
      </Row>

      <Row gutter={[24, 16]}>
        {/* Phần thông tin cửa hàng - Full width */}
        <Col xs={24}>
          <Form.Item label="Địa chỉ cửa hàng">
            <Input
              value={store?.address || "Đang tải..."}
              disabled
              placeholder="Địa chỉ cửa hàng"
              style={{ backgroundColor: "#f5f5f5" }}
            />
          </Form.Item>
        </Col>

        {/* Phần thông tin người nhận - 2 cột */}
        <Col xs={24} lg={12}>
          <Form.Item
            name="receiver_name"
            label="Tên người nhận"
            rules={[
              { required: true, message: "Vui lòng nhập tên người nhận!" },
            ]}
          >
            <Input placeholder="Nhập tên người nhận" />
          </Form.Item>
        </Col>
        <Col xs={24} lg={12}>
          <Form.Item
            name="receiver_phone"
            label="Số điện thoại"
            rules={[
              { required: true, message: "Vui lòng nhập số điện thoại!" },
              {
                pattern: /^[0-9]{10,11}$/,
                message: "Số điện thoại không hợp lệ!",
              },
            ]}
          >
            <Input placeholder="Nhập số điện thoại" />
          </Form.Item>
        </Col>
        <Col xs={24} lg={12}>
          <Form.Item
            name="receiver_email"
            label="Email người nhận"
            rules={[{ type: "email", message: "Email không hợp lệ!" }]}
          >
            <Input placeholder="Nhập email (không bắt buộc)" />
          </Form.Item>
        </Col>
        <Col xs={24} lg={12}>
          <Form.Item
            name="addressType"
            label="Loại địa chỉ giao hàng"
            initialValue="DELIVERY"
            rules={[{ required: true, message: "Vui lòng chọn loại địa chỉ!" }]}
          >
            <Select placeholder="Chọn loại địa chỉ">
              <Select.Option value="HOME">Nhà riêng</Select.Option>
              <Select.Option value="STORE">Cửa hàng</Select.Option>
              <Select.Option value="OFFICE">Văn phòng</Select.Option>
            </Select>
          </Form.Item>
        </Col>

        {/* Phần địa chỉ giao hàng - Full width */}
        <Col xs={24}>
          <Form.Item label="Địa chỉ giao hàng" required>
            <Input
              placeholder="Địa chỉ sẽ hiển thị sau khi chọn tỉnh/huyện/xã"
              value={addressValue}
              readOnly
              style={{
                marginBottom: 16,
                borderRadius: 6,
                backgroundColor: addressValue ? "#f5f5f5" : "#fff",
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
                ⚠️ Không tìm thấy tọa độ cho địa chỉ này
              </div>
            )}

            {/*
            {process.env.NODE_ENV === 'development' && (
              <div style={{ marginBottom: 12, display: 'flex', gap: '8px', flexWrap: 'wrap' }}>
                <Button size="small" onClick={handleDebugFormValues}>
                  Debug Form Values
                </Button>
                <Button size="small" onClick={handleTestGeocoding} disabled={!addressValue}>
                  Test Geocoding
                </Button>
                <Button size="small" onClick={handleTestWithFakeCoords} type="primary">
                  Test với Fake Coords
                </Button>
              </div>
            )}
            */}
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

        {/* Phần mô tả và ghi chú - 2 cột trên desktop */}
        <Col xs={24} lg={12}>
          <Form.Item name="description" label="Mô tả đơn hàng">
            <Input.TextArea
              rows={4}
              placeholder="Mô tả chi tiết (không bắt buộc)"
            />
          </Form.Item>
        </Col>
        <Col xs={24} lg={12}>
          <Form.Item name="notes" label="Ghi chú">
            <Input.TextArea rows={4} placeholder="Ghi chú bổ sung (nếu có)" />
          </Form.Item>
        </Col>
      </Row>

      {/* <AddressFormModal
        visible={isAddressModalVisible}
        onCancel={() => setIsAddressModalVisible(false)}
        onOk={handleAddressSelect}
        contactName={form.getFieldValue("receiver_name") || ""}
        contactPhone={form.getFieldValue("receiver_phone") || ""}
        contactEmail={form.getFieldValue("receiver_email") || ""}
      /> */}
    </>
  );
}
