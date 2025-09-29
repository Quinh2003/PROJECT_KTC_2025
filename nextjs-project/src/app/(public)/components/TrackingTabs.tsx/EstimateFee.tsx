"use client";

import { useState, useEffect } from "react";
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

interface EstimateForm {
  pickupAddress: string;
  deliveryAddress: string;
  weight: number;
  service_type: ServiceType;
  is_fragile: boolean;
  delivery_city?: string;
  delivery_address_detail?: string;
  delivery_latitude?: number | null;
  delivery_longitude?: number | null;
}

interface ServiceResult {
  serviceType: string;
  serviceName: string;
  multiplier: number;
  totalFee: number;
  baseFee: number;
  distanceFee: number;
  distanceKm: number;
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
  // Create mock OrderItem from form values
  const mockOrderItem = {
    product_name: "Estimated Product",
    quantity: 1,
    weight: values.weight,
    height: 10,
    width: 10,
    length: 10,
    is_fragile: values.is_fragile || false,
  };

  // Calculate base fee based on product
  const baseFee = calculateBaseShippingFee([mockOrderItem], values.is_fragile);

  let distanceFee = 0;
  let distanceKm = 0;
  let region = "Unknown";

  // Calculate distance fee if coordinates are available
  let storeLatitude = store?.latitude;
  let storeLongitude = store?.longitude;

  // If store doesn't have coordinates, try to geocode store address
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
      console.warn("Cannot get coordinates for store:", error);
    }
  }

  if (
    storeLongitude &&
    storeLatitude &&
    values.delivery_latitude &&
    values.delivery_longitude
  ) {
    try {
      // Use Mapbox to calculate actual distance
      const coordinates = await getMapboxRoute(
        storeLongitude,
        storeLatitude,
        values.delivery_longitude,
        values.delivery_latitude
      );

      if (coordinates.length >= 2) {
        // Calculate total distance through waypoints
        const points: [number, number][] = coordinates.map((coord) => [
          coord[0],
          coord[1],
        ]);
        distanceKm = 0;

        for (let i = 1; i < points.length; i++) {
          distanceKm += haversineDistance(points[i - 1], points[i]);
        }

        // Calculate distance-based fee
        const distanceResult = calculateDistanceFee(distanceKm);
        distanceFee = distanceResult.fee;
        region = distanceResult.region;
      }
    } catch (error) {
      console.warn(
        "Failed to get Mapbox route, using Haversine distance:",
        error
      );

      // Fallback: use straight-line distance
      distanceKm = haversineDistance(
        [storeLongitude, storeLatitude],
        [values.delivery_longitude, values.delivery_latitude]
      );

      const distanceResult = calculateDistanceFee(distanceKm);
      distanceFee = distanceResult.fee;
      region = distanceResult.region;
    }
  }

  // Apply service fee multiplier
  const serviceType = values.service_type || "STANDARD";
  const serviceFeeMultiplier =
    SERVICE_MULTIPLIERS[serviceType as ServiceType] || 1.0;

  // Calculate total fee according to the correct formula
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

// Helper function to get service name
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

export default function EstimateFee() {
  // States for form
  const [weight, setWeight] = useState("");
  const [isFragile, setIsFragile] = useState(false);
  const [isLoading, setIsLoading] = useState(false);

  // States for provinces/districts/wards
  const [provinces, setProvinces] = useState<Province[]>([]);

  // States for pickup address
  const [pickupAddressValue, setPickupAddressValue] = useState<string>("");
  const [pickupSelectedProvince, setPickupSelectedProvince] =
    useState<string>("");
  const [pickupSelectedDistrict, setPickupSelectedDistrict] =
    useState<string>("");
  const [pickupSelectedWard, setPickupSelectedWard] = useState<string>("");
  const [pickupStreetAddress, setPickupStreetAddress] = useState<string>("");
  const [pickupCoordinates, setPickupCoordinates] = useState<{
    latitude: number | null;
    longitude: number | null;
  }>({ latitude: null, longitude: null });
  const [isPickupGeocodingLoading, setIsPickupGeocodingLoading] =
    useState(false);
  const [pickupDistricts, setPickupDistricts] = useState<District[]>([]);
  const [pickupWards, setPickupWards] = useState<Ward[]>([]);

  // States for delivery address
  const [deliveryAddressValue, setDeliveryAddressValue] = useState<string>("");
  const [deliverySelectedProvince, setDeliverySelectedProvince] =
    useState<string>("");
  const [deliverySelectedDistrict, setDeliverySelectedDistrict] =
    useState<string>("");
  const [deliverySelectedWard, setDeliverySelectedWard] = useState<string>("");
  const [deliveryStreetAddress, setDeliveryStreetAddress] =
    useState<string>("");
  const [deliveryCoordinates, setDeliveryCoordinates] = useState<{
    latitude: number | null;
    longitude: number | null;
  }>({ latitude: null, longitude: null });
  const [isDeliveryGeocodingLoading, setIsDeliveryGeocodingLoading] =
    useState(false);
  const [deliveryDistricts, setDeliveryDistricts] = useState<District[]>([]);
  const [deliveryWards, setDeliveryWards] = useState<Ward[]>([]);

  // States for results
  const [store, setStore] = useState<Store | null>(null);
  const [allServices, setAllServices] = useState<ServiceResult[]>([]);

  // Load store info and provinces when component mounts
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
          // Don't automatically set address anymore, let user choose
        }
      } catch (error) {
        console.error("Error loading store:", error);
      }
    };

    const loadProvinces = async () => {
      try {
        const provincesData = await addressService.getProvinces();
        setProvinces(provincesData);
      } catch (error) {
        console.error("Error loading provinces:", error);
      }
    };

    fetchStore();
    loadProvinces();
  }, []);

  // Functions for pickup address
  const handlePickupProvinceChange = async (value: string) => {
    setPickupSelectedProvince(value);
    setPickupSelectedDistrict("");
    setPickupSelectedWard("");
    setPickupDistricts([]);
    setPickupWards([]);

    await updatePickupAddressDisplay(value, "", "", pickupStreetAddress);

    try {
      const districtsData = await addressService.getDistricts(value);
      setPickupDistricts(districtsData);
    } catch (error) {
      console.error("Error loading pickup districts:", error);
    }
  };

  const handlePickupDistrictChange = async (value: string) => {
    setPickupSelectedDistrict(value);
    setPickupSelectedWard("");
    setPickupWards([]);

    await updatePickupAddressDisplay(
      pickupSelectedProvince,
      value,
      "",
      pickupStreetAddress
    );

    try {
      const wardsData = await addressService.getWards(value);
      setPickupWards(wardsData);
    } catch (error) {
      console.error("Error loading pickup wards:", error);
    }
  };

  const handlePickupWardChange = async (value: string) => {
    setPickupSelectedWard(value);
    await updatePickupAddressDisplay(
      pickupSelectedProvince,
      pickupSelectedDistrict,
      value,
      pickupStreetAddress
    );
  };

  const handlePickupStreetAddressChange = async (
    e: React.ChangeEvent<HTMLInputElement>
  ) => {
    const value = e.target.value;
    setPickupStreetAddress(value);
    await updatePickupAddressDisplay(
      pickupSelectedProvince,
      pickupSelectedDistrict,
      pickupSelectedWard,
      value
    );
  };

  const updatePickupAddressDisplay = async (
    provinceCode: string,
    districtCode: string,
    wardCode: string,
    street: string
  ) => {
    const provinceName =
      provinces.find((p) => p.code === provinceCode)?.name || "";
    const districtName =
      pickupDistricts.find((d) => d.code === districtCode)?.name || "";
    const wardName = pickupWards.find((w) => w.code === wardCode)?.name || "";

    // Create address display
    const addressParts = [];
    if (street.trim()) addressParts.push(street.trim());
    if (wardName) addressParts.push(wardName);
    if (districtName) addressParts.push(districtName);
    if (provinceName) addressParts.push(provinceName);

    const displayAddress = addressParts.join(", ");
    setPickupAddressValue(displayAddress);

    // Get coordinates when there's enough information
    if (provinceName && districtName && displayAddress.trim()) {
      let geocodeAddress = displayAddress;
      if (!displayAddress.toLowerCase().includes("việt nam")) {
        geocodeAddress += ", Việt Nam";
      }

      try {
        setIsPickupGeocodingLoading(true);
        const coords = await getCoordinatesFromAddress(geocodeAddress);
        setPickupCoordinates(coords);
      } catch (error) {
        console.error("Error in pickup geocoding:", error);
        setPickupCoordinates({ latitude: null, longitude: null });
      } finally {
        setIsPickupGeocodingLoading(false);
      }
    } else {
      setPickupCoordinates({ latitude: null, longitude: null });
    }
  };

  const handleClearPickupAddress = () => {
    setPickupSelectedProvince("");
    setPickupSelectedDistrict("");
    setPickupSelectedWard("");
    setPickupStreetAddress("");
    setPickupAddressValue("");
    setPickupCoordinates({ latitude: null, longitude: null });
    setPickupDistricts([]);
    setPickupWards([]);
  };

  // Functions for delivery address
  const handleDeliveryProvinceChange = async (value: string) => {
    setDeliverySelectedProvince(value);
    setDeliverySelectedDistrict("");
    setDeliverySelectedWard("");
    setDeliveryDistricts([]);
    setDeliveryWards([]);

    await updateDeliveryAddressDisplay(value, "", "", deliveryStreetAddress);

    try {
      const districtsData = await addressService.getDistricts(value);
      setDeliveryDistricts(districtsData);
    } catch (error) {
      console.error("Error loading delivery districts:", error);
    }
  };

  const handleDeliveryDistrictChange = async (value: string) => {
    setDeliverySelectedDistrict(value);
    setDeliverySelectedWard("");
    setDeliveryWards([]);

    await updateDeliveryAddressDisplay(
      deliverySelectedProvince,
      value,
      "",
      deliveryStreetAddress
    );

    try {
      const wardsData = await addressService.getWards(value);
      setDeliveryWards(wardsData);
    } catch (error) {
      console.error("Error loading delivery wards:", error);
    }
  };

  const handleDeliveryWardChange = async (value: string) => {
    setDeliverySelectedWard(value);
    await updateDeliveryAddressDisplay(
      deliverySelectedProvince,
      deliverySelectedDistrict,
      value,
      deliveryStreetAddress
    );
  };

  const handleDeliveryStreetAddressChange = async (
    e: React.ChangeEvent<HTMLInputElement>
  ) => {
    const value = e.target.value;
    setDeliveryStreetAddress(value);
    await updateDeliveryAddressDisplay(
      deliverySelectedProvince,
      deliverySelectedDistrict,
      deliverySelectedWard,
      value
    );
  };

  const updateDeliveryAddressDisplay = async (
    provinceCode: string,
    districtCode: string,
    wardCode: string,
    street: string
  ) => {
    const provinceName =
      provinces.find((p) => p.code === provinceCode)?.name || "";
    const districtName =
      deliveryDistricts.find((d) => d.code === districtCode)?.name || "";
    const wardName = deliveryWards.find((w) => w.code === wardCode)?.name || "";

    // Create address display
    const addressParts = [];
    if (street.trim()) addressParts.push(street.trim());
    if (wardName) addressParts.push(wardName);
    if (districtName) addressParts.push(districtName);
    if (provinceName) addressParts.push(provinceName);

    const displayAddress = addressParts.join(", ");
    setDeliveryAddressValue(displayAddress);

    // Get coordinates when there's enough information
    if (provinceName && districtName && displayAddress.trim()) {
      let geocodeAddress = displayAddress;
      if (!displayAddress.toLowerCase().includes("việt nam")) {
        geocodeAddress += ", Việt Nam";
      }

      try {
        setIsDeliveryGeocodingLoading(true);
        const coords = await getCoordinatesFromAddress(geocodeAddress);
        setDeliveryCoordinates(coords);
      } catch (error) {
        console.error("Error in delivery geocoding:", error);
        setDeliveryCoordinates({ latitude: null, longitude: null });
      } finally {
        setIsDeliveryGeocodingLoading(false);
      }
    } else {
      setDeliveryCoordinates({ latitude: null, longitude: null });
    }
  };

  const handleClearDeliveryAddress = () => {
    setDeliverySelectedProvince("");
    setDeliverySelectedDistrict("");
    setDeliverySelectedWard("");
    setDeliveryStreetAddress("");
    setDeliveryAddressValue("");
    setDeliveryCoordinates({ latitude: null, longitude: null });
    setDeliveryDistricts([]);
    setDeliveryWards([]);
  };

  const handleEstimate = async (e: React.FormEvent) => {
    e.preventDefault();
    if (
      !pickupAddressValue.trim() ||
      !deliveryAddressValue.trim() ||
      !weight.trim()
    ) {
      alert("Please fill in all required information!");
      return;
    }

    if (!pickupCoordinates.latitude || !pickupCoordinates.longitude) {
      alert(
        "Unable to determine pickup address coordinates. Please select the correct address!"
      );
      return;
    }

    if (!deliveryCoordinates.latitude || !deliveryCoordinates.longitude) {
      alert(
        "Unable to determine delivery address coordinates. Please select the correct address!"
      );
      return;
    }

    setIsLoading(true);

    try {
      // Calculate for all service types
      const allServiceResults: ServiceResult[] = [];

      // Create mock store with pickup coordinates
      const mockStore: Store = {
        ...store,
        address: pickupAddressValue,
        latitude: pickupCoordinates.latitude,
        longitude: pickupCoordinates.longitude,
      } as Store;

      for (const [serviceKey, multiplier] of Object.entries(
        SERVICE_MULTIPLIERS
      )) {
        const values: EstimateForm = {
          pickupAddress: pickupAddressValue,
          deliveryAddress: deliveryAddressValue,
          weight: parseFloat(weight),
          service_type: serviceKey as ServiceType,
          is_fragile: isFragile,
          delivery_latitude: deliveryCoordinates.latitude,
          delivery_longitude: deliveryCoordinates.longitude,
        };

        const result = await calculateShippingFee(values, mockStore);
        allServiceResults.push({
          serviceType: serviceKey,
          serviceName: getServiceName(serviceKey as ServiceType),
          multiplier,
          ...result,
        });
      }

      setAllServices(allServiceResults);
    } catch (error) {
      console.error("Error calculating shipping fee:", error);
      alert("An error occurred while calculating shipping fee!");
      setAllServices([]);
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div className="bg-white rounded-2xl  p-6 sm:p-8 w-full">
      <div className="w-full max-w-4xl mx-auto">
        <h2 className="text-lg sm:text-2xl font-bold text-green-700 mb-6 text-center">
          Estimate Shipping Fee
        </h2>

        <form onSubmit={handleEstimate} className="space-y-6">
          {/* Pickup address with province/district/ward selection system */}
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Pickup Address <span className="text-red-500">*</span>
            </label>

            {/* Hiển thị địa chỉ gửi đầy đủ */}
            <div className="mb-4">
              <input
                type="text"
                value={pickupAddressValue}
                readOnly
                placeholder="Address will be displayed after selecting province/district/ward"
                className="w-full px-4 py-3 border border-gray-300 rounded-lg bg-gray-50 text-sm sm:text-base"
              />
              {pickupAddressValue && (
                <button
                  type="button"
                  onClick={handleClearPickupAddress}
                  className="mt-2 text-sm text-red-600 hover:text-red-800"
                >
                  ✗ Clear pickup address
                </button>
              )}
            </div>

            {/* Hiển thị trạng thái tọa độ gửi */}
            {isPickupGeocodingLoading && (
              <div className="mb-4 p-3 bg-blue-50 border border-blue-200 rounded text-sm text-blue-700">
                🔄 Getting pickup address coordinates...
              </div>
            )}
            {!isPickupGeocodingLoading &&
              pickupCoordinates.latitude &&
              pickupCoordinates.longitude && (
                <div className="mb-4 p-3 bg-green-50 border border-green-200 rounded text-sm text-green-700">
                  📍 Pickup coordinates: {pickupCoordinates.latitude.toFixed(6)}
                  , {pickupCoordinates.longitude.toFixed(6)}
                </div>
              )}
            {!isPickupGeocodingLoading &&
              pickupAddressValue &&
              !pickupCoordinates.latitude && (
                <div className="mb-4 p-3 bg-red-50 border border-red-200 rounded text-sm text-red-700">
                  ⚠️ No coordinates found for this pickup address
                </div>
              )}

            {/* Chọn tỉnh/huyện/xã cho địa chỉ gửi */}
            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-3">
              {/* Số nhà/đường gửi */}
              <div>
                <label className="block text-xs font-medium text-gray-600 mb-1">
                  House number, street (pickup)
                </label>
                <input
                  type="text"
                  value={pickupStreetAddress}
                  onChange={handlePickupStreetAddressChange}
                  placeholder="E.g: 123 Le Loi"
                  className="w-full px-3 py-2 border border-gray-300 rounded text-sm focus:ring-2 focus:ring-green-700 focus:border-green-700"
                />
              </div>

              {/* Tỉnh/Thành phố gửi */}
              <div>
                <label className="block text-xs font-medium text-gray-600 mb-1">
                  Province/City (pickup)
                </label>
                <select
                  value={pickupSelectedProvince}
                  onChange={(e) => handlePickupProvinceChange(e.target.value)}
                  className="w-full px-3 py-2 border border-gray-300 rounded text-sm focus:ring-2 focus:ring-green-700 focus:border-green-700"
                >
                  <option value="">Select province/city</option>
                  {provinces.map((province) => (
                    <option key={province.code} value={province.code}>
                      {province.name}
                    </option>
                  ))}
                </select>
              </div>

              {/* Quận/Huyện gửi */}
              <div>
                <label className="block text-xs font-medium text-gray-600 mb-1">
                  District (pickup)
                </label>
                <select
                  value={pickupSelectedDistrict}
                  onChange={(e) => handlePickupDistrictChange(e.target.value)}
                  disabled={!pickupSelectedProvince}
                  className="w-full px-3 py-2 border border-gray-300 rounded text-sm focus:ring-2 focus:ring-green-700 focus:border-green-700 disabled:bg-gray-100"
                >
                  <option value="">Select district</option>
                  {pickupDistricts.map((district) => (
                    <option key={district.code} value={district.code}>
                      {district.name}
                    </option>
                  ))}
                </select>
              </div>

              {/* Xã/Phường gửi */}
              <div>
                <label className="block text-xs font-medium text-gray-600 mb-1">
                  Ward (pickup)
                </label>
                <select
                  value={pickupSelectedWard}
                  onChange={(e) => handlePickupWardChange(e.target.value)}
                  disabled={!pickupSelectedDistrict}
                  className="w-full px-3 py-2 border border-gray-300 rounded text-sm focus:ring-2 focus:ring-green-700 focus:border-green-700 disabled:bg-gray-100"
                >
                  <option value="">Select ward</option>
                  {pickupWards.map((ward) => (
                    <option key={ward.code} value={ward.code}>
                      {ward.name}
                    </option>
                  ))}
                </select>
              </div>
            </div>
          </div>

          {/* Địa chỉ nhận với hệ thống chọn tỉnh/huyện/xã */}
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Delivery Address <span className="text-red-500">*</span>
            </label>

            {/* Hiển thị địa chỉ giao hàng đầy đủ */}
            <div className="mb-4">
              <input
                type="text"
                value={deliveryAddressValue}
                readOnly
                placeholder="Address will be displayed after selecting province/district/ward"
                className="w-full px-4 py-3 border border-gray-300 rounded-lg bg-gray-50 text-sm sm:text-base"
              />
              {deliveryAddressValue && (
                <button
                  type="button"
                  onClick={handleClearDeliveryAddress}
                  className="mt-2 text-sm text-red-600 hover:text-red-800"
                >
                  ✗ Clear delivery address
                </button>
              )}
            </div>

            {/* Hiển thị trạng thái tọa độ giao hàng */}
            {isDeliveryGeocodingLoading && (
              <div className="mb-4 p-3 bg-blue-50 border border-blue-200 rounded text-sm text-blue-700">
                🔄 Getting delivery address coordinates...
              </div>
            )}
            {!isDeliveryGeocodingLoading &&
              deliveryCoordinates.latitude &&
              deliveryCoordinates.longitude && (
                <div className="mb-4 p-3 bg-green-50 border border-green-200 rounded text-sm text-green-700">
                  📍 Delivery coordinates:{" "}
                  {deliveryCoordinates.latitude.toFixed(6)},{" "}
                  {deliveryCoordinates.longitude.toFixed(6)}
                </div>
              )}
            {!isDeliveryGeocodingLoading &&
              deliveryAddressValue &&
              !deliveryCoordinates.latitude && (
                <div className="mb-4 p-3 bg-red-50 border border-red-200 rounded text-sm text-red-700">
                  ⚠️ No coordinates found for this delivery address
                </div>
              )}

            {/* Chọn tỉnh/huyện/xã cho địa chỉ giao hàng */}
            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-3">
              {/* Số nhà/đường nhận */}
              <div>
                <label className="block text-xs font-medium text-gray-600 mb-1">
                  House number, street (delivery)
                </label>
                <input
                  type="text"
                  value={deliveryStreetAddress}
                  onChange={handleDeliveryStreetAddressChange}
                  placeholder="E.g: 456 Nguyen Hue"
                  className="w-full px-3 py-2 border border-gray-300 rounded text-sm focus:ring-2 focus:ring-green-700 focus:border-green-700"
                />
              </div>

              {/* Tỉnh/Thành phố nhận */}
              <div>
                <label className="block text-xs font-medium text-gray-600 mb-1">
                  Province/City (delivery)
                </label>
                <select
                  value={deliverySelectedProvince}
                  onChange={(e) => handleDeliveryProvinceChange(e.target.value)}
                  className="w-full px-3 py-2 border border-gray-300 rounded text-sm focus:ring-2 focus:ring-green-700 focus:border-green-700"
                >
                  <option value="">Select province/city</option>
                  {provinces.map((province) => (
                    <option key={province.code} value={province.code}>
                      {province.name}
                    </option>
                  ))}
                </select>
              </div>

              {/* Quận/Huyện nhận */}
              <div>
                <label className="block text-xs font-medium text-gray-600 mb-1">
                  District (delivery)
                </label>
                <select
                  value={deliverySelectedDistrict}
                  onChange={(e) => handleDeliveryDistrictChange(e.target.value)}
                  disabled={!deliverySelectedProvince}
                  className="w-full px-3 py-2 border border-gray-300 rounded text-sm focus:ring-2 focus:ring-green-700 focus:border-green-700 disabled:bg-gray-100"
                >
                  <option value="">Select district</option>
                  {deliveryDistricts.map((district) => (
                    <option key={district.code} value={district.code}>
                      {district.name}
                    </option>
                  ))}
                </select>
              </div>

              {/* Xã/Phường nhận */}
              <div>
                <label className="block text-xs font-medium text-gray-600 mb-1">
                  Ward (delivery)
                </label>
                <select
                  value={deliverySelectedWard}
                  onChange={(e) => handleDeliveryWardChange(e.target.value)}
                  disabled={!deliverySelectedDistrict}
                  className="w-full px-3 py-2 border border-gray-300 rounded text-sm focus:ring-2 focus:ring-green-700 focus:border-green-700 disabled:bg-gray-100"
                >
                  <option value="">Select ward</option>
                  {deliveryWards.map((ward) => (
                    <option key={ward.code} value={ward.code}>
                      {ward.name}
                    </option>
                  ))}
                </select>
              </div>
            </div>
          </div>

          {/* Khối lượng và hàng dễ vỡ */}
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Weight (Kg) <span className="text-red-500">*</span>
              </label>
              <input
                type="number"
                value={weight}
                onChange={(e) => setWeight(e.target.value)}
                placeholder="E.g: 500"
                min="1"
                className="w-full px-4 py-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-green-700 focus:border-green-700 text-sm sm:text-base"
              />
            </div>

            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Fragile Item
              </label>
              <div className="flex items-center pt-3">
                <input
                  type="checkbox"
                  checked={isFragile}
                  onChange={(e) => setIsFragile(e.target.checked)}
                  className="w-4 h-4 text-green-700 border-gray-300 rounded focus:ring-green-700"
                />
                <label className="ml-2 text-sm text-gray-700">
                  Mark if item is fragile
                </label>
              </div>
            </div>
          </div>

          {/* Nút tính toán */}
          <div className="flex justify-center pt-4">
            <button
              type="submit"
              disabled={
                isLoading ||
                !pickupAddressValue ||
                !deliveryAddressValue ||
                !pickupCoordinates.latitude ||
                !deliveryCoordinates.latitude
              }
              className="bg-green-700 text-white px-8 py-3 rounded-lg hover:bg-green-800 disabled:opacity-50 transition-colors text-sm sm:text-base font-semibold min-w-[200px]"
            >
              {isLoading ? "Calculating..." : "Estimate Shipping Fee"}
            </button>
          </div>
        </form>

        {/* Hiển thị kết quả */}
        {allServices.length > 0 && (
          <div className="mt-8 space-y-4">
            <h3 className="text-xl font-semibold text-gray-800 text-center mb-6">
              Shipping Fee Estimation Result
            </h3>

            {/* Bảng kết quả */}
            <div className="overflow-x-auto">
              <table className="w-full bg-white border border-gray-200 rounded-lg">
                <thead className="bg-gray-50">
                  <tr>
                    <th className="px-4 py-3 text-left text-sm font-medium text-gray-700">
                      Service Type
                    </th>
                    <th className="px-4 py-3 text-center text-sm font-medium text-gray-700">
                      Multiplier
                    </th>
                    <th className="px-4 py-3 text-center text-sm font-medium text-gray-700">
                      Distance
                    </th>
                    <th className="px-4 py-3 text-right text-sm font-medium text-gray-700">
                      Total Fee
                    </th>
                  </tr>
                </thead>
                <tbody>
                  {allServices.map((service, index) => (
                    <tr
                      key={service.serviceType}
                      className={`${
                        service.serviceType === "STANDARD"
                          ? "bg-green-50"
                          : "bg-white"
                      } ${
                        index < allServices.length - 1
                          ? "border-b border-gray-200"
                          : ""
                      }`}
                    >
                      <td className="px-4 py-3">
                        <div className="font-medium text-gray-800">
                          {service.serviceName}
                        </div>
                        {service.serviceType === "STANDARD" && (
                          <div className="text-xs text-green-600">
                            (Recommended)
                          </div>
                        )}
                      </td>
                      <td className="px-4 py-3 text-center">
                        x{service.multiplier}
                      </td>
                      <td className="px-4 py-3 text-center">
                        {service.distanceKm.toFixed(1)} km
                      </td>
                      <td className="px-4 py-3 text-right font-bold text-green-700">
                        {service.totalFee.toLocaleString("vi-VN")}đ
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>

            <div className="text-center text-sm text-gray-600 mt-4">
              <p>* Prices are subject to change and for reference only</p>
              <p>
                Official shipping fee will be confirmed when creating the order
              </p>
            </div>
          </div>
        )}
      </div>
    </div>
  );
}
