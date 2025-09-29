import { FormInstance } from "antd";
import { Store } from "@/types/Store";
import {
  calculateShippingFee,
  getServiceMultiplier,
  calculateBaseShippingFee,
} from "./shipping";
import { calculateDistanceFee, calculateTotalDistance } from "./distance";
import { getMapboxRoute } from "./mapbox";
import { isValidItem, calculateVolume } from "./orderItems";

/**
 * Interface cho payload tạo address
 */
export interface AddressPayload {
  addressType: string;
  address: string;
  city: string;
  contactName: string;
  contactPhone: string;
  contactEmail?: string | null;
  state?: string | null;
  country: string;
  region?: string | null;
  postalCode?: string | null;
  floorNumber?: string | null;
  latitude?: number | null;
  longitude?: number | null;
}

/**
 * Interface cho payload tạo product
 */
export interface ProductPayload {
  name: string;
  weight: number;
  volume?: number | null;
  fragile: boolean;
  createdByUserId: number;
  unitPrice: number;
  categoryId: number;
  productStatus: number;
  description?: string | null;
  stockQuantity: number;
  temporary: boolean;
  notes?: string | null;
  warehouseId?: number | null;
}

/**
 * Interface cho payload tạo order
 */
export interface OrderPayload {
  store: { id: number };
  address: { id: number };
  status: { id: number };
  createdBy: { id: number };
  orderCode?: string | null;
  description?: string | null;
  totalAmount?: number | null;
  notes?: string | null;
  pickupDate?: string | null;
  pickupTimePeriod?: string | null;
}

/**
 * Interface cho payload tạo order item
 */
export interface OrderItemPayload {
  orderId: number;
  productId: number;
  quantity: number;
  shippingFee: number;
  notes?: string | null;
}

/**
 * Interface cho payload tạo delivery
 */
export interface DeliveryPayload {
  orderId: number;
  deliveryFee: number;
  serviceType: string;
  transportMode: string;
  pickupDate?: string | null;
  scheduleDeliveryTime?: string | null;
  orderDate: string;
  vehicleId?: number | null;
  driverId?: number | null;
  routeId?: number | null;
  lateDeliveryRisk: boolean;
  deliveryNotes?: string | null;
}

/**
 * Tạo address payload từ form values
 */
export const createAddressPayload = (values: any): AddressPayload => {
  // Map string sang enum AddressType
  let addressType = "DELIVERY";
  if (values.addressType) {
    switch (values.addressType.toLowerCase()) {
      case "home":
        addressType = "HOME";
        break;
      case "store":
        addressType = "STORE";
        break;
      case "office":
        addressType = "OFFICE";
        break;
      default:
        addressType = "DELIVERY";
    }
  }

  // Validate required fields
  if (!values.address || values.address.trim() === "") {
    throw new Error("Địa chỉ là bắt buộc");
  }
  if (!values.city || values.city.trim() === "") {
    throw new Error("Thành phố là bắt buộc");
  }
  if (!values.receiver_name || values.receiver_name.trim() === "") {
    throw new Error("Tên người nhận là bắt buộc");
  }
  if (!values.receiver_phone || values.receiver_phone.trim() === "") {
    throw new Error("Số điện thoại người nhận là bắt buộc");
  }

  // Chỉ trả về những field cần thiết cho address, không bao gồm pickup_date và pickup_time_period
  return {
    addressType,
    address: values.address.trim(),
    city: values.city.trim(),
    contactName: values.receiver_name.trim(),
    contactPhone: values.receiver_phone.trim(),
    contactEmail: values.receiver_email || null,
    state: null,
    country: "Vietnam",
    region: null,
    postalCode: null,
    floorNumber: null,
    latitude: values.latitude || null,
    longitude: values.longitude || null,
  };
};

/**
 * Tạo product payload từ item
 */
export const createProductPayload = (
  item: any,
  storeId: number
): ProductPayload => {
  const volume = calculateVolume(item);

  return {
    name: item.product_name,
    weight: item.weight,
    volume: volume > 0 ? parseFloat(volume.toFixed(3)) : null,
    fragile: item.is_fragile || false,
    createdByUserId: storeId,
    unitPrice: item.unitPrice || 0,
    categoryId: 1,
    productStatus: 1, // ACTIVE
    description: null,
    stockQuantity: 0,
    temporary: false,
    notes: null,
    warehouseId: null,
  };
};

/**
 * Tạo order payload
 */
export const createOrderPayload = (
  store: Store,
  addressId: number,
  values: any,
  userId: number
): OrderPayload => {
  // Tạo notes từ pickup_time_period và notes gốc
  let notes = values.notes || "";
  if (values.pickup_time_period) {
    const timePeriodText = values.pickup_time_period === "morning" ? "Sáng" :
                          values.pickup_time_period === "afternoon" ? "Chiều" :
                          values.pickup_time_period === "evening" ? "Tối" : values.pickup_time_period;
    notes = notes ? `${notes} | Buổi lấy hàng: ${timePeriodText}` : `Buổi lấy hàng: ${timePeriodText}`;
  }

  return {
    store: { id: store.id },
    address: { id: addressId },
    status: { id: 1 },
    createdBy: { id: userId },
    orderCode: null,
    description: values.description || null,
    totalAmount: null,
    notes: notes || null,
    pickupDate: values.pickup_date ? values.pickup_date.format("YYYY-MM-DD") : null,
    pickupTimePeriod: values.pickup_time_period || null,
  };
};

/**
 * Tạo order item payload
 */
export const createOrderItemPayload = (
  orderId: number,
  productId: number,
  item: any,
  serviceType: string
): OrderItemPayload => {
  const itemFragile = item.is_fragile || false;
  // Lưu phí cơ bản (chưa áp dụng hệ số dịch vụ) để đồng nhất với UI
  const calculatedShippingFee = calculateBaseShippingFee([item], itemFragile);

  return {
    orderId,
    productId,
    quantity: item.quantity,
    shippingFee: Math.round(calculatedShippingFee),
    notes: item.notes || null,
  };
};

/**
 * Tính tổng delivery fee
 */
export const calculateTotalDeliveryFee = async (
  items: any[],
  serviceType: string,
  store: Store,
  targetLatitude?: number,
  targetLongitude?: number
): Promise<{
  totalFee: number;
  baseShippingFee: number;
  distanceFee: number;
  serviceFeeMultiplier: number;
}> => {
  // Tính phí sản phẩm cơ bản
  let baseShippingFee = 0;
  items.forEach((item) => {
    if (isValidItem(item)) {
      const itemFragile = item.is_fragile || false;
      const itemFee = calculateBaseShippingFee([item], itemFragile);
      baseShippingFee += itemFee;
    }
  });

  // Hệ số service type
  const serviceFeeMultiplier = getServiceMultiplier(serviceType);

  // Tính phí khoảng cách từ Mapbox
  let distanceFee = 0;

  if (
    store?.longitude &&
    store?.latitude &&
    targetLatitude &&
    targetLongitude
  ) {
    try {
      const coordinates = await getMapboxRoute(
        store.longitude,
        store.latitude,
        targetLongitude,
        targetLatitude
      );

      if (coordinates.length >= 2) {
        const distance = calculateTotalDistance(coordinates);
        const feeResult = calculateDistanceFee(distance);
        distanceFee = feeResult.fee;
      }
    } catch (error) {
      console.warn("Failed to get Mapbox route, distanceFee = 0:", error);
    }
  }

  const totalFee = Math.round(
    baseShippingFee * serviceFeeMultiplier + distanceFee
  );

  return {
    totalFee,
    baseShippingFee,
    distanceFee,
    serviceFeeMultiplier,
  };
};

/**
 * Tạo delivery payload
 */
export const createDeliveryPayload = (
  orderId: number,
  form: FormInstance,
  serviceType: string,
  notes?: string
): DeliveryPayload => {
  // Lấy thông tin thời gian lấy hàng từ form
  const pickupDate = form.getFieldValue("pickup_date");
  const pickupTimePeriod = form.getFieldValue("pickup_time_period");

  // Format pickup date nếu có
  let formattedPickupDate = null;
  if (pickupDate) {
    formattedPickupDate = pickupDate.format("YYYY-MM-DD");
  }

  // Tạo schedule delivery time dựa trên pickup date và time period
  let scheduleDeliveryTime = null;
  if (formattedPickupDate && pickupTimePeriod) {
    switch (pickupTimePeriod) {
      case "morning":
        scheduleDeliveryTime = `${formattedPickupDate} 10:00:00`;
        break;
      case "afternoon":
        scheduleDeliveryTime = `${formattedPickupDate} 15:00:00`;
        break;
      case "evening":
        scheduleDeliveryTime = `${formattedPickupDate} 19:00:00`;
        break;
      default:
        scheduleDeliveryTime = `${formattedPickupDate} 10:00:00`;
    }
  }

  return {
    orderId,
    deliveryFee: form.getFieldValue("delivery_fee") || 0,
    serviceType,
    transportMode: "ROAD",
    pickupDate: formattedPickupDate,
    scheduleDeliveryTime,
    orderDate: new Date().toISOString().replace("T", " ").substring(0, 19),
    vehicleId: null,
    driverId: null,
    routeId: null,
    lateDeliveryRisk: false,
    deliveryNotes: notes || null,
  };
};

/**
 * Lấy current user ID từ localStorage
 */
export const getCurrentUserId = (): number => {
  try {
    const userStr = localStorage.getItem("user");
    if (userStr) {
      const user = JSON.parse(userStr);
      return user.id || 1;
    }
  } catch (e) {
    console.warn("Cannot parse user from localStorage, using default ID: 1");
  }
  return 1;
};
