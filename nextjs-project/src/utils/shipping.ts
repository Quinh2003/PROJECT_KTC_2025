import { OrderItem } from "@/types/orders";

// Hệ số cho từng loại dịch vụ - THEO VIETTELPOST
export const SERVICE_MULTIPLIERS = {
  SECOND_CLASS: 0.8,
  STANDARD: 1.0,
  FIRST_CLASS: 1.2, // Theo bảng giá ViettelPost
  EXPRESS: 1.3,
  PRIORITY: 1.5,
} as const;

export type ServiceType = keyof typeof SERVICE_MULTIPLIERS;

// BẢNG GIÁ VIETTELPOST CHO ĐÀ NẴNG (Theo hình user cung cấp)
// Sử dụng cột "Nội cụm" cho Đà Nẵng
const VIETTEL_PRICES = {
  UNDER_250G: 28000,      // Đến 250g
  FROM_250_500G: 30000,   // Trên 250 - 500g  
  FROM_500_1000G: 33000,  // Trên 500 - 1000g
  FROM_1000_1500G: 36000, // Trên 1000 - 1500g
  FROM_1500_2000G: 39000, // Trên 1500 - 2000g
  FROM_2000_2500G: 42000, // Trên 2000 - 2500g
  FROM_2500_3000G: 45000, // Trên 2500 - 3000g
  ADDITIONAL_500G: 3000,  // Mỗi 0,5kg tiếp theo
};

/**
 * Tính phí vận chuyển theo công thức ViettelPost dựa trên trọng lượng
 */
export const calculateViettelPostShippingFee = (
  items: OrderItem[],
  isFragile?: boolean
): number => {
  if (!items || items.length === 0) return 0;
  
  // Tính tổng trọng lượng (gram)
  const totalWeightGrams = items.reduce((total, item) => {
    const weight = Number(item.weight) || 0; // weight trong kg
    const quantity = Number(item.quantity) || 1;
    const weightInGrams = weight * 1000 * quantity; // Chuyển kg sang gram
    return total + weightInGrams;
  }, 0);
  
  // Tính phí theo bảng giá ViettelPost
  let baseFee = 0;
  
  if (totalWeightGrams <= 250) {
    baseFee = VIETTEL_PRICES.UNDER_250G;
  } else if (totalWeightGrams <= 500) {
    baseFee = VIETTEL_PRICES.FROM_250_500G;
  } else if (totalWeightGrams <= 1000) {
    baseFee = VIETTEL_PRICES.FROM_500_1000G;
  } else if (totalWeightGrams <= 1500) {
    baseFee = VIETTEL_PRICES.FROM_1000_1500G;
  } else if (totalWeightGrams <= 2000) {
    baseFee = VIETTEL_PRICES.FROM_1500_2000G;
  } else if (totalWeightGrams <= 2500) {
    baseFee = VIETTEL_PRICES.FROM_2000_2500G;
  } else if (totalWeightGrams <= 3000) {
    baseFee = VIETTEL_PRICES.FROM_2500_3000G;
  } else {
    // Trên 3kg: phí cơ bản 3kg + phí cho từng 500g tiếp theo
    baseFee = VIETTEL_PRICES.FROM_2500_3000G;
    const excessWeight = totalWeightGrams - 3000;
    const additional500gBlocks = Math.ceil(excessWeight / 500);
    baseFee += additional500gBlocks * VIETTEL_PRICES.ADDITIONAL_500G;
  }
  
  // Áp dụng hệ số dễ vỡ nếu có
  const riskMultiplier = isFragile ? 1.3 : 1.0;
  return Math.round(baseFee * riskMultiplier);
};

/**
 * DEPRECATED: Tính phí vận chuyển cơ bản (logic cũ)
 */
export const calculateBaseShippingFee = (
  items: OrderItem[],
  isFragile?: boolean
): number => {
  // Sử dụng công thức ViettelPost mới
  return calculateViettelPostShippingFee(items, isFragile);
};

/**
 * Tính phí vận chuyển dựa trên sản phẩm (bao gồm hệ số dịch vụ)
 */
export const calculateShippingFee = (
  items: OrderItem[],
  isFragile?: boolean,
  serviceType?: string
): number => {
  const baseFee = calculateBaseShippingFee(items, isFragile);
  const serviceMultiplier = SERVICE_MULTIPLIERS[serviceType as ServiceType] || 1.0;
  return baseFee * serviceMultiplier;
};

/**
 * Lấy hệ số cho loại dịch vụ
 */
export const getServiceMultiplier = (serviceType: string): number => {
  return SERVICE_MULTIPLIERS[serviceType as ServiceType] || 1.0;
};
