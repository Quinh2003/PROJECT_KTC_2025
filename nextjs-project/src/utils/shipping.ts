import { OrderItem } from "@/types/orders";

// Hệ số cho từng loại dịch vụ
export const SERVICE_MULTIPLIERS = {
  SECOND_CLASS: 0.8,
  STANDARD: 1.0,
  FIRST_CLASS: 1.3,
  EXPRESS: 1.8,
  PRIORITY: 2.0,
} as const;

export type ServiceType = keyof typeof SERVICE_MULTIPLIERS;

/**
 * Tính phí vận chuyển cơ bản (chưa áp dụng hệ số dịch vụ)
 */
export const calculateBaseShippingFee = (
  items: OrderItem[],
  isFragile?: boolean
): number => {
  if (!items || items.length === 0) return 0;
  
  const riskMultiplier = isFragile ? 1.3 : 1.0;
  
  return items.reduce((total, item) => {
    const weight = Number(item.weight) || 0;
    const height = Number(item.height) || 0;
    const width = Number(item.width) || 0;
    const length = Number(item.length) || 0;
    const quantity = Number(item.quantity) || 1;
    
    // Tính thể tích (cm³)
    const volume = height * width * length;
    // Trọng lượng quy đổi = thể tích / 5000
    const volumeWeight = volume / 5000;
    // Trọng lượng tính phí = max(trọng lượng thực tế, trọng lượng quy đổi)
    const billableWeight = Math.max(weight, volumeWeight);
    // Phí cơ bản = trọng lượng tính phí × 6,500
    const baseFee = billableWeight * 6500;
    // Tổng phí cho item này (chỉ áp dụng hệ số dễ vỡ, chưa áp dụng hệ số dịch vụ)
    const itemFee = baseFee * riskMultiplier * quantity;
    
    return total + itemFee;
  }, 0);
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
