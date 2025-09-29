import { OrderItem } from "@/types/orders";

/**
 * Kiểm tra item có hợp lệ không
 */
export const isValidItem = (item: OrderItem): boolean => {
  return !!(item && item.product_name && item.quantity > 0 && item.weight > 0);
};

/**
 * Tính thể tích sản phẩm (cm³)
 */
export const calculateVolume = (item: OrderItem): number => {
  const height = Number(item.height) || 0;
  const width = Number(item.width) || 0;
  const length = Number(item.length) || 0;
  return height * width * length;
};

/**
 * Kiểm tra có item nào dễ vỡ không
 */
export const hasFragileItems = (items: OrderItem[]): boolean => {
  return items.some(item => {
    if (!isValidItem(item)) return false;
    return (item as any)?.is_fragile || false;
  });
};
