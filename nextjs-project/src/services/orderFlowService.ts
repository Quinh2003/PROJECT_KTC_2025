import {
  AddressPayload,
  ProductPayload,
  OrderPayload,
  OrderItemPayload,
  DeliveryPayload
} from "@/utils/orderFlow";

/**
 * Service cho các API calls trong order flow
 */
export class OrderFlowService {
  /**
   * Tạo address
   */
  static async createAddress(payload: AddressPayload) {
    try {
      const response = await fetch('/api/address', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload),
      });

      if (!response.ok) {
        const errorData = await response.json().catch(() => ({}));
        const errorMessage = errorData.error || errorData.message || response.statusText;
        throw new Error(`Lưu address thất bại: ${errorMessage}`);
      }

      return response.json();
    } catch (error: any) {
      console.error('Error creating address:', error);
      throw error;
    }
  }

  /**
   * Tạo product
   */
  static async createProduct(payload: ProductPayload) {
    const response = await fetch('/api/products', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(payload),
    });

    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(`Tạo product thất bại: ${errorData.error || response.statusText}`);
    }

    return response.json();
  }

  /**
   * Tạo order
   */
  static async createOrder(payload: OrderPayload) {
    const response = await fetch('/api/orders', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(payload),
    });

    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(`Tạo order thất bại: ${errorData.error || response.statusText}`);
    }

    return response.json();
  }

  /**
   * Tạo order item
   */
  static async createOrderItem(payload: OrderItemPayload) {
    const response = await fetch('/api/order-items', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(payload),
    });

    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(`Tạo order item thất bại: ${errorData.error || response.statusText}`);
    }

    return response.json();
  }

  /**
   * Tạo delivery
   */
  static async createDelivery(payload: DeliveryPayload) {
    const response = await fetch('/api/deliveries', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(payload),
    });

    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(`Tạo delivery thất bại: ${errorData.error || response.statusText}`);
    }

    return response.json();
  }
}
