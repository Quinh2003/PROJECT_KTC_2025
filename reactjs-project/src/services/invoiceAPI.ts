/**
 * Invoice API Service
 * Xử lý các API calls liên quan đến invoice management
 */

import { authService } from './authService';

// Base URL cho invoice APIs
const INVOICE_API_BASE = 'http://localhost:8080/api/invoices';

// Types cho Invoice
export interface Invoice {
  id: number;
  invoiceNumber: string;
  orderId: number;
  customerName?: string;
  customerEmail?: string;
  totalAmount: number;
  taxAmount: number;
  netAmount: number;
  invoiceStatus: InvoiceStatus;
  issuedAt: string;
  sentAt?: string;
  deliveredAt?: string;
  pdfFilePath?: string;
  pdfFileName?: string;
  createdBy?: {
    id: number;
    username: string;
    email: string;
  };
  order?: OrderInfo;
}

export interface OrderInfo {
  id: number;
  description: string;
  totalAmount: number;
  createdAt: string;
  status?: {
    name: string;
  };
  pickupAddress?: string;
  deliveryAddress?: string;
  store?: StoreInfo;
}

export interface StoreInfo {
  id: number;
  name: string;
  email: string;
  phone?: string;
  address?: string;
  createdAt: string;
}

export type InvoiceStatus = 'CREATED' | 'SENT' | 'DELIVERED' | 'CANCELLED';

export interface InvoiceListResponse {
  data: Invoice[];
  message: string;
  success: boolean;
}

export interface InvoiceDetailResponse {
  data: Invoice;
  message: string;
  success: boolean;
}

export interface SendEmailRequest {
  invoiceId: number;
  emailAddress: string;
}

class InvoiceAPIService {
  
  /**
   * Lấy headers authorization cho API calls
   */
  private getAuthHeaders(): HeadersInit {
    const token = authService.getToken();
    return {
      'Content-Type': 'application/json',
      'Authorization': token ? `Bearer ${token}` : '',
    };
  }

  /**
   * Xử lý API response và error handling
   */
  private async handleResponse<T>(response: Response): Promise<T> {
    if (!response.ok) {
      const errorText = await response.text();
      throw new Error(`API Error: ${response.status} - ${errorText}`);
    }

    const data = await response.json();
    if (!data.success) {
      throw new Error(data.message || 'API request failed');
    }

    return data.data;
  }

  /**
   * Lấy danh sách tất cả hóa đơn
   */
  async getAllInvoices(
    status?: InvoiceStatus,
    orderId?: number,
    customerEmail?: string
  ): Promise<Invoice[]> {
    try {
      const params = new URLSearchParams();
      if (status) params.append('status', status);
      if (orderId) params.append('orderId', orderId.toString());
      if (customerEmail) params.append('customerEmail', customerEmail);

      const url = `${INVOICE_API_BASE}${params.toString() ? `?${params.toString()}` : ''}`;
      
      const response = await fetch(url, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });

      return await this.handleResponse<Invoice[]>(response);
    } catch (error) {
      console.error('Error fetching invoices:', error);
      throw error;
    }
  }

  /**
   * Lấy chi tiết hóa đơn theo ID
   */
  async getInvoiceById(id: number): Promise<Invoice> {
    try {
      const response = await fetch(`${INVOICE_API_BASE}/${id}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });

      return await this.handleResponse<Invoice>(response);
    } catch (error) {
      console.error('Error fetching invoice detail:', error);
      throw error;
    }
  }

  /**
   * Lấy hóa đơn theo Order ID
   */
  async getInvoiceByOrderId(orderId: number): Promise<Invoice> {
    try {
      const response = await fetch(`${INVOICE_API_BASE}/by-order/${orderId}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });

      return await this.handleResponse<Invoice>(response);
    } catch (error) {
      console.error('Error fetching invoice by order ID:', error);
      throw error;
    }
  }

  /**
   * Gửi hóa đơn qua email
   */
  async sendInvoiceByEmail(invoiceId: number, emailAddress: string): Promise<string> {
    try {
      const params = new URLSearchParams();
      params.append('emailAddress', emailAddress);

      const response = await fetch(`${INVOICE_API_BASE}/${invoiceId}/send-email?${params.toString()}`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
      });

      return await this.handleResponse<string>(response);
    } catch (error) {
      console.error('Error sending invoice email:', error);
      throw error;
    }
  }

  /**
   * Tải file PDF hóa đơn
   */
  async downloadInvoicePdf(invoiceId: number): Promise<Blob> {
    try {
      const response = await fetch(`${INVOICE_API_BASE}/${invoiceId}/download-pdf`, {
        method: 'GET',
        headers: {
          'Authorization': authService.getToken() ? `Bearer ${authService.getToken()}` : '',
        },
      });

      if (!response.ok) {
        throw new Error(`Download failed: ${response.status}`);
      }

      return await response.blob();
    } catch (error) {
      console.error('Error downloading invoice PDF:', error);
      throw error;
    }
  }

  /**
   * Tạo file PDF cho hóa đơn
   */
  async generateInvoicePdf(invoiceId: number): Promise<string> {
    try {
      const response = await fetch(`${INVOICE_API_BASE}/${invoiceId}/generate-pdf`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
      });

      return await this.handleResponse<string>(response);
    } catch (error) {
      console.error('Error generating invoice PDF:', error);
      throw error;
    }
  }

  /**
   * Helper: Download file với tên
   */
  downloadFileFromBlob(blob: Blob, fileName: string): void {
    const url = window.URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = fileName;
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    window.URL.revokeObjectURL(url);
  }

  /**
   * Helper: Lấy email store từ hóa đơn
   * Logic: store.createdAt = user.id trong bảng users
   */
  async getStoreEmailFromInvoice(invoice: Invoice): Promise<string | null> {
    try {
      if (!invoice.order?.store) {
        return null;
      }

      // Giả sử có API để lấy user by ID
      // Thực tế cần implement API này ở backend
      const userId = invoice.order.store.createdAt; // Theo yêu cầu: store.createdAt = user.id
      
      // TODO: Implement API call để lấy user email by ID
      // const userResponse = await fetch(`${USER_API_BASE}/${userId}`, {...});
      // return userResponse.email;
      
      // Tạm thời return store email nếu có
      return invoice.order.store.email || null;
    } catch (error) {
      console.error('Error getting store email:', error);
      return null;
    }
  }

  /**
   * Format currency display
   */
  formatCurrency(amount: number): string {
    return new Intl.NumberFormat('vi-VN', {
      style: 'currency',
      currency: 'VND',
    }).format(amount);
  }

  /**
   * Format date display
   */
  formatDate(dateString: string): string {
    return new Intl.DateTimeFormat('vi-VN', {
      year: 'numeric',
      month: '2-digit',
      day: '2-digit',
      hour: '2-digit',
      minute: '2-digit',
    }).format(new Date(dateString));
  }

  /**
   * Get status display name
   */
  getStatusDisplayName(status: InvoiceStatus, language: string = 'vi'): string {
    const statusMap = {
      'CREATED': language === 'vi' ? 'Đã tạo' : 'Created',
      'SENT': language === 'vi' ? 'Đã gửi' : 'Sent',
      'DELIVERED': language === 'vi' ? 'Đã giao' : 'Delivered',
      'CANCELLED': language === 'vi' ? 'Đã hủy' : 'Cancelled',
    };
    return statusMap[status] || status;
  }

  /**
   * Get status color for UI
   */
  getStatusColor(status: InvoiceStatus): string {
    const colorMap = {
      'CREATED': 'bg-blue-100 text-blue-800',
      'SENT': 'bg-yellow-100 text-yellow-800',
      'DELIVERED': 'bg-green-100 text-green-800',
      'CANCELLED': 'bg-red-100 text-red-800',
    };
    return colorMap[status] || 'bg-gray-100 text-gray-800';
  }
}

// Export singleton instance
export const invoiceAPI = new InvoiceAPIService();
export default invoiceAPI;


