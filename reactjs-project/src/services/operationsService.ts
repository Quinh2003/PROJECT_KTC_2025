import { getAuthHeaders } from './operationsAPI';
import type { 
  Vehicle, 
  Order, 
  Route, 
  PerformanceMetrics, 
  OperationsSummary, 
  OperationsApiResponse 
} from '../types/Operations';

const API_BASE_URL = 'http://localhost:8080/api';

export class OperationsService {
  private static instance: OperationsService;

  public static getInstance(): OperationsService {
    if (!OperationsService.instance) {
      OperationsService.instance = new OperationsService();
    }
    return OperationsService.instance;
  }

  // Vehicle operations
  async getVehicles(): Promise<Vehicle[]> {
    try {
      const response = await fetch(`${API_BASE_URL}/vehicles`, {
        method: 'GET',
        headers: getAuthHeaders(),
      });

      if (!response.ok) {
        throw new Error(`Failed to fetch vehicles: ${response.status}`);
      }

      const result: OperationsApiResponse<Vehicle[]> = await response.json();
      return result.data;
    } catch (error) {
      console.error('Error fetching vehicles:', error);
      throw new Error('Failed to fetch vehicles');
    }
  }

  async getVehicleById(id: string): Promise<Vehicle> {
    try {
      const response = await fetch(`${API_BASE_URL}/vehicles/${id}`, {
        method: 'GET',
        headers: getAuthHeaders(),
      });

      if (!response.ok) {
        throw new Error(`Failed to fetch vehicle: ${response.status}`);
      }

      const result: OperationsApiResponse<Vehicle> = await response.json();
      return result.data;
    } catch (error) {
      console.error(`Error fetching vehicle ${id}:`, error);
      throw new Error(`Failed to fetch vehicle ${id}`);
    }
  }

  async updateVehicleStatus(id: string, status: Vehicle['status']): Promise<Vehicle> {
    try {
      const response = await fetch(`${API_BASE_URL}/vehicles/${id}/status`, {
        method: 'PUT',
        headers: getAuthHeaders(),
        body: JSON.stringify({ status }),
      });

      if (!response.ok) {
        throw new Error(`Failed to update vehicle status: ${response.status}`);
      }

      const result: OperationsApiResponse<Vehicle> = await response.json();
      return result.data;
    } catch (error) {
      console.error(`Error updating vehicle ${id} status:`, error);
      throw new Error(`Failed to update vehicle ${id} status`);
    }
  }

  // Order operations
  async getOrders(): Promise<Order[]> {
    try {
      const response = await fetch(`${API_BASE_URL}/orders`, {
        method: 'GET',
        headers: getAuthHeaders(),
      });

      if (!response.ok) {
        throw new Error(`Failed to fetch orders: ${response.status}`);
      }

      const result: OperationsApiResponse<Order[]> = await response.json();
      return result.data;
    } catch (error) {
      console.error('Error fetching orders:', error);
      throw new Error('Failed to fetch orders');
    }
  }

  async getOrderById(id: string): Promise<Order> {
    try {
      const response = await fetch(`${API_BASE_URL}/orders/${id}`, {
        method: 'GET',
        headers: getAuthHeaders(),
      });

      if (!response.ok) {
        throw new Error(`Failed to fetch order: ${response.status}`);
      }

      const result: OperationsApiResponse<Order> = await response.json();
      return result.data;
    } catch (error) {
      console.error(`Error fetching order ${id}:`, error);
      throw new Error(`Failed to fetch order ${id}`);
    }
  }

  async updateOrderStatus(id: string, status: Order['status']): Promise<Order> {
    try {
      const response = await fetch(`${API_BASE_URL}/orders/${id}/status`, {
        method: 'PUT',
        headers: getAuthHeaders(),
        body: JSON.stringify({ status }),
      });

      if (!response.ok) {
        throw new Error(`Failed to update order status: ${response.status}`);
      }

      const result: OperationsApiResponse<Order> = await response.json();
      return result.data;
    } catch (error) {
      console.error(`Error updating order ${id} status:`, error);
      throw new Error(`Failed to update order ${id} status`);
    }
  }

  async assignOrderToVehicle(orderId: string, vehicleId: string): Promise<Order> {
    try {
      const response = await fetch(`${API_BASE_URL}/orders/${orderId}/assign`, {
        method: 'PUT',
        headers: getAuthHeaders(),
        body: JSON.stringify({ vehicleId }),
      });

      if (!response.ok) {
        throw new Error(`Failed to assign order: ${response.status}`);
      }

      const result: OperationsApiResponse<Order> = await response.json();
      return result.data;
    } catch (error) {
      console.error(`Error assigning order ${orderId} to vehicle ${vehicleId}:`, error);
      throw new Error(`Failed to assign order ${orderId} to vehicle ${vehicleId}`);
    }
  }

  // Route operations
  async getRoutes(): Promise<Route[]> {
    try {
      const response = await fetch(`${API_BASE_URL}/routes`, {
        method: 'GET',
        headers: getAuthHeaders(),
      });

      if (!response.ok) {
        throw new Error(`Failed to fetch routes: ${response.status}`);
      }

      const result: OperationsApiResponse<Route[]> = await response.json();
      return result.data;
    } catch (error) {
      console.error('Error fetching routes:', error);
      throw new Error('Failed to fetch routes');
    }
  }

  async optimizeRoute(vehicleId: string, orderIds: string[]): Promise<Route> {
    try {
      const response = await fetch(`${API_BASE_URL}/routes/optimize`, {
        method: 'POST',
        headers: getAuthHeaders(),
        body: JSON.stringify({ vehicleId, orderIds }),
      });

      if (!response.ok) {
        throw new Error(`Failed to optimize route: ${response.status}`);
      }

      const result: OperationsApiResponse<Route> = await response.json();
      return result.data;
    } catch (error) {
      console.error('Error optimizing route:', error);
      throw new Error('Failed to optimize route');
    }
  }

  // Dashboard and analytics
  async getOperationsSummary(): Promise<OperationsSummary> {
    try {
      const response = await fetch(`${API_BASE_URL}/dashboard/summary`, {
        method: 'GET',
        headers: getAuthHeaders(),
      });

      if (!response.ok) {
        throw new Error(`Failed to fetch operations summary: ${response.status}`);
      }

      const result: OperationsApiResponse<OperationsSummary> = await response.json();
      return result.data;
    } catch (error) {
      console.error('Error fetching operations summary:', error);
      throw new Error('Failed to fetch operations summary');
    }
  }

  async getPerformanceMetrics(startDate: string, endDate: string): Promise<PerformanceMetrics> {
    try {
      const response = await fetch(
        `${API_BASE_URL}/analytics/performance?start=${startDate}&end=${endDate}`,
        {
          method: 'GET',
          headers: getAuthHeaders(),
        }
      );

      if (!response.ok) {
        throw new Error(`Failed to fetch performance metrics: ${response.status}`);
      }

      const result: OperationsApiResponse<PerformanceMetrics> = await response.json();
      return result.data;
    } catch (error) {
      console.error('Error fetching performance metrics:', error);
      throw new Error('Failed to fetch performance metrics');
    }
  }
}

// Export singleton instance
export const operationsService = OperationsService.getInstance();
