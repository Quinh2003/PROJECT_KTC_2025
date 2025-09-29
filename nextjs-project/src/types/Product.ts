export interface Product {
  id: number;
  name: string;
  description?: string;
  unitPrice: number;
  weight?: number;
  volume?: number;
  fragile?: boolean;
  stockQuantity?: number;
  notes?: string;
  warehouseId?: number | null;
  createdByUserId?: number | null;
  createdAt?: string;
  updatedAt?: string;
}
