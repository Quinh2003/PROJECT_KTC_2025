import { Product } from '@/types/Product';

const API_URL = 'http://localhost:8080/api/products';

export async function fetchProducts(): Promise<Product[]> {
  const res = await fetch(API_URL);
  if (!res.ok) throw new Error('Lỗi lấy danh sách sản phẩm');
  return res.json();
}

export async function createProduct(product: any): Promise<any> {
  // Map dữ liệu sang đúng định dạng Spring Boot backend yêu cầu
  if (!product.categoryId) {
    throw new Error('Thiếu categoryId cho sản phẩm');
  }
  const payload = {
    name: product.name,
    description: product.description || '',
    unitPrice: product.unitPrice || null,
    weight: product.weight || null,
    volume: product.volume || null, // Thêm volume vào payload
    fragile: product.is_fragile ?? false,
    stockQuantity: product.stockQuantity || 0,
    notes: product.notes || '',
    warehouseId: product.warehouseId || null,
    createdByUserId: product.created_by || null,
    categoryId: product.categoryId,
    product_status: product.product_status ?? 1,
  };
  const res = await fetch(API_URL, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(payload),
  });
  if (!res.ok) throw new Error('Lỗi tạo sản phẩm');
  return res.json();
}
