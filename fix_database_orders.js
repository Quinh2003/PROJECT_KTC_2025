#!/usr/bin/env node

/**
 * Script để tự động gán store và address cho các orders cũ trong database
 * 
 * Chạy: node fix_database_orders.js
 */

const BASE_URL = 'http://localhost:8080/api';

// Danh sách stores và addresses có sẵn (lấy mẫu)
const SAMPLE_STORES = [93, 94, 95, 96, 97, 98, 99, 100, 101, 102];
const SAMPLE_ADDRESSES = [3009, 2955, 2956, 2957, 2958, 2959, 2960, 2961, 2962, 2963];

// Mô tả orders mẫu
const ORDER_DESCRIPTIONS = [
  'Đơn hàng giao thiết bị điện tử',
  'Đơn hàng giao thực phẩm tươi sống', 
  'Đơn hàng giao văn phòng phẩm',
  'Đơn hàng giao quần áo thời trang',
  'Đơn hàng giao đồ gia dụng',
  'Đơn hàng giao sách và tạp chí',
  'Đơn hàng giao đồ chơi trẻ em',
  'Đơn hàng giao mỹ phẩm',
  'Đơn hàng giao dụng cụ thể thao',
  'Đơn hàng giao thực phẩm khô'
];

async function fetchOrders(page = 1, size = 100) {
  const response = await fetch(`${BASE_URL}/orders?page=${page}&size=${size}`);
  return await response.json();
}

async function updateOrder(orderId, orderData) {
  const response = await fetch(`${BASE_URL}/orders/${orderId}`, {
    method: 'PATCH',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify(orderData)
  });
  return response.ok;
}

function getRandomElement(array) {
  return array[Math.floor(Math.random() * array.length)];
}

function generateRandomAmount() {
  return Math.floor(Math.random() * 3000 + 500); // 500-3500
}

async function fixDatabaseOrders() {
  console.log('🚀 Bắt đầu khắc phục orders trong database...');
  
  try {
    // Lấy trang đầu tiên để kiểm tra
    const firstPage = await fetchOrders(1, 50);
    console.log(`📊 Tổng số orders: ${firstPage.totalRecords}`);
    
    let fixedCount = 0;
    let currentPage = 1;
    
    while (currentPage <= Math.min(10, firstPage.totalPages)) { // Giới hạn 10 trang đầu
      console.log(`\n📄 Đang xử lý trang ${currentPage}...`);
      
      const pageData = await fetchOrders(currentPage, 50);
      const emptyOrders = pageData.data.filter(order => !order.store || !order.address);
      
      for (const order of emptyOrders) {
        const updateData = {
          description: getRandomElement(ORDER_DESCRIPTIONS),
          totalAmount: generateRandomAmount(),
          benefitPerOrder: Math.floor(generateRandomAmount() * 0.1),
          orderProfitPerOrder: Math.floor(generateRandomAmount() * 0.08),
          store: { id: getRandomElement(SAMPLE_STORES) },
          address: { id: getRandomElement(SAMPLE_ADDRESSES) }
        };
        
        const success = await updateOrder(order.id, updateData);
        if (success) {
          fixedCount++;
          console.log(`✅ Đã sửa order #${order.id}`);
        } else {
          console.log(`❌ Lỗi khi sửa order #${order.id}`);
        }
        
        // Delay để tránh quá tải server
        await new Promise(resolve => setTimeout(resolve, 100));
      }
      
      currentPage++;
    }
    
    console.log(`\n🎉 Hoàn thành! Đã sửa ${fixedCount} orders`);
    console.log('📝 Gợi ý: Refresh lại trang dispatcher để xem kết quả');
    
  } catch (error) {
    console.error('❌ Lỗi:', error.message);
  }
}

// Chạy script
if (require.main === module) {
  fixDatabaseOrders();
}

module.exports = { fixDatabaseOrders };

