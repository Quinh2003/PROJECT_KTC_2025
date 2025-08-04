'use client'

import { useState } from 'react'
import { EyeIcon, PencilIcon, TrashIcon } from '@heroicons/react/24/outline'

interface Order {
  id: string
  orderCode: string
  customer: string
  pickup: string
  delivery: string
  driver: string
  status: 'pending' | 'assigned' | 'picking_up' | 'delivering' | 'completed' | 'cancelled'
  amount: string
  createdAt: string
}

const mockOrders: Order[] = [
  {
    id: '1',
    orderCode: 'ORD-2024-001',
    customer: 'Công ty ABC',
    pickup: 'Kho Quận 7',
    delivery: 'Quận 1, TP.HCM',
    driver: 'Nguyễn Văn A',
    status: 'delivering',
    amount: '2,500,000',
    createdAt: '2024-08-04 08:30',
  },
  {
    id: '2',
    orderCode: 'ORD-2024-002',
    customer: 'Siêu thị XYZ',
    pickup: 'Kho Bình Tân',
    delivery: 'Quận 3, TP.HCM',
    driver: 'Trần Thị B',
    status: 'completed',
    amount: '1,800,000',
    createdAt: '2024-08-04 09:15',
  },
  {
    id: '3',
    orderCode: 'ORD-2024-003',
    customer: 'Nhà hàng DEF',
    pickup: 'Kho Quận 12',
    delivery: 'Quận 7, TP.HCM',
    driver: 'Chưa phân công',
    status: 'pending',
    amount: '3,200,000',
    createdAt: '2024-08-04 10:00',
  },
]

const getStatusColor = (status: Order['status']) => {
  switch (status) {
    case 'pending':
      return 'bg-yellow-100 text-yellow-800'
    case 'assigned':
      return 'bg-blue-100 text-blue-800'
    case 'picking_up':
      return 'bg-purple-100 text-purple-800'
    case 'delivering':
      return 'bg-indigo-100 text-indigo-800'
    case 'completed':
      return 'bg-green-100 text-green-800'
    case 'cancelled':
      return 'bg-red-100 text-red-800'
    default:
      return 'bg-gray-100 text-gray-800'
  }
}

const getStatusText = (status: Order['status']) => {
  switch (status) {
    case 'pending':
      return 'Chờ xử lý'
    case 'assigned':
      return 'Đã phân công'
    case 'picking_up':
      return 'Đang lấy hàng'
    case 'delivering':
      return 'Đang giao'
    case 'completed':
      return 'Hoàn thành'
    case 'cancelled':
      return 'Đã hủy'
    default:
      return 'Không xác định'
  }
}

export default function OrdersTable() {
  const [orders] = useState<Order[]>(mockOrders)
  const [selectedStatus, setSelectedStatus] = useState<string>('all')

  const filteredOrders = selectedStatus === 'all' 
    ? orders 
    : orders.filter(order => order.status === selectedStatus)

  return (
    <div className="bg-white shadow-sm ring-1 ring-gray-900/5 rounded-lg">
      {/* Filters */}
      <div className="px-6 py-4 border-b border-gray-200">
        <div className="flex items-center justify-between">
          <div className="flex space-x-4">
            <select
              value={selectedStatus}
              onChange={(e) => setSelectedStatus(e.target.value)}
              className="rounded-md border-gray-300 text-sm focus:border-indigo-500 focus:ring-indigo-500"
            >
              <option value="all">Tất cả trạng thái</option>
              <option value="pending">Chờ xử lý</option>
              <option value="assigned">Đã phân công</option>
              <option value="picking_up">Đang lấy hàng</option>
              <option value="delivering">Đang giao</option>
              <option value="completed">Hoàn thành</option>
              <option value="cancelled">Đã hủy</option>
            </select>
          </div>
          <div className="text-sm text-gray-700">
            Showing {filteredOrders.length} of {orders.length} orders
          </div>
        </div>
      </div>

      {/* Table */}
      <div className="overflow-x-auto">
        <table className="min-w-full divide-y divide-gray-200">
          <thead className="bg-gray-50">
            <tr>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Order Code
              </th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Customer
              </th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Route
              </th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Driver
              </th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Status
              </th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Amount
              </th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Created
              </th>
              <th className="px-6 py-3 text-right text-xs font-medium text-gray-500 uppercase tracking-wider">
                Actions
              </th>
            </tr>
          </thead>
          <tbody className="bg-white divide-y divide-gray-200">
            {filteredOrders.map((order) => (
              <tr key={order.id} className="hover:bg-gray-50">
                <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                  {order.orderCode}
                </td>
                <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                  {order.customer}
                </td>
                <td className="px-6 py-4 text-sm text-gray-900">
                  <div>
                    <div className="font-medium">From: {order.pickup}</div>
                    <div className="text-gray-500">To: {order.delivery}</div>
                  </div>
                </td>
                <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                  {order.driver}
                </td>
                <td className="px-6 py-4 whitespace-nowrap">
                  <span className={`inline-flex px-2 py-1 text-xs font-semibold rounded-full ${getStatusColor(order.status)}`}>
                    {getStatusText(order.status)}
                  </span>
                </td>
                <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                  {order.amount} VNĐ
                </td>
                <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                  {order.createdAt}
                </td>
                <td className="px-6 py-4 whitespace-nowrap text-right text-sm font-medium">
                  <div className="flex justify-end space-x-2">
                    <button className="text-indigo-600 hover:text-indigo-900">
                      <EyeIcon className="h-4 w-4" />
                    </button>
                    <button className="text-green-600 hover:text-green-900">
                      <PencilIcon className="h-4 w-4" />
                    </button>
                    <button className="text-red-600 hover:text-red-900">
                      <TrashIcon className="h-4 w-4" />
                    </button>
                  </div>
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>

      {filteredOrders.length === 0 && (
        <div className="text-center py-12">
          <p className="text-gray-500">No orders found matching the selected criteria.</p>
        </div>
      )}
    </div>
  )
}
