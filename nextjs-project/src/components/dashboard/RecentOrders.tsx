'use client'

import { ClockIcon, CheckCircleIcon, TruckIcon } from '@heroicons/react/20/solid'

const orders = [
  {
    id: 'ORD-001',
    customer: 'Công ty TNHH ABC',
    destination: 'Quận 1, TP.HCM',
    status: 'delivering',
    driver: 'Nguyễn Văn A',
    time: '2 giờ trước',
    amount: '2,500,000 VNĐ',
  },
  {
    id: 'ORD-002',
    customer: 'Siêu thị XYZ',
    destination: 'Quận 3, TP.HCM',
    status: 'completed',
    driver: 'Trần Thị B',
    time: '4 giờ trước',
    amount: '1,800,000 VNĐ',
  },
  {
    id: 'ORD-003',
    customer: 'Nhà hàng DEF',
    destination: 'Quận 7, TP.HCM',
    status: 'pending',
    driver: 'Chưa phân công',
    time: '1 giờ trước',
    amount: '3,200,000 VNĐ',
  },
  {
    id: 'ORD-004',
    customer: 'Cửa hàng GHI',
    destination: 'Quận 10, TP.HCM',
    status: 'delivering',
    driver: 'Lê Văn C',
    time: '3 giờ trước',
    amount: '950,000 VNĐ',
  },
  {
    id: 'ORD-005',
    customer: 'Văn phòng JKL',
    destination: 'Quận 2, TP.HCM',
    status: 'completed',
    driver: 'Phạm Văn D',
    time: '5 giờ trước',
    amount: '1,250,000 VNĐ',
  },
]

const getStatusIcon = (status: string) => {
  switch (status) {
    case 'delivering':
      return <TruckIcon className="h-5 w-5 text-blue-500" />
    case 'completed':
      return <CheckCircleIcon className="h-5 w-5 text-green-500" />
    case 'pending':
      return <ClockIcon className="h-5 w-5 text-yellow-500" />
    default:
      return <ClockIcon className="h-5 w-5 text-gray-500" />
  }
}

const getStatusText = (status: string) => {
  switch (status) {
    case 'delivering':
      return 'Đang giao'
    case 'completed':
      return 'Hoàn thành'
    case 'pending':
      return 'Chờ xử lý'
    default:
      return 'Không xác định'
  }
}

const getStatusColor = (status: string) => {
  switch (status) {
    case 'delivering':
      return 'bg-blue-100 text-blue-800'
    case 'completed':
      return 'bg-green-100 text-green-800'
    case 'pending':
      return 'bg-yellow-100 text-yellow-800'
    default:
      return 'bg-gray-100 text-gray-800'
  }
}

export default function RecentOrders() {
  return (
    <div className="rounded-lg bg-white shadow">
      <div className="px-6 py-4">
        <h3 className="text-lg font-medium text-gray-900">Recent Orders</h3>
        <p className="text-sm text-gray-500">Latest delivery orders and their status</p>
      </div>
      <div className="overflow-hidden">
        <table className="min-w-full divide-y divide-gray-200">
          <thead className="bg-gray-50">
            <tr>
              <th className="px-6 py-3 text-left text-xs font-medium uppercase tracking-wide text-gray-500">
                Order ID
              </th>
              <th className="px-6 py-3 text-left text-xs font-medium uppercase tracking-wide text-gray-500">
                Customer
              </th>
              <th className="px-6 py-3 text-left text-xs font-medium uppercase tracking-wide text-gray-500">
                Destination
              </th>
              <th className="px-6 py-3 text-left text-xs font-medium uppercase tracking-wide text-gray-500">
                Driver
              </th>
              <th className="px-6 py-3 text-left text-xs font-medium uppercase tracking-wide text-gray-500">
                Status
              </th>
              <th className="px-6 py-3 text-left text-xs font-medium uppercase tracking-wide text-gray-500">
                Amount
              </th>
              <th className="px-6 py-3 text-left text-xs font-medium uppercase tracking-wide text-gray-500">
                Time
              </th>
            </tr>
          </thead>
          <tbody className="divide-y divide-gray-200 bg-white">
            {orders.map((order) => (
              <tr key={order.id} className="hover:bg-gray-50">
                <td className="whitespace-nowrap px-6 py-4 text-sm font-medium text-gray-900">
                  {order.id}
                </td>
                <td className="whitespace-nowrap px-6 py-4 text-sm text-gray-900">
                  {order.customer}
                </td>
                <td className="whitespace-nowrap px-6 py-4 text-sm text-gray-900">
                  {order.destination}
                </td>
                <td className="whitespace-nowrap px-6 py-4 text-sm text-gray-900">
                  {order.driver}
                </td>
                <td className="whitespace-nowrap px-6 py-4">
                  <div className="flex items-center">
                    {getStatusIcon(order.status)}
                    <span
                      className={`ml-2 inline-flex rounded-full px-2 text-xs font-semibold leading-5 ${getStatusColor(
                        order.status
                      )}`}
                    >
                      {getStatusText(order.status)}
                    </span>
                  </div>
                </td>
                <td className="whitespace-nowrap px-6 py-4 text-sm text-gray-900">
                  {order.amount}
                </td>
                <td className="whitespace-nowrap px-6 py-4 text-sm text-gray-500">
                  {order.time}
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  )
}
