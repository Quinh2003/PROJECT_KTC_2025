import { PlusIcon } from '@heroicons/react/24/outline'
import OrdersTable from '@/components/orders/OrdersTable'

export default function OrdersPage() {
  return (
    <div className="space-y-6">
      {/* Page header */}
      <div className="sm:flex sm:items-center">
        <div className="sm:flex-auto">
          <h1 className="text-2xl font-bold text-gray-900">Orders Management</h1>
          <p className="mt-2 text-sm text-gray-700">
            Manage all delivery orders, track status, and assign drivers
          </p>
        </div>
        <div className="mt-4 sm:ml-16 sm:mt-0 sm:flex-none">
          <button
            type="button"
            className="block rounded-md bg-indigo-600 px-3 py-2 text-center text-sm font-semibold text-white shadow-sm hover:bg-indigo-500 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600"
          >
            <PlusIcon className="inline h-4 w-4 mr-2" />
            New Order
          </button>
        </div>
      </div>

      {/* Orders table */}
      <OrdersTable />
    </div>
  )
}
