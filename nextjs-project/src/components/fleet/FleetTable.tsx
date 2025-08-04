'use client'

import { useState } from 'react'
import { TruckIcon, WrenchScrewdriverIcon, CheckCircleIcon, ExclamationCircleIcon } from '@heroicons/react/24/outline'

interface Vehicle {
  id: string
  licensePlate: string
  type: string
  capacity: string
  driver: string
  status: 'available' | 'busy' | 'maintenance' | 'offline'
  location: string
  lastMaintenance: string
  nextMaintenance: string
}

const mockVehicles: Vehicle[] = [
  {
    id: '1',
    licensePlate: '51A-12345',
    type: 'Xe tải nhỏ',
    capacity: '1.5 tấn',
    driver: 'Nguyễn Văn A',
    status: 'busy',
    location: 'Quận 1, TP.HCM',
    lastMaintenance: '2024-07-15',
    nextMaintenance: '2024-10-15',
  },
  {
    id: '2',
    licensePlate: '51B-67890',
    type: 'Xe tải trung',
    capacity: '3.5 tấn',
    driver: 'Trần Thị B',
    status: 'available',
    location: 'Kho chính',
    lastMaintenance: '2024-06-20',
    nextMaintenance: '2024-09-20',
  },
  {
    id: '3',
    licensePlate: '51C-11111',
    type: 'Xe tải lớn',
    capacity: '8 tấn',
    driver: 'Lê Văn C',
    status: 'maintenance',
    location: 'Garage',
    lastMaintenance: '2024-08-01',
    nextMaintenance: '2024-11-01',
  },
  {
    id: '4',
    licensePlate: '51D-22222',
    type: 'Xe van',
    capacity: '800 kg',
    driver: 'Phạm Văn D',
    status: 'busy',
    location: 'Quận 7, TP.HCM',
    lastMaintenance: '2024-05-10',
    nextMaintenance: '2024-08-10',
  },
]

const getStatusColor = (status: Vehicle['status']) => {
  switch (status) {
    case 'available':
      return 'bg-green-100 text-green-800'
    case 'busy':
      return 'bg-blue-100 text-blue-800'
    case 'maintenance':
      return 'bg-yellow-100 text-yellow-800'
    case 'offline':
      return 'bg-red-100 text-red-800'
    default:
      return 'bg-gray-100 text-gray-800'
  }
}

const getStatusText = (status: Vehicle['status']) => {
  switch (status) {
    case 'available':
      return 'Sẵn sàng'
    case 'busy':
      return 'Đang hoạt động'
    case 'maintenance':
      return 'Bảo trì'
    case 'offline':
      return 'Offline'
    default:
      return 'Không xác định'
  }
}

const getStatusIcon = (status: Vehicle['status']) => {
  switch (status) {
    case 'available':
      return <CheckCircleIcon className="h-5 w-5 text-green-500" />
    case 'busy':
      return <TruckIcon className="h-5 w-5 text-blue-500" />
    case 'maintenance':
      return <WrenchScrewdriverIcon className="h-5 w-5 text-yellow-500" />
    case 'offline':
      return <ExclamationCircleIcon className="h-5 w-5 text-red-500" />
    default:
      return <TruckIcon className="h-5 w-5 text-gray-500" />
  }
}

export default function FleetTable() {
  const [vehicles] = useState<Vehicle[]>(mockVehicles)
  const [selectedStatus, setSelectedStatus] = useState<string>('all')

  const filteredVehicles = selectedStatus === 'all' 
    ? vehicles 
    : vehicles.filter(vehicle => vehicle.status === selectedStatus)

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
              <option value="available">Sẵn sàng</option>
              <option value="busy">Đang hoạt động</option>
              <option value="maintenance">Bảo trì</option>
              <option value="offline">Offline</option>
            </select>
          </div>
          <div className="text-sm text-gray-700">
            Showing {filteredVehicles.length} of {vehicles.length} vehicles
          </div>
        </div>
      </div>

      {/* Table */}
      <div className="overflow-x-auto">
        <table className="min-w-full divide-y divide-gray-200">
          <thead className="bg-gray-50">
            <tr>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Vehicle
              </th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Driver
              </th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Status
              </th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Location
              </th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Maintenance
              </th>
              <th className="px-6 py-3 text-right text-xs font-medium text-gray-500 uppercase tracking-wider">
                Actions
              </th>
            </tr>
          </thead>
          <tbody className="bg-white divide-y divide-gray-200">
            {filteredVehicles.map((vehicle) => (
              <tr key={vehicle.id} className="hover:bg-gray-50">
                <td className="px-6 py-4 whitespace-nowrap">
                  <div className="flex items-center">
                    <TruckIcon className="h-10 w-10 text-gray-400" />
                    <div className="ml-4">
                      <div className="text-sm font-medium text-gray-900">
                        {vehicle.licensePlate}
                      </div>
                      <div className="text-sm text-gray-500">
                        {vehicle.type} • {vehicle.capacity}
                      </div>
                    </div>
                  </div>
                </td>
                <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                  {vehicle.driver}
                </td>
                <td className="px-6 py-4 whitespace-nowrap">
                  <div className="flex items-center">
                    {getStatusIcon(vehicle.status)}
                    <span className={`ml-2 inline-flex px-2 py-1 text-xs font-semibold rounded-full ${getStatusColor(vehicle.status)}`}>
                      {getStatusText(vehicle.status)}
                    </span>
                  </div>
                </td>
                <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                  {vehicle.location}
                </td>
                <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                  <div>
                    <div className="text-gray-900">Last: {vehicle.lastMaintenance}</div>
                    <div className="text-gray-500">Next: {vehicle.nextMaintenance}</div>
                  </div>
                </td>
                <td className="px-6 py-4 whitespace-nowrap text-right text-sm font-medium">
                  <div className="flex justify-end space-x-2">
                    <button className="text-indigo-600 hover:text-indigo-900 text-sm">
                      View
                    </button>
                    <button className="text-green-600 hover:text-green-900 text-sm">
                      Edit
                    </button>
                    <button className="text-yellow-600 hover:text-yellow-900 text-sm">
                      Maintenance
                    </button>
                  </div>
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>

      {filteredVehicles.length === 0 && (
        <div className="text-center py-12">
          <p className="text-gray-500">No vehicles found matching the selected criteria.</p>
        </div>
      )}
    </div>
  )
}
