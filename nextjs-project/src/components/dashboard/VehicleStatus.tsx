'use client'

import { TruckIcon } from '@heroicons/react/24/outline'
import { CheckCircleIcon, ExclamationCircleIcon, ClockIcon } from '@heroicons/react/20/solid'

const vehicles = [
  {
    id: 'VH001',
    driver: 'Nguyễn Văn A',
    status: 'delivering',
    location: 'Quận 1, TP.HCM',
    progress: 75,
  },
  {
    id: 'VH002',
    driver: 'Trần Thị B',
    status: 'available',
    location: 'Kho chính',
    progress: 0,
  },
  {
    id: 'VH003',
    driver: 'Lê Văn C',
    status: 'maintenance',
    location: 'Garage',
    progress: 0,
  },
  {
    id: 'VH004',
    driver: 'Phạm Văn D',
    status: 'delivering',
    location: 'Quận 7, TP.HCM',
    progress: 45,
  },
]

const getStatusIcon = (status: string) => {
  switch (status) {
    case 'delivering':
      return <ClockIcon className="h-5 w-5 text-blue-500" />
    case 'available':
      return <CheckCircleIcon className="h-5 w-5 text-green-500" />
    case 'maintenance':
      return <ExclamationCircleIcon className="h-5 w-5 text-red-500" />
    default:
      return <ClockIcon className="h-5 w-5 text-gray-500" />
  }
}

const getStatusText = (status: string) => {
  switch (status) {
    case 'delivering':
      return 'Đang giao hàng'
    case 'available':
      return 'Sẵn sàng'
    case 'maintenance':
      return 'Bảo trì'
    default:
      return 'Không xác định'
  }
}

const getStatusColor = (status: string) => {
  switch (status) {
    case 'delivering':
      return 'bg-blue-100 text-blue-800'
    case 'available':
      return 'bg-green-100 text-green-800'
    case 'maintenance':
      return 'bg-red-100 text-red-800'
    default:
      return 'bg-gray-100 text-gray-800'
  }
}

export default function VehicleStatus() {
  return (
    <div className="rounded-lg bg-white p-6 shadow">
      <div className="mb-4">
        <h3 className="text-lg font-medium text-gray-900">Vehicle Status</h3>
        <p className="text-sm text-gray-500">Real-time vehicle and driver tracking</p>
      </div>
      <div className="space-y-4">
        {vehicles.map((vehicle) => (
          <div key={vehicle.id} className="flex items-center justify-between rounded-lg border p-4">
            <div className="flex items-center space-x-4">
              <div className="flex h-10 w-10 items-center justify-center rounded-lg bg-gray-100">
                <TruckIcon className="h-6 w-6 text-gray-600" />
              </div>
              <div>
                <div className="flex items-center space-x-2">
                  <h4 className="font-medium text-gray-900">{vehicle.id}</h4>
                  <span
                    className={`inline-flex items-center rounded-full px-2.5 py-0.5 text-xs font-medium ${getStatusColor(
                      vehicle.status
                    )}`}
                  >
                    {getStatusText(vehicle.status)}
                  </span>
                </div>
                <p className="text-sm text-gray-500">{vehicle.driver}</p>
                <p className="text-sm text-gray-500">{vehicle.location}</p>
              </div>
            </div>
            <div className="flex items-center space-x-2">
              {getStatusIcon(vehicle.status)}
              {vehicle.status === 'delivering' && (
                <div className="w-20">
                  <div className="overflow-hidden rounded-full bg-gray-200">
                    <div
                      className="h-2 bg-blue-500"
                      style={{ width: `${vehicle.progress}%` }}
                    ></div>
                  </div>
                  <p className="mt-1 text-xs text-gray-500">{vehicle.progress}%</p>
                </div>
              )}
            </div>
          </div>
        ))}
      </div>
    </div>
  )
}
